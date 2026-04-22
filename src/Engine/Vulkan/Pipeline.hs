module Engine.Vulkan.Pipeline
  ( PipelineContext(..)
  , HudPipeline(..)
  , EntityPipeline(..)
  , DepthResources(..)
  , createRenderPass
  , createGraphicsPipeline
  , createHudPipeline
  , createEntityPipeline
  , destroyPipelineContext
  , destroyHudPipeline
  , destroyEntityPipeline
  , createFramebuffers
  , destroyFramebuffers
  , createDepthResources
  , destroyDepthResources
  , findDepthFormat
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

import Engine.Vulkan.Swapchain (SwapchainContext(..))

import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)
import Data.Bits ((.|.), (.&.), shiftL)
import Control.Exception (throwIO)

-- | Pipeline and render pass resources
data PipelineContext = PipelineContext
  { pcRenderPass     :: !Vk.RenderPass
  , pcPipelineLayout :: !Vk.PipelineLayout
  , pcPipeline       :: !Vk.Pipeline
  } deriving stock (Show)

-- | HUD overlay pipeline (no depth test, alpha blending)
data HudPipeline = HudPipeline
  { hpPipelineLayout :: !Vk.PipelineLayout
  , hpPipeline       :: !Vk.Pipeline
  } deriving stock (Show)

-- | Entity billboard pipeline (depth test on, alpha blend, no cull)
data EntityPipeline = EntityPipeline
  { epPipelineLayout :: !Vk.PipelineLayout
  , epPipeline       :: !Vk.Pipeline
  } deriving stock (Show)

-- | Depth buffer resources
data DepthResources = DepthResources
  { drImage     :: !Vk.Image
  , drMemory    :: !Vk.DeviceMemory
  , drImageView :: !Vk.ImageView
  , drFormat    :: !Vk.Format
  } deriving stock (Show)

-- | Create render pass with color + depth attachments
createRenderPass :: Vk.Device -> Vk.Format -> Vk.Format -> IO Vk.RenderPass
createRenderPass device colorFormat depthFormat = do
  let colorAttachment = Vk.AttachmentDescription
        { Vk.flags          = Vk.zero
        , Vk.format         = colorFormat
        , Vk.samples        = Vk.SAMPLE_COUNT_1_BIT
        , Vk.loadOp         = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp        = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout    = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        }

  let depthAttachment = Vk.AttachmentDescription
        { Vk.flags          = Vk.zero
        , Vk.format         = depthFormat
        , Vk.samples        = Vk.SAMPLE_COUNT_1_BIT
        , Vk.loadOp         = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp        = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout    = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        }

  let colorRef = Vk.AttachmentReference
        { Vk.attachment = 0
        , Vk.layout     = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }

  let depthRef = Vk.AttachmentReference
        { Vk.attachment = 1
        , Vk.layout     = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
        }

  let subpass = Vk.SubpassDescription
        { Vk.flags                   = Vk.zero
        , Vk.pipelineBindPoint       = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.inputAttachments        = V.empty
        , Vk.colorAttachments        = V.singleton colorRef
        , Vk.resolveAttachments      = V.empty
        , Vk.depthStencilAttachment  = Just depthRef
        , Vk.preserveAttachments     = V.empty
        }

  let dependency = Vk.SubpassDependency
        { Vk.srcSubpass    = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass    = 0
        , Vk.srcStageMask  = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                           .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
        , Vk.srcAccessMask = Vk.zero
        , Vk.dstStageMask  = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                           .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
        , Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                           .|. Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
        , Vk.dependencyFlags = Vk.zero
        }

  let createInfo = Vk.RenderPassCreateInfo
        { Vk.next         = ()
        , Vk.flags        = Vk.zero
        , Vk.attachments  = V.fromList [colorAttachment, depthAttachment]
        , Vk.subpasses    = V.singleton subpass
        , Vk.dependencies = V.singleton dependency
        }

  Vk.createRenderPass device createInfo Nothing

-- | Create shader module from SPIR-V bytecode
createShaderModule :: Vk.Device -> BS.ByteString -> IO Vk.ShaderModule
createShaderModule device code = do
  let createInfo = Vk.ShaderModuleCreateInfo
        { Vk.next  = ()
        , Vk.flags = Vk.zero
        , Vk.code  = code
        }
  Vk.createShaderModule device createInfo Nothing

-- | Create graphics pipeline for block rendering with vertex input and descriptor sets
createGraphicsPipeline
  :: Vk.Device
  -> Vk.RenderPass
  -> Vk.DescriptorSetLayout
  -> FilePath    -- ^ Vertex shader SPIR-V path
  -> FilePath    -- ^ Fragment shader SPIR-V path
  -> IO PipelineContext
createGraphicsPipeline device renderPass dsLayout vertPath fragPath = do
  vertCode <- BS.readFile vertPath
  fragCode <- BS.readFile fragPath

  vertModule <- createShaderModule device vertCode
  fragModule <- createShaderModule device fragCode

  let vertStage = Vk.PipelineShaderStageCreateInfo
        { Vk.next               = ()
        , Vk.flags              = Vk.zero
        , Vk.stage              = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.module'            = vertModule
        , Vk.name               = "main"
        , Vk.specializationInfo = Nothing
        }

  let fragStage = Vk.PipelineShaderStageCreateInfo
        { Vk.next               = ()
        , Vk.flags              = Vk.zero
        , Vk.stage              = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.module'            = fragModule
        , Vk.name               = "main"
        , Vk.specializationInfo = Nothing
        }

  -- Vertex input: BlockVertex (pos vec3, normal vec3, texcoord vec2, ao float) = 36 bytes
  let bindingDesc = Vk.VertexInputBindingDescription
        { Vk.binding   = 0
        , Vk.stride    = 36  -- sizeof BlockVertex
        , Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
        }

  let attrDescs = V.fromList
        [ Vk.VertexInputAttributeDescription  -- position: vec3 at offset 0
            { Vk.location = 0
            , Vk.binding  = 0
            , Vk.format   = Vk.FORMAT_R32G32B32_SFLOAT
            , Vk.offset   = 0
            }
        , Vk.VertexInputAttributeDescription  -- normal: vec3 at offset 12
            { Vk.location = 1
            , Vk.binding  = 0
            , Vk.format   = Vk.FORMAT_R32G32B32_SFLOAT
            , Vk.offset   = 12
            }
        , Vk.VertexInputAttributeDescription  -- texcoord: vec2 at offset 24
            { Vk.location = 2
            , Vk.binding  = 0
            , Vk.format   = Vk.FORMAT_R32G32_SFLOAT
            , Vk.offset   = 24
            }
        , Vk.VertexInputAttributeDescription  -- ao: float at offset 32
            { Vk.location = 3
            , Vk.binding  = 0
            , Vk.format   = Vk.FORMAT_R32_SFLOAT
            , Vk.offset   = 32
            }
        ]

  let vertexInputInfo = Vk.PipelineVertexInputStateCreateInfo
        { Vk.next                           = ()
        , Vk.flags                          = Vk.zero
        , Vk.vertexBindingDescriptions      = V.singleton bindingDesc
        , Vk.vertexAttributeDescriptions    = attrDescs
        }

  let inputAssembly = Vk.PipelineInputAssemblyStateCreateInfo
        { Vk.flags                 = Vk.zero
        , Vk.topology              = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        , Vk.primitiveRestartEnable = False
        }

  -- Dynamic viewport and scissor (set per-frame via cmdSetViewport/cmdSetScissor)
  let viewportState = Vk.PipelineViewportStateCreateInfo
        { Vk.next      = ()
        , Vk.flags     = Vk.zero
        , Vk.viewportCount = 1
        , Vk.viewports = V.empty  -- dynamic
        , Vk.scissorCount  = 1
        , Vk.scissors  = V.empty  -- dynamic
        }

  let rasterizer = Vk.PipelineRasterizationStateCreateInfo
        { Vk.next                   = ()
        , Vk.flags                  = Vk.zero
        , Vk.depthClampEnable       = False
        , Vk.rasterizerDiscardEnable = False
        , Vk.polygonMode            = Vk.POLYGON_MODE_FILL
        , Vk.lineWidth              = 1.0
        , Vk.cullMode               = Vk.CULL_MODE_BACK_BIT
        , Vk.frontFace              = Vk.FRONT_FACE_CLOCKWISE
        , Vk.depthBiasEnable        = False
        , Vk.depthBiasConstantFactor = 0
        , Vk.depthBiasClamp          = 0
        , Vk.depthBiasSlopeFactor    = 0
        }

  let multisampling = Vk.PipelineMultisampleStateCreateInfo
        { Vk.next                 = ()
        , Vk.flags                = Vk.zero
        , Vk.sampleShadingEnable  = False
        , Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.minSampleShading     = 1
        , Vk.sampleMask           = V.empty
        , Vk.alphaToCoverageEnable = False
        , Vk.alphaToOneEnable      = False
        }

  let colorBlendAttachment = Vk.PipelineColorBlendAttachmentState
        { Vk.colorWriteMask      = Vk.COLOR_COMPONENT_R_BIT
                                 .|. Vk.COLOR_COMPONENT_G_BIT
                                 .|. Vk.COLOR_COMPONENT_B_BIT
                                 .|. Vk.COLOR_COMPONENT_A_BIT
        , Vk.blendEnable         = False
        , Vk.srcColorBlendFactor = Vk.BLEND_FACTOR_ONE
        , Vk.dstColorBlendFactor = Vk.BLEND_FACTOR_ZERO
        , Vk.colorBlendOp        = Vk.BLEND_OP_ADD
        , Vk.srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
        , Vk.dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
        , Vk.alphaBlendOp        = Vk.BLEND_OP_ADD
        }

  let colorBlending = Vk.PipelineColorBlendStateCreateInfo
        { Vk.next           = ()
        , Vk.flags          = Vk.zero
        , Vk.logicOpEnable  = False
        , Vk.logicOp        = Vk.LOGIC_OP_COPY
        , Vk.attachmentCount = 1
        , Vk.attachments    = V.singleton colorBlendAttachment
        , Vk.blendConstants = (0, 0, 0, 0)
        }

  -- Depth/stencil testing
  let depthStencil = Vk.PipelineDepthStencilStateCreateInfo
        { Vk.flags                = Vk.zero
        , Vk.depthTestEnable      = True
        , Vk.depthWriteEnable     = True
        , Vk.depthCompareOp       = Vk.COMPARE_OP_LESS
        , Vk.depthBoundsTestEnable = False
        , Vk.stencilTestEnable    = False
        , Vk.front                = Vk.zero
        , Vk.back                 = Vk.zero
        , Vk.minDepthBounds       = 0
        , Vk.maxDepthBounds       = 1
        }

  -- Pipeline layout with descriptor set for UBO + texture
  let layoutInfo = Vk.PipelineLayoutCreateInfo
        { Vk.flags              = Vk.zero
        , Vk.setLayouts         = V.singleton dsLayout
        , Vk.pushConstantRanges = V.empty
        }

  pipelineLayout <- Vk.createPipelineLayout device layoutInfo Nothing

  let dynamicState = Vk.PipelineDynamicStateCreateInfo
        { Vk.flags       = Vk.zero
        , Vk.dynamicStates = V.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR]
        }

  let pipelineInfo = Vk.GraphicsPipelineCreateInfo
        { Vk.next               = ()
        , Vk.flags              = Vk.zero
        , Vk.stageCount         = 2
        , Vk.stages             = V.fromList [Vk.SomeStruct vertStage, Vk.SomeStruct fragStage]
        , Vk.vertexInputState   = Just (Vk.SomeStruct vertexInputInfo)
        , Vk.inputAssemblyState = Just inputAssembly
        , Vk.tessellationState  = Nothing
        , Vk.viewportState      = Just (Vk.SomeStruct viewportState)
        , Vk.rasterizationState = Just (Vk.SomeStruct rasterizer)
        , Vk.multisampleState   = Just (Vk.SomeStruct multisampling)
        , Vk.depthStencilState  = Just depthStencil
        , Vk.colorBlendState    = Just (Vk.SomeStruct colorBlending)
        , Vk.dynamicState       = Just dynamicState
        , Vk.layout             = pipelineLayout
        , Vk.renderPass         = renderPass
        , Vk.subpass            = 0
        , Vk.basePipelineHandle = Vk.zero
        , Vk.basePipelineIndex  = -1
        }

  (_, pipelines) <- Vk.createGraphicsPipelines device Vk.zero (V.singleton (Vk.SomeStruct pipelineInfo)) Nothing
  let pipeline = V.head pipelines

  -- Cleanup shader modules (no longer needed after pipeline creation)
  Vk.destroyShaderModule device vertModule Nothing
  Vk.destroyShaderModule device fragModule Nothing

  pure PipelineContext
    { pcRenderPass     = renderPass
    , pcPipelineLayout = pipelineLayout
    , pcPipeline       = pipeline
    }

-- | Destroy pipeline resources
destroyPipelineContext :: Vk.Device -> PipelineContext -> IO ()
destroyPipelineContext device pc = do
  Vk.destroyPipeline device (pcPipeline pc) Nothing
  Vk.destroyPipelineLayout device (pcPipelineLayout pc) Nothing
  Vk.destroyRenderPass device (pcRenderPass pc) Nothing

-- | Create HUD overlay pipeline: 2D colored vertices, no depth test, alpha blending
createHudPipeline :: Vk.Device -> Vk.RenderPass -> FilePath -> FilePath -> IO HudPipeline
createHudPipeline device renderPass vertPath fragPath = do
  vertCode <- BS.readFile vertPath
  fragCode <- BS.readFile fragPath
  vertModule <- createShaderModule device vertCode
  fragModule <- createShaderModule device fragCode

  let vertStage = Vk.PipelineShaderStageCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.module' = vertModule, Vk.name = "main"
        , Vk.specializationInfo = Nothing }
      fragStage = Vk.PipelineShaderStageCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.module' = fragModule, Vk.name = "main"
        , Vk.specializationInfo = Nothing }

  -- Vertex input: vec2 position + vec4 color = 24 bytes
  let bindingDesc = Vk.VertexInputBindingDescription 0 24 Vk.VERTEX_INPUT_RATE_VERTEX
      attrDescs = V.fromList
        [ Vk.VertexInputAttributeDescription 0 0 Vk.FORMAT_R32G32_SFLOAT 0      -- position
        , Vk.VertexInputAttributeDescription 1 0 Vk.FORMAT_R32G32B32A32_SFLOAT 8 -- color
        ]
      vertexInput = Vk.PipelineVertexInputStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.vertexBindingDescriptions = V.singleton bindingDesc
        , Vk.vertexAttributeDescriptions = attrDescs }
      inputAssembly = Vk.PipelineInputAssemblyStateCreateInfo
        Vk.zero Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST False
      viewportState = Vk.PipelineViewportStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.viewportCount = 1, Vk.viewports = V.empty
        , Vk.scissorCount = 1, Vk.scissors = V.empty }
      rasterizer = Vk.PipelineRasterizationStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.depthClampEnable = False, Vk.rasterizerDiscardEnable = False
        , Vk.polygonMode = Vk.POLYGON_MODE_FILL, Vk.lineWidth = 1.0
        , Vk.cullMode = Vk.zero, Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
        , Vk.depthBiasEnable = False, Vk.depthBiasConstantFactor = 0
        , Vk.depthBiasClamp = 0, Vk.depthBiasSlopeFactor = 0 }
      multisampling = Vk.PipelineMultisampleStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.sampleShadingEnable = False, Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.minSampleShading = 1, Vk.sampleMask = V.empty
        , Vk.alphaToCoverageEnable = False, Vk.alphaToOneEnable = False }
      -- Alpha blending enabled for HUD transparency
      colorBlendAttachment = Vk.PipelineColorBlendAttachmentState
        { Vk.colorWriteMask = Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT
                            .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT
        , Vk.blendEnable = True
        , Vk.srcColorBlendFactor = Vk.BLEND_FACTOR_SRC_ALPHA
        , Vk.dstColorBlendFactor = Vk.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        , Vk.colorBlendOp = Vk.BLEND_OP_ADD
        , Vk.srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
        , Vk.dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
        , Vk.alphaBlendOp = Vk.BLEND_OP_ADD }
      colorBlending = Vk.PipelineColorBlendStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.logicOpEnable = False, Vk.logicOp = Vk.LOGIC_OP_COPY
        , Vk.attachmentCount = 1, Vk.attachments = V.singleton colorBlendAttachment
        , Vk.blendConstants = (0, 0, 0, 0) }
      -- No depth test for HUD
      depthStencil = Vk.PipelineDepthStencilStateCreateInfo
        { Vk.flags = Vk.zero
        , Vk.depthTestEnable = False, Vk.depthWriteEnable = False
        , Vk.depthCompareOp = Vk.COMPARE_OP_ALWAYS
        , Vk.depthBoundsTestEnable = False, Vk.stencilTestEnable = False
        , Vk.front = Vk.zero, Vk.back = Vk.zero
        , Vk.minDepthBounds = 0, Vk.maxDepthBounds = 1 }
      dynamicState = Vk.PipelineDynamicStateCreateInfo
        { Vk.flags = Vk.zero
        , Vk.dynamicStates = V.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR] }

  let layoutInfo = Vk.PipelineLayoutCreateInfo Vk.zero V.empty V.empty
  pipelineLayout <- Vk.createPipelineLayout device layoutInfo Nothing

  let pipelineInfo = Vk.GraphicsPipelineCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero, Vk.stageCount = 2
        , Vk.stages = V.fromList [Vk.SomeStruct vertStage, Vk.SomeStruct fragStage]
        , Vk.vertexInputState = Just (Vk.SomeStruct vertexInput)
        , Vk.inputAssemblyState = Just inputAssembly
        , Vk.tessellationState = Nothing
        , Vk.viewportState = Just (Vk.SomeStruct viewportState)
        , Vk.rasterizationState = Just (Vk.SomeStruct rasterizer)
        , Vk.multisampleState = Just (Vk.SomeStruct multisampling)
        , Vk.depthStencilState = Just depthStencil
        , Vk.colorBlendState = Just (Vk.SomeStruct colorBlending)
        , Vk.dynamicState = Just dynamicState
        , Vk.layout = pipelineLayout
        , Vk.renderPass = renderPass, Vk.subpass = 0
        , Vk.basePipelineHandle = Vk.zero, Vk.basePipelineIndex = -1 }

  (_, pipelines) <- Vk.createGraphicsPipelines device Vk.zero (V.singleton (Vk.SomeStruct pipelineInfo)) Nothing
  let pipeline = V.head pipelines

  Vk.destroyShaderModule device vertModule Nothing
  Vk.destroyShaderModule device fragModule Nothing

  pure $ HudPipeline pipelineLayout pipeline

-- | Destroy HUD pipeline resources
destroyHudPipeline :: Vk.Device -> HudPipeline -> IO ()
destroyHudPipeline device hp = do
  Vk.destroyPipeline device (hpPipeline hp) Nothing
  Vk.destroyPipelineLayout device (hpPipelineLayout hp) Nothing

-- | Create entity billboard pipeline: 3D colored vertices, depth test on, alpha blending, no face culling
createEntityPipeline :: Vk.Device -> Vk.RenderPass -> Vk.DescriptorSetLayout -> FilePath -> FilePath -> IO EntityPipeline
createEntityPipeline device renderPass dsLayout vertPath fragPath = do
  vertCode <- BS.readFile vertPath
  fragCode <- BS.readFile fragPath
  vertModule <- createShaderModule device vertCode
  fragModule <- createShaderModule device fragCode

  let vertStage = Vk.PipelineShaderStageCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.module' = vertModule, Vk.name = "main"
        , Vk.specializationInfo = Nothing }
      fragStage = Vk.PipelineShaderStageCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.module' = fragModule, Vk.name = "main"
        , Vk.specializationInfo = Nothing }

  -- Vertex input: vec3 position (12 bytes) + vec4 color (16 bytes) = 28 bytes
  let bindingDesc = Vk.VertexInputBindingDescription 0 28 Vk.VERTEX_INPUT_RATE_VERTEX
      attrDescs = V.fromList
        [ Vk.VertexInputAttributeDescription 0 0 Vk.FORMAT_R32G32B32_SFLOAT 0       -- position: vec3
        , Vk.VertexInputAttributeDescription 1 0 Vk.FORMAT_R32G32B32A32_SFLOAT 12    -- color: vec4
        ]
      vertexInput = Vk.PipelineVertexInputStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.vertexBindingDescriptions = V.singleton bindingDesc
        , Vk.vertexAttributeDescriptions = attrDescs }
      inputAssembly = Vk.PipelineInputAssemblyStateCreateInfo
        Vk.zero Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST False
      viewportState = Vk.PipelineViewportStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.viewportCount = 1, Vk.viewports = V.empty
        , Vk.scissorCount = 1, Vk.scissors = V.empty }
      -- No face culling for billboards (visible from all angles)
      rasterizer = Vk.PipelineRasterizationStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.depthClampEnable = False, Vk.rasterizerDiscardEnable = False
        , Vk.polygonMode = Vk.POLYGON_MODE_FILL, Vk.lineWidth = 1.0
        , Vk.cullMode = Vk.zero, Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
        , Vk.depthBiasEnable = False, Vk.depthBiasConstantFactor = 0
        , Vk.depthBiasClamp = 0, Vk.depthBiasSlopeFactor = 0 }
      multisampling = Vk.PipelineMultisampleStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.sampleShadingEnable = False, Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.minSampleShading = 1, Vk.sampleMask = V.empty
        , Vk.alphaToCoverageEnable = False, Vk.alphaToOneEnable = False }
      -- Alpha blending for entity transparency
      colorBlendAttachment = Vk.PipelineColorBlendAttachmentState
        { Vk.colorWriteMask = Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT
                            .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT
        , Vk.blendEnable = True
        , Vk.srcColorBlendFactor = Vk.BLEND_FACTOR_SRC_ALPHA
        , Vk.dstColorBlendFactor = Vk.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        , Vk.colorBlendOp = Vk.BLEND_OP_ADD
        , Vk.srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
        , Vk.dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
        , Vk.alphaBlendOp = Vk.BLEND_OP_ADD }
      colorBlending = Vk.PipelineColorBlendStateCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero
        , Vk.logicOpEnable = False, Vk.logicOp = Vk.LOGIC_OP_COPY
        , Vk.attachmentCount = 1, Vk.attachments = V.singleton colorBlendAttachment
        , Vk.blendConstants = (0, 0, 0, 0) }
      -- Depth test ON, depth write ON for proper occlusion with terrain
      depthStencil = Vk.PipelineDepthStencilStateCreateInfo
        { Vk.flags = Vk.zero
        , Vk.depthTestEnable = True, Vk.depthWriteEnable = True
        , Vk.depthCompareOp = Vk.COMPARE_OP_LESS
        , Vk.depthBoundsTestEnable = False, Vk.stencilTestEnable = False
        , Vk.front = Vk.zero, Vk.back = Vk.zero
        , Vk.minDepthBounds = 0, Vk.maxDepthBounds = 1 }
      dynamicState = Vk.PipelineDynamicStateCreateInfo
        { Vk.flags = Vk.zero
        , Vk.dynamicStates = V.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR] }

  -- Layout with descriptor set for UBO (same as block pipeline, reuses the UBO)
  let layoutInfo = Vk.PipelineLayoutCreateInfo Vk.zero (V.singleton dsLayout) V.empty
  pipelineLayout <- Vk.createPipelineLayout device layoutInfo Nothing

  let pipelineInfo = Vk.GraphicsPipelineCreateInfo
        { Vk.next = (), Vk.flags = Vk.zero, Vk.stageCount = 2
        , Vk.stages = V.fromList [Vk.SomeStruct vertStage, Vk.SomeStruct fragStage]
        , Vk.vertexInputState = Just (Vk.SomeStruct vertexInput)
        , Vk.inputAssemblyState = Just inputAssembly
        , Vk.tessellationState = Nothing
        , Vk.viewportState = Just (Vk.SomeStruct viewportState)
        , Vk.rasterizationState = Just (Vk.SomeStruct rasterizer)
        , Vk.multisampleState = Just (Vk.SomeStruct multisampling)
        , Vk.depthStencilState = Just depthStencil
        , Vk.colorBlendState = Just (Vk.SomeStruct colorBlending)
        , Vk.dynamicState = Just dynamicState
        , Vk.layout = pipelineLayout
        , Vk.renderPass = renderPass, Vk.subpass = 0
        , Vk.basePipelineHandle = Vk.zero, Vk.basePipelineIndex = -1 }

  (_, pipelines) <- Vk.createGraphicsPipelines device Vk.zero (V.singleton (Vk.SomeStruct pipelineInfo)) Nothing
  let pipeline = V.head pipelines

  Vk.destroyShaderModule device vertModule Nothing
  Vk.destroyShaderModule device fragModule Nothing

  pure $ EntityPipeline pipelineLayout pipeline

-- | Destroy entity pipeline resources
destroyEntityPipeline :: Vk.Device -> EntityPipeline -> IO ()
destroyEntityPipeline device ep = do
  Vk.destroyPipeline device (epPipeline ep) Nothing
  Vk.destroyPipelineLayout device (epPipelineLayout ep) Nothing

-- | Create framebuffers for each swapchain image view (color + depth)
createFramebuffers :: Vk.Device -> Vk.RenderPass -> SwapchainContext -> Vk.ImageView -> IO (Vector Vk.Framebuffer)
createFramebuffers device renderPass sc depthView =
  V.mapM createFB (scImageViews sc)
  where
    createFB imageView = do
      let Vk.Extent2D{width = extW, height = extH} = scExtent sc
      let createInfo = Vk.FramebufferCreateInfo
            { Vk.next        = ()
            , Vk.flags       = Vk.zero
            , Vk.renderPass  = renderPass
            , Vk.attachments = V.fromList [imageView, depthView]
            , Vk.width       = extW
            , Vk.height      = extH
            , Vk.layers      = 1
            }
      Vk.createFramebuffer device createInfo Nothing

-- | Destroy framebuffers
destroyFramebuffers :: Vk.Device -> Vector Vk.Framebuffer -> IO ()
destroyFramebuffers device = V.mapM_ (\fb -> Vk.destroyFramebuffer device fb Nothing)

-- | Find a supported depth format
findDepthFormat :: Vk.PhysicalDevice -> IO Vk.Format
findDepthFormat physDevice = do
  let candidates = [Vk.FORMAT_D32_SFLOAT, Vk.FORMAT_D32_SFLOAT_S8_UINT, Vk.FORMAT_D24_UNORM_S8_UINT]
  findSupportedFormat physDevice candidates Vk.IMAGE_TILING_OPTIMAL Vk.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

findSupportedFormat :: Vk.PhysicalDevice -> [Vk.Format] -> Vk.ImageTiling -> Vk.FormatFeatureFlags -> IO Vk.Format
findSupportedFormat _ [] _ _ = throwIO $ userError "Failed to find supported depth format"
findSupportedFormat physDevice (fmt:rest) tiling features = do
  props <- Vk.getPhysicalDeviceFormatProperties physDevice fmt
  let Vk.FormatProperties linearFeats optimalFeats _bufFeats = props
      supported = case tiling of
        Vk.IMAGE_TILING_LINEAR  -> linearFeats .&. features == features
        Vk.IMAGE_TILING_OPTIMAL -> optimalFeats .&. features == features
        _                       -> False
  if supported then pure fmt else findSupportedFormat physDevice rest tiling features

-- | Create depth buffer image, memory, and image view
createDepthResources :: Vk.PhysicalDevice -> Vk.Device -> Vk.Extent2D -> IO DepthResources
createDepthResources physDevice device extent = do
  depthFormat <- findDepthFormat physDevice
  let Vk.Extent2D{width = extW, height = extH} = extent

  -- Create depth image
  let imageInfo = Vk.ImageCreateInfo
        { Vk.next          = ()
        , Vk.flags         = Vk.zero
        , Vk.imageType     = Vk.IMAGE_TYPE_2D
        , Vk.format        = depthFormat
        , Vk.extent        = Vk.Extent3D extW extH 1
        , Vk.mipLevels     = 1
        , Vk.arrayLayers   = 1
        , Vk.samples       = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling        = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage         = Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
        , Vk.sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.queueFamilyIndices = V.empty
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
  image <- Vk.createImage device imageInfo Nothing

  -- Allocate memory
  memReqs <- Vk.getImageMemoryRequirements device image
  memProps <- Vk.getPhysicalDeviceMemoryProperties physDevice
  let Vk.MemoryRequirements{memoryTypeBits = reqMemTypeBits, size = reqSize} = memReqs
      memTypeIdx = findMemTypeIdx reqMemTypeBits Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT memProps
  let allocInfo = Vk.MemoryAllocateInfo
        { Vk.next            = ()
        , Vk.allocationSize  = reqSize
        , Vk.memoryTypeIndex = memTypeIdx
        }
  memory <- Vk.allocateMemory device allocInfo Nothing
  Vk.bindImageMemory device image memory 0

  -- Create image view
  let aspectMask = Vk.IMAGE_ASPECT_DEPTH_BIT
  let viewInfo = Vk.ImageViewCreateInfo
        { Vk.next             = ()
        , Vk.flags            = Vk.zero
        , Vk.image            = image
        , Vk.viewType         = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format           = depthFormat
        , Vk.components       = Vk.ComponentMapping
            Vk.COMPONENT_SWIZZLE_IDENTITY Vk.COMPONENT_SWIZZLE_IDENTITY
            Vk.COMPONENT_SWIZZLE_IDENTITY Vk.COMPONENT_SWIZZLE_IDENTITY
        , Vk.subresourceRange = Vk.ImageSubresourceRange aspectMask 0 1 0 1
        }
  imageView <- Vk.createImageView device viewInfo Nothing

  pure $ DepthResources image memory imageView depthFormat

-- | Destroy depth buffer resources
destroyDepthResources :: Vk.Device -> DepthResources -> IO ()
destroyDepthResources device dr = do
  Vk.destroyImageView device (drImageView dr) Nothing
  Vk.destroyImage device (drImage dr) Nothing
  Vk.freeMemory device (drMemory dr) Nothing

-- | Find memory type index (local helper)
findMemTypeIdx :: Word32 -> Vk.MemoryPropertyFlags -> Vk.PhysicalDeviceMemoryProperties -> Word32
findMemTypeIdx typeFilter props memProps =
  let memTypes = Vk.memoryTypes memProps
      count = Vk.memoryTypeCount memProps
      go i
        | i >= fromIntegral count = error "Failed to find suitable memory type for depth buffer"
        | typeFilter .&. (1 `shiftL'` fromIntegral i) /= 0
          && (Vk.propertyFlags (memTypes V.! fromIntegral i) .&. props) == props
          = i
        | otherwise = go (i + 1)
  in go 0
  where
    shiftL' :: Word32 -> Int -> Word32
    shiftL' = shiftL
