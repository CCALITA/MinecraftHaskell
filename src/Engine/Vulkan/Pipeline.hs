module Engine.Vulkan.Pipeline
  ( PipelineContext(..)
  , createRenderPass
  , createGraphicsPipeline
  , destroyPipelineContext
  , createFramebuffers
  , destroyFramebuffers
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

import Engine.Vulkan.Swapchain (SwapchainContext(..))

import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)
import Data.Bits ((.|.))

-- | Pipeline and render pass resources
data PipelineContext = PipelineContext
  { pcRenderPass     :: !Vk.RenderPass
  , pcPipelineLayout :: !Vk.PipelineLayout
  , pcPipeline       :: !Vk.Pipeline
  } deriving stock (Show)

-- | Create render pass with single color attachment
createRenderPass :: Vk.Device -> Vk.Format -> IO Vk.RenderPass
createRenderPass device format = do
  let colorAttachment = Vk.AttachmentDescription
        { Vk.flags          = Vk.zero
        , Vk.format         = format
        , Vk.samples        = Vk.SAMPLE_COUNT_1_BIT
        , Vk.loadOp         = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp        = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout    = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        }

  let colorRef = Vk.AttachmentReference
        { Vk.attachment = 0
        , Vk.layout     = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }

  let subpass = Vk.SubpassDescription
        { Vk.flags                   = Vk.zero
        , Vk.pipelineBindPoint       = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.inputAttachments        = V.empty
        , Vk.colorAttachments        = V.singleton colorRef
        , Vk.resolveAttachments      = V.empty
        , Vk.depthStencilAttachment  = Nothing
        , Vk.preserveAttachments     = V.empty
        }

  let dependency = Vk.SubpassDependency
        { Vk.srcSubpass    = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass    = 0
        , Vk.srcStageMask  = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.srcAccessMask = Vk.zero
        , Vk.dstStageMask  = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        , Vk.dependencyFlags = Vk.zero
        }

  let createInfo = Vk.RenderPassCreateInfo
        { Vk.next         = ()
        , Vk.flags        = Vk.zero
        , Vk.attachments  = V.singleton colorAttachment
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

-- | Create graphics pipeline for triangle rendering
createGraphicsPipeline
  :: Vk.Device
  -> Vk.RenderPass
  -> Vk.Extent2D
  -> FilePath    -- ^ Vertex shader SPIR-V path
  -> FilePath    -- ^ Fragment shader SPIR-V path
  -> IO PipelineContext
createGraphicsPipeline device renderPass extent vertPath fragPath = do
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

  -- Vertex input: no buffers for hardcoded triangle
  let vertexInputInfo = Vk.PipelineVertexInputStateCreateInfo
        { Vk.next                           = ()
        , Vk.flags                          = Vk.zero
        , Vk.vertexBindingDescriptions      = V.empty
        , Vk.vertexAttributeDescriptions    = V.empty
        }

  let inputAssembly = Vk.PipelineInputAssemblyStateCreateInfo
        { Vk.flags                 = Vk.zero
        , Vk.topology              = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        , Vk.primitiveRestartEnable = False
        }

  let Vk.Extent2D{width = extW, height = extH} = extent

  let viewport = Vk.Viewport
        { Vk.x        = 0
        , Vk.y        = 0
        , Vk.width    = fromIntegral extW
        , Vk.height   = fromIntegral extH
        , Vk.minDepth = 0
        , Vk.maxDepth = 1
        }

  let scissor = Vk.Rect2D
        { Vk.offset = Vk.Offset2D 0 0
        , Vk.extent = extent
        }

  let viewportState = Vk.PipelineViewportStateCreateInfo
        { Vk.next      = ()
        , Vk.flags     = Vk.zero
        , Vk.viewportCount = 1
        , Vk.viewports = V.singleton viewport
        , Vk.scissorCount  = 1
        , Vk.scissors  = V.singleton scissor
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

  -- Pipeline layout (empty for now — no uniforms yet)
  let layoutInfo = Vk.PipelineLayoutCreateInfo
        { Vk.flags              = Vk.zero
        , Vk.setLayouts         = V.empty
        , Vk.pushConstantRanges = V.empty
        }

  pipelineLayout <- Vk.createPipelineLayout device layoutInfo Nothing

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
        , Vk.depthStencilState  = Nothing
        , Vk.colorBlendState    = Just (Vk.SomeStruct colorBlending)
        , Vk.dynamicState       = Nothing
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

-- | Create framebuffers for each swapchain image view
createFramebuffers :: Vk.Device -> Vk.RenderPass -> SwapchainContext -> IO (Vector Vk.Framebuffer)
createFramebuffers device renderPass sc =
  V.mapM createFB (scImageViews sc)
  where
    createFB imageView = do
      let Vk.Extent2D{width = extW, height = extH} = scExtent sc
      let createInfo = Vk.FramebufferCreateInfo
            { Vk.next        = ()
            , Vk.flags       = Vk.zero
            , Vk.renderPass  = renderPass
            , Vk.attachments = V.singleton imageView
            , Vk.width       = extW
            , Vk.height      = extH
            , Vk.layers      = 1
            }
      Vk.createFramebuffer device createInfo Nothing

-- | Destroy framebuffers
destroyFramebuffers :: Vk.Device -> Vector Vk.Framebuffer -> IO ()
destroyFramebuffers device = V.mapM_ (\fb -> Vk.destroyFramebuffer device fb Nothing)
