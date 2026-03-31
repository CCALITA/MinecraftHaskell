module UI.Text
  ( BitmapFont(..)
  , GlyphInfo(..)
  , TextLayout(..)
  , TextAlign(..)
  , defaultFont
  , layoutText
  , layoutMultiline
  , textWidth
  , textHeight
  ) where

import Linear (V2(..))
import qualified Data.Map.Strict as Map
import Data.Char (ord)

-- | Information about a single glyph in the font atlas
data GlyphInfo = GlyphInfo
  { giAtlasPos  :: !(V2 Int)    -- position in atlas (pixels)
  , giSize      :: !(V2 Int)    -- glyph size (pixels)
  , giAdvance   :: !Int         -- horizontal advance after this glyph
  } deriving stock (Show, Eq)

-- | Bitmap font definition
data BitmapFont = BitmapFont
  { bfGlyphs     :: !(Map.Map Char GlyphInfo)
  , bfLineHeight :: !Int        -- pixels per line
  , bfBaseline   :: !Int        -- pixels from top to baseline
  , bfAtlasSize  :: !(V2 Int)  -- atlas texture dimensions
  , bfCharWidth  :: !Int        -- monospace character width
  } deriving stock (Show, Eq)

-- | Text alignment
data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving stock (Show, Eq)

-- | Laid-out text: list of (character, screen position, atlas UV)
data TextLayout = TextLayout
  { tlChars :: ![(Char, V2 Float, V2 Float, V2 Float)]  -- char, screenPos, uvMin, uvMax
  , tlWidth :: !Float
  , tlHeight :: !Float
  } deriving stock (Show, Eq)

-- | Default monospace bitmap font (8x8 ASCII)
--   Assumes a 128x64 atlas with 16 chars per row, 8 rows
--   Characters 32 (space) through 127 (~)
defaultFont :: BitmapFont
defaultFont = BitmapFont
  { bfGlyphs     = Map.fromList
      [ (c, GlyphInfo
          { giAtlasPos = V2 ((col * 8)) ((row * 8))
          , giSize     = V2 8 8
          , giAdvance  = 8
          })
      | c <- [' '..'~']
      , let idx = ord c - 32
            col = idx `mod` 16
            row = idx `div` 16
      ]
  , bfLineHeight = 10    -- 8px glyph + 2px spacing
  , bfBaseline   = 8
  , bfAtlasSize  = V2 128 64
  , bfCharWidth  = 8
  }

-- | Layout a single line of text
layoutText :: BitmapFont -> String -> Float -> V2 Float -> TextAlign -> TextLayout
layoutText font text scale (V2 startX startY) align =
  let atlasW = fromIntegral $ v2x (bfAtlasSize font)
      atlasH = fromIntegral $ v2y (bfAtlasSize font)

      -- Calculate total width for alignment
      totalWidth = fromIntegral (length text * bfCharWidth font) * scale
      offsetX = case align of
        AlignLeft   -> 0
        AlignCenter -> -totalWidth / 2
        AlignRight  -> -totalWidth

      layoutChar (chars, x) c = case Map.lookup c (bfGlyphs font) of
        Nothing -> (chars, x + fromIntegral (bfCharWidth font) * scale)
        Just gi ->
          let V2 ax ay = giAtlasPos gi
              V2 gw gh = giSize gi
              screenPos = V2 x startY
              uvMin = V2 (fromIntegral ax / atlasW) (fromIntegral ay / atlasH)
              uvMax = V2 (fromIntegral (ax + gw) / atlasW) (fromIntegral (ay + gh) / atlasH)
              advance = fromIntegral (giAdvance gi) * scale
          in ((c, screenPos, uvMin, uvMax) : chars, x + advance)

      (chars, finalX) = foldl layoutChar ([], startX + offsetX) text
      width = finalX - (startX + offsetX)
      height = fromIntegral (bfLineHeight font) * scale

  in TextLayout
    { tlChars  = reverse chars
    , tlWidth  = width
    , tlHeight = height
    }

-- | Layout multiple lines of text
layoutMultiline :: BitmapFont -> [String] -> Float -> V2 Float -> TextAlign -> [TextLayout]
layoutMultiline font lines scale (V2 startX startY) align =
  let lineH = fromIntegral (bfLineHeight font) * scale
  in zipWith (\line idx ->
    layoutText font line scale (V2 startX (startY + fromIntegral idx * lineH)) align
    ) lines [0 :: Int ..]

-- | Calculate text width in pixels (before scaling)
textWidth :: BitmapFont -> String -> Int
textWidth font text = length text * bfCharWidth font

-- | Calculate text height for N lines (before scaling)
textHeight :: BitmapFont -> Int -> Int
textHeight font numLines = numLines * bfLineHeight font

-- | V2 component helpers
v2x :: V2 Int -> Int
v2x (V2 x _) = x

v2y :: V2 Int -> Int
v2y (V2 _ y) = y
