module Engine.BitmapFont
  ( renderText
  , renderTextCentered
  , charWidth
  , charHeight
  ) where

-- | Simple 3x5 bitmap font rendered as colored quads.
--   Each character is a list of (row, col) positions where a pixel is drawn.
--   Returns a list of floats for the HUD vertex buffer (vec2 pos + vec4 color = 6 floats per vertex).

-- | Character pixel size in NDC
charWidth, charHeight, charSpacing :: Float
charWidth   = 0.008   -- width of one pixel
charHeight  = 0.012   -- height of one pixel
charSpacing = 0.032   -- total width per character (3 pixels + gap)

-- | Render a string at position (x, y) in NDC with given color.
--   Returns vertex data as [Float] for the HUD pipeline.
renderText :: Float -> Float -> Float -> (Float, Float, Float, Float) -> String -> [Float]
renderText x y scale color str = concatMap renderChar (zip [0..] str)
  where
    sw = charWidth * scale
    sh = charHeight * scale
    sp = charSpacing * scale
    renderChar (i, ch) =
      let cx = x + fromIntegral (i :: Int) * sp
          pixels = charPixels ch
      in concatMap (\(row, col) ->
           let px = cx + fromIntegral col * sw
               py = y + fromIntegral row * sh
               (r, g, b, a) = color
           in [px, py, r, g, b, a
              ,px + sw, py, r, g, b, a
              ,px + sw, py + sh, r, g, b, a
              ,px, py, r, g, b, a
              ,px + sw, py + sh, r, g, b, a
              ,px, py + sh, r, g, b, a]
         ) pixels

-- | Render text centered horizontally at y position
renderTextCentered :: Float -> Float -> (Float, Float, Float, Float) -> String -> [Float]
renderTextCentered y scale color str =
  let totalW = fromIntegral (length str) * charSpacing * scale
      x = -(totalW / 2)
  in renderText x y scale color str

-- | 3x5 pixel bitmap for each character (row 0 = top)
charPixels :: Char -> [(Int, Int)]
charPixels 'A' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2),(3,0),(3,2),(4,0),(4,2)]
charPixels 'B' = [(0,0),(0,1),(1,0),(1,2),(2,0),(2,1),(3,0),(3,2),(4,0),(4,1)]
charPixels 'C' = [(0,0),(0,1),(0,2),(1,0),(2,0),(3,0),(4,0),(4,1),(4,2)]
charPixels 'D' = [(0,0),(0,1),(1,0),(1,2),(2,0),(2,2),(3,0),(3,2),(4,0),(4,1)]
charPixels 'E' = [(0,0),(0,1),(0,2),(1,0),(2,0),(2,1),(3,0),(4,0),(4,1),(4,2)]
charPixels 'F' = [(0,0),(0,1),(0,2),(1,0),(2,0),(2,1),(3,0),(4,0)]
charPixels 'G' = [(0,0),(0,1),(0,2),(1,0),(2,0),(2,1),(2,2),(3,0),(3,2),(4,0),(4,1),(4,2)]
charPixels 'H' = [(0,0),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2),(3,0),(3,2),(4,0),(4,2)]
charPixels 'I' = [(0,0),(0,1),(0,2),(1,1),(2,1),(3,1),(4,0),(4,1),(4,2)]
charPixels 'J' = [(0,2),(1,2),(2,2),(3,0),(3,2),(4,0),(4,1),(4,2)]
charPixels 'K' = [(0,0),(0,2),(1,0),(1,2),(2,0),(2,1),(3,0),(3,2),(4,0),(4,2)]
charPixels 'L' = [(0,0),(1,0),(2,0),(3,0),(4,0),(4,1),(4,2)]
charPixels 'M' = [(0,0),(0,2),(1,0),(1,1),(1,2),(2,0),(2,2),(3,0),(3,2),(4,0),(4,2)]
charPixels 'N' = [(0,0),(0,2),(1,0),(1,1),(1,2),(2,0),(2,2),(3,0),(3,2),(4,0),(4,2)]
charPixels 'O' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,2),(3,0),(3,2),(4,0),(4,1),(4,2)]
charPixels 'P' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2),(3,0),(4,0)]
charPixels 'Q' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,2),(3,0),(3,1),(4,0),(4,1),(4,2)]
charPixels 'R' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(3,0),(3,2),(4,0),(4,2)]
charPixels 'S' = [(0,0),(0,1),(0,2),(1,0),(2,0),(2,1),(2,2),(3,2),(4,0),(4,1),(4,2)]
charPixels 'T' = [(0,0),(0,1),(0,2),(1,1),(2,1),(3,1),(4,1)]
charPixels 'U' = [(0,0),(0,2),(1,0),(1,2),(2,0),(2,2),(3,0),(3,2),(4,0),(4,1),(4,2)]
charPixels 'V' = [(0,0),(0,2),(1,0),(1,2),(2,0),(2,2),(3,0),(3,2),(4,1)]
charPixels 'W' = [(0,0),(0,2),(1,0),(1,2),(2,0),(2,2),(3,0),(3,1),(3,2),(4,0),(4,2)]
charPixels 'X' = [(0,0),(0,2),(1,0),(1,2),(2,1),(3,0),(3,2),(4,0),(4,2)]
charPixels 'Y' = [(0,0),(0,2),(1,0),(1,2),(2,1),(3,1),(4,1)]
charPixels 'Z' = [(0,0),(0,1),(0,2),(1,2),(2,1),(3,0),(4,0),(4,1),(4,2)]
charPixels '0' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,2),(3,0),(3,2),(4,0),(4,1),(4,2)]
charPixels '1' = [(0,1),(1,0),(1,1),(2,1),(3,1),(4,0),(4,1),(4,2)]
charPixels '2' = [(0,0),(0,1),(0,2),(1,2),(2,0),(2,1),(2,2),(3,0),(4,0),(4,1),(4,2)]
charPixels '3' = [(0,0),(0,1),(0,2),(1,2),(2,0),(2,1),(2,2),(3,2),(4,0),(4,1),(4,2)]
charPixels '4' = [(0,0),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2),(3,2),(4,2)]
charPixels '5' = [(0,0),(0,1),(0,2),(1,0),(2,0),(2,1),(2,2),(3,2),(4,0),(4,1),(4,2)]
charPixels '6' = [(0,0),(0,1),(0,2),(1,0),(2,0),(2,1),(2,2),(3,0),(3,2),(4,0),(4,1),(4,2)]
charPixels '7' = [(0,0),(0,1),(0,2),(1,2),(2,2),(3,2),(4,2)]
charPixels '8' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2),(3,0),(3,2),(4,0),(4,1),(4,2)]
charPixels '9' = [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2),(3,2),(4,0),(4,1),(4,2)]
charPixels ' ' = []
charPixels ':' = [(1,1),(3,1)]
charPixels '.' = [(4,1)]
charPixels '-' = [(2,0),(2,1),(2,2)]
charPixels '/' = [(4,0),(3,1),(2,1),(1,2)]
charPixels '(' = [(0,1),(1,0),(2,0),(3,0),(4,1)]
charPixels ')' = [(0,1),(1,2),(2,2),(3,2),(4,1)]
charPixels _   = [(0,0),(0,2),(4,0),(4,2)]  -- corners for unknown chars
