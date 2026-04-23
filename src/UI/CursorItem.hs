module UI.CursorItem
  ( buildCursorItemVerts
  ) where

import Game.Inventory (ItemStack(..))
import Game.ItemDisplay (itemMiniIcon)
import Engine.BitmapFont (renderText)

-- | Build HUD vertices for the cursor item that follows the mouse position.
--   When 'cursorItem' is 'Just (ItemStack item count)', renders the item's
--   mini-icon centered at (mouseX, mouseY), plus a count label when count > 1.
--   Rendered LAST so it draws on top of all other inventory UI elements.
buildCursorItemVerts :: Float -> Float -> Maybe ItemStack -> [Float]
buildCursorItemVerts _ _ Nothing = []
buildCursorItemVerts mouseX mouseY (Just (ItemStack item cnt)) =
  let colors = itemMiniIcon item
      sw = 0.08 :: Float
      sh = 0.08 :: Float
      x = mouseX - sw / 2
      y = mouseY - sh / 2
      pixW = sw / 3
      pixH = sh / 3
      iconVerts = concatMap (\(row, col, clr) ->
           quad (x + fromIntegral col * pixW) (y + fromIntegral row * pixH)
                (x + fromIntegral (col + 1) * pixW) (y + fromIntegral (row + 1) * pixH) clr
           ) colors
      countText = if cnt > 1
        then renderText (x + sw - 0.025) (y + sh - 0.02) 0.6 (1, 1, 1, 1) (show cnt)
        else []
  in iconVerts ++ countText
  where
    quad x0 y0 x1 y1 (r, g, b, a) =
      [x0, y0, r, g, b, a,  x1, y0, r, g, b, a,  x1, y1, r, g, b, a
      ,x0, y0, r, g, b, a,  x1, y1, r, g, b, a,  x0, y1, r, g, b, a]
