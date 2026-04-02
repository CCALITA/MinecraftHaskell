# Fly Toggle Reliability Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the `F` key toggle flight reliably by preserving the queued toggle across render frames until a physics tick consumes it.

**Architecture:** Add a small helper in `Game.Player` that resets frame-local input while optionally preserving a queued fly toggle when no physics tick ran. Wire `app/Main.hs` to use that helper after `playerLoop`, and add regression tests that cover both the queueing behavior and one-shot flight toggling.

**Tech Stack:** Haskell, Cabal, Hspec, GLFW input loop

---

### Task 1: Add failing regression tests for queued fly toggles

**Files:**
- Modify: `test/Spec.hs`
- Modify: `src/Game/Player.hs`

- [ ] **Step 1: Write the failing test**

```haskell
import Game.Player
import Linear (V2(..), V3(..))
```

```haskell
main :: IO ()
main = hspec $ do
  blockSpec
  chunkSpec
  noiseSpec
  inventorySpec
  craftingSpec
  worldCoordSpec
  playerInputSpec
```

```haskell
playerInputSpec :: Spec
playerInputSpec = describe "Game.Player input queue" $ do
  let airQuery _ _ _ = pure False
      queuedToggle = noInput { piToggleFly = True, piMouseDX = 4, piMouseDY = -2 }

  it "preserves fly toggle when no physics tick ran" $ do
    endFrameInput False queuedToggle `shouldBe` noInput { piToggleFly = True }

  it "clears fly toggle after a physics tick runs" $ do
    endFrameInput True queuedToggle `shouldBe` noInput

  it "applies a queued fly toggle exactly once" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = True }

    player1 <- updatePlayer 0 queuedToggle airQuery player0
    player2 <- updatePlayer 0 (endFrameInput True queuedToggle) airQuery player1

    plFlying player1 `shouldBe` False
    plFlying player2 `shouldBe` False
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='--match "Game.Player input queue"'`

Expected: FAIL because `endFrameInput` is not exported or defined yet.

- [ ] **Step 3: Write minimal implementation**

In `src/Game/Player.hs`, export `endFrameInput` and add:

```haskell
  , endFrameInput
```

```haskell
-- | Clear frame-local input, preserving queued one-shot actions until physics consumes them.
endFrameInput :: Bool -> PlayerInput -> PlayerInput
endFrameInput physicsTickRan input =
  noInput { piToggleFly = not physicsTickRan && piToggleFly input }
```

- [ ] **Step 4: Run test to verify the new tests pass**

Run: `cabal test --test-options='--match "Game.Player input queue"'`

Expected: PASS for all `Game.Player input queue` examples.

### Task 2: Wire the main loop to preserve queued toggles until consumption

**Files:**
- Modify: `app/Main.hs`
- Modify: `src/Game/Player.hs`
- Test: `test/Spec.hs`

- [ ] **Step 1: Update the end-of-frame input reset**

In `app/Main.hs`, replace:

```haskell
            -- Reset mouse deltas and toggle flags
            writeIORef inputRef noInput
```

with:

```haskell
            let physicsTickRan = accum' >= tickRate

            -- Clear per-frame mouse movement, but keep one-shot toggles queued
            -- until a physics tick has consumed them.
            writeIORef inputRef (endFrameInput physicsTickRan baseInput)
```

- [ ] **Step 2: Run the targeted regression tests**

Run: `cabal test --test-options='--match "Game.Player input queue"'`

Expected: PASS for all targeted examples.

- [ ] **Step 3: Run the full test suite**

Run: `cabal test`

Expected: PASS with the existing suite still green.

- [ ] **Step 4: Commit the fix**

```bash
git add app/Main.hs src/Game/Player.hs test/Spec.hs docs/superpowers/plans/2026-04-02-fly-toggle-reliability.md
git commit -m "fix: preserve queued fly toggle until physics tick"
```
