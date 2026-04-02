# Fly Toggle Reliability Design

## Goal

Make the `F` key reliably toggle player flight mode during gameplay without changing the default saved/player startup state.

## Context

The current input flow mixes render-frame input collection with fixed-timestep physics updates:

- GLFW key callbacks write one-shot intent into `inputRef`.
- The main loop rebuilds `PlayerInput` every render frame.
- `playerLoop` only consumes input on 20 Hz physics ticks.
- `inputRef` is reset to `noInput` after every render frame.

This works for level-triggered inputs such as movement keys because those are rebuilt from current keyboard state each frame. It fails for edge-triggered actions such as fly toggle because the action can be cleared before any physics tick consumes it.

## Root Cause

`piToggleFly` is treated as a per-frame transient bit, but it semantically represents a queued one-shot action.

When the user presses `F`, the GLFW callback sets `piToggleFly = True`. If the current render frame does not execute a physics tick because the accumulator is still below `tickRate`, the main loop resets `inputRef` back to `noInput` and erases the pending toggle. As a result, the player never sees the action in `updatePlayer`.

## Chosen Approach

Keep the fly-state transition inside `Game.Player.updatePlayer`, but change the input lifetime of `piToggleFly` so it is latched until consumed by a physics tick.

This preserves the existing ownership boundary:

- `app/Main.hs` gathers and stores user input.
- `playerLoop` decides when physics ticks happen.
- `Game.Player.updatePlayer` remains the single place that mutates `plFlying`.

## Behavioral Requirements

1. Pressing `F` queues exactly one fly-toggle action.
2. A queued toggle survives across render frames until the next physics tick consumes it.
3. One queued toggle flips `plFlying` exactly once.
4. After consumption, the toggle bit is cleared so the same key press does not flip repeatedly on later ticks.
5. Continuous inputs such as movement and mouse look keep their current behavior.

## Implementation Shape

### Input Handling

In `app/Main.hs`, keep using the GLFW key callback to set `piToggleFly = True`.

Change the per-frame input reset so it only clears transient mouse deltas and other frame-local fields, while preserving a pending `piToggleFly` flag until `playerLoop` actually consumes it.

### Physics Consumption

In `playerLoop`, continue clearing `piToggleFly` only after a physics tick runs. This makes tick consumption the boundary that drains the queued one-shot action.

No direct player mutation should be added to the GLFW callback.

### Player Update

In `src/Game/Player.hs`, keep the current `flying = if piToggleFly input then not (plFlying player) else plFlying player` behavior. That logic is already correct once the toggle input survives long enough to reach a tick.

## Testing Strategy

Add regression coverage for the player input/update path:

1. Verify that a queued fly toggle flips `plFlying` on the next `updatePlayer` call.
2. Verify that the same queued action does not flip twice after the toggle bit is cleared.
3. Verify that non-toggle input still leaves `plFlying` unchanged.

The tests should use a simple non-solid `BlockQuery` so they isolate toggle behavior from collision and gravity.

## Out of Scope

This design does not change:

- the player's default startup flight state
- save/load behavior for `plFlying`
- gravity defaults
- horizontal mouse-look direction
- broader world or entity physics

These can be handled as separate follow-up changes.
