# heph-input

![license](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)

A type-safe, high-performance input action mapping system for games and interactive applications.

This library is part of the Hephaestus rendering system, but is sufficiently general-purpose for any application that needs semantic input handling.

`heph-input` lets you map low-level input events (keyboard, mouse, gamepad) to high-level application actions with compile-time type safety and runtime performance optimizations. Instead of checking if `SCANCODE_SPACE` is pressed, you check if the `Jump` action is active.

## What it gives you

- **Type-safe action mappings** - Actions can only be bound to compatible input sources. A `Button` action can't accidentally receive axis input.
- **Multiple input sources per action** - Bind keyboard, mouse, and gamepad inputs to the same action. Players can jump with Space, Gamepad A, or any combination.
- **Flexible input types** - Support for buttons (pressed/held/released), 1D axes (analog sticks, mouse), and 2D axes (dual-stick, WASD movement).
- **Efficient runtime performance** - O(1) lookups using bounded arrays, double-buffered state for delta queries, and aggressive inlining.
- **Rebindable controls** - Separate action definitions from input bindings, making remapping trivial.
- **Template Haskell generation** - Automatically derive safe `Actionlike` instances with compile-time guarantees.

## Quick Start

Define your actions as a GADT, derive the typeclass with Template Haskell, and create your bindings:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Heph.Input

-- 1. Define your game's actions
data GameAction (src :: ActionSource) where
  Jump   :: GameAction Button
  Sprint :: GameAction Button
  Move   :: GameAction Axis2D
  Look   :: GameAction Axis2D

-- 2. Generate the Actionlike instance
makeAction ''GameAction

-- 3. Create your input bindings
gameBindings :: [ActionMapping GameAction]
gameBindings =
  [ Jump   :=> [Key ScancodeSpace, GamepadButton ControllerButtonA]
  , Sprint :=> [Key ScancodeLShift, GamepadButton ControllerButtonB]
  , Move   :=> [DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD)
              , LeftStick 1.0 0.15  -- sensitivity 1.0, deadzone 0.15
              ]
  , Look   :=> [MouseMotion 0.5     -- sensitivity 0.5
              , RightStick 2.0 0.2
              ]
  ]

-- 4. Compile the action map (do this once at initialization)
actionMap :: ActionMap GameAction
actionMap = newActionMap gameBindings
```

### Integration Example (with apecs and SDL)

In your game loop, integrate the input system with your ECS and event handling:

```haskell
{-# LANGUAGE TypeFamilies #-}

import Heph.Input
import Apecs
import qualified SDL

-- Define a component to hold the buffered input state
newtype InputState = InputState BufferedInput

-- Initialize the input system
initInput :: System' InputState
initInput = do
  buffered <- liftIO newBufferedInput
  set global (InputState buffered)

-- At the top of each frame, prepare the input buffer
-- This swaps the buffers and resets delta values (like mouse motion)
prepareInput :: System' ()
prepareInput = do
  InputState buffered <- get global
  liftIO $ prepareBufferedInput buffered

-- Handle SDL events by writing to the input buffer
handleEvent :: SDL.Event -> System' ()
handleEvent event = do
  InputState buffered <- get global
  -- Use your SDL integration library here:
  -- liftIO $ writeSDLEvent buffered event
  pure ()

-- Query actions in your game systems
updatePlayer :: System' ()
updatePlayer = do
  InputState buffered <- get global

  -- Check if jump was just pressed this frame
  jumpState <- liftIO $ deltaInput buffered actionMap Jump
  when (jumpState == JustPressed) $ do
    -- Apply jump velocity...
    pure ()

  -- Get movement as a 2D vector
  moveVec <- liftIO $ absoluteInput buffered actionMap Move
  -- moveVec :: V2 Float, e.g. V2 1.0 0.5 for forward-right

  -- Apply movement...
  pure ()
```

**Integration Notes:**

- Call `prepareBufferedInput` at the **top** of each frame, before processing events
- Process SDL events and write them to the input buffer (requires SDL integration library - coming soon)
- Query actions in your game systems using `absoluteInput` (current state) or `deltaInput` (state changes)

## Design

### Type Safety with GADTs

Actions are defined as a GADT parameterized by `ActionSource`, which can be `Button`, `Axis1D`, or `Axis2D`. This ensures actions can only be bound to compatible inputs:

```haskell
-- This compiles - Button action with button sources
Jump :=> [Key ScancodeSpace, GamepadButton ControllerButtonA]

-- This won't compile - Button action can't use axis sources
Jump :=> [LeftStick 1.0 0.15]  -- Type error!
```

The type system prevents mismatched bindings at compile time, while runtime `Typeable` reflection provides additional validation in `newActionMap`.

### Performance

- **Bounded arrays** indexed by `Enum` provide O(1) lookups for all input queries
- **Double buffering** enables efficient "just pressed" / "just released" detection by comparing current and previous frame state
- **Strict evaluation** and bang patterns prevent thunk accumulation (verified by memory leak tests)
- **INLINE pragmas** on hot paths ensure tight inner loops

The library includes comprehensive memory leak tests using the `nothunks` library to verify no thunks accumulate during typical game loop workflows.

### Input Aggregation

When multiple input sources are bound to a single action, their values are aggregated:

- **Button**: OR'd together (any source pressed = action pressed)
- **1D Axis**: Maximum absolute value
- **2D Axis**: Vector with largest magnitude

This means keyboard input (which returns ±1.0) will override partial gamepad input (e.g., 0.5) due to magnitude comparison.

## Considerations

### Sensitivity and Deadzone

Analog inputs support sensitivity (multiplier) and deadzone (threshold) configuration:

```haskell
-- GamepadStick X-axis Y-axis Sensitivity Deadzone
LeftStick 1.0 0.15  -- 1.0x sensitivity, 15% deadzone

-- MouseMotion Sensitivity
MouseMotion 0.5     -- 0.5x sensitivity for mouse look
```

Deadzones are clamped to ±0.99 to prevent division by zero during normalization.

### Delta vs. Absolute Input

- **`deltaInput`**: For detecting state _changes_ (button pressed/released, axis movement since last frame)
  - Button actions return `ButtonState` (`JustPressed`, `Held`, `JustReleased`, `NotPressed`)
  - Axis actions return the change in value (delta)

- **`absoluteInput`**: For the current state
  - Button actions return `Bool` (pressed or not)
  - Axis actions return current value

Use `deltaInput` for events (jump, shoot) and `absoluteInput` for continuous actions (movement, aiming).

### Pattern Synonyms for Convenience

The library provides pattern synonyms for common input sources:

```haskell
Key ScancodeW                                    -- Keyboard
GamepadButton ControllerButtonA                  -- Gamepad button
MouseButton MouseButtonLeft                      -- Mouse button
LeftStick sens dz                                -- Left analog stick
RightStick sens dz                               -- Right analog stick
MouseMotion sens                                 -- Mouse motion (X/Y)
DPad left up down right                          -- D-pad as 2D axis
AsAxis (Key ScancodeW)                           -- Button as 1D axis (0.0 or 1.0)
```

## Roadmap

- SDL2 integration library for event handling
- GLFW integration library
- Serialization support for saving/loading control schemes
- Action modifiers (inversion, scaling curves)
- Extensible input aggregation
- Input chords
