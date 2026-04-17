module Engine.Sound
  ( SoundSystem
  , SoundEvent(..)
  , initSoundSystem
  , playSound
  , cleanupSoundSystem
  ) where

import Data.IORef

data SoundEvent
  = SndBlockBreak
  | SndBlockPlace
  | SndPlayerHurt
  | SndItemPickup
  | SndMobDeath
  | SndEat
  | SndLevelUp
  | SndExplosion
  | SndArrowShoot
  | SndDoorOpen
  deriving stock (Show, Eq)

newtype SoundSystem = SoundSystem (IORef Bool)  -- enabled flag

initSoundSystem :: IO SoundSystem
initSoundSystem = SoundSystem <$> newIORef True

playSound :: SoundSystem -> SoundEvent -> IO ()
playSound _ _ = pure ()  -- no-op stub; future: check enabled flag and play actual audio

cleanupSoundSystem :: SoundSystem -> IO ()
cleanupSoundSystem _ = pure ()
