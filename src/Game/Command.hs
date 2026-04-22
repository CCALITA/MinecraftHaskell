module Game.Command
  ( Command(..)
  , CommandResult(..)
  , ChatState(..)
  , ChatMessage(..)
  , parseCommand
  , executeCommand
  , commandHelp
  , allCommands
  , emptyChatState
  , chatAddChar
  , chatDeleteChar
  , chatGetBuffer
  , chatClear
  , addChatMessage
  , updateChatMessages
  ) where

import Data.Char (toLower, isSpace)
import Text.Read (readMaybe)

-- | Supported chat commands
data Command
  = CmdGive String Int              -- /give <item> <count>
  | CmdTeleport Float Float Float   -- /tp <x> <y> <z>
  | CmdTime String                  -- /time set day|night|<ticks>
  | CmdWeather String               -- /weather clear|rain
  | CmdGamemode String              -- /gamemode survival|creative
  | CmdKill                         -- /kill
  | CmdSeed                         -- /seed
  | CmdHelp                         -- /help
  | CmdSpawnMob String              -- /summon <mobtype>
  deriving stock (Show, Eq)

-- | Result of executing a command
data CommandResult
  = CmdSuccess String
  | CmdError String
  deriving stock (Show, Eq)

-- | Parse a raw chat string into a Command. Returns Nothing for
--   unrecognized commands or strings that don't start with '/'.
parseCommand :: String -> Maybe Command
parseCommand input =
  let stripped = dropWhile isSpace input
  in case stripped of
    '/' : _ -> parseTokens (words stripped)
    _       -> Nothing

parseTokens :: [String] -> Maybe Command
parseTokens [] = Nothing
parseTokens (cmd : args) = case map toLower cmd of
  "/give"     -> parseGive args
  "/tp"       -> parseTeleport args
  "/teleport" -> parseTeleport args
  "/time"     -> parseTime args
  "/weather"  -> parseSingleArg CmdWeather args
  "/gamemode" -> parseSingleArg CmdGamemode args
  "/kill"     -> parseNoArgs CmdKill args
  "/seed"     -> parseNoArgs CmdSeed args
  "/help"     -> parseNoArgs CmdHelp args
  "/summon"   -> parseSingleArg CmdSpawnMob args
  _           -> Nothing

-- | /give <item> <count>
parseGive :: [String] -> Maybe Command
parseGive [item, countStr]
  | Just n <- readMaybe countStr
  , n > 0
  = Just (CmdGive (map toLower item) n)
parseGive [item] = Just (CmdGive (map toLower item) 1)
parseGive _ = Nothing

-- | /tp <x> <y> <z>
parseTeleport :: [String] -> Maybe Command
parseTeleport [xStr, yStr, zStr] = do
  x <- readMaybe xStr
  y <- readMaybe yStr
  z <- readMaybe zStr
  Just (CmdTeleport x y z)
parseTeleport _ = Nothing

-- | /time set <value>  or  /time <value>
parseTime :: [String] -> Maybe Command
parseTime ["set", val] = Just (CmdTime (map toLower val))
parseTime [val]        = Just (CmdTime (map toLower val))
parseTime _            = Nothing

-- | Parse a single lowercase argument into a command constructor
parseSingleArg :: (String -> Command) -> [String] -> Maybe Command
parseSingleArg mk [val] = Just (mk (map toLower val))
parseSingleArg _  _     = Nothing

-- | Commands with no arguments
parseNoArgs :: Command -> [String] -> Maybe Command
parseNoArgs cmd [] = Just cmd
parseNoArgs _   _  = Nothing

-- | Help text for all commands: (usage, description)
commandHelp :: [(String, String)]
commandHelp =
  [ ("/give <item> [count]", "Give items to the player")
  , ("/tp <x> <y> <z>",     "Teleport to coordinates")
  , ("/time set <value>",    "Set time (day, night, or tick number)")
  , ("/weather <type>",      "Set weather (clear, rain)")
  , ("/gamemode <mode>",     "Set game mode (survival, creative)")
  , ("/kill",                "Kill the player")
  , ("/seed",                "Show the world seed")
  , ("/help",                "Show this help message")
  , ("/summon <mob>",        "Summon a mob")
  ]

-- | List of all recognized command names
allCommands :: [String]
allCommands =
  [ "/give"
  , "/tp"
  , "/teleport"
  , "/time"
  , "/weather"
  , "/gamemode"
  , "/kill"
  , "/seed"
  , "/help"
  , "/summon"
  ]

-- | Execute a parsed command and produce a result message.
-- This is a pure function that describes what the command does;
-- the caller applies the side effects.
executeCommand :: Command -> CommandResult
executeCommand cmd = case cmd of
  CmdGive item count ->
    CmdSuccess $ "Gave " ++ show count ++ " " ++ item
  CmdTeleport x y z ->
    CmdSuccess $ "Teleported to " ++ showF x ++ " " ++ showF y ++ " " ++ showF z
  CmdTime val ->
    CmdSuccess $ "Time set to " ++ val
  CmdWeather val ->
    CmdSuccess $ "Weather set to " ++ val
  CmdGamemode val ->
    CmdSuccess $ "Game mode set to " ++ val
  CmdKill ->
    CmdSuccess "Killed player"
  CmdSeed ->
    CmdSuccess "Seed: 12345"
  CmdHelp ->
    CmdSuccess $ "Commands: " ++ unwords (map fst commandHelp)
  CmdSpawnMob mobType ->
    CmdSuccess $ "Summoned " ++ mobType
  where
    showF :: Float -> String
    showF f = let n = round (f * 100) :: Int
                  whole = div (abs n) 100
                  frac  = mod (abs n) 100
                  sign  = if f < 0 && n /= 0 then "-" else ""
                  pad   = if frac < 10 then "0" else ""
              in sign ++ show whole ++ "." ++ pad ++ show frac

-- | A timed message displayed in the HUD
data ChatMessage = ChatMessage
  { cmText    :: !String
  , cmTimer   :: !Float    -- ^ seconds remaining before fade
  } deriving stock (Show, Eq)

-- | Chat input state (immutable value, stored in an IORef)
data ChatState = ChatState
  { csBuffer   :: !String          -- ^ current input buffer
  , csMessages :: ![ChatMessage]   -- ^ visible output messages
  } deriving stock (Show, Eq)

-- | Empty chat state
emptyChatState :: ChatState
emptyChatState = ChatState
  { csBuffer   = ""
  , csMessages = []
  }

-- | Append a character to the chat buffer
chatAddChar :: Char -> ChatState -> ChatState
chatAddChar c cs = cs { csBuffer = csBuffer cs ++ [c] }

-- | Delete the last character from the chat buffer
chatDeleteChar :: ChatState -> ChatState
chatDeleteChar cs = case csBuffer cs of
  [] -> cs
  s  -> cs { csBuffer = init s }

-- | Get the current buffer contents
chatGetBuffer :: ChatState -> String
chatGetBuffer = csBuffer

-- | Clear the buffer
chatClear :: ChatState -> ChatState
chatClear cs = cs { csBuffer = "" }

-- | Add a message with a display duration
addChatMessage :: String -> Float -> ChatState -> ChatState
addChatMessage text duration cs =
  cs { csMessages = csMessages cs ++ [ChatMessage text duration] }

-- | Tick message timers, removing expired ones
updateChatMessages :: Float -> ChatState -> ChatState
updateChatMessages dt cs =
  cs { csMessages = filter (\m -> cmTimer m > 0)
                    $ map (\m -> m { cmTimer = cmTimer m - dt })
                    $ csMessages cs
     }
