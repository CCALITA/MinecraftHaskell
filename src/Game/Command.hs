module Game.Command
  ( Command(..)
  , CommandResult(..)
  , parseCommand
  , commandHelp
  , allCommands
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
