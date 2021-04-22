-- | The Client Application
module Popcorn.Engine.Application
    ( -- * Types
      Application(..)
    , CliArgs(..)

    -- * CLI Args parsing
    , parseArgs
    ) where

import Data.Char (isDigit)
import Data.Version (Version)

import qualified Data.Text as T
import qualified Options.Applicative as Opts

-- | Application properties
data Application = Application
    { applicationName :: T.Text
    , applicationVersion :: Version
    , applicationDebugEnabled :: Bool
    , applicationMainWindowWidth :: Int
    , applicationMainWindowHeight :: Int
    } deriving stock (Eq, Show)

-- | Standard command line options
data CliArgs = CliArgs
    { cliArgsDebugMode :: Bool
    , cliArgsAppMainWindowWidth :: Int
    , cliArgsAppMainWindowHeight :: Int
    } deriving stock (Eq, Show)

-- | Parse command line interface options
parseArgs
    :: T.Text     -- ^ The application name
    -> IO CliArgs -- ^ Returns the parsed command line arguments
parseArgs = Opts.execParser . cliArgsParserInfo

cliArgsParserInfo :: T.Text -> Opts.ParserInfo CliArgs
cliArgsParserInfo appName = Opts.info (cliArgsParser Opts.<**> Opts.helper)
    (Opts.fullDesc <> Opts.header header)
  where
    header = "The Popcorn Engine version " <> T.unpack appName <> "!"

cliArgsParser :: Opts.Parser CliArgs
cliArgsParser = CliArgs
    <$> Opts.switch
        (Opts.long "debug"
        <> Opts.short 'd'
        <> Opts.help "Enable debug mode")
    <*> Opts.option (uintArg 160 3840)
        (Opts.long "width"
        <> Opts.short 'w'
        <> Opts.value 1024
        <> Opts.metavar "<main-window-width>"
        <> Opts.help "Main window width")
    <*> Opts.option (uintArg 120 2160)
        (Opts.long "height"
        <> Opts.short 'h'
        <> Opts.value 768
        <> Opts.metavar "<main-window-height>"
        <> Opts.help "Main window height"
        )

uintArg :: Int -> Int -> Opts.ReadM Int
uintArg minVal maxVal = Opts.eitherReader $ \s -> do
  if not (null s) && all isDigit s
    then
      let val = read s :: Int
      in
        if inRange minVal maxVal val
          then Right val
          else Left (mconcat
                [ "Value "
                , show val
                , " not within allowed bounds : ["
                , show minVal
                , ","
                , show maxVal
                , "]"
                ]
            )
    else Left "Invalid value"

inRange :: Ord a => a -> a -> a -> Bool
inRange lo hi val = val >= lo && val <= hi