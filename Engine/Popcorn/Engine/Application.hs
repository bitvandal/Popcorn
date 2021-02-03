-- | The Client Application
module Popcorn.Engine.Application
    ( -- * Types
      Application(..)
    , CliArgs(..)

    -- * CLI Args parsing
    , parseArgs
    ) where

import Data.Version (Version)

import qualified Data.Text as T
import qualified Options.Applicative as Opts

-- | Application properties
data Application = Application
    { applicationName :: T.Text
    , applicationVersion :: Version
    , applicationDebugEnabled :: Bool
    } deriving stock (Eq, Show)

-- | Standard command line options
newtype CliArgs = CliArgs
    { cliArgsDebugMode :: Bool
    } deriving stock (Eq, Show)

-- | Parse command line interface options
parseArgs :: IO CliArgs
parseArgs = Opts.execParser cliArgsParserInfo

cliArgsParserInfo :: Opts.ParserInfo CliArgs
cliArgsParserInfo = Opts.info (cliArgsParser Opts.<**> Opts.helper)
  (Opts.fullDesc <> Opts.header "The Popcorn Engine Sandbox!")

cliArgsParser :: Opts.Parser CliArgs
cliArgsParser = CliArgs
    <$> Opts.switch
        (Opts.long "debug"
        <> Opts.short 'd'
        <> Opts.help "Enable debug mode")