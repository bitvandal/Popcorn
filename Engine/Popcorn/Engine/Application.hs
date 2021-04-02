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