module NS3473Beam.Params where

import Data.Semigroup ((<>))
import Options.Applicative (Parser,info,helper,showDefault,fullDesc,progDesc,(<**>))
import Options.Applicative.Builder (strArgument,strOption,switch,metavar,long,short,value,help)
import Options.Applicative.Extra (execParser)

data Params = Params {
    databaseIp :: String
    , allPaper :: Bool
    , downloadOnly :: Bool
    , htmlOnly :: Bool
    , feed :: String
  } deriving (Show)

mkParams :: Parser Params
mkParams =
  Params
    -- <$> strArgument (metavar "IP"  <> help "Database ip address" <> value "172.17.0.2" <> showDefault)
    <$> strArgument (metavar "IP"  <> help "Database ip address")
    <*> switch (long "all-paper" <> short 'p' <> help "All stock prices from paper history")
    <*> switch (long "download-only" <> short 'd' <> help "Download only, no update database")
    <*> switch (long "html-only" <> short 'x' <> help "Download html only, no trading depth or buyers/sellers")
    <*> strOption (long "feed" <> short 'f' <> help "Feed path" <> value "/home/rcs/opt/haskell/etradejanitor/feed" <> showDefault)

cmdLineParser :: IO Params
cmdLineParser =
  let opts = info (mkParams <**> helper)
                  (fullDesc <> progDesc "EtradeJanitor")
  in
  execParser opts

{-
data Sample
  = Hello [String]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello = Hello <$> many (argument str (metavar "TARGET..."))

sample :: Parser Sample
sample = subparser
       ( command "hello" (info hello (progDesc "Print greeting"))
      <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
       )
      <|> subparser
       ( command "bonjour" (info hello (progDesc "Print greeting"))
      <> command "au-revoir" (info (pure Goodbye) (progDesc "Say goodbye"))
      <> commandGroup "French commands:"
      <> hidden
       )
-}
