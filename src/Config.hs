module Config (Config(..), opts) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Config = Config
  { c_romFile :: FilePath
  , c_clock   :: Int
  } deriving (Show)

opts :: ParserInfo Config
opts = info (config <**> helper)
    (  fullDesc
    <> header "Chip-8 emulator"
    <> progDesc "Emulate rom specified by ROM path" )

config :: Parser Config
config = Config
    <$> argument str
        (  metavar "ROM"
        <> help "Path to ROM file" )
    <*> option auto
        (  long "clock"
        <> short 'c'
        <> metavar "CLOCK"
        <> value 500
        <> help "Interpeter clock speed in Hz. Defaults to 500")
