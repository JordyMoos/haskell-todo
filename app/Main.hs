module Main where
    
import Options.Applicative

type ItemIndex = Int

type ItemDescription = Maybe String

data Options = Options FilePath ItemIndex ItemDescription deriving Show


defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"


optionsParser :: Parser Options
optionsParser = Options
    <$> dataPathParser
    <*> itemIndexParser
    <*> updateItemDescriptionParser



dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long "data-path"
    <> short 'p'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")


itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")


itemDescriptionValueParser  :: Parser String
itemDescriptionValueParser =
    strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
    Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-desc")

main :: IO ()
main = do
    options <- execParser (info (optionsParser) (progDesc "To-do list manager"))
    putStrLn $ "options=" ++ show options

