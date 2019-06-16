module Main where
    
import Options.Applicative

type ItemIndex = Int

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long "data-path"
    <> short 'p'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")


itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

main :: IO ()
main = do
    dataPath <- execParser (info (dataPathParser) (progDesc "To-do list manager"))
    putStrLn $ "dataPath=" ++ show dataPath

