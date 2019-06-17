{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           System.IO.Error
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           Options.Applicative     hiding ( infoParser )


type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe String
type ItemDueBy = Maybe String

data Item = Item
    { title :: ItemTitle
    , description :: ItemDescription
    , priority :: ItemPriority
    , dueBy :: ItemDueBy
    } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

data ItemUpdate = ItemUpdate
    { titleUpdate :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate :: Maybe ItemPriority
    , dueByUpdate :: Maybe ItemDueBy
    } deriving Show


data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList


data Options = Options FilePath Command deriving Show

data Command
    = Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving Show

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"


infoParser :: Parser Command
infoParser = pure Info


initParser :: Parser Command
initParser = pure Init


listParser :: Parser Command
listParser = pure List


addParser :: Parser Command
addParser =
    Add <$> addItemparser


addItemparser :: Parser Item
addItemparser =
    Item
        <$> argument str (metavar "TITLE" <> help "title")
        <*> optional itemDescriptionValueParser
        <*> optional itemPriorityValueParser
        <*> optional itemDueByValueParser


viewParser :: Parser Command
viewParser =
    View <$> itemIndexParser


updateParser :: Parser Command
updateParser =
    Update
        <$> itemIndexParser
        <*> updateItemParser


updateItemParser :: Parser ItemUpdate
updateItemParser =
    ItemUpdate
        <$> optional updateItemTitleParser
        <*> optional updateItemDescriptionParser
        <*> optional updateItemPriorityParser
        <*> optional updateItemDueByParser


updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser


updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
    Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-desc")


updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser =
    Just <$> itemPriorityValueParser
    <|> flag' Nothing (long "clear-priority")


updateItemDueByParser :: Parser ItemPriority
updateItemDueByParser =
    Just <$> itemDueByValueParser
    <|> flag' Nothing (long "clear-due-by")


itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")


itemTitleValueParser :: Parser String
itemTitleValueParser =
    strOption $ long "title" <> short 't' <> metavar "TITLE" <> help "title"


itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
    strOption $ long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description"


itemPriorityValueParser :: Parser String
itemPriorityValueParser =
    strOption $ long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority"


itemDueByValueParser :: Parser String
itemDueByValueParser =
    strOption $ long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due-by date/time"


removeParser :: Parser Command
removeParser =
    Remove <$> itemIndexParser


commandParser :: Parser Command
commandParser = subparser $ mconcat
    [ command "info" $ info infoParser (progDesc "Show info")
    , command "init" $ info initParser (progDesc "Initialize items")
    , command "list" $ info listParser (progDesc "List all items")
    , command "add" $ info addParser (progDesc "Add item")
    , command "view" $ info viewParser (progDesc "View item")
    , command "update" $ info updateParser (progDesc "Update item")
    , command "remove" $ info removeParser (progDesc "Remove item")
    ]


optionsParser :: Parser Main.Options
optionsParser =
    Options
        <$> dataPathParser
        <*> commandParser


dataPathParser :: Parser FilePath
dataPathParser =
    strOption
        $  value defaultDataPath
        <> long "data-path"
        <> short 'p'
        <> metavar "DATAPATH"
        <> help ("path to data file (default " ++ defaultDataPath ++ ")")


main :: IO ()
main = do
    toDoList <- readToDoList "file.txt"
    print toDoList


run :: FilePath -> Command -> IO ()
run dataPath Info = putStrLn "Info"
run dataPath Init = putStrLn "Init"
run dataPath List = putStrLn "List"
run dataPath (Add item) =
    putStrLn $ "Add: item=" ++ show item
run dataPath (View idx) = 
    putStrLn $ "View: idx=" ++ show idx
run dataPath (Update idx itemUpdate) =
    putStrLn $ "Update: idx=" ++ show idx ++ " itemUpdate=" ++ show itemUpdate
run dataPath (Remove idx) = 
    putStrLn $ "Remove: idx=" ++ show idx


writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)


readToDoList :: FilePath -> IO (Maybe ToDoList)
readToDoList dataPath =
    catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (ToDoList []))
