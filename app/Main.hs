module Main (main) where

import Claude.V1
import Claude.V1.Messages
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as AesonText
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Vector qualified as Vector
import Protolude
import System.Directory qualified as Directory
import System.Environment qualified as Environment
import System.FilePath qualified as FilePath
import System.IO qualified as System

data AgentTool = AgentTool
  { tool :: Tool,
    handler :: Aeson.Value -> IO (Either Text Text)
  }

data Agent = Agent
  { createMessage :: CreateMessage -> IO MessageResponse,
    getUserMessage :: IO (Maybe Text),
    tools :: Vector.Vector AgentTool
  }

newAgent :: Methods -> IO (Maybe Text) -> Vector.Vector AgentTool -> Agent
newAgent Methods {createMessage} getUserMessage tools = Agent {..}

main :: IO ()
main = do
  key <- Environment.getEnv "ANTHROPIC_KEY"
  clientEnv <- getClientEnv "https://api.anthropic.com"
  let methods = makeMethods clientEnv (Text.pack key) (Just "2023-06-01")
      getUserMsg = do
        eof <- System.isEOF
        if eof
          then pure Nothing
          else Just <$> Text.IO.getLine
  runAgent (newAgent methods getUserMsg [readFileAgentTool, listFilesAgentTool, editFileAgentTool])

runInference :: Agent -> Vector.Vector Message -> IO MessageResponse
runInference Agent {createMessage, tools} conversation =
  createMessage
    _CreateMessage
      { model = "claude-sonnet-4-5-20250929",
        messages = conversation,
        max_tokens = 1024,
        tools = Just $ Vector.map (\AgentTool {tool = t} -> inlineTool t) tools
      }

runAgent :: Agent -> IO ()
runAgent agent@Agent {getUserMessage, tools} = do
  Text.IO.putStrLn "Chat with Claude (use 'ctrl-c' to quit)"
  loop Vector.empty
  where
    loop :: Vector.Vector Message -> IO ()
    loop conversation = do
      Text.IO.putStr "\ESC[94mYou\ESC[0m: "
      System.hFlush System.stdout
      mInput <- getUserMessage
      case mInput of
        Nothing -> pure ()
        Just userInput -> do
          let userMsg =
                Message
                  { role = User,
                    content = [Content_Text {text = userInput, cache_control = Nothing}],
                    cache_control = Nothing
                  }
          inferenceLoop (Vector.snoc conversation userMsg)

    inferenceLoop :: Vector.Vector Message -> IO ()
    inferenceLoop conversation = do
      MessageResponse {content, stop_reason} <- runInference agent conversation
      let assistantMsg =
            Message
              { role = Assistant,
                content = Vector.mapMaybe contentBlockToContent content,
                cache_control = Nothing
              }
          conversation' = Vector.snoc conversation assistantMsg
      traverse_ displayBlock content
      case stop_reason of
        Just Tool_Use -> do
          toolResults <- traverse (executeToolUse tools) (Vector.filter isToolUseBlock content)
          let toolResultMsg =
                Message
                  { role = User,
                    content = toolResults,
                    cache_control = Nothing
                  }
          inferenceLoop (Vector.snoc conversation' toolResultMsg)
        _ -> loop conversation'

    executeToolUse :: Vector.Vector AgentTool -> ContentBlock -> IO Content
    executeToolUse agentTools ContentBlock_Tool_Use {id = toolId, name = callName, input} = do
      let maybeHandler = handler <$> Vector.find (\AgentTool {tool = Tool {name = n}} -> n == callName) agentTools
      result <- case maybeHandler of
        Nothing -> pure $ Left "tool not found"
        Just h -> do
          Text.IO.putStrLn ("\ESC[92mtool\ESC[0m: " <> callName <> "(" <> show input <> ")")
          h input
      pure $ case result of
        Right text -> Content_Tool_Result {tool_use_id = toolId, content = Just text, is_error = Nothing}
        Left err -> Content_Tool_Result {tool_use_id = toolId, content = Just err, is_error = Just True}
    executeToolUse _ block = panic ("executeToolUse called on non-tool-use block: " <> show block)

    isToolUseBlock :: ContentBlock -> Bool
    isToolUseBlock ContentBlock_Tool_Use {} = True
    isToolUseBlock _ = False

    displayBlock :: ContentBlock -> IO ()
    displayBlock ContentBlock_Text {text} =
      Text.IO.putStrLn ("\ESC[93mClaude\ESC[0m: " <> text)
    displayBlock _ = pure ()

-- Tools

newtype ReadFileInput = ReadFileInput {path :: Text}
  deriving newtype (Aeson.FromJSON)

newtype ListFilesInput = ListFilesInput {path :: Maybe Text}
  deriving newtype (Aeson.FromJSON)

readFileTool :: Tool
readFileTool =
  functionTool
    "read_file"
    (Just "Read the contents of a given relative file path. Use this when you want to see what's inside a file. Do not use this with directory names.")
    ( Aeson.object
        [ "type" Aeson..= ("object" :: Text),
          "properties"
            Aeson..= Aeson.object
              [ "path"
                  Aeson..= Aeson.object
                    [ "type" Aeson..= ("string" :: Text),
                      "description" Aeson..= ("The relative path of a file in the working directory." :: Text)
                    ]
              ],
          "required" Aeson..= (["path"] :: [Text])
        ]
    )

readFileHandler :: Aeson.Value -> IO (Either Text Text)
readFileHandler input =
  case Aeson.fromJSON input of
    Aeson.Error _ -> pure (Left "missing or invalid 'path' field")
    Aeson.Success ReadFileInput {path} -> do
      eContents <- try (Text.IO.readFile (Text.unpack path))
      pure $ case (eContents :: Either IOException Text) of
        Left err -> Left (show err)
        Right contents -> Right contents

readFileAgentTool :: AgentTool
readFileAgentTool =
  AgentTool
    { tool = readFileTool,
      handler = readFileHandler
    }

listFilesTool :: Tool
listFilesTool =
  functionTool
    "list_files"
    (Just "List files and directories at a given path. If no path is provided, lists files in the current directory.")
    ( Aeson.object
        [ "type" Aeson..= ("object" :: Text),
          "properties"
            Aeson..= Aeson.object
              [ "path"
                  Aeson..= Aeson.object
                    [ "type" Aeson..= ("string" :: Text),
                      "description" Aeson..= ("Optional relative path to list files from. Defaults to current directory if not provided." :: Text)
                    ]
              ]
        ]
    )

listFilesHandler :: Aeson.Value -> IO (Either Text Text)
listFilesHandler input = do
  let dir = case Aeson.fromJSON input of
        Aeson.Success ListFilesInput {path = Just p} | not (Text.null p) -> Text.unpack p
        _ -> "."
  eFiles <- try (walkDir dir)
  pure $ case (eFiles :: Either IOException [FilePath]) of
    Left err -> Left (show err)
    Right files -> Right (toS (AesonText.encodeToLazyText files))

walkDir :: FilePath -> IO [FilePath]
walkDir rootDir = go ""
  where
    go relDir = do
      let absDir = if null relDir then rootDir else rootDir FilePath.</> relDir
      entries <- Directory.listDirectory absDir
      fmap concat $ for (sort entries) $ \entry -> do
        let relPath = if null relDir then entry else relDir FilePath.</> entry
        isDir <- Directory.doesDirectoryExist (absDir FilePath.</> entry)
        if isDir
          then (relPath <> "/" :) <$> go relPath
          else pure [relPath]

listFilesAgentTool :: AgentTool
listFilesAgentTool =
  AgentTool
    { tool = listFilesTool,
      handler = listFilesHandler
    }

data EditFileInput = EditFileInput
  { path :: Text,
    old_str :: Text,
    new_str :: Text
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON)

editFileTool :: Tool
editFileTool =
  functionTool
    "edit_file"
    (Just "Make edits to a text file. Replaces 'old_str' with 'new_str' in the given file. 'old_str' and 'new_str' MUST be different from each other. If the file specified with path doesn't exist, it will be created.")
    ( Aeson.object
        [ "type" Aeson..= ("object" :: Text),
          "properties"
            Aeson..= Aeson.object
              [ "path"
                  Aeson..= Aeson.object
                    [ "type" Aeson..= ("string" :: Text),
                      "description" Aeson..= ("The path to the file" :: Text)
                    ],
                "old_str"
                  Aeson..= Aeson.object
                    [ "type" Aeson..= ("string" :: Text),
                      "description" Aeson..= ("Text to search for - must match exactly and must only have one match exactly" :: Text)
                    ],
                "new_str"
                  Aeson..= Aeson.object
                    [ "type" Aeson..= ("string" :: Text),
                      "description" Aeson..= ("Text to replace old_str with" :: Text)
                    ]
              ],
          "required" Aeson..= (["path", "old_str", "new_str"] :: [Text])
        ]
    )

editFileHandler :: Aeson.Value -> IO (Either Text Text)
editFileHandler input =
  case Aeson.fromJSON input of
    Aeson.Error _ -> pure (Left "missing or invalid fields")
    Aeson.Success EditFileInput {path, old_str, new_str}
      | old_str == new_str -> pure (Left "old_str and new_str must be different")
      | otherwise -> do
          eContents <- try (Text.IO.readFile (Text.unpack path))
          case (eContents :: Either IOException Text) of
            Left _
              | old_str == "" -> createNewFile (Text.unpack path) new_str
              | otherwise -> pure (Left ("file not found: " <> path))
            Right oldContent -> do
              let newContent = Text.replace old_str new_str oldContent
              if newContent == oldContent && old_str /= ""
                then pure (Left "old_str not found in file")
                else do
                  eWrite <- try (Text.IO.writeFile (Text.unpack path) newContent)
                  pure $ case (eWrite :: Either IOException ()) of
                    Left err -> Left (show err)
                    Right _ -> Right "OK"

createNewFile :: FilePath -> Text -> IO (Either Text Text)
createNewFile filePath content = do
  let dir = FilePath.takeDirectory filePath
  eResult <- try $ do
    when (dir /= ".") $ Directory.createDirectoryIfMissing True dir
    Text.IO.writeFile filePath content
  pure $ case (eResult :: Either IOException ()) of
    Left err -> Left (show err)
    Right _ -> Right ("Successfully created file " <> Text.pack filePath)

editFileAgentTool :: AgentTool
editFileAgentTool =
  AgentTool
    { tool = editFileTool,
      handler = editFileHandler
    }
