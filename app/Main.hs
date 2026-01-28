{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Toml.Builder (parseDoc)
import Toml.Parser
import Toml.Types
import Toml.Decode
import Data.Text (Text)
import System.Environment (getArgs)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

matchesName :: Text -> Text -> Bool
matchesName query name =
  let q = T.toCaseFold $ T.strip query
      n = T.toCaseFold name
  in not (T.null q) && q `T.isPrefixOf` n

bestEmail :: Cfg -> [Tag] -> Contact  -> Maybe T.Text
bestEmail cmdCfg pref c =
  case c.mail of
    Left single -> Just single
    Right dict  ->
      case cmdCfg.preferedMatch of
        Just tag -> M.lookup tag dict <|> firstPref pref dict
        Nothing  -> firstPref pref dict
  where
    firstPref [] _ = Nothing
    firstPref (t:ts) m =
      case M.lookup t m of
        Just x  -> Just x
        Nothing -> firstPref ts m

-- Need (<|>) for Maybe:
infixl 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) (Just a) _ = Just a
(<|>) Nothing b  = b

data Mode = List | Search

data Cfg = Cfg
  { preferedMatch :: Maybe Tag
  , mode          :: Mode
  }

defaultConfig :: Cfg
defaultConfig = Cfg {preferedMatch = Nothing, mode = Search}

parseArgs :: [Text] -> Either String (Cfg, Maybe Text)
parseArgs xs =
  case xs of
    [] ->
      Left "Usage:\n  contacts list [--Work|--Personal|--University]\n  contacts [--Work|--Personal|--University] <name-fragment>"
    ["--help"] ->
      Left "Usage:\n  contacts list [--Work|--Personal|--University]\n  contacts [--Work|--Personal|--University] <name-fragment>"

    ("list":rest) ->
      let cfg = foldl applyFlag defaultConfig rest
      in Right (cfg { mode = List }, Nothing)

    -- default: treat last non-flag as query
    _ ->
      case splitFlags xs of
        (flags, [q]) ->
          let cfg = foldl applyFlag defaultConfig flags
          in Right (cfg, Just q)
        (_, []) ->
          Left "Missing query. Try: contacts <name-fragment> or contacts list"
        (_, _) ->
          Left "Too many positional arguments."
  where
    applyFlag cfg "--Work"       = cfg { preferedMatch = Just Work }
    applyFlag cfg "--University" = cfg { preferedMatch = Just University }
    applyFlag cfg "--Personal"   = cfg { preferedMatch = Just Personal }
    applyFlag cfg _              = cfg

    -- all args starting with "--" are flags; the rest are positionals
    splitFlags :: [Text] -> ([Text], [Text])
    splitFlags = foldr step ([], [])
      where
        step a (fs, ps)
          | "--" `T.isPrefixOf` a = (a:fs, ps)
          | otherwise             = (fs, a:ps)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs (map T.pack args) of
    Left msg -> putStrLn msg
    Right (cmdCfg, mQuery) -> run cmdCfg mQuery

run :: Cfg -> Maybe Text -> IO ()
run cmdCfg mQuery = do
  input <- T.pack <$> readFile "/home/andrease/.contacts/addresses.toml"
  let opts = Options
        { allowMultiInlineTables = True
        , allowLeadingCommaInLineTabels = True
        }

  stmts <- case runParser (parseFile opts) input of
    Left perr     -> error ("Parse error: " ++ show perr)
    Right (ss, _) -> pure ss

  doc <- case parseDoc stmts of
    Left berr -> error ("Build error: " ++ show berr)
    Right d   -> pure d

  (cfg, contacts) <- case decodeAll doc of
    Left derr -> error ("Decode error: " ++ show derr)
    Right x   -> pure x

  case cmdCfg.mode of
    List   -> mapM_ (printList cmdCfg cfg) contacts
    Search -> case mQuery of
      Nothing    -> putStrLn "Missing query!"
      Just query -> do
        let hits = filter (matchesName query . name) contacts
        case hits of
          [] -> putStrLn "No matches."
          cs -> mapM_ (printHit cmdCfg cfg) cs

printList :: Cfg -> Config -> Contact -> IO ()
printList cmdCfg cfg c =
  case bestEmail cmdCfg cfg.searchPattern c of
    Nothing -> pure ()
    Just e  -> TIO.putStrLn (e <> "\t" <> c.name)

printContact :: Cfg -> Config -> Contact -> IO ()
printContact cmdCfg cfg c =
  case bestEmail cmdCfg cfg.searchPattern c of
    Nothing -> TIO.putStrLn ("<no email>\t" <> c.name)
    Just e  -> TIO.putStrLn (e <> "\t" <> c.name)

printHit :: Cfg -> Config -> Contact -> IO ()
printHit cmdCfg cfg c =
  case bestEmail cmdCfg cfg.searchPattern c of
    Nothing -> TIO.putStrLn ("<no email>\t" <> c.name)
    Just e  -> TIO.putStrLn (e <> "\t" <> c.name)
