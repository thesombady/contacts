--{-# LANGUAGE BlockArguments #-}
module Toml.Builder where

import Toml.Parser
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

type Document = M.Map Text TomlValue

data BuildError
  = TypeConflict [Text]
  | NotAtTable [Text]
  | RedfinedKey [Text]
  deriving (Show, Eq)

data TomlValue
  = VBool Bool
  | VInteger Integer
  | VDouble Double
  | VString Text
  | VArray [TomlValue]
  | VTable (M.Map Text TomlValue)
  deriving (Show)

exprToValue :: TomlExpr -> TomlValue
exprToValue e = case e of
  TomlBool b -> VBool b
  TomlInteger i -> VInteger i
  TomlDouble d -> VDouble d
  TomlString t -> VString t
  TomlArray xs -> VArray (exprToValue <$> xs)
  TomlInlineTable m -> VTable (exprToValue <$> m)
  TomlTable m -> VTable (exprToValue <$> m)

data Context
  = ContextTable [Text]
  | ContextArrayElem [Text] Int

emptyTable :: TomlValue
emptyTable = VTable M.empty

insertAt :: [Text] -> TomlValue -> TomlValue -> Either BuildError TomlValue
-- insertAt [] _ root = Right root
-- insertAt [k] v (VTable m) = Right (VTable (M.insert k v m))
-- insertAt (k:ks) v (VTable m) = do
--   let child = M.findWithDefault emptyTable k m
--   child' <- insertAt ks v child
--   Right (VTable (M.insert k child' m))
-- insertAt path _ _ = Left (NotAtTable path)
insertAt path v = go path
  where
    go [] r = Right r

    go [k] (VTable m) =
      case M.lookup k m of
        Nothing -> Right $ VTable (M.insert k v m)
        Just _  -> Left $ RedfinedKey path

    go (k:ks) (VTable m) =
      case M.lookup k m of
        Nothing -> do
          child' <- go ks emptyTable
          Right $ VTable (M.insert k child' m)

        Just child ->
          case child of
            VTable _ -> do
              child' <- go ks child
              Right $ VTable (M.insert k child' m)
            _ -> Left $ NotAtTable (k:ks)

    go _ _ = Left $ NotAtTable path

appendArrayTable :: [Text] -> TomlValue -> Either BuildError (TomlValue, Int)
appendArrayTable path root = go path root
  where
    go [] r = Right (r, 0)

    go [k] (VTable m) =
      case M.lookup k m of
        Nothing ->
          Right (VTable (M.insert k (VArray [emptyTable]) m), 0)

        Just (VArray xs) ->
          let idx = length xs
          in Right (VTable (M.insert k (VArray (xs ++ [emptyTable])) m), idx)

        Just _ ->
          Left (TypeConflict [k])

    go (k:ks) (VTable m) =
      case M.lookup k m of
        Nothing -> do
          (child', idx) <- go ks emptyTable
          Right (VTable (M.insert k child' m), idx)

        Just child ->
          case child of
            VTable _ -> do
              (child', idx) <- go ks child
              Right (VTable (M.insert k child' m), idx)
            _ ->
              Left (NotAtTable (k:ks))

    go _ _ = Left (NotAtTable path)
-- appendArrayTable [] root = Right (root, 0)
-- appendArrayTable [k] (VTable m) =
--   case M.lookup k m of
--     Nothing          -> Right (VTable (M.insert k (VArray [emptyTable]) m), 0)
--     Just (VArray xs) ->
--       let idx = length xs
--       in Right (VTable (M.insert k (VArray (xs ++ [emptyTable])) m), idx)
--     Just _           -> Left (TypeConflict [k])

-- appendArrayTable (k:ks) (VTable m) = do
--   let child = M.findWithDefault emptyTable k m
--   (child', idx) <- appendArrayTable ks child
--   Right (VTable (M.insert k child' m), idx)

-- appendArrayTable path _ = Left $ NotAtTable path

insertIntoArrayElem
  :: [Text] -> Int -> [Text] -> TomlValue -> TomlValue
  -> Either BuildError TomlValue
-- insertIntoArrayElem [k] idx rel v (VTable m)
--   = case M.lookup k m of
--       Just (VArray xs)
--         | idx < length xs -> do
--             xs' <- sequence
--               [ if i == idx then insertAt rel v x else Right x
--               | (i, x) <- zip [0..] xs
--               ]
--             Right (VTable (M.insert k (VArray xs') m))
--       _ -> Left $ TypeConflict [k]
-- insertIntoArrayElem (k:ks) idx rel v (VTable m) = do
--   let child = M.findWithDefault emptyTable k m
--   child' <- insertIntoArrayElem ks idx rel v child
--   Right (VTable (M.insert k child' m))
-- insertIntoArrayElem path _ _ _ _ = Left $ NotAtTable path
insertIntoArrayElem [k] idx rel v (VTable m) =
  case M.lookup k m of
    Just (VArray xs)
      | idx < length xs -> do
          xs' <- sequence
            [ if i == idx then insertAt rel v x else Right x
            | (i, x) <- zip [0..] xs
            ]
          Right (VTable (M.insert k (VArray xs') m))
      | otherwise -> Left (TypeConflict [k])  -- bad idx shouldn't happen
    Just _  -> Left (TypeConflict [k])
    Nothing -> Left (TypeConflict [k])        -- array header should have created it
insertIntoArrayElem (k:ks) idx rel v (VTable m) = do
  let child = M.findWithDefault emptyTable k m
  child' <- insertIntoArrayElem ks idx rel v child
  Right (VTable (M.insert k child' m))
insertIntoArrayElem path _ _ _ _ = Left (NotAtTable path)

parseDoc :: [TomlStmt] -> Either BuildError TomlValue
parseDoc = go (ContextTable []) emptyTable
  where
    go _ root [] = Right root
    go ctx root (st:rest) =
      case st of
        Table (Key kpath) ->
          go (ContextTable kpath) root rest
        ArrayTable (Key kpath) -> do
          (root', idx) <- appendArrayTable kpath root
          go (ContextArrayElem kpath idx) root' rest
        KeyVal (Key rel) expr -> do
          let v = exprToValue expr
          root' <- case ctx of
            ContextTable base    -> insertAt (base ++ rel) v root
            ContextArrayElem a i -> insertIntoArrayElem a i rel v root
          go ctx root' rest

data TomlError
  = ParseErr (ParserError, Text)
  | BuildErr BuildError
  deriving (Show, Eq)

buildFromText :: Options -> Text -> Either TomlError TomlValue
buildFromText opts input = do
  stmts <- case runParser (parseFile opts) input of
    Left pe          -> Left (ParseErr pe)
    Right (ss, rest) ->
      if T.null rest then Right ss else Left $ ParseErr (UnexpectedEOF, rest)
  case parseDoc stmts of
    Left be -> Left (BuildErr be)
    Right v -> Right v

buildDoc :: Options -> FilePath -> IO (Either TomlError TomlValue)
buildDoc opts fp = do
  ts <- T.pack <$> readFile fp
  pure $ buildFromText opts ts


buildDocStr :: Options -> FilePath -> IO ()
buildDocStr opts fp = do
  ts <- T.pack <$> readFile fp
  case buildFromText opts ts of
    Left err -> print err
    Right v  -> print v

