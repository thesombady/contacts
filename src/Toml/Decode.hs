{-# LANGUAGE OverloadedStrings #-}
module Toml.Decode where

import Toml.Builder
import Toml.Types

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

data DecodeError
  = ExpectedTable [Text]
  | ExpectedString [Text]
  | ExpectedBool [Text]
  | ExpectedArray [Text]
  | MissingKey [Text]
  | ExpectedStringOrTable [Text]
  | BadTag Text
  | BadAddressFormat Text
  deriving (Show, Eq)

lookupPath :: [Text] -> TomlValue -> Either DecodeError TomlValue
lookupPath = go []
  where
    -- go :: Text -> [] -> TomlValue ->  Either DecodeError TomlValue
    go _ [] v = Right v
    go prefix (k:ks) (VTable m) =
      case M.lookup k m of
        Nothing -> Left $ MissingKey (prefix ++ [k])
        Just v  -> go (prefix ++ [k]) ks v
    go prefix _ _ = Left $ ExpectedTable prefix

lookupOptPath
  :: [Text]
  -> TomlValue
  -> Either DecodeError (Maybe TomlValue)
lookupOptPath path v0 =
  case lookupPath path v0 of
    Left (MissingKey _) -> Right Nothing
    Left e              -> Left e
    Right v             -> Right (Just v)

asTable
  :: [Text]
  -> TomlValue
  -> Either DecodeError (M.Map Text TomlValue)
asTable p v = case v of
  VTable m -> Right m
  _        -> Left $ ExpectedTable p

asString
  :: [Text]
  -> TomlValue
  -> Either DecodeError Text
asString p v = case v of
  VString t -> Right t
  _         -> Left (ExpectedString p)

asBool
  :: [Text]
  -> TomlValue
  -> Either DecodeError Bool
asBool p v = case v of
  VBool t -> Right t
  _         -> Left (ExpectedBool p)
  
asArray
  :: [Text]
  -> TomlValue
  -> Either DecodeError [TomlValue]
asArray p v = case v of
  VArray t -> Right t
  _        -> Left (ExpectedArray p)
  
parseTag :: Text -> Either DecodeError Tag
parseTag t = case T.toCaseFold t of
  "personal"   -> Right Personal
  "work"       -> Right Work
  "university" -> Right University
  other        -> Left (BadTag other)

defaultConfig :: Config
defaultConfig = Config
  { searchPattern = [Work, University, Personal]
  , uuid = False
  }

decodeConfig :: TomlValue -> Either DecodeError Config
decodeConfig doc = do
  cfgV <- lookupPath ["Config"] doc
  cfgM <- asTable ["Config"] cfgV

  spV <- case M.lookup "searchPattern" cfgM of
    Nothing -> Left $ MissingKey ["Config", "searchPattern"]
    Just v  -> Right v

  spArr  <- asArray ["Config", "searchPattern"] spV
  spStrs <- traverse (asString ["Config", "searchPattern"]) spArr
  tags   <- traverse parseTag spStrs

  uuidB <- case M.lookup "uuid" cfgM of
    Nothing -> Right False
    Just v  -> asBool ["Config", "uuid"] v
  
  Right $ defaultConfig {searchPattern = tags, uuid = uuidB}

lookupOptString
  :: [Text]
  -> Text
  -> M.Map Text TomlValue
  -> Either DecodeError (Maybe Text)
lookupOptString prefix k m =
  case M.lookup k m of
    Nothing -> Right Nothing
    Just v  -> Just <$> asString (prefix ++ [k]) v

lookupField
  :: [Text]
  -> Text
  -> M.Map Text TomlValue
  -> Either DecodeError TomlValue
lookupField prefix k m =
  case M.lookup k m of
    Nothing -> Left $ MissingKey (prefix ++ [k])
    Just v  -> Right v

parseAddress :: Text -> Either DecodeError Address
parseAddress s =
  case T.splitOn ";" s of
    [a,b,c,d] -> Right (a,b,c,d)
    _         -> Left $ BadAddressFormat s

decodeMail
  :: [Text]
  -> TomlValue
  -> Either DecodeError (Either Text MailDict)
decodeMail prefix v =
  case v of
    VString t -> Right (Left t)
    VTable m  -> do
      pairs <- traverse one (M.toList m)
      Right (Right (M.fromList pairs))
    _         -> Left $ ExpectedStringOrTable (prefix ++ ["Mail"])
  where
    one (k, tv) = do
      tag <- parseTag k
      addr <- asString (prefix ++ ["Mail", k]) tv
      Right (tag, addr)

decodeContact :: TomlValue -> Either DecodeError Contact
decodeContact v = do
  let prefix = ["Contact[]"]
  m <- asTable prefix v

  nameV <- lookupField prefix "Name" m
  nameT <- asString (prefix ++ ["Name"]) nameV

  mailV <- lookupField prefix "Mail" m
  mailT <- decodeMail prefix mailV

  phoneT <- lookupOptString prefix "Phone" m
  descrT <- lookupOptString prefix "Descr" m

  addressV <- lookupOptString prefix "Address" m
  addressT  <- traverse parseAddress addressV

  Right Contact
    { name    = nameT
    , mail    = mailT
    , phone   = phoneT
    , address = addressT
    , descr   = descrT
    }

decodeContacts :: TomlValue -> Either DecodeError [Contact]
decodeContacts doc = do
  root <- asTable [] doc
  contactsV <-
    case M.lookup "Contact" root of
      Nothing -> Right (VArray [])
      Just v  -> Right v
  xs <- asArray ["Contact"] contactsV
  traverse decodeContact xs

decodeAll :: TomlValue -> Either DecodeError (Config, [Contact])
decodeAll doc = do
  cfg <- decodeConfig doc
  cs  <- decodeContacts doc
  Right (cfg, cs)
