{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Toml.Parser where

import Data.Functor (($>), void)
import Control.Monad (when)
import Data.Text (Text)
import Control.Applicative (optional, Alternative(..), many, some)
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

data ParserError
  = SyntaxError String
  | ParserError String
  | UndefinedSymbol String
  | UnexpectedSymbol String
  | EmptyError
  | ExpectedEOF
  | UnexpectedEOF
  deriving (Show, Eq)

newtype Parser a
  = Parser { runParser ::  Text -> Either (ParserError, Text) (a, Text) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a, s') <- p s
    pure (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)

  (<*>) (Parser pf) (Parser pa) = Parser $ \s -> do
    (f, s1) <- pf s
    (a, s2) <- pa s1
    pure (f a, s2)

instance Alternative Parser where
  empty = Parser $ \s -> Left (EmptyError, s)

  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> do
    case p1 s of
      Left _  -> p2 s
      Right r -> Right r

instance Monad Parser where
  Parser pa >>= f = Parser $ \s -> do
    (a, s1) <- pa s
    runParser (f a) s1

-- ===== General parsing functionalities =====

peek :: Parser (Maybe Char)
peek = Parser $ \s -> do
  Right (if T.null s then Nothing else Just (T.head s), s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> do
  case T.uncons s of
    Nothing -> Left (ExpectedEOF, s)
    Just (c, s')
      | f c       -> Right (c, s')
      | otherwise -> Left (UnexpectedSymbol $ "Unexpected char:" <> [c], s)

char :: Char -> Parser Char
char c = satisfy (==c)

text :: Text -> Parser Text
text t = Parser $ \s -> do
  if t `T.isPrefixOf` s
    then Right (t, T.drop (T.length t) s)
    else Left (UndefinedSymbol (T.unpack t), s)

hspace :: Parser ()
hspace = satisfy (\c -> c == ' ' || c == '\t') $> ()

comment :: Parser ()
comment = do
  _ <- char '#'
  _ <- many (satisfy (/='\n'))
  pure ()

ws :: Parser ()
ws = many (hspace <|> (comment *> optional hspace $> ())) $> ()

ws1 :: Parser ()
ws1 = some (hspace <|> (comment *> optional hspace $> ())) $> ()

wsnl :: Parser ()
wsnl = many (newline <|> ws1) $> ()

newline :: Parser ()
newline = char '\n' $> () <|> (text "\r\n" $> ())

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

symbol :: Text -> Parser Text
symbol t = lexeme (text t)

eof :: Parser ()
eof = Parser $ \s -> do
  if T.null s then Right ((), s) else Left (UnexpectedEOF, s)

endOfLine :: Parser ()
endOfLine = ws *> newline *> ws

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go = (end $> []) <|> ((:) <$> p <*> go)

-- tryP :: Parser a -> Parser a
-- tryP (Parser p) = Parser $ \s -> do
--   case p s of
--     Left e  -> Left e
--     Right r -> Right r

ident :: Parser Text
ident = lexeme $ do
  let ok c = isAlphaNum c || c == '_' || c == '-'
  cs <- some (satisfy ok)
  pure (T.pack cs)

eq :: Parser ()
eq = lexeme (char '=') $> ()

anyChar :: Parser Char
anyChar = satisfy (const True)

quoted :: Parser Text
quoted = lexeme $ do
  _ <- char '"'
  content <- manyTill strChar (char '"')
  pure (T.pack content)
  where
    strChar :: Parser Char
    strChar = (char '\\' *> escape) <|> satisfy (/= '"')

    escape :: Parser Char
    escape = (char '"'       $> '"')
              <|> (char '\\' $> '\\')
              <|> (char  'n' $> '\n')
              <|> (char  't' $> '\t')
              <|> (char  'r' $> '\r')
              <|> anyChar


-- ===== Specific Parsing functionalities =====

-- data Tokens
--   = TokenLbracket | TokenLbracket2
--   | TokenRbracket | TokenRbracket2
--   | TokenLbrace   | TokenRbrace
--   | TokenInteger Integer
--   | TokenDouble  Double
--   | TokenString  Text
--   | TokenIdent   Text
--   | TokenBool    Bool
--   | TokenEOF

newtype Key = Key [Text] deriving (Ord, Eq, Show)

data TomlExpr
  = TomlBool Bool
  | TomlInteger Integer
  | TomlDouble Double
  | TomlString Text
  | TomlArray [TomlExpr]
  | TomlInlineTable (M.Map Text TomlExpr)
  | TomlTable (M.Map Text TomlExpr)

data TomlStmt
  = KeyVal Key TomlExpr
  | Table Key
  | ArrayTable Key


data Options = Options
  { allowMultiInlineTables :: Bool
  , allowLeadingCommaInLineTabels :: Bool
  }

pInlineTable :: Options -> Parser (M.Map Text TomlExpr)
pInlineTable opts = do
  _ <- char '{'
  skipWsInline opts
  pairs <- sepInline opts (pInlinePair opts)
  skipWsInline opts
  _ <- char '}'
  pure (M.fromList pairs)

pInlinePair :: Options -> Parser (Text, TomlExpr)
pInlinePair opts = do
  k <- ident <|> quoted
  skipWsInline opts
  _ <- char '='
  skipWsInline opts
  v <- parseExpr opts
  pure (k, v)

sepInline :: Options -> Parser a -> Parser [a]
sepInline opts p = go []
  where
    go acc =
      (do
          when opts.allowLeadingCommaInLineTabels $
            void (optional (char ',' *> skipWsInline opts))

          x <- p
          skipWsInline opts
          _ <- optional (char ',')
          skipWsInline opts
          go (x:acc)
      ) <|> pure (reverse acc)

skipWsInline :: Options -> Parser ()
skipWsInline opts =
  many ( inlineSpaces <|> inlineComments <|> inlineNl ) $> ()
  where
    inlineSpaces = satisfy (\c -> c == ' ' || c == '\t') $> ()
    inlineComments = do
      _ <- char '#'
      _ <- many $ satisfy (/= '\n')
      pure ()
    inlineNl =
      if opts.allowMultiInlineTables
        then char '\n' $> ()
        else empty

digitChar :: Parser Char
digitChar = satisfy isDigit

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

signed :: Parser Char
signed = oneOf "+-" <|> pure '+'

digitsUnderscore :: Parser Text
digitsUnderscore = do
  cs <- some (digitChar <|> char '_')
  pure $ T.pack cs

stripUnderscore :: Text -> Text
stripUnderscore = T.filter (/= '_')

keyword :: Text -> Parser Text
keyword t = lexeme $ do
  _  <- text t
  mc <- peek
  case mc of
    Just c | isAlphaNum c || c == '_' || c == '-' -> empty
    _ -> pure t

parseArray :: Options -> Parser TomlExpr
parseArray opts = do
  _ <- lexeme $ char '['
  wsnl
  xs <- arrayElems
  wsnl
  _ <- lexeme $ char ']'
  pure (TomlArray xs)
  where
    arrayElems =
      (do
        x <- parseExpr opts
        wsnl
        more <- many $ do
          _ <- lexeme $ char ','
          wsnl
          parseExpr opts <* wsnl
        _ <- optional (lexeme $ char ',')
        pure (x:more)
      ) <|> pure []

parseInteger :: Parser Integer
parseInteger = lexeme $ do
  s <- signed
  ds <- digitsUnderscore
  let t = (if s == '-' then "-" else "") <> stripUnderscore ds
  case reads (T.unpack t) of
    [(n, "")] -> pure n
    _         -> empty

dot :: Parser ()
dot = lexeme $ char '.' $> ()

parseDouble :: Parser Double
parseDouble = lexeme $ do
  s  <- signed
  ip <- digitsUnderscore
  fp <- optional $ do
    _  <- char '.'
    ds <- digitsUnderscore
    pure ds
  ep <- optional $ do
    _ <- oneOf "eE"
    es <- signed
    ed <- digitsUnderscore
    pure (es, ed)

  when (isNothing fp && isNothing ep) empty

  let base =stripUnderscore ip <> maybe "" (\f -> "." <> stripUnderscore f) fp
      expo = maybe "" (\(es, ed) -> "e" <> (if es == '-' then "-" else "") <> stripUnderscore ed) ep
      t    = (if s == '-' then "-" else "") <> base <> expo

  case reads (T.unpack t) of
    [(x, "")] -> pure x
    _         -> empty

parseKey :: Parser Key
parseKey = do
  lead  <- keyPart
  trail <- many (dot *> keyPart)
  pure (Key (lead:trail))
  where
    keyPart = ident <|> quoted

parseBool :: Parser TomlExpr
parseBool = (keyword "true" $> TomlBool True) <|> (keyword "false" $> TomlBool False)

parseTableHeader :: Parser TomlStmt
parseTableHeader = do
  _   <- wsnl
  _   <- lexeme $ char '['
  key <- parseKey
  _   <- lexeme $ char ']'
  void $ optional endOfLine
  pure (Table key)

parseArrayTableHeader :: Parser TomlStmt
parseArrayTableHeader = do
  _   <- wsnl
  _   <- lexeme $ text "[["
  key <- parseKey
  _   <- lexeme $ text "]]"
  void $ optional endOfLine
  -- _   <- optional (endOfLine <|> pure ()) $> ()
  pure (ArrayTable key)
  
parseKeyVal :: Options -> Parser TomlStmt
parseKeyVal opts = do
  k <- parseKey
  _ <- eq
  v <- parseExpr opts
  -- _ <- optional (endOfLine <|> pure ()) $> ()
  void $ optional endOfLine
  pure (KeyVal k v)

parseStmt :: Options -> Parser TomlStmt
parseStmt opts = parseArrayTableHeader
            <|> parseTableHeader
            <|> parseKeyVal opts

parseFile :: Options -> Parser [TomlStmt]
parseFile opts = wsnl *> many (parseStmt opts <* wsnl) <* eof


parseExpr :: Options -> Parser TomlExpr
parseExpr opts = wsnl *> expr
  where
  expr = parseBool
        <|> (TomlString  <$> quoted)
        <|> (TomlInlineTable <$> pInlineTable opts)
        <|> parseArray opts
        <|> (TomlDouble  <$> parseDouble)
        <|> (TomlInteger <$> parseInteger)
