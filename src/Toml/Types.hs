module Toml.Types where

import Data.Text (Text)
-- import qualified Data.Text as T

import qualified Data.Map.Strict as M

data Tag = Personal | Work | University
  deriving (Eq, Ord, Show)
-- [[Contact]]
-- Name ="Foo bar"
type MailDict = M.Map Tag Text
-- Mail =
-- 	{ Work      = "foo.barBaz@host1.com"
-- 	, Personal  = "foo.barTui@host2.com"
-- 	, University= "foo.bar@host3.com"
-- 	}
-- # or
-- # Mail = "foo.barBaz@host1.com"

type Phone = Text
-- # Phone   = "08324123123"
type Address = (Text, Text, Text, Text)
 
-- # Address = "FooBarRoad 90;11111;FoobarCounty;FooLand"
-- # Descr   = "This is a small description; usefull for vcard"
-- Address, county, county number, country

data Contact = Contact
  { name    :: Text
  , mail    :: Either Text MailDict
  , phone   :: Maybe Phone
  , address :: Maybe Address
  , descr   :: Maybe Text
  } deriving (Show)

data Config = Config
  { searchPattern :: [Tag]
  , uuid          :: Bool    
  } deriving (Show)
