module JsonParser where

import Parser
import Data.Functor (($>))
import Control.Applicative ((<|>), many)
import Data.Char (isDigit)
import Text.Read (readMaybe)

data JValue
    = JNull
    | JBool Bool
    | JInt Int
    | JString String
    | JArray [JValue]
    deriving (Show)

jvalue :: Parser JValue
jvalue = jnull <|> jbool <|> jint <|> jstr <|> jarray

jnull :: Parser JValue
jnull = string "null" $> JNull

jbool :: Parser JValue
jbool = (string "true" $> JBool True) <|> (string "false" $> JBool False)

jint :: Parser JValue
jint = JInt . read <$> notEmpty (spanCheck isDigit)

jstr :: Parser JValue
jstr = JString <$> literalString

jarray :: Parser JValue
jarray = JArray <$> list jvalue (char '[') (char ']') (char ',')