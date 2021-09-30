module JsonParser where

import Parser
import Data.Functor (($>))
import Control.Applicative ((<|>))

type JsonParser = Parser JValue

data JValue
    = JNull
    | JBool Bool
    deriving (Show)

jvalue :: JsonParser
jvalue = jnull <|> jbool

jnull :: JsonParser
jnull = string "null" $> JNull

jbool :: JsonParser
jbool = (string "true"  $> JBool True) <|> (string "false" $> JBool False)
