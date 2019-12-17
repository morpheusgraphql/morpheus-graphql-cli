{-# LANGUAGE OverloadedStrings #-}

module Rendering.Terms
  ( indent
  , renderReturn
  , renderData
  , renderCon
  , renderMaybe
  , renderList
  , renderTuple
  , renderAssignment
  , renderExtension
  , renderWrapped
  , renderSet
  , renderUnionCon
  , renderEqual
  , Scope(..)
  , Context(..)
  , ioRes
  , renderDeriving
  , renderInstanceHead
  , renderGQLTypeInstance
  , renderTypeIntro
  , newline
  , double
  )
where


import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , intercalate
                                                , toUpper
                                                )
import qualified Data.Text                     as T
                                                ( unwords )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST
                                                ( TypeWrapper(..)
                                                , Name
                                                )


double :: Text -> Text
double x = x <> x

ioRes :: Text -> Text
ioRes event = "IORes " <> event <> " "

indent :: Text
indent = "  "

renderEqual :: Text -> Text -> Text
renderEqual key value = key <> " = " <> value

renderReturn :: Text
renderReturn = "return "

newline :: Text
newline = "\n"

renderData :: Text -> [Text] -> Text
renderData name tyArgs =
  "data " <> T.unwords (name : tyArgs) <> " =" <> newline <> indent

renderCon :: Text -> Text
renderCon name = name <> " "

renderMaybe :: Text -> Text
renderMaybe typeName = "Maybe " <> typeName

renderList :: Text -> Text
renderList typeName = "[" <> typeName <> "]"

renderTuple :: Text -> Text
renderTuple typeName = "(" <> typeName <> ")"

renderSet :: [Text] -> Text
renderSet fields =
  bracket "{ " <> intercalate ("\n  ," <> indent) fields <> bracket "}\n"
  where bracket x = "\n    " <> x

renderAssignment :: Text -> Text -> Text
renderAssignment key value = key <> " :: " <> value

renderExtension :: Text -> Text
renderExtension name = "{-# LANGUAGE " <> name <> " #-}\n"

renderWrapped :: [TypeWrapper] -> Text -> Text
renderWrapped (TypeList  : xs) = renderList . renderWrapped xs
renderWrapped (TypeMaybe : xs) = renderMaybe . renderWrapped xs
renderWrapped []               = strToText

strToText :: Text -> Text
strToText "String"  = "Text"
strToText "Boolean" = "Bool"
strToText x         = x

renderUnionCon :: Text -> Text -> Text
renderUnionCon typeName conName = renderCon (typeName <> conName)


renderGQLTypeInstance :: Name -> Name -> Text
renderGQLTypeInstance typeName kind =
  "\n\n"
    <> renderInstanceHead "GQLType" typeName
    <> indent
    <> "type KIND "
    <> typeName
    <> " = "
    <> kind
    <> "\n\n"

renderTypeIntro :: Name -> Text
renderTypeIntro typeName =
  "\n--- GQL " <> typeName <> " ------------------------------- \n"

renderInstanceHead :: Text -> Text -> Text
renderInstanceHead className name =
  "instance " <> className <> " " <> name <> " where\n"


renderDeriving :: [Text] -> Text
renderDeriving list =
  " deriving " <> renderTuple (intercalate ", " ("Generic" : list))

data Scope
  = Mutation
  | Subscription
  | Query

data Context = Context
  { moduleName :: Text
  , imports    :: [(Text, [Text])]
  , extensions :: [Text]
  , scope      :: Scope
  , pubSub     :: (Text, Text)
  }
