{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.Types
  ( renderType
  )
where

import           Data.Maybe                     ( catMaybes )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , intercalate
                                                , pack
                                                , toUpper
                                                )
import qualified Data.Text                     as T
                                                ( head
                                                , tail
                                                )

-- MORPHEUS
import           Rendering.Terms                ( Context(..)
                                                , Scope(..)
                                                , indent
                                                , renderAssignment
                                                , renderCon
                                                , renderData
                                                , renderSet
                                                , renderTuple
                                                , renderUnionCon
                                                , renderWrapped
                                                , ioRes
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataArgument
                                                , DataField(..)
                                                , DataType(..)
                                                , DataTyCon(..)
                                                , TypeAlias(..)
                                                , isNullable
                                                , DataEnumValue(..)
                                                )

renderType :: Context -> (Text, DataType) -> Text
renderType context (name, dataType) = typeIntro <> renderT dataType
 where
  renderT (DataScalar _) =
    renderData name []
      <> renderCon name
      <> "Int Int"
      <> defineTypeClass "SCALAR"
      <> renderGQLScalar name
  renderT (DataEnum DataTyCon { typeData }) =
    renderData name [] <> unionType (map enumName typeData) <> defineTypeClass
      "ENUM"
  renderT (DataUnion DataTyCon { typeData }) =
    renderData name [] <> renderUnion name typeData <> defineTypeClass "UNION"
  renderT (DataInputObject DataTyCon { typeData }) =
    renderData name []
      <> renderCon name
      <> renderObject renderInputField typeData
      <> defineTypeClass "INPUT_OBJECT"
  renderT (DataInputUnion _) = "\n -- Error: Input Union Not Supported"
  renderT (DataObject DataTyCon { typeData }) =
    renderData name ["m"]
      <> renderCon name
      <> renderObject (renderField context) typeData
      <> defineTypeClass "OBJECT"
  ----------------------------------------------------------------------------------------------------------
  typeIntro = "\n\n---- GQL " <> name <> " ------------------------------- \n"
  ----------------------------------------------------------------------------------------------------------
  defineTypeClass kind =
    "\n\n"
      <> renderTypeInstanceHead "GQLType" name
      <> indent
      <> "type KIND "
      <> name
      <> " = "
      <> kind
      <> "\n\n"
    ----------------------------------------------------------------------------------------------------------

renderTypeInstanceHead :: Text -> Text -> Text
renderTypeInstanceHead className name =
  "instance " <> className <> " " <> name <> " where\n"

renderGQLScalar :: Text -> Text
renderGQLScalar name =
  renderTypeInstanceHead "GQLScalar " name
    <> renderParse
    <> renderSerialize
    <> "\n\n"
 where
  renderParse = indent <> "parseValue _ = pure (" <> name <> " 0 0 )" <> "\n"
  renderSerialize = indent <> "serialize (" <> name <> " x y ) = Int (x + y)"

renderUnion :: Text -> [Text] -> Text
renderUnion typeName = unionType . map renderElem
  where renderElem name = renderUnionCon typeName name <> name

unionType :: [Text] -> Text
unionType ls =
  "\n"
    <> indent
    <> intercalate ("\n" <> indent <> "| ") ls
    <> " deriving (Generic)"

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
 where
  renderMainType  = renderSet fields <> " deriving (Generic)"
  (fields, types) = unzip (map f list)

renderInputField :: (Text, DataField) -> (Text, Maybe Text)
renderInputField (key, DataField { fieldType = TypeAlias { aliasTyCon, aliasWrappers } })
  = (key `renderAssignment` renderWrapped aliasWrappers aliasTyCon, Nothing)

renderField :: Context -> (Text, DataField) -> (Text, Maybe Text)
renderField Context { pubSub = (channel, content) } (key, DataField { fieldType = TypeAlias { aliasWrappers, aliasTyCon }, fieldArgs })
  = ( key
      `renderAssignment` argTypeName
      <>                 " -> m "
      <>                 result aliasWrappers
    , argTypes
    )
 where
  -----------------------------------------------------------------
  result wrappers
    | isNullable wrappers = renderTuple (renderWrapped wrappers aliasTyCon)
    | otherwise           = renderWrapped wrappers aliasTyCon
  (argTypeName, argTypes) = renderArguments fieldArgs
  renderArguments :: [(Text, DataArgument)] -> (Text, Maybe Text)
  renderArguments [] = ("()", Nothing)
  renderArguments list =
    ( fieldArgTypeName
    , Just
      (  renderData fieldArgTypeName []
      <> renderCon fieldArgTypeName
      <> renderObject renderInputField list
      )
    )
   where
    fieldArgTypeName = "Arg" <> camelCase key
    camelCase :: Text -> Text
    camelCase ""   = ""
    camelCase text = toUpper (pack [T.head text]) <> T.tail text
