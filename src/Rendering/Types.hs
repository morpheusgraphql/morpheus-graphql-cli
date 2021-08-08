{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.Types
  ( renderType,
  )
where

import Data.Maybe (catMaybes)
-- MORPHEUS

import Data.Morpheus.Types.Internal.AST
  ( DataArgument,
    DataEnumValue (..),
    DataField (..),
    DataType (..),
    DataTypeContent (..),
    DataTypeKind (..),
    DataTypeLib,
    Name,
    TypeRef (..),
    allDataTypes,
    isNullable,
    isOutputObject,
    kindOf,
    lookupType,
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    intercalate,
    pack,
    toUpper,
    unpack,
  )
import qualified Data.Text as T
  ( head,
    tail,
  )
import Data.Text.Prettyprint.Doc
  ( Doc,
    pretty,
  )
import Rendering.Terms
  ( Context (..),
    double,
    indent,
    label,
    newline,
    renderAssignment,
    renderCon,
    renderData,
    renderDeriving,
    renderGQLTypeInstance,
    renderInstanceHead,
    renderSet,
    renderTuple,
    renderUnionCon,
    renderWrapped,
  )

renderType :: Context -> (Text, DataType) -> Doc ann
renderType context (_, datatype) = case render context datatype of
  Right x -> x
  Left x -> error (unpack x)

type Result = Either Text

class RenderType a where
  render :: Context -> a -> Result (Doc ann)

getKind :: DataTypeLib -> Name -> Result DataTypeKind
getKind _ "String" = pure KindScalar
getKind _ "Boolean" = pure KindScalar
getKind _ "int" = pure KindScalar
getKind _ "Float" = pure KindScalar
getKind lib name =
  kindOf <$> lookupType ("type " <> name <> "not found") (allDataTypes lib) name

instance RenderType DataType where
  render context DataType {typeContent, typeName} =
    (label typeName <>) . pretty <$> renderT typeContent
    where
      renderT (DataScalar _) =
        pure $
          renderData typeName []
            <> renderCon typeName
            <> "Int Int"
            <> double newline
            <> renderGQLScalar typeName
            <> newline
            <> renderGQLTypeInstance typeName "SCALAR"
      renderT (DataEnum enums) =
        pure $
          renderData typeName []
            <> unionType (map enumName enums)
            <> newline
            <> renderDeriving []
            <> renderGQLTypeInstance typeName "ENUM"
      renderT (DataInputObject fields) =
        pure $
          renderData typeName []
            <> renderCon typeName
            <> renderObject renderInputField fields
            <> renderGQLTypeInstance typeName "INPUT"
      renderT (DataObject fields) = do
        body <- renderResObject context fields
        pure $ renderData typeName ["(m :: * -> *)"] <> renderCon typeName <> body
      renderT (DataUnion members) =
        pure $
          renderData typeName ["(m :: * -> *)"]
            <> renderUnion typeName members
            <> renderDeriving ["GQLType"]
      renderT (DataInputUnion _) = Left "Input Union Not Supported"

renderGQLScalar :: Text -> Text
renderGQLScalar name =
  renderInstanceHead "GQLScalar " name <> renderParse <> renderSerialize
  where
    renderParse =
      indent <> "parseValue _ = pure (" <> name <> " 0 0 )" <> newline
    renderSerialize =
      indent <> "serialize (" <> name <> " x y ) = Int (x + y)" <> newline

renderUnion :: Text -> [Text] -> Text
renderUnion typeName = unionType . map renderElem
  where
    renderElem name = renderUnionCon typeName name <> ("(" <> name <> " m)")

unionType :: [Text] -> Text
unionType ls = indent <> intercalate ("\n" <> indent <> "| ") ls

renderResObject :: Context -> [(Name, DataField)] -> Result Text
renderResObject context list = do
  (fields, arguments) <- unzip <$> traverse (renderField context) list
  pure
    $ intercalate (double newline)
    $ (renderSet fields <> renderDeriving ["GQLType"])
      : catMaybes arguments

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
  where
    renderMainType = renderSet fields <> renderDeriving []
    (fields, types) = unzip (map f list)

renderInputField :: (Text, DataField) -> (Text, Maybe Text)
renderInputField (key, DataField {fieldType = TypeRef {typeConName, typeWrappers}}) =
  (key `renderAssignment` renderWrapped typeWrappers typeConName, Nothing)

renderField :: Context -> (Text, DataField) -> Result (Text, Maybe Text)
renderField Context {schema} (key, DataField {fieldType = tyRef@TypeRef {typeConName, typeWrappers}, fieldArgs}) =
  do
    kind <- getKind schema typeConName
    pure
      (key `renderAssignment` argTypeName <> " -> m " <> result kind, argTypes)
  where
    result :: DataTypeKind -> Text
    result kind
      | isNullable tyRef = renderTuple namedType
      | otherwise = namedType
      where
        tName = renderWrapped typeWrappers typeConName
        namedType
          | KindUnion == kind || isOutputObject kind = "(" <> tName <> " m)"
          | otherwise = tName
    (argTypeName, argTypes) = renderArguments fieldArgs
    renderArguments :: [(Text, DataArgument)] -> (Text, Maybe Text)
    renderArguments [] = ("()", Nothing)
    renderArguments list =
      ( fieldArgTypeName,
        Just
          ( renderData fieldArgTypeName []
              <> renderCon fieldArgTypeName
              <> renderObject renderInputField list
          )
      )
      where
        fieldArgTypeName = "Arg" <> camelCase key
        camelCase :: Text -> Text
        camelCase "" = ""
        camelCase text = toUpper (pack [T.head text]) <> T.tail text
