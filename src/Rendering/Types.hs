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
                                                , renderDeriving
                                                , renderInstanceHead
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataArgument
                                                , DataField(..)
                                                , DataType(..)
                                                , DataTypeContent(..)
                                                , TypeRef(..)
                                                , isNullable
                                                , DataEnumValue(..)
                                                )


renderType :: Context -> (Text, DataType) -> Text
renderType context (_, datatype) = render context datatype

class RenderType a where
    render :: Context -> a -> Text

instance RenderType DataType where
  render context DataType { typeContent, typeName } = typeIntro
    <> renderT typeContent
   where
    renderT (DataScalar _) =
      renderData typeName []
        <> renderCon typeName
        <> "Int Int"
        <> defineTypeClass "SCALAR"
        <> renderGQLScalar typeName
    renderT (DataEnum enums) =
      renderData typeName []
        <> unionType (map enumName enums)
        <> defineTypeClass "ENUM"
    renderT (DataUnion members) =
      renderData typeName [] <> renderUnion typeName members <> defineTypeClass
        "UNION"
    renderT (DataInputObject fields) =
      renderData typeName []
        <> renderCon typeName
        <> renderObject renderInputField fields
        <> defineTypeClass "INPUT_OBJECT"
    renderT (DataObject fields) =
      renderData typeName ["m"]
        <> renderCon typeName
        <> renderObject (renderField context) fields
        <> defineTypeClass "OBJECT"
    renderT (DataInputUnion _) = "\n -- Error: Input Union Not Supported"
    ----------------------------------------------------------------------------------------------------------
    typeIntro =
      "\n\n---- GQL " <> typeName <> " ------------------------------- \n"
    ----------------------------------------------------------------------------------------------------------
    defineTypeClass kind =
      "\n\n"
        <> renderInstanceHead "GQLType" typeName
        <> indent
        <> "type KIND "
        <> typeName
        <> " = "
        <> kind
        <> "\n\n"
        ----------------------------------------------------------------------------------------------------------


renderGQLScalar :: Text -> Text
renderGQLScalar name =
  renderInstanceHead "GQLScalar " name
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
  "\n" <> indent <> intercalate ("\n" <> indent <> "| ") ls <> renderDeriving []

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
 where
  renderMainType  = renderSet fields <> renderDeriving []
  (fields, types) = unzip (map f list)

renderInputField :: (Text, DataField) -> (Text, Maybe Text)
renderInputField (key, DataField { fieldType = TypeRef { typeConName, typeWrappers } })
  = (key `renderAssignment` renderWrapped typeWrappers typeConName, Nothing)

renderField :: Context -> (Text, DataField) -> (Text, Maybe Text)
renderField Context { pubSub = (channel, content) } (key, DataField { fieldType = tyRef@TypeRef { typeConName, typeWrappers }, fieldArgs })
  = (key `renderAssignment` argTypeName <> " -> m " <> result, argTypes)
 where
  -----------------------------------------------------------------
  result
    | isNullable tyRef = renderTuple (renderWrapped typeWrappers typeConName)
    | otherwise        = renderWrapped typeWrappers typeConName
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
