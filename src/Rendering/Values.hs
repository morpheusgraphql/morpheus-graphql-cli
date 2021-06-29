{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.Values
  ( renderRootResolver
  , renderResolver
  , Scope(..)
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )

-- MORPHEUS
import           Rendering.Terms                ( Context(..)
                                                , Scope(..)
                                                , renderAssignment
                                                , renderCon
                                                , renderEqual
                                                , renderPure
                                                , renderSet
                                                , renderUnionCon
                                                , ioRes
                                                , newline
                                                , double
                                                , indent
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , DataType(..)
                                                , DataTypeContent(..)
                                                , DataTypeLib(..)
                                                , DataEnumValue(..)
                                                , TypeRef(..)
                                                , TypeWrapper(..)
                                                , Collection
                                                , Name
                                                , OperationType(..)
                                                )

renderResolver :: Context -> (Text, DataType) -> Text
renderResolver cxt = render cxt . snd

class RenderValue a where
  render :: Context -> a -> Text

instance RenderValue DataType where
  render cxt@Context { scope } DataType { typeName, typeContent } = __render
    typeContent
   where
    __render DataScalar{} =
      renderFunc False <> renderPure <> "$ " <> renderCon typeName <> "0 0"
    __render (DataEnum enums) =
      renderFunc False <> renderPure <> renderCon (enumName $ head enums)
    __render (DataUnion (member : _)) =
      renderFunc True
        <> indent
        <> renderUnionCon typeName member
        <> " <$> "
        <> "resolve"
        <> member
    __render (DataObject fields) =
      renderFunc True <> renderPure <> renderCon typeName <> renderObjFields
     where
      renderObjFields = renderResObject (map renderFieldRes fields)
      renderFieldRes (key, field) = (key, render cxt field)
    __render _ = "" -- INPUT Types Does not Need Resolvers
    ------
    renderPure | isOperation typeName = indent
               | otherwise            = indent <> "pure "
    --------------------------------
    renderFunc x = funcSig x <> funcName <> "= " <> newline
    funcName = "resolve" <> typeName <> " "
    funcSig :: Bool -> Text
    funcSig isOutput
      | isOutput  = __renderSig $ "(" <> typeName <> " " <> monadName <> ")"
      | otherwise = __renderSig typeName
     where
      __renderSig x = renderAssignment funcName (monadSig <> x) <> newline

      monadName | scope == Mutation = "ApiRes"
                | otherwise         = "APiRes"
      ---------------------------------------------------------------------------------
      monadSig | isOperation typeName = ""
               | otherwise            = monadName <> " "

isOperation :: Name -> Bool
isOperation name = name `elem` operationNames

operationNames :: [Name]
operationNames = ["Mutation", "Subscription", "Query"]

instance RenderValue DataField where
  render cxt@Context { scope } DataField { fieldType } = "const "
    <> withScope scope (render cxt fieldType)
   where
    withScope Subscription x =
      "$ SubResolver $ Event { channels = [Channel], content = const "
        <> x
        <> " }"
    withScope Mutation x =
      "$ MutResolver $ [Event {channels = [Channel], content = Content}] " <> x
    withScope _ x = x

instance RenderValue TypeRef where
  render _ TypeRef { typeWrappers, typeConName } = renderValue typeWrappers
   where
    renderValue (TypeMaybe : _) = "$ " <> renderPure <> "Nothing"
    renderValue (TypeList  : _) = "$ " <> renderPure <> "[]"
    renderValue []              = renderName typeConName
    ---------------------------------------------------
    renderName "String"  = "$ pure \"\""
    renderName "Int"     = "$ pure 0"
    renderName "Float"   = "$ pure 0"
    renderName "Boolean" = "$ pure False"
    renderName fName     = "resolve" <> fName


renderRootResolver :: Context -> DataTypeLib -> Text
renderRootResolver Context { pubSub = (channel, _) } DataTypeLib { mutation, subscription }
  = renderSignature <> renderBody
 where
  renderSignature = "rootResolver :: " <> renderRootSig <> "\n"
   where
    renderRootSig =
      "GQLRootResolver IO "
        <> channel
        <> " Query "
        <> maybeOperation mutation
        <> " "
        <> maybeOperation subscription
    ----------------------
    maybeOperation (Just (name, _)) = name
    maybeOperation Nothing          = "Undefined"
  renderBody = "rootResolver =\n  GQLRootResolver" <> renderResObject fields
   where
    fields =
      [ ("queryResolver"       , "resolveQuery")
      , ("mutationResolver"    , maybeRes mutation)
      , ("subscriptionResolver", maybeRes subscription)
      ]
  ---------------------------------------------
    maybeRes (Just (name, _)) = "resolve" <> name
    maybeRes Nothing          = "Undefined"


renderResObject :: [(Text, Text)] -> Text
renderResObject = renderSet . map renderEntry
  where renderEntry (key, value) = renderEqual key value
