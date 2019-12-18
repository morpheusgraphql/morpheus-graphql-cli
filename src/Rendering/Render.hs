{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.Render
  ( renderHaskellDocument
  )
where

import           Data.Maybe                     ( isJust )
import           Data.ByteString.Lazy.Char8     ( ByteString )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , intercalate
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T
                                                ( concat
                                                , length
                                                )
import qualified Data.Text.Lazy                as LT
                                                ( fromStrict )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )

-- MORPHEUS
import           Rendering.Terms                ( Context(..)
                                                , renderExtension
                                                , newline
                                                , double
                                                )
import           Rendering.Types                ( renderType )
import           Rendering.Values               ( Scope(..)
                                                , renderResolver
                                                , renderRootResolver
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataTypeLib(..)
                                                , allDataTypes
                                                )
import           Data.Text.Prettyprint.Doc      ( pretty
                                                , parens
                                                , Doc
                                                , nest
                                                , encloseSep
                                                , lbracket
                                                , rbracket
                                                , comma
                                                , align
                                                , lparen
                                                , rparen
                                                , list
                                                , line
                                                , vsep
                                                , (<+>)
                                                , hsep
                                                )

renderHaskellDocument :: String -> DataTypeLib -> ByteString
renderHaskellDocument modName lib =
  encodeText
    $  renderLanguageExtensions context
    <> pack (show $ renderExports context)
    <> renderImports context
    <> renderApiEvents
    <> apiRes
    <> renderRootResolver context lib
    <> types
 where
  encodeText = encodeUtf8 . LT.fromStrict
  onSub onS els = case subscription lib of
    Nothing -> els
    _       -> onS
  apiRes :: Text
  apiRes = "type ApiRes = IORes ApiEvent" <> double newline
  renderApiEvents :: Text
  renderApiEvents
    | isJust (subscription lib)
    = "data Channel = Channel -- ChannelA | ChannelB"
      <> "\n\n"
      <> "data Content = Content -- ContentA Int | ContentB String"
      <> "\n\n"
      <> "type ApiEvent = Event Channel Content"
      <> double newline
    | otherwise
    = "type ApiEvent = ()" <> double newline
  types = intercalate newline $ map renderFullType (allDataTypes lib)
   where
    renderFullType x = renderType cont x <> newline <> renderResolver cont x
     where
      cont = context { scope = getScope $ fst x }
      getScope "Mutation"     = Mutation
      getScope "Subscription" = Subscription
      getScope _              = Query
  context = Context
    { moduleName = pack modName
    , imports    = [ ("Data.Typeable", ["Typeable"])
                   , ("GHC.Generics" , ["Generic"])
                   , ( "Data.Morpheus.Kind"
                     , ["SCALAR", "ENUM", "INPUT", "OBJECT", "UNION"]
                     )
                   , ( "Data.Morpheus.Types"
                     , [ "GQLRootResolver(..)"
                       , "Resolver(..)"
                       , "IORes"
                       , "IOMutRes"
                       , "IOSubRes"
                       , "Event(..)"
                       , "GQLType(..)"
                       , "GQLScalar(..)"
                       , "ScalarValue(..)"
                       , "Undefined(..)"
                       , "ResolveQ"
                       ]
                     )
                   , ("Data.Text", ["Text"])
                   ]
    , extensions = [ "OverloadedStrings"
                   , "DeriveGeneric"
                   , "TypeFamilies"
                   , "DeriveAnyClass"
                   ]
    , scope      = Query
    , pubSub     = onSub ("Channel", "Content") ("()", "()")
    , schema     = lib
    }


renderLanguageExtensions :: Context -> Text
renderLanguageExtensions Context { extensions } =
  T.concat (map renderExtension extensions) <> "\n"

renderExports :: Context -> Doc ann
renderExports Context { moduleName } =
  "-- generated by 'Morpheus' CLI"
    <+> line
    <+> "module "
    <+> txt moduleName
    <+> line
    <+> nest 2 (parens $ txt "rootResolver")
    <+> line
    <+> txt "where"
    <+> line
    <+> line

renderImports :: Context -> Text
renderImports Context { imports } = pack
  (show $ align $ vsep (map renderImport (fillLength imports)) <> line)
  where renderImport (src, ls) = hsep ["import", txt src, align (sepMap ls)]


sepMap :: [Text] -> Doc ann
sepMap ls = align (encloseSep lparen rparen comma (map pretty ls))

txt :: Text -> Doc ann
txt = pretty


fillLength :: [(Text, a)] -> [(Text, a)]
fillLength ls = map fillfst ls
 where
  fillfst (x, y) = (fillSpace x, y)
  maxL = foldr (max . T.length . fst) 0 ls
  fillSpace x = x <> pack (map (const ' ') [0 .. (maxL - T.length x)])
