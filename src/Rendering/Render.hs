{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.Render
  ( renderHaskellDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
-- MORPHEUS

import Data.HashMap.Lazy
  ( toList,
  )
import Data.Maybe (isJust)
import Data.Morpheus.Types.Internal.AST
  ( DataType,
    DataTypeLib (..),
    OperationType (..),
  )
import Data.Semigroup ((<>))
import Data.Text
  ( Text,
    pack,
  )
import qualified Data.Text as T
  ( length,
  )
import qualified Data.Text.Lazy as LT
  ( fromStrict,
  )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    align,
    cat,
    comma,
    encloseSep,
    hsep,
    line,
    lparen,
    pretty,
    rparen,
    vsep,
  )
import Rendering.Terms
  ( Context (..),
    double,
    newline,
    renderExtension,
  )
import Rendering.Types (renderType)

renderHaskellDocument :: String -> DataTypeLib -> ByteString
renderHaskellDocument modName lib =
  encodeText
    $ pack
    $ show
    $ renderLanguageExtensions context
      <> renderExports context
      <> renderImports context
      <> renderApiEvents
      <> apiRes
      <> renderTypes
  where
    encodeText = encodeUtf8 . LT.fromStrict
    onSub onS els = case subscription lib of
      Nothing -> els
      _ -> onS
    apiRes :: Doc ann
    apiRes = "type ApiRes = IORes ApiEvent" <+> txt (double newline)
    renderApiEvents :: Doc ann
    renderApiEvents
      | isJust (subscription lib) =
        vsep
          [ txt "data Channel = Channel -- ChannelA | ChannelB",
            txt "data Content = Content -- ContentA Int | ContentB String",
            txt "type ApiEvent = Event Channel Content"
          ]
          <+> txt (double newline)
      | otherwise =
        "type ApiEvent = ()" <+> txt (double newline)
    renderTypes = cat $ map renderFullType ts
      where
        ts :: [(Text, DataType)]
        ts = toList (types lib)
        renderFullType :: (Text, DataType) -> Doc ann
        renderFullType = renderType context
    context =
      Context
        { moduleName = pack modName,
          imports =
            [ ("Data.Typeable", ["Typeable"]),
              ("GHC.Generics", ["Generic"]),
              ( "Data.Morpheus.Kind",
                ["SCALAR", "ENUM", "INPUT", "OBJECT", "UNION"]
              ),
              ( "Data.Morpheus.Types",
                [ "GQLRootResolver(..)",
                  "Resolver(..)",
                  "IORes",
                  "IOMutRes",
                  "IOSubRes",
                  "Event(..)",
                  "GQLType(..)",
                  "GQLScalar(..)",
                  "ScalarValue(..)",
                  "Undefined(..)",
                  "ResolveQ"
                ]
              ),
              ("Data.Text", ["Text"])
            ],
          extensions =
            [ "OverloadedStrings",
              "DeriveGeneric",
              "TypeFamilies",
              "DeriveAnyClass"
            ],
          scope = Query,
          pubSub = onSub ("Channel", "Content") ("()", "()"),
          schema = lib
        }

renderLanguageExtensions :: Context -> Doc ann
renderLanguageExtensions Context {extensions} =
  vsep (map renderExtension extensions) <> line

renderExports :: Context -> Doc ann
renderExports Context {moduleName} =
  "module "
    <+> txt moduleName
    <+> line
    <+> txt "where"
    <+> line
    <+> line

renderImports :: Context -> Doc ann
renderImports Context {imports} =
  align $ vsep (map renderImport (fillLength imports)) <> line
  where
    renderImport (src, ls) = hsep ["import", txt src, align (sepMap ls)]

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
