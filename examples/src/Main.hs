{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Main
  ( main
  )
where


import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Morpheus                  ( Interpreter(..) )
import           Data.Morpheus.Document         ( toGraphQLDocument )
import           Data.Morpheus.Server           ( GQLState
                                                , gqlSocketApp
                                                , initGQLState
                                                )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WebSockets
                                               as WaiWs
import           Network.WebSockets             ( defaultConnectionOptions )
import           Web.Scotty                     ( body
                                                , file
                                                , get
                                                , post
                                                , raw
                                                , scottyApp
                                                )

-- examples
import qualified Simple                         ( rootResolver )
--import qualified Mutation                       ( rootResolver )
--import qualified Subscription                   ( rootResolver )

apiBy path api = do
  post path $ raw =<< (liftIO . api =<< body)
  get path $ file "./examples/index.html"

main :: IO ()
main = do
  state   <- initGQLState
  httpApp <- httpServer state
  Warp.runSettings settings httpApp
  --Warp.runSettings settings
  --  $ WaiWs.websocketsOr defaultConnectionOptions (wsApp state) httpApp


 where
  settings = Warp.setPort 3000 Warp.defaultSettings
  --wsApp    = gqlSocketApp gqlRoot
--  httpServer :: GQLState IO EVENT -> IO Wai.Application
  httpServer state =
    scottyApp $ apiBy "/simple" (interpreter Simple.rootResolver)
  --  post "/" $ raw =<< (liftIO . interpreter gqlRoot state =<< body)
  --  get "/" $ file "./examples/index.html"
  --  get "/schema.gql" $ raw $ toGraphQLDocument $ Identity gqlRoot
  --  apiBy "/mutation" Mutation.rootResolver


