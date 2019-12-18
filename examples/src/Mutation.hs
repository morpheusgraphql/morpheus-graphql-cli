{-# LANGUAGE  OverloadedStrings  #-}
{-# LANGUAGE  DeriveGeneric  #-}
{-# LANGUAGE  TypeFamilies  #-}
{-# LANGUAGE  DeriveAnyClass  #-}
module Mutation
  ( rootResolver
  )
where

import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Data.Morpheus.Kind             ( SCALAR
                                                , ENUM
                                                , INPUT
                                                , OBJECT
                                                , UNION
                                                )
import           Data.Morpheus.Types            ( GQLRootResolver(..)
                                                , Resolver(..)
                                                , IORes
                                                , IOMutRes
                                                , IOSubRes
                                                , Event(..)
                                                , GQLType(..)
                                                , GQLScalar(..)
                                                , ScalarValue(..)
                                                , Undefined(..)
                                                , ResolveQ
                                                )
import           Data.Text                      ( Text )
type ApiEvent = ()

type ApiRes = IORes ApiEvent

rootResolver :: GQLRootResolver IO () Query Mutation Undefined
rootResolver = GQLRootResolver { queryResolver        = resolveQuery
                               , mutationResolver     = resolveMutation
                               , subscriptionResolver = Undefined
                               }

---- GQL Query ------------------------------- 
data Query (m :: * -> *) =
  Query
    { deity :: ArgDeity -> m (Deity m)
  ,  character :: ArgCharacter -> m (Character m)
  ,  hero :: () -> m (Human m)
    }
 deriving (Generic, GQLType)


data ArgDeity =
  ArgDeity
    { name :: Maybe [Maybe [Maybe [[Maybe [Text]]]]]
  ,  mythology :: Maybe Text
    }
 deriving (Generic)


data ArgCharacter =
  ArgCharacter
    { characterID :: Text
  ,  age :: Maybe Int
    }
 deriving (Generic)

resolveQuery :: (Query ApiRes)
resolveQuery = Query { deity     = const resolveDeity
                     , character = const resolveCharacter
                     , hero      = const resolveHuman
                     }


---- GQL Mutation ------------------------------- 
data Mutation (m :: * -> *) =
  Mutation
    { createDeity :: ArgCreateDeity -> m (Deity m)
  ,  createCharacter :: ArgCreateCharacter -> m (Character m)
    }
 deriving (Generic, GQLType)


data ArgCreateDeity =
  ArgCreateDeity
    { deityName :: Maybe [Maybe [Maybe [[Maybe [Text]]]]]
  ,  deityMythology :: Maybe Text
    }
 deriving (Generic)


data ArgCreateCharacter =
  ArgCreateCharacter
    { charRealm :: Realm
  ,  charMutID :: Text
    }
 deriving (Generic)

resolveMutation :: (Mutation ApiRes)
resolveMutation = Mutation { createDeity     = const resolveDeity
                           , createCharacter = const resolveCharacter
                           }


---- GQL Deity ------------------------------- 
data Deity (m :: * -> *) =
  Deity
    { fullName :: () -> m Text
  ,  power :: () -> m Power
    }
 deriving (Generic, GQLType)

resolveDeity :: ApiRes (Deity ApiRes)
resolveDeity =
  pure Deity { fullName = const $ pure "", power = const resolvePower }


---- GQL Human ------------------------------- 
data Human (m :: * -> *) =
  Human
    { humanName :: () -> m Text
  ,  profession :: () -> m (Maybe Text)
    }
 deriving (Generic, GQLType)

resolveHuman :: ApiRes (Human ApiRes)
resolveHuman = pure Human { humanName  = const $ pure ""
                          , profession = const $ return Nothing
                          }


---- GQL City ------------------------------- 
data City =
    Athens
  | Ithaca
  | Sparta
  | Troy
 deriving (Generic)
instance GQLType City where
  type KIND City = ENUM

resolveCity :: ApiRes City
resolveCity = pure Athens

---- GQL Power ------------------------------- 
data Power =
  Power Int Int

instance GQLScalar  Power where
  parseValue _ = pure (Power 0 0)
  serialize (Power x y) = Int (x + y)

instance GQLType Power where
  type KIND Power = SCALAR

resolvePower :: ApiRes Power
resolvePower = pure $ Power 0 0

---- GQL Realm ------------------------------- 
data Realm =
  Realm
    { owner :: Text
  ,  place :: Maybe Int
    }
 deriving (Generic)
instance GQLType Realm where
  type KIND Realm = INPUT



---- GQL Creature ------------------------------- 
data Creature (m :: * -> *) =
  Creature
    { creatureName :: () -> m Text
  ,  realm :: () -> m City
    }
 deriving (Generic, GQLType)

resolveCreature :: ApiRes (Creature ApiRes)
resolveCreature =
  pure Creature { creatureName = const $ pure "", realm = const resolveCity }


---- GQL Character ------------------------------- 
data Character (m :: * -> *) =
    CharacterCreature (Creature m)
  | CharacterDeity (Deity m)
  | CharacterHuman (Human m) deriving (Generic, GQLType)

resolveCharacter :: ApiRes (Character ApiRes)
resolveCharacter = CharacterCreature <$> resolveCreature
