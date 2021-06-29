{-# LANGUAGE  OverloadedStrings  #-}
{-# LANGUAGE  DeriveGeneric  #-}
{-# LANGUAGE  TypeFamilies  #-}
{-# LANGUAGE  DeriveAnyClass  #-}
module Simple
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

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver { queryResolver        = resolveQuery
                               , mutationResolver     = Undefined
                               , subscriptionResolver = Undefined
                               }

---- GQL Query ------------------------------- 
data Query (m :: * -> *) =
  Query
    { deity :: ArgDeity -> m (Deity m)
  ,  character :: ArgCharacter -> m (Character m)
    }
 deriving (Generic, GQLType)


data ArgDeity =
  ArgDeity
    { name :: Maybe [Maybe [Maybe [[Maybe [Text]]]]]
    }
 deriving (Generic)


data ArgCharacter =
  ArgCharacter
    { characterID :: Text
  ,  age :: Maybe Int
    }
 deriving (Generic)

resolveQuery :: Applicative m => (Query m)
resolveQuery =
  Query { deity = const resolveDeity, character = const resolveCharacter }


---- GQL Deity ------------------------------- 
data Deity (m :: * -> *) =
  Deity
    { fullName :: () -> m Text
  ,  power :: () -> m (Maybe Power)
    }
 deriving (Generic, GQLType)

resolveDeity :: Applicative m => m (Deity m)
resolveDeity =
  pure Deity { fullName = const $ pure "", power = const $ pure Nothing }


---- GQL City ------------------------------- 
data City =
    Athens
  | Ithaca
  | Sparta
  | Troy
 deriving (Generic)
instance GQLType City where
  type KIND City = ENUM

resolveCity :: Applicative m => m City
resolveCity = pure Athens

---- GQL Power ------------------------------- 
data Power =
  Power Int Int

instance GQLScalar  Power where
  parseValue _ = pure (Power 0 0)
  serialize (Power x y) = Int (x + y)

instance GQLType Power where
  type KIND Power = SCALAR

resolvePower :: Applicative m => m Power
resolvePower = pure $ Power 0 0

---- GQL Creature ------------------------------- 
data Creature (m :: * -> *) =
  Creature
    { creatureName :: () -> m Text
  ,  realm :: () -> m City
  ,  immortality :: () -> m Bool
    }
 deriving (Generic, GQLType)

resolveCreature :: Applicative m => m (Creature m)
resolveCreature = pure Creature { creatureName = const $ pure ""
                                , realm        = const resolveCity
                                , immortality  = const $ pure False
                                }


---- GQL Character ------------------------------- 
data Character (m :: * -> *) =
    CharacterCreature (Creature m)
  | CharacterDeity (Deity m) deriving (Generic, GQLType)

resolveCharacter :: Applicative m => m (Character m)
resolveCharacter = CharacterCreature <$> resolveCreature
