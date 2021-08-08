{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Mutation where

import Data.Morpheus.Kind (ENUM, INPUT, SCALAR)
import Data.Morpheus.Types
  ( GQLScalar (..),
    GQLType (..),
    IORes,
    ScalarValue (..),
  )
import Data.Text (Text)
import GHC.Generics (Generic)

type ApiEvent = ()

type ApiRes = IORes ApiEvent

---- GQL Deity -------------------------------
data Deity (m :: * -> *) = Deity
  { fullName :: () -> m Text,
    power :: () -> m Power
  }
  deriving (Generic, GQLType)

---- GQL Human -------------------------------
data Human (m :: * -> *) = Human
  { humanName :: () -> m Text,
    profession :: () -> m (Maybe Text)
  }
  deriving (Generic, GQLType)

---- GQL City -------------------------------
data City
  = Athens
  | Ithaca
  | Sparta
  | Troy
  deriving (Generic)

instance GQLType City where
  type KIND City = ENUM

---- GQL Power -------------------------------
data Power
  = Power Int Int

instance GQLScalar Power where
  parseValue _ = pure (Power 0 0)
  serialize (Power x y) = Int (x + y)

instance GQLType Power where
  type KIND Power = SCALAR

---- GQL Realm -------------------------------
data Realm = Realm
  { owner :: Text,
    place :: Maybe Int
  }
  deriving (Generic)

instance GQLType Realm where
  type KIND Realm = INPUT

---- GQL Creature -------------------------------
data Creature (m :: * -> *) = Creature
  { creatureName :: () -> m Text,
    realm :: () -> m City
  }
  deriving (Generic, GQLType)

---- GQL Character -------------------------------
data Character (m :: * -> *)
  = CharacterCreature (Creature m)
  | CharacterDeity (Deity m)
  | CharacterHuman (Human m)
  deriving (Generic, GQLType)
