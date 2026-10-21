{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Parsing domain types out of their wire representations.

A domain type here is constructed by a parser that can fail, and by nothing else. Holding
an 'Anima.Types.Project.ProjectId' is then /evidence/ the value was checked, not a promise
that someone remembered to.

== Writing a domain type

Give the type a 'Parse' instance and derive the wire instances it should have through
'Parsed':

@
newtype BrushSize = BrushSize Double
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed BrushSize

instance Parse BrushSize where
    type Raw BrushSize = Double
    parse raw = if raw >= 0.5 && raw <= 200 then Right (BrushSize raw) else Left "expected a brush size from 0.5 to 200"
    unparse (BrushSize raw) = raw
@

and __export it abstractly__ — @BrushSize@, never @BrushSize (..)@.

== Rules

* @'parse' . 'unparse' ≡ Right@ for every instance, property-tested.
* A rejection says what was expected, never what was given: messages reach logs.
-}
module Anima.Types.Parse
    ( Parse (..)
    , Parsed (..)
    , parseM
    )
where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import Data.Text qualified as Text

{- | A type parsed out of a simpler representation.

'Raw' is what crosses the wire and sits on disk. 'parse' is the only way in; 'unparse' is
the only way out.
-}
type Parse :: Type -> Constraint
class Parse a where
    type Raw a :: Type

    -- | Reject with what was expected, not what was given.
    parse :: Raw a -> Either Text a

    unparse :: a -> Raw a

-- | 'parse' in any 'MonadFail', for aeson and anywhere a @do@ block reads better.
parseM :: (MonadFail m, Parse a) => Raw a -> m a
parseM = either (fail . Text.unpack) pure . parse

{- | Deriving-via carrier: every wire instance from the one 'Parse' instance.

Not meant to appear in a signature; it exists only long enough for GHC to coerce.
-}
type Parsed :: Type -> Type
newtype Parsed a = Parsed a

instance (FromJSON (Raw a), Parse a) => FromJSON (Parsed a) where
    parseJSON = parseJSON >=> fmap Parsed . parseM

instance (Parse a, ToJSON (Raw a)) => ToJSON (Parsed a) where
    toJSON (Parsed a) = toJSON (unparse a)

instance (FromHttpApiData (Raw a), Parse a) => FromHttpApiData (Parsed a) where
    parseUrlPiece = parseUrlPiece >=> fmap Parsed . parse

instance (Parse a, ToHttpApiData (Raw a)) => ToHttpApiData (Parsed a) where
    toUrlPiece (Parsed a) = toUrlPiece (unparse a)
