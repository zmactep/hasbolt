{-# LANGUAGE RankNTypes #-}

module Database.Bolt.Lens
  ( exact
  , field
  , prop
  )
where

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import qualified Database.Bolt              as B

-- | @Getter@ from @lens@ package.
type Getter s a = forall f. (Functor f, Contravariant f) => (a -> f a) -> (s -> f s)

-- | @Fold@ from @lens@ package.
type Fold s a = forall f. (Applicative f, Contravariant f) => (a -> f a) -> (s -> f s)

-- | This 'Fold' extracts value of required type from 'B.Value'. If 'B.Value' contains wrong
-- type, 'exact' is an empty 'Fold'.
exact :: B.RecordValue a => Fold B.Value a
exact = to B.exactMaybe . _Just

-- | Extract field by given key from 'B.Record'. If there is no such key or the type is wrong,
-- this is an empty 'Fold'.
field :: B.RecordValue a => Text -> Fold B.Record a
field key = ix key . exact

-- | Extract any property from 'B.Node'. If there is no such property or the type is wrong,
-- this is an emtpy 'Fold'.
prop :: B.RecordValue a => Text -> Fold B.Node a
prop key = to B.nodeProps . ix key . exact

-- INTERNAL STUFF

-- | Simplistic implementation of @to@ from @lens@.
to :: (s -> a) -> Getter s a
to f g = contramap f . g . f

-- | Simplistic implementation of @_Just@ prism. We use this prism only in one direction,
-- so @Fold@ suffices.
_Just :: Fold (Maybe a) a
_Just f s =
  case s of
    Just a  -> s <$ f a
    Nothing -> pure s

-- | Simplistic implementation of @ix@. We don't need a full lens here, so this is just a 'Fold'.
ix :: Ord k => k -> Fold (Map k v) v
ix k = to (M.lookup k) . _Just
