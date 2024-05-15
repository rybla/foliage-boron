module Data.OrderedMap where

import Prelude

import Control.Plus (class Alt, class Plus, alt, empty)
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Unsafe as Unsafe

-- | A map that remembers insertion order.
newtype OrderedMap k v
  = OrderedMap
  { keys :: Map k Int
  , vals :: Map Int (k /\ v)
  }

instance _Alt_OrderedMap :: Ord k => Alt (OrderedMap k) where
  alt (OrderedMap om1) (OrderedMap om2) =
    OrderedMap
      { keys: om1.keys `alt` om2.keys
      , vals: om1.vals `alt` om2.vals
      }

instance _Plus_OrderedMap :: Ord k => Plus (OrderedMap k) where
  empty = OrderedMap { keys: empty, vals: empty }

derive newtype instance _Show_OrderedMap :: (Show k, Show v) => Show (OrderedMap k v)

-- | Equality up to reordering insertion order.
instance _Eq_OrderedMap :: (Eq k, Ord k, Eq v) => Eq (OrderedMap k v) where
  eq om1 om2 = toMap om1 == toMap om2

instance _Functor_OrderedMap :: Ord k => Functor (OrderedMap k) where
  map f (OrderedMap om@{ vals }) = OrderedMap om { vals = vals # map (map f) }

instance _FunctorWithIndex_OrderedMap :: Ord k => FunctorWithIndex k (OrderedMap k) where
  mapWithIndex f (OrderedMap om@{ vals }) = OrderedMap om { vals = vals # map (\(k /\ v) -> (k /\ f k v)) }

lookup :: forall k v. Ord k => k -> OrderedMap k v -> Maybe v
lookup k (OrderedMap { keys, vals }) = do
  i <- keys # Map.lookup k
  vals # Map.lookup i # map snd

lookupIndex :: forall v. Int -> Map Int v -> v
lookupIndex i =
  Map.lookup i
    >>> Unsafe.fromJust ("[OrderedMap] the index " <> show i <> " of a key was not found in vals")

insert :: forall k v. Ord k => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v (OrderedMap { keys, vals }) =
  let
    i = Map.size keys
  in
    OrderedMap
      { keys: keys # Map.insert k i
      , vals: vals # Map.insert i (k /\ v)
      }

delete :: forall k v. Ord k => k -> OrderedMap k v -> OrderedMap k v
delete k om@(OrderedMap { keys, vals }) =
  keys
    # Map.lookup k
    # maybe
        om
        (\i -> OrderedMap { keys: keys # Map.delete k, vals: vals # Map.delete i })

alter :: forall k v. Ord k => k -> (Maybe v -> Maybe v) -> OrderedMap k v -> OrderedMap k v
alter k f om@(OrderedMap { keys, vals }) =
  keys
    # Map.lookup k
    # maybe
        -- k ISN'T in map already
        ( f empty
            # maybe
                om -- f deletes k
                (\v -> om # insert k v) -- f inserts (k => v)
        )
        -- k IS in map already
        ( \i ->
            let
              _ /\ v = vals # lookupIndex i
            in
              f (pure v)
                # maybe
                    (om # delete k) -- f deletes k
                    (\v' -> om # insert k v') -- f inserts (k => v')
        )

update :: forall k v. Ord k => k -> (v -> Maybe v) -> OrderedMap k v -> OrderedMap k v
update k f = alter k (maybe empty f)

-- update :: forall k v. Ord k => k -> ((k /\ v) -> Maybe v)
toMap :: forall k v. Ord k => OrderedMap k v -> Map k v
toMap (OrderedMap { keys, vals }) =
  keys
    # map
        ( \i ->
            Map.lookup i vals
              # Unsafe.fromJust ""
              # snd
        )

-- fromAssoc :: forall k v. List (k /\ v) -> OrderedMap k v
-- fromAssoc = foldr ?a ?a
-- | Converts the `OrderedMap` into an `Unfoldable` over items in insertion
-- | order.
toUnfoldable :: forall f k v. Unfoldable f => Ord k => OrderedMap k v -> f (k /\ v)
toUnfoldable (OrderedMap { keys, vals }) =
  keys
    # (Map.toUnfoldable :: _ -> List _)
    # unfoldr case _ of
        Nil -> empty
        Cons (k /\ i) keys' ->
          let
            v =
              Map.lookup i vals
                # Unsafe.fromJust ("[OrderedMap.toUnfoldable] the index " <> show i <> " of a key was not found in vals")
                # snd
          in
            pure ((k /\ v) /\ keys')

fromFoldable :: forall f k v. Foldable f => Ord k => f (k /\ v) -> OrderedMap k v
fromFoldable = foldl (flip (uncurry insert)) empty
