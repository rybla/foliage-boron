module Foliage.Interpretation where

import Data.Tuple.Nested
import Foliage.Language
import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, asks)
import Control.Monad.State (StateT)
import Control.Monad.State as State
import Control.Monad.Writer (WriterT)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Debug as Debug
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Record as Record
import Unsafe (todo)

--------------------------------------------------------------------------------
-- # Interpret
--------------------------------------------------------------------------------
interpret :: Mstatic Aff Env
interpret = do
  Debug.traceM "[Interpretation.interpret]"
  todo "interpret"

interpret_loop :: Mdynamic Unit
interpret_loop = do
  Env { gas, props_queue } <- State.modify (Newtype.over Env (Record.modify _gas (_ - 1)))
  when (gas <= 0) do
    throwExc
      (Code "interpret_loop" ⊕ pempty)
      (Prose "ran out of gas" ⊕ pempty)
  -- TODO: if a queued prop left, then use it, else done
  case props_queue of
    Nil -> do
      tellLog
        (Prose "reached fixpoint" ⊕ pempty)
        ((keyval (Code "gas" ⊕ pempty) (Code (show gas) ⊕ pempty) :: Mdynamic_Hs) ⊕ pempty)
      pure unit
    Cons prop props -> do
      todo "interpret_fixpoint"

--------------------------------------------------------------------------------
-- # Compare, Ordering
--------------------------------------------------------------------------------
--| A partial meta ordering. 
--|   - "partial" -- can be unordered
--|   - "meta" -- can substitute metavariables
type PartialMetaOrdering
  = Maybe (Ordering /\ MetaVarSubst)

class PartialMetaCompare a where
  partialMetaCompare :: a -> a -> Mdynamic PartialMetaOrdering

instance _PartialMetaCompare_Prop :: PartialMetaCompare Prop where
  partialMetaCompare = todo "PartialMetaCompare Prop"

instance _PartialMetaCompare_PoType_and_Term :: PartialMetaCompare (PoType /\ Term) where
  partialMetaCompare = todo "PartialMetaCompare (PoType /\\ Term)"

--------------------------------------------------------------------------------
-- # Miscellaneous
--------------------------------------------------------------------------------
from_check :: forall a. Mdynamic_Hs -> Result a -> Mdynamic a
from_check source = case _ of
  Left (Exc exc) -> do
    source' <- source ⊕ Code " . " ⊕ exc.source ⊕ pempty
    throwError (Exc exc { source = source' })
  Right a -> pure a

check_PoType_compatible_with_DataType :: PoType -> DataType -> Mdynamic (Result Unit)
check_PoType_compatible_with_DataType (NamedPoType x) dt = do
  todo "sub x for po; rec po dt"

check_PoType_compatible_with_DataType po (NamedDataType x) = do
  todo "sub x for dt; rec po dt"

check_PoType_compatible_with_DataType UnitPoType UnitDataType = pure (pure unit)

check_PoType_compatible_with_DataType (SumPoType _ _ _) dt = todo ""

check_PoType_compatible_with_DataType (ProdPoType _ _ _) dt = todo ""

check_PoType_compatible_with_DataType pt dt =
  throwExc
    (Code "failed check_PoType_compatible_with_DataType" ⊕ pempty)
    ( Prose "The partially-ordered type "
        ⊕ pt
        ⊕ Prose " is not compatible with the data type "
        ⊕ dt
        ⊕ pempty
    )
