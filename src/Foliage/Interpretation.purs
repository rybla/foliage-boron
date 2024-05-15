module Foliage.Interpretation where

import Foliage.Language
import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, asks)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Data.Tuple.Nested
import Unsafe (todo)

--------------------------------------------------------------------------------
-- # Interpret
--------------------------------------------------------------------------------
interpret :: Mdynamic Unit
interpret = todo "interpret"

--------------------------------------------------------------------------------
-- # Compare, Ordering
--------------------------------------------------------------------------------
--| A partial meta ordering. 
--| - partial: can be unordered
--| - meta: can substitute metavariables
type PartialMetaOrdering
  = Maybe (Ordering /\ MetaVarSubst)

class PartialMetaCompare a where
  partialMetaCompare :: a -> a -> Mdynamic PartialMetaOrdering

instance _PartialMetaCompare_Prop :: PartialMetaCompare Prop where
  partialMetaCompare = todo "PartialMetaCompare Prop"

instance _PartialMetaCompare_PoType_and_Term :: PartialMetaCompare (PoType /\ Term) where
  partialMetaCompare = todo "PartialMetaCompare (PoType /\\ Term)"

--------------------------------------------------------------------------------
-- # Miscellaneos
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
  throwExcM
    (Code "failed check_PoType_compatible_with_DataType" ⊕ pempty)
    ( Prose "The partially-ordered type "
        ⊕ pt
        ⊕ Prose " is not compatible with the data type "
        ⊕ dt
        ⊕ pempty
    )
