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
import Unsafe (todo)

--------------------------------------------------------------------------------
-- # Miscellaneos
--------------------------------------------------------------------------------
from_check :: forall a. M (Array PlainHTML) -> Either Exc a -> M a
from_check source = case _ of
  Left (Exc exc) -> do 
    source' <- source ⊕ CodeString " . " ⊕ exc.source ⊕ pempty
    throwError (Exc exc { source = source' })
  Right a -> pure a

check_PoType_compatible_with_DataType :: PoType -> DataType -> M (Either Exc Unit)
check_PoType_compatible_with_DataType (NamedPoType x) dt = do
  todo "sub x for po; rec po dt"

check_PoType_compatible_with_DataType po (NamedDataType x) = do
  todo "sub x for dt; rec po dt"

check_PoType_compatible_with_DataType UnitPoType UnitDataType = pure (pure unit)

check_PoType_compatible_with_DataType (SumPoType _ _ _) dt = todo ""

check_PoType_compatible_with_DataType (ProdPoType _ _ _) dt = todo ""

check_PoType_compatible_with_DataType pt dt =
  throwExcM
    (CodeString "failed check_PoType_compatible_with_DataType" ⊕ pempty)
    ( ProseString "The partially-ordered type "
        ⊕ pt
        ⊕ ProseString " is not compatible with the data type "
        ⊕ dt
        ⊕ pempty
    )
