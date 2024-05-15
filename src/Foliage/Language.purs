--------------------------------------------------------------------------------
-- # Language
--------------------------------------------------------------------------------
module Foliage.Language where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Plus (class Plus, empty)
import Data.Either (Either)
import Data.Exists (Exists)
import Data.Identity (Identity)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Unsafe (todo)

--------------------------------------------------------------------------------
-- ## Stage
--------------------------------------------------------------------------------
-- | Stage where syntax includes syntax sugar.
type SugarStage
  = Unit

-- | Stage where syntax is fully expanded.
type CoreStage
  = Void

--------------------------------------------------------------------------------
-- ## Module
--------------------------------------------------------------------------------
type Module
  = Module_ CoreStage

data Module_ (stage :: Type)
  = Module
    { name :: ModuleName
    , doc :: forall m. Maybe (MRender_Hs m)
    , dataTypeDefs :: Map DataTypeName (DataTypeDef_ stage)
    , poTypeDefs :: Map PoTypeName (PoTypeDef_ stage)
    , functionDefs :: Map FunctionName (FunctionDef_ stage)
    , relationDefs :: Map RelationName (RelationDef_ stage)
    , ruleDefs :: Map RuleName (RuleDef_ stage)
    , fixpointDefs :: Map FixpointName (FixpointDef_ stage)
    }

--------------------------------------------------------------------------------
-- ## DataTypeDef
--------------------------------------------------------------------------------
type DataTypeDef
  = DataTypeDef_ CoreStage

data DataTypeDef_ (stage :: Type)
  = DataTypeDef
    { doc :: forall m. Maybe (MRender_Hs m), dataType :: DataType_ stage
    }
  | ExternalDataTypeDef
    { doc :: forall m. Maybe (MRender_Hs m)
    , represented :: Exists ExternalDataTypeDef_Represented
    }

newtype ExternalDataTypeDef_Represented repr
  = DataTypeDefRepresented
  { to :: repr -> Term
  , from :: Term -> Either String repr
  , render :: forall m. Monad m => repr -> MRender_Hs m
  , canonical :: repr
  }

--------------------------------------------------------------------------------
-- ## PoTypeDef
--------------------------------------------------------------------------------
type PoTypeDef
  = PoTypeDef_ CoreStage

data PoTypeDef_ (stage :: Type)
  = PoTypeDef
    { doc :: forall m. Maybe (MRender_Hs m)
    , poType :: PoType_ stage
    }
  | ExternalPoTypeDef
    { doc :: forall m. Maybe (MRender_Hs m)
    , dataType :: DataType_ stage -- underlying dataType
    , represented :: Exists ExternalPoTypeDef_Represented
    }

newtype ExternalPoTypeDef_Represented repr
  = ExternalPoTypeDefRepresented
  { compare :: repr -> repr -> Ordering
  }

--------------------------------------------------------------------------------
-- ## FunctionDef
--------------------------------------------------------------------------------
type FunctionDef
  = FunctionDef_ CoreStage

data FunctionDef_ (stage :: Type)
  = ExternalFunctionDef
    { doc :: forall m. Maybe (MRender_Hs m)
    , label :: String
    , signature :: { inputs :: Array (DataType_ stage), output :: DataType_ stage }
    , implementation :: Term -> Either String Term
    }

--------------------------------------------------------------------------------
-- ## RelationDef
--------------------------------------------------------------------------------
type RelationDef
  = RelationDef_ CoreStage

data RelationDef_ (stage :: Type)
  = RelationDef
    { doc :: forall m. Maybe (MRender_Hs m)
    , poType :: PoType_ stage -- underlying poType
    , render :: forall m. Monad m => Term -> MRender_Hs m
    , canonical :: Term
    }

--------------------------------------------------------------------------------
-- ## RuleDef
--------------------------------------------------------------------------------
type RuleDef
  = RuleDef_ CoreStage

data RuleDef_ (stage :: Type)
  = RuleDef
    { doc :: forall m. Maybe (MRender_Hs m)
    , rule :: Rule_ stage MetaVarName
    }

--------------------------------------------------------------------------------
-- ## FixpointDef
--------------------------------------------------------------------------------
-- TODO: other user-specified optimizations go here
type FixpointDef
  = FixpointDef_ CoreStage

data FixpointDef_ (stage :: Type)
  = FixpointDef
    { doc :: forall m. Maybe (MRender_Hs m)
    -- | The user can optimize the order in which `Prop`s of a `Relation` are
    -- | used via a `PoType` over that relation's domain. The `PoType`'s orering
    -- | will be used to order the `Prop`s of the `Relation` as they are
    -- | inserted into the queue.
    , queue_relation_potypes :: Map RelationName (PoType_ stage)
    }

--------------------------------------------------------------------------------
-- ## Rule
--------------------------------------------------------------------------------
type Rule
  = Rule_ CoreStage MetaVarName

data Rule_ (stage :: Type) x
  = Rule
    { hypotheses :: List (Hypothesis_ stage x)
    , conclusion :: Prop_ stage x
    }

--------------------------------------------------------------------------------
-- ## RipeRule
--------------------------------------------------------------------------------
type RipeRule
  = RipeRule_ MetaVarName

-- | Note that `RipeRule` will only be used during interpretation, so it can
-- | only exist at `CoreStage`.
data RipeRule_ x
  = RipeRule
    { hypothesis :: Hypothesis_ CoreStage x
    , rule :: Rule_ CoreStage x
    }

--------------------------------------------------------------------------------
-- ## Hypothesis
--------------------------------------------------------------------------------
type Hypothesis
  = Hypothesis_ CoreStage MetaVarName

data Hypothesis_ (stage :: Type) x
  = Hypothesis (Prop_ stage x) (Array (SideHypothesis_ stage x))

--------------------------------------------------------------------------------
-- ### SideHypothesis
--------------------------------------------------------------------------------
type SideHypothesis
  = SideHypothesis_ CoreStage MetaVarName

data SideHypothesis_ (stage :: Type) x
  = FunctionEvaluationSideHypothesis -- let x = f(a, b, c)
    { result_name :: x -- x 
    , function_name :: FunctionName -- f 
    , arguments :: Array (Term_ stage x) -- a, b, c
    }

--------------------------------------------------------------------------------
-- ## DataType
--------------------------------------------------------------------------------
type DataType
  = DataType_ CoreStage

data DataType_ (stage :: Type)
  = UnitDataType
  | NamedDataType DataTypeName
  | SumDataType (DataType_ stage) (DataType_ stage)
  | ProdDataType (DataType_ stage) (DataType_ stage)

--------------------------------------------------------------------------------
-- ## PoType
--------------------------------------------------------------------------------
type PoType
  = PoType_ CoreStage

data PoType_ (stage :: Type)
  = UnitPoType
  | NamedPoType PoTypeName
  | SumPoType SumPoTypeOrdering (PoType_ stage) (PoType_ stage)
  | ProdPoType ProdPoTypeOrdering (PoType_ stage) (PoType_ stage)

data SumPoTypeOrdering
  = LeftGreaterThanRight_SumPoTypeOrdering
  | LeftLessThanRight_SumPoTypeOrdering
  | LeftIncomparableRight_SumPoTypeOrdering
  | LeftEqualRight_SumPoTypeOrdering

data ProdPoTypeOrdering
  = FirstThenSecond_ProdPoTypeOrdering
  | SecondThenFirst_ProdPoTypeOrdering
  | FirstAndSecond_ProdPoTypeOrdering

--------------------------------------------------------------------------------
-- ## Prop
--------------------------------------------------------------------------------
type Prop
  = Prop_ CoreStage MetaVarName

data Prop_ (stage :: Type) x
  = Prop { name :: RelationName, term :: Term_ stage x }

--------------------------------------------------------------------------------
-- ## Term
--------------------------------------------------------------------------------
type Term
  = Term_ CoreStage MetaVarName

data Term_ (stage :: Type) x
  = ExternalTerm { name :: DataTypeName, value :: Exists Identity }
  | MetaVarTerm x
  | UnitTerm
  | LeftTerm (Term_ stage x)
  | RightTerm (Term_ stage x)
  | PairTerm (Term_ stage x) (Term_ stage x)

--------------------------------------------------------------------------------
-- ## VarSubst
--------------------------------------------------------------------------------
type MetaVarSubst
  = MetaVarSubst_ MetaVarName

type MetaVarSubst_ x
  = Map MetaVarName (Term_ CoreStage x)

class ApplyMetaVarSubst a where
  applyMetaVarSubst :: MetaVarSubst -> a -> MInterp a

instance _ApplyMetaVarSubst_Term_Term :: ApplyMetaVarSubst Term where
  applyMetaVarSubst = todo "ApplyMetaVarSubst Term Term"

instance _ApplyMetaVarSubst_Rule_Rule :: ApplyMetaVarSubst Rule where
  applyMetaVarSubst = todo "ApplyMetaVarSubst Rule Rule"

instance _ApplyMetaVarSubst_RipeRule_RipeRule :: ApplyMetaVarSubst RipeRule where
  applyMetaVarSubst = todo "ApplyMetaVarSubst RipeRule RipeRule"

instance _ApplyMetaVarSubst_Hypothesis_Hypothesis :: ApplyMetaVarSubst Hypothesis where
  applyMetaVarSubst = todo "ApplyMetaVarSubst Hypothesis Hypothesis"

instance _ApplyMetaVarSubst_SideHypothesis_SideHypothesis :: ApplyMetaVarSubst SideHypothesis where
  applyMetaVarSubst = todo "ApplyMetaVarSubst SideHypothesis SideHypothesis"

instance _ApplyMetaVarSubst_Prop_Prop :: ApplyMetaVarSubst Prop where
  applyMetaVarSubst = todo "ApplyMetaVarSubst Prop Prop"

saturateMetaVarSubst :: MetaVarSubst -> MInterp (Result MetaVarSubst)
saturateMetaVarSubst = todo "saturateMetaVarSubst"

--------------------------------------------------------------------------------
-- ## StaticName
--------------------------------------------------------------------------------
newtype StaticName (sort :: Symbol)
  = StaticName String

derive instance _Newtype_StaticName :: Newtype (StaticName sort) _

derive newtype instance _Show_StaticName :: Show (StaticName sort)

derive newtype instance _Eq_StaticName :: Eq (StaticName sort)

derive newtype instance _Ord_StaticName :: Ord (StaticName sort)

type ModuleName
  = StaticName "Module"

type DataTypeName
  = StaticName "DataType"

type PoTypeName
  = StaticName "PoType"

type FunctionName
  = StaticName "Function"

type RelationName
  = StaticName "Relation"

type RuleName
  = StaticName "Rule"

type FixpointName
  = StaticName "Fixpoint"

--------------------------------------------------------------------------------
-- ## MetaVarName
--------------------------------------------------------------------------------
newtype MetaVarName
  = MetaVarName { label :: String, freshity :: Int }

derive instance _Newtype_MetaVarName :: Newtype MetaVarName _

derive newtype instance _Show_MetaVarName :: Show MetaVarName

derive newtype instance _Eq_MetaVarName :: Eq MetaVarName

derive newtype instance _Ord_MetaVarName :: Ord MetaVarName

--------------------------------------------------------------------------------
-- ## M(onad)
--------------------------------------------------------------------------------
-- | The monad for interpretation computations. Has the following effects:
-- |   - `MonadReader Ctx`
-- |   - `MonadWriter (Array Log)`
-- |   - `MonadExcept Exc`
-- |   - `MonadState Env`
-- |   - `MonadAff`
-- | 
-- | Throwing an `Exc` corresponds to an invalid input (assuming implementation 
-- | is correct).
type MInterp
  = MRender MInterp_

type MInterp_
  = StateT Env Aff

-- | The monad for rendering computations. Has the following effects:
-- |   - `MonadReader Ctx`
-- |   - `MonadWriter (Array Log)`
-- |   - `MonadExcept Exc`
type MRender (m :: Type -> Type)
  = ReaderT Ctx (ExceptT Exc (WriterT (Array Log) m))

--------------------------------------------------------------------------------
-- ### Ctx
--------------------------------------------------------------------------------
newtype Ctx
  = Ctx
  { focusModule :: Module
  }

lookup_focusModule ::
  forall k v.
  Ord k =>
  Render MInterp_ k =>
  MInterp_Hs -> (_ -> Map k v) -> k -> MInterp v
lookup_focusModule source get_field name = do
  Ctx { focusModule: Module mod } <- ask
  case mod # get_field # Map.lookup name of
    Nothing -> do
      source <- source
      message <- Prose "Unknown " ⊕ name ⊕ pempty
      throwError $ Exc { source, message }
    Just v -> pure v

--------------------------------------------------------------------------------
-- ### Env
--------------------------------------------------------------------------------
newtype Env
  = Env {}

--------------------------------------------------------------------------------
-- ### Log
--------------------------------------------------------------------------------
data Log
  = Log { label :: Hs, message :: Hs }

--------------------------------------------------------------------------------
-- ### Exc
--------------------------------------------------------------------------------
data Exc
  = Exc { source :: Hs, message :: Hs }

throwExcM :: forall m a. Monad m => MRender_Hs m -> MRender_Hs m -> MRender m a
throwExcM m_source m_message = do
  source <- m_source
  message <- m_message
  throwError (Exc { source, message })

--------------------------------------------------------------------------------
-- ## Render
--------------------------------------------------------------------------------
class Render m a where
  render :: a -> MRender_Hs m

type Hs
  = Array PlainHTML

type MRender_Hs m
  = MRender m Hs

type MInterp_Hs
  = MInterp Hs

instance _Render_PlainHTML :: Monad m => Render m PlainHTML where
  render = pure >>> pure

instance _Render_Hs :: Monad m => Render m Hs where
  render = pure

instance _Render_MRender_Hs :: Monad m => Render m (MRender_Hs m) where
  render = identity

append_render :: forall m a. Monad m => Render m a => a -> MRender_Hs m -> MRender_Hs m
append_render a m_htmls = do
  htmls <- a # render
  append htmls <$> m_htmls

infixr 5 append_render as ⊕

pempty :: forall f1 f2 a. Applicative f1 => Plus f2 => f1 (f2 a)
pempty = pure empty

newtype Prose
  = Prose String

instance _Render_Prose :: Monad m => Render m Prose where
  render (Prose s) = [ HH.span [] [ HH.text s :: PlainHTML ] ] ⊕ pempty

newtype Literal
  = Literal String

instance _Render_Literal :: Monad m => Render m Literal where
  render (Literal s) = [ HH.span [] [ HH.text s :: PlainHTML ] ] ⊕ pempty

newtype Code
  = Code String

instance _Render_Code :: Monad m => Render m Code where
  render (Code s) = [ HH.span [] [ HH.text s :: PlainHTML ] ] ⊕ pempty

instance _Render_Module :: Monad m => Render m Module where
  render = todo "Render Module"

instance _Render_DataTypeDef :: Monad m => Render m DataTypeDef where
  render = todo "Render DataTypeDef"

instance _Render_DataType :: Monad m => Render m DataType where
  render = todo "Render DataType"

instance _Render_PoType :: Monad m => Render m PoType where
  render = todo "Render PoType"

instance _Render_PoTypeDef :: Monad m => Render m PoTypeDef where
  render = todo "Render PoTypeDef"

instance _Render_FunctionDef :: Monad m => Render m FunctionDef where
  render = todo "Render FunctionDef"

instance _Render_RelationDef :: Monad m => Render m RelationDef where
  render = todo "Render RelationDef"

instance _Render_RuleDef :: Monad m => Render m RuleDef where
  render = todo "Render RuleDef"

instance _Render_Term :: Monad m => Render m Term where
  render = todo "Render Term"

instance _Render_Rule :: Monad m => Render m Rule where
  render = todo "Render Rule"

instance _Render_Hypothesis :: Monad m => Render m Hypothesis where
  render = todo "Render Hypothesis"

instance _Render_SideHypothesis :: Monad m => Render m SideHypothesis where
  render = todo "Render SideHypothesis"

instance _Render_Prop :: Monad m => Render m Prop where
  render = todo "Render Prop"

instance _Render_StaticName :: Monad m => Render m (StaticName sort) where
  render = todo "Render StaticName"

instance _Render_MetaVarName :: Monad m => Render m MetaVarName where
  render = todo "Render MetaVarName"

--------------------------------------------------------------------------------
-- ## Result
--------------------------------------------------------------------------------
type Result
  = Either Exc
