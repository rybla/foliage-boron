--------------------------------------------------------------------------------
-- # Language
--------------------------------------------------------------------------------
module Foliage.Language where

import Data.Tuple.Nested
import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, State)
import Control.Monad.State as State
import Control.Monad.Writer (Writer, WriterT, tell)
import Control.Plus (class Plus, empty)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Exists (Exists)
import Data.Identity (Identity)
import Data.List (List, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe (todo)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- ## Module
--------------------------------------------------------------------------------
data Module
  = Module
    { name :: ModuleName
    , doc :: forall m. Maybe (Mstatic_Hs m)
    , dataTypeDefs :: Map DataTypeName (DataTypeDef)
    , poTypeDefs :: Map PoTypeName (PoTypeDef)
    , functionDefs :: Map FunctionName (FunctionDef)
    , relationDefs :: Map RelationName (RelationDef)
    , ruleDefs :: Map RuleName (RuleDef)
    , fixpointDefs :: Map FixpointName (FixpointDef)
    }

--------------------------------------------------------------------------------
-- ## DataTypeDef
--------------------------------------------------------------------------------
data DataTypeDef
  = DataTypeDef
    { doc :: forall m. Maybe (Mstatic_Hs m), dataType :: DataType_
    }
  | ExternalDataTypeDef
    { doc :: forall m. Maybe (Mstatic_Hs m)
    , represented :: Exists ExternalDataTypeDef_Represented
    }

newtype ExternalDataTypeDef_Represented repr
  = DataTypeDef_Represented
  { to :: repr -> Term
  , from :: Term -> Either String repr
  , render :: forall m. Monad m => repr -> Mstatic_Hs m
  , canonical :: repr
  }

--------------------------------------------------------------------------------
-- ## PoTypeDef
--------------------------------------------------------------------------------
data PoTypeDef
  = PoTypeDef
    { doc :: forall m. Maybe (Mstatic_Hs m)
    , poType :: PoType
    }
  | ExternalPoTypeDef
    { doc :: forall m. Maybe (Mstatic_Hs m)
    , dataType :: DataType_ -- underlying dataType
    , represented :: Exists ExternalPoTypeDef_Represented
    }

newtype ExternalPoTypeDef_Represented repr
  = ExternalPoTypeDef_Represented
  { compare :: repr -> repr -> Ordering
  }

--------------------------------------------------------------------------------
-- ## FunctionDef
--------------------------------------------------------------------------------
data FunctionDef
  = ExternalFunctionDef
    { doc :: forall m. Maybe (Mstatic_Hs m)
    , label :: String
    , signature :: { inputs :: Array (DataType), output :: DataType_ }
    , implementation :: Term -> Either String Term
    }

--------------------------------------------------------------------------------
-- ## RelationDef
--------------------------------------------------------------------------------
data RelationDef
  = RelationDef
    { doc :: forall m. Maybe (Mstatic_Hs m)
    , poType :: PoType -- underlying poType
    , render :: forall m. Monad m => Term -> Mstatic_Hs m
    , canonical :: Term
    }

--------------------------------------------------------------------------------
-- ## RuleDef
--------------------------------------------------------------------------------
data RuleDef
  = RuleDef
    { doc :: forall m. Maybe (Mstatic_Hs m)
    , rule :: Rule_ MetaVarName
    }

--------------------------------------------------------------------------------
-- ## FixpointDef
--------------------------------------------------------------------------------
-- TODO: other user-specified optimizations go here
data FixpointDef
  = FixpointDef
    { doc :: forall m. Maybe (Mstatic_Hs m)
    --| The user can optimize the order in which `Prop`s of a `Relation` are
    --| used via a `PoType` over that relation's domain. The `PoType`'s orering
    --| will be used to order the `Prop`s of the `Relation` as they are
    --| inserted into the queue.
    , queue_relation_potypes :: Map RelationName (PoType)
    }

--------------------------------------------------------------------------------
-- ## Rule
--------------------------------------------------------------------------------
type Rule
  = Rule_ MetaVarName

data Rule_ x
  = Rule
    { hypotheses :: List (Hypothesis_ x)
    , conclusion :: Prop_ x
    }

--------------------------------------------------------------------------------
-- ## RipeRule
--------------------------------------------------------------------------------
type RipeRule
  = RipeRule_ MetaVarName

--| Note that `RipeRule` will only be used during interpretation, so it can
--| only exist at `Void`.
data RipeRule_ x
  = RipeRule
    { hypothesis :: Hypothesis_ x
    , rule :: Rule_ x
    }

--------------------------------------------------------------------------------
-- ## Hypothesis
--------------------------------------------------------------------------------
type Hypothesis
  = Hypothesis_ MetaVarName

data Hypothesis_ x
  = Hypothesis (Prop_ x) (Array (SideHypothesis_ x))

--------------------------------------------------------------------------------
-- ### SideHypothesis
--------------------------------------------------------------------------------
type SideHypothesis
  = SideHypothesis_ MetaVarName

data SideHypothesis_ x
  = FunctionApplicationSideHypothesis -- let x = f(a, b, c)
    { result_name :: x -- x 
    , functionEvaluation :: FunctionApplication_ x -- f(a, b, c) 
    }

-- | Corresponds to `f(a, b, c)`
type FunctionApplication
  = FunctionApplication_ MetaVarName

type FunctionApplication_ x
  = { name :: FunctionName -- f 
    , arguments :: Array (Term_ x) -- a, b, c
    }

--------------------------------------------------------------------------------
-- ## DataType
--------------------------------------------------------------------------------
type DataType
  = DataType_

data DataType_
  = UnitDataType
  | NamedDataType DataTypeName
  | SumDataType (DataType) (DataType)
  | ProdDataType (DataType) (DataType)

--------------------------------------------------------------------------------
-- ## PoType
--------------------------------------------------------------------------------
data PoType
  = UnitPoType
  | NamedPoType PoTypeName
  | SumPoType SumPoTypeOrdering (PoType) (PoType)
  | ProdPoType ProdPoTypeOrdering (PoType) (PoType)

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
  = Prop_ MetaVarName

data Prop_ x
  = Prop { name :: RelationName, term :: Term_ x }

--------------------------------------------------------------------------------
-- ## Term
--------------------------------------------------------------------------------
type Term
  = Term_ MetaVarName

data Term_ x
  = ExternalTerm { name :: DataTypeName, value :: Exists Identity }
  | MetaVarTerm x
  | UnitTerm
  | LeftTerm (Term_ x)
  | RightTerm (Term_ x)
  | PairTerm (Term_ x) (Term_ x)

--------------------------------------------------------------------------------
-- ## VarSubst
--------------------------------------------------------------------------------
type MetaVarSubst
  = MetaVarSubst_ MetaVarName

type MetaVarSubst_ x
  = Map MetaVarName (Term_ x)

class ApplyMetaVarSubst a where
  applyMetaVarSubst :: MetaVarSubst -> a -> Mdynamic a

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

saturateMetaVarSubst :: MetaVarSubst -> Mdynamic (Result MetaVarSubst)
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
--| `MetaVarName`:
--|   - `label: Left label` means that `label` is statically generated rather 
--|     than given by the user.
--|   - `label: Right label` means that `label` is given by the user.
newtype MetaVarName
  = MetaVarName
  { label :: Either String String
  , freshity :: Int
  }

derive instance _Newtype_MetaVarName :: Newtype MetaVarName _

derive newtype instance _Show_MetaVarName :: Show MetaVarName

derive newtype instance _Eq_MetaVarName :: Eq MetaVarName

derive newtype instance _Ord_MetaVarName :: Ord MetaVarName

freshenMetaVarName :: MetaVarName -> Mdynamic MetaVarName
freshenMetaVarName (MetaVarName { label }) = do
  Env { next_freshity } <- State.modify (Newtype.over Env (Record.modify _next_freshity (_ + 1)))
  pure (MetaVarName { label, freshity: next_freshity })

staticMetaVarName :: String -> MetaVarName
staticMetaVarName label = MetaVarName { label: Left label, freshity: 0 }

userMetaVarName :: String -> MetaVarName
userMetaVarName label = MetaVarName { label: Right label, freshity: 0 }

--------------------------------------------------------------------------------
-- ## Render
-- TODO: I will have to define the `Render` instances for the `Sugar = Unit`
-- versions of the syntax also.
--------------------------------------------------------------------------------
class Render m a where
  render :: a -> Mstatic_Hs m

type Hs
  = Array PlainHTML

type Mstatic_Hs m
  = Mstatic m Hs

type Mdynamic_Hs
  = Mdynamic Hs

instance _Render_PlainHTML :: Monad m => Render m PlainHTML where
  render = pure >>> pure

instance _Render_Hs :: Monad m => Render m Hs where
  render = pure

instance _Render_Mstatic_Hs :: Monad m => Render m (Mstatic_Hs m) where
  render = identity

append_render :: forall m a. Monad m => Render m a => a -> Mstatic_Hs m -> Mstatic_Hs m
append_render a m_htmls = do
  htmls <- a # render
  append htmls <$> m_htmls

infixr 5 append_render as ⊕

pempty :: forall f1 f2 a. Applicative f1 => Plus f2 => f1 (f2 a)
pempty = pure empty

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

keyval :: forall m. Monad m => Mstatic_Hs m -> Mstatic_Hs m -> Mstatic_Hs m
keyval m_key m_val = do
  key <- m_key
  val <- m_val
  pure [ HH.span [] key, HH.span [] val ]

--------------------------------------------------------------------------------
-- ## M(onad)
--------------------------------------------------------------------------------
--| The monad for interpretation computations. Provides the following effects:
--|   - `MonadReader Ctx`
--|   - `MonadWriter (Array Log)`
--|   - `MonadExcept Exc`
--|   - `MonadState Env`
--|   - `MonadAff`
--| 
--| Throwing an `Exc` corresponds to an invalid input (assuming implementation 
--| is correct).
type Mdynamic
  = Mstatic Mdynamic_

type Mdynamic_
  = StateT Env Aff

--| The monad for rendering computations. Provides the following effects:
--|   - `MonadReader Ctx`
--|   - `MonadWriter (Array Log)`
--|   - `MonadExcept Exc`
type Mstatic (m :: Type -> Type)
  = ReaderT Ctx (WriterT (Array Log) (ExceptT Exc m))

--------------------------------------------------------------------------------
-- ### Ctx
--------------------------------------------------------------------------------
newtype Ctx
  = Ctx
  { focusModule :: Module
  }

derive instance _Newtype_Ctx :: Newtype Ctx _

lookup_focusModule ::
  forall k v.
  Ord k =>
  Render Mdynamic_ k =>
  Mdynamic_Hs -> (_ -> Map k v) -> k -> Mdynamic v
lookup_focusModule source get_field name = do
  Ctx { focusModule: Module mod } <- ask
  case mod # get_field # Map.lookup name of
    Nothing -> throwExc source (Prose "Unknown " ⊕ name ⊕ pempty)
    Just v -> pure v

--------------------------------------------------------------------------------
-- ### Env
--------------------------------------------------------------------------------
newtype Env
  = Env
  { next_freshity :: Int
  , gas :: Int
  , props_queue :: List Prop
  }

_next_freshity = Proxy :: Proxy "next_freshity"

_gas = Proxy :: Proxy "gas"

_props_queue = Proxy :: Proxy "props_queue"

derive instance _Newtype_Env :: Newtype Env _

initialEnv :: forall m. Monad m => Mstatic m Env
initialEnv = todo "initialEnv"

--------------------------------------------------------------------------------
-- ### Log
--------------------------------------------------------------------------------
newtype Log
  = Log { label :: Hs, message :: Hs }

tellLog :: forall m. Monad m => Mstatic_Hs m -> Mstatic_Hs m -> Mstatic m Unit
tellLog m_label m_message = do
  label <- m_label
  message <- m_message
  tell [ Log { label, message } ]

--------------------------------------------------------------------------------
-- ### Exc
--------------------------------------------------------------------------------
newtype Exc
  = Exc { source :: Hs, message :: Hs }

throwExc :: forall m a. Monad m => Mstatic_Hs m -> Mstatic_Hs m -> Mstatic m a
throwExc m_source m_message = do
  source <- m_source
  message <- m_message
  throwError (Exc { source, message })

--------------------------------------------------------------------------------
-- ## Result
--------------------------------------------------------------------------------
type Result
  = Either Exc
