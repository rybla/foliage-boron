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
-- ## Meta
--------------------------------------------------------------------------------
newtype Documented a
  = Documented { it :: a, doc :: forall m. Monad m => Maybe (MHs m) }

--------------------------------------------------------------------------------
-- ## Module
--------------------------------------------------------------------------------
data Module
  = Module
    { name :: StaticName
    , datatypeDefs :: Map StaticName DataTypeDef
    , potypeDefs :: Map StaticName PoTypeDef
    , functionDefs :: Map StaticName FunctionDef
    , relationDefs :: Map StaticName RelationDef
    , ruleDefs :: Map StaticName RuleDef
    }

--------------------------------------------------------------------------------
-- ## DataTypeDef
--------------------------------------------------------------------------------
data DataTypeDef
  = DataTypeDef DataType
  | ExternalDataTypeDef
    { represented :: Exists ExternalDataTypeDef_Represented
    }

newtype ExternalDataTypeDef_Represented repr
  = DataTypeDefRepresented
  { to :: repr -> Term
  , from :: Term -> Either String repr
  , render :: forall m. Monad m => repr -> MHs m
  , canonical :: repr
  }

--------------------------------------------------------------------------------
-- ## DataType
--------------------------------------------------------------------------------
data DataType
  = UnitDataType
  | NamedDataType StaticName
  | SumDataType DataType DataType
  | ProdDataType DataType DataType


--------------------------------------------------------------------------------
-- ## PoTypeDef
--------------------------------------------------------------------------------
data PoTypeDef
  = PoTypeDef PoType
  | ExternalPoTypeDef
    { datatype :: DataType -- underlying datatype
    , represented :: Exists ExternalPoTypeDef_Represented
    }

newtype ExternalPoTypeDef_Represented repr
  = ExternalPoTypeDefRepresented
  { compare :: repr -> repr -> Ordering
  }

--------------------------------------------------------------------------------
-- ## PoType
--------------------------------------------------------------------------------
data PoType
  = UnitPoType
  | NamedPoType StaticName
  | SumPoType SumPoTypeOrdering PoType PoType
  | ProdPoType ProdPoTypeOrdering PoType PoType

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
-- ## FunctionDef
--------------------------------------------------------------------------------
data FunctionDef
  = ExternalFunctionDef
    { label :: String
    , signature :: { inputs :: Array DataType, output :: DataType }
    , implementation :: Term -> Either String Term
    }

--------------------------------------------------------------------------------
-- ## RelationDef
--------------------------------------------------------------------------------
data RelationDef
  = RelationDef
    { potype :: PoType -- underlying potype
    , render :: forall m. Monad m => Term -> MHs m
    , canonical :: Term
    }

--------------------------------------------------------------------------------
-- ## RuleDef
--------------------------------------------------------------------------------
data RuleDef
  = RuleDef
    { rule :: Rule
    }

--------------------------------------------------------------------------------
-- ## Rule
--------------------------------------------------------------------------------
type Rule
  = RuleF VarName

data RuleF x
  = Rule
    { hypotheses :: List (HypothesisF x)
    , conclusion :: PropF x
    }

--------------------------------------------------------------------------------
-- ## Hypothesis
--------------------------------------------------------------------------------
type Hypothesis
  = HypothesisF VarName

data HypothesisF x
  = Hypothesis (PropF x) (Array (SideHypothesisF x))

--------------------------------------------------------------------------------
-- ### SideHypothesis
--------------------------------------------------------------------------------
type SideHypothesis
  = SideHypothesisF VarName

data SideHypothesisF x
  = FunctionEvaluationSideHypothesis -- let x = f(a, b, c)
    { result_name :: x
    , function_name :: StaticName
    , arguments :: Array (TermF x)
    }

--------------------------------------------------------------------------------
-- ## Prop
--------------------------------------------------------------------------------
type Prop
  = PropF VarName

data PropF x
  = Prop { prop_name :: StaticName, term :: TermF x }

--------------------------------------------------------------------------------
-- ## Term
--------------------------------------------------------------------------------
type Term
  = TermF VarName

data TermF x
  = ExternalTerm { externalDataTypeName :: StaticName, value :: Exists Identity }
  | VarTerm x
  | UnitTerm
  | LeftTerm (TermF x)
  | RightTerm (TermF x)
  | PairTerm (TermF x) (TermF x)

--------------------------------------------------------------------------------
-- ## StaticName
--------------------------------------------------------------------------------
newtype StaticName
  = StaticName String

derive instance _Newtype_StaticName :: Newtype StaticName _

derive newtype instance _Show_StaticName :: Show StaticName

derive newtype instance _Eq_StaticName :: Eq StaticName

derive newtype instance _Ord_StaticName :: Ord StaticName

--------------------------------------------------------------------------------
-- ## VarName
--------------------------------------------------------------------------------
newtype VarName
  = VarName { label :: String, freshity :: Int }

derive instance _Newtype_VarName :: Newtype VarName _

derive newtype instance _Show_VarName :: Show VarName

derive newtype instance _Eq_VarName :: Eq VarName

derive newtype instance _Ord_VarName :: Ord VarName

--------------------------------------------------------------------------------
-- ## M(onad)
--------------------------------------------------------------------------------
type M
  = ReaderT Ctx (WriterT (Array Log) (StateT Env (ExceptT Exc Aff)))

type M_Ctx (m :: Type -> Type)
  = ReaderT Ctx m

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
  Render k =>
  Hs -> (_ -> Map k v) -> k -> M v
lookup_focusModule source get_field name = do
  Ctx { focusModule: Module mod } <- ask
  case mod # get_field # Map.lookup name of
    Nothing -> todo "" -- throwError $ Exc { source, message: [ HH.text $ "Unknown " ] <> render name }
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

throwExcM :: forall a. MHs _ -> MHs _ -> M a
throwExcM m_source m_message = do
  source <- m_source
  message <- m_message
  throwError (Exc { source, message })

--------------------------------------------------------------------------------
-- ## Render
--------------------------------------------------------------------------------
class Render a where
  render :: forall m. Monad m => a -> MHs m

type Hs
  = Array PlainHTML

type MHs m
  = M_Ctx m Hs

append_render :: forall m a. Monad m => Render a => a -> MHs m -> MHs m
append_render a m_htmls = do
  htmls <- a # render
  append htmls <$> m_htmls

infixr 5 append_render as ⊕

instance _Render_PlainHTML :: Render PlainHTML where
  render = pure >>> pure

instance _Render_Array_PlainHTML :: Render Hs where
  render = pure

pempty :: forall f1 f2 a. Applicative f1 => Plus f2 => f1 (f2 a)
pempty = pure empty

newtype ProseString
  = ProseString String

instance _Render_ProseString :: Render ProseString where
  render (ProseString s) = [ HH.span [] [ HH.text s :: PlainHTML ] ] ⊕ pempty

newtype ExternalString
  = ExternalString String

instance _Render_ExternalString :: Render ExternalString where
  render (ExternalString s) = [ HH.span [] [ HH.text s :: PlainHTML ] ] ⊕ pempty

newtype CodeString
  = CodeString String

instance _Render_CodeString :: Render CodeString where
  render (CodeString s) = [ HH.span [] [ HH.text s :: PlainHTML ] ] ⊕ pempty

instance _Render_Module :: Render Module where
  render = todo "Render Module"

instance _Render_DataTypeDef :: Render DataTypeDef where
  render = todo "Render DataTypeDef"

instance _Render_DataType :: Render DataType where
  render = todo "Render DataType"

instance _Render_PoType :: Render PoType where
  render = todo "Render PoType"

instance _Render_PoTypeDef :: Render PoTypeDef where
  render = todo "Render PoTypeDef"

instance _Render_FunctionDef :: Render FunctionDef where
  render = todo "Render FunctionDef"

instance _Render_RelationDef :: Render RelationDef where
  render = todo "Render RelationDef"

instance _Render_RuleDef :: Render RuleDef where
  render = todo "Render RuleDef"

instance _Render_Term :: Render Term where
  render = todo "Render Term"

instance _Render_Rule :: Render Rule where
  render = todo "Render Rule"

instance _Render_Hypothesis :: Render Hypothesis where
  render = todo "Render Hypothesis"

instance _Render_SideHypothesis :: Render SideHypothesis where
  render = todo "Render SideHypothesis"

instance _Render_Prop :: Render Prop where
  render = todo "Render Prop"

instance _Render_StaticName :: Render StaticName where
  render = todo "Render StaticName"

instance _Render_VarName :: Render VarName where
  render = todo "Render VarName"
