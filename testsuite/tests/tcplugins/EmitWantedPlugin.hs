{-# LANGUAGE RecordWildCards #-}

module EmitWantedPlugin where

-- base
import Data.Maybe
  ( catMaybes )

-- ghc
import GHC.Builtin.Types
  ( unitTy )
import GHC.Core
  ( Expr(Type) )
import GHC.Core.Class
  ( Class(..) )
import GHC.Core.Coercion
  ( mkSymCo, mkSubCo, mkPrimEqPred )
import GHC.Core.DataCon
  ( classDataCon )
import GHC.Core.Make
  ( mkCoreConApps, unitExpr )
import GHC.Core.Type
  ( eqType )
import GHC.Core.Utils
  ( mkCast )
import GHC.Plugins
  ( Plugin )
import GHC.Tc.Plugin
  ( TcPluginM, newWanted )
import GHC.Tc.Types
  ( TcPluginSolveResult(..) )
import GHC.Tc.Types.Constraint
  ( Ct(..), ctLoc, ctEvCoercion, mkNonCanonical )
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm(EvExpr) )

-- common
import Common
  ( PluginDefs(..), mkPlugin, don'tRewrite )

--------------------------------------------------------------------------------

-- This plugin emits a new Wanted constraint @ty ~# ()@ whenever it encounters
-- a Wanted constraint of the form @MyClass ty@, and uses the coercion hole
-- from the @ty ~# ()@ constraint to solve the @MyClass ty@ constraint.
--
-- This is used to check that unsolved Wanted constraints are reported
-- with the correct source location information.

plugin :: Plugin
plugin = mkPlugin solver don'tRewrite

-- Find Wanteds of the form @MyClass ty@ for some type @ty@,
-- emits a new Wanted equality @ty ~ ()@, and solves the
-- @MyClass ty@ constraint using it.
solver :: [String]
       -> PluginDefs -> EvBindsVar -> [Ct] -> [Ct]
       -> TcPluginM TcPluginSolveResult
solver args defs _ev _gs ws = do
  (solved, new) <- unzip . catMaybes <$> traverse ( solveCt defs ) ws
  pure $ TcPluginOk solved new

solveCt :: PluginDefs -> Ct -> TcPluginM ( Maybe ( (EvTerm, Ct), Ct ) )
solveCt ( PluginDefs {..} ) ct@( CDictCan { cc_class, cc_tyargs } )
  | className cc_class == className myClass
  , [tyArg] <- cc_tyargs
  = do
      new_wanted_ctev <- newWanted (ctLoc ct) (mkPrimEqPred tyArg unitTy)
      let
        -- co :: tyArg ~# ()
        co = ctEvCoercion new_wanted_ctev
        new_wanted_ct = mkNonCanonical new_wanted_ctev
        ev_term = EvExpr $
          mkCoreConApps ( classDataCon myClass )
            [ Type tyArg, mkCast unitExpr (mkSubCo $ mkSymCo co) ]
      pure $ Just ( ( ev_term, ct ), new_wanted_ct )
solveCt _ ct = pure Nothing
