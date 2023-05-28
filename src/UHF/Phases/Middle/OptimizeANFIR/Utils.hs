module UHF.Phases.Middle.OptimizeANFIR.Utils (ANFIR, iterate_over_all_subexpressions) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ANFIR as ANFIR

type ANFIR = ANFIR.ANFIR () (Maybe (Type.Type Void)) ()
type Binding = ANFIR.Binding () (Maybe (Type.Type Void)) ()

iterate_over_bindings :: Monad m => (Binding -> m Binding) -> ANFIR -> m ANFIR
iterate_over_bindings change (ANFIR.ANFIR decls adts type_synonyms vars bindings params mod) =
    runStateT (do_module (Arena.get decls mod)) bindings >>= \ ((), bindings) ->
    pure (ANFIR.ANFIR decls adts type_synonyms vars bindings params mod)
    where
        do_module (ANFIR.Decl'Module group _ _) = do_group group
        do_module (ANFIR.Decl'Type _) = pure () -- should not happen

        do_group (ANFIR.BindingGroup _ _ bindings) = mapM_ do_binding bindings

        -- ideally would use modifyM but that is not in the transformers package of this stackage snapshot
        do_binding bk = StateT $ \ bindings -> ((),) <$> Arena.modifyM bindings bk change

iterate_over_all_subexpressions :: Monad m => (ANFIR.BindingKey -> m ANFIR.BindingKey) -> ANFIR -> m ANFIR
iterate_over_all_subexpressions modify = iterate_over_bindings do_binding
    where
        do_binding (ANFIR.Binding bw init) = ANFIR.Binding bw <$> do_expr init

        do_expr (ANFIR.Expr'Refer id ty bk) = modify bk >>= \ bk -> pure (ANFIR.Expr'Refer id ty bk)

        do_expr (ANFIR.Expr'Int id ty i) = pure (ANFIR.Expr'Int id ty i)
        do_expr (ANFIR.Expr'Float id ty r) = pure (ANFIR.Expr'Float id ty r)
        do_expr (ANFIR.Expr'Bool id ty b) = pure (ANFIR.Expr'Bool id ty b)
        do_expr (ANFIR.Expr'Char id ty c) = pure (ANFIR.Expr'Char id ty c)
        do_expr (ANFIR.Expr'String id ty s) = pure (ANFIR.Expr'String id ty s)
        do_expr (ANFIR.Expr'Tuple id ty a b) = modify a >>= \ a -> modify b >>= \ b -> pure (ANFIR.Expr'Tuple id ty a b)
        do_expr (ANFIR.Expr'MakeADT id ty variant args) = mapM modify args >>= \ args -> pure (ANFIR.Expr'MakeADT id ty variant args)

        do_expr (ANFIR.Expr'Lambda id ty param group res) = modify res >>= \ res -> pure (ANFIR.Expr'Lambda id ty param group res)
        do_expr (ANFIR.Expr'Param id ty param) = pure (ANFIR.Expr'Param id ty param)

        do_expr (ANFIR.Expr'Call id ty callee arg) = modify callee >>= \ callee -> modify arg >>= \ arg -> pure (ANFIR.Expr'Call id ty callee arg)

        do_expr (ANFIR.Expr'Switch id ty scrutinee arms) = modify scrutinee >>= \ scrutinee -> mapM (\ (matcher, group, res) -> (matcher, group,) <$> modify res) arms >>= \ arms -> pure (ANFIR.Expr'Switch id ty scrutinee arms)

        do_expr (ANFIR.Expr'Seq id ty a b) = modify a >>= \ a -> modify b >>= \ b -> pure (ANFIR.Expr'Seq id ty a b)

        do_expr (ANFIR.Expr'TupleDestructure1 id ty tup) = modify tup >>= \ tup -> pure (ANFIR.Expr'TupleDestructure1 id ty tup)
        do_expr (ANFIR.Expr'TupleDestructure2 id ty tup) = modify tup >>= \ tup -> pure (ANFIR.Expr'TupleDestructure2 id ty tup)

        do_expr (ANFIR.Expr'Forall id ty tys group res) = modify res >>= \ res -> pure (ANFIR.Expr'Forall id ty tys group res)
        do_expr (ANFIR.Expr'TypeApply id ty other argty) = modify other >>= \ other -> pure (ANFIR.Expr'TypeApply id ty other argty)

        do_expr (ANFIR.Expr'Poison id ty poison_allowed) = pure (ANFIR.Expr'Poison id ty poison_allowed)
