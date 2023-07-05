module UHF.Phases.Middle.OptimizeANFIR.Utils (iterate_over_all_subexpressions) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.IR.ANFIR as ANFIR

iterate_over_bindings :: Monad m => (ANFIR.Binding -> m ANFIR.Binding) -> ANFIR.ANFIR -> m ANFIR.ANFIR
iterate_over_bindings change (ANFIR.ANFIR adts type_synonyms vars bindings params cu) =
    runStateT (do_cu cu) bindings >>= \ ((), bindings) ->
    pure (ANFIR.ANFIR adts type_synonyms vars bindings params cu)
    where
        do_cu (ANFIR.CU group _ _) = do_group group

        do_group (ANFIR.BindingGroup _ chunks) = mapM_ do_chunk chunks

        do_chunk (ANFIR.SingleBinding b) = do_binding b
        do_chunk (ANFIR.MutuallyRecursiveBindings b) = mapM_ do_binding b

        -- ideally would use modifyM but that is not in the transformers package of this stackage snapshot
        do_binding bk =
            StateT (\ bindings -> ((),) <$> Arena.modifyM bindings bk change) >>
            ANFIR.binding_initializer <$> (Arena.get <$> get <*> pure bk) >>= \case
                    ANFIR.Expr'Lambda _ _ _ group _ -> do_group group
                    ANFIR.Expr'Switch _ _ _ arms -> mapM_ (\ (_, group, _) -> do_group group) arms
                    ANFIR.Expr'Forall _ _ _ group _ -> do_group group

                    _ -> pure ()

iterate_over_all_subexpressions :: Monad m => (ANFIR.BindingKey -> m ANFIR.BindingKey) -> ANFIR.ANFIR -> m ANFIR.ANFIR
iterate_over_all_subexpressions modify = iterate_over_bindings do_binding
    where
        do_binding (ANFIR.Binding init) = ANFIR.Binding <$> do_expr init

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

        do_expr (ANFIR.Expr'TupleDestructure1 id ty tup) = modify tup >>= \ tup -> pure (ANFIR.Expr'TupleDestructure1 id ty tup)
        do_expr (ANFIR.Expr'TupleDestructure2 id ty tup) = modify tup >>= \ tup -> pure (ANFIR.Expr'TupleDestructure2 id ty tup)

        do_expr (ANFIR.Expr'Forall id ty tys group res) = modify res >>= \ res -> pure (ANFIR.Expr'Forall id ty tys group res)
        do_expr (ANFIR.Expr'TypeApply id ty other argty) = modify other >>= \ other -> pure (ANFIR.Expr'TypeApply id ty other argty)

        do_expr (ANFIR.Expr'Poison id ty) = pure (ANFIR.Expr'Poison id ty)
