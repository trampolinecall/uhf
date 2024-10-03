module UHF.Parts.OptimizeANFIR.Utils (iterate_over_all_subexpressions) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.ANFIR as ANFIR

-- TODO: figure out how this is supposed to work (remove?)
iterate_over_bindings :: Monad m => (ANFIR.Binding -> m ANFIR.Binding) -> ANFIR.ANFIR -> m ANFIR.ANFIR
iterate_over_bindings change (ANFIR.ANFIR adts type_synonyms vars bindings params cu) =
    runStateT (do_cu cu) bindings >>= \ ((), bindings) ->
    pure (ANFIR.ANFIR adts type_synonyms vars bindings params cu)
    where
        do_cu (ANFIR.CU group _ _) = do_group group

        do_group (ANFIR.BindingGroup chunks) = mapM_ do_chunk chunks

        do_chunk (ANFIR.SingleBinding b) = do_binding b
        do_chunk (ANFIR.MutuallyRecursiveBindings b) = mapM_ do_binding b

        -- ideally would use modifyM but that is not in the transformers package of this stackage snapshot
        do_binding bk =
            StateT (\ bindings -> ((),) <$> Arena.modifyM bindings bk change) >>
            ANFIR.binding_initializer <$> (Arena.get <$> get <*> pure bk) >>= \case
                    ANFIR.Expr'Lambda _ _ _ _ _ -> pure () -- do_group group TODO: fix this (lambdas have an inline expression, so change doesnt work because there is no binding to change)
                    ANFIR.Expr'Forall _ _ _ _ -> pure () -- do_group group TODO: same todo as above
                    ANFIR.Expr'Match _ _ tree -> do_tree tree
                        where
                            do_tree (ANFIR.MatchTree arms) = mapM_
                                (\ (_, result) ->
                                    case result of
                                        Left subtree -> do_tree subtree
                                        Right _ -> pure ()
                                )
                                arms

                    _ -> pure ()

-- TODO: figure out how this is supposed to work (remove?)
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
        do_expr (ANFIR.Expr'MakeADT id ty variant tyargs args) = mapM modify args >>= \ args -> pure (ANFIR.Expr'MakeADT id ty variant tyargs args)

        do_expr (ANFIR.Expr'Lambda id ty param captures res) = {- modify res >>= \ res -> TODO: figure this out -} pure (ANFIR.Expr'Lambda id ty param captures res)
        do_expr (ANFIR.Expr'Param id ty param) = pure (ANFIR.Expr'Param id ty param)

        do_expr (ANFIR.Expr'Let id ty group result) = {- modify result >>= \ result -> TODO: figure this out -} pure (ANFIR.Expr'Let id ty group result)

        do_expr (ANFIR.Expr'Call id ty callee arg) = modify callee >>= \ callee -> modify arg >>= \ arg -> pure (ANFIR.Expr'Call id ty callee arg)

        do_expr (ANFIR.Expr'Match id ty tree) = do_tree tree >>= \ tree -> pure (ANFIR.Expr'Match id ty tree)
            where
                do_tree (ANFIR.MatchTree arms) =
                    ANFIR.MatchTree <$>
                        mapM
                            (\ (clauses, result) ->
                                mapM do_match_clause clauses >>= \ clauses ->
                                (case result of
                                    Left subtree -> Left <$> do_tree subtree
                                    Right res -> {- modify res >>= \ res -> TODO: figure this out -} pure (Right res)) >>= \ result ->
                                pure (clauses, result))
                            arms

                do_match_clause (ANFIR.MatchClause'Match binding matcher) = ANFIR.MatchClause'Match <$> modify binding <*> pure matcher
                do_match_clause (ANFIR.MatchClause'Binding b) = ANFIR.MatchClause'Binding <$> modify b

        do_expr (ANFIR.Expr'TupleDestructure1 id ty tup) = modify tup >>= \ tup -> pure (ANFIR.Expr'TupleDestructure1 id ty tup)
        do_expr (ANFIR.Expr'TupleDestructure2 id ty tup) = modify tup >>= \ tup -> pure (ANFIR.Expr'TupleDestructure2 id ty tup)
        do_expr (ANFIR.Expr'ADTDestructure id ty base field_idx) = modify base >>= \ base -> pure (ANFIR.Expr'ADTDestructure id ty base field_idx)

        do_expr (ANFIR.Expr'Forall id ty tys res) = {- modify res >>= \ res -> TODO: figure this out -} pure (ANFIR.Expr'Forall id ty tys res)
        do_expr (ANFIR.Expr'TypeApply id ty other argty) = modify other >>= \ other -> pure (ANFIR.Expr'TypeApply id ty other argty)

        do_expr (ANFIR.Expr'Poison id ty) = pure (ANFIR.Expr'Poison id ty)
