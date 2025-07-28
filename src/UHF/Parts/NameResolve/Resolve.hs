module UHF.Parts.NameResolve.Resolve (resolve) where

import UHF.Prelude

import Control.Arrow (first, second)
import Data.Functor.Const (Const (Const))
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.NameResolve.Error as Error
import qualified UHF.Parts.NameResolve.NRReader as NRReader
import qualified UHF.Parts.NameResolve.NameMaps as NameMaps
import qualified UHF.Parts.TypeSolver as TypeSolver
import UHF.Source.Located (Located)
import qualified UHF.Util.Arena as Arena

type PreResolve = (NameMaps.NameMapStackKey, Const () (), (), (), (), ())
type MidResolve =
    ( NameMaps.NameMapStackKey
    , MidResolveFunctor ()
    , SIR.DeclRef TypeSolver.Type
    , TypeSolver.Type
    , ()
    , ()
    )
type PostResolve = (NameMaps.NameMapStackKey, Maybe (), SIR.DeclRef TypeSolver.Type, TypeSolver.Type, (), ())

-- TODO: remove these
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey
type MidResolveModuleArena = Arena.Arena (SIR.Module MidResolve) SIR.ModuleKey
type MidResolveADTArena = Arena.Arena (SIR.ADT MidResolve) Type.ADTKey
type MidResolveTypeSynonymArena = Arena.Arena (SIR.TypeSynonym MidResolve) Type.TypeSynonymKey
type MidResolveVariableArena = Arena.Arena (SIR.Variable MidResolve) SIR.VariableKey

-- MidResolveFunctor {{{
-- TODO: split this into its own module and make the field of Errored a type parameter
data MidResolveFunctor result
    = Inconclusive
    | Errored Compiler.ErrorReportedPromise
    | Resolved result

instance Functor MidResolveFunctor where
    fmap _ Inconclusive = Inconclusive
    fmap _ (Errored e) = Errored e
    fmap f (Resolved r) = Resolved $ f r

instance Applicative MidResolveFunctor where
    pure = Resolved
    Inconclusive <*> _ = Inconclusive
    Errored e <*> _ = Errored e
    Resolved f <*> a = fmap f a

instance Monad MidResolveFunctor where
    Inconclusive >>= _ = Inconclusive
    Errored e >>= _ = Errored e
    Resolved r >>= f = f r

convert_either_to_mid_resolve_functor :: Either Error.Error r -> MidResolveFunctor r
convert_either_to_mid_resolve_functor (Right r) = Resolved r
convert_either_to_mid_resolve_functor (Left e) = Errored todo -- TODO: e

if_inconclusive :: Monad m => MidResolveFunctor result -> m (MidResolveFunctor result) -> m (MidResolveFunctor result)
if_inconclusive thing compute_new = case thing of
    Inconclusive -> compute_new
    _ -> pure thing

-- }}}

resolve :: SIR.SIR PreResolve -> Error.WithErrors (SIR.SIR PostResolve)
resolve sir = resolve_names (prepare_for_resolve sir) >>= finalize_resolve

prepare_for_resolve :: SIR.SIR PreResolve -> SIR.SIR MidResolve
prepare_for_resolve (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    SIR.SIR
        (Arena.transform prepare_mod mods)
        (Arena.transform prepare_adt adts)
        (Arena.transform prepare_type_synonym type_synonyms)
        type_vars
        (Arena.transform prepare_variable variables)
        (SIR.CU root_module main_function)
    where
        prepare_mod :: SIR.Module PreResolve -> SIR.Module MidResolve
        prepare_mod (SIR.Module id name_map bindings adts type_synonyms) = SIR.Module id name_map (map prepare_binding bindings) adts type_synonyms

        prepare_adt :: SIR.ADT PreResolve -> SIR.ADT MidResolve
        prepare_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars (map prepare_variant variants)
            where
                prepare_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id (map (\(id, name, (ty, Const ())) -> (id, name, (prepare_type_expr ty, Inconclusive))) fields)
                prepare_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id (map (\(id, (ty, Const ())) -> (id, (prepare_type_expr ty, Inconclusive))) fields)

        prepare_type_synonym :: SIR.TypeSynonym PreResolve -> SIR.TypeSynonym MidResolve
        prepare_type_synonym (Type.TypeSynonym id name (expansion, Const ())) = Type.TypeSynonym id name (prepare_type_expr expansion, Inconclusive)

        prepare_variable :: SIR.Variable PreResolve -> SIR.Variable MidResolve
        prepare_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

        prepare_binding :: SIR.Binding PreResolve -> SIR.Binding MidResolve
        prepare_binding (SIR.Binding target eq_sp expr) = SIR.Binding (prepare_pat target) eq_sp (prepare_expr expr)

        prepare_type_expr :: SIR.TypeExpr PreResolve -> SIR.TypeExpr MidResolve
        prepare_type_expr (SIR.TypeExpr'Refer (Const ()) sp name_maps id (Const ())) = SIR.TypeExpr'Refer Inconclusive sp name_maps id Inconclusive
        prepare_type_expr (SIR.TypeExpr'Get (Const ()) sp parent name) = SIR.TypeExpr'Get Inconclusive sp (prepare_type_expr parent) name
        prepare_type_expr (SIR.TypeExpr'Tuple (Const ()) sp a b) = SIR.TypeExpr'Tuple Inconclusive sp (prepare_type_expr a) (prepare_type_expr b)
        prepare_type_expr (SIR.TypeExpr'Hole (Const ()) (Const ()) sp hid) = SIR.TypeExpr'Hole Inconclusive Inconclusive sp hid
        prepare_type_expr (SIR.TypeExpr'Function (Const ()) sp arg res) = SIR.TypeExpr'Function Inconclusive sp (prepare_type_expr arg) (prepare_type_expr res)
        prepare_type_expr (SIR.TypeExpr'Forall (Const ()) sp name_maps vars ty) = SIR.TypeExpr'Forall Inconclusive sp name_maps vars (prepare_type_expr ty)
        prepare_type_expr (SIR.TypeExpr'Apply (Const ()) sp ty args) = SIR.TypeExpr'Apply Inconclusive sp (prepare_type_expr ty) (prepare_type_expr args)
        prepare_type_expr (SIR.TypeExpr'Wild (Const ()) sp) = SIR.TypeExpr'Wild Inconclusive sp
        prepare_type_expr (SIR.TypeExpr'Poison (Const ()) sp) = SIR.TypeExpr'Poison Inconclusive sp

        prepare_expr :: SIR.Expr PreResolve -> SIR.Expr MidResolve
        prepare_expr (SIR.Expr'Refer id type_info sp iden (Const ())) = SIR.Expr'Refer id type_info sp (prepare_split_iden iden) Inconclusive
        prepare_expr (SIR.Expr'Char id type_info sp c) = SIR.Expr'Char id type_info sp c
        prepare_expr (SIR.Expr'String id type_info sp s) = SIR.Expr'String id type_info sp s
        prepare_expr (SIR.Expr'Int id type_info sp i) = SIR.Expr'Int id type_info sp i
        prepare_expr (SIR.Expr'Float id type_info sp f) = SIR.Expr'Float id type_info sp f
        prepare_expr (SIR.Expr'Bool id type_info sp b) = SIR.Expr'Bool id type_info sp b
        prepare_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp (prepare_expr a) (prepare_expr b)
        prepare_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp (prepare_pat param) (prepare_expr body)
        prepare_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp name_maps (map prepare_binding bindings) adts type_synonyms (prepare_expr body)
        prepare_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp name_maps (map prepare_binding bindings) adts type_synonyms (prepare_expr body)
        prepare_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
            SIR.Expr'BinaryOps
                id
                allowed
                type_info
                sp
                (prepare_expr first)
                (map (\(sp, iden, Const (), rhs) -> (sp, prepare_split_iden iden, Inconclusive, prepare_expr rhs)) ops)
        prepare_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp (prepare_expr callee) (prepare_expr arg)
        prepare_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp (prepare_expr cond) (prepare_expr t) (prepare_expr f)
        prepare_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) = SIR.Expr'Match id type_info sp match_tok_sp (prepare_expr e) (map (\(ncs, pat, expr) -> (ncs, prepare_pat pat, prepare_expr expr)) arms)
        prepare_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, Const ()) e) = SIR.Expr'TypeAnnotation id type_info sp (prepare_type_expr ty, Inconclusive) (prepare_expr e)
        prepare_expr (SIR.Expr'Forall id type_info sp ncs vars e) = SIR.Expr'Forall id type_info sp ncs vars (prepare_expr e)
        prepare_expr (SIR.Expr'TypeApply id type_info sp e (arg, Const ())) = SIR.Expr'TypeApply id type_info sp (prepare_expr e) (prepare_type_expr arg, Inconclusive)
        prepare_expr (SIR.Expr'Hole id type_info sp hid) = SIR.Expr'Hole id type_info sp hid
        prepare_expr (SIR.Expr'Poison id type_info sp) = SIR.Expr'Poison id type_info sp

        prepare_pat :: SIR.Pattern PreResolve -> SIR.Pattern MidResolve
        prepare_pat (SIR.Pattern'Variable type_info sp bnk) = SIR.Pattern'Variable type_info sp bnk
        prepare_pat (SIR.Pattern'Wildcard type_info sp) = SIR.Pattern'Wildcard type_info sp
        prepare_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp (prepare_pat a) (prepare_pat b)
        prepare_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk (prepare_pat subpat)
        prepare_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden (Const ()) tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp (prepare_split_iden variant_iden) Inconclusive tyargs (map prepare_pat subpat)
        prepare_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden (Const ()) tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp (prepare_split_iden variant_iden) Inconclusive tyargs (map (second prepare_pat) subpat)
        prepare_pat (SIR.Pattern'Poison type_info sp) = SIR.Pattern'Poison type_info sp

        prepare_split_iden :: SIR.SplitIdentifier resolved PreResolve -> SIR.SplitIdentifier resolved MidResolve
        prepare_split_iden (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get (prepare_type_expr texpr) next
        prepare_split_iden (SIR.SplitIdentifier'Single name_maps i (Const ())) = SIR.SplitIdentifier'Single name_maps i Inconclusive

data ProgressMade = NoProgressMade | ProgressMade deriving Show
instance Semigroup ProgressMade where
    NoProgressMade <> NoProgressMade = NoProgressMade
    ProgressMade <> NoProgressMade = ProgressMade
    NoProgressMade <> ProgressMade = ProgressMade
    ProgressMade <> ProgressMade = ProgressMade
instance Monoid ProgressMade where
    mempty = NoProgressMade

-- TODO: look for all Inconclusive above and make sure that they are handled here
resolve_names :: SIR.SIR MidResolve -> Error.WithErrors (SIR.SIR MidResolve)
resolve_names sir = go sir
    where
        -- TODO: better organize these functions so that they are not all under this where clause (split this file up into multiple files?)
        go sir = do
            (sir', progress_made) <- runWriterT $ resolve_single_step sir
            case progress_made of
                NoProgressMade -> pure sir'
                ProgressMade -> go sir'

        resolve_single_step :: SIR.SIR MidResolve -> WriterT ProgressMade Error.WithErrors (SIR.SIR MidResolve)
        resolve_single_step (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
            mods <- runReaderT (Arena.transformM resolve_in_module mods) (adts, type_synonyms, variables, type_vars, ())
            adts <- runReaderT (Arena.transformM resolve_in_adt adts) ((), (), (), type_vars, ())
            type_synonyms <- runReaderT (Arena.transformM resolve_in_type_synonym type_synonyms) ((), (), (), type_vars, ())
            pure (SIR.SIR mods adts type_synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
            where
                change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

        resolve_in_module ::
            SIR.Module MidResolve ->
            ( NRReader.NRReader
                MidResolveADTArena
                MidResolveTypeSynonymArena
                MidResolveVariableArena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
            )
                (SIR.Module MidResolve)
        resolve_in_module (SIR.Module id name_maps bindings adts type_synonyms) = do
            bindings <- mapM resolve_in_binding bindings
            pure $ SIR.Module id name_maps bindings adts type_synonyms

        resolve_in_adt ::
            SIR.ADT MidResolve ->
            NRReader.NRReader
                adt_arena
                type_synonym_arena
                var_arena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (SIR.ADT MidResolve)
        resolve_in_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM resolve_in_variant variants
            where
                resolve_in_variant (Type.ADT.Variant'Named name id fields) =
                    Type.ADT.Variant'Named name id
                        <$> mapM
                            ( \(id, name, (texpr, as_type)) -> do
                                texpr <- resolve_in_type_expr texpr
                                as_type <- if_inconclusive as_type (type_expr_evaled_as_type texpr)
                                pure (id, name, (texpr, as_type))
                            )
                            fields
                resolve_in_variant (Type.ADT.Variant'Anon name id fields) =
                    Type.ADT.Variant'Anon name id
                        <$> mapM
                            ( \(id, (texpr, as_type)) -> do
                                texpr <- resolve_in_type_expr texpr
                                as_type <- if_inconclusive as_type (type_expr_evaled_as_type texpr)
                                pure (id, (texpr, as_type))
                            )
                            fields

        resolve_in_type_synonym ::
            SIR.TypeSynonym MidResolve ->
            NRReader.NRReader
                adt_arena
                type_synonym_arena
                var_arena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (SIR.TypeSynonym MidResolve)
        resolve_in_type_synonym (Type.TypeSynonym id name (expansion, expansion_as_type)) = do
            expansion <- resolve_in_type_expr expansion
            expansion_as_type <- if_inconclusive expansion_as_type (type_expr_evaled_as_type expansion)
            pure $ Type.TypeSynonym id name (expansion, expansion_as_type)

        resolve_in_binding ::
            SIR.Binding MidResolve ->
            NRReader.NRReader
                MidResolveADTArena
                MidResolveTypeSynonymArena
                MidResolveVariableArena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (SIR.Binding MidResolve)
        resolve_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat target <*> pure eq_sp <*> resolve_in_expr expr

        resolve_in_type_expr ::
            SIR.TypeExpr MidResolve ->
            NRReader.NRReader
                adt_arena
                type_synonym_arena
                var_arena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (SIR.TypeExpr MidResolve)
        resolve_in_type_expr (SIR.TypeExpr'Refer _ sp nc_stack id resolved) = do
            resolved <- if_inconclusive resolved (look_up_decl nc_stack id)
            pure $ SIR.TypeExpr'Refer resolved sp nc_stack id resolved
        resolve_in_type_expr (SIR.TypeExpr'Get evaled sp parent name) = do
            parent <- resolve_in_type_expr parent
            evaled <-
                if_inconclusive
                    evaled
                    ( case SIR.type_expr_evaled parent of
                        Inconclusive -> pure Inconclusive
                        Errored err -> pure $ Errored err
                        Resolved r -> get_decl_child r name
                    )
            pure $ SIR.TypeExpr'Get evaled sp parent name
        resolve_in_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = do
            a <- resolve_in_type_expr a
            b <- resolve_in_type_expr b
            evaled <-
                if_inconclusive
                    evaled
                    ( do
                        a' <- type_expr_evaled_as_type a
                        b' <- type_expr_evaled_as_type b
                        pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Tuple <$> a' <*> b')
                    )
            pure $ SIR.TypeExpr'Tuple evaled sp a b
        resolve_in_type_expr (SIR.TypeExpr'Hole evaled evaled_as_type sp hid) = do
            evaled_as_type <- if_inconclusive evaled_as_type (make_infer_var (TypeSolver.TypeHole sp))
            evaled <- if_inconclusive evaled (pure $ SIR.DeclRef'Type <$> evaled_as_type)
            pure $ SIR.TypeExpr'Hole evaled evaled_as_type sp hid
        resolve_in_type_expr (SIR.TypeExpr'Function evaled sp arg res) = do
            arg <- resolve_in_type_expr arg
            res <- resolve_in_type_expr res
            evaled <-
                if_inconclusive
                    evaled
                    ( do
                        arg' <- type_expr_evaled_as_type arg
                        res' <- type_expr_evaled_as_type res
                        pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Function <$> arg' <*> res')
                    )
            pure $ SIR.TypeExpr'Function evaled sp arg res
        resolve_in_type_expr (SIR.TypeExpr'Forall evaled sp name_maps vars inner) = do
            inner <- resolve_in_type_expr inner
            evaled <-
                if_inconclusive
                    evaled
                    ( do
                        inner' <- type_expr_evaled_as_type inner
                        pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Forall vars <$> inner')
                    )
            pure $ SIR.TypeExpr'Forall evaled sp name_maps vars inner
        resolve_in_type_expr (SIR.TypeExpr'Apply evaled sp ty arg) = do
            ty <- resolve_in_type_expr ty
            arg <- resolve_in_type_expr arg
            evaled <-
                if_inconclusive
                    evaled
                    ( do
                        ty' <- type_expr_evaled_as_type ty
                        arg' <- type_expr_evaled_as_type arg

                        todo -- TODO: applying type requires you to interact with the type solver
                    )
            pure $ SIR.TypeExpr'Apply evaled sp ty arg
        resolve_in_type_expr (SIR.TypeExpr'Wild evaled sp) = do
            evaled <-
                if_inconclusive
                    evaled
                    ( do
                        infer_var <- make_infer_var (TypeSolver.TypeExpr sp)
                        pure $ SIR.DeclRef'Type <$> infer_var
                    )
            pure $ SIR.TypeExpr'Wild evaled sp
        resolve_in_type_expr (SIR.TypeExpr'Poison evaled sp) = do
            evaled <-
                if_inconclusive
                    evaled
                    ( do
                        infer_var <- make_infer_var (TypeSolver.TypeExpr sp)
                        pure $ SIR.DeclRef'Type <$> infer_var
                    )
            pure $ SIR.TypeExpr'Poison evaled sp

        type_expr_evaled_as_type ::
            SIR.TypeExpr MidResolve ->
            ReaderT
                (adt_arena, type_synonym_arena, var_arena, QuantVarArena, sir_child_maps)
                (WriterT ProgressMade Error.WithErrors)
                (MidResolveFunctor TypeSolver.Type)
        type_expr_evaled_as_type te =
            let sp = SIR.type_expr_span te
                d = SIR.type_expr_evaled te
            in case d of
                Resolved (SIR.DeclRef'Module _) -> do
                    _ <- lift $ lift $ Compiler.tell_error (Error.Error'NotAType sp "a module")
                    make_infer_var (TypeSolver.TypeExpr sp) -- TODO: don't make variables for these?
                Resolved (SIR.DeclRef'Type ty) -> pure $ Resolved ty
                Resolved (SIR.DeclRef'ExternPackage _) -> do
                    _ <- lift $ lift $ Compiler.tell_error (Error.Error'NotAType sp "external package")
                    make_infer_var (TypeSolver.TypeExpr sp) -- TODO: don't make variables for these?
                Errored _ -> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: make this message better
                Inconclusive -> pure Inconclusive

        resolve_in_expr ::
            SIR.Expr MidResolve ->
            ( NRReader.NRReader
                MidResolveADTArena
                MidResolveTypeSynonymArena
                MidResolveVariableArena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
            )
                (SIR.Expr MidResolve)
        resolve_in_expr (SIR.Expr'Refer id type_info sp iden resolved) = do
            case resolved of
                Inconclusive -> do
                    (split_iden, result) <- resolve_split_iden look_up_value get_value_child iden
                    pure $ SIR.Expr'Refer id type_info sp split_iden result
                _ -> pure $ SIR.Expr'Refer id type_info sp iden resolved
        resolve_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
        resolve_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
        resolve_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
        resolve_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
        resolve_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
        resolve_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr a <*> resolve_in_expr b
        resolve_in_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> resolve_in_pat param <*> resolve_in_expr body
        resolve_in_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) =
            SIR.Expr'Let id type_info sp name_maps
                <$> mapM resolve_in_binding bindings
                <*> pure adts
                <*> pure type_synonyms
                <*> resolve_in_expr body
        resolve_in_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = do
            SIR.Expr'LetRec id type_info sp name_maps
                <$> mapM resolve_in_binding bindings
                <*> pure adts
                <*> pure type_synonyms
                <*> resolve_in_expr body
        resolve_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
            SIR.Expr'BinaryOps id allowed type_info sp
                <$> resolve_in_expr first
                <*> mapM
                    ( \(sp, iden, resolved, rhs) -> do
                        rhs <- resolve_in_expr rhs
                        case resolved of
                            Inconclusive -> do
                                (iden, result) <- resolve_split_iden look_up_value get_value_child iden
                                pure (sp, iden, result, rhs)
                            _ -> pure (sp, iden, resolved, rhs)
                    )
                    ops
        resolve_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr callee <*> resolve_in_expr arg
        resolve_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr cond <*> resolve_in_expr t <*> resolve_in_expr f
        resolve_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
            SIR.Expr'Match id type_info sp match_tok_sp
                <$> resolve_in_expr e
                <*> mapM
                    ( \(name_maps, pat, expr) -> do
                        pat' <- resolve_in_pat pat
                        expr' <- resolve_in_expr expr
                        pure (name_maps, pat', expr')
                    )
                    arms
        resolve_in_expr (SIR.Expr'TypeAnnotation id type_info sp (tye, tye_as_type) e) = do
            e <- resolve_in_expr e
            tye <- resolve_in_type_expr tye
            tye_as_type <- if_inconclusive tye_as_type (type_expr_evaled_as_type tye)
            pure $ SIR.Expr'TypeAnnotation id type_info sp (tye, tye_as_type) e
        resolve_in_expr (SIR.Expr'Forall id type_info sp name_maps vars e) = SIR.Expr'Forall id type_info sp name_maps vars <$> resolve_in_expr e
        resolve_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type)) = do
            e <- resolve_in_expr e
            arg <- resolve_in_type_expr arg
            arg_as_type <- if_inconclusive arg_as_type (type_expr_evaled_as_type arg)
            pure $ SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type)
        resolve_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
        resolve_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

        resolve_in_pat ::
            SIR.Pattern MidResolve ->
            NRReader.NRReader
                adt_arena
                type_synonym_arena
                var_arena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (SIR.Pattern MidResolve)
        resolve_in_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
        resolve_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
        resolve_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat a <*> resolve_in_pat b
        resolve_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat subpat
        resolve_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden resolved tyargs subpats) = do
            subpats <- mapM resolve_in_pat subpats
            case resolved of
                Inconclusive -> do
                    (variant_iden, result) <- resolve_split_iden look_up_variant get_variant_child variant_iden
                    pure $ SIR.Pattern'AnonADTVariant type_info sp variant_iden result tyargs subpats
                _ -> pure $ SIR.Pattern'AnonADTVariant type_info sp variant_iden resolved tyargs subpats
        resolve_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden resolved tyargs subpats) = do
            subpats <- mapM (\(field_name, field_pat) -> (field_name,) <$> resolve_in_pat field_pat) subpats
            case resolved of
                Inconclusive -> do
                    (variant_iden, result) <- resolve_split_iden look_up_variant get_variant_child variant_iden
                    pure $ SIR.Pattern'NamedADTVariant type_info sp variant_iden result tyargs subpats
                _ -> pure $ SIR.Pattern'NamedADTVariant type_info sp variant_iden resolved tyargs subpats
        resolve_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

        resolve_split_iden ::
            ( NameMaps.NameMapStackKey ->
              Located Text ->
              NRReader.NRReader
                adt_arena
                type_synonym_arena
                var_arena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (MidResolveFunctor resolved)
            ) ->
            ( SIR.DeclRef TypeSolver.Type ->
              Located Text ->
              NRReader.NRReader
                adt_arena
                type_synonym_arena
                var_arena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (MidResolveFunctor resolved)
            ) ->
            SIR.SplitIdentifier resolved MidResolve ->
            NRReader.NRReader
                adt_arena
                type_synonym_arena
                var_arena
                QuantVarArena
                sir_child_maps
                (WriterT ProgressMade Error.WithErrors)
                (SIR.SplitIdentifier resolved MidResolve, MidResolveFunctor resolved)
        resolve_split_iden _ resolve_get (SIR.SplitIdentifier'Get texpr next) = do
            texpr' <- resolve_in_type_expr texpr
            let texpr_evaled = SIR.type_expr_evaled texpr'
            gotten <- case texpr_evaled of
                Resolved texpr_evaled -> resolve_get texpr_evaled next
                Errored err -> pure $ Errored err
                Inconclusive -> pure Inconclusive
            pure (SIR.SplitIdentifier'Get texpr' next, gotten)
        resolve_split_iden resolve_single _ (SIR.SplitIdentifier'Single name_maps name resolved) =
            case resolved of
                Inconclusive -> do
                    result <- resolve_single name_maps name
                    pure (SIR.SplitIdentifier'Single name_maps name result, result)
                Errored err -> pure (SIR.SplitIdentifier'Single name_maps name resolved, Errored err)
                Resolved result -> pure (SIR.SplitIdentifier'Single name_maps name resolved, Resolved result)

        look_up_decl name_maps_stack_key name = todo >>= \name_maps_arena -> pure $ convert_either_to_mid_resolve_functor $ NameMaps.look_up_decl name_maps_arena name_maps_stack_key name
        look_up_value name_maps_stack_key name = todo >>= \name_maps_arena -> pure $ convert_either_to_mid_resolve_functor $ NameMaps.look_up_value name_maps_arena name_maps_stack_key name
        look_up_variant name_maps_stack_key name = todo >>= \name_maps_arena -> pure $ convert_either_to_mid_resolve_functor $ NameMaps.look_up_variant name_maps_arena name_maps_stack_key name

        get_decl_child parent name = todo >>= \sir_child_maps -> pure $ convert_either_to_mid_resolve_functor $ NameMaps.get_decl_child sir_child_maps parent name
        get_value_child parent name = todo >>= \sir_child_maps -> pure $ convert_either_to_mid_resolve_functor $ NameMaps.get_value_child sir_child_maps parent name
        get_variant_child parent name = todo >>= \sir_child_maps -> pure $ convert_either_to_mid_resolve_functor $ NameMaps.get_variant_child sir_child_maps parent name

        make_infer_var ::
            TypeSolver.InferVarForWhat ->
            ReaderT
                (adt_arena, type_synonym_arena, var_arena, QuantVarArena, sir_child_maps)
                (WriterT ProgressMade Error.WithErrors)
                (MidResolveFunctor TypeSolver.Type)
        make_infer_var for_what = todo -- TODO: TypeSolver.Type'InferVar <$> TypeSolver.new_infer_var for_what

finalize_resolve :: SIR.SIR MidResolve -> Error.WithErrors (SIR.SIR PostResolve)
finalize_resolve (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    SIR.SIR
        <$> Arena.transformM finalize_mod mods
        <*> Arena.transformM finalize_adt adts
        <*> Arena.transformM finalize_type_synonym type_synonyms
        <*> pure type_vars
        <*> Arena.transformM finalize_variable variables
        <*> pure (SIR.CU root_module main_function)
    where
        finalize_mod :: SIR.Module MidResolve -> Error.WithErrors (SIR.Module PostResolve)
        finalize_mod (SIR.Module id name_map bindings adts type_synonyms) = SIR.Module id name_map <$> mapM finalize_binding bindings <*> pure adts <*> pure type_synonyms

        finalize_adt :: SIR.ADT MidResolve -> Error.WithErrors (SIR.ADT PostResolve)
        finalize_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM finalize_variant variants
            where
                finalize_variant (Type.ADT.Variant'Named name id fields) =
                    Type.ADT.Variant'Named name id
                        <$> mapM
                            (\(id, name, (ty, as_type)) -> finalize_type_expr ty >>= \ty -> finalize_functor as_type >>= \as_type -> pure (id, name, (ty, as_type)))
                            fields
                finalize_variant (Type.ADT.Variant'Anon name id fields) =
                    Type.ADT.Variant'Anon name id
                        <$> mapM (\(id, (ty, as_type)) -> finalize_type_expr ty >>= \ty -> finalize_functor as_type >>= \as_type -> pure (id, (ty, as_type))) fields

        finalize_type_synonym :: SIR.TypeSynonym MidResolve -> Error.WithErrors (SIR.TypeSynonym PostResolve)
        finalize_type_synonym (Type.TypeSynonym id name (expansion, as_type)) = do
            expansion <- finalize_type_expr expansion
            as_type <- finalize_functor as_type
            pure $ Type.TypeSynonym id name (expansion, as_type)

        finalize_variable :: SIR.Variable MidResolve -> Error.WithErrors (SIR.Variable PostResolve)
        finalize_variable (SIR.Variable varid tyinfo n) = pure $ SIR.Variable varid tyinfo n

        finalize_binding :: SIR.Binding MidResolve -> Error.WithErrors (SIR.Binding PostResolve)
        finalize_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> finalize_pat target <*> pure eq_sp <*> finalize_expr expr

        finalize_type_expr :: SIR.TypeExpr MidResolve -> Error.WithErrors (SIR.TypeExpr PostResolve)
        finalize_type_expr (SIR.TypeExpr'Refer evaled sp name_maps id id_resolved) = SIR.TypeExpr'Refer <$> finalize_functor evaled <*> pure sp <*> pure name_maps <*> pure id <*> finalize_functor id_resolved
        finalize_type_expr (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get <$> finalize_functor evaled <*> pure sp <*> finalize_type_expr parent <*> pure name
        finalize_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple <$> finalize_functor evaled <*> pure sp <*> finalize_type_expr a <*> finalize_type_expr b
        finalize_type_expr (SIR.TypeExpr'Hole evaled evaled_as_type sp hid) = SIR.TypeExpr'Hole <$> finalize_functor evaled <*> finalize_functor evaled_as_type <*> pure sp <*> pure hid
        finalize_type_expr (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function <$> finalize_functor evaled <*> pure sp <*> finalize_type_expr arg <*> finalize_type_expr res
        finalize_type_expr (SIR.TypeExpr'Forall evaled sp name_maps vars ty) = SIR.TypeExpr'Forall <$> finalize_functor evaled <*> pure sp <*> pure name_maps <*> pure vars <*> finalize_type_expr ty
        finalize_type_expr (SIR.TypeExpr'Apply evaled sp ty args) = SIR.TypeExpr'Apply <$> finalize_functor evaled <*> pure sp <*> finalize_type_expr ty <*> finalize_type_expr args
        finalize_type_expr (SIR.TypeExpr'Wild evaled sp) = SIR.TypeExpr'Wild <$> finalize_functor evaled <*> pure sp
        finalize_type_expr (SIR.TypeExpr'Poison evaled sp) = SIR.TypeExpr'Poison <$> finalize_functor evaled <*> pure sp

        finalize_expr :: SIR.Expr MidResolve -> Error.WithErrors (SIR.Expr PostResolve)
        finalize_expr (SIR.Expr'Refer id type_info sp iden iden_resolved) = SIR.Expr'Refer id type_info sp <$> finalize_split_iden iden <*> finalize_functor iden_resolved
        finalize_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
        finalize_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
        finalize_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
        finalize_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
        finalize_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
        finalize_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> finalize_expr a <*> finalize_expr b
        finalize_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> finalize_pat param <*> finalize_expr body
        finalize_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp name_maps <$> mapM finalize_binding bindings <*> pure adts <*> pure type_synonyms <*> finalize_expr body
        finalize_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp name_maps <$> mapM finalize_binding bindings <*> pure adts <*> pure type_synonyms <*> finalize_expr body
        finalize_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
            SIR.Expr'BinaryOps id allowed type_info sp
                <$> finalize_expr first
                <*> mapM (\(sp, iden, op_resolved, rhs) -> (sp,,,) <$> finalize_split_iden iden <*> finalize_functor op_resolved <*> finalize_expr rhs) ops
        finalize_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> finalize_expr callee <*> finalize_expr arg
        finalize_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> finalize_expr cond <*> finalize_expr t <*> finalize_expr f
        finalize_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
            SIR.Expr'Match id type_info sp match_tok_sp
                <$> finalize_expr e
                <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> finalize_pat pat <*> finalize_expr expr) arms
        finalize_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_resolved) e) =
            finalize_type_expr ty >>= \ty -> finalize_functor ty_resolved >>= \ty_resolved -> SIR.Expr'TypeAnnotation id type_info sp (ty, ty_resolved) <$> finalize_expr e
        finalize_expr (SIR.Expr'Forall id type_info sp ncs vars e) = SIR.Expr'Forall id type_info sp ncs vars <$> finalize_expr e
        finalize_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_resolved)) =
            finalize_type_expr arg >>= \arg -> finalize_functor arg_resolved >>= \arg_resolved -> SIR.Expr'TypeApply id type_info sp <$> finalize_expr e <*> pure (arg, arg_resolved)
        finalize_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
        finalize_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

        finalize_pat :: SIR.Pattern MidResolve -> Error.WithErrors (SIR.Pattern PostResolve)
        finalize_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
        finalize_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
        finalize_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> finalize_pat a <*> finalize_pat b
        finalize_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> finalize_pat subpat
        finalize_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden variant_resolved tyargs subpat) =
            SIR.Pattern'AnonADTVariant type_info sp
                <$> finalize_split_iden variant_iden
                <*> finalize_functor variant_resolved
                <*> pure tyargs
                <*> mapM finalize_pat subpat
        finalize_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden variant_resolved tyargs subpat) =
            SIR.Pattern'NamedADTVariant
                type_info
                sp
                <$> finalize_split_iden variant_iden
                <*> finalize_functor variant_resolved
                <*> pure tyargs
                <*> mapM (\ (name, pat) -> (name,) <$> finalize_pat pat) subpat
        finalize_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

        finalize_split_iden :: SIR.SplitIdentifier resolved MidResolve -> Error.WithErrors (SIR.SplitIdentifier resolved PostResolve)
        finalize_split_iden (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get <$> finalize_type_expr texpr <*> pure next
        finalize_split_iden (SIR.SplitIdentifier'Single name_maps i resolved) = SIR.SplitIdentifier'Single name_maps i <$> finalize_functor resolved

        finalize_functor :: MidResolveFunctor a -> Error.WithErrors (Maybe a)
        finalize_functor Inconclusive = todo -- TODO: report error and then return Nothing
        finalize_functor (Errored _) = pure $ Nothing
        finalize_functor (Resolved r) = pure $ Just r
