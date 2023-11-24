module UHF.Phases.EvalTypeExprs
    ( eval
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import qualified UHF.IO.Located as Located
import UHF.IO.Span (Span)
import UHF.IO.Located (Located (Located, unlocate))
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified Data.Map as Map
import qualified Data.List as List

-- TODO: change errors, clean up this whole module, clean up duplication with name resolution module and resolving starts module

-- TODO: deduplicate child map things from the 3 name resolution modules
type DeclMap = Map.Map Text SIR.DeclKey

type ModuleDeclMaps = Arena.Arena DeclMap SIR.ModuleKey
type DeclArena = Arena.Arena SIR.Decl SIR.DeclKey

type EvaledDIden = Maybe SIR.DeclKey

type VIdenStart = Maybe SIR.BoundValueKey
type PIdenStart = Maybe Type.ADTVariantIndex

type Unevaled = (EvaledDIden, (), (), VIdenStart, (), PIdenStart, (), (), ())

-- TODO: remove these type aliases
type UnevaledSIR = SIR.SIR Unevaled
type UnevaledModule = SIR.Module Unevaled
type UnevaledADT = Type.ADT UnevaledTypeExpr
type UnevaledTypeSynonym = Type.TypeSynonym UnevaledTypeExpr
type UnevaledTypeExpr = SIR.TypeExpr Unevaled
type UnevaledBinding = SIR.Binding Unevaled
type UnevaledExpr = SIR.Expr Unevaled
type UnevaledPattern = SIR.Pattern Unevaled

type UnevaledModuleArena = Arena.Arena UnevaledModule SIR.ModuleKey
type UnevaledADTArena = Arena.Arena UnevaledADT Type.ADTKey
type UnevaledTypeSynonymArena = Arena.Arena UnevaledTypeSynonym Type.TypeSynonymKey
type UnevaledBoundValueArena = Arena.Arena (SIR.BoundValue Unevaled) SIR.BoundValueKey

type Evaled = (EvaledDIden, EvaledDIden, Maybe (Type.Type Void), VIdenStart, (), PIdenStart, (), (), ())

type EvaledSIR = SIR.SIR Evaled
type EvaledModule = SIR.Module Evaled
type EvaledADT = Type.ADT EvaledTypeExpr
type EvaledTypeSynonym = Type.TypeSynonym EvaledTypeExpr
type EvaledTypeExpr = SIR.TypeExpr Evaled
type EvaledBinding = SIR.Binding Evaled
type EvaledExpr = SIR.Expr Evaled
type EvaledPattern = SIR.Pattern Evaled

type EvaledModuleArena = Arena.Arena EvaledModule SIR.ModuleKey
type EvaledADTArena = Arena.Arena EvaledADT Type.ADTKey
type EvaledTypeSynonymArena = Arena.Arena EvaledTypeSynonym Type.TypeSynonymKey

type CollectingErrors = Compiler.WithDiagnostics Error Void

type MakeDeclState = StateT DeclArena
type EvalReader adt_arena bv_arena module_child_maps = ReaderT (DeclArena, adt_arena, bv_arena, module_child_maps)

data Error
    = CouldNotFind (Maybe (Located Text)) (Located Text)
    | MultipleDecls Text [DeclAt]
    | NotAType Span Text

instance Diagnostic.ToError Error where
    to_error (CouldNotFind prev (Located sp name)) =
        let message =
                "could not find name '" <> name <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error Diagnostic.Codes.undef_name (Just sp) message [] []

    to_error (MultipleDecls name decl_ats) =
        let span = headMay $ mapMaybe decl_at_span decl_ats -- take the first span of the decl_ats; if there are no decl_ats with a span, then this will be Ntohing
        in Diagnostic.Error
            Diagnostic.Codes.multiple_decls
            span
            (show (length decl_ats) <> " declarations of '" <> convert_str name <> "'")
            (map (\ at -> (decl_at_span at, Diagnostic.MsgError, decl_at_message name at)) decl_ats)
            []
        where
            decl_at_span (DeclAt sp) = Just sp
            decl_at_span ImplicitPrim = Nothing
            decl_at_message _ (DeclAt _) = Nothing
            decl_at_message n ImplicitPrim = Just $ "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by prelude')

    to_error (NotAType sp instead) =
        Diagnostic.Error Diagnostic.Codes.not_a_type (Just sp) ("not a type: got " <> instead) [] []

eval :: UnevaledSIR -> Compiler.WithDiagnostics Error Void EvaledSIR
eval (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    runStateT
        (
            runReaderT (collect_child_maps mods type_synonyms) (decls, adts, bound_values, ()) >>= \ module_child_maps ->
            runReaderT (resolve_in_mods mods) (decls, adts, bound_values, module_child_maps) >>= \ mods ->
            runReaderT (resolve_in_adts adts) (decls, (), (), module_child_maps) >>= \ adts ->
            runReaderT (resolve_in_type_synonyms type_synonyms) (decls, (), (), module_child_maps) >>= \ synonyms ->
            pure (mods, adts, synonyms)
        )
        decls >>= \ ((mods, adts, synonyms), decls) ->
    pure (SIR.SIR decls mods adts synonyms type_vars (Arena.transform change_bound_value bound_values) mod)
    where
        change_bound_value (SIR.BoundValue bvid tyinfo n) = SIR.BoundValue bvid tyinfo n
        change_bound_value (SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp) = SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp

new_decl :: Monad under => SIR.Decl -> MakeDeclState under SIR.DeclKey
new_decl d = StateT (\ arena -> pure $ Arena.put d arena)

ask_decl_arena :: Applicative under => EvalReader adt_arena bv_arena module_child_maps under DeclArena
ask_decl_arena = ReaderT $ \ (decls, _, _, _) -> pure decls
ask_adt_arena :: Applicative under => EvalReader adt_arena bv_arena module_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (_, adts, _, _) -> pure adts
ask_bv_arena :: Applicative under => EvalReader adt_arena bv_arena module_child_maps under bv_arena
ask_bv_arena = ReaderT $ \ (_, _, bvs, _) -> pure bvs
ask_module_child_maps :: Applicative under => EvalReader adt_arena bv_arena module_child_maps under module_child_maps
ask_module_child_maps = ReaderT $ \ (_, _, _, mcms) -> pure mcms

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

type DeclChildrenList = [(Text, DeclAt, SIR.DeclKey)]
type BoundValueList = [(Text, DeclAt, SIR.BoundValueKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADTVariantIndex)]

binding_children :: Monad under => UnevaledBinding -> EvalReader UnevaledADTArena UnevaledBoundValueArena module_child_maps under (DeclChildrenList, BoundValueList, ADTVariantList)
binding_children (SIR.Binding pat _ _) = ([],, []) <$> pattern_bvs pat
binding_children (SIR.Binding'ADTVariant sp bvk _ variant_index) = bv_name bvk >>= \ name -> pure ([], [(name, DeclAt sp, bvk)], [(name, DeclAt sp, variant_index)]) -- TODO: move variants to inside their types, also dont handle adt variants here

pattern_bvs :: Monad under => UnevaledPattern -> EvalReader UnevaledADTArena UnevaledBoundValueArena module_child_maps under BoundValueList
pattern_bvs (SIR.Pattern'Identifier _ sp bvk) = bv_name bvk >>= \ name -> pure [(name, DeclAt sp, bvk)]
pattern_bvs (SIR.Pattern'Wildcard _ _) = pure []
pattern_bvs (SIR.Pattern'Tuple _ _ a b) = pattern_bvs a >>= \ a -> pattern_bvs b >>= \ b -> pure (a ++ b)
pattern_bvs (SIR.Pattern'Named _ _ _ (Located bv_span bvk) subpat) = bv_name bvk >>= \ name -> pattern_bvs subpat >>= \ subpat -> pure ((name, DeclAt bv_span, bvk) : subpat)
pattern_bvs (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_bvs fields
pattern_bvs (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_bvs . snd) fields
pattern_bvs (SIR.Pattern'Poison _ _) = pure []

bv_name :: Monad under => SIR.BoundValueKey -> EvalReader UnevaledADTArena UnevaledBoundValueArena module_child_maps under Text
bv_name bvk =
    ask_bv_arena >>= \ bv_arena ->
    case Arena.get bv_arena bvk of
        SIR.BoundValue _ _ (Located _ name) -> pure name
        SIR.BoundValue'ADTVariant _ variant_index _ _ _ ->
            ask_adt_arena >>= \ adt_arena ->
            let variant = Type.get_adt_variant adt_arena variant_index
            in pure $ unlocate $ Type.variant_name variant

make_child_maps :: DeclChildrenList -> BoundValueList -> ADTVariantList -> CollectingErrors DeclMap
make_child_maps decls bound_values adt_variants =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_values
        variant_dups = find_dups adt_variants
    in report_dups decl_dups >> report_dups bn_dups >> report_dups variant_dups >>
    pure (make_map decls)
    where
        -- separate finding duplicates from making maps so that if there is a duplicate the whole name contexet doesnt just disappear
        -- duplicates will just take the last bound name in the last, because of the how Map.fromList is implemented
        find_dups x =
            let grouped = List.groupBy ((==) `on` get_name) $ List.sortBy (compare `on` get_name) x -- compare names of bound names only
            in filter ((1/=) . length) grouped
            where
                get_name (n, _, _) = n

        make_map x = Map.fromList (map (\ (n, _, v) -> (n, v)) x)

        report_dups = mapM_
            (\ decls ->
                let (first_name, _, _) = head decls
                in Compiler.tell_error $ MultipleDecls first_name (map get_decl_at decls))
            where
                get_decl_at (_, d, _) = d

collect_child_maps :: UnevaledModuleArena -> UnevaledTypeSynonymArena -> (EvalReader UnevaledADTArena UnevaledBoundValueArena module_child_maps (MakeDeclState CollectingErrors)) (Arena.Arena DeclMap SIR.ModuleKey)
collect_child_maps mod_arena type_synonym_arena = Arena.transformM go mod_arena
    where
        primitive_decls =
            new_decl (SIR.Decl'Type Type.Type'Int) >>= \ int ->
            new_decl (SIR.Decl'Type Type.Type'Float) >>= \ float ->
            new_decl (SIR.Decl'Type Type.Type'Char) >>= \ char ->
            new_decl (SIR.Decl'Type Type.Type'String) >>= \ string ->
            new_decl (SIR.Decl'Type Type.Type'Bool) >>= \ bool ->
            pure
                [ ("int", ImplicitPrim, int)
                , ("float", ImplicitPrim, float)
                , ("char", ImplicitPrim, char)
                , ("string", ImplicitPrim, string)
                , ("bool", ImplicitPrim, bool)
                ]
        primitive_bvs = pure []

        go (SIR.Module _ bindings adts type_synonyms) =
            (unzip3 <$> mapM binding_children bindings) >>= \ (binding_decl_entries, binding_bv_entries, binding_variant_entries) ->
            unzip3 <$>
                mapM
                    (\ adt ->
                        ask_adt_arena >>= \ adt_arena ->
                        let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                        in lift (new_decl (SIR.Decl'Type $ Type.Type'ADT adt [])) >>= \ adt_decl_key ->
                        pure ([(name, DeclAt name_sp, adt_decl_key)], [], []) -- constructor bvs and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                    )
                    adts >>= \ (adt_decl_entries, adt_bv_entries, adt_variant_entries) ->
            unzip3 <$>
                mapM
                    (\ synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                        in lift (new_decl (SIR.Decl'Type $ Type.Type'Synonym synonym)) >>= \ synonym_decl_key ->
                        pure ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                    type_synonyms >>= \ (type_synonym_decl_entries, type_synonym_bv_entries, type_synonym_variant_entries) ->
            lift primitive_decls >>= \ primitive_decls ->
            lift primitive_bvs >>= \ primitive_bvs ->
            lift (lift $ make_child_maps
                (concat $ primitive_decls : binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
                (concat $ primitive_bvs : binding_bv_entries ++ adt_bv_entries ++ type_synonym_bv_entries)
                (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries))

resolve_in_mods :: UnevaledModuleArena -> (EvalReader UnevaledADTArena UnevaledBoundValueArena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledModuleArena
resolve_in_mods module_arena = Arena.transformM resolve_in_module module_arena

resolve_in_adts :: UnevaledADTArena -> (EvalReader adt_arena bv_arena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledADTArena
resolve_in_adts adt_arena = Arena.transformM resolve_in_adt adt_arena

resolve_in_type_synonyms :: UnevaledTypeSynonymArena -> (EvalReader adt_arena bv_arena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledTypeSynonymArena
resolve_in_type_synonyms type_synonym_arena = Arena.transformM resolve_in_type_synonym type_synonym_arena

resolve_in_module :: UnevaledModule -> EvalReader UnevaledADTArena UnevaledBoundValueArena ModuleDeclMaps (MakeDeclState CollectingErrors) EvaledModule
resolve_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM resolve_in_binding bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: UnevaledADT -> (EvalReader adt_arena bv_arena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledADT
resolve_in_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM resolve_in_variant variants
    where
        resolve_in_variant (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id <$> mapM (\ (id, name, ty) -> (id, name, ) <$> resolve_in_type_expr ty) fields
        resolve_in_variant (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id <$> mapM (\ (id, ty) -> (id,) <$> resolve_in_type_expr ty) fields

resolve_in_type_synonym :: UnevaledTypeSynonym -> (EvalReader adt_arena bv_arena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledTypeSynonym
resolve_in_type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> resolve_in_type_expr expansion

resolve_in_binding :: UnevaledBinding -> (EvalReader UnevaledADTArena UnevaledBoundValueArena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledBinding
resolve_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat target <*> pure eq_sp <*> resolve_in_expr expr
resolve_in_binding (SIR.Binding'ADTVariant bvk variant vars sp) = pure $ SIR.Binding'ADTVariant bvk variant vars sp

-- TODO: all of the todos here will be fixed when types get rewritten
resolve_in_type_expr :: UnevaledTypeExpr -> (EvalReader adt_arena bv_arena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledTypeExpr
resolve_in_type_expr (SIR.TypeExpr'Refer () sp iden) = pure (SIR.TypeExpr'Refer iden sp iden)
resolve_in_type_expr (SIR.TypeExpr'Get () sp parent name) = do
    decls <- ask_decl_arena
    mods <- ask_module_child_maps
    parent <- resolve_in_type_expr parent
    result <- case SIR.type_expr_evaled parent of
        Just parent -> case get_decl_child decls mods parent name of
            Right r -> pure $ Just r
            Left e -> lift (lift $ Compiler.tell_error e) >> pure Nothing
        Nothing -> pure Nothing

    pure (SIR.TypeExpr'Get result sp parent name)
resolve_in_type_expr (SIR.TypeExpr'Tuple () sp a b) =
    resolve_in_type_expr a >>= \ a_conv ->
    resolve_in_type_expr b >>= \ b_conv ->
    lift (evaled_as_type a_conv) >>= \ a_as_type ->
    lift (evaled_as_type b_conv) >>= \ b_as_type ->
    pure (SIR.TypeExpr'Tuple (todo $ Type.Type'Tuple <$> a_as_type <*> b_as_type) sp a_conv b_conv)
resolve_in_type_expr (SIR.TypeExpr'Hole () ty sp hid) = pure $ SIR.TypeExpr'Hole Nothing todo sp hid  -- TODO: also make this an unknown; also rewrite typing thingies
resolve_in_type_expr (SIR.TypeExpr'Function () sp arg res) =
    resolve_in_type_expr arg >>= \ arg ->
    resolve_in_type_expr res >>= \ res ->
    lift (evaled_as_type arg) >>= \ arg_as_type ->
    lift (evaled_as_type res) >>= \ res_as_type ->
    pure (SIR.TypeExpr'Function (todo $ Type.Type'Function <$> arg_as_type <*> res_as_type) sp arg res)
resolve_in_type_expr (SIR.TypeExpr'Forall () sp vars inner) =
    resolve_in_type_expr inner >>= \ inner ->
    lift (evaled_as_type inner) >>= \ inner_as_type ->
    pure (SIR.TypeExpr'Forall (todo (Type.Type'Forall vars <$> inner_as_type)) sp vars inner)
{- TODO
type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    type_expr ty >>= \ ty ->
    type_expr arg >>= \ arg ->
    apply_type (TypeExpr sp) sp (_ ty) (_ arg) >>= \ result_ty ->
    pure (SIR.TypeExpr'Apply result_ty sp ty arg)
-}
resolve_in_type_expr (SIR.TypeExpr'Apply () sp ty args) = SIR.TypeExpr'Apply todo sp <$> resolve_in_type_expr ty <*> resolve_in_type_expr args
resolve_in_type_expr (SIR.TypeExpr'Wild () sp) = pure $ SIR.TypeExpr'Wild Nothing sp -- TODO: make this an unknown to be inferred and not a Nothing
resolve_in_type_expr (SIR.TypeExpr'Poison () sp) = pure $ SIR.TypeExpr'Poison Nothing sp

resolve_in_pat :: UnevaledPattern -> (EvalReader adt_arena bv_arena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledPattern
resolve_in_pat (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat a <*> resolve_in_pat b
resolve_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat field_pat) subpat
resolve_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: UnevaledExpr -> (EvalReader UnevaledADTArena UnevaledBoundValueArena ModuleDeclMaps (MakeDeclState CollectingErrors)) EvaledExpr
resolve_in_expr (SIR.Expr'Identifier id type_info sp iden_split ()) = SIR.Expr'Identifier id type_info sp <$> resolve_split_iden iden_split <*> pure ()
resolve_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr a <*> resolve_in_expr b

resolve_in_expr (SIR.Expr'Lambda id type_info sp param body) =
    SIR.Expr'Lambda id type_info sp <$> resolve_in_pat param <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'Let id type_info sp bindings body) =
    SIR.Expr'Let id type_info sp <$> mapM resolve_in_binding bindings <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'LetRec id type_info sp bindings body) =
    SIR.Expr'LetRec id type_info sp <$> mapM resolve_in_binding bindings <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> resolve_in_expr first
        <*> mapM
            (\ (sp, iden, (), rhs) ->
                (sp,,(),)
                    <$> resolve_split_iden iden
                    <*> resolve_in_expr rhs)
            ops

resolve_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr callee <*> resolve_in_expr arg

resolve_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr cond <*> resolve_in_expr t <*> resolve_in_expr f
resolve_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> resolve_in_expr e
        <*> mapM
                (\ (pat, expr) ->
                    (,)
                        <$> resolve_in_pat pat
                        <*> resolve_in_expr expr
                )
                arms

resolve_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    resolve_in_type_expr ty >>= \ ty ->
    resolve_in_expr e >>= \ e ->
    lift (evaled_as_type ty) >>= \ ty_as_type ->
    pure (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_as_type) e)

resolve_in_expr (SIR.Expr'Forall id type_info sp vars e) = SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr e
resolve_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) = resolve_in_expr e >>= \ e -> resolve_in_type_expr arg >>= \ arg -> lift (evaled_as_type arg) >>= \ arg_as_type -> pure (SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type))

resolve_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

resolve_split_iden :: SIR.SplitIdentifier Unevaled start -> EvalReader adt_arena bv_arena ModuleDeclMaps (MakeDeclState CollectingErrors) (SIR.SplitIdentifier Evaled start)
resolve_split_iden (SIR.SplitIdentifier'Get texpr next) = resolve_in_type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr next)
resolve_split_iden (SIR.SplitIdentifier'Single start) = pure (SIR.SplitIdentifier'Single start)

get_decl_child :: DeclArena -> ModuleDeclMaps -> SIR.DeclKey -> Located Text -> Either Error SIR.DeclKey
get_decl_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let d_children = Arena.get mods m
                in Map.lookup (Located.unlocate name) d_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

evaled_as_type :: EvaledTypeExpr -> MakeDeclState CollectingErrors (Maybe (Type.Type Void))
evaled_as_type texpr =
    case SIR.type_expr_evaled texpr of
        Just evaled ->
            get >>= \ decls ->
            case Arena.get decls evaled of
                SIR.Decl'Module _ -> lift (Compiler.tell_error (NotAType (SIR.type_expr_span texpr) "a module")) >> pure Nothing
                SIR.Decl'Type ty -> pure $ Just ty
        Nothing -> pure Nothing
