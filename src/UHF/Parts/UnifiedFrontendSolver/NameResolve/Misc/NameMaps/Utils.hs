module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps.Utils
    ( decls_to_children
    , binding_to_children
    , pattern_to_children
    , quant_vars_to_children
    ) where

import UHF.Prelude

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.DeclAt (DeclAt (..))
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps (ADTVariantChild, DeclChild, ValueChild)
import UHF.Source.Located (Located (Located))
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (ValueRef(..), DeclRef (..))

decls_to_children :: Monad under => [SIR.Binding stage] -> [Type.ADTKey] -> [Type.TypeSynonymKey] -> ReaderT (SIR.SIR stage) under ([DeclChild], [ValueChild], [ADTVariantChild])
decls_to_children bindings adts type_synonyms = do
    (SIR.SIR _ adt_arena type_synonym_arena _ _ _) <- ask
    let (adt_decl_entries, adt_val_entries, adt_variant_entries) =
            adts
                & map
                    ( \adt ->
                        let (Type.ADT _ (Located adt_name_sp adt_name) _ _) = Arena.get adt_arena adt
                            (variant_constructors, variant_patterns) =
                                Type.ADT.variant_idxs adt_arena adt
                                    & map
                                        ( \variant_index ->
                                            case Type.ADT.get_variant adt_arena variant_index of
                                                Type.ADT.Variant'Anon (Located variant_name_sp variant_name) _ _ -> ((variant_name, DeclAt variant_name_sp, ValueRef'ADTVariantConstructor variant_index), (variant_name, DeclAt variant_name_sp, variant_index))
                                                Type.ADT.Variant'Named _ _ _ -> todo
                                        )
                                    & unzip
                        in ([(adt_name, DeclAt adt_name_sp, DeclRef'Type $ TypeWithInferVar.Type'ADT adt [])], variant_constructors, variant_patterns) -- TODO: make this deal with named variants too; also TODO: move variants to inside their types
                    )
                & unzip3

    let (type_synonym_decl_entries, type_synonym_val_entries, type_synonym_variant_entries) =
            type_synonyms
                & map
                    ( \synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                            synonym_decl_key = DeclRef'Type $ TypeWithInferVar.Type'Synonym synonym
                        in ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                & unzip3

    (binding_decl_entries, binding_val_entries, binding_variant_entries) <- unzip3 <$> mapM binding_to_children bindings

    pure
        ( concat $ binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries
        , concat $ binding_val_entries ++ adt_val_entries ++ type_synonym_val_entries
        , concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries
        )

binding_to_children :: Monad under => SIR.Binding stage -> ReaderT (SIR.SIR stage) under ([DeclChild], [ValueChild], [ADTVariantChild])
binding_to_children (SIR.Binding _ pat _ _) = ([],,[]) <$> pattern_to_children pat

pattern_to_children :: Monad under => SIR.Pattern stage -> ReaderT (SIR.SIR stage) under [ValueChild]
pattern_to_children (SIR.Pattern'Variable _ _ sp var_key) = do
    name <- var_name var_key
    pure [(name, DeclAt sp, ValueRef'Variable var_key)]
pattern_to_children (SIR.Pattern'Wildcard _ _ _) = pure []
pattern_to_children (SIR.Pattern'Tuple _ _ _ a b) = do
    a' <- pattern_to_children a
    b' <- pattern_to_children b
    pure $ a' ++ b'
pattern_to_children (SIR.Pattern'Named _ _ _ _ (Located var_span var_key) subpat) = do
    name <- var_name var_key
    subpat' <- pattern_to_children subpat
    pure ((name, DeclAt var_span, ValueRef'Variable var_key) : subpat')
pattern_to_children (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_to_children fields
pattern_to_children (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_to_children . snd) fields
pattern_to_children (SIR.Pattern'Poison _ _ _) = pure []
var_name :: Monad under => SIR.VariableKey -> ReaderT (SIR.SIR stage) under Text
var_name var_key = do
    (SIR.SIR _ _ _ _ var_arena _) <- ask
    let SIR.Variable _ _ _ (Located _ name) = Arena.get var_arena var_key
    pure name

quant_vars_to_children :: Monad under => [Type.QuantVarKey] -> ReaderT (SIR.SIR stage) under [DeclChild]
quant_vars_to_children =
    mapM
        ( \var -> do
            (SIR.SIR _ _ _ quant_var_arena _ _) <- ask
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            pure (name, DeclAt name_sp, DeclRef'Type $ TypeWithInferVar.Type'QuantVar var)
        )
