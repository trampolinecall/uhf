module UHF.Phases.Middle.Type.AddTypes (adt, type_synonym, type_expr, bound_value) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error
import UHF.Phases.Middle.Type.StateWithUnk

adt :: UntypedDeclArena -> UntypedADT -> StateWithUnk TypedWithUnkADT
adt decls (Type.ADT id name variants) = Type.ADT id name <$> mapM (convert_variant decls) variants
    where
        convert_variant decls (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> type_expr decls ty) fields
        convert_variant decls (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> mapM (type_expr decls) fields

type_synonym :: UntypedDeclArena -> UntypedTypeSynonym -> StateWithUnk TypedWithUnkTypeSynonym
type_synonym decls (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> type_expr decls expansion

type_expr :: UntypedDeclArena -> UntypedTypeExpr -> StateWithUnk TypedWithUnkTypeExpr
type_expr decls (SIR.TypeExpr'Identifier () sp iden) = do
    ty <- case iden of -- TODO: make poison type variable
        Just i -> case Arena.get decls i of
            SIR.Decl'Module _ _ _ _ _ -> lift (Compiler.tell_error $ NotAType sp "a module") >> Type.Type'Unknown <$> new_type_unknown (TypeExpr sp)
            SIR.Decl'Type ty -> pure $ void_var_to_key ty
        Nothing -> Type.Type'Unknown <$> new_type_unknown (TypeExpr sp)
    pure (SIR.TypeExpr'Identifier ty sp iden)
    where
        -- basically useless function for converting Type Void to Type TypeUnknownKey
        void_var_to_key (Type.Type'ADT k) = Type.Type'ADT k
        void_var_to_key (Type.Type'Synonym k) = Type.Type'Synonym k
        void_var_to_key Type.Type'Int = Type.Type'Int
        void_var_to_key Type.Type'Float = Type.Type'Float
        void_var_to_key Type.Type'Char = Type.Type'Char
        void_var_to_key Type.Type'String = Type.Type'String
        void_var_to_key Type.Type'Bool = Type.Type'Bool
        void_var_to_key (Type.Type'Function a r) = Type.Type'Function (void_var_to_key a) (void_var_to_key r)
        void_var_to_key (Type.Type'Tuple a b) = Type.Type'Tuple (void_var_to_key a) (void_var_to_key b)
        void_var_to_key (Type.Type'Unknown void) = absurd void

type_expr decls (SIR.TypeExpr'Tuple () a b) =
    type_expr decls a >>= \ a_conv ->
    type_expr decls b >>= \ b_conv ->
    pure (SIR.TypeExpr'Tuple (Type.Type'Tuple (SIR.type_expr_type_info a_conv) (SIR.type_expr_type_info b_conv)) a_conv b_conv)

type_expr _ (SIR.TypeExpr'Hole () hid) = pure $ SIR.TypeExpr'Hole (todo) hid
type_expr decls (SIR.TypeExpr'Forall () names ty) = SIR.TypeExpr'Forall todo names <$> type_expr decls ty
type_expr decls (SIR.TypeExpr'Apply () ty args) = SIR.TypeExpr'Apply todo <$> type_expr decls ty <*> mapM (type_expr decls) args
type_expr _ (SIR.TypeExpr'Wild () sp) = Type.Type'Unknown <$> new_type_unknown (TypeExpr sp) >>= \ ty -> pure (SIR.TypeExpr'Wild ty sp)
type_expr _ (SIR.TypeExpr'Poison () sp) = Type.Type'Unknown <$> new_type_unknown (TypeExpr sp) >>= \ ty -> pure (SIR.TypeExpr'Poison ty sp)

bound_value :: UntypedBoundValue -> StateWithUnk TypedWithUnkBoundValue
bound_value (SIR.BoundValue id () def_span) = SIR.BoundValue id <$> (Type.Type'Unknown <$> new_type_unknown (BoundValue def_span)) <*> pure def_span
