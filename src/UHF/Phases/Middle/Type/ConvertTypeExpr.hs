module UHF.Phases.Middle.Type.ConvertTypeExpr (adt, type_synonym, type_expr) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.Phases.Middle.Type.Var
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error
import UHF.Phases.Middle.Type.StateWithVars

adt :: UntypedDeclArena -> UntypedADT -> StateWithVars TypedWithVarsADT
adt decls (Type.ADT name variants) = Type.ADT name <$> mapM (convert_variant decls) variants
    where
        convert_variant decls (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> type_expr decls ty) fields
        convert_variant decls (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> mapM (type_expr decls) fields

type_synonym :: UntypedDeclArena -> UntypedTypeSynonym -> StateWithVars TypedWithVarsTypeSynonym
type_synonym decls (Type.TypeSynonym name expansion) = Type.TypeSynonym name <$> type_expr decls expansion

type_expr :: UntypedDeclArena -> TypeExpr -> StateWithVars TypeWithVars
type_expr decls (HIR.TypeExpr'Identifier sp iden) =
    case iden of -- TODO: make poison type variable
        Just i -> case Arena.get decls i of
            HIR.Decl'Module _ _ _ _ -> lift (Compiler.tell_error $ NotAType sp "a module") >> Type.Type'Variable <$> new_type_variable (TypeExpr sp)
            HIR.Decl'Type ty -> pure $ void_var_to_key ty
        Nothing -> Type.Type'Variable <$> new_type_variable (TypeExpr sp)
    where
        -- basically useless function for converting Type Void to Type TypeVarKey
        void_var_to_key (Type.Type'ADT k) = Type.Type'ADT k
        void_var_to_key (Type.Type'Synonym k) = Type.Type'Synonym k
        void_var_to_key Type.Type'Int = Type.Type'Int
        void_var_to_key Type.Type'Float = Type.Type'Float
        void_var_to_key Type.Type'Char = Type.Type'Char
        void_var_to_key Type.Type'String = Type.Type'String
        void_var_to_key Type.Type'Bool = Type.Type'Bool
        void_var_to_key (Type.Type'Function a r) = Type.Type'Function (void_var_to_key a) (void_var_to_key r)
        void_var_to_key (Type.Type'Tuple a b) = Type.Type'Tuple (void_var_to_key a) (void_var_to_key b)
        void_var_to_key (Type.Type'Variable void) = absurd void

type_expr decls (HIR.TypeExpr'Tuple a b) = Type.Type'Tuple <$> type_expr decls a <*> type_expr decls b
type_expr _ (HIR.TypeExpr'Poison sp) = Type.Type'Variable <$> new_type_variable (TypeExpr sp)
