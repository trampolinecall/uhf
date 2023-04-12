module UHF.Data.IR.Type.PP
    ( define_adt
    , define_type_synonym
    , refer_adt
    , refer_type_synonym
    , refer_type
    , refer_type_m
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PPUtils as PPUtils

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

import Data.Functor.Identity (runIdentity)

define_adt :: Type.ADT ty -> PPUtils.PP ()
define_adt (Type.ADT _ name _ _) = PPUtils.write "data " >> PPUtils.write name >> PPUtils.write ";\n" -- TODO: variants and type vars

define_type_synonym :: (ty -> PPUtils.PP ()) -> Type.TypeSynonym ty -> PPUtils.PP ()
define_type_synonym show_ty (Type.TypeSynonym _ name expansion) = PPUtils.write "typesyn " >> PPUtils.write name >> PPUtils.write " = " >> show_ty expansion >> PPUtils.write ";\n"

refer_adt :: Type.ADT ty -> PPUtils.PP ()
refer_adt (Type.ADT id _ _ _) = PPUtils.write $ ID.stringify id

refer_type_synonym :: Type.TypeSynonym ty -> PPUtils.PP ()
refer_type_synonym (Type.TypeSynonym id _ _) = PPUtils.write $ ID.stringify id

-- TODO: construct an ast and print it
refer_type :: (tyunk -> PPUtils.PP ()) -> Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Arena.Arena Type.Var Type.TypeVarKey -> Type.Type tyunk -> PPUtils.PP ()
refer_type show_tyunk adts type_synonyms vars ty = runIdentity $ refer_type_m (pure . show_tyunk) adts type_synonyms vars ty

refer_type_m :: Monad m => (tyunk -> m (PPUtils.PP ())) -> Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Arena.Arena Type.Var Type.TypeVarKey -> Type.Type tyunk -> m (PPUtils.PP ())
refer_type_m _ adts _ _ (Type.Type'ADT k params) =
    let params'
            | null params = PPUtils.write ""
            | otherwise = PPUtils.write "#(" >> PPUtils.write ")" -- TODO
    in pure $ refer_adt (Arena.get adts k) >> params'
refer_type_m _ _ type_synonyms _ (Type.Type'Synonym k) = pure $ refer_type_synonym $ Arena.get type_synonyms k
refer_type_m _ _ _ _ Type.Type'Int = pure $ PPUtils.write "int"
refer_type_m _ _ _ _ Type.Type'Float = pure $ PPUtils.write "float"
refer_type_m _ _ _ _ Type.Type'Char = pure $ PPUtils.write "char"
refer_type_m _ _ _ _ Type.Type'String = pure $ PPUtils.write "string"
refer_type_m _ _ _ _ Type.Type'Bool = pure $ PPUtils.write "bool"
refer_type_m show_tyunk adts type_synonyms vars (Type.Type'Function a r) = do
    a_shown <- refer_type_m show_tyunk adts type_synonyms vars a
    r_shown <- refer_type_m show_tyunk adts type_synonyms vars r
    pure (a_shown >> PPUtils.write " -> " >> r_shown)
refer_type_m show_tyunk adts type_synonyms vars (Type.Type'Tuple a b) = do
    a_shown <- refer_type_m show_tyunk adts type_synonyms vars a
    b_shown <- refer_type_m show_tyunk adts type_synonyms vars b
    pure (PPUtils.write "(" >> a_shown >> PPUtils.write ", " >> b_shown >> PPUtils.write ")")
refer_type_m show_tyunk _ _ _ (Type.Type'Unknown unk) = show_tyunk unk
refer_type_m _ _ _ vars (Type.Type'Variable var) =
    let (Type.Var name) = Arena.get vars var
    in pure $ PPUtils.write name -- TODO: write id
refer_type_m show_tyunk adts type_synonyms vars (Type.Type'Forall new_vars ty) = do
    ty <- refer_type_m show_tyunk adts type_synonyms vars ty
    pure $ PPUtils.write "#(" >> PPUtils.write ") " >> ty -- TODO: print vars
