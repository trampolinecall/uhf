module UHF.Data.IR.Type.PP
    ( define_adt
    , define_type_synonym
    , refer_adt
    , refer_type_synonym
    , refer_type
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PPUtils as PPUtils

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

define_adt :: Type.ADT ty -> PPUtils.PP ()
define_adt (Type.ADT _ name _) = PPUtils.write "data " >> PPUtils.write name >> PPUtils.write ";\n" -- TODO

define_type_synonym :: (ty -> PPUtils.PP ()) -> Type.TypeSynonym ty -> PPUtils.PP ()
define_type_synonym show_ty (Type.TypeSynonym _ name expansion) = PPUtils.write "typesyn " >> PPUtils.write name >> PPUtils.write " = " >> show_ty expansion >> PPUtils.write ";\n"

refer_adt :: Type.ADT ty -> PPUtils.PP ()
refer_adt (Type.ADT id _ _) = PPUtils.write $ ID.stringify id

refer_type_synonym :: Type.TypeSynonym ty -> PPUtils.PP ()
refer_type_synonym (Type.TypeSynonym id _ _) = PPUtils.write $ ID.stringify id

refer_type :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Type.Type Void -> PPUtils.PP ()
refer_type adts _ (Type.Type'ADT k) = refer_adt $ Arena.get adts k
refer_type _ type_synonyms (Type.Type'Synonym k) = refer_type_synonym $ Arena.get type_synonyms k
refer_type _ _ Type.Type'Int = PPUtils.write "int"
refer_type _ _ Type.Type'Float = PPUtils.write "float"
refer_type _ _ Type.Type'Char = PPUtils.write "char"
refer_type _ _ Type.Type'String = PPUtils.write "string"
refer_type _ _ Type.Type'Bool = PPUtils.write "bool"
refer_type adts type_synonyms (Type.Type'Function a r) = refer_type adts type_synonyms a >> PPUtils.write " -> " >> refer_type adts type_synonyms r
refer_type adts type_synonyms (Type.Type'Tuple a b) = PPUtils.write "(" >> refer_type adts type_synonyms a >> PPUtils.write ", " >> refer_type adts type_synonyms b >> PPUtils.write ")"
refer_type _ _ (Type.Type'Variable void) = absurd void
