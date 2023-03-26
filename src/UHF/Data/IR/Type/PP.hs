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

-- TODO: just print adt, dont get from adt arena
define_adt :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Type.ADTKey -> PPUtils.PP ()
define_adt adts k =
    let (Type.ADT _ name _) = Arena.get adts k
    in PPUtils.write "data " >> PPUtils.write name >> PPUtils.write ";\n" -- TODO

-- TODO: dont get from type_synonyms either
define_type_synonym :: (ty -> PPUtils.PP ()) -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Type.TypeSynonymKey -> PPUtils.PP ()
define_type_synonym show_ty type_synonyms k =
    let (Type.TypeSynonym _ name expansion) = Arena.get type_synonyms k
    in PPUtils.write "typesyn " >> PPUtils.write name >> PPUtils.write " = " >> show_ty expansion >> PPUtils.write ";\n"

-- TODO: just print adt, dont get from adt arena
refer_adt :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Type.ADTKey -> PPUtils.PP ()
refer_adt adts k =
    let (Type.ADT id _ _) = Arena.get adts k
    in PPUtils.write $ ID.stringify id

-- TODO: dont get from type_synonyms either
refer_type_synonym :: Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Type.TypeSynonymKey -> PPUtils.PP ()
refer_type_synonym type_synonyms k =
    let (Type.TypeSynonym id _ _) = Arena.get type_synonyms k
    in PPUtils.write $ ID.stringify id

refer_type :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Type.Type Void -> PPUtils.PP ()
refer_type adts _ (Type.Type'ADT k) = refer_adt adts k
refer_type _ type_synonyms (Type.Type'Synonym k) = refer_type_synonym type_synonyms k
refer_type _ _ Type.Type'Int = PPUtils.write "int"
refer_type _ _ Type.Type'Float = PPUtils.write "float"
refer_type _ _ Type.Type'Char = PPUtils.write "char"
refer_type _ _ Type.Type'String = PPUtils.write "string"
refer_type _ _ Type.Type'Bool = PPUtils.write "bool"
refer_type adts type_synonyms (Type.Type'Function a r) = refer_type adts type_synonyms a >> PPUtils.write " -> " >> refer_type adts type_synonyms r
refer_type adts type_synonyms (Type.Type'Tuple a b) = PPUtils.write "(" >> refer_type adts type_synonyms a >> PPUtils.write ", " >> refer_type adts type_synonyms b >> PPUtils.write ")"
refer_type _ _ (Type.Type'Variable void) = absurd void
