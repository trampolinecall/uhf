module UHF.Data.IR.Type.Dump
    ( define_adt
    , define_type_synonym
    , refer_adt
    , refer_type_synonym
    , refer_type
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

-- TODO: just print adt, dont get from adt arena
define_adt :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Type.ADTKey -> DumpUtils.Dumper ()
define_adt adts k =
    let (Type.ADT _ name _) = Arena.get adts k
    in DumpUtils.dump "data " >> DumpUtils.dump name >> DumpUtils.dump ";\n" -- TODO

-- TODO: dont get from type_synonyms either
define_type_synonym :: (ty -> DumpUtils.Dumper ()) -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Type.TypeSynonymKey -> DumpUtils.Dumper ()
define_type_synonym show_ty type_synonyms k =
    let (Type.TypeSynonym _ name expansion) = Arena.get type_synonyms k
    in DumpUtils.dump "typesyn " >> DumpUtils.dump name >> DumpUtils.dump " = " >> show_ty expansion >> DumpUtils.dump ";\n"

-- TODO: just print adt, dont get from adt arena
refer_adt :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Type.ADTKey -> DumpUtils.Dumper ()
refer_adt adts k =
    let (Type.ADT id _ _) = Arena.get adts k
    in DumpUtils.dump $ ID.stringify id

-- TODO: dont get from type_synonyms either
refer_type_synonym :: Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Type.TypeSynonymKey -> DumpUtils.Dumper ()
refer_type_synonym type_synonyms k =
    let (Type.TypeSynonym id _ _) = Arena.get type_synonyms k
    in DumpUtils.dump $ ID.stringify id

refer_type :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Type.Type Void -> DumpUtils.Dumper ()
refer_type adts _ (Type.Type'ADT k) = refer_adt adts k
refer_type _ type_synonyms (Type.Type'Synonym k) = refer_type_synonym type_synonyms k
refer_type _ _ Type.Type'Int = DumpUtils.dump "int"
refer_type _ _ Type.Type'Float = DumpUtils.dump "float"
refer_type _ _ Type.Type'Char = DumpUtils.dump "char"
refer_type _ _ Type.Type'String = DumpUtils.dump "string"
refer_type _ _ Type.Type'Bool = DumpUtils.dump "bool"
refer_type adts type_synonyms (Type.Type'Function a r) = refer_type adts type_synonyms a >> DumpUtils.dump " -> " >> refer_type adts type_synonyms r
refer_type adts type_synonyms (Type.Type'Tuple a b) = DumpUtils.dump "(" >> refer_type adts type_synonyms a >> DumpUtils.dump ", " >> refer_type adts type_synonyms b >> DumpUtils.dump ")"
refer_type _ _ (Type.Type'Variable void) = absurd void
