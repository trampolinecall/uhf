module UHF.Phases.Back.ToBackendIR (convert) where

import UHF.Util.Prelude

import qualified Arena
import qualified Unique

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.BackendIR as BackendIR
import qualified UHF.Data.IR.Type as Type

type Type = Maybe (Type.Type Void)

type ANFIR = ANFIR.ANFIR
type ANFIRDecl = ANFIR.Decl
type ANFIRExpr = ANFIR.Expr
type ANFIRParam = ANFIR.Param
type ANFIRBinding = ANFIR.Binding
type ANFIRBindingGroup = ANFIR.BindingGroup

type ANFIRDeclArena = Arena.Arena ANFIRDecl ANFIR.DeclKey
type ANFIRBindingArena = Arena.Arena ANFIRBinding ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

type BackendIR = BackendIR.BackendIR () () () Type ()
type BackendIRExpr = BackendIR.Expr () Type ()
type BackendIRParam = BackendIR.Param Type
type BackendIRBinding = BackendIR.Binding () () () Type ()
type BackendIRBindingGroup = BackendIR.BindingGroup ()

type BackendIRBindingArena = Arena.Arena BackendIRBinding BackendIR.BindingKey
type BackendIRParamArena = Arena.Arena BackendIRParam BackendIR.ParamKey

convert :: ANFIR -> BackendIR
convert (ANFIR.ANFIR decls adts type_synonyms type_vars bindings params mod) =
    Unique.run_unique_maker $
        Arena.transformM convert_binding bindings >>= \ bindings ->
        let params' = Arena.transform convert_param params
        in assemble_cu decls mod >>= \ cu ->
        pure (BackendIR.BackendIR adts type_synonyms type_vars bindings params' cu)

assemble_cu :: ANFIRDeclArena -> ANFIR.DeclKey -> Unique.UniqueMaker (BackendIR.CU ())
assemble_cu decls mod = go_decl (Arena.get decls mod)
    where
        go_decl (ANFIR.Decl'Module group adts synonyms) = BackendIR.CU <$> (convert_binding_group group) <*> pure adts <*> pure synonyms
        go_decl (ANFIR.Decl'Type _) = unreachable

convert_binding :: ANFIRBinding -> Unique.UniqueMaker BackendIRBinding
convert_binding (ANFIR.Binding initializer) = BackendIR.Binding () () <$> (convert_expr initializer)

convert_binding_group :: ANFIRBindingGroup -> Unique.UniqueMaker BackendIRBindingGroup
convert_binding_group (ANFIR.BindingGroup bindings) = Unique.make_unique >>= \ uniq -> pure (BackendIR.BindingGroup uniq () bindings)

convert_param :: ANFIRParam -> BackendIRParam
convert_param (ANFIR.Param bvid ty) = BackendIR.Param bvid ty

-- TODO: figure out a better solution than this
convert_id :: ANFIR.ID -> BackendIR.ID
convert_id (ANFIR.ExprID e) = BackendIR.ExprID e
convert_id (ANFIR.BVID e) = BackendIR.BVID e

convert_expr :: ANFIRExpr -> Unique.UniqueMaker BackendIRExpr
convert_expr (ANFIR.Expr'Refer id ty bk) = pure $ BackendIR.Expr'Refer (convert_id id) ty bk
convert_expr (ANFIR.Expr'Int id ty i) = pure $ BackendIR.Expr'Int (convert_id id) ty i
convert_expr (ANFIR.Expr'Float id ty f) = pure $ BackendIR.Expr'Float (convert_id id) ty f
convert_expr (ANFIR.Expr'Bool id ty b) = pure $ BackendIR.Expr'Bool (convert_id id) ty b
convert_expr (ANFIR.Expr'Char id ty c) = pure $ BackendIR.Expr'Char (convert_id id) ty c
convert_expr (ANFIR.Expr'String id ty s) = pure $ BackendIR.Expr'String (convert_id id) ty s
convert_expr (ANFIR.Expr'Tuple id ty a b) = pure $ BackendIR.Expr'Tuple (convert_id id) ty a b
convert_expr (ANFIR.Expr'MakeADT id ty var_idx args) = pure $ BackendIR.Expr'MakeADT (convert_id id) ty var_idx args
convert_expr (ANFIR.Expr'Lambda id ty param group result) = BackendIR.Expr'Lambda (convert_id id) ty param <$> (convert_binding_group group) <*> pure result
convert_expr (ANFIR.Expr'Param id ty param) = pure $ BackendIR.Expr'Param (convert_id id) ty param
convert_expr (ANFIR.Expr'Call id ty callee arg) = pure $ BackendIR.Expr'Call (convert_id id) ty callee arg
convert_expr (ANFIR.Expr'Switch id ty scrutinee arms) = BackendIR.Expr'Switch (convert_id id) ty scrutinee <$> mapM (\ (matcher, group, result) -> (convert_matcher matcher,, result) <$> convert_binding_group group) arms
    where
        convert_matcher (ANFIR.Switch'BoolLiteral b) = BackendIR.Switch'BoolLiteral b
        convert_matcher (ANFIR.Switch'Tuple) = BackendIR.Switch'Tuple
        convert_matcher (ANFIR.Switch'Default) = BackendIR.Switch'Default
convert_expr (ANFIR.Expr'Seq id ty a b) = pure $ BackendIR.Expr'Seq (convert_id id) ty a b
convert_expr (ANFIR.Expr'TupleDestructure1 id ty tup) = pure $ BackendIR.Expr'TupleDestructure1 (convert_id id) ty tup
convert_expr (ANFIR.Expr'TupleDestructure2 id ty tup) = pure $ BackendIR.Expr'TupleDestructure2 (convert_id id) ty tup
convert_expr (ANFIR.Expr'Forall id ty tvars group result) = BackendIR.Expr'Forall (convert_id id) ty tvars <$> convert_binding_group group <*> pure result
convert_expr (ANFIR.Expr'TypeApply id ty e tyarg) = pure $ BackendIR.Expr'TypeApply (convert_id id) ty e tyarg
convert_expr (ANFIR.Expr'Poison id ty) = pure $ BackendIR.Expr'Poison (convert_id id) ty ()
