module UHF.Phases.ToBackendIR (convert) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.ANFIR as ANFIR
import qualified UHF.Data.BackendIR as BackendIR
import qualified UHF.Data.IR.Type as Type

type Type = Maybe (Type.Type Void)

type ANFIR = ANFIR.ANFIR
type ANFIRExpr = ANFIR.Expr
type ANFIRParam = ANFIR.Param
type ANFIRBinding = ANFIR.Binding
type ANFIRBindingGroup = ANFIR.BindingGroup
type ANFIRBindingChunk = ANFIR.BindingChunk

type ANFIRBindingArena = Arena.Arena ANFIRBinding ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

type BackendIR = BackendIR.BackendIR Type ()
type BackendIRExpr = BackendIR.Expr Type ()
type BackendIRParam = BackendIR.Param Type
type BackendIRBinding = BackendIR.Binding Type ()
type BackendIRBindingGroup = BackendIR.BindingGroup
type BackendIRBindingChunk = BackendIR.BindingChunk

type BackendIRBindingArena = Arena.Arena BackendIRBinding BackendIR.BindingKey
type BackendIRParamArena = Arena.Arena BackendIRParam BackendIR.ParamKey

convert :: ANFIR -> BackendIR
convert (ANFIR.ANFIR adts type_synonyms type_vars bindings params cu) =
    let bindings' = Arena.transform convert_binding bindings
        params' = Arena.transform convert_param params
        cu' = convert_cu cu
    in BackendIR.BackendIR adts type_synonyms type_vars bindings' params' cu'

convert_cu :: ANFIR.CU -> BackendIR.CU
convert_cu (ANFIR.CU group adts type_synonyms) = BackendIR.CU (convert_binding_group group) adts type_synonyms

convert_binding :: ANFIRBinding -> BackendIRBinding
convert_binding (ANFIR.Binding initializer) = BackendIR.Binding $ convert_expr initializer

convert_binding_group :: ANFIRBindingGroup -> BackendIRBindingGroup
convert_binding_group (ANFIR.BindingGroup chunks) = BackendIR.BindingGroup (map convert_binding_chunk chunks)

convert_binding_chunk :: ANFIRBindingChunk -> BackendIRBindingChunk
convert_binding_chunk (ANFIR.SingleBinding bk) = BackendIR.SingleBinding bk
convert_binding_chunk (ANFIR.MutuallyRecursiveBindings bs) = BackendIR.MutuallyRecursiveBindings bs

convert_param :: ANFIRParam -> BackendIRParam
convert_param (ANFIR.Param bvid ty) = BackendIR.Param bvid ty

-- TODO: figure out a better solution than this
convert_id :: ANFIR.ID -> BackendIR.ID
convert_id (ANFIR.ExprID e) = BackendIR.ExprID e
convert_id (ANFIR.BVID e) = BackendIR.BVID e

convert_expr :: ANFIRExpr -> BackendIRExpr
convert_expr (ANFIR.Expr'Refer id ty bk) = BackendIR.Expr'Refer (convert_id id) ty bk
convert_expr (ANFIR.Expr'Int id ty i) = BackendIR.Expr'Int (convert_id id) ty i
convert_expr (ANFIR.Expr'Float id ty f) = BackendIR.Expr'Float (convert_id id) ty f
convert_expr (ANFIR.Expr'Bool id ty b) = BackendIR.Expr'Bool (convert_id id) ty b
convert_expr (ANFIR.Expr'Char id ty c) = BackendIR.Expr'Char (convert_id id) ty c
convert_expr (ANFIR.Expr'String id ty s) = BackendIR.Expr'String (convert_id id) ty s
convert_expr (ANFIR.Expr'Tuple id ty a b) = BackendIR.Expr'Tuple (convert_id id) ty a b
convert_expr (ANFIR.Expr'MakeADT id ty var_idx tyargs args) = BackendIR.Expr'MakeADT (convert_id id) ty var_idx tyargs args
convert_expr (ANFIR.Expr'Lambda id ty param captures group result) = BackendIR.Expr'Lambda (convert_id id) ty param captures (convert_binding_group group) result
convert_expr (ANFIR.Expr'Param id ty param) = BackendIR.Expr'Param (convert_id id) ty param
convert_expr (ANFIR.Expr'Call id ty callee arg) = BackendIR.Expr'Call (convert_id id) ty callee arg
convert_expr (ANFIR.Expr'Match id ty tree) = BackendIR.Expr'Match (convert_id id) ty (convert_tree tree)
    where
        convert_tree (ANFIR.MatchTree arms) =
            BackendIR.MatchTree $
                map
                    (\ (clauses, result) ->
                        let result' = case result of
                                Right (group, result) -> Right (convert_binding_group group, result)
                                Left subtree -> Left $ convert_tree subtree
                        in (map convert_clause clauses, result'))
                    arms

        convert_clause (ANFIR.MatchClause'Match b matcher) = BackendIR.MatchClause'Match b (convert_matcher matcher)
        convert_clause (ANFIR.MatchClause'Binding b) = BackendIR.MatchClause'Binding b

        convert_matcher (ANFIR.Match'BoolLiteral b) = BackendIR.Match'BoolLiteral b
        convert_matcher ANFIR.Match'Tuple = BackendIR.Match'Tuple
        convert_matcher (ANFIR.Match'AnonADTVariant (Just v)) = BackendIR.Match'AnonADTVariant (Right v)
        convert_matcher (ANFIR.Match'AnonADTVariant Nothing) = BackendIR.Match'AnonADTVariant (Left ())
convert_expr (ANFIR.Expr'TupleDestructure1 id ty tup) = BackendIR.Expr'TupleDestructure1 (convert_id id) ty tup
convert_expr (ANFIR.Expr'TupleDestructure2 id ty tup) = BackendIR.Expr'TupleDestructure2 (convert_id id) ty tup
convert_expr (ANFIR.Expr'ADTDestructure id ty b field_idx) = BackendIR.Expr'ADTDestructure (convert_id id) ty b (maybe (Left ()) Right field_idx)
convert_expr (ANFIR.Expr'Forall id ty tvars group result) = BackendIR.Expr'Forall (convert_id id) ty tvars (convert_binding_group group) result
convert_expr (ANFIR.Expr'TypeApply id ty e tyarg) = BackendIR.Expr'TypeApply (convert_id id) ty e tyarg
convert_expr (ANFIR.Expr'Poison id ty) = BackendIR.Expr'Poison (convert_id id) ty ()
