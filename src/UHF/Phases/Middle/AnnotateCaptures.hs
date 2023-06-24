{-# LANGUAGE OverloadedLists #-}

module UHF.Phases.Middle.AnnotateCaptures (annotate) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Set as Set

import qualified UHF.Data.IR.ANFIR as ANFIR

type CaptureList = Set ANFIR.BindingKey -- TODO: dont use BindingKey Ord for order of captures in ts backend arguments

annotate :: ANFIR.ANFIR () ty poison_allowed -> ANFIR.ANFIR CaptureList ty poison_allowed
annotate (ANFIR.ANFIR decls adts type_synonyms type_vars bindings params mod) =
    let bindings' = Arena.transform (annotate_binding bindings') bindings
    in ANFIR.ANFIR (Arena.transform (annotate_decl bindings') decls) adts type_synonyms type_vars bindings' params mod
-- loops are not possible because an expression cannot have its parent binding group as a child binding group
-- so the annotation of an expression cannot depend on itself because any binding groups that it has cannot contain itself

annotate_decl :: Arena.Arena (ANFIR.Binding CaptureList ty poison_allowed) ANFIR.BindingKey -> ANFIR.Decl () -> ANFIR.Decl CaptureList
annotate_decl binding_arena (ANFIR.Decl'Module bindings adts type_synonyms) = ANFIR.Decl'Module (annotate_binding_group binding_arena bindings) adts type_synonyms
annotate_decl _ (ANFIR.Decl'Type ty) = ANFIR.Decl'Type ty

annotate_binding_group :: Arena.Arena (ANFIR.Binding CaptureList ty poison_allowed) ANFIR.BindingKey -> ANFIR.BindingGroup () -> ANFIR.BindingGroup CaptureList
annotate_binding_group binding_arena (ANFIR.BindingGroup unique () () bindings) =
    let captures = Set.unions $ map get_outward_references bindings
    in ANFIR.BindingGroup unique captures bindings
    where
        get_outward_references = Set.filter is_outward . get_references . ANFIR.binding_initializer . Arena.get binding_arena

        exclude_if_part_of_group :: ANFIR.BindingGroup captures -> ANFIR.BindingKey -> Set ANFIR.BindingKey
        exclude_if_part_of_group (ANFIR.BindingGroup unique _ _) binding =
            let (ANFIR.Binding (ANFIR.BoundWhere def_bg) _) = Arena.get binding_arena binding
            in if def_bg == unique then [] else [binding]

        get_references :: ANFIR.Expr CaptureList ty poison_allowed -> (Set ANFIR.BindingKey, Set ANFIR.BindingKey)
        get_references (ANFIR.Expr'Refer _ _ i) = ([i], [])
        get_references (ANFIR.Expr'Char _ _ _) = ([], [])
        get_references (ANFIR.Expr'String _ _ _) = ([], [])
        get_references (ANFIR.Expr'Int _ _ _) = ([], [])
        get_references (ANFIR.Expr'Float _ _ _) = ([], [])
        get_references (ANFIR.Expr'Bool _ _ _) = ([], [])
        get_references (ANFIR.Expr'Tuple _ _ a b) = [a, b]
        get_references (ANFIR.Expr'MakeADT _ _ _ args) = Set.fromList args
        get_references (ANFIR.Expr'Lambda _ _ _ group result) = ([], ANFIR.binding_group_immediate_captures group <> ANFIR.binding_group_late_captures group <> exclude_if_part_of_group group result) -- all captures get turned into late captures
        get_references (ANFIR.Expr'Param _ _ _) = ([], [])
        get_references (ANFIR.Expr'Call _ _ callee arg) = [callee, arg]
        get_references (ANFIR.Expr'Switch _ _ test arms) = ([test] <> Set.unions (map (\ (_, group, result) -> ANFIR.binding_group_immediate_captures group <> exclude_if_part_of_group group result) arms), Set.unions (map (\ (_, group, result) -> ANFIR.binding_group_late_captures group) arms))
        get_references (ANFIR.Expr'Seq _ _ a b) = [a, b]
        get_references (ANFIR.Expr'TupleDestructure1 _ _ tup) = [tup]
        get_references (ANFIR.Expr'TupleDestructure2 _ _ tup) = [tup]
        get_references (ANFIR.Expr'Forall _ _ _ group e) = (ANFIR.binding_group_immediate_captures group <> exclude_if_part_of_group group e, ANFIR.binding_group_late_captures group)
        get_references (ANFIR.Expr'TypeApply _ _ e _) = [e]
        get_references (ANFIR.Expr'Poison _ _ _) = ([], [])

        is_outward k =
            let ANFIR.Binding (ANFIR.BoundWhere def_bg) _ = Arena.get binding_arena k
            in def_bg /= unique -- is not defined in this current binding group; usually identifiers cannot refer to bindings defined in an inner binding group so this should be fine

annotate_binding :: Arena.Arena (ANFIR.Binding CaptureList ty poison_allowed) ANFIR.BindingKey -> ANFIR.Binding () ty poison_allowed -> ANFIR.Binding CaptureList ty poison_allowed
annotate_binding binding_arena (ANFIR.Binding bv initializer) = ANFIR.Binding bv (annotate_expr binding_arena initializer)

annotate_expr :: Arena.Arena (ANFIR.Binding CaptureList ty poison_allowed) ANFIR.BindingKey -> ANFIR.Expr () ty poison_allowed -> ANFIR.Expr CaptureList ty poison_allowed
annotate_expr _ (ANFIR.Expr'Refer id ty i) = ANFIR.Expr'Refer id ty i
annotate_expr _ (ANFIR.Expr'Char id ty c) = ANFIR.Expr'Char id ty c
annotate_expr _ (ANFIR.Expr'String id ty s) = ANFIR.Expr'String id ty s
annotate_expr _ (ANFIR.Expr'Int id ty i) = ANFIR.Expr'Int id ty i
annotate_expr _ (ANFIR.Expr'Float id ty r) = ANFIR.Expr'Float id ty r
annotate_expr _ (ANFIR.Expr'Bool id ty b) = ANFIR.Expr'Bool id ty b
annotate_expr _ (ANFIR.Expr'Tuple id ty a b) = ANFIR.Expr'Tuple id ty a b
annotate_expr binding_arena (ANFIR.Expr'Lambda id ty param group body) = ANFIR.Expr'Lambda id ty param (annotate_binding_group binding_arena group) body
annotate_expr _ (ANFIR.Expr'Param id ty param) = ANFIR.Expr'Param id ty param
annotate_expr _ (ANFIR.Expr'Call id ty callee arg) = ANFIR.Expr'Call id ty callee arg
annotate_expr binding_arena (ANFIR.Expr'Switch id ty test arms) = ANFIR.Expr'Switch id ty test (map (\ (p, group, e) -> (p, annotate_binding_group binding_arena group, e)) arms)
annotate_expr _ (ANFIR.Expr'Seq id ty a b) = ANFIR.Expr'Seq id ty a b
annotate_expr _ (ANFIR.Expr'TupleDestructure1 id ty tup) = ANFIR.Expr'TupleDestructure1 id ty tup
annotate_expr _ (ANFIR.Expr'TupleDestructure2 id ty tup) = ANFIR.Expr'TupleDestructure2 id ty tup
annotate_expr binding_arena (ANFIR.Expr'Forall id ty vars group e) = ANFIR.Expr'Forall id ty vars (annotate_binding_group binding_arena group) e
annotate_expr _ (ANFIR.Expr'TypeApply id ty e arg) = ANFIR.Expr'TypeApply id ty e arg
annotate_expr _ (ANFIR.Expr'MakeADT id ty variant args) = ANFIR.Expr'MakeADT id ty variant args
annotate_expr _ (ANFIR.Expr'Poison id ty allowed) = ANFIR.Expr'Poison id ty allowed
