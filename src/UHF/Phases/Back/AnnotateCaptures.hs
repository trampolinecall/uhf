{-# LANGUAGE OverloadedLists #-}

module UHF.Phases.Back.AnnotateCaptures (annotate) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified UHF.Data.IR.BackendIR as BackendIR

type CaptureList = Set BackendIR.BindingKey -- TODO: dont use BindingKey Ord for order of captures in ts backend arguments
type DependencyList = Set BackendIR.BindingKey

annotate :: BackendIR.BackendIR () () () ty poison_allowed -> BackendIR.BackendIR BackendIR.BoundWhere CaptureList DependencyList ty poison_allowed
annotate (BackendIR.BackendIR adts type_synonyms type_vars bindings params cu) =
    let bindings' = assign_bound_wheres cu bindings
        bindings'' = Arena.transform (annotate_binding bindings'') bindings'
    in BackendIR.BackendIR adts type_synonyms type_vars bindings'' params (annotate_cu bindings'' cu)
-- loops are not possible because an expression cannot have its parent binding group as a child binding group
-- so the annotation of an expression cannot depend on itself because any binding groups that it has cannot contain itself

assign_bound_wheres :: BackendIR.CU () -> Arena.Arena (BackendIR.Binding () () () ty poison_allowed) BackendIR.BindingKey -> Arena.Arena (BackendIR.Binding BackendIR.BoundWhere () () ty poison_allowed) BackendIR.BindingKey
assign_bound_wheres cu bindings =
    let bw_map = execWriter $
            let (BackendIR.CU module_binding_group _ _) = cu
            in process_group module_binding_group >>
            Arena.transformM
                ((\case
                    BackendIR.Expr'Lambda _ _ _ group _ -> process_group group
                    BackendIR.Expr'Switch _ _ _ arms -> mapM_ (\ (_, group, _) -> process_group group) arms
                    BackendIR.Expr'Forall _ _ _ group _ -> process_group group

                    BackendIR.Expr'Refer _ _ _ -> pure ()
                    BackendIR.Expr'Int _ _ _ -> pure ()
                    BackendIR.Expr'Float _ _ _ -> pure ()
                    BackendIR.Expr'Bool _ _ _ -> pure ()
                    BackendIR.Expr'Char _ _ _ -> pure ()
                    BackendIR.Expr'String _ _ _ -> pure ()
                    BackendIR.Expr'Tuple _ _ _ _  -> pure ()
                    BackendIR.Expr'MakeADT _ _ _ _ -> pure ()
                    BackendIR.Expr'Param _ _ _ -> pure ()
                    BackendIR.Expr'Call _ _ _ _ -> pure ()
                    BackendIR.Expr'Seq _ _ _ _ -> pure ()
                    BackendIR.Expr'TupleDestructure1 _ _ _  -> pure ()
                    BackendIR.Expr'TupleDestructure2 _ _ _ -> pure ()
                    BackendIR.Expr'TypeApply _ _ _ _ -> pure ()
                    BackendIR.Expr'Poison _ _ _ -> pure ()
                ) . BackendIR.binding_initializer)
                bindings
    in Arena.transform_with_key
        (\ bk binding ->
            binding { BackendIR.binding_bound_where = (bw_map Map.! bk) }
        )
        bindings
    where
        tell_bw bk bw = tell $ Map.singleton bk bw

        process_group (BackendIR.BindingGroup unique _ bindings) = mapM_ (\ bk -> tell_bw bk (BackendIR.BoundWhere unique)) bindings

annotate_cu :: Arena.Arena (BackendIR.Binding BackendIR.BoundWhere CaptureList DependencyList ty poison_allowed) BackendIR.BindingKey -> BackendIR.CU () -> BackendIR.CU CaptureList
annotate_cu binding_arena (BackendIR.CU bindings adts type_synonyms) = BackendIR.CU (annotate_binding_group binding_arena bindings) adts type_synonyms

annotate_binding_group :: Arena.Arena (BackendIR.Binding BackendIR.BoundWhere CaptureList DependencyList ty poison_allowed) BackendIR.BindingKey -> BackendIR.BindingGroup () -> BackendIR.BindingGroup CaptureList
annotate_binding_group binding_arena (BackendIR.BindingGroup unique () bindings) =
    let captures = Set.unions $ map get_outward_references bindings
    in BackendIR.BindingGroup unique captures bindings
    where
        get_outward_references = Set.filter is_outward . BackendIR.binding_dependencies . Arena.get binding_arena

        is_outward k =
            let BackendIR.Binding (BackendIR.BoundWhere def_bg) _ _ = Arena.get binding_arena k
            in def_bg /= unique -- is not defined in this current binding group; inward dependencies are not included in binding dependencies so this should be fine

exclude_if_in_group :: Arena.Arena (BackendIR.Binding BackendIR.BoundWhere captures dependencies ty poison_allowed) BackendIR.BindingKey -> BackendIR.BindingGroup captures -> BackendIR.BindingKey -> Set BackendIR.BindingKey
exclude_if_in_group binding_arena (BackendIR.BindingGroup unique _ _) binding =
    let (BackendIR.Binding (BackendIR.BoundWhere def_bg) _ _) = Arena.get binding_arena binding
    in if def_bg == unique then [] else [binding]

annotate_binding :: Arena.Arena (BackendIR.Binding BackendIR.BoundWhere CaptureList DependencyList ty poison_allowed) BackendIR.BindingKey -> BackendIR.Binding BackendIR.BoundWhere () () ty poison_allowed -> BackendIR.Binding BackendIR.BoundWhere CaptureList DependencyList ty poison_allowed
annotate_binding binding_arena (BackendIR.Binding bv () initializer) =
    let (dependencies, initializer') = annotate_expr binding_arena initializer
    in BackendIR.Binding bv dependencies initializer' -- these exclude inward dependencies: e.g. in the lambda '\ (x) -> let y = 0; y' the y is not counted as a dependency

annotate_expr :: Arena.Arena (BackendIR.Binding BackendIR.BoundWhere CaptureList DependencyList ty poison_allowed) BackendIR.BindingKey -> BackendIR.Expr () ty poison_allowed -> (DependencyList, BackendIR.Expr CaptureList ty poison_allowed)
annotate_expr _ (BackendIR.Expr'Refer id ty i) = ([i], BackendIR.Expr'Refer id ty i)
annotate_expr _ (BackendIR.Expr'Char id ty c) = ([], BackendIR.Expr'Char id ty c)
annotate_expr _ (BackendIR.Expr'String id ty s) = ([], BackendIR.Expr'String id ty s)
annotate_expr _ (BackendIR.Expr'Int id ty i) = ([], BackendIR.Expr'Int id ty i)
annotate_expr _ (BackendIR.Expr'Float id ty r) = ([], BackendIR.Expr'Float id ty r)
annotate_expr _ (BackendIR.Expr'Bool id ty b) = ([], BackendIR.Expr'Bool id ty b)
annotate_expr _ (BackendIR.Expr'Tuple id ty a b) = ([a, b], BackendIR.Expr'Tuple id ty a b)
annotate_expr binding_arena (BackendIR.Expr'Lambda id ty param group result) =
    let group' = annotate_binding_group binding_arena group
    in (BackendIR.binding_group_captures group' <> exclude_if_in_group binding_arena group' result, BackendIR.Expr'Lambda id ty param group' result)
annotate_expr _ (BackendIR.Expr'Param id ty param) = ([], BackendIR.Expr'Param id ty param)
annotate_expr _ (BackendIR.Expr'Call id ty callee arg) = ([callee, arg], BackendIR.Expr'Call id ty callee arg)
annotate_expr binding_arena (BackendIR.Expr'Switch id ty test arms) =
    let arms' = map (\ (p, group, e) -> (p, annotate_binding_group binding_arena group, e)) arms
    in
        ( [test] <> Set.unions (map (\ (_, g, res) -> BackendIR.binding_group_captures g <> exclude_if_in_group binding_arena g res) arms')
        , BackendIR.Expr'Switch id ty test arms'
        )
annotate_expr _ (BackendIR.Expr'Seq id ty a b) = ([a, b], BackendIR.Expr'Seq id ty a b)
annotate_expr _ (BackendIR.Expr'TupleDestructure1 id ty tup) = ([tup], BackendIR.Expr'TupleDestructure1 id ty tup)
annotate_expr _ (BackendIR.Expr'TupleDestructure2 id ty tup) = ([tup], BackendIR.Expr'TupleDestructure2 id ty tup)
annotate_expr binding_arena (BackendIR.Expr'Forall id ty vars group e) =
    let group' =annotate_binding_group binding_arena group
    in (BackendIR.binding_group_captures group' <> exclude_if_in_group binding_arena group' e, BackendIR.Expr'Forall id ty vars group' e)
annotate_expr _ (BackendIR.Expr'TypeApply id ty e arg) = ([e], BackendIR.Expr'TypeApply id ty e arg)
annotate_expr _ (BackendIR.Expr'MakeADT id ty variant args) = (Set.fromList args, BackendIR.Expr'MakeADT id ty variant args)
annotate_expr _ (BackendIR.Expr'Poison id ty allowed) = ([], BackendIR.Expr'Poison id ty allowed)
