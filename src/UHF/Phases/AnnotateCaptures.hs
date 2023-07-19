{-# LANGUAGE OverloadedLists #-}

module UHF.Phases.AnnotateCaptures (annotate) where

import UHF.Util.Prelude

import qualified Data.Set as Set

import qualified UHF.Data.IR.RIR as RIR

type CaptureList = Set.Set RIR.BoundValueKey

annotate :: RIR.RIR () -> RIR.RIR CaptureList
annotate (RIR.RIR adts type_synonyms type_vars bound_values cu) = RIR.RIR adts type_synonyms type_vars bound_values (annotate_cu cu)

annotate_cu :: RIR.CU () -> RIR.CU CaptureList
annotate_cu (RIR.CU bindings adts type_synonyms) = RIR.CU (map annotate_binding bindings) adts type_synonyms

annotate_binding :: RIR.Binding () -> RIR.Binding CaptureList
annotate_binding = snd . annotate_binding'

annotate_binding' :: RIR.Binding () -> (CaptureList, RIR.Binding CaptureList)
annotate_binding' (RIR.Binding bvk e) =
    let (e_captures, e') = annotate_expr' e
    in (Set.delete bvk e_captures, RIR.Binding bvk e')

annotate_expr' :: RIR.Expr () -> (CaptureList, RIR.Expr CaptureList)
annotate_expr' (RIR.Expr'Identifier id ty sp mi@(Just i)) = ([i], RIR.Expr'Identifier id ty sp mi)
annotate_expr' (RIR.Expr'Identifier id ty sp Nothing) = ([], RIR.Expr'Identifier id ty sp Nothing)
annotate_expr' (RIR.Expr'Char id ty sp c) = ([], RIR.Expr'Char id ty sp c)
annotate_expr' (RIR.Expr'String id ty sp s) = ([], RIR.Expr'String id ty sp s)
annotate_expr' (RIR.Expr'Int id ty sp i) = ([], RIR.Expr'Int id ty sp i)
annotate_expr' (RIR.Expr'Float id ty sp f) = ([], RIR.Expr'Float id ty sp f)
annotate_expr' (RIR.Expr'Bool id ty sp b) = ([], RIR.Expr'Bool id ty sp b)
annotate_expr' (RIR.Expr'Tuple id ty sp a b) =
    let (a_captures, a') = annotate_expr' a
        (b_captures, b') = annotate_expr' b
    in (a_captures <> b_captures, RIR.Expr'Tuple id ty sp a' b')
annotate_expr' (RIR.Expr'Lambda id ty sp () param body) =
    let (body_captures, body') = annotate_expr' body
    in (body_captures, RIR.Expr'Lambda id ty sp body_captures param body')
annotate_expr' (RIR.Expr'Let id ty sp bindings result) =
    let (bindings_captures, bindings') = unzip $ map annotate_binding' bindings
        (result_captures, result') = annotate_expr' result

        defined_in_let = map (\ (RIR.Binding bvk _) -> bvk) bindings

    in ((Set.unions bindings_captures <> result_captures) `Set.difference` (Set.fromList defined_in_let), RIR.Expr'Let id ty sp bindings' result')
annotate_expr' (RIR.Expr'Call id ty sp callee result) =
    let (callee_captures, callee') = annotate_expr' callee
        (result_captures, result') = annotate_expr' result
    in (callee_captures <> result_captures, RIR.Expr'Call id ty sp callee' result')
annotate_expr' (RIR.Expr'Switch id ty sp scrutinee arms) =
    let (scrutinee_captures, scrutinee') = annotate_expr' scrutinee
        (arms_captures, arms') =
            unzip $
                map
                    (\ (matcher, res) ->
                        let (res_captures, res') = annotate_expr' res
                        in (res_captures, (matcher, res'))
                    )
                    arms

    in (scrutinee_captures <> Set.unions arms_captures, RIR.Expr'Switch id ty sp scrutinee' arms')
annotate_expr' (RIR.Expr'Forall id ty sp tys result) =
    let (result_captures, result') = annotate_expr' result
    in (result_captures, RIR.Expr'Forall id ty sp tys result')
annotate_expr' (RIR.Expr'TypeApply id ty sp e tyarg) =
    let (e_captures, e') = annotate_expr' e
    in (e_captures, RIR.Expr'TypeApply id ty sp e' tyarg)
annotate_expr' (RIR.Expr'MakeADT id ty sp var_idx tyargs args) =
    let (args_captures, args') = unzip $ map annotate_expr' args
    in (Set.unions args_captures, RIR.Expr'MakeADT id ty sp var_idx tyargs args')
annotate_expr' (RIR.Expr'Poison id ty sp) = ([], RIR.Expr'Poison id ty sp)
