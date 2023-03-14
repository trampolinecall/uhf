{-# LANGUAGE OverloadedLists #-}

module UHF.Phases.Middle.AnnotateCaptures (annotate) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Set as Set

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

type BoundValue = RIR.BoundValue (Maybe (Type.Type Void))

type CaptureList = Set BoundValueKey -- TODO: dont use BoundValueKey Ord for order of captures in ts backend arguments

annotate :: RIR.RIR () -> RIR.RIR CaptureList
annotate (RIR.RIR decls adts type_synonyms bvs mod) = RIR.RIR (Arena.transform (annotate_decl bvs) decls) adts type_synonyms bvs mod

annotate_decl :: Arena.Arena BoundValue BoundValueKey -> RIR.Decl () -> RIR.Decl CaptureList
annotate_decl bvs (RIR.Decl'Module bindings adts type_synonyms) = RIR.Decl'Module (map (annotate_binding bvs) bindings) adts type_synonyms
annotate_decl _ (RIR.Decl'Type ty) = RIR.Decl'Type ty

annotate_binding :: Arena.Arena BoundValue BoundValueKey -> RIR.Binding () -> RIR.Binding CaptureList
annotate_binding bvs (RIR.Binding bv initializer) = RIR.Binding bv (annotate_expr bvs initializer)

annotate_expr :: Arena.Arena BoundValue BoundValueKey -> RIR.Expr () -> RIR.Expr CaptureList
annotate_expr _ (RIR.Expr'Identifier id ty sp i) = RIR.Expr'Identifier id ty sp i
annotate_expr _ (RIR.Expr'Char id ty sp c) = RIR.Expr'Char id ty sp c
annotate_expr _ (RIR.Expr'String id ty sp s) = RIR.Expr'String id ty sp s
annotate_expr _ (RIR.Expr'Int id ty sp i) = RIR.Expr'Int id ty sp i
annotate_expr _ (RIR.Expr'Float id ty sp r) = RIR.Expr'Float id ty sp r
annotate_expr _ (RIR.Expr'Bool id ty sp b) = RIR.Expr'Bool id ty sp b
annotate_expr bvs (RIR.Expr'Tuple id ty sp a b) = RIR.Expr'Tuple id ty sp (annotate_expr bvs a) (annotate_expr bvs b)
annotate_expr bvs (RIR.Expr'Lambda id ty sp uniq () param body) =
    let body' = annotate_expr bvs body
    in RIR.Expr'Lambda id ty sp uniq (get_captures body') param body'
    where
        get_captures :: RIR.Expr CaptureList -> Set BoundValueKey
        get_captures (RIR.Expr'Identifier _ _ _ (Just i))
            | is_capture i = [i]
            | otherwise = []
        get_captures (RIR.Expr'Identifier _ _ _ Nothing) = []
        get_captures (RIR.Expr'Char _ _ _ _) = []
        get_captures (RIR.Expr'String _ _ _ _) = []
        get_captures (RIR.Expr'Int _ _ _ _) = []
        get_captures (RIR.Expr'Float _ _ _ _) = []
        get_captures (RIR.Expr'Bool _ _ _ _) = []
        get_captures (RIR.Expr'Tuple _ _ _ a b) = get_captures a <> get_captures b
        get_captures (RIR.Expr'Lambda _ _ _ _ captures _ _) = Set.filter is_capture captures
        get_captures (RIR.Expr'Let _ _ _ bindings result) = Set.unions (map (\ (RIR.Binding _ init) -> get_captures init) bindings) <> get_captures result
        get_captures (RIR.Expr'Call _ _ _ callee arg) = get_captures callee <> get_captures arg
        get_captures (RIR.Expr'Switch _ _ _ test arms) = get_captures test <> Set.unions (map (\ (_, e) -> get_captures e) arms)
        get_captures (RIR.Expr'Seq _ _ _ a b) = get_captures a <> get_captures b
        get_captures (RIR.Expr'Poison _ _ _) = []

        is_capture k = case Arena.get bvs k of
            RIR.BoundValue _ _ RIR.InModule _ -> False
            RIR.BoundValue _ _ (RIR.InLambdaBody def_l) _ -> def_l /= uniq -- is not defined in this current lambda

annotate_expr bvs (RIR.Expr'Let id ty sp bindings result) = RIR.Expr'Let id ty sp (map (annotate_binding bvs) bindings) (annotate_expr bvs result)
annotate_expr bvs (RIR.Expr'Call id ty sp callee arg) = RIR.Expr'Call id ty sp (annotate_expr bvs callee) (annotate_expr bvs arg)
annotate_expr bvs (RIR.Expr'Switch id ty sp test arms) = RIR.Expr'Switch id ty sp (annotate_expr bvs test) (map (\ (p, e) -> (p, annotate_expr bvs e)) arms)
annotate_expr bvs (RIR.Expr'Seq id ty sp a b) = RIR.Expr'Seq id ty sp (annotate_expr bvs a) (annotate_expr bvs b)
annotate_expr _ (RIR.Expr'Poison id ty sp) = RIR.Expr'Poison id ty sp
