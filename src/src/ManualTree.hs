-- Copyright 2021 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- Encode manual decision tree here based partially Maecker et al. 2012  to map to ImmPort cell definitions 
{-# LANGUAGE OverloadedStrings #-}


module ManualTree where


--import LoadCellInfo

import Data.ByteString.UTF8 (toString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Vector as V

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

import Data.Maybe (fromJust, isJust)
import Data.List (sortBy)

import Control.Exception

--------------------------------------------------------------------------------

import Membrane


data CellTreeError = MissingMarker B.ByteString | MarkerStatusUnknown B.ByteString deriving Show
instance Exception CellTreeError



type CellLabel = (B.ByteString, B.ByteString)
type DecisionVariables = S.Set B.ByteString


-- from cell ontology exact synoynms
cd3_marker = "CD3e" -- "http://purl.obolibrary.org/obo/PR_000001020"
cd4_marker = "CD4" -- "http://purl.obolibrary.org/obo/PR_000001004"
cd8_marker = "CD8A" -- http://purl.obolibrary.org/obo/PR_000001084,T-cell surface glycoprotein CD8 alpha chain
ccr4_marker = "CCR4" -- http://purl.obolibrary.org/obo/PR_000001200
cd25_marker = "CD25" -- http://purl.obolibrary.org/obo/PR_000001380

cd127_marker = "CD127" -- http://purl.obolibrary.org/obo/PR_000001869
ccr7_marker = "CCR7" -- http://purl.obolibrary.org/obo/PR_000001203
cd45_marker = "CD45RA" -- http://purl.obolibrary.org/obo/PR_000001015
cd38_marker = "CD38" -- http://purl.obolibrary.org/obo/PR_000001408
cd19_marker = "CD19" -- http://purl.obolibrary.org/obo/PR_000001002

cd27_marker = "CD27" -- http://purl.obolibrary.org/obo/PR_000001963
cd24_marker = "CD24" -- http://purl.obolibrary.org/obo/PR_000001932
cd14_marker = "CD14" -- http://purl.obolibrary.org/obo/PR_000001889
cd20_marker = "CD20" -- http://purl.obolibrary.org/obo/PR_000001289
cd16_marker = "CD16" -- http://purl.obolibrary.org/obo/PR_000001483

hla_marker = "HLA-DRA" -- http://purl.obolibrary.org/obo/PR_000002015
cd56_marker = "CD56" -- http://purl.obolibrary.org/obo/PR_000001024
cd11_marker = "CD11c"-- http://purl.obolibrary.org/obo/PR_000001013
cd123_marker = "CD123" -- http://purl.obolibrary.org/obo/PR_000001865
igd_marker = "IgD" -- http://purl.obolibrary.org/obo/GO_0071738


cd45ro_marker = "PTPRC/iso:CD45RO" -- http://purl.obolibrary.org/obo/PR_000001017
cxcr3_marker = "CXCR3" -- http://purl.obolibrary.org/obo/PR_000001207
ccr6_marker = "CCR6" -- http://purl.obolibrary.org/obo/PR_000001202

cd33_marker = "CD33" -- http://purl.obolibrary.org/obo/PR_000001892 -- this may not be needed, only shows up in Immport.Upload.Templates.Description.pdf

most_markers = [ cd3_marker, cd4_marker, cd8_marker, ccr4_marker, cd25_marker
               , cd127_marker, ccr7_marker, cd45_marker, cd38_marker, cd19_marker
               , cd27_marker, cd24_marker, cd14_marker, cd20_marker, cd16_marker
               , hla_marker, cd56_marker, cd11_marker, cd123_marker, igd_marker
               , cd45ro_marker
               ]

all_markers = most_markers ++ [cxcr3_marker, ccr6_marker] ++ [cd33_marker]



{- variables to print cell names in Maecker et al. style with Ontology IRI
-}
cl_cell :: CellLabel
cl_cell = ("cell", "http://purl.obolibrary.org/obo/CL_0000000")

cl_tcell :: CellLabel
cl_tcell =  ("T cell", "http://purl.obolibrary.org/obo/CL_0000084")

cl_cd4_tcell :: CellLabel
cl_cd4_tcell = ("CD4+ T cell", "http://purl.obolibrary.org/obo/CL_0000624")

{- activated T-cell sub types do not yet have enough experimental evidence and are not supported by the cell ontology
cl_acm_cd4_tcell :: CellLabel
cl_acm_cd4_tcell = ("activated central memory CD4+ T cell", "TODO acm cd4")

cl_an_cd4_tcell :: CellLabel
cl_an_cd4_tcell = ("activated naive CD4+ T cell", "TODO activate cd4")

cl_aem_cd4_tcell :: CellLabel
cl_aem_cd4_tcell = ("activated effector memory CD4+ T cell", "activated effector memory")

cl_ae_cd4_tcell :: CellLabel
cl_ae_cd4_tcell = ("activated effector CD4+ T cell", "activated effector")

cl_acm_cd8_tcell :: CellLabel
cl_acm_cd8_tcell = ("activated central memory CD8+ T cell", "TODO")

cl_am_cd8_tcell :: CellLabel
cl_am_cd8_tcell = ("activated naive memory CD8+ T cell", "TODO")

cl_ae_cd8_tcell :: CellLabel
cl_ae_cd8_tcell = ("activated effector CD8+ T cell", "TODO ae cd8")

cl_aem_cd8_tcell :: CellLabel
cl_aem_cd8_tcell = ("activated effector memory CD8+ T cell", "TODO aem cd8")

-}

cl_naive_cd4_tcell :: CellLabel
cl_naive_cd4_tcell = ("naive CD4+ T cell", "http://purl.obolibrary.org/obo/CL_0000895")

cl_memory_cd4_tcell :: CellLabel
cl_memory_cd4_tcell = ("central memory CD4+ T cell", "http://purl.obolibrary.org/obo/CL_0000904")


cl_effector_cd4_tcell :: CellLabel
cl_effector_cd4_tcell = ("effector CD4+ T cell", "http://purl.obolibrary.org/obo/CL_0001044")

cl_activated_cd4_tcell :: CellLabel
cl_activated_cd4_tcell = ("activated CD4+ T cell", "http://purl.obolibrary.org/obo/CL_0001043")


cl_em_cd4_tcell :: CellLabel
cl_em_cd4_tcell = ("effector memory CD4+ T cell", "http://purl.obolibrary.org/obo/CL_0000905")

--cl_cd8_tcell :: CellLabel
--cl_cd8_tcell = ("CD8+ T cell", "http://purl.obolibrary.org/obo/CL_0000900") -- is naive CD8+ .. same as just CD8+ T cell?

cl_cd8_tcell :: CellLabel
cl_cd8_tcell = ("CD8+ T cell", "http://purl.obolibrary.org/obo/CL_0000625") -- is naive CD8+ .. same as just CD8+ T cell?

cl_naive_cd8_tcell :: CellLabel
cl_naive_cd8_tcell = ("naive CD8+ T cell", "http://purl.obolibrary.org/obo/CL_0000900")





cl_cm_cd8_tcell :: CellLabel
cl_cm_cd8_tcell = ("central memory CD8+ T cell", "http://purl.obolibrary.org/obo/CL_0000907")

cl_activated_cd8_tcell :: CellLabel
cl_activated_cd8_tcell = ("activated CD8+ T cell", "http://purl.obolibrary.org/obo/CL_0001049")

cl_effector_cd8_tcell :: CellLabel
cl_effector_cd8_tcell = ("effector CD8+ T cell", "http://purl.obolibrary.org/obo/CL_0001050")

cl_effector_memory_cd8_tcell :: CellLabel
cl_effector_memory_cd8_tcell = ("effector memory CD8+ T cell", "http://purl.obolibrary.org/obo/CL_0000913")



--cl_memory_treg = ("memory Treg cell", "http://purl.obolibrary.org/obo/CL_0002678")
cl_memory_treg :: CellLabel
cl_memory_treg = ("memory CCR4+ Treg cell", "http://purl.obolibrary.org/obo/CL_0001046")


cl_naive_treg :: CellLabel
cl_naive_treg = ("naive CCR4+ Treg cell", "http://purl.obolibrary.org/obo/CL_0001045")

cl_am_treg :: CellLabel
cl_am_treg = ("activated memory Treg cell", "http://purl.obolibrary.org/obo/CL_0001047")

cl_an_treg :: CellLabel
cl_an_treg = ("activated naive Treg cell", "http://purl.obolibrary.org/obo/CL_0001048") -- ontology does not call this naive

cl_non_tcell :: CellLabel
cl_non_tcell = ("B cell or DC or monocyte or NK cell", "No specific ontology ID (B,DC,monocyte,NK)")

cl_treg :: CellLabel
cl_treg = ("Treg cell", "http://purl.obolibrary.org/obo/CL_0001047")

cl_bcell :: CellLabel
cl_bcell = ("B cell", "http://purl.obolibrary.org/obo/CL_0000236")

cl_naive_bcell :: CellLabel
cl_naive_bcell = ("naive B cell", "http://purl.obolibrary.org/obo/CL_0000788")

cl_transitional_bcell :: CellLabel
cl_transitional_bcell = ("transitional B cell", "http://purl.obolibrary.org/obo/CL_0000818")

cl_plasmablast :: CellLabel
cl_plasmablast = ("plasmablast", "http://purl.obolibrary.org/obo/CL_0000980")

cl_igd_minus_bcell :: CellLabel
cl_igd_minus_bcell = ("IgD- memory B cell", "http://purl.obolibrary.org/obo/CL_0001053")

cl_igd_plus_bcell :: CellLabel
cl_igd_plus_bcell = ("IgD+ memory B cell", "http://purl.obolibrary.org/obo/CL_0000970")

cl_memory_bcell :: CellLabel
cl_memory_bcell = ("memory B cell", "http://purl.obolibrary.org/obo/CL_0000787")

cl_dc_nk_monocyte :: CellLabel
cl_dc_nk_monocyte = ("DC or monocyte or NK cell", "No specific ontology ID (DC, monocyte, NK)")

cl_classical_monocyte :: CellLabel
cl_classical_monocyte = ("classical monocyte", "http://purl.obolibrary.org/obo/CL_0002057")

cl_cd16plus_monocyte :: CellLabel
cl_cd16plus_monocyte = ("non-classical CD16+ monocyte", "http://purl.obolibrary.org/obo/CL_0002397")

cl_monocyte :: CellLabel
cl_monocyte = ("monocyte", "http://purl.obolibrary.org/obo/CL_0000576")

cl_myeloid_dc :: CellLabel
cl_myeloid_dc = ("myeloid DC", "http://purl.obolibrary.org/obo/CL_0000782")

cl_plasmacytoid_dc :: CellLabel
cl_plasmacytoid_dc = ("plasmacytoid DC", "http://purl.obolibrary.org/obo/CL_0000784")

cl_dendritic_cell :: CellLabel
cl_dendritic_cell = ("dendritic cell", "http://purl.obolibrary.org/obo/CL_0000451")

cl_dc_nk_cell :: CellLabel
cl_dc_nk_cell = ("DC or NK cell", "No specific ontology ID (DC, NK)")

cl_cd16_minus_nk_cell :: CellLabel
cl_cd16_minus_nk_cell = ("CD16- NK cell", "http://purl.obolibrary.org/obo/CL_0000938") -- closest match in ontology, although tree does not test CD56++

cl_cd16_plus_nk_cell :: CellLabel
cl_cd16_plus_nk_cell = ("CD16+ NK cell", "http://purl.obolibrary.org/obo/CL_0000939")

cl_nk_cell :: CellLabel
cl_nk_cell = ("NK cell", "http://purl.obolibrary.org/obo/CL_0000623")

cl_helper1 :: CellLabel
cl_helper1 = ("Th1 cell", "http://purl.obolibrary.org/obo/CL_0000545")

cl_helper2 :: CellLabel
cl_helper2 = ("Th2 cell", "TODO")

cl_helper17 :: CellLabel
cl_helper17 = ("Th17 cell", "http://purl.obolibrary.org/obo/CL_0000899")


-- Support for ImmPort-specific template, when possible
-- Immport's CL_0000623 looks underspecified, "http://purl.obolibrary.org/obo/CL_0000623"
--cl_todo = ("??", "http://purl.obolibrary.org/obo/CL_0000546")

cl_immport_th2 :: CellLabel
cl_immport_th2 = ("T-helper 2 cell", "http://purl.obolibrary.org/obo/CL_0000546")

cl_immport_tc1 :: CellLabel
cl_immport_tc1 = ("Tc1 cell", "http://purl.obolibrary.org/obo/CL_0000917")

cl_immport_tc17 :: CellLabel
cl_immport_tc17 = ("Tc17 cell", "http://purl.obolibrary.org/obo/CL_0002128")

cl_immport_tc2 :: CellLabel
cl_immport_tc2 = ("Tc2 cell", "http://purl.obolibrary.org/obo/CL_0001052")

cl_immport_nk_t_cell :: CellLabel
cl_immport_nk_t_cell = ("mature NK T cell", "http://purl.obolibrary.org/obo/CL_0000814")

cl_immport_treg :: CellLabel
cl_immport_treg = ("CD4-positive, CD25+, alpha-beta regulatory T cell", "http://purl.obolibrary.org/obo/CL_0000792")

--------------------------------------------------------------------------------

empty_vars :: DecisionVariables
empty_vars = S.empty

isPlus :: MembraneStatus -> Bool
isPlus Unknown = False
isPlus Lacking = False
isPlus _ = True


(+<) :: DecisionVariables -> B.ByteString -> DecisionVariables
prev_vars +< x = S.union prev_vars (S.fromList [x])
infixl 5 +<


classify_markers :: MarkerProfile -> (CellLabel, DecisionVariables)
classify_markers profile = cd3_switch profile empty_vars
 
cd3_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
cd3_switch profile prev_vars = case (HM.lookup cd3_marker profile) of
                     Just Unknown -> throw $ MarkerStatusUnknown cd3_marker
                     Just Lacking -> cd19_switch profile vars'
                     Just _ -> case (cd14, cd56, cd33) of
                                 (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd14_marker
                                 (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd14_marker
                                 (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd14_marker
                                 (Just Lacking, Just x, Just Lacking) | (isPlus x)  -> (cl_immport_nk_t_cell, vars'')
                                 _ -> tcell_switch profile vars'
                     Nothing -> (cl_cell, prev_vars)
  where
    vars' = S.union prev_vars (S.fromList [cd3_marker])
    cd14 = HM.lookup cd14_marker profile
    cd56 = HM.lookup cd56_marker profile
    cd33 = HM.lookup cd33_marker profile
    vars'' = S.union vars' (S.fromList [cd14_marker, cd56_marker, cd33_marker])



-- so what if cd4 is missing but cd8 is not?
tcell_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
tcell_switch profile prev_vars = case (cd4, cd8) of
                     (Just Unknown, _) -> throw $ MarkerStatusUnknown cd4_marker
                     (_, Just Unknown) -> throw $ MarkerStatusUnknown cd8_marker
                     (Just x, Just Lacking) | (isPlus x) -> cd4_switch profile $ prev_vars +< cd4_marker
                     (Just x, Nothing) | (isPlus x)      -> cd4_switch profile $ prev_vars +< cd4_marker
                     (Just Lacking, Just x) | (isPlus x) -> cd8_switch profile $ prev_vars +< cd8_marker
                     (Nothing, Just x) | (isPlus x)      -> cd8_switch profile $ prev_vars +< cd8_marker
                     (Just Lacking, Just Lacking)        -> (cl_tcell, prev_vars)
                     _                                   -> (cl_tcell, prev_vars)


  where
    cd4 = HM.lookup cd4_marker profile
    cd8 = HM.lookup cd8_marker profile
    vars' = S.union prev_vars (S.fromList [cd4_marker, cd8_marker])

{-
  Use Helper T-cells classifications if CXCR3 and CCR6 are observed
-}
cd4_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
cd4_switch profile prev_vars = case (ccr4, cd25, cd127) of
                     (Just Unknown, _, _) -> throw $ MarkerStatusUnknown ccr4_marker
                     (_, Just Unknown, _) -> throw $ MarkerStatusUnknown cd25_marker
                     (_, _, Just Unknown) -> throw $ MarkerStatusUnknown cd127_marker
                     (Nothing, Just x, Just LowAmount) | (isPlus x) -> (cl_immport_treg, prev_vars +< cd25_marker +< cd127_marker)
                     (Just x, Just y, Just LowAmount) | (isPlus x) && (isPlus y) -> treg_switch profile vars'
                     _ -> case (cxcr3, ccr6) of
                                                   (Just x, Just y) | (isPlus x) && (isPlus y) -> ccr7_switch profile prev_vars
                                                   (Just x, Just y) -> helper_cd4_switch profile vars''
                                                   (Just Lacking, _) -> (cl_immport_th2, prev_vars +< cxcr3_marker)
                                                   _ -> ccr7_switch profile prev_vars

                     -- (_, Nothing, _) -> cl_cd4_tcell -- ccr7_switch profile vars' -- throw $ MissingMarker cd25_marker
                     -- (_, _, Nothing) -> cl_cd4_tcell -- ccr7_switch profile vars' -- throw $ MissingMarker cd127_marker


   where
     ccr4 = HM.lookup ccr4_marker profile
     cd25 = HM.lookup cd25_marker profile
     cd127 = HM.lookup cd127_marker profile
     cxcr3 = HM.lookup cxcr3_marker profile
     ccr6 = HM.lookup ccr6_marker profile
     vars' = S.union prev_vars (S.fromList [ccr4_marker, cd25_marker, cd127_marker])
     vars'' = S.union prev_vars (S.fromList [cxcr3_marker, ccr6_marker])


ccr7_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
ccr7_switch profile prev_vars = case (cd38, hla_dr) of
                                  (Just x, Just y) | (isPlus x) && (isPlus y) -> (cl_activated_cd4_tcell, vars_activated_tcell)
                                  _  -> case (ccr7, has_submarkers) of
                                          (Just Unknown, _)     -> throw $ MarkerStatusUnknown ccr7_marker
                                          (Just Lacking, True)  -> effector_cd4_switch profile vars'
                                          (Just _, True)        -> memory_cd4_switch profile vars'
                                          _ ->  (cl_cd4_tcell, prev_vars)


                                  

  where
    ccr7 = HM.lookup ccr7_marker profile
    vars' = S.union prev_vars (S.fromList [ccr7_marker])
    has_submarkers = isJust $ HM.lookup cd45_marker profile
    cd38 = HM.lookup cd38_marker profile
    hla_dr = HM.lookup hla_marker profile
    vars_activated_tcell = S.union prev_vars (S.fromList [cd38_marker, hla_marker])


memory_cd4_switch :: MarkerProfile -> DecisionVariables ->(CellLabel, DecisionVariables)
memory_cd4_switch profile prev_vars = case (cd45, cd38, hla_dr) of
                          (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd45_marker
                          (_, Just Unknown, _) -> throw $ MarkerStatusUnknown cd38_marker
                          (_, _, Just Unknown) -> throw $ MarkerStatusUnknown hla_marker
                          (Just Lacking, Just x, Just y) | (isPlus x) && (isPlus y)  -> (cl_activated_cd4_tcell, vars')
                          (Just x, Just y, Just z) | (isPlus x) && (isPlus y) && (isPlus z)  -> (cl_activated_cd4_tcell, vars')
                          (Just Lacking, _, _) -> (cl_memory_cd4_tcell, prev_vars +< cd45_marker)
                          (Just x, _, _) | isPlus x -> (cl_naive_cd4_tcell, prev_vars +< cd45_marker)
                          (Nothing, _, _) -> (cl_cd4_tcell, prev_vars)
                          (_, Nothing, _) -> (cl_cd4_tcell, prev_vars)
                          (_, _, Nothing) -> (cl_cd4_tcell, prev_vars)

  where
    cd45 = HM.lookup cd45_marker profile
    cd38 = HM.lookup cd38_marker profile
    hla_dr = HM.lookup hla_marker profile
    --vars' = S.union prev_vars (S.fromList [cd45_marker, cd38_marker, hla_marker])
    vars' = S.union prev_vars (S.fromList [cd38_marker, hla_marker])

effector_cd4_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
effector_cd4_switch profile prev_vars = case (cd45, cd38, hla_dr) of
                          (_, Just Unknown, _) -> throw $ MarkerStatusUnknown cd38_marker
                          (_, _, Just Unknown) -> throw $ MarkerStatusUnknown hla_marker
                          (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd45_marker
                          (Just Lacking, Just x, Just y) | (isPlus x) && (isPlus y)  -> (cl_activated_cd4_tcell, vars')
                          (Just Lacking, _, _) -> (cl_em_cd4_tcell, prev_vars +< cd45_marker)
                          (Just x, Just y, Just z) | (isPlus x) && (isPlus y) && (isPlus z)  -> (cl_activated_cd4_tcell, prev_vars +< cd45_marker +< cd38_marker +< hla_marker)
                          (Just x, _, _) | (isPlus x) -> (cl_effector_cd4_tcell, prev_vars +< cd45_marker)
                          (Nothing, _, _) -> (cl_cd4_tcell, prev_vars)
                          (_, Nothing, _) -> (cl_cd4_tcell, prev_vars)
                          (_, _, Nothing) -> (cl_cd4_tcell, prev_vars)



  where
    cd45 = HM.lookup cd45_marker profile
    cd38 = HM.lookup cd38_marker profile
    hla_dr = HM.lookup hla_marker profile
    vars' = S.union prev_vars (S.fromList [cd45_marker, cd38_marker, hla_marker])


cd8_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
cd8_switch profile prev_vars = case (ccr7, has_submarkers, cxcr3, ccr6) of
                     (Just Unknown, _, _, _)            -> throw $ MarkerStatusUnknown ccr7_marker
                     (_, _, Just x, _) | isPlus x       -> (cl_immport_tc1, prev_vars +< cxcr3_marker)
                     (_, _, _, Just x) | isPlus x       -> (cl_immport_tc17, prev_vars +< ccr6_marker)
                     (_, _, Just Lacking, Just Lacking) -> (cl_immport_tc2, prev_vars +< cxcr3_marker +< ccr6_marker)
                     _                                  -> cd8_ccr7_switch profile prev_vars

  where
    ccr7 = HM.lookup ccr7_marker profile
    cd38 = HM.lookup cd38_marker profile
    hla_dr = HM.lookup hla_marker profile
    cxcr3 = HM.lookup cxcr3_marker profile
    ccr6 = HM.lookup ccr6_marker profile    
    has_submarkers = isJust $ HM.lookup cd45_marker profile


cd8_ccr7_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
cd8_ccr7_switch profile prev_vars =  case (cd38, hla_dr) of
                                       (Just x, Just y) | (isPlus x) && (isPlus y) -> (cl_activated_cd8_tcell, vars_activated_tcell)
                                       _ -> case (ccr7, has_submarkers) of
                                              (Just Lacking, True) -> effector_cd8_switch profile vars'
                                              (Just _, True)       -> memory_cd8_switch profile vars'
                                              _                    -> (cl_cd8_tcell, prev_vars)
  where
    ccr7 = HM.lookup ccr7_marker profile
    cd38 = HM.lookup cd38_marker profile
    hla_dr = HM.lookup hla_marker profile
    vars' = S.union prev_vars (S.fromList [ccr7_marker])
    vars_activated_tcell = S.union prev_vars (S.fromList [cd38_marker, hla_marker])
    has_submarkers = isJust $ HM.lookup cd45_marker profile



memory_cd8_switch :: MarkerProfile -> DecisionVariables ->(CellLabel, DecisionVariables)
memory_cd8_switch profile prev_vars = case (cd45, cd38, hla_dr) of
                          (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd45_marker
                          (_, Just Unknown, _) -> throw $ MarkerStatusUnknown cd38_marker
                          (_, _, Just Unknown) -> throw $ MarkerStatusUnknown hla_marker
                          (Just Lacking, Just x, Just y) | (isPlus x) && (isPlus y)  -> (cl_activated_cd8_tcell, vars')
                          (Just Lacking, _, _) -> (cl_cm_cd8_tcell, prev_vars +< cd45_marker)
                          (Just x, Just y, Just z) | (isPlus x) && (isPlus y) && (isPlus z)  -> (cl_activated_cd8_tcell, vars')
                          (Just x, _, _) | (isPlus x) -> (cl_naive_cd8_tcell, prev_vars +< cd45_marker)
                          (Nothing, _, _) -> (cl_cd8_tcell, prev_vars)
                          (_, Nothing, _) -> (cl_cd8_tcell, prev_vars)
                          (_, _, Nothing) -> (cl_cd8_tcell, prev_vars)

  where
    cd45 = HM.lookup cd45_marker profile
    cd38 = HM.lookup cd38_marker profile
    hla_dr = HM.lookup hla_marker profile
    vars' = S.union prev_vars (S.fromList [cd45_marker, cd38_marker, hla_marker])



effector_cd8_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
effector_cd8_switch profile prev_vars = case (cd45, cd38, hla_dr) of
                          (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd45_marker
                          (_, Just Unknown, _) -> throw $ MarkerStatusUnknown cd38_marker
                          (_, _, Just Unknown) -> throw $ MarkerStatusUnknown hla_marker
                          (Just Lacking, Just x, Just y) | (isPlus x) && (isPlus y)  -> (cl_activated_cd8_tcell, vars')
                          (Just Lacking, _, _) -> (cl_effector_memory_cd8_tcell, prev_vars +< cd45_marker)
                          (Just x, Just y, Just z) | (isPlus x) && (isPlus y) && (isPlus z) -> (cl_activated_cd8_tcell, vars')
                          (Just x, _, _) | isPlus x -> (cl_effector_cd8_tcell, prev_vars +< cd45_marker)
                          (Nothing, _, _) -> (cl_cd8_tcell, prev_vars)
                          (_, Nothing, _) -> (cl_cd8_tcell, prev_vars)
                          (_, _, Nothing) -> (cl_cd8_tcell, prev_vars)
                          (_, _, _) -> (cl_cd8_tcell, prev_vars)

  where
    cd45 = HM.lookup cd45_marker profile
    cd38 = HM.lookup cd38_marker profile
    hla_dr = HM.lookup hla_marker profile
    vars' = S.union prev_vars (S.fromList [cd45_marker, cd38_marker, hla_marker])




treg_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
treg_switch profile prev_vars = case (cd45ro, hla_dr) of
                          (Just Unknown, _) -> throw $ MarkerStatusUnknown cd45ro_marker
                          (_, Just Unknown) -> throw $ MarkerStatusUnknown hla_marker
                          (_, Just x) | isPlus x -> (cl_an_treg, prev_vars +< hla_marker)
                          (Just Lacking, _) -> (cl_naive_treg, prev_vars +< cd45ro_marker)
                          (Just x, _) | isPlus x -> (cl_memory_treg, prev_vars +< cd45ro_marker)
                          (_, _) -> (cl_treg, prev_vars)

  where
    cd45ro = HM.lookup cd45ro_marker profile
    hla_dr = HM.lookup hla_marker profile
    vars' = S.union prev_vars (S.fromList [cd45ro_marker, hla_marker])

{-  What about T Reg cells?

-}


{-  What about TH1 TH2, TH17 cells?

-}

cd19_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
cd19_switch profile prev_vars = case (cd19, cd56) of
                     (Just Unknown, _)      -> throw $ MarkerStatusUnknown cd19_marker
                     (_, Just Unknown)      -> throw $ MarkerStatusUnknown cd56_marker
                     (_, Just x) | isPlus x -> nk_switch profile (prev_vars +< cd56_marker)
                     (Just Lacking, _)      -> monocytes_switch profile vars'
                     (Just _, _)            -> bcell_switch profile vars'
                     _                      -> (cl_non_tcell, prev_vars)

                                
  where
    cd56 = HM.lookup cd56_marker profile
    cd19 = HM.lookup cd19_marker profile
    vars' = S.union prev_vars (S.fromList [cd19_marker])


bcell_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
bcell_switch profile prev_vars = case (cd27, cd24, cd38) of
                    (Just Unknown, _, _) -> throw $ MarkerStatusUnknown cd27_marker
                    (_, Just Unknown, _) -> throw $ MarkerStatusUnknown cd24_marker
                    (_, _, Just Unknown) -> throw $ MarkerStatusUnknown cd38_marker
                    (_, Just HighAmount, Just HighAmount) -> (cl_transitional_bcell, prev_vars +< cd24_marker +< cd38_marker)
                    (Just Lacking, _, _) -> (cl_naive_bcell, prev_vars +< cd27_marker)
                    (Just x, _, _) | isPlus x -> igd_switch profile $ prev_vars +< cd27_marker
                    (Nothing, _, _) -> (cl_bcell, prev_vars)
                    (_, Nothing, _) -> (cl_bcell, prev_vars)
                    (_, _, Nothing) -> (cl_bcell, prev_vars)
  where
    cd27 = HM.lookup cd27_marker profile
    cd24 = HM.lookup cd24_marker profile
    cd38 = HM.lookup cd38_marker profile
    vars' = S.union prev_vars (S.fromList [cd27_marker, cd24_marker, cd38_marker])




igd_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
igd_switch profile prev_vars = case (igd, cd20, cd38) of
                    (Just Unknown, _, _) -> throw $ MarkerStatusUnknown igd_marker
                    (_, Just Unknown, _) -> throw $ MarkerStatusUnknown cd20_marker
                    (_, _, Just Unknown) -> throw $ MarkerStatusUnknown cd38_marker
                    (_, Just Lacking, Just x) | isPlus x -> (cl_plasmablast, prev_vars +< cd20_marker +< cd38_marker)
                    (Just Lacking, _, _) -> (cl_igd_minus_bcell, prev_vars +< igd_marker)
                    (Just x, _, _) | isPlus x -> (cl_igd_plus_bcell, prev_vars +< igd_marker)
                    (Nothing, _, _) -> (cl_memory_bcell, prev_vars)
                    (_, Nothing, _) -> (cl_memory_bcell, prev_vars)
                    (_, _, Nothing) -> (cl_memory_bcell, prev_vars)
                    (_, _, _) -> (cl_memory_bcell, prev_vars)
  where
    igd = HM.lookup igd_marker profile
    cd20 = HM.lookup cd20_marker profile
    cd38 = HM.lookup cd38_marker profile
    vars' = S.union prev_vars (S.fromList [igd_marker, cd20_marker, cd38_marker])


monocytes_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
monocytes_switch profile prev_vars = case (cd14, cd20) of
                          (Just Unknown, _) -> throw $ MarkerStatusUnknown cd14_marker
                          (_, Just Unknown) -> throw $ MarkerStatusUnknown cd20_marker
                          (Just Lacking, Just Lacking) -> nk_cell_switch profile vars'
                          (Just x, _) | isPlus x -> cd16_switch profile vars'
                          (Nothing, _) -> (cl_dc_nk_monocyte, prev_vars)
                          (_, Nothing) -> (cl_dc_nk_monocyte, prev_vars)
                          _ -> (cl_dc_nk_monocyte, prev_vars)

  where
    cd14 = HM.lookup cd14_marker profile
    cd20 = HM.lookup cd20_marker profile
    vars' = S.union prev_vars (S.fromList [cd14_marker, cd20_marker])

cd16_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
cd16_switch profile prev_vars = case cd16 of
                     Just Unknown -> throw $ MarkerStatusUnknown cd16_marker
                     Just Lacking -> (cl_classical_monocyte, vars')
                     Just _ -> (cl_cd16plus_monocyte, vars')
                     Nothing -> (cl_monocyte, prev_vars)

  where
    cd16 = HM.lookup cd16_marker profile
    vars' = S.union prev_vars (S.fromList [cd16_marker])


nk_cell_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
nk_cell_switch profile prev_vars = case (hla_dr, cd56) of
                          (Just Unknown, _) -> throw $ MarkerStatusUnknown hla_marker
                          (_, Just Unknown) -> throw $ MarkerStatusUnknown cd56_marker
                          (Just x, _) | isPlus x -> dc_switch profile vars_hla
                          (_, Just x) | isPlus x -> nk_switch profile vars_cd56
                          (Nothing, _) -> (cl_dc_nk_cell, prev_vars)
                          (_, Nothing) -> (cl_dc_nk_cell, prev_vars)
                          _ -> (cl_dc_nk_cell, prev_vars)

  where
    hla_dr = HM.lookup hla_marker profile
    cd56 = HM.lookup cd56_marker profile
    vars_hla = S.union prev_vars (S.fromList [hla_marker])
    vars_cd56 = S.union prev_vars (S.fromList [cd56_marker])


dc_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
dc_switch profile prev_vars = case (cd11, cd123) of
                          (Just Unknown, _) -> throw $ MarkerStatusUnknown cd11_marker
                          (_, Just Unknown) -> throw $ MarkerStatusUnknown cd123_marker
                          (Just x, _) | isPlus x -> (cl_myeloid_dc, vars_cd11)
                          (_, Just x) | isPlus x -> (cl_plasmacytoid_dc, vars_cd123)
                          (Nothing, _) -> (cl_dendritic_cell, prev_vars)
                          (_, Nothing) -> (cl_dendritic_cell, prev_vars)
                          _ -> (cl_dendritic_cell, prev_vars)

  where
    cd11 = HM.lookup cd11_marker profile
    cd123 = HM.lookup cd123_marker profile
    vars_cd11 = S.union prev_vars (S.fromList [cd11_marker])
    vars_cd123 = S.union prev_vars (S.fromList [cd123_marker])


nk_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
nk_switch profile prev_vars = case cd16 of

                     Just Unknown -> throw $ MarkerStatusUnknown cd16_marker
                     Just Lacking -> (cl_cd16_minus_nk_cell, vars')
                     Just x | isPlus x -> (cl_cd16_plus_nk_cell, vars')
                     _ -> (cl_nk_cell, prev_vars)

  where
    cd16 = HM.lookup cd16_marker profile
    vars' = S.union prev_vars (S.fromList [cd16_marker])


helper_cd4_switch :: MarkerProfile -> DecisionVariables -> (CellLabel, DecisionVariables)
helper_cd4_switch profile prev_vars = case (cxcr3, ccr6) of
                     (Just Unknown, _) -> throw $ MarkerStatusUnknown cxcr3_marker
                     (_, Just Unknown) -> throw $ MarkerStatusUnknown ccr6_marker
                     (Just Lacking, Just Lacking) -> (cl_helper2, vars')
                     (Just Lacking, Just _) -> (cl_helper17, vars')
                     (Just _, Just Lacking) -> (cl_helper1, vars')
                     (Nothing, _) -> ccr7_switch profile prev_vars
                     (_, Nothing) -> ccr7_switch profile prev_vars
                     _ -> ccr7_switch profile prev_vars


   where
     cxcr3 = HM.lookup cxcr3_marker profile
     ccr6 = HM.lookup ccr6_marker profile
     vars' = S.union prev_vars (S.fromList [cxcr3_marker, ccr6_marker])



extended_markers :: MarkerProfile -> (CellLabel, DecisionVariables) -> [B.ByteString]
extended_markers profile ((label, ontology_id), vars) = remaining_markers
  where
    markers = HM.toList profile
    remaining = [ (marker_label, intensity) | (marker_label, intensity) <- markers, S.notMember marker_label vars]
    initial_text = label <> " (" <> ontology_id <> ")"
    remaining_markers = case length remaining of
                          0 -> []
                          _ -> map (\(m,i) -> m <> to_intensity_label i) remaining
    
