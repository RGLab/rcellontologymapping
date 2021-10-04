-- Copyright 2021 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- ImmPort cell definitions from ImmPort.Upload.Templates.Description.pdf

{-# LANGUAGE OverloadedStrings #-}

module ImmportDefinitions where


import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.Tuple.Extra (fst3, snd3, thd3)

import ManualTree
import Membrane


-- with plus or lo markers?
cells :: [ (B.ByteString, [(B.ByteString, B.ByteString)]) ]
cells = [ ("http://purl.obolibrary.org/obo/CL_0000084", [ (cd3_marker,"+") ])
        , ("http://purl.obolibrary.org/obo/CL_0000624", [ (cd3_marker,"+"), (cd4_marker,"+") ])
        , ("http://purl.obolibrary.org/obo/CL_0000792", [ (cd3_marker,"+"), (cd4_marker,"+"), (cd127_marker,"+-"), (cd25_marker,"+") ]) -- lo 127
        , ("http://purl.obolibrary.org/obo/CL_0001046", [ (cd3_marker,"+"), (cd4_marker,"+"), (cd127_marker,"+-"), (cd25_marker,"+"), (ccr4_marker,"+")
                                                        , (cd45ro_marker,"+") ])
        , ("http://purl.obolibrary.org/obo/CL_0001045", [ (cd3_marker,"+"), (cd4_marker,"+")
                                                        , (cd127_marker,"+-"), (cd25_marker,"+")
                                                        , (ccr4_marker,"+"), (cd45ro_marker, "-")   ])

        -- they have a line about cd45ro - being the same as "http://purl.obolibrary.org/obo/CL_0001045"
        , ("http://purl.obolibrary.org/obo/CL_0001048", [ (cd3_marker,"+"), (cd4_marker,"+"), (cd127_marker,"+-"), (cd25_marker,"+"), (ccr4_marker,"+")
                                                        , (hla_marker,"+") ])
        , ("http://purl.obolibrary.org/obo/CL_0000895", [ (cd3_marker,"+"), (cd4_marker,"+"), (ccr7_marker,"+")
                                                        , (cd45_marker,"+"), (cd8_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000904", [ (cd3_marker,"+"), (cd4_marker,"+"), (ccr7_marker,"+")
                                                        , (cd8_marker, "-"), (cd45_marker, "-")
                                                        ])

        , ("http://purl.obolibrary.org/obo/CL_0001044", [ (cd3_marker,"+"), (cd4_marker,"+"), (cd45_marker,"+")
                                                        , (cd8_marker, "-"), (ccr7_marker, "-")  ])


        , ("http://purl.obolibrary.org/obo/CL_0000905", [ (cd3_marker,"+"), (cd4_marker,"+")
                                                        , (cd8_marker, "-"), (ccr7_marker, "-"), (cd45_marker, "-")  ])

        , ("http://purl.obolibrary.org/obo/CL_0001043", [ (cd3_marker,"+"), (cd4_marker,"+"), (cd38_marker,"+"), (hla_marker,"+")
                                                        , (cd8_marker, "-")   ])

        , ("http://purl.obolibrary.org/obo/CL_0000900", [ (cd3_marker, "+"), (cd8_marker,"+"), (ccr7_marker,"+"), (cd45_marker,"+")
                                                        , (cd4_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000907", [ (cd3_marker, "+"), (cd8_marker,"+"), (ccr7_marker,"+")
                                                        , (cd4_marker, "-"), (cd45_marker, "-")  ])

        , ("http://purl.obolibrary.org/obo/CL_0001050", [ (cd3_marker, "+"), (cd8_marker,"+"), (cd45_marker,"+")
                                                        , (cd4_marker, "-"), (ccr7_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000913", [ (cd3_marker, "+"), (cd8_marker,"+")
                                                        , (cd4_marker, "-"), (ccr7_marker, "-"), (cd45_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0001049", [ (cd3_marker, "+"), (cd8_marker,"+"), (cd38_marker,"+"), (hla_marker,"+")
                                                        , (cd4_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000814", [ (cd3_marker,"+"), (cd56_marker,"+")
                                                        , (cd14_marker, "-"), (cd33_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000625", [ (cd3_marker,"+"), (cd8_marker,"+") ])

        , ("http://purl.obolibrary.org/obo/CL_0000236", [ (cd19_marker,"+"), (cd20_marker,"+")
                                                        , (cd3_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000818", [ (cd19_marker,"+"), (cd20_marker,"+"), (cd24_marker,"++"), (cd38_marker,"++")
                                                        , (cd3_marker, "-") ]) --cd24++, cd38++

        , ("http://purl.obolibrary.org/obo/CL_0000787", [ (cd19_marker,"+"), (cd20_marker,"+"), (cd27_marker,"+")
                                                        , (cd3_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000970", [ (cd19_marker,"+"), (cd20_marker,"+"), (cd27_marker,"+"), (igd_marker,"+")
                                                        , (cd3_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0001053", [ (cd19_marker,"+"), (cd20_marker,"+"), (cd27_marker,"+")
                                                        , (cd3_marker, "-"), (igd_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000788", [ (cd19_marker,"+"), (cd20_marker,"+"), (igd_marker,"+")
                                                        , (cd3_marker, "-"), (cd27_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000980", [ (cd19_marker,"+"), (cd27_marker,"++"), (cd38_marker,"++")
                                                        , (cd3_marker, "-"), (cd20_marker,"-") ]) -- cd27++, cd38++

        , ("http://purl.obolibrary.org/obo/CL_0000576", [ (cd14_marker,"+")
                                                        , (cd3_marker, "-"), (cd19_marker, "-"), (cd20_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0002397", [ (cd14_marker,"+"), (cd16_marker,"+")
                                                        , (cd3_marker, "-"), (cd19_marker, "-"), (cd20_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0002057", [ (cd14_marker,"+")
                                                        , (cd3_marker, "-"), (cd19_marker, "-"), (cd20_marker, "-"), (cd16_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000451", [ (hla_marker,"+")
                                                        , (cd3_marker, "-"), (cd19_marker, "-"), (cd20_marker, "-"), (cd14_marker, "-"), (cd16_marker, "-"), (cd56_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000782", [ (hla_marker,"+"), (cd11_marker,"+")
                                                        , (cd3_marker, "-"), (cd19_marker,"-"), (cd20_marker, "-"), (cd14_marker, "-"), (cd16_marker, "-"), (cd56_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000784", [ (hla_marker,"+"), (cd123_marker,"+")
                                                        , (cd3_marker, "-"), (cd19_marker, "-"), (cd20_marker, "-"), (cd14_marker, "-")
                                                        , (cd16_marker, "-"), (cd56_marker, "-"), (cd11_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000939", [ (cd16_marker,"+"), (cd56_marker,"+")
                                                        , (cd3_marker, "-"), (cd19_marker, "-"), (cd20_marker, "-"), (cd14_marker, "-")
                                                        , (hla_marker, "-") ])

        , ("http://purl.obolibrary.org/obo/CL_0000938", [ (cd56_marker,"++")
                                                        , (cd3_marker, "-"), (cd19_marker, "-"), (cd20_marker, "-"), (cd14_marker, "-")
                                                        , (hla_marker, "-"), (cd16_marker, "-") ]) --cd56++

        , ("http://purl.obolibrary.org/obo/CL_0000623", [ (cd56_marker,"+"), (cd3_marker, "-") ])
        ]

helper_t_cells :: [ (B.ByteString, [(B.ByteString, B.ByteString)]) ]
helper_t_cells = [ ("http://purl.obolibrary.org/obo/CL_0000545", [ (cd3_marker,"+"), (cd4_marker,"+"), (cxcr3_marker, "+"),
                                                                   (ccr6_marker, "-"), (cd8_marker, "-") ])
                 , ("http://purl.obolibrary.org/obo/CL_0000899", [ (cd3_marker,"+"), (cd4_marker,"+"), (ccr6_marker, "+"),
                                                                   (cxcr3_marker, "-"), (cd8_marker, "-") ]) --Th17
                 , ("http://purl.obolibrary.org/obo/CL_0000546", [ (cd3_marker,"+"), (cd4_marker,"+"),
                                                                   (cd8_marker, "-"), (cxcr3_marker, "-") ]) -- Th2
                 , ("http://purl.obolibrary.org/obo/CL_0000917", [ (cd3_marker,"+"), (cd8_marker,"+"), (cxcr3_marker,"+") ]) -- not handled?
                 , ("http://purl.obolibrary.org/obo/CL_0002128", [ (cd3_marker,"+"), (cd8_marker,"+"), (ccr6_marker,"+") ]) -- not handled?
                 , ("http://purl.obolibrary.org/obo/CL_0001052", [ (cd3_marker,"+"), (cd4_marker,"-"), (cd8_marker,"+"), (cxcr3_marker, "-"),
                                                                   (ccr6_marker, "-")  ]) -- or http://purl.obolibrary.org/obo/CL_0000918
        ]


immport_labels :: [ (B.ByteString, B.ByteString, B.ByteString)]
immport_labels = [ ("B: B cell", "B cell", "http://purl.obolibrary.org/obo/CL_0000236")
                 , ("B: IgD+ memory B cell", "unswitched memory B cell", "http://purl.obolibrary.org/obo/CL_0000970")
                 , ("B: IgD- memory B cell", "IgD-negative memory B cell", "http://purl.obolibrary.org/obo/CL_0001053")
                 , ("B: memory B cell", "memory B cell", "http://purl.obolibrary.org/obo/CL_0000787")
                 , ("B: naive B cell", "naive B cell", "http://purl.obolibrary.org/obo/CL_0000788")
                 , ("B: plasmablast", "plasmablast", "http://purl.obolibrary.org/obo/CL_0000980")
                 , ("B: transitional B cell", "transitional stage B cell", "http://purl.obolibrary.org/obo/CL_0000818")
                 , ("DC: dendritic cell", "dendritic cell", "http://purl.obolibrary.org/obo/CL_0000451")
                 , ("DC: myeloid dendritic cell", "myeloid dendritic cell", "http://purl.obolibrary.org/obo/CL_0000782")
                 , ("DC: plasmacytoid dendritic cell", "plasmacytoid dendritic cell", "http://purl.obolibrary.org/obo/CL_0000784")
                 , ("M: CD16+ monocyte", "CD14-positive, CD16-positive monocyte", "http://purl.obolibrary.org/obo/CL_0002397")
                 , ("M: CD16- monocyte", "CD14-positive, CD16-negative classical monocyte", "http://purl.obolibrary.org/obo/CL_0002057")
                 , ("M: monocyte", "monocyte", "http://purl.obolibrary.org/obo/CL_0000576")
                 , ("NK: CD16+ CD56+ NK cell", "CD16-positive, CD56-dim natural killer cell", "http://purl.obolibrary.org/obo/CL_0000939")
                 , ("NK: CD16- CD56bright NK cell", "CD16-negative, CD56-bright natural killer cell", "http://purl.obolibrary.org/obo/CL_0000938")
                 , ("NK: NK cell", "natural killer cell", "http://purl.obolibrary.org/obo/CL_0000623")
                 , ("PBMC", "peripheral blood mononuclear cell", "http://purl.obolibrary.org/obo/CL_2000001")
                 , ("T: activated CCR4+ Treg", "activated CD4-positive, CD25-positive, CCR4-positive, alpha-beta regulator T cell, human", "http://purl.obolibrary.org/obo/CL_0001048")
                 , ("T: activated CD4+ T cell", "activated CD4-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0001043")
                 , ("T: activated CD8+ T cell", "activated CD8-positive, alpha-beta T cell, human", "http://purl.obolibrary.org/obo/CL_0001049")
                 , ("T: CCR4+ Treg", "naive CCR4-positive regulatory T cell", "http://purl.obolibrary.org/obo/CL_0001045")
                 , ("T: CD4+ T cell", "CD4-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000624")
                 , ("T: CD8+ T cell", "CD8-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000625")
                 , ("T: central memory CD4+ T cell", "central memory CD4-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000904")
                 , ("T: central memory CD8+ T cell", "central memory CD8-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000907")
                 , ("T: effector CD4+ T cell", "effector CD4-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0001044")
                 , ("T: effector CD8+ T cell", "effector CD8-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0001050")
                 , ("T: effector memory CD4+ T cell", "effector memory CD4-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000905")
                 , ("T: effector memory CD8+ T cell", "effector memory CD8-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000913")
                 , ("T: memory CCR4+ Treg", "memory CCR4-positive regulatory T cell", "http://purl.obolibrary.org/obo/CL_0001046")
                 , ("T: naive CCR4+ Treg", "naive CCR4-positive regulatory T cell", "http://purl.obolibrary.org/obo/CL_0001045")
                 , ("T: naive CD4+ T cell", "naive thymus-derived CD4-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000895")
                 , ("T: naive CD8+ T cell", "naive thymus-derived CD8-positive, alpha-beta T cell", "http://purl.obolibrary.org/obo/CL_0000900")
                 , ("T: NK T cell", "mature NK T cell", "http://purl.obolibrary.org/obo/CL_0000814")
                 , ("T: non-Tc1/Tc17 CD8+ T cell", "Tc2 cell", "http://purl.obolibrary.org/obo/CL_0001052") --  Also listed "http://purl.obolibrary.org/obo/CL_0000918"
                 , ("T: non-Th1/Th17 CD4+ T cell", "T-helper 2 cell", "http://purl.obolibrary.org/obo/CL_0000546")
                 , ("T: T cell", "T cell", "http://purl.obolibrary.org/obo/CL_0000084")
                 , ("T: Tc1 CD8+ T cell", "Tc1 cell", "http://purl.obolibrary.org/obo/CL_0000917")
                 , ("T: Tc17 CD8+ T cell", "Tc17 cell", "http://purl.obolibrary.org/obo/CL_0002128")
                 , ("T: Th1 CD4+ T cell", "T-helper 1 cell", "http://purl.obolibrary.org/obo/CL_0000545")
                 , ("T: Th17 CD4+ T cell", "T-helper 17 cell", "http://purl.obolibrary.org/obo/CL_0000899")
                 , ("T: Treg", "CD4-positive, CD25+, alpha-beta regulatory T cell", "http://purl.obolibrary.org/obo/CL_0000792")
                 ]




-- create marker status data structure where the unspecifed markers
-- that are in "markers" are set to Lacking by default, and also only
-- uses the markers in "markers"

create_row_from_immport :: [B.ByteString] -> [(B.ByteString,B.ByteString)] -> Row
create_row_from_immport markers defined_markers = row
  where
    specified = HM.fromList [ (m, intensity_status intensity) | (m, intensity) <- defined_markers]
    base = HM.fromList [(m, Lacking) | m <- markers]
    features = HM.union specified base
    row = Row { r_class = "unset", r_human_readable="unset", r_features = features}


-- treat the markers listed in the immport definitions as the only observed markers
simple_row_from_immport :: [(B.ByteString,B.ByteString)] -> Row
simple_row_from_immport defined_markers = row
  where
    specified = HM.fromList [ (m, intensity_status intensity) | (m, intensity) <- defined_markers]
    row = Row { r_class = "unset", r_human_readable="unset", r_features = specified}


ontology_to_immport_label = HM.fromList [ (ontology, name) | (name, desc, ontology) <- immport_labels ]



immport_basic_rows = [  (create_row_from_immport most_markers markers) { r_class = ontology, r_human_readable = fromJust . (HM.lookup ontology) $ ontology_to_immport_label}   | (ontology, markers) <- cells]
immport_helper_t_rows = [  (create_row_from_immport all_markers markers) { r_class = ontology, r_human_readable = fromJust . (HM.lookup ontology) $ ontology_to_immport_label}   | (ontology, markers) <- helper_t_cells]

refine_row_description :: B.ByteString -> Row -> Row
refine_row_description ontology row = row'
  where
    row' = row { r_class = ontology, r_human_readable = fromJust . (HM.lookup ontology) $ ontology_to_immport_label} 

--immport_rows = immport_basic_rows ++ immport_helper_t_rows
--immport_rows = immport_helper_t_rows
immport_rows = [ refine_row_description ontology $ simple_row_from_immport markers | (ontology, markers) <- cells ++ helper_t_cells ]

compare_classification :: IO ()
compare_classification = do
    let called_classes = map (fst . fst . (\x -> cd3_switch x empty_vars) . r_features) immport_rows
        comparison2 = zip immport_rows called_classes
    putStrLn "ontology ID|ImmPort name|called as"
    mapM_ show_as_columns3 comparison2
  where
    show2 :: (B.ByteString, B.ByteString) -> IO ()
    show2 t = do
        putStrLn $ "Comparison: " ++ (show . fst $ t) ++ ", " ++ (show . snd $ t)
    show3 :: (B.ByteString, B.ByteString, B.ByteString) -> IO ()
    show3 t = do
        putStrLn $ "Comparison: " ++ (show . fst3 $ t) ++ ", " ++ (show . snd3 $ t) ++ ", " ++ (show . thd3 $ t)
    show_columns3 t = do
        let text = B.intercalate "|" [ thd3 t, fst3 t, snd3 t]
        B.putStrLn text
    show_as_columns3 t = do
        let text_class = r_class . fst $ t
            text_label = r_human_readable . fst $ t
            text = B.intercalate "|" [ text_class, text_label, snd t]
        B.putStrLn text
