-- Copyright 2021 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- Most of the interface logic is to live here, so that Main.hs can be a thinner layer

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module MappingInterface where



import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C

import qualified Data.Vector as V

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

import Data.Maybe (fromJust)
import Data.List (sortBy, subsequences)


import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.UTF8 (toString)

import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S


import LoadCellInfo
import ManualTree
import Membrane
import ImmportDefinitions
import ParseMarkers



--------------------------------------------------------------------------------



data MappingResults = ErrorMessage String | MappingResults CellLabel DecisionVariables [B.ByteString]
  deriving (Show)


instance ToJSON MappingResults where
    toJSON (ErrorMessage message) = object ["error_message" .= message]
    toJSON (MappingResults (human_label, ontology_id) vars extra_markers) = object [ "label" .= human_label
                                                                    , "ontology_id" .= ontology_id
                                                                    , "extra_markers" .= extra_markers -- extended_markers profile classification_results
                                                                    , "decision_variables" .= vars ]

{-
instance ToJSON DecisionVariables where
    toJSON vars = toJSON (S.toList vars)
-}

instance ToJSON C.ByteString where
  toJSON s = toJSON (toString s)



generate_results_obj :: String -> MappingResults
generate_results_obj text = case (read_marker_profile text) of
                         Left msg -> ErrorMessage msg
                         Right profile -> let classification_results@(labels, vars_used) = classify_markers profile
                                              extra_signal = extended_markers profile classification_results
                                          in MappingResults labels vars_used extra_signal


extended_classification_text :: MappingResults -> String
extended_classification_text (MappingResults (label, ontology_id) _ extra_markers) = toString formatted
  where
    initial_text = label <> " (" <> ontology_id <> ")"
    remaining_marker_text = case length  extra_markers of
                              0 -> ""
                              _ -> " with " <> B.intercalate ", " extra_markers
    formatted = initial_text <> remaining_marker_text


print_human_friendly_results :: MappingResults -> IO ()
print_human_friendly_results (ErrorMessage msg) =  putStrLn msg
print_human_friendly_results x@(MappingResults (label, ontology_id) vars_used extra_markers) = do
   putStrLn $ "Cell classification: " ++ extended_classification_text x
   putStrLn $ "Decision variables used: " ++ show (S.toList vars_used)


print_json_text :: MappingResults -> IO ()
print_json_text results = CL.putStrLn $ encode results
