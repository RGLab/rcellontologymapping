--------------------------------------------------------------------------------
--- Test whether there is a smaller set of decision variables that would classify to
--- the same classification

-- Immport cell definitions

{-# LANGUAGE OverloadedStrings #-}


module ExcessDecisionVariablesSpec where

import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.Semigroup ((<>))
import Data.Maybe (listToMaybe)
import Data.List (subsequences)

import ManualTree
import Membrane
import ImmportDefinitions


immport_table :: Map.Map B.ByteString Row
immport_table = Map.fromList [ (r_class row, row) | row <- immport_rows]


subset_marker_profile :: MarkerProfile -> DecisionVariables -> MarkerProfile
subset_marker_profile profile vars = profile'
  where
    profile' = HM.fromList [ (marker, intensity) | (marker, intensity) <- HM.toList profile, S.member marker vars]
    

--search to find a subset of the given decision variables that generates the same cell classification
classify_via_all_proper_subsets :: DecisionVariables -> MarkerProfile -> B.ByteString -> Maybe (DecisionVariables, B.ByteString)
classify_via_all_proper_subsets vars profile expected = results
  where
    n = S.size vars
    subsets = filter (\s -> S.size s < n) [ S.fromList items | items <- subsequences (S.toList vars)]
    rewrite :: (CellLabel, DecisionVariables) -> (DecisionVariables, B.ByteString)
    rewrite ((informal_label, ontology_id), found_vars) = (found_vars, ontology_id)
    
    filtering = filter (\(found_vars, oid) -> (oid == expected) && (found_vars /= vars) )
    classify = \d -> cd3_switch (subset_marker_profile profile d) empty_vars
    results = listToMaybe . filtering . (map (rewrite . classify)) $ subsets



excess_decision_variables2 :: B.ByteString -> (DecisionVariables, B.ByteString)
excess_decision_variables2 label = results
  where
    prefix = "http://purl.obolibrary.org/obo/"
    label' = prefix <> label
    results = case Map.lookup label' immport_table of
                Nothing -> (empty_vars, "")
                Just row -> let profile = r_features row
                                ((informal_label, ontology_id), found_vars) = cd3_switch profile empty_vars
                                preliminary = classify_via_all_proper_subsets found_vars profile ontology_id
                                in case preliminary of 
                                       Nothing -> (empty_vars, ontology_id)
                                       Just x -> x



spec :: Spec
spec = describe "Immport cell marker tests for minimal decision variables" $ do
  it "excess_decision_variables2 CL_0000084" $ do
    excess_decision_variables2 "CL_0000084" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000084")

  it "excess_decision_variables2 CL_0000624" $ do
    excess_decision_variables2 "CL_0000624" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000624")


  it "excess_decision_variables2 CL_0000792" $ do
    excess_decision_variables2 "CL_0000792" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000792")



  it "excess_decision_variables2 CL_0001046" $ do
    excess_decision_variables2 "CL_0001046" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001046")


  it "excess_decision_variables2 CL_0001045" $ do
    excess_decision_variables2 "CL_0001045" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001045")
    

  it "excess_decision_variables2 CL_0001048" $ do
    excess_decision_variables2 "CL_0001048" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001048")
    

  it "excess_decision_variables2 CL_0000895" $ do
    excess_decision_variables2 "CL_0000895" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000895")
    

  it "excess_decision_variables2 CL_0000904" $ do
    excess_decision_variables2 "CL_0000904" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000904")
    

  it "excess_decision_variables2 CL_0001044" $ do
    excess_decision_variables2 "CL_0001044" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001044")
    

  it "excess_decision_variables2 CL_0000905" $ do
    excess_decision_variables2 "CL_0000905" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000905")
    

  it "excess_decision_variables2 CL_0000900" $ do
    excess_decision_variables2 "CL_0000900" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000900")
    

  it "excess_decision_variables2 CL_0000907" $ do
    excess_decision_variables2 "CL_0000907" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000907")
    

  it "excess_decision_variables2 CL_0001050" $ do
    excess_decision_variables2 "CL_0001050" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001050")
    

  it "excess_decision_variables2 CL_0000913" $ do
    excess_decision_variables2 "CL_0000913" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000913")
    

  it "excess_decision_variables2 CL_0001049" $ do
    excess_decision_variables2 "CL_0001049" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001049")
    


  it "excess_decision_variables2 CL_0000814" $ do
    excess_decision_variables2 "CL_0000814" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000814")


  it "excess_decision_variables2 CL_0000625" $ do
    excess_decision_variables2 "CL_0000625" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000625")
    

  it "excess_decision_variables2 CL_0000236" $ do
    excess_decision_variables2 "CL_0000236" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000236")
    

  it "excess_decision_variables2 CL_0000818" $ do
    excess_decision_variables2 "CL_0000818" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000818")
    

  it "excess_decision_variables2 CL_0000787" $ do
    excess_decision_variables2 "CL_0000787" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000787")
    

  it "excess_decision_variables2 CL_0000970" $ do
    excess_decision_variables2 "CL_0000970" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000970")
    

  it "excess_decision_variables2 CL_0001053" $ do
    excess_decision_variables2 "CL_0001053" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001053")
    

  it "excess_decision_variables2 CL_0000788" $ do
    excess_decision_variables2 "CL_0000788" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000788")
    

  it "excess_decision_variables2 CL_0000980" $ do
    excess_decision_variables2 "CL_0000980" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000980")
    

  it "excess_decision_variables2 CL_0000576" $ do
    excess_decision_variables2 "CL_0000576" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000576")
    

  it "excess_decision_variables2 CL_0002397" $ do
    excess_decision_variables2 "CL_0002397" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0002397")
    

  it "excess_decision_variables2 CL_0002057" $ do
    excess_decision_variables2 "CL_0002057" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0002057")
    

  it "excess_decision_variables2 CL_0000451" $ do
    excess_decision_variables2 "CL_0000451" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000451")
    

  it "excess_decision_variables2 CL_0000782" $ do
    excess_decision_variables2 "CL_0000782" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000782")
    

  it "excess_decision_variables2 CL_0000784" $ do
    excess_decision_variables2 "CL_0000784" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000784")
    

  it "excess_decision_variables2 CL_0000939" $ do
    excess_decision_variables2 "CL_0000939" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000939")
    

  it "excess_decision_variables2 CL_0000938" $ do
    excess_decision_variables2 "CL_0000938" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000938")
    


  it "excess_decision_variables2 CL_0000623" $ do
    excess_decision_variables2 "CL_0000623" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000623")


  it "excess_decision_variables2 CL_0000545" $ do
    excess_decision_variables2 "CL_0000545" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000545")

  it "excess_decision_variables2 CL_0000899" $ do
    excess_decision_variables2 "CL_0000899" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000899")
    

  it "excess_decision_variables2 CL_0000546" $ do
    excess_decision_variables2 "CL_0000546" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000546")




  it "excess_decision_variables2 CL_0000917" $ do
    excess_decision_variables2 "CL_0000917" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0000917")
    

  it "excess_decision_variables2 CL_0002128" $ do
    excess_decision_variables2 "CL_0002128" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0002128")
    

  it "excess_decision_variables2 CL_0001052" $ do
    excess_decision_variables2 "CL_0001052" `shouldBe` (S.empty, "http://purl.obolibrary.org/obo/CL_0001052")

