--------------------------------------------------------------------------------
--- Test whether decision tree is correctly reporting decision variables by checking for obvious excess decision variables

-- Immport cell definitions

{-# LANGUAGE OverloadedStrings #-}


module DecisionVariablesSpec where

import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.Semigroup ((<>))

import ManualTree
import Membrane
import ImmportDefinitions


immport_table :: Map.Map B.ByteString Row
immport_table = Map.fromList [ (r_class row, row) | row <- immport_rows]


subset_marker_profile :: MarkerProfile -> DecisionVariables -> MarkerProfile
subset_marker_profile profile vars = profile'
  where
    profile' = HM.fromList [ (marker, intensity) | (marker, intensity) <- HM.toList profile, S.member marker vars]
    


excess_decision_variables :: B.ByteString -> S.Set B.ByteString
excess_decision_variables label = excess_variables
  where
    prefix = "http://purl.obolibrary.org/obo/"
    label' = prefix <> label
    (profile, found_variables) = case Map.lookup label' immport_table of
                                   Nothing -> (HM.fromList [], empty_vars)
                                   Just row -> (r_features row, snd (cd3_switch (r_features row) empty_vars))
    expected_vars = S.fromList [ marker | (marker, _) <- HM.toList profile]
    excess_variables = S.fromList [ v | v <- S.toList found_variables, S.notMember v expected_vars]


spec :: Spec
spec = describe "Immport cell marker tests for decision variables" $ do
  it "check for consistent decision variables CL_0000084" $ do
    excess_decision_variables "CL_0000084" `shouldBe` S.empty

  it "check for consistent decision variables CL_0000624" $ do
    excess_decision_variables "CL_0000624" `shouldBe` S.empty

-- Immport is missing CCR4 
  it "check for consistent decision variables CL_0000792" $ do
    excess_decision_variables "CL_0000792" `shouldBe` S.empty

  -- TODO it may not find the right one, but probably still should not excess decision variables
  -- CCR4+ not in Maeker et al.
  it "check for consistent decision variables CL_0001046" $ do
    excess_decision_variables "CL_0001046" `shouldBe` S.empty


  it "check for consistent decision variables CL_0001045" $ do
    excess_decision_variables "CL_0001045" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0001048" $ do
    excess_decision_variables "CL_0001048" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000895" $ do
    excess_decision_variables "CL_0000895" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000904" $ do
    excess_decision_variables "CL_0000904" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0001044" $ do
    excess_decision_variables "CL_0001044" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000905" $ do
    excess_decision_variables "CL_0000905" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000900" $ do
    excess_decision_variables "CL_0000900" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000907" $ do
    excess_decision_variables "CL_0000907" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0001050" $ do
    excess_decision_variables "CL_0001050" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000913" $ do
    excess_decision_variables "CL_0000913" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0001049" $ do
    excess_decision_variables "CL_0001049" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000814" $ do
    excess_decision_variables "CL_0000814" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000625" $ do
    excess_decision_variables "CL_0000625" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000236" $ do
    excess_decision_variables "CL_0000236" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000818" $ do
    excess_decision_variables "CL_0000818" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000787" $ do
    excess_decision_variables "CL_0000787" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000970" $ do
    excess_decision_variables "CL_0000970" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0001053" $ do
    excess_decision_variables "CL_0001053" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000788" $ do
    excess_decision_variables "CL_0000788" `shouldBe` S.empty
    
-- TODO investigate
  it "check for consistent decision variables CL_0000980" $ do
    excess_decision_variables "CL_0000980" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000576" $ do
    excess_decision_variables "CL_0000576" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0002397" $ do
    excess_decision_variables "CL_0002397" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0002057" $ do
    excess_decision_variables "CL_0002057" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000451" $ do
    excess_decision_variables "CL_0000451" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000782" $ do
    excess_decision_variables "CL_0000782" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000784" $ do
    excess_decision_variables "CL_0000784" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000939" $ do
    excess_decision_variables "CL_0000939" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0000938" $ do
    excess_decision_variables "CL_0000938" `shouldBe` S.empty
    
-- missing CD19
  it "check for consistent decision variables CL_0000623" $ do
    excess_decision_variables "CL_0000623" `shouldBe` S.empty
    
-- TODO investigate Immport is missing CCR6- in its markers, or maybe I missed it
  it "check for consistent decision variables CL_0000545" $ do
    excess_decision_variables "CL_0000545" `shouldBe` S.empty

-- DOUBLE-CHECK Immport is missing CXCR3-- in its markers, or maybe I missed it
  it "check for consistent decision variables CL_0000899" $ do
    excess_decision_variables "CL_0000899" `shouldBe` S.empty
    
-- DOUBLE-CHECK not specifically a TH2 type so un-recognized, todo
  it "check for consistent decision variables CL_0000546" $ do
    excess_decision_variables "CL_0000546" `shouldBe` S.empty
    
{- Not in Maecker et al.
  it "check for consistent decision variables CL_0000917" $ do
    excess_decision_variables "CL_0000917" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0002128" $ do
    excess_decision_variables "CL_0002128" `shouldBe` S.empty
    

  it "check for consistent decision variables CL_0001052" $ do
    excess_decision_variables "CL_0001052" `shouldBe` S.empty
-}
