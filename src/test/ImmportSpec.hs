--------------------------------------------------------------------------------
--- Test whether decision tree is replicating Immport cell definitions

{-# LANGUAGE OverloadedStrings #-}


module ImmportSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.Semigroup ((<>))

import ManualTree
import Membrane
import ImmportDefinitions


immport_table :: Map.Map B.ByteString Row
immport_table = Map.fromList [ (r_class row, row) | row <- immport_rows]


classify_label :: B.ByteString -> B.ByteString
classify_label label = snd . fst $ results
  where
    prefix = "http://purl.obolibrary.org/obo/"
    label' = prefix <> label
    results = case Map.lookup label' immport_table of
                Nothing -> (("INVALID ID", "NO ONTOLOGY ID"), empty_vars)
                Just row -> cd3_switch (r_features row) empty_vars


spec :: Spec
spec = describe "Immport cell marker tests" $ do
  it "classify CL_0000084" $ do
    classify_label "CL_0000084" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000084"

  it "classify CL_0000624" $ do
    classify_label "CL_0000624" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000624"


  it "classify CL_0000792" $ do
    classify_label "CL_0000792" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000792"

  -- CCR4+ not in Maeker et al.
  it "classify CL_0001046" $ do
    classify_label "CL_0001046" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001046"
    

  it "classify CL_0001045" $ do
    classify_label "CL_0001045" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001045"
    

  it "classify CL_0001048" $ do
    classify_label "CL_0001048" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001048"
    

  it "classify CL_0000895" $ do
    classify_label "CL_0000895" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000895"
    

  it "classify CL_0000904" $ do
    classify_label "CL_0000904" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000904"
    

  it "classify CL_0001044" $ do
    classify_label "CL_0001044" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001044"
    

  it "classify CL_0000905" $ do
    classify_label "CL_0000905" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000905"
    

  it "classify CL_0000900" $ do
    classify_label "CL_0000900" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000900"
    

  it "classify CL_0000907" $ do
    classify_label "CL_0000907" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000907"
    

  it "classify CL_0001050" $ do
    classify_label "CL_0001050" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001050"
    

  it "classify CL_0000913" $ do
    classify_label "CL_0000913" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000913"
    

  it "classify CL_0001049" $ do
    classify_label "CL_0001049" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001049"
    
-- cd33 and NK T cells are not in Maecker et al.
  it "classify CL_0000814" $ do
    classify_label "CL_0000814" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000814"
  

  it "classify CL_0000625" $ do
    classify_label "CL_0000625" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000625"
    

  it "classify CL_0000236" $ do
    classify_label "CL_0000236" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000236"
    

  it "classify CL_0000818" $ do
    classify_label "CL_0000818" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000818"
    

  it "classify CL_0000787" $ do
    classify_label "CL_0000787" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000787"
    

  it "classify CL_0000970" $ do
    classify_label "CL_0000970" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000970"
    

  it "classify CL_0001053" $ do
    classify_label "CL_0001053" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001053"
    

  it "classify CL_0000788" $ do
    classify_label "CL_0000788" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000788"
    

  it "classify CL_0000980" $ do
    classify_label "CL_0000980" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000980"
    

  it "classify CL_0000576" $ do
    classify_label "CL_0000576" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000576"
    

  it "classify CL_0002397" $ do
    classify_label "CL_0002397" `shouldBe` "http://purl.obolibrary.org/obo/CL_0002397"
    

  it "classify CL_0002057" $ do
    classify_label "CL_0002057" `shouldBe` "http://purl.obolibrary.org/obo/CL_0002057"
    

  it "classify CL_0000451" $ do
    classify_label "CL_0000451" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000451"
    

  it "classify CL_0000782" $ do
    classify_label "CL_0000782" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000782"
    

  it "classify CL_0000784" $ do
    classify_label "CL_0000784" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000784"
    

  it "classify CL_0000939" $ do
    classify_label "CL_0000939" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000939"
    

  it "classify CL_0000938" $ do
    classify_label "CL_0000938" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000938"
    

  it "classify CL_0000623" $ do
    classify_label "CL_0000623" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000623"
    

  it "classify CL_0000545" $ do
    classify_label "CL_0000545" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000545"


  it "classify CL_0000899" $ do
    classify_label "CL_0000899" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000899"
    
-- DOUBLE-CHECK not specifically a TH2 type so un-recognized, todo
  it "classify CL_0000546" $ do
    classify_label "CL_0000546" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000546"
    
-- Not in Maecker et al.
  it "classify CL_0000917" $ do
    classify_label "CL_0000917" `shouldBe` "http://purl.obolibrary.org/obo/CL_0000917"
    
-- Not in Maecker et al.
  it "classify CL_0002128" $ do
    classify_label "CL_0002128" `shouldBe` "http://purl.obolibrary.org/obo/CL_0002128"
    
-- Not in Maecker et al.
  it "classify CL_0001052" $ do
    classify_label "CL_0001052" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001052"


  it "classify CL_0001043" $ do
    classify_label "CL_0001043" `shouldBe` "http://purl.obolibrary.org/obo/CL_0001043"
