--------------------------------------------------------------------------------
--- Using QuickCheck, test if the reported decision variables would
--- actually classify to the same cell classification for some random
--- profiles

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module RepeatableDecisionVariablesSpec where


import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Control.Monad (join)
import Data.Maybe (isNothing)


import ManualTree
import Membrane

import ExcessDecisionVariablesSpec (classify_via_all_proper_subsets)

--------------------------------------------------------------------------------

marker_name_gen :: Gen [B.ByteString]
marker_name_gen = sublistOf ManualTree.all_markers

intensity_gen :: Gen MembraneStatus
intensity_gen = elements [Lacking, LowAmount, HasPart, HighAmount]


add_on_intensity :: [B.ByteString] -> [Gen (B.ByteString, MembraneStatus)]
add_on_intensity markers = map add_on markers
  where
    add_on :: B.ByteString -> Gen (B.ByteString, MembraneStatus)
    add_on marker = fmap (\intensity -> (marker, intensity)) intensity_gen


markers_gen :: Gen [(B.ByteString, MembraneStatus)]
markers_gen = stage3
  where
    stage1 :: Gen [ Gen (B.ByteString, MembraneStatus)]
    stage1 = fmap add_on_intensity marker_name_gen
    
    stage2 :: Gen (Gen [(B.ByteString, MembraneStatus)])
    stage2 = fmap sequence stage1

    stage3 :: Gen [(B.ByteString, MembraneStatus)]
    stage3 = join stage2




instance Arbitrary (HM.HashMap B.ByteString MembraneStatus) where
  arbitrary = fmap HM.fromList markers_gen


subset_marker_profile :: MarkerProfile -> DecisionVariables -> MarkerProfile
subset_marker_profile profile vars = profile'
  where
    profile' = HM.fromList [ (marker, intensity) | (marker, intensity) <- HM.toList profile, S.member marker vars]
    


-- property: decision variables reproduce the classification
prop_reproducable ::  (HM.HashMap B.ByteString MembraneStatus) -> Bool
prop_reproducable profile = id1 == id2
  where
    ((_, id1), decision_vars) = classify_markers profile
    profile' = subset_marker_profile profile decision_vars
    ((_, id2), _) = classify_markers profile'


{-


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

-}


-- property: a proper subset of the decision variables does not reproduce the classification

prop_minimal_variables :: (HM.HashMap B.ByteString MembraneStatus) -> Bool
prop_minimal_variables profile = isNothing preliminary
  where
    ((_, ontology_id), nominal_vars) = classify_markers profile
    preliminary = classify_via_all_proper_subsets nominal_vars profile ontology_id
    




spec :: Spec
spec = describe "classification and decision variable properties" $ do
  prop "decision variables reproduce the classification" $ prop_reproducable -- `shouldBe` True
  prop "decision variables are minimal" $ prop_minimal_variables
