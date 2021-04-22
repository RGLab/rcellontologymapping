-- Copyright 2021 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- App to classify cells based on observed surface markers

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}



module Main where





import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C

import qualified Data.Vector as V

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

import Data.Maybe (fromJust)
import Data.List (sortBy, subsequences)

--import qualified Options.Applicative as O
import Data.Semigroup ((<>))

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt

import Data.Aeson

import Lib
import LoadCellInfo
import ManualTree
import Membrane
import ImmportDefinitions
import ParseMarkers
import MappingInterface


--------------------------------------------------------------------------------

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]



to_profile :: [(String, MembraneStatus)] -> MarkerProfile
to_profile profile_list = results
  where
    results = HM.fromList [ (C.pack m, intensity) | (m, intensity) <- profile_list]



getArgOrExit = getArgOrExitWith patterns

-- Marker text should have the format like "CD3e-, CD19+, CD24++, CD38++"

main = do
  args <- parseArgsOrExit patterns =<< getArgs
  --print . show $ args


  when (args `isPresent` (command "classify")) $ do
    --print . show $ args
    marker_text <- args `getArgOrExit` (argument "marker_text")
    let results = generate_results_obj marker_text
    case (args `isPresent` (longOption "json")) of 
      True -> print_json_text results
      False -> print_human_friendly_results results

  when (args `isPresent` (command "markers")) $ do
    putStrLn "Marker names: "
    putStrLn $ show ManualTree.all_markers


