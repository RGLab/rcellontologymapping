-- Copyright 2021 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- Parse Markers listed on command line, following Everton et al. suggestions for format
--- where commas are used as separators, and markers that have commas are enclosed in double quotes

{-# LANGUAGE TypeFamilies        #-}


-- MegaParsec??

module ParseMarkers where

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)
import Data.Char
import Data.Void
import Data.List (sortBy)

import Data.ByteString.UTF8 (toString)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)

import Membrane
import qualified ManualTree as ManualTree


{-

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



-}

type Parser = Parsec Void String



all_markers = sort_by_length [ toString i | i <- ManualTree.all_markers]
  where
    compare_length :: String -> String -> Ordering
    compare_length a b = compare (length b) (length a)
    sort_by_length = sortBy compare_length


closest_match :: String -> String
closest_match typo = candidate
  where
    scores = map (levenshteinDistance defaultEditCosts typo) all_markers
    compare_score :: (String,Int) -> (String, Int) -> Ordering
    compare_score (_,a) (_,b) = compare a b
    combined = zip all_markers scores
    sorted_by_score = sortBy compare_score combined
    candidate = fst . head $ sorted_by_score


{-
known_marker :: Parser String
known_marker = try (p >>= check)
  where
     p = (:) <$> letterChar <*> many non_space_char
     check x = if x `elem` all_markers then return x else fail $ "Marker " ++ show x ++ " is not recognized"
-}


known_marker :: Parser String
known_marker = choice (map string all_markers)


non_space_char :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
non_space_char = satisfy is_non_space <?> "non-white space but printable"
  where
    is_non_space c =  not (isSpace c) && (isAlphaNum c || ('/' == c) || (':' == c) || '"' == c)

intensity_char :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
intensity_char = satisfy is_relevant <?> "character used in marker intensity"
  where
    is_relevant c = ('-' == c) || ('+' == c) || ('~' == c)
    
marker_intensity :: Parser MembraneStatus
marker_intensity = try (p >>= check)
  where
     p = (:) <$> intensity_char <*> many intensity_char
     check x = if x `elem` ["-", "+", "+-", "+~", "++"] then return (intensity_status . C.pack $ x) else fail $ "Intensity " ++ show x ++ " is not recognized"

plain_marker_status :: Parser (String, MembraneStatus)
plain_marker_status = do
  marker <- known_marker
  status <- marker_intensity
  return (marker, status)

quoted_marker_status :: Parser (String, MembraneStatus)
quoted_marker_status = do
  char '"'
  marker <- known_marker
  status <- marker_intensity
  char '"'
  return (marker, status)

-- do search over markers
failed_marker :: Parser (String, MembraneStatus)
failed_marker = do
  p <- (:) <$> non_space_char <*> many non_space_char
  let candidate = closest_match p
  fail $ "Marker " ++ show p ++ " is not recognized. Did you mean " ++ candidate ++ "?"


marker_status :: Parser (String, MembraneStatus)
marker_status = do
  (marker, status) <- (try quoted_marker_status) <|> (try plain_marker_status) <|>  failed_marker
  return (marker, status)



{-
marker_terminator :: Parser ()
marker_terminater = eol <|> satisfy (\c -> isSpace c || c == ',')

minus_marker :: Parser (String, MembraneStatus)
minus_marker = do
    ends with ?

low_marker :: Parser (String, MembraneStatus)


has_marker :: Parser (String, MembraneStatus)

intermediate_marker :: Parser (String, MembraneStatus)

high_marker :: Parser (String, MembraneStatus)
-}






marker_profile :: Parser [(String, MembraneStatus)]
marker_profile = do
  remainder <- (try go_marker) <|> end
  return remainder
  where
    go_marker :: Parser [(String, MembraneStatus)]
    go_marker = do
      p <- marker_status
      remainder <- try go_next <|> end
      return $ p : remainder
      
    go_next :: Parser [(String, MembraneStatus)]
    go_next = do
      next_token
      remainder <- try go_marker <|> end
      return remainder
      
    end :: Parser [(String, MembraneStatus)]
    end = do
      eof
      return []



next_token :: Parser ()
next_token = do
  space
  char ','
  space


to_profile :: [(String, MembraneStatus)] -> MarkerProfile
to_profile profile_list = results
  where
    results = HM.fromList [ (C.pack m, intensity) | (m, intensity) <- profile_list]

 

read_marker_profile :: String -> Either String MarkerProfile
read_marker_profile text = case (parse marker_profile "" text) of
                             Left bundle -> Left (parseErrorPretty bundle)
                             Right x -> Right . to_profile $ x

