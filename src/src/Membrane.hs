-- Copyright 2021 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- data structures for membrane status stuff

{-# LANGUAGE OverloadedStrings #-}


module Membrane where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S


--------------------------------------------------------------------------------

data MembraneStatus = Unknown | Lacking | LowAmount | HasPart | HighAmount
  deriving (Show, Eq, Ord)

type MarkerProfile = HM.HashMap B.ByteString MembraneStatus

data Row = Row
   { r_class :: B.ByteString
   , r_human_readable :: B.ByteString
   , r_features :: MarkerProfile
   } deriving (Show)



create_row_from_markers :: [B.ByteString] -> (S.Set B.ByteString) -> Row
create_row_from_markers markers observed_markers = row
  where
    features = HM.fromList [ (m, if S.member m observed_markers then HasPart else Lacking) | m <- markers]
    row = Row { r_class = "unset", r_human_readable = "unset", r_features = features}


intensity_status :: B.ByteString -> MembraneStatus
intensity_status "-" = Lacking
intensity_status "+" = HasPart
intensity_status "+-" = LowAmount
intensity_status "+~" = HasPart -- intermediate
intensity_status "++" = HighAmount
intensity_status _ = Unknown

to_intensity_label :: MembraneStatus -> B.ByteString
to_intensity_label Unknown = "NA"
to_intensity_label Lacking = "-"
to_intensity_label LowAmount = "+-"
to_intensity_label HasPart = "+"
to_intensity_label HighAmount = "++"
