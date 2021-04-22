-- Copyright 2021 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- Load the cells and markers from the CSV file

{-# LANGUAGE OverloadedStrings #-}

module LoadCellInfo where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.ByteString (hGetLine)
import qualified Data.Csv as Cassava
import qualified Data.Vector as V


import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S

import Data.Bits.Utils (w82c, c2w8)
--import qualified Data.HashMap.Lazy as H
import System.IO

--------------------------------------------------------------------------------

import Membrane

data BasicRow = BasicRow
    { br_class :: B.ByteString
    , br_features :: HM.HashMap B.ByteString B.ByteString
    }
  deriving (Show)



-- (..=) = (Cassava..:)

-- cell_file = "/home/kcurtis/remote-work/cytometry/task-related/t16-cell-sparql/pruned_cell_table.csv"
cell_file = "/home/kcurtis/remote-work/cytometry/task-related/t16-cell-sparql/cell_table_with_full_labels.csv"


load_plain_info :: FilePath -> IO (Either String (V.Vector (V.Vector BL.ByteString)))
load_plain_info filename = do
    text <- BL.readFile filename
    let results = Cassava.decode Cassava.HasHeader text
    return results


load_cell_info :: FilePath -> IO (Either String (Cassava.Header, V.Vector Cassava.NamedRecord))
load_cell_info filename = do
    text <- BL.readFile filename
    let results = Cassava.decodeByName text
    return results



simple_read_header :: String -> IO [BL.ByteString]
simple_read_header filename = do
  handle <- openFile filename ReadMode
  header_line <- B.hGetLine handle
  hClose handle
  return $ BL.split (c2w8 ',') (BL.fromStrict header_line)




celltype_column = "celltype"


to_membrane_status :: B.ByteString -> MembraneStatus
to_membrane_status "unknown"                         = Unknown
to_membrane_status "lacks_plasma_membrane_part"      = Lacking
to_membrane_status "has_low_plasma_membrane_amount"  = LowAmount
to_membrane_status "has plasma membrane part"        = HasPart
to_membrane_status "has_high_plasma_membrane_amount" = HighAmount



convert_to_rows :: (V.Vector B.ByteString, V.Vector (HM.HashMap B.ByteString B.ByteString)) -> [BasicRow]
convert_to_rows (header, records) = results
  where
    hashmaps = V.toList records
    
    combine :: HM.HashMap B.ByteString B.ByteString -> [BasicRow] -> [BasicRow]
    combine m rows = new_row : rows
      where
        Just cell_type = HM.lookup celltype_column m
        new_row = BasicRow cell_type (HM.delete celltype_column m)
    results = foldr combine [] hashmaps


encode_row :: BasicRow -> Row
encode_row row = row'
  where
    features = HM.map to_membrane_status (br_features row)
    row' = Row "unset" (br_class row) features



load_encoded_rows :: IO [Row]
load_encoded_rows = do
     Right (header, results) <- load_cell_info cell_file

     let rows = convert_to_rows (header, results)
         rows' = map encode_row rows
     return rows'
