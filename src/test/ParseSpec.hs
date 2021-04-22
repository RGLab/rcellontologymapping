

module ParseSpec where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

import Test.Hspec.Megaparsec

--------------------------------------------------------------------------------

import ParseMarkers
import Membrane

spec :: Spec
spec = describe "Parsing marker profile tests" $ do
  it "parse CD4" $ do
    parse known_marker "" "CD4" `shouldParse` "CD4"
    
  it "parse HLA-DRA" $ do
    parse known_marker "" "HLA-DRA" `shouldParse` "HLA-DRA"
    
  it "parse CD45RA" $ do
    parse known_marker "" "CD45RA" `shouldParse` "CD45RA"

  it "parse intensity ++" $ do
    parse marker_intensity "" "++" `shouldParse` HighAmount
    
  it "parse HLA-DRA-" $ do
    parse marker_status "" "HLA-DRA-" `shouldParse` ("HLA-DRA", Lacking)
    
  it "parse quoted HLA-DRA+" $ do
    parse marker_status "" "\"HLA-DRA+\"" `shouldParse` ("HLA-DRA", HasPart)

{-

parseTest known_marker "CD4"


parseTest marker_intensity "++"
HighAmount

parseTest marker_status "CD4+~"
("CD4",HasPart)

parseTest marker_status "CD4-"
("CD4",Lacking)


parseTest marker_profile "CD4+, CD3e+"
[("CD4",HasPart),("CD3e",HasPart)]


-- also allowed
parseTest marker_profile "CD4+ , CD3e+"
[("CD4",HasPart),("CD3e",HasPart)]

stack exec -- decision-table-exe -m "CD3e+, CD4+"
("CD4+ T cell","http://purl.obolibrary.org/obo/CL_0000624")


-}
