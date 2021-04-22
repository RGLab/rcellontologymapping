################################################################################
### Handle interface to command line app

library("rjson")

map_markers <- function(marker_profile_text) {
  result <- system2(c("cell-ontology-mapping", "classify", "--json", marker_profile_text), stdout=TRUE)
  json_result <- fromJSON(result)
  if ("error_message" %in% names(json_result)) {
    stop(json_result$error_message)
  }
  return(json_result)
}
