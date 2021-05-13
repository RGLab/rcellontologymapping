################################################################################
### Handle interface to command line app


library("rjson")


#' Map markers to Cell Ontology ID
#' 
#' @param marker_profile_text The cell surface marker text in Overton et al. format
#' @param y A number.
#' @return A list containing the fields \code{extra_markers} (markers not used for the mapping), \code{decision_variables} (markers used in the mapping), \code{ontology_id} (Cell Ontology ID), and \code{label} (human-readable cell label).
#' @examples
#' map_markers("CD3e-")
#' map_markers("CD3e+, CD4+")
#' @export
map_markers <- function(marker_profile_text) {
  result <- system2(c("cell-ontology-mapping", "classify", "--json", marker_profile_text), stdout=TRUE)
  json_result <- rjson::fromJSON(result)
  if ("error_message" %in% names(json_result)) {
    stop(json_result$error_message)
  }
  return(json_result)
}
