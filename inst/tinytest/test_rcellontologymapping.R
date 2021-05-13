library("rcellontologymapping")

# Placeholder with simple test
expect_equal(1 + 1, 2)


json_result = map_markers("CD3e-, CD19+, CD24++, CD38++")

expect_equal(json_result$ontology_id, "http://purl.obolibrary.org/obo/CL_0000818")