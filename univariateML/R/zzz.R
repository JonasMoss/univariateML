dense_candidates <- list.files("R")
dense_candidates <- dense_candidates[sapply(dense_candidates, function(x) substr(x, 1, 2) == "ml")]
dense_candidates <- dense_candidates[!grepl("select", dense_candidates)]
dense_candidates <- unname(unlist(sapply(dense_candidates, function(x) strsplit(x, ".R"))))

# Prefer generated list; fall back to discovered files.
if (!exists("generated_models")) generated_models <- character(0)
if (length(generated_models) > 0) {
  univariateML_models <- substring(generated_models, first = 3)
  densities <- generated_models
} else {
  univariateML_models <- substring(dense_candidates, first = 3)
  densities <- dense_candidates
}


# This function populate all (currently empty) univariateML functions.

if (length(univariateML_models) > 0) {
  for (model in univariateML_models) {
    fun <- paste0("ml", model)
    if (!(fun %in% generated_models)) {
      assign(fun, decorator(fun))
    }
  }
  if (exists("fun")) rm(fun)
}

out <- univariateML_metadata[["mlunif"]]
out$names_ <- out$names
out$names <- NULL
`attributes<-`(mlunif, out)
