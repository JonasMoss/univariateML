# Identity unless a changed-family run explicitly requests a subset. Accept both
# dossier ids and R fitter names so the same selector serves existing loops.
umvl_filter_families <- function(ids) {
  setting <- Sys.getenv("UMVL_TEST_FAMILIES")
  if (!nzchar(setting)) return(ids)
  selected <- strsplit(setting, ",", fixed = TRUE)[[1L]]
  ids[sub("^ml", "", ids) %in% sub("^ml", "", selected)]
}
