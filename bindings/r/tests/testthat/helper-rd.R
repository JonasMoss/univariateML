# Parsed Rd for a help page: the source man/ file in a checkout, else the
# installed package's Rd database (R CMD check), else skip.
umvl_rd <- function(name) {
  path <- file.path("..", "..", "man", name)
  if (file.exists(path)) return(tools::parse_Rd(path))
  db <- tryCatch(tools::Rd_db("univariateML"), error = function(e) NULL)
  if (!is.null(db[[name]])) return(db[[name]])
  testthat::skip(paste("help source unavailable:", name))
}

umvl_rd_text <- function(name) {
  gsub("[[:space:]]+", " ", paste(as.character(umvl_rd(name)), collapse = ""))
}
