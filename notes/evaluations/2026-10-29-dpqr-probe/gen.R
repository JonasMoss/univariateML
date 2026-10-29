# Generate an Rcpp shim exporting our C++ d/p/q for families whose C++
# signature order matches README param order and whose reference is external.
suppressMessages({library(yaml); library(jsonlite)})
D <- "/home/jonas/Files/programming/univariateML/cppunivariate/include/cppunivariate/distributions"
ids <- basename(list.dirs(D, recursive = FALSE))
fm <- function(id) {
  l <- readLines(file.path(D, id, "README.md"), warn = FALSE)
  e <- which(l == "---"); yaml.load(paste(l[(e[1] + 1):(e[2] - 1)], collapse = "\n"))
}
sig <- function(id, fn) {
  txt <- paste(unlist(lapply(list.files(file.path(D, id), "\\.hpp$", full.names = TRUE), readLines)), collapse = " ")
  m <- regmatches(txt, regexpr(paste0("inline double ", fn, "\\s*\\(double [a-z_]+[^)]*\\)"), txt))
  if (!length(m)) return(NULL)
  a <- strsplit(sub(".*\\((.*)\\)", "\\1", m), ",")[[1]]
  a <- trimws(sub("=.*", "", a)); sub(".*\\s", "", a)
}
rows <- list(); code <- c('#include <Rcpp.h>', '#include <cmath>')
for (id in ids) {
  f <- fm(id); ref <- f$r$density
  if (is.null(ref) || !grepl("^(extraDistr|actuar)::", ref)) next
  pn <- vapply(f$params, `[[`, "", "name")
  ds <- sig(id, paste0("d", id)); ps <- sig(id, paste0("p", id)); qs <- sig(id, paste0("q", id))
  ok <- !is.null(ds) && !is.null(ps) && !is.null(qs) &&
    identical(ds[2:(length(pn) + 1)], pn) && identical(ps[2:(length(pn) + 1)], pn) &&
    identical(qs[2:(length(pn) + 1)], pn) && length(ds) == length(pn) + 2
  rows[[id]] <- data.frame(id = id, ref = ref, params = paste(pn, collapse = ","),
                           probed = ok, cpp_d = paste(ds, collapse = ","))
  if (!ok) next
  code <- c(code, sprintf('#include "%s/dpqr.hpp"', file.path(D, id)))
  a <- paste0("a[", seq_along(pn) - 1, "]", collapse = ",")
  ns <- "cppunivariate::distributions::"
  code <- c(code, sprintf('
// [[Rcpp::export]]
Rcpp::NumericVector cd_%1$s(Rcpp::NumericVector x, Rcpp::NumericVector a, bool lg){Rcpp::NumericVector o(x.size());for(R_xlen_t i=0;i<x.size();++i)o[i]=%2$sd%1$s(x[i],%3$s,lg);return o;}
// [[Rcpp::export]]
Rcpp::NumericVector cp_%1$s(Rcpp::NumericVector x, Rcpp::NumericVector a, bool lt, bool lp){Rcpp::NumericVector o(x.size());for(R_xlen_t i=0;i<x.size();++i)o[i]=%2$sp%1$s(x[i],%3$s,lt,lp);return o;}
// [[Rcpp::export]]
Rcpp::NumericVector cq_%1$s(Rcpp::NumericVector x, Rcpp::NumericVector a, bool lt, bool lp){Rcpp::NumericVector o(x.size());for(R_xlen_t i=0;i<x.size();++i)o[i]=%2$sq%1$s(x[i],%3$s,lt,lp);return o;}
', id, ns, a))
}
tab <- do.call(rbind, rows)
write.csv(tab, "/tmp/dpqr-probe/families.csv", row.names = FALSE)
# headers in one TU: move includes to the top
inc <- grep("^#include", code, value = TRUE); body <- grep("^#include", code, value = TRUE, invert = TRUE)
writeLines(c(unique(inc), body), "/tmp/dpqr-probe/shim.cpp")
cat(sum(tab$probed), "probed of", nrow(tab), "external-reference families\n")
print(tab[!tab$probed, c("id", "params", "cpp_d")], row.names = FALSE)
