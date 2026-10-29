# Writes the parameter table (2026-10-29-parameter-table.csv): every
# parameter with its README flags, adapter default and reference default.
# Needs an assembled package (just regen). Output path below.
setwd("univariateML")  # run from the repository root
source("scripts/load_distributions.R")
m <- load_distributions("../cppunivariate/include/cppunivariate/distributions")
env <- new.env()
for (e in parse("package/R/RcppExports.R")) try(eval(e, env), silent = TRUE)
fmt <- function(v) if (is.null(v)) "" else if (is.language(v) || is.symbol(v)) deparse(v) else paste(format(v), collapse = ",")
fdef <- function(fl, a) { if (!(a %in% names(fl))) return("<absent>"); if (identical(fl[[a]], quote(expr=))) return("<none>"); fmt(fl[[a]]) }
rows <- list()
for (d in m) {
  ref <- d$density
  reff <- tryCatch({ if (grepl("::", ref)) { p <- strsplit(ref, "::")[[1]]; getExportedValue(p[1], p[2]) } else NULL }, error = function(e) NULL)
  rf <- if (!is.null(reff)) formals(reff) else NULL
  wf <- formals(get0(paste0("ml", d$id, "_"), env))
  args <- d$args  # ref arg -> coef
  for (p in d$params) {
    refargs <- if (!is.null(args)) names(args)[unlist(args) == p$name] else p$name
    refdef <- if (is.null(rf)) "(own)" else paste(vapply(refargs, function(a) fdef(rf, a), ""), collapse = ";")
    wdef <- if (p$name %in% names(wf)) fdef(wf, p$name) else "-"
    flags <- paste(c(if (isTRUE(p$fixable)) "fixable", if (!is.null(p$fixed)) paste0("fixed=", fmt(p$fixed)), if (isTRUE(p$required)) "required", if (!is.null(p$bound)) paste0("bound=", p$bound), if (isTRUE(p$integer)) "integer", if (isTRUE(p$kink)) "kink"), collapse = " ")
    rows[[length(rows)+1]] <- data.frame(id = d$id, src = if (grepl("::", ref)) sub("::.*","",ref) else "own", support = paste0(d$support$type, if (isTRUE(d$support$closed[1])) "[" else "(", fmt(d$support$lower), ",", fmt(d$support$upper), if (isTRUE(d$support$closed[2])) "]" else ")"),
      param = p$name, role = fmt(p$role), domain = fmt(p$domain), default = fmt(p$default), flags = flags, wrapper_default = wdef, ref_arg = paste(refargs, collapse=";"), ref_default = refdef, stringsAsFactors = FALSE)
  }
}
out <- do.call(rbind, rows)
write.csv(out, "../notes/evaluations/2026-10-29-parameter-table.csv", row.names = FALSE)
cat(nrow(out), "params in", length(m), "families\n")
print(table(out$role))
