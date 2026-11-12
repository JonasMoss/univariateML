# Run from the worktree root after just test, under /tmp/umvl-cpu/run.
# Rebuild baseline dispatch from its commit; do not archive source trees.
pkgload::load_all("univariateML-r",compile=FALSE,quiet=TRUE)
ns <- asNamespace("univariateML")
old <- new.env(parent=ns)
baseline <- "85f2bb6790d7678b780c3bf104b5c50c3e48125a"
eval(parse(text=system2("git",c("show",paste0(baseline,":bindings/r/R/code_helpers.R")),stdout=TRUE)),envir=old)
# modifiers.R is unchanged by this job; only its dispatch environment differs.
old$ml_modifier_function <- get("ml_modifier_function",ns)
environment(old$ml_modifier_function) <- old
rows <- list()
for(id in c("geom","nbinom")) {
  args <- if(id=="geom")list(prob=.3) else list(size=2,prob=.3)
  fit <- do.call(get(paste0("ml",id)),c(args,list(truncate=c(2,7))))
  attr(fit,"density") <- paste0("stats::d",id)
  rows[[id]] <- data.frame(id,x=3:5,
    baseline_stats=old$univariateML_to_function(fit,"p")(3:5),
    current_core=pml(3:5,fit),scalar=vapply(3:5,pml,numeric(1),obj=fit))
}
write.csv(do.call(rbind,rows),
  "dev/research/evaluations/2026-11-12-stats-route-data/modifier-vectorization.csv",row.names=FALSE)
print(do.call(rbind,rows))
