# Run after `just regen` from the repository root, using one CPU slot.
pkgload::load_all("univariateML-r", quiet = TRUE)
root <- "univariateML-cpp/include/cppunivariate/distributions"
construct <- getFromNamespace("univariateML_construct", "univariateML")
files <- Sys.glob(file.path(root, "*", "golden.json"))
results <- lapply(files, function(file) {
  g <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  x <- as.numeric(unlist(g$data)); est <- as.numeric(unlist(g$estimates))
  fixed <- names(Filter(Negate(is.null), g$fit_args))
  fixed <- intersect(fixed, names(g$estimates))
  fit <- construct(est, paste0("ml", g$id), list(n=length(x), logLik=g$logLik,
                   call="NULL"), fixed=fixed, data=x)
  old <- fit; attr(old,"inference_data") <- NULL; attr(old,"inference_free") <- NULL
  status <- "ok"
  elapsed <- system.time(tryCatch(suppressWarnings(vcov(fit)), error=function(e) {
    status <<- conditionMessage(e)
  }))[["elapsed"]]
  data.frame(id=g$id, n=length(x), bytes=as.numeric(object.size(fit)-object.size(old)),
             elapsed=elapsed, status=status)
})
tab <- do.call(rbind, results)
write.csv(tab, "build/scratch/vcov-measure.csv", row.names=FALSE)
print(head(tab[order(-tab$elapsed),], 15), row.names=FALSE)
print(tab[tab$id %in% c("norm","gamma","pois"),], row.names=FALSE)
