# Run from the worktree root after the R suite has compiled the package.
pkgload::load_all("univariateML-r", quiet=TRUE, compile=FALSE, export_all=FALSE)
cases <- read.delim("bindings/r/tests/testthat/fixtures/short-window.tsv")
output <- numeric(nrow(cases))
for (name in unique(cases$case)) {
  indices <- which(cases$case==name & cases$op!="refuse")
  if (!length(indices)) next
  rows <- cases[indices,]
  family <- rows$family[1]
  parameters <- switch(family,nbinom=c("size","prob"),binom=c("size","prob"),
                       geom="prob",pois="lambda",zeta="s")
  values <- as.numeric(strsplit(rows$parameters[1],":",fixed=TRUE)[[1]])
  model <- do.call(get(paste0("ml",family)),
    c(as.list(setNames(values,parameters)),list(truncate=c(rows$lo[1],rows$hi[1]))))
  for (i in indices) {
    row <- cases[i,]
    lower <- as.logical(row$lower)
    logarithm <- as.logical(row$log)
    output[i] <- switch(row$op,
      d=dml(row$input,model,log=logarithm),
      p=pml(row$input,model,lower.tail=lower,log.p=logarithm),
      q=qml(row$input,model,lower.tail=lower,log.p=logarithm))
  }
}
cat(sprintf("%.17g",output[cases$op!="refuse"]),sep="\n")
