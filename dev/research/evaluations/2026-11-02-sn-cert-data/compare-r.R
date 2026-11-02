# Replay the same 600 old-core samples through the generated R binding.
pkgload::load_all('univariateML-r', quiet=TRUE)
con <- pipe('build/scratch/sn_emit_old', open='r')
finite <- 0L
faces <- 0L
max_gap <- 0
for (i in 0:599) {
  line <- readLines(con, n=1L, warn=FALSE)
  if (length(line) != 1L) stop('old fitter stopped at sample ',i)
  p <- as.numeric(strsplit(line, ' ', fixed=TRUE)[[1L]])
  stopifnot(length(p)==104L,p[1L]==i)
  f <- tryCatch(mlsn(p[-(1:4)]),error=identity)
  if (p[2L]==0L) {
    if (inherits(f,'error')) stop('unexpected R error for sample ',i,': ',conditionMessage(f))
    gap <- as.numeric(logLik(f))-p[4L]
    max_gap <- max(max_gap,abs(gap))
    if(abs(gap)>1e-6) stop('likelihood disagreement at sample ',i,': ',gap)
    finite <- finite+1L
  } else if (p[2L] %in% c(3L,4L)) {
    if (!inherits(f,'error') || !grepl('mlhnorm',conditionMessage(f),fixed=TRUE))
      stop('face disagreement at sample ',i)
    if (p[2L]==4L && !grepl('on `-x`',conditionMessage(f),fixed=TRUE))
      stop('face direction disagreement at sample ',i)
    faces <- faces+1L
  } else stop('old fitter failed at sample ',i)
}
close(con)
cat('R paired samples:',finite,'finite,',faces,'faces; maximum likelihood gap',format(max_gap,digits=17),'\n')
