## Run one or more independent, checkpointed family probes, from repo root.
ids <- commandArgs(TRUE)
if (!length(ids)) stop('give at least one family id')
dir <- 'notes/evaluations/2026-10-29-p0-genbeta'
for (id in ids) {
  cat('starting',id,'\n'); flush.console()
  status <- system2(file.path(R.home('bin'),'Rscript'),
                    c(file.path(dir,'oracle.R'),id,'all',file.path(dir,paste0(id,'.csv'))))
  cat('finished',id,'status',status,'\n'); flush.console()
  if(status != 0) quit(status=status)
}
