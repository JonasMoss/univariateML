# Verify the saved reference comparisons and subset coverage from repo root.
dir='dev/research/evaluations/2026-10-29-p0-discrete2/'
files=Sys.glob(paste0(dir,'oracle-*.csv'))
stopifnot(length(files)==12L)
d=do.call(rbind,lapply(files,read.csv)); ok=!is.na(d$master)
bad=ok & (!is.finite(d$oracle) | !is.finite(d$reference_at_fit) | abs(d$master-d$oracle)>1e-6*d$n | abs(d$master-d$reference_at_fit)>1e-6*d$n)
counts=aggregate(cbind(accepted=ok,disagree=bad)~id,d,sum)
print(counts); cat('accepted:',sum(ok),'disagreements:',sum(bad),'\n')
checks=read.csv(paste0(dir,'formula-checks.csv'));print(checks)
stopifnot(sum(bad)==0, all(checks$max_abs_log_error<1e-10))
