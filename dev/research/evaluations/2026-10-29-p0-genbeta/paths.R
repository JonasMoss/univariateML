## Reference log densities along endpoint-singularity and collapse paths.
## Run from repository root: Rscript dev/research/evaluations/2026-10-29-p0-genbeta/paths.R
suppressPackageStartupMessages(library(actuar))
suppressPackageStartupMessages(library(extraDistr))
x <- c(1,2,3,4,5); e <- 10^-(1:8)
L <- function(d,x,p) sum(do.call(d,c(list(x=x),p,list(log=TRUE))))
for (v in e) {
 p <- list(
  pareto2=L(actuar::dpareto2,x,list(min=1-v,shape=.2,scale=v)),
  pareto3=L(actuar::dpareto3,x,list(min=1-v,shape=.4,scale=1)),
  pareto4=L(actuar::dpareto4,x,list(min=1-v,shape1=2,shape2=.4,scale=1)),
  pareto4_c_ge_1=L(actuar::dpareto4,x,list(min=1-v,shape1=2,shape2=.6,scale=1)),
  fpareto=L(actuar::dfpareto,x,list(min=1-v,shape1=2,shape2=1,shape3=.4,scale=1)),
  fpareto_c_ge_1=L(actuar::dfpareto,x,list(min=1-v,shape1=2,shape2=1,shape3=.4,scale=1)),
  nsbeta=L(extraDistr::dnsbeta,x,list(min=1-v,max=6,shape1=.4,shape2=2)),
  genbeta=L(actuar::dgenbeta,x,list(scale=5+v,shape1=2,shape2=.4,shape3=1)))
 cat(format(v,scientific=TRUE),paste(names(p),format(unlist(p),digits=7),sep='=',collapse=' '),'\n')
}
