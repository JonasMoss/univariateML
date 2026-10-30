## New free-endpoint limits under the proposed lower bounds; CSV on stdout.
## Usage: Rscript dev/research/evaluations/2026-10-29-p0-genbeta/limit-paths.R
suppressPackageStartupMessages(library(actuar))
suppressPackageStartupMessages(library(extraDistr))
x <- c(.2,.4,.7,1,1.6); mu <- .7; sigma <- .4
out <- list(); j <- 0
record <- function(family,r,ll,target) {
 j <<- j+1; out[[j]] <<- data.frame(family=family,r=r,ll=ll,limit=target,difference=ll-target)
}
logistic <- sum(dlogis(x,location=mu,scale=sigma,log=TRUE))
for(r in c(10,30,100,300,1000)) {
 p <- list(min=-r,scale=r+mu,shape=r/sigma)
 record('pareto3',r,sum(do.call(actuar::dpareto3,c(list(x=x,log=TRUE),p))),logistic)
 p4 <- list(min=-r,scale=r+mu,shape1=1,shape2=r/sigma)
 record('pareto4',r,sum(do.call(actuar::dpareto4,c(list(x=x,log=TRUE),p4))),logistic)
 pf <- c(p4,list(shape3=1))
 record('fpareto',r,sum(do.call(actuar::dfpareto,c(list(x=x,log=TRUE),pf))),logistic)
}
normal <- sum(dnorm(x,0,1,log=TRUE))
for(r in c(3,5,10,30,100)) {
 a <- r^2/2
 record('nsbeta',r,sum(extraDistr::dnsbeta(x,shape1=a,shape2=a,min=-r,max=r,log=TRUE)),normal)
}
gamma <- sum(dgamma(x,shape=2,scale=1,log=TRUE))
for(r in c(10,30,100,300,1000))
 record('genbeta',r,sum(actuar::dgenbeta(x,shape1=2,shape2=r,shape3=1,scale=r,log=TRUE)),gamma)
args <- commandArgs(TRUE)
write.csv(do.call(rbind,out),if(length(args)) args[1] else stdout(),row.names=FALSE)
