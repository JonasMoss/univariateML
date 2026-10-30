#!/usr/bin/env Rscript
# Probe ordinary reference endpoints in-process; isolate dangerous nhyper calls.
suppressPackageStartupMessages(library(univariateML,lib.loc="/tmp/umvl-p0lib"))
rows<-list(
 data.frame(case="cmb prob=0",value=paste(dcmb(0:2,2,0,1),collapse=",")),
 data.frame(case="cmb prob=1",value=paste(dcmb(0:2,2,1,1),collapse=",")),
 data.frame(case="cmb size>1e7",value=as.character(suppressWarnings(dcmb(0,1e7+1,.5,1)))),
 data.frame(case="tbinom infinite bounds",value=as.character(extraDistr::dtbinom(0,3,.5,-Inf,Inf))),
 data.frame(case="bbinom shape=0",value=paste(extraDistr::dbbinom(0:3,3,0,1),collapse=",")))
# Separate R processes: a crash cannot terminate this script or lose its output.
for (expr in c('extraDistr::dnhyper(3,0,5,3)', 'extraDistr::dnhyper(0,0,0,0)')) {
  cmd<-paste("Rscript -e",shQuote(expr),"> /dev/null 2>&1")
  status<-system(cmd,ignore.stdout=TRUE,ignore.stderr=TRUE)
  rows[[length(rows)+1]]<-data.frame(case=expr,value=paste("subprocess exit",status))
}
write.table(do.call(rbind,rows),"dev/research/evaluations/2026-10-29-p0-binomial/domains.tsv",sep="\t",row.names=FALSE,quote=TRUE)
