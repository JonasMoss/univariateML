# Run from repository root after just test. Single process; no package edits.
# OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 MAKEFLAGS=-j1 nice -n 10 Rscript dev/research/ml-behaviour-sweep.R
Sys.setenv(OMP_NUM_THREADS=1, OPENBLAS_NUM_THREADS=1, MKL_NUM_THREADS=1)
suppressMessages(pkgload::load_all("univariateML-r", compile=FALSE, quiet=TRUE))
meta <- univariateML_metadata
stopifnot(length(meta)>=42L)
resolve <- function(ref) { z <- strsplit(ref, "::", fixed=TRUE)[[1]]; if(length(z)==2) getExportedValue(z[1], z[2]) else get(ref, asNamespace("univariateML")) }
fmt <- function(x) if(!length(x)) "—" else paste(paste(names(x), formatC(x, digits=7, format="g"), sep="="), collapse=", ")
clean <- function(x) gsub("[\r\n|]", " ", x)
num <- function(x) if(length(x)) formatC(x, digits=9, format="g") else "—"
rows <- list(); began <- proc.time()[3]
for(ii in seq_along(meta)) {
 key <- names(meta)[ii]; id <- sub("^ml", "", key); m <- meta[[ii]]
 def <- setNames(m$default, m$names); dens <- resolve(m$density)
 sampler <- resolve(univariateML_samplers[[key]])
 draw <- function(n) do.call(sampler, c(list(n=n),univariateML_dpqr_args(def,key)))
 ll <- function(p,x) suppressWarnings(tryCatch({v<-do.call(dens,c(list(x=x),univariateML_dpqr_args(p,key),list(log=TRUE))); if(length(v)!=length(x) || any(!is.finite(v))) -Inf else sum(v)},error=function(e) -Inf))
 sets <- list()
 for(n in c(20,200)) for(s in 1:2) {set.seed(100000+ii*100+n+s); sets[[paste0("family",n,"-s",s)]]<-draw(n)}
 set.seed(200000+ii)
 sets$small3 <- draw(3); base <- draw(200)
 # Preserve open support when rounding; force repeated values by duplicating pairs.
 tied <- round(base,1); valid <- tied>m$support$bounds[1] & tied<m$support$bounds[2]
 tied[!valid] <- base[!valid]; tied[seq(2,200,2)]<-tied[seq(1,199,2)]
 sets$ties <- tied
 out <- base; out[1] <- if(is.finite(m$support$bounds[2])) m$support$bounds[2]-(m$support$bounds[2]-max(base))/1000 else if(m$support$bounds[1]>=0) max(base)*1000 else max(base)+1000
 sets$outlier <- out
 for(j in 1:2) if(is.finite(m$support$bounds[j]) && m$support$closed[j]) {z<-base; z[1]<-m$support$bounds[j]; sets[[paste0("bound",j)]]<-z}
 # Parameter-dependent finite support endpoints, absent from the generic support.
 if(id %in% c("unif","dunif")) {z<-base; z[1:2]<-def; sets$bounds<-z}
 if(id=="zipf") {z<-base; z[1:2]<-c(1,def["N"]); sets$bounds<-z}
 set.seed(300000+ii)
 limit <- switch(id, std=rnorm(200),sstd=rnorm(200),ged=runif(200,-1,1),sged=runif(200,-1,1),snorm=rexp(200),lomax=rexp(200),gompertz=rexp(200),nbinom=rpois(200,4),binom=rpois(200,4),burr=extraDistr::rpareto(200,1,2),invburr=runif(200)^(1/2),invgauss=1/rnorm(200)^2,naka=sqrt(rgamma(200,.3)),zip={z<-rpois(200,3); z[!(z==0 & runif(200)<.9)]},NULL)
 if(!is.null(limit)) sets$limit200<-limit
 if(id=="burr") sets$nearPareto<-c(1+rexp(199),.99)
 if(id %in% c("beta","kumar")) {sets$betaExtreme<-rbeta(200,.05,.05); sets$betaClipped<-pmin(pmax(sets$betaExtreme,1e-12),1-1e-12)}
 for(ds in names(sets)) {
 x<-sets[[ds]]; set.seed(400000+ii*100+match(ds,names(sets)))
 warnings<-character(); err<-""
 fit<-tryCatch(withCallingHandlers(get(key)(x),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e){err<<-conditionMessage(e);NULL})
 ours<-if(is.null(fit)) numeric() else setNames(as.numeric(fit),names(fit))
 reported<-if(is.null(fit)) NA_real_ else as.numeric(attr(fit,"logLik"))
 if(!is.null(fit) && (any(!is.finite(ours)) || !is.finite(reported))) err<-paste("Nonfinite fitter return (no exception):",fmt(ours),"logLik",reported)
 # Always align by metadata names (never density formal order).
 pfit<-if(length(ours) && all(m$names %in% names(ours))) ours[m$names] else NULL
 reflik<-if(is.null(pfit)) NA_real_ else ll(pfit,x)
 fixed<-integer(); status<-""; best<- -Inf; bp<-numeric(); conv<-NA_integer_; attempts<-0L
 if(id %in% c("binom","zipf")) {fixed<-1L; if(is.null(pfit)) status<-"SKIP: integer size unavailable" else status<-"integer size held at fitter value"}
 # Positive except location/endpoints; shifted log for nu>2 and fatigue mu<min(x).
 kind<-rep("log",length(def)); names(kind)<-names(def)
 kind[names(def) %in% c("mean","meanlog","location","mu","min","max")]<-"identity"
 kind[names(def) %in% c("prob","pi","theta")]<-"logit"
 if(id %in% c("std","sstd")) kind["nu"]<-"df"
 if(id=="fatigue") kind["mu"]<-"below"
 if(id=="invgauss") kind["mean"]<-"log"
 encode<-function(p) {z<-p; for(k in seq_along(p)) z[k]<-switch(kind[k],log=log(pmax(p[k],1e-12)),logit=qlogis(pmin(pmax(p[k],1e-12),1-1e-12)),df=log(pmax(p[k]-2,1e-12)),below=log(pmax(min(x)-p[k],1e-12)),p[k]);z}
 decode<-function(z) {p<-z; for(k in seq_along(z)) p[k]<-switch(kind[k],log=exp(z[k]),logit=plogis(z[k]),df=2+exp(z[k]),below=min(x)-exp(z[k]),z[k]);p}
 if(!startsWith(status,"SKIP")) {
 free<-setdiff(seq_along(def),fixed); anchor<-encode(def)
 if(length(fixed)) anchor[fixed]<-encode(pfit)[fixed]
 objective<-function(z) {a<-anchor;a[free]<-z;p<-decode(a);v<-ll(p,x);if(is.finite(v)) -v else 1e100}
 starts<-list(anchor)
 if(id=="gompertz" && is.null(pfit)) starts[[2]]<-encode(setNames(c(1/mean(x),0.1/max(x)),names(def)))
 if(!is.null(pfit) && all(is.finite(pfit))) starts[[2]]<-encode(pfit)
 # Data-based centre helps location families when defaults are far away.
 centre<-if(length(starts)>1) starts[[2]] else anchor
 while(length(starts)<6) starts[[length(starts)+1]]<-centre+rnorm(length(def),sd=1)
 for(a in starts) {
 z<-a[free]; attempts<-attempts+1L
 for(method in c("Nelder-Mead","BFGS")) {
 opt<-suppressWarnings(tryCatch(optim(z,objective,method=method,control=list(maxit=if(method=="Nelder-Mead") 800 else 400,reltol=1e-10)),error=function(e) NULL))
 if(!is.null(opt)) {z<-opt$par; full<-anchor;full[free]<-z;p<-decode(full);v<-ll(p,x);if(is.finite(v) && v>best){best<-v;bp<-p;conv<-opt$convergence}}
 }
 # Keep valid starting candidates too (including exact support/boundary fits).
 full<-anchor; full[free]<-a[free];p<-decode(full);v<-ll(p,x)
 if(is.finite(v) && v>best){best<-v;bp<-p;conv<-NA_integer_}
 }
 if(!is.finite(best)) status<-paste(status,"NO FINITE ORACLE")
 }
 flags<-character()
 if(nzchar(err)) flags<-c(flags,"ERROR")
 if(grepl("boost",paste(err,warnings,collapse=" "),ignore.case=TRUE)) flags<-c(flags,"BOOST")
 # All large estimates; small positive scale/shape/df parameters only.
 loc<-names(ours) %in% c("mean","meanlog","location","mu","min","max","prob","pi","theta")
 if(length(ours) && any(abs(ours)>1e4 | (!loc & abs(ours)<1e-4),na.rm=TRUE)) flags<-c(flags,"ESCAPE")
 if(is.finite(reported) && is.finite(best) && best-reported>1e-4*(abs(reported)+1)) flags<-c(flags,"NOT_MAX")
 if(!length(flags)) flags<-"OK"
 rows[[length(rows)+1]]<-list(id=id,dataset=ds,n=length(x),ours=ours,ll=reported,reference=reflik,oracle=bp,oracle_ll=best,flags=paste(flags,collapse=","),message=paste(Filter(nzchar,c(err,warnings,status)),collapse="; "),starts=attempts,convergence=conv)
 }
 cat(id,":",length(sets),"datasets; elapsed",round(proc.time()[3]-began,1),"s\n");flush.console()
}
saveRDS(rows,"/tmp/ml-behaviour-sweep-results.rds")
summary<-c("## Summary", "",sprintf("%d rows across %d fitters; elapsed %.1f seconds on one core. Flags may overlap.",length(rows),length(meta),proc.time()[3]-began), "")
for(id in unique(vapply(rows,`[[`,"", "id"))) {
 rr<-Filter(function(r) r$id==id && r$flags!="OK",rows)
 if(length(rr)) summary<-c(summary,paste0("- **",id,"**: ",paste(vapply(rr,function(r) paste0(r$dataset," [",r$flags,"]",if(nzchar(r$message)) paste0(" — ",clean(r$message)) else ""),""),collapse="; "),"."))
}
summary<-c(summary,"", "### Reading the flags", "", "Interpretation is in dev/research/ml-behaviour.tex. ESCAPE is a magnitude threshold, not a proof of nonexistence. NOT_MAX is a counterexample from a finite search by an unrestricted oracle, so it also fires where the fitter deliberately restricts the space (GED and SGED fit nu >= 1, decision D6; fatigue fixes mu = 0). OK does not certify optimality. ERROR rows include the intended limit-model stops (decision D1).")
header<-c("# Behaviour sweep against reference-density optimisation", "", "Run: `OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 MAKEFLAGS=-j1 nice -n 10 Rscript dev/research/ml-behaviour-sweep.R` after `just test`.", "", "Six starts per oracle: defaults, fitter when available, and seeded Gaussian perturbations in transformed coordinates (a data-scaled Gompertz start is used when its fitter fails). Each start runs Nelder–Mead (800 iterations), then BFGS (400), reltol 1e-10. Positive parameters use log, probabilities logit, Student-t nu uses 2+exp; fatigue location is min(x)-exp. Other locations/endpoints are unconstrained with infeasible densities rejected. All finite start candidates are retained. Binomial and Zipf integer size is fixed at the fitter value; skipped if unavailable. Discrete-uniform endpoints are evaluated by the reference PMF. Convergence is the best candidate's optim code (NA denotes a retained start). No global-optimality claim.", "", "Family seeds: 100000+100*family_index+n+replicate, alphabetic metadata order; edge seeds 200000+index; limit seeds 300000+index; oracle seeds 400000+100*index+dataset_index. Ties duplicate rounded pairs and preserve open support. Outliers on bounded support approach the upper endpoint instead of leaving support. Exact closed finite metadata bounds are tested, plus uniform and Zipf parameter-dependent endpoints. Open endpoints are not inserted. betaExtreme retains floating-point endpoints; betaClipped is the note's clipped control. Limit samples have n=200 before zero deflation. nearPareto reproduces the note's burr stress design.", "", "NOT_MAX compares oracle logLik with the fitter's stored logLik using 1e-4*(abs(logLik)+1). 'ref LL' independently evaluates the returned parameters. ESCAPE tests all large parameters, and small non-location, non-probability parameters. ERROR/BOOST include caught fitter diagnostics; synthetic ERROR also marks nonfinite returns, including legitimate -Inf likelihood at a zero-density support endpoint (Rayleigh), which is not by itself a fitter bug; oracle density exceptions/nonfinite values are infeasible candidates.", "")
table<-c("", "## All fits", "", "| id | dataset (n) | flags | estimates | LL | ref LL | oracle estimates | oracle LL | starts / conv | diagnostics |", "|---|---|---|---|---:|---:|---|---:|---|---|")
for(r in rows) table<-c(table,paste0("| ",paste(c(r$id,paste0(r$dataset," (",r$n,")"),r$flags,fmt(r$ours),num(r$ll),num(r$reference),fmt(r$oracle),num(r$oracle_ll),paste(r$starts,r$convergence,sep=" / "),clean(r$message)),collapse=" | ")," |"))
writeLines(c(header,summary,table),"dev/research/ml-behaviour-sweep.md")

cat("Rows:",length(rows),"; finite oracle:",sum(vapply(rows,function(r)is.finite(r$oracle_ll),TRUE)),"\n")
