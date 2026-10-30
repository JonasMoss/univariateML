## Independent reference-density multistart probe. Run: Rscript .../oracle.R burr
## Optional second argument: comma-separated sample labels; third: CSV path.
suppressPackageStartupMessages(library(univariateML, lib.loc='/tmp/umvl-p0lib'))
suppressPackageStartupMessages(library(actuar))
suppressPackageStartupMessages(library(extraDistr))
args <- commandArgs(TRUE)
id <- if (length(args)) args[1] else 'burr'
spec <- list(
 burr=c(shape1=1,shape2=2,scale=1), invburr=c(shape1=1,shape2=2,scale=1),
 paralogis=c(shape=1,scale=1), invparalogis=c(shape=1,scale=1),
 trgamma=c(shape1=3,shape2=1.5,scale=2), invtrgamma=c(shape1=3,shape2=1.5,scale=.5),
 gb2=c(shape1=.3,shape2=2,shape3=.4,scale=1),
 genbeta=c(shape1=.4,shape2=.3,shape3=2,scale=1),
 fpareto=c(shape1=.3,shape2=2,shape3=.4,scale=1,min=0),
 pareto2=c(shape=.4,scale=1,min=0), pareto3=c(shape=2,scale=1,min=0),
 pareto4=c(shape1=.3,shape2=.7,scale=1,min=0),
 nsbeta=c(shape1=2,shape2=3,min=0,max=1))
stopifnot(id %in% names(spec))
defaults <- spec[[id]]; pars <- names(defaults)
dname <- if(id=='gb2') 'dtrbeta' else paste0('d',id)
dens <- if(id=='nsbeta') get(dname, asNamespace('extraDistr')) else get(dname, asNamespace('actuar'))
set.seed(791029)
samples <- lapply(c(2,3,5,20,100,1000), function(n) simulate_default(paste0('ml',id),n))
names(samples) <- paste0('n',c(2,3,5,20,100,1000))
samples$ties <- rep(samples$n20[1:10],each=2)
samples$constant <- rep(samples$n20[1],5)
samples$ties_half <- c(rep(min(samples$n20),10),samples$n20[11:20])
samples$ties_major <- c(rep(min(samples$n20),11),samples$n20[12:20])
## Every named proper face, with compatible held threshold values.
power <- function(n) runif(n)^(1/2)
pareto <- function(n) 1/power(n)
doublepareto <- function(n) exp(ifelse(runif(n)<.5,rexp(n)/2,-rexp(n)/2))
samples <- c(samples, switch(id,
 burr=list(weibull=rweibull(100,2),pareto=pareto(100)),
 invburr=list(invweibull=1/rweibull(100,2),power=power(100)),
 trgamma=list(lnorm=rlnorm(100),power=power(100)),
 invtrgamma=list(lnorm=rlnorm(100),pareto=pareto(100)),
 gb2=list(trgamma=actuar::rtrgamma(100,2,1.3,scale=1),
          invtrgamma=actuar::rinvtrgamma(100,2,1.3,scale=1),lnorm=rlnorm(100),
          pareto=pareto(100),power=power(100),doublepareto=doublepareto(100)),
 genbeta=list(unitgamma=exp(-rgamma(100,2,rate=3)),power=.9*power(100)),
 fpareto=list(trgamma=actuar::rtrgamma(100,2,1.3,scale=1),
          invtrgamma=actuar::rinvtrgamma(100,2,1.3,scale=1),lnorm=rlnorm(100),
          pareto=pareto(100),power=power(100),doublepareto=doublepareto(100)),
 pareto2=list(exponential=rexp(100)), pareto4=list(weibull=rweibull(100,2),pareto=pareto(100)),
 list()))
if(id %in% c('burr','invburr','trgamma','invtrgamma','gb2')) {
 samples$below <- c(.2,.3,.5,.7,.9)
 samples$above <- c(1.1,1.4,2,3,5)
 samples$cross <- c(.3,.8,1,1.4,2)
 samples$at <- c(.3,.8,1,1,1)
}
if(length(args)>1 && args[2]!='all') samples <- samples[strsplit(args[2],',',fixed=TRUE)[[1]]]
subsets <- unlist(lapply(0:(length(pars)-1),function(k) combn(pars,k,simplify=FALSE)),recursive=FALSE)
outfile <- if(length(args)>2) args[3] else ''
done <- if(nzchar(outfile) && file.exists(outfile)) read.csv(outfile,stringsAsFactors=FALSE) else NULL
results <- list(); j <- 0L
for (label in names(samples)) for (held in subsets) {
 if(!is.null(done) && any(done$sample==label & done$held==paste(held,collapse='+'))) next
 x <- samples[[label]]; free <- setdiff(pars,held)
 ## Held endpoints incompatible with a generated sample are intentionally reported.
 unpack <- function(z) {
  p <- defaults; names(z) <- free
  for(nm in free) p[nm] <- switch(nm,
   min=min(x)-exp(z[nm]), max=max(x)+exp(z[nm]),
   scale=if(id=='genbeta') max(x)+exp(z[nm]) else exp(z[nm]), exp(z[nm]))
  p
 }
 objective <- function(z) {
  p <- unpack(z)
  v <- suppressWarnings(tryCatch(do.call(dens,c(list(x=x),as.list(p),list(log=TRUE))),error=function(e) NA_real_))
  if(length(v)!=length(x) || any(!is.finite(v))) return(1e100)
  -sum(v)
 }
 initial <- sapply(free,function(nm) switch(nm,min=log(max(1e-3,min(x)-defaults[nm])),
  max=log(max(1e-3,defaults[nm]-max(x))),scale=if(id=='genbeta') log(max(1e-3,defaults[nm]-max(x))) else log(defaults[nm]),log(defaults[nm])))
 set.seed(791029+length(x)+sum(match(held,pars)))
 fits <- lapply(1:4,function(i) tryCatch(optim(initial+if(i==1) 0 else rnorm(length(free),0,1),objective,method='BFGS',control=list(maxit=400,reltol=1e-10)),error=function(e) NULL))
 vals <- sapply(fits,function(z) if(is.null(z)) 1e100 else z$value); best <- which.min(vals)
 fit <- tryCatch(do.call(get(paste0('ml',id)),c(list(x=x),as.list(defaults[held]))),error=function(e)e)
 ml <- if(inherits(fit,'error')) NA_real_ else as.numeric(logLik(fit))
 msg <- if(inherits(fit,'error')) conditionMessage(fit) else ''
 cf <- if(inherits(fit,'error')) numeric() else coef(fit)
 held_ok <- if(inherits(fit,'error')) NA else all(held %in% names(cf)) && all(abs(cf[held]-defaults[held]) < 1e-8)
 j <- j+1L; results[[j]] <- data.frame(family=id,sample=label,n=length(x),held=paste(held,collapse='+'),oracle=if(vals[best]<1e99) -vals[best] else NA_real_,master=ml,held_ok=held_ok,difference=ml+vals[best],message=msg,convergence=if(is.null(fits[[best]])) NA else fits[[best]]$convergence)
 if(nzchar(outfile)) write.table(results[[j]],outfile,sep=',',row.names=FALSE,col.names=!file.exists(outfile),append=file.exists(outfile),qmethod='double')
}
if(!nzchar(outfile) && length(results)) write.csv(do.call(rbind,results),stdout(),row.names=FALSE)
