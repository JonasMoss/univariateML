test_that("zero knobs reproduce legacy fixtures and coefficient holds", {
  pairs <- list(pois=c("ztpois","zmpois","zip"),binom=c("ztbinom","zmbinom","zib"),
                nbinom=c("ztnbinom","zmnbinom","zinb"),geom=c("ztgeom","zmgeom"))
  for(id in names(pairs)) for(i in seq_along(pairs[[id]])) {
    old <- pairs[[id]][i]
    g <- jsonlite::fromJSON(file.path("fixtures",old,"golden.json"),simplifyVector=TRUE)
    args <- lapply(g$fit_args,function(v) if(is.character(v) && v %in% c("Inf","-Inf"))as.numeric(v) else v)
    if(is.null(args)) args <- list()
    zero <- c("truncated","modified","inflated")[i]
    legacy <- do.call(get(paste0("ml",old)),c(list(x=g$data),args))
    fit <- do.call(get(paste0("ml",id)),c(list(x=g$data,zero=zero),args))
    expect_equal(as.numeric(fit),as.numeric(legacy),tolerance=1e-10,info=old)
    expect_equal(as.numeric(logLik(fit)),as.numeric(logLik(legacy)),tolerance=1e-10)
    expect_identical(attr(fit,"zero"),zero)
    expect_equal(dml(g$data,fit,log=TRUE),dml(g$data,legacy,log=TRUE),tolerance=1e-10)
    expect_equal(pml(g$data,fit),pml(g$data,legacy),tolerance=1e-10)
    expect_equal(qml(c(.1,.5,.9),fit),qml(c(.1,.5,.9),legacy))
    set.seed(1);a<-rml(20,fit);set.seed(1);b<-rml(20,legacy);expect_equal(a,b)
    if(i==1) {
      window <- do.call(get(paste0("ml",id)),c(list(x=g$data,truncate=c(1,Inf)),args))
      expect_equal(as.numeric(window),as.numeric(fit),tolerance=1e-10)
    }
  }
})
test_that("modified laws contain the base law and truncated law", {
  x <- c(0,0,1,2,3,5)
  for(id in c("pois","geom","binom","nbinom")) {
    args <- if(id=="binom")list(size=10) else list()
    base <- do.call(get(paste0("ml",id)),c(list(x=x),args))
    held <- as.list(setNames(as.numeric(base),names(base)))
    p0 <- dml(0,base)
    fit <- do.call(get(paste0("ml",id)),c(list(x=x,zero="modified",p0=p0),held))
    expect_equal(as.numeric(logLik(fit)),sum(dml(x,base,log=TRUE)),tolerance=1e-10)
    pos <- x[x>0]
    a <- do.call(get(paste0("ml",id)),c(list(x=pos,zero="modified",p0=0),args))
    b <- do.call(get(paste0("ml",id)),c(list(x=pos,zero="truncated"),args))
    expect_equal(as.numeric(a[names(b)]),as.numeric(b),tolerance=1e-10)
  }
})
test_that("truncation is closed and honours holds and estimated bounds", {
  x <- c(1,1,2,2,3,4,5)
  a <- mlpois(x,truncate=c(1,5))
  expect_equal(sum(dml(1:5,a)),1,tolerance=1e-12)
  expect_equal(pml(c(0,5),a),c(0,1))
  expect_equal(qml(c(0,1),a),c(1,5))
  expect_equal(sum(dml(x,a,log=TRUE)),as.numeric(logLik(a)),tolerance=1e-10)
  b <- mlpois(x,truncate=c(NA,NA))
  expect_equal(as.numeric(a),as.numeric(b),tolerance=1e-10)
  expect_equal(attr(b,"truncate"),c(1,5))
  expect_error(mlpois(c(0,1),truncate=c(1,3)),"invalid truncation")
  expect_error(mlpois(x,zero="modified",truncate=c(1,5)),"zero \\+ truncate")
  expect_error(mlpois(x,zero="modified",p0=c(.1,.2)),"finite scalars")
  expect_error(mlpois(x,zero="nonsense"),"must be one of")
  n <- mlnorm(c(-1,-.2,.1,.3,.4,.9),truncate=c(-2,2))
  expect_equal(as.numeric(n),as.numeric(mltnorm(c(-1,-.2,.1,.3,.4,.9),a=-2,b=2)[c("mean","sd")]),tolerance=1e-10)
})
test_that("eligible unimplemented values have specific refusals", {
  for(name in names(univariateML:::univariateML_metadata)) {
    m <- univariateML:::univariateML_metadata[[name]]
    if(m$modifier_eligibility$zero) for(zero in c("truncated","modified","inflated"))
      if(!identical(m$modifiers$zero[[zero]]$status,"implemented"))
        expect_error(do.call(get(name),list(x=c(0,1,2),zero=zero)),paste0('zero = "',zero,'".*not implemented.*',name))
    if(!identical(m$modifiers$truncate$status,"implemented"))
      expect_error(do.call(get(name),list(x=c(1,2,3),truncate=c(0,Inf))),paste0('truncate.*not implemented.*',name))
  }
})
test_that("censoring reaches native fits and never fits recorded values", {
  x <- cbind(c(1,2,3,4),c(1,Inf,3,Inf))
  a <- mlexp(x)
  expect_equal(unname(a[["rate"]]),2/10)
  expect_equal(as.numeric(logLik(a)),2*log(.2)-2)
  expect_error(mlgamma(x),"censoring.*not implemented")
  expect_error(mlexp(cbind(c(0,1),c(2,3))),"censoring scheme or hold set.*not implemented")
  surv <- structure(cbind(c(1,2,3,4),c(1,0,1,0)),class="Surv",type="right")
  expect_equal(as.numeric(mlexp(surv)),as.numeric(a))
  expect_error(mlexp(structure(surv,type="counting")),"Surv type")
  expect_error(mlpois(x,zero="modified"),"censor \\+ zero")
  expect_error(bootstrapml(a,reps=2),"requires a censoring mechanism")
  set.seed(1)
  expect_length(bootstrapml(a,reps=2,type="nonparametric",reducer=identity),2)
  selected <- model_select(x,models=c("exp","gamma"))
  expect_true("mlgamma" %in% names(attr(selected,"skipped")))
  for(id in c("exp","weibull","rayleigh","pareto","lomax")) {
    args <- switch(id,pareto=list(b=1),lomax=list(lambda=1),list())
    fit <- do.call(get(paste0("ml",id)),c(list(x=x),args))
    expect_true(is.finite(as.numeric(logLik(fit))))
  }
})
test_that("ordinary truncation reproduces frozen t-family goldens", {
  for(old in c("tpois","tbinom","tnorm")) {
    g <- jsonlite::fromJSON(file.path("fixtures",old,"golden.json"))
    args <- lapply(g$fit_args,function(v) if(is.character(v) && v %in% c("Inf","-Inf"))as.numeric(v) else v)
    if(is.null(args)) args <- list()
    legacy <- do.call(get(paste0("ml",old)),c(list(x=g$data),args))
    a <- unname(legacy[["a"]]);b <- unname(legacy[["b"]])
    if(old!="tnorm")a<-floor(a)+1
    args$a <- NULL;args$b <- NULL
    fit <- do.call(get(paste0("ml",sub("^t","",old))),c(list(x=g$data,truncate=c(a,b)),args))
    wanted <- legacy[names(fit)]
    expect_equal(as.numeric(fit),as.numeric(wanted),tolerance=1e-10,info=old)
    expect_equal(as.numeric(logLik(fit)),as.numeric(logLik(legacy)),tolerance=1e-10)
    expect_equal(dml(g$data,fit,log=TRUE),dml(g$data,legacy,log=TRUE),tolerance=1e-10)
  }
})
test_that("fixed modifier evaluation keeps small upper tails", {
  f <- mlnorm(mean=0,sd=1,truncate=c(30,31))
  expect_true(is.finite(dml(30.5,f,log=TRUE)))
  expect_equal(pml(qml(.9,f),f),.9,tolerance=1e-10)
  expect_equal(pml(qml(-10,f,lower.tail=FALSE,log.p=TRUE),f,lower.tail=FALSE,log.p=TRUE),-10,tolerance=1e-10)
  z <- mlpois(lambda=2,zero="modified",p0=.3)
  expect_equal(dml(0,z),.3)
  expect_equal(qml(c(0,1),z),c(0,Inf))
  expect_true(all(rml(20,z)>=0))
  for(id in c("geom","nbinom")) {
    args <- if(id=="nbinom")list(size=2,prob=.3) else list(prob=.3)
    f <- do.call(get(paste0("ml",id)),c(args,list(truncate=c(2,7))))
    expect_equal(sum(dml(2:7,f)),1,tolerance=1e-12)
    expect_equal(qml(c(0,1),f),c(2,7))
  }
})
test_that("geometric inflation and fixed atom holds are exposed", {
  x <- c(0,0,0,1,2,4)
  a <- mlgeom(x,zero="inflated")
  b <- mlzinb(x,size=1)
  expect_equal(as.numeric(a),as.numeric(b[c("prob","pi")]),tolerance=1e-10)
  expect_equal(dml(0:4,a),dml(0:4,b),tolerance=1e-10)
  f <- mlgeom(prob=.4,zero="inflated",pi=.2)
  expect_equal(dml(0,f),.2+.8*.4,tolerance=1e-12)
  expect_error(vcov(a),"modified or censored")
})
test_that("exact-only interval matrices reduce to vector fits", {
  for(name in c("mlexp","mlgamma","mlnorm","mlpois")) {
    x <- c(1,2,3,4)
    expect_equal(as.numeric(do.call(get(name),list(cbind(x,x)))),as.numeric(do.call(get(name),list(x))),tolerance=1e-10)
  }
})
test_that("positive-truncation boundary extensions agree in both spellings", {
  a <- mlpois(rep(1,4),zero="truncated")
  b <- mlpois(rep(1,4),truncate=c(1,Inf))
  expect_equal(as.numeric(a),as.numeric(b))
  expect_equal(dml(0:3,a),dml(0:3,b))
  a <- mlpois(lambda=0,zero="truncated")
  b <- mlpois(lambda=0,truncate=c(1,Inf))
  expect_equal(dml(0:3,a),dml(0:3,b))
  expect_error(mlpois(lambda=1,truncate=c(.1,.2)),"zero or unresolved probability")
})
test_that("every declared unimplemented modifier/hold cell refuses explicitly", {
  cells <- read.csv(file.path("fixtures","capabilities.csv"),stringsAsFactors=FALSE,na.strings=NULL)
  cells <- cells[cells$status=="not-implemented" & cells$modifier!="none",]
  for(i in seq_len(nrow(cells))) {
    cell<-cells[i,];name<-paste0("ml",cell$family)
    meta<-univariateML:::univariateML_metadata[[name]]
    params<-c(meta$names,if(cell$modifier=="zero=modified")"p0" else if(cell$modifier=="zero=inflated")"pi")
    held<-if(nzchar(cell$held))strsplit(cell$held,";",fixed=TRUE)[[1]] else character()
    args<-setNames(rep(list(NULL),length(params)),params)
    values<-setNames(as.list(meta$simulate),meta$names)
    values$p0<-.2;values$pi<-.2
    for(p in held)args[p]<-values[p]
    if(startsWith(cell$modifier,"zero=")) {
      args$zero<-sub("^zero=","",cell$modifier);data<-c(1,2,3)
    } else if(startsWith(cell$modifier,"truncate=")) {
      args$truncate<-c(1,5);data<-c(1,2,3)
    } else {
      scheme<-sub("^censor=","",cell$modifier)
      data<-switch(scheme,right=cbind(c(1,2),c(1,Inf)),left=cbind(c(-Inf,-Inf),c(1,2)),interval=cbind(c(1,2),c(2,3)))
    }
    expect_error(do.call(get(name),c(list(x=data),args)),"not implemented",info=paste(name,cell$modifier,cell$held))
  }
})
test_that("unsupplied fixed-model atom coefficients use their simulation defaults", {
  f<-mlpois(lambda=2,zero="modified")
  expect_true(is.finite(f[["p0"]]))
  expect_equal(sum(dml(0:100,f)),1,tolerance=1e-12)
})
test_that("estimated window bounds retain refit instructions and degrees of freedom", {
  f<-mlpois(c(1,1,2,3,4),truncate=c(NA,NA))
  expect_equal(attr(f,"truncate_spec"),c(NA_real_,NA_real_))
  expect_equal(attr(f,"truncate_estimated"),c(TRUE,TRUE))
  expect_equal(attr(logLik(f),"df"),3)
  expect_output(print(f),"Truncate")
})
test_that("numerical windows expose work controls on closed-form parents", {
  x<-c(1,1,2,3,5)
  expect_true(is.finite(mlpois(x,truncate=c(1,5),iterlim=200,reltol=1e-10)[["lambda"]]))
  expect_error(mlpois(x,truncate=c(1,5),iterlim=1),"failed to certify")
  expect_error(mlpois(x,truncate=c(1,5),reltol=0),"invalid modifier controls")
})
test_that("finite-window probability-zero faces stop for the mathematical reason", {
  expect_error(mlgeom(c(2,3,4),truncate=c(1,4)),"upper boundary limit")
  expect_error(mlnbinom(c(2,3,4),size=1,truncate=c(1,4)),"upper boundary limit")
})
test_that("window quantile endpoints intersect the parent support", {
  for(id in c("geom","nbinom")) {
    args<-if(id=="geom")list(prob=.3) else list(size=2,prob=.3)
    f<-do.call(get(paste0("ml",id)),c(args,list(truncate=c(-5,5))))
    expect_equal(qml(c(0,1),f),c(0,5))
    args$prob<-1
    point<-do.call(get(paste0("ml",id)),c(args,list(truncate=c(-Inf,Inf))))
    expect_equal(qml(c(0,1),point),c(0,0))
  }
})
test_that("an unfinished optional boundary calculation does not suppress a certified root", {
  x<-c(1,1,2,3,5)
  fit<-mlnbinom(x,size=2,truncate=c(1,1e8),iterlim=50)
  positive<-mlnbinom(x,size=2,zero="truncated")
  expect_equal(as.numeric(fit),as.numeric(positive),tolerance=1e-10)
  expect_equal(as.numeric(logLik(fit)),as.numeric(logLik(positive)),tolerance=1e-10)
  expect_true(isTRUE(attr(fit,"certified")))
})
test_that("generic conditional tails preserve tiny log complements and refuse unresolved normalization", {
  f<-mlgeom(prob=.3,truncate=c(0,Inf))
  expected<-stats::pgeom(250,.3,log.p=TRUE)
  observed<-pml(250,f,log.p=TRUE)
  expect_lt(observed,0)
  expect_equal(observed/expected,1,tolerance=1e-12)
  for(id in c("geom","nbinom")) {
    args<-if(id=="geom")list(prob=.3) else list(size=2,prob=.3)
    expect_error(do.call(get(paste0("ml",id)),c(args,list(truncate=c(1e9,1e9+10)))),"normalization.*floating-point accuracy")
  }
})
test_that("generic quantiles resolve endpoint atoms and refuse ill-conditioned transforms", {
  f<-mlgeom(prob=1e-9,truncate=c(1e9,Inf))
  expect_equal(qml(log(1e-100),f,log.p=TRUE),1e9)
  expect_error(qml(log(2e-9),f,log.p=TRUE),"quantile.*floating-point accuracy")
  expect_error(mlgeom(prob=1e-100,truncate=c(1e100,Inf)),"integer window.*floating-point accuracy")
})
test_that("retirement replacements cover free binomial size and logarithmic mapping", {
  x<-c(1,1,2,3,3)
  for(prob in list(NULL,.4)) {
    old<-mltbinom(x,a=0,b=5,size=NULL,prob=prob)
    new<-mlbinom(x,size=NULL,prob=prob,truncate=c(1,5))
    expect_equal(as.numeric(new),as.numeric(old[c("size","prob")]),tolerance=1e-10)
    expect_equal(attr(new,"certified"),attr(old,"certified"))
    expect_equal(as.numeric(logLik(new)),as.numeric(logLik(old)),tolerance=1e-10)
  }
  for(theta in list(NULL,0,.4)) {
    x<-if(identical(theta,0))c(0,0,1,1) else c(0,1,1,2,4)
    a<-mllgser(x,zero="modified",theta=theta)
    b<-mlzmlogarithmic(x,prob=theta)
    expect_equal(as.numeric(a),as.numeric(b),tolerance=1e-10)
    expect_equal(dml(0:5,a),dml(0:5,b),tolerance=1e-10)
  }
  a<-mllgser(theta=.4,p0=.2,zero="modified")
  expect_equal(dml(0,a),.2)
  expect_equal(attr(a,"support")[[1]],0)
})
test_that("finite zeta windows support every real exponent", {
  x<-c(1,2,3,4,4,4)
  a<-mlzeta(x,truncate=c(1,4))
  expect_lt(a[["s"]],0)
  expect_equal(sum(dml(1:4,a)),1,tolerance=1e-12)
  expect_equal(sum(dml(x,a,log=TRUE)),as.numeric(logLik(a)),tolerance=1e-10)
  expect_equal(qml(c(0,1),a),c(1,4))
  expect_true(all(rml(10,a) %in% 1:4))
  for(s in c(-100,-2,0,.5,1,2,100)) {
    f<-mlzeta(s=s,truncate=c(1,4))
    w<-exp(-s*log(1:4)-max(-s*log(1:4)));w<-w/sum(w)
    expect_equal(dml(1:4,f),w,tolerance=1e-12)
    expect_equal(pml(1:4,f),cumsum(w),tolerance=1e-12)
  }
  expect_error(mlzeta(c(4,4),truncate=c(1,4)),"point-mass")
  expect_error(mlzeta(s=0),"must be in")
})
test_that("retirement replay preserves fully held density routes and controls", {
  x<-c(1000000,1000001,1000003,1000008,1000011)
  for(pair in list(c("zinb","nbinom","inflated"),c("zmnbinom","nbinom","modified"),c("ztnbinom","nbinom","truncated"))) {
    held<-list(size=2,prob=.4)
    if(pair[3]=="inflated")held$pi<-.2
    if(pair[3]=="modified")held$p0<-.2
    old<-do.call(get(paste0("ml",pair[1])),c(list(x=x),held))
    new<-do.call(get(paste0("ml",pair[2])),c(list(x=x,zero=pair[3]),held))
    expect_equal(as.numeric(new),as.numeric(old),tolerance=1e-10)
    expect_equal(as.numeric(logLik(new)),as.numeric(logLik(old)),tolerance=1e-10)
    expect_identical(attr(new,"certified"),attr(old,"certified"))
  }
  old<-tryCatch(mlztbinom(1:3,size=NULL,reltol=1e-18),error=identity)
  new<-tryCatch(mlbinom(1:3,size=NULL,zero="truncated",reltol=1e-18),error=identity)
  expect_equal(new$kind,old$kind)
  f<-mlzeta(x,truncate=c(1,NA))
  expect_lt(f[["s"]],0)
  expect_equal(sum(dml(x,f,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-10)
})
test_that("finite zeta reaches the integer endpoint and preserves tiny log complements", {
  N<-.Machine$integer.max
  f<-mlzeta(c(N-1,N),truncate=c(1,NA))
  expect_lt(f[["s"]],0)
  expect_equal(sum(dml(c(N-1,N),f,log=TRUE)),as.numeric(logLik(f)),tolerance=1e-10)
  fixed<-mlzeta(s=-2,truncate=c(1,N))
  expected<-log1p(-6/(N*(N+1)*(2*N+1)))
  expect_lt(pml(1,fixed,lower.tail=FALSE,log.p=TRUE),0)
  expect_equal(pml(1,fixed,lower.tail=FALSE,log.p=TRUE)/expected,1,tolerance=1e-10)
  prefix<-mlzeta(s=-2,truncate=c(-Inf,4.9))
  expect_equal(dml(1:4,prefix),(1:4)^2/30,tolerance=1e-12)
  expect_warning(mlzeta(c(1,1),truncate=c(1,NA)),"not unique")
})
test_that("binomial window work controls reach both size routes", {
  x<-c(1,1,2,3,3)
  expect_error(mlbinom(x,size=NULL,truncate=c(1,5),iterlim=1),"budget|accuracy|precision")
  expect_error(mlbinom(x,size=12,truncate=c(1,5),iterlim=1),"budget|accuracy|precision")
  f<-mlbinom(x,size=NULL,truncate=c(1,5),iterlim=512)
  expect_equal(as.numeric(f),as.numeric(mltbinom(x,a=0,b=5,size=NULL)[c("size","prob")]),tolerance=1e-10)
})
test_that("duplicate modifier holds are rejected", {
  expect_error(mlpois(c(0,1,2),zero="modified",p0=.2,p0=.3),"Duplicate")
})

test_that("finite zeta windows keep huge held exponents and tiny log normalisers", {
  fit<-mlzeta(c(1,1),s=1e308,truncate=c(1,.Machine$integer.max))
  expect_equal(unname(fit[["s"]]),1e308)
  expect_equal(as.numeric(logLik(fit)),0)
  tiny<-mlzeta(c(1,1),s=100,truncate=c(1,.Machine$integer.max))
  expect_lt(abs(as.numeric(logLik(tiny))/(-2*2^-100)-1),1e-10)
})

test_that("fully held windows evaluate density without an optimizer work budget", {
  huge<-mlpois(c(0,0),lambda=1e300,truncate=c(0,Inf),iterlim=1)
  expect_equal(as.numeric(logLik(huge)),-2e300)
  tiny<-mlpois(c(1,1),lambda=1e-300,truncate=c(0,Inf),iterlim=1)
  expect_equal(as.numeric(logLik(tiny)),2*log(1e-300))
  held<-mlbinom(c(0,1),size=5,prob=.3,truncate=c(0,5),iterlim=1)
  expect_equal(as.numeric(logLik(held)),sum(dbinom(c(0,1),5,.3,log=TRUE)))
})

test_that("fixed finite zeta models validate their window at construction", {
  expect_error(mlzeta(s=-1,truncate=c(2,4)),"finite zeta window")
  expect_error(mlzeta(s=-1,truncate=c(1,Inf)),"finite zeta window")
  expect_error(mlzeta(s=-1,truncate=c(0,0)),"finite zeta window")
})

test_that("finite zeta statistics retain accuracy for large uniform histograms", {
  x<-rep(1:4,each=1000000L)
  fit<-mlzeta(x,truncate=c(1,4))
  expect_lte(abs(fit[["s"]]),1e-10)
  expect_equal(as.numeric(logLik(fit)),-length(x)*log(4),tolerance=1e-10)
})

test_that("unbounded window evaluation preserves the settled conditional kernels", {
  x<-1:4
  cases<-list(
    list(old=mltpois(x,a=-1,b=Inf),fit=mlpois(x,truncate=c(0,Inf)),name="mlpois",parents="lambda",window=c(0,Inf),points=0:10),
    list(old=mltbinom(x,a=-1,b=Inf,size=10),fit=mlbinom(x,size=10,truncate=c(0,Inf)),name="mlbinom",parents=c("size","prob"),window=c(0,Inf),points=0:10),
    list(old=mltnorm(c(-1,-.3,.2,.8,1),a=-Inf,b=Inf),fit=mlnorm(c(-1,-.3,.2,.8,1),truncate=c(-Inf,Inf)),name="mlnorm",parents=c("mean","sd"),window=c(-Inf,Inf),points=seq(-2,2,length.out=11)))
  for(c in cases) {
    values<-stats::setNames(as.list(as.numeric(c$old[c$parents])),c$parents)
    fixed<-do.call(get(c$name),c(values,list(truncate=c$window)))
    for(model in list(c$fit,fixed)) {
      expect_equal(dml(c$points,model,log=TRUE),dml(c$points,c$old,log=TRUE),tolerance=1e-10)
      expect_equal(pml(c$points,model,lower.tail=FALSE,log.p=TRUE),pml(c$points,c$old,lower.tail=FALSE,log.p=TRUE),tolerance=1e-10)
      expect_equal(qml(c(.01,.5,.99),model),qml(c(.01,.5,.99),c$old),tolerance=1e-10)
    }
  }
  point<-mlpois(lambda=0,truncate=c(0,Inf))
  expect_equal(dml(0:1,point),c(1,0))
  expect_error(mlpois(lambda=0,truncate=c(1,5)),"median quantile evaluation failed")
  expect_error(mlbinom(size=5,prob=0,truncate=c(1,5)),"median quantile evaluation failed")
})

test_that("finite zeta normalizer bounds cross integer powers without refusals", {
  N<-3000L
  for(r in c(7,7+.Machine$double.eps*4,8,8+.Machine$double.eps*8)) {
    fit<-mlzeta(c(N-1L,N),s=-r,truncate=c(1,N))
    reference<-r*log1p(-1/N)-2*log(sum(((1:N)/N)^r))
    expect_equal(as.numeric(logLik(fit)),reference,tolerance=1e-10)
    expect_true(is.finite(qml(.5,fit)))
  }
})
test_that("explicit 512 work units retain the controlled binomial window solver", {
  for(limit in c(400,511,512,513)) {
    f<-mlbinom(0:3,size=1e308,truncate=c(0,Inf),iterlim=limit)
    expect_equal(unname(f[["prob"]])/(1.5e-308),1,tolerance=1e-10)
    expect_equal(as.numeric(logLik(f)),6*log(1.5)-6-lgamma(1)-lgamma(2)-lgamma(3)-lgamma(4),tolerance=1e-10)
  }
})
test_that("finite zeta score roots justify very small requested tolerances", {
  fit<-tryCatch(mlzeta(1:4,truncate=c(1,4),reltol=1e-20),error=identity)
  if(inherits(fit,"error"))expect_match(conditionMessage(fit),"accuracy")
  else expect_lte(abs(unname(fit[["s"]])),1e-20)
  held<-mlzeta(1:4,s=0,truncate=c(1,4),reltol=1e-20)
  expect_identical(unname(held[["s"]]),0)
})
