test_that("fpareto: required endpoint, reference likelihood and reproducibility", {
 expect_error(mlfpareto(1:5), "unbounded")
 held <- list(min=0)
 set.seed(313)
 x <- actuar::qfpareto((1:400-.5)/400,0,.3,2,.4)
 fit <- do.call(mlfpareto, c(list(x),held))
 expect_equal(attr(fit,"fixed"), names(held))
 expect_equal(attr(fit,"logLik"), sum(dml(x,fit,log=TRUE)), tolerance=1e-10)
 expect_error(do.call(mlfpareto,c(list(c(0,x)),held)), "strictly")
 expect_error(do.call(mlfpareto,c(list(rep(.5,5)),held)))
 set.seed(17); a <- rml(20,fit)
 set.seed(17); expect_identical(rml(20,fit),a)
 expect_equal(pml(qml(c(.01,.3,.9),fit),fit),c(.01,.3,.9),tolerance=1e-12)
 # Slow independent likelihood optimization from the fitted point and perturbations.
 par <- setdiff(names(fit),names(held))
 objective <- function(z) {
   co <- as.list(as.numeric(fit)); names(co)<-names(fit)
   if(any(!is.finite(z)) || any(abs(z)>500)) return(1e100)
   co[par]<-as.list(exp(z))
   f <- get(paste0("d","fpareto"),asNamespace("actuar"))
   value <- suppressWarnings(-sum(do.call(f,c(list(x=x,log=TRUE),co))))
   if(is.finite(value)) value else 1e100
 }
 for(delta in c(-.2,0,.2)) {
   o <- optim(log(as.numeric(fit[par]))+delta,objective,method="BFGS",control=list(maxit=1000,reltol=1e-12))
   expect_lte(-o$value,attr(fit,"logLik")+1e-6*length(x))
 }
})

test_that("fpareto: held subsets map to GB2", {
 x<-actuar::qfpareto((1:150-.5)/150,2,.3,2,.4,scale=3)
 values<-list(shape1=.3,shape2=2,shape3=.4,scale=3)
 for(mask in 1:15) {
   held<-values[as.logical(intToBits(mask)[1:4])]
   a<-do.call(mlfpareto,c(list(x,min=2),held))
   b<-do.call(mlgb2,c(list(x-2),held))
   expect_equal(as.numeric(a[1:4]),as.numeric(b),tolerance=1e-10)
   expect_equal(attr(a,"logLik"),attr(b,"logLik"),tolerance=1e-10)
   expect_equal(sort(attr(a,"fixed")),sort(c("min",names(held))))
 }
 expect_error(mlfpareto(x,shape1=.3),"not supported")
 expect_error(mlfpareto(x,min=2,shape1=-1),"positive")
})

test_that("fpareto: all delegate escapes name the shifted fit", {
 u <- (1:200-.5)/200
 cases <- list(lnorm=qlnorm(u),trgamma=actuar::qtrgamma(u,.7,2),
   invtrgamma=actuar::qinvtrgamma(u,.7,2),pareto=(1-u)^(-1/2),power=u^(1/2),
   double=ifelse(u<.5,(2*u)^(1/2),(2*(1-u))^(-1/2)))
 for(id in names(cases)) {
   expect_error(mlfpareto(cases[[id]]+2,min=2),if(id=="double") "double Pareto" else
     paste0("ml",id,"\\(x - min\\)"),info=id)
 }
 expect_error(mlfpareto(actuar::qfpareto(u,0,.3,2,.4),min=0,iterlim=1),"unresolved")
})

test_that("fpareto: reference mapping and unbounded unrestricted path", {
 x <- c(1e-12,.01,.1,1,20,1e10)
 expect_lt(max(abs((actuar::dfpareto(x,0,.3,.7,.4,scale=2,log=TRUE))-(actuar::dtrbeta(x,.3,.7,.4,scale=2,log=TRUE)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(actuar::dfpareto(y,1-e,2,1,.5,scale=1,log=TRUE)))
 expect_true(all(diff(ll)>0))
})

test_that("fpareto: held scale preserves delegate endpoint-search outcomes", {
 u <- (1:200-.5)/200
 for(x in list((1-u)^(-1/2),sqrt(u))) {
   b <- try(mlgb2(x,scale=1,iterlim=1000),silent=TRUE)
   if(inherits(b,"try-error")) {
     pattern <- if(grepl("unresolved",as.character(b))) "unresolved" else
       "endpoint held|both endpoints held"
     expect_error(mlfpareto(x,min=0,scale=1),pattern)
   } else {
     a <- mlfpareto(x,min=0,scale=1)
     expect_equal(as.numeric(a[1:4]),as.numeric(b),tolerance=1e-10)
   }
 }
})


test_that("P2 fpareto holds agree with independent multistart reference likelihoods", {
  pars <- c(shape1=0.3, shape2=2, shape3=0.4, scale=1, min=0)
  thresholds <- c("min")
  regular <- setdiff(names(pars), thresholds)
  density <- actuar::dfpareto
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(actuar::qfpareto, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlfpareto, c(list(x=x),args)), error=identity)
    always <- all(c("shape2","scale") %in% held) || all(c("shape1","shape3") %in% held)
    if(always) expect_false(inherits(fit,"error"),info=paste(length(x),paste(held,collapse=",")))
    if (inherits(fit,"error")) {
      expect_match(conditionMessage(fit), "[Ll]imit|[Uu]se `ml|[Uu]nbounded|[Nn]o MLE|[Nn]o verified|unresolved|failed|does not exist|no held-endpoint Pareto fitter")
      next
    }
    expect_identical(unname(as.numeric(fit[held])),unname(pars[held]))
    expect_true(all(held %in% attr(fit,"fixed")))
    objective <- function(z) {
      p <- pars; p[free] <- exp(z)
      value <- suppressWarnings(-sum(do.call(density,c(list(x=x,log=TRUE),as.list(p)))))
      if(is.finite(value)) value else 1e100
    }
    actual <- attr(fit,"logLik")
    expect_equal(actual, -objective(log(fit[free])), tolerance=1e-7)
    if(length(free)) {
      # Starts depend only on reference parameters and observations.
      starts <- lapply(c(-1,0,1), function(s) log(pars[free])+s)
      oracle <- vapply(starts,function(z) {
        o <- suppressWarnings(nlminb(z,objective,control=list(iter.max=1000,eval.max=3000,rel.tol=1e-12)))
        -o$objective
      },0.)
      expect_gte(actual, max(oracle)-1e-6*length(x))
      expect_lte(actual, max(oracle)+1e-5*length(x))
    }
  }
})

test_that("P2 fpareto constant samples with coercive holds fit", {
  expect_true(is.finite(attr(mlfpareto(rep(2,5),shape1=2,shape2=3,scale=1,min=0),"logLik")))
  expect_true(is.finite(attr(mlfpareto(rep(2,5),shape3=2,shape2=3,scale=1,min=0),"logLik")))
  expect_true(is.finite(attr(mlfpareto(rep(2,5),shape1=2,shape3=3,scale=1,min=0),"logLik")))
})
