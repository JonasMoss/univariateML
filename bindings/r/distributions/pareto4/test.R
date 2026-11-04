test_that("pareto4: required endpoint, reference likelihood and reproducibility", {
 expect_error(mlpareto4(1:5), "unbounded")
 held <- list(min=0)
 set.seed(313)
 x <- ml_simulate("mlpareto4", 300)
 fit <- do.call(mlpareto4, c(list(x),held))
 expect_equal(attr(fit,"fixed"), names(held))
 expect_equal(attr(fit,"logLik"), sum(dml(x,fit,log=TRUE)), tolerance=1e-10)
 expect_error(do.call(mlpareto4,c(list(c(0,x)),held)), "strictly")
 expect_error(do.call(mlpareto4,c(list(rep(.5,5)),held)))
 set.seed(17); a <- rml(20,fit)
 set.seed(17); expect_identical(rml(20,fit),a)
 expect_equal(pml(qml(c(.01,.3,.9),fit),fit),c(.01,.3,.9),tolerance=1e-12)
 # Slow independent likelihood optimization from the fitted point and perturbations.
 par <- setdiff(names(fit),names(held))
 objective <- function(z) {
   co <- as.list(as.numeric(fit)); names(co)<-names(fit)
   if(any(!is.finite(z)) || any(abs(z)>500)) return(1e100)
   co[par]<-as.list(exp(z))
   f <- get(paste0("d","pareto4"), asNamespace("univariateML"))
   value <- suppressWarnings(-sum(do.call(f,c(list(x=x,log=TRUE),co))))
   if(is.finite(value)) value else 1e100
 }
 for(delta in c(-.2,0,.2)) {
   o <- optim(log(as.numeric(fit[par]))+delta,objective,method="BFGS",control=list(maxit=1000,reltol=1e-12))
   expect_lte(-o$value,attr(fit,"logLik")+1e-6*length(x))
 }
})

test_that("pareto4: fixed scale and shifted escape messages", {
 x <- qpareto4(p = (1:100 - 0.5)/100, shape1 = 0.5, shape2 = 2, scale = 3, min = 2)
 a <- mlpareto4(x,min=2,scale=3); b <- mlburr(x-2,scale=3,abstol=1e-10,iterlim=1000)
 expect_equal(as.numeric(a[1:3]),as.numeric(b),tolerance=1e-10)
 expect_equal(attr(a,"fixed"),c("min","scale"))
 expect_error(mlpareto4(x,scale=3),"not supported")
 expect_error(mlpareto4(3:10,min=2,scale=1),"endpoint held at min .* scale")
})

test_that("pareto4: free-scale faces preserve the threshold", {
 u <- (1:200-.5)/200
 expect_error(mlpareto4(2+sqrt(-log(1-u)),min=2),"mlweibull\\(x - min\\)")
 expect_error(mlpareto4(2+1/sqrt(1-u),min=2),"mlpareto\\(x - min\\)")
})

test_that("pareto4: reference mapping and unbounded unrestricted path", {
 x <- c(1e-12,.01,.1,1,20,1e10)
 expect_lt(max(abs((dpareto4(x = x, shape1 = 0.3, shape2 = 0.7, scale = 2, min = 0, log = TRUE))-(dburr(x = x, shape1 = 0.3, shape2 = 0.7, scale = 2, log = TRUE)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(dpareto4(x = y, shape1 = 2, shape2 = 0.5, scale = 1, min = 1 - e, log = TRUE)))
 expect_true(all(diff(ll)>0))
})


test_that("P2 pareto4 holds agree with independent multistart reference likelihoods", {
  skip_unless_tier("full")
  pars <- c(shape1=0.3, shape2=0.7, scale=1, min=0)
  thresholds <- c("min")
  regular <- setdiff(names(pars), thresholds)
  density <- dpareto4
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qpareto4, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlpareto4, c(list(x=x),args)), error=identity)
    always <- "shape1" %in% held || all(c("shape2","scale") %in% held)
    if(always) expect_false(inherits(fit,"error"),info=paste(length(x),paste(held,collapse=",")))
    if (inherits(fit,"error")) {
      expect_match(conditionMessage(fit), "[Ll]imit|[Uu]se `ml|[Uu]nbounded|[Nn]o MLE|[Nn]o verified|unresolved|failed|does not exist")
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

test_that("P2 pareto4 validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlpareto4(c(.2,.3,.5), min=0, shape1=v),"positive|finite")
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlpareto4(c(.2,.3,.5), min=0, shape2=v),"positive|finite")
})

 test_that("P2 pareto4 constant samples respect the held tangent", {
   x <- rep(2,5)
   for(args in list(list(shape1=2,shape2=3),list(shape2=3,scale=1),list(shape1=2,scale=1),list(shape1=2,shape2=3,scale=1))) {
     f <- do.call(mlpareto4,c(list(x=x, min=0),args))
     expect_true(is.finite(attr(f,"logLik")))
     expect_identical(unname(as.numeric(f[names(args)])),unname(unlist(args)))
   }
   expect_error(mlpareto4(rep(1,5), min=0,shape1=2,scale=1),"unbounded")
   expect_error(mlpareto4(x, min=0,shape1=2),"unbounded")
 })

 test_that("pareto4 preserves certificate refusal diagnostics", {
  x <- exp(seq(-1e-6,1e-6,length.out=20))
  expect_error(mlpareto4(x,min=0), "certificate budget exhausted; no verified MLE")
  x <- c(.2,.5,1,2,4)
  expect_error(mlpareto4(x,min=0,iterlim=2), "certificate numerical failure.*no verified MLE")
  x <- c(0.0049057434842350416,0.042501201178458653,0.19481544379842536,0.29869422522389177,0.40386521559557592,0.49096570363940406,0.5326256684997116,0.72206471601197653,0.74104603671600355,0.82688168528714312,0.98659907210500519,1.1056048590023038,1.140076235502067,1.1606952121643113,1.4300263464615877,1.4793399841671804,1.5655241685783525,1.6390163836698108,2.0838622791695847,5.1042846241626947)
  expect_error(mlpareto4(x,min=0), "certificate unresolved.*sign-uncertain seam; no verified MLE")
  expect_error(mlpareto4(rep(2,3),min=0), "unbounded.*constant sample; no MLE")
})

test_that("pareto4 refuses excess overflow and artificial ties", {
  expect_error(mlpareto4(c(1,.Machine$double.xmax), min=-.Machine$double.xmax), "certificate numerical failure; no verified MLE")
  expect_error(mlpareto4(c(1,1+.Machine$double.eps,2), min=-1e100), "certificate numerical failure; no verified MLE")
})
