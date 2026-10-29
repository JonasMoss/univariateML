test_that("nsbeta: required endpoint, reference likelihood and reproducibility", {
 expect_error(mlnsbeta(1:5), "unbounded")
 held <- list(min=0,max=1)
 set.seed(313)
 x <- simulate_default("mlnsbeta", 300)
 fit <- do.call(mlnsbeta, c(list(x),held))
 expect_equal(attr(fit,"fixed"), names(held))
 expect_equal(attr(fit,"logLik"), sum(dml(x,fit,log=TRUE)), tolerance=1e-10)
 expect_error(do.call(mlnsbeta,c(list(c(0,x)),held)), "strictly")
 expect_error(do.call(mlnsbeta,c(list(rep(.5,5)),held)))
 set.seed(17); a <- rml(20,fit)
 set.seed(17); expect_identical(rml(20,fit),a)
 expect_equal(pml(qml(c(.01,.3,.9),fit),fit),c(.01,.3,.9),tolerance=1e-12)
 # Slow independent likelihood optimization from the fitted point and perturbations.
 par <- setdiff(names(fit),names(held))
 objective <- function(z) {
   co <- as.list(as.numeric(fit)); names(co)<-names(fit)
   if(any(!is.finite(z)) || any(abs(z)>500)) return(1e100)
   co[par]<-as.list(exp(z))
   f <- get(paste0("d","nsbeta"),asNamespace("extraDistr"))
   value <- suppressWarnings(-sum(do.call(f,c(list(x=x,log=TRUE),co))))
   if(is.finite(value)) value else 1e100
 }
 for(delta in c(-.2,0,.2)) {
   o <- optim(log(as.numeric(fit[par]))+delta,objective,method="BFGS",control=list(maxit=1000,reltol=1e-12))
   expect_lte(-o$value,attr(fit,"logLik")+1e-6*length(x))
 }
})

test_that("held bounds survive selection and bootstrap", {
 x <- extraDistr::qnsbeta((1:100-.5)/100,2,3,2,5)
 fit <- mlnsbeta(x,min=2,max=5)
 expect_equal(attr(logLik(fit),"df"),2)
 set.seed(19)
 b <- bootstrapml(fit,reps=3,map=function(z) z["min"],reducer=identity)
 expect_equal(as.numeric(b),rep(2,3))
 selected <- model_select(x,models="nsbeta",min=2,max=5)
 expect_equal(attr(selected,"fixed"),c("min","max"))
 expect_error(model_select(x,models="nsbeta"),"unbounded")
 expect_identical(unname(mlnsbeta(x,min=2,max=5,shape1=2)["shape1"]),2)
 expect_error(mlnsbeta(x,shape1=2),"not supported")
 expect_error(mlnsbeta(x,min=5,max=2),"ordered")
 expect_error(mlnsbeta(x,min=2,max=Inf),"finite")
})

test_that("nsbeta: reference mapping and unbounded unrestricted path", {
 x <- seq(.01,.99,length.out=50)
 expect_lt(max(abs((extraDistr::dnsbeta(2+3*x,2,3,2,5,log=TRUE))-(dbeta(x,2,3,log=TRUE)-log(3)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(extraDistr::dnsbeta(y,.5,2,1-e,8,log=TRUE)))
 expect_true(all(diff(ll)>0))
})


test_that("P2 nsbeta holds agree with independent multistart reference likelihoods", {
  pars <- c(shape1=2, shape2=3, min=0, max=1)
  thresholds <- c("min", "max")
  regular <- setdiff(names(pars), thresholds)
  density <- extraDistr::dnsbeta
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(extraDistr::qnsbeta, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qbeta(((1:20)-.5)/20,2,3)
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlnsbeta, c(list(x=x),args)), error=identity)
    always <- TRUE
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

test_that("P2 nsbeta validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlnsbeta(c(.2,.3,.5), min=0, max=1, shape1=v),"positive|finite")
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlnsbeta(c(.2,.3,.5), min=0, max=1, shape2=v),"positive|finite")
})

test_that("P2 nsbeta constant samples with coercive holds fit", {
  expect_true(is.finite(attr(mlnsbeta(rep(.4,5),min=0,max=1,shape1=2),"logLik")))
  expect_true(is.finite(attr(mlnsbeta(rep(.4,5),min=0,max=1,shape2=3),"logLik")))
  expect_true(is.finite(attr(mlnsbeta(rep(.4,5),min=0,max=1,shape1=2,shape2=3),"logLik")))
})
