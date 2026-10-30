test_that("pareto2: required endpoint, reference likelihood and reproducibility", {
 expect_error(mlpareto2(1:5), "unbounded")
 held <- list(min=0)
 set.seed(313)
 x <- simulate_default("mlpareto2", 300)
 fit <- do.call(mlpareto2, c(list(x),held))
 expect_equal(attr(fit,"fixed"), names(held))
 expect_equal(attr(fit,"logLik"), sum(dml(x,fit,log=TRUE)), tolerance=1e-10)
 expect_error(do.call(mlpareto2,c(list(c(0,x)),held)), "strictly")
 expect_error(do.call(mlpareto2,c(list(rep(.5,5)),held)))
 set.seed(17); a <- rml(20,fit)
 set.seed(17); expect_identical(rml(20,fit),a)
 expect_equal(pml(qml(c(.01,.3,.9),fit),fit),c(.01,.3,.9),tolerance=1e-12)
 # Slow independent likelihood optimization from the fitted point and perturbations.
 par <- setdiff(names(fit),names(held))
 objective <- function(z) {
   co <- as.list(as.numeric(fit)); names(co)<-names(fit)
   if(any(!is.finite(z)) || any(abs(z)>500)) return(1e100)
   co[par]<-as.list(exp(z))
   f <- get(paste0("d","pareto2"), asNamespace("univariateML"))
   value <- suppressWarnings(-sum(do.call(f,c(list(x=x,log=TRUE),co))))
   if(is.finite(value)) value else 1e100
 }
 for(delta in c(-.2,0,.2)) {
   o <- optim(log(as.numeric(fit[par]))+delta,objective,method="BFGS",control=list(maxit=1000,reltol=1e-12))
   expect_lte(-o$value,attr(fit,"logLik")+1e-6*length(x))
 }
})

test_that("pareto2: shifted exponential escape and unsupported subsets", {
 expect_error(mlpareto2(1:10,min=0),"mlexp\\(x - min\\)")
 expect_error(mlpareto2(1:10,shape=2),"not supported")
 expect_identical(unname(mlpareto2(1:10,min=0,shape=2)["shape"]),2)
 expect_error(mlpareto2(1:10,min=Inf),"finite")
})

test_that("pareto2: reference mapping and unbounded unrestricted path", {
 x <- c(1e-12,.01,.1,1,20,1e10)
 expect_lt(max(abs((dpareto2(x = x, shape = 0.3, scale = 2, min = 0, log = TRUE))-(dlomax(x = x, lambda = 0.5, kappa = 0.3, log = TRUE)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(dpareto2(x = y, shape = 0.1, scale = e, min = 1 - e, log = TRUE)))
 expect_true(all(diff(ll)>0))
})


test_that("P2 pareto2 holds agree with independent multistart reference likelihoods", {
  pars <- c(shape=0.4, scale=1, min=0)
  thresholds <- c("min")
  regular <- setdiff(names(pars), thresholds)
  density <- dpareto2
  samples <- lapply(c(3, 20, 100), function(n)
    do.call(qpareto2, c(list(p = (seq_len(n)-.5)/n), as.list(pars))))
  samples[[4]] <- rep(samples[[2]][seq(1, 20, 2)], each=2)
  # These samples deliberately include proper limit-model data.
  samples[[5]] <- qexp(((1:20)-.5)/20)
  samples[[6]] <- c(.3,.8,1,1,1)*pars["scale"]
  samples[[7]] <- c(1.1,1.4,2,3,5)*pars["scale"]
  for (x in samples) for (mask in seq_len(2^length(regular)-1)) {
    held <- c(thresholds, regular[as.logical(intToBits(mask)[seq_along(regular)])])
    free <- setdiff(names(pars), held)
    args <- as.list(pars[held])
    fit <- tryCatch(do.call(mlpareto2, c(list(x=x),args)), error=identity)
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

test_that("P2 pareto2 validates new held values", {
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlpareto2(c(.2,.3,.5), min=0, shape=v),"positive|finite")
  for (v in c(0,-1,Inf,NA_real_)) expect_error(mlpareto2(c(.2,.3,.5), min=0, scale=v),"positive|finite")
})

 test_that("P2 pareto2 constant data with held shape fit", {
   f <- mlpareto2(rep(2,5),min=0,shape=2)
   expect_identical(unname(f["shape"]),2)
   expect_true(is.finite(attr(f,"logLik")))
   f <- mlpareto2(rep(2,5),min=0,shape=2,scale=1)
   expect_identical(unname(f["scale"]),1)
 })
