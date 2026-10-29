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
   f <- get(paste0("d","pareto2"),asNamespace("actuar"))
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
 expect_error(mlpareto2(1:10,min=0,shape=2),"not supported")
 expect_error(mlpareto2(1:10,min=Inf),"finite")
})

test_that("pareto2: reference mapping and unbounded unrestricted path", {
 x <- c(1e-12,.01,.1,1,20,1e10)
 expect_lt(max(abs((actuar::dpareto2(x,0,.3,scale=2,log=TRUE))-(extraDistr::dlomax(x,.5,.3,log=TRUE)))),1e-12)
 y <- c(1,2,4,7)
 ll <- sapply(c(1e-3,1e-6,1e-9),function(e) sum(actuar::dpareto2(y,1-e,.1,scale=e,log=TRUE)))
 expect_true(all(diff(ll)>0))
})
