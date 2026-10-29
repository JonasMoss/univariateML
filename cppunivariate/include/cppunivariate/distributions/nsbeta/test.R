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
 expect_error(mlnsbeta(x,min=2,max=5,shape1=2),"not supported")
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
