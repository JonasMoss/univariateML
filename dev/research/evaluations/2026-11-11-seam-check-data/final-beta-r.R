options(pkg.build_extra_flags = FALSE)
Sys.setenv(PKG_BUILD_EXTRA_FLAGS="false", UMVL_TEST_TIER="dev", TESTTHAT_CPUS="1",
           UMVL_DISTRIBUTIONS_DIR=file.path(getwd(),"univariateML-cpp/include/cppunivariate/distributions"))
testthat::test_local("univariateML-r",filter="^dist-(beta|invgauss)$",stop_on_failure=TRUE)
pkgload::load_all("univariateML-r",quiet=TRUE)
x <- c(0.2499999999995,0.24999999999975,0.25,0.25000000000025,0.25000000000075)
fit <- univariateML::mlbeta(x)
point <- 135.27888712858683
value <- as.numeric(logLik(fit))
density <- sum(univariateML::dml(x,fit,log=TRUE))
record <- list(R=R.version.string,data=x,parameters=as.numeric(fit),
               reported=value,stats_dbeta=density,independent_point=point,
               stats_relative_error=abs(density-point)/max(1,abs(point)))
writeLines(jsonlite::toJSON(record,auto_unbox=TRUE,digits=17,pretty=TRUE),
  "dev/research/evaluations/2026-11-11-seam-check-data/r-beta-density.json")
