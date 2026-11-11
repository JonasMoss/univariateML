# Run after targeted-r.sh has assembled and compiled the current package.
options(pkg.build_extra_flags=FALSE)
Sys.setenv(PKG_BUILD_EXTRA_FLAGS='false')
pkgload::load_all('univariateML-r',quiet=TRUE)
out <- list()
for (id in c('std','lst')) {
  g <- jsonlite::fromJSON(file.path('univariateML-cpp/include/cppunivariate/distributions',id,'golden.json'))
  nuisance <- if (id=='std') c('mean','sd') else c('mu','sigma')
  fun <- get(paste0('ml',id),asNamespace('univariateML'))
  for (held in list(nuisance[1],nuisance[2],nuisance)) {
    elapsed <- system.time(f <- do.call(fun,c(list(x=g$data,iterlim=200),g$estimates[held])))[['elapsed']]
    stopifnot(inherits(f,'univariateML'),all(is.finite(f)),identical(attr(f,'certified'),length(held)==2L))
    expected <- unlist(g$estimates[names(f)])
    relative <- abs(as.numeric(f)-expected)/pmax(abs(expected),1e-10)
    stopifnot(all(relative<=3e-5),as.numeric(logLik(f))>=g$logLik-1e-6*length(g$data))
    out[[length(out)+1L]] <- list(family=id,held=held,iterlim=200,certified=attr(f,'certified'),coefficients=stats::setNames(as.list(as.numeric(f)),names(f)),logLik=as.numeric(logLik(f)),maximum_relative_golden_error=max(relative),seconds=elapsed)
  }
}
cat(jsonlite::toJSON(out,auto_unbox=TRUE,pretty=TRUE,digits=17),'\n')
