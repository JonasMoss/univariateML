# Run after just test has built univariateML-r, from the repository root.
options(pkg.build_extra_flags=FALSE)
Sys.setenv(PKG_BUILD_EXTRA_FLAGS='false')
pkgload::load_all('univariateML-r', quiet=TRUE, compile=FALSE, recompile=FALSE)
ns <- asNamespace('univariateML')
directory <- 'univariateML-cpp/include/cppunivariate/distributions'
here <- 'dev/research/evaluations/2026-11-11-quantile-rule-data'
decisions <- list(); counts <- list(); total <- 0L; forward_failures <- 0L
for (folder in sort(list.dirs(directory, recursive=FALSE))) {
  id <- basename(folder)
  if (!exists(paste0('q',id), ns, inherits=FALSE) ||
      any(grepl('type: integer',readLines(file.path(folder,'README.md'))))) next
  t <- jsonlite::fromJSON(file.path(folder,'truth.json'), simplifyVector=FALSE)
  counts[[id]] <- 0L
  for (index in seq_along(t$quantiles)) {
    row <- vapply(t$quantiles[[index]],as.numeric,numeric(1))
    total <- total+1L
    p <- setNames(lapply(t$parameters[[row[1]+1L]],as.numeric),unlist(t$parameter_names))
    args <- c(p,list(lower.tail=as.logical(row[2]),log.p=as.logical(row[3])))
    a <- suppressWarnings(do.call(get(paste0('q',id),ns),c(list(row[4]),args)))
    b <- row[5]
    ok <- a==b || is.finite(a) && is.finite(b) && abs(a-b)<=max(1e-9*abs(b),row[9]) ||
      if(row[8]!=0) a==row[6] || a==row[7] else a>=row[6] && a<=row[7]
    if (!isTRUE(ok)) forward_failures <- forward_failures+1L
    if (isTRUE(ok) || !is.finite(a) || !is.finite(b) ||
        !isTRUE(if(row[3]!=0) is.finite(row[4]) && row[4]<0 else row[4]>0 && row[4]<1)) next
    at <- do.call(get(paste0('p',id),ns),c(list(a),args))
    scale <- if(row[3]!=0) max(1,abs(row[4])) else abs(row[4])
    if (is.finite(at) && abs(at-row[4])<=1e-12*scale) {
      counts[[id]] <- counts[[id]]+1L
      decisions[[length(decisions)+1L]] <- list(family=id,index=index-1L,
        row=t$quantiles[[index]],parameters=unname(p),actual=a,cdf=at)
    }
  }
}
jsonlite::write_json(decisions,file.path(here,'r-decisions.json'),auto_unbox=TRUE,pretty=TRUE,digits=17L)
jsonlite::write_json(list(total_rows=total,families=counts,forward_failures=forward_failures,backward_accepted=length(decisions)),
                     file.path(here,'r-counts.json'),auto_unbox=TRUE,pretty=TRUE,digits=17L)
cat('R rows',total,'accepted',length(decisions),'\n')
row <- list(0,1,1,-7.3890561137087625,-1.9991476033510408592152e-18,
            -1.999147603351041e-18,-1.9991476033510405e-18,0,0)
parameters <- list(1e9,-999999998,1e9)
a <- get('qfrechet',ns)(row[[4]],1e9,-999999998,1e9,lower.tail=TRUE,log.p=TRUE)
at <- get('pfrechet',ns)(a,1e9,-999999998,1e9,lower.tail=TRUE,log.p=TRUE)
jsonlite::write_json(list(list(family='frechet',index='regression probe (not a truth row)',
  row=row,parameters=parameters,actual=a,cdf=at)),file.path(here,'r-probe-decisions.json'),
  auto_unbox=TRUE,pretty=TRUE,digits=17L)
# In this jsonlite version digits=NA retains only 15 significant digits.
# Verify the actual probability input and both observed outputs round-trip.
saved <- jsonlite::fromJSON(file.path(here,'r-probe-decisions.json'),simplifyVector=FALSE)[[1L]]
stopifnot(identical(vapply(saved$row,as.numeric,numeric(1)),vapply(row,as.numeric,numeric(1))),
          identical(as.numeric(saved$actual),as.numeric(a)),
          identical(as.numeric(saved$cdf),as.numeric(at)))
