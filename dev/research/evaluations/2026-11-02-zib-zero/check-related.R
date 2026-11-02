# From repository root, after just regen; read-only checks of other families.
options(pkg.build_extra_flags=FALSE)
pkgload::load_all('univariateML-r', quiet=TRUE)
x <- rep(0L,3)
check <- function(label, thunk) {
  cat(label, ': ')
  tryCatch(withCallingHandlers({
    f <- thunk()
    cat(paste(names(f), as.numeric(f), collapse=', '), '; logLik=',attr(f,'logLik'),'\n')
  }, warning=function(w) {cat('warning=',conditionMessage(w),'; ');invokeRestart('muffleWarning')}),
  error=function(e) cat('STOP ',conditionMessage(e),'\n'))
}
for(p in list(NULL,0,.5,1)) {
  check(paste('binom prob',p),function() mlbinom(x,size=NULL,prob=p))
  check(paste('ztbinom prob',p),function() mlztbinom(x,size=NULL,prob=p))
  for(mass in list(NULL,0,.2,1))
    check(paste('zmbinom prob',p,'p0',mass),function() mlzmbinom(x,size=NULL,prob=p,p0=mass))
  for(bounds in list(c(-1,0),c(-1,5),c(0,5)))
    check(paste('tbinom prob',p,'bounds',paste(bounds,collapse=',')),
          function() mltbinom(x,size=NULL,prob=p,a=bounds[1],b=bounds[2]))
}
for(a in list(NULL,2)) for(b in list(NULL,3))
  check(paste('bbinom alpha',a,'beta',b),function() mlbbinom(x,size=NULL,alpha=a,beta=b))
