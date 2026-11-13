# Retained likelihood regressions use private kernel adapters, never retired public wrappers.
modifier_kernel <- function(id) {
  if(id=="zipf")return(function(x,na.rm=FALSE,N=NULL,s=NULL,...) {
    if(missing(x))return(mlzeta(s=if(is.null(s))1 else s,truncate=c(1,if(is.null(N))10 else N)))
    mlzeta(x,na.rm=na.rm,s=s,truncate=c(1,if(is.null(N))NA else N),...)
  })
  name<-paste0(".modifier_",id)
  adapter<-get(paste0("modifier_",id,"_"),asNamespace("univariateML"))
  function(x,na.rm=FALSE,...) {
    dots<-list(...)
    univariateML:::ml_check_args(adapter,dots,name)
    if(missing(x))return(univariateML:::ml_fixed_model(name,dots,match.call(),TRUE))
    meta<-univariateML:::ml_law_metadata(name)
    x<-univariateML:::ml_check_modify(x,na.rm,name)
    if(all(vapply(meta$names,function(p)!is.null(dots[[p]]),logical(1))))
      return(univariateML:::ml_fixed_model(name,dots,match.call(),FALSE,x))
    missing<-setdiff(meta$required,names(dots))
    if(length(missing))stop(name," requires `",missing[1],"`; hold a value or supply NULL")
    out<-do.call(adapter,c(list(x=x),dots))
    if(inherits(out,"univariateML_stop"))stop(out)
    univariateML:::univariateML_construct(out$estimates,name,list(logLik=out$logLik,n=length(x),call=deparse(match.call())),fixed=out$fixed,restricted=out$restricted,data=x)
  }
}
.modifier_tbinom <- modifier_kernel("tbinom")
.modifier_tnorm <- modifier_kernel("tnorm")
.modifier_tpois <- modifier_kernel("tpois")
.modifier_zipf <- modifier_kernel("zipf")
.modifier_zib <- modifier_kernel("zib")
.modifier_zinb <- modifier_kernel("zinb")
.modifier_zip <- modifier_kernel("zip")
.modifier_zmbinom <- modifier_kernel("zmbinom")
.modifier_zmgeom <- modifier_kernel("zmgeom")
.modifier_zmlogarithmic <- modifier_kernel("zmlogarithmic")
.modifier_zmnbinom <- modifier_kernel("zmnbinom")
.modifier_zmpois <- modifier_kernel("zmpois")
.modifier_ztbinom <- modifier_kernel("ztbinom")
.modifier_ztgeom <- modifier_kernel("ztgeom")
.modifier_ztnbinom <- modifier_kernel("ztnbinom")
.modifier_ztpois <- modifier_kernel("ztpois")

ml_simulate <- function(dens,n) {
  fun<-get(dens,mode="function")
  rml(n,fun())
}

# Preserve both the warning assertion and the evaluated value across testthat editions.
kernel_warning_value <- function(code,regexp) {
  value<-NULL
  testthat::expect_warning(value<-code,regexp)
  value
}
