# Modifier routing; keep default wrappers and their diagnostics unchanged.
ml_modifier_error <- function(name, value) {
  stop(value, " is not implemented for `", name,
       "`; see TASK-191 or its successor", call. = FALSE)
}
ml_interval_data <- function(x, na.rm = FALSE) {
  if (inherits(x, "Surv")) {
    type <- attr(x, "type")
    if (!identical(type, "right") || NCOL(x) != 2L)
      stop("Surv type is not implemented; supply a two-column interval matrix", call. = FALSE)
    z <- unclass(x)
    if (any(!is.na(z[,2]) & !z[,2] %in% c(0,1))) stop("Invalid Surv status")
    x <- cbind(z[,1], ifelse(z[,2] == 1, z[,1], Inf))
  }
  if (!is.numeric(x) || !is.matrix(x) || ncol(x) != 2L)
    stop("Censored data require a numeric two-column interval matrix")
  if (na.rm) x <- x[complete.cases(x), , drop = FALSE]
  if (!nrow(x) || anyNA(x) || any(x[,1] > x[,2]) ||
      any(x[,1] == x[,2] & !is.finite(x[,1]))) stop("Invalid interval observations")
  x
}
ml_zero_variant <- function(id, zero) {
  if (zero == "truncated") return(paste0("zt", id))
  if (zero == "modified") return(if (id == "lgser") "zmlogarithmic" else paste0("zm",id))
  if (zero == "inflated") return(switch(id, pois="zip", binom="zib", nbinom="zinb", geom="zinb"))
  NULL
}
ml_modified_call <- function(name, x, frozen, na.rm, dots, zero, truncate, call) {
  meta <- univariateML_metadata[[name]]; id <- sub("^ml", "", name)
  if(length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots)))))
    stop("Modifier holds and controls must be named")
  if(anyDuplicated(names(dots)))stop("Duplicate coefficient or control arguments")
  if (!is.character(zero) || length(zero)!=1L || is.na(zero) ||
      !zero %in% c("none","truncated","modified","inflated"))
    stop('`zero` must be one of none, truncated, modified, inflated')
  if(is.logical(truncate) && length(truncate)==2L && all(is.na(truncate)))truncate<-as.numeric(truncate)
  truncate_spec <- truncate
  window <- !is.null(truncate)
  if (window && zero != "none") ml_modifier_error(name,"zero + truncate")
  censored <- !frozen && ((is.matrix(x) && NCOL(x)==2L) || inherits(x,"Surv"))
  if(censored) {
    x <- ml_interval_data(x,na.rm)
    if(all(x[,1]==x[,2])) {
      x <- x[,1];censored <- FALSE
      if(zero=="none" && !window) return(do.call(get(name),c(list(x=x,na.rm=na.rm),dots)))
    }
  }
  if(!censored && zero=="none" && !window)
    return(do.call(get(name),c(if(frozen)list() else list(x=x,na.rm=na.rm),dots)))
  if (censored && zero != "none") ml_modifier_error(name,"censor + zero")
  if (censored && window) ml_modifier_error(name,"censor + truncate")
  if (censored && !identical(meta$modifiers$censor$right$status,"certified"))
    ml_modifier_error(name,"censoring")
  if (zero != "none" && !identical(meta$modifiers$zero[[zero]]$status,"implemented"))
    ml_modifier_error(name,paste0('`zero = "',zero,'"`'))
  if (window && !identical(meta$modifiers$truncate$status,"implemented")) ml_modifier_error(name,"`truncate`")
  if (window && (!is.numeric(truncate) || length(truncate)!=2L ||
      any(is.nan(truncate)) || (!anyNA(truncate) && truncate[1]>truncate[2])))
    stop("`truncate` must be a closed window c(lower, upper); NA estimates a bound")
  if(window && id=="zeta" &&
     ((!is.na(truncate[1]) && max(1,ceiling(truncate[1]))!=1) ||
      (!is.na(truncate[2]) && (!is.finite(truncate[2]) || floor(truncate[2])<1 || floor(truncate[2])>.Machine$integer.max))))
    stop("finite zeta window requires [1,N], integer N in [1,INT_MAX]")
  extra <- if (zero=="modified") "p0" else if (zero=="inflated") "pi" else character()
  modifier_controls<-if(window || censored)c("reltol","iterlim") else
    intersect(c("reltol","iterlim"),names(formals(get(paste0("ml",ml_zero_variant(id,zero),"_")))))
  ml_check_args(get(paste0(name,"_")), dots[setdiff(names(dots),c(extra,modifier_controls))], name)
  for(control in intersect(names(dots),c("reltol","abstol","iterlim"))) {
    v<-dots[[control]]
    if(!is.numeric(v) || length(v)!=1L || !is.finite(v) || v<=0 ||
       control=="iterlim" && (v!=floor(v) || v>.Machine$integer.max))
      stop("invalid modifier controls")
  }
  for(p in intersect(names(dots),c(meta$names,extra))) {
    v<-dots[[p]]
    if(!is.null(v) && (!is.numeric(v) || length(v)!=1L || !is.finite(v)))
      stop("held coefficients must be finite scalars")
  }
  if (!frozen) {
    absent <- setdiff(meta$required,names(dots))
    if (length(absent)) stop(name," requires `", absent[1], "`; hold a value or supply NULL")
    if (censored) {
      x <- ml_interval_data(x,na.rm)
      # An interval matrix of exact observations reduces to the ordinary API.
      if (all(x[,1]==x[,2])) return(do.call(get(name),c(list(x=x[,1],na.rm=na.rm),dots)))
    } else x <- ml_check_modify(x,na.rm,if(id=="lgser" && zero=="modified")"mlzmlogarithmic" else name)
  }
  if (window && anyNA(truncate)) {
    if (frozen) stop("A fixed model requires known truncation bounds")
    truncate[is.na(truncate)] <- range(x)[is.na(truncate)]
  }
  if(window && meta$support$type=="Z" && ceiling(truncate[1])>floor(truncate[2]))
    stop("Truncation event has zero or unresolved probability")
  alias <- window && identical(as.numeric(truncate),c(1,Inf)) && id %in% c("pois","binom","nbinom","geom")
  if (frozen) {
    if (zero != "none" || alias) {
      variant <- ml_zero_variant(id,if(alias)"truncated" else zero)
      if (id=="geom" && zero=="inflated") dots$size <- 1
      if(id=="lgser") {if("theta" %in% names(dots))dots["prob"]<-dots["theta"];dots$theta<-NULL}
      out <- ml_fixed_model(paste0("ml",variant),dots,call,TRUE)
      if (id=="geom" && zero=="inflated") out <- out[names(out)!="size"]
    } else if(id=="zeta") {
      s<-if(!"s" %in% names(dots))meta$simulate[[1]] else dots$s
      if(!is.numeric(s) || length(s)!=1L || !is.finite(s))stop("s must be a finite scalar")
      out<-c(s=s)
    } else out <- ml_fixed_model(name,dots,call,TRUE)
    # Rebuild metadata after construction of the modifier's coefficient vector.
    values <- as.numeric(out); names(values) <- names(out)
    if(id=="lgser")names(values)[names(values)=="prob"]<-"theta"
    fixed <- names(values)
    obj <- univariateML_construct(values,name,list(call=deparse(call),n=0,logLik=NA_real_),fixed=fixed)
    attr(obj,"frozen") <- TRUE
  } else {
    holds <- dots[intersect(names(dots),c(meta$names,extra))]
    tol <- if (!is.null(dots$reltol))dots$reltol else if(!is.null(dots$abstol))dots$abstol else 1e-10
    maxit <- if (is.null(dots$iterlim)) if(window && !alias && id=="binom" && is.null(holds$size))512L else if((window && !alias) || censored)400L else 200L else dots$iterlim
    fully_held<-all(vapply(c(meta$names,extra),function(p)!is.null(holds[[p]]),logical(1)))
    if(fully_held && (zero!="none" || alias)) {
      variant<-ml_zero_variant(id,if(alias)"truncated" else zero)
      legacy_args<-holds
      if(id=="lgser") {legacy_args$prob<-legacy_args$theta;legacy_args$theta<-NULL}
      if(id=="geom" && zero=="inflated")legacy_args$size<-1
      legacy<-do.call(get(paste0("ml",variant)),c(list(x=x),legacy_args))
      estimates<-as.numeric(legacy);names(estimates)<-names(legacy)
      if(id=="geom" && zero=="inflated")estimates<-estimates[names(estimates)!="size"]
      if(id=="lgser")names(estimates)[names(estimates)=="prob"]<-"theta"
      out<-list(estimates=estimates,logLik=as.numeric(logLik(legacy)),fixed=c(meta$names,extra),restricted=attr(legacy,"restricted"))
      if(alias && id=="binom" && anyNA(truncate_spec))out$certified<-TRUE
    } else out <- ml_modifier_(id,if(censored) x[,1] else x,if(censored) x[,2] else numeric(),
      holds,zero,window,if(window && !is.na(truncate_spec[1])) truncate[1] else NaN,if(window && !is.na(truncate_spec[2])) truncate[2] else NaN,censored,tol,maxit)
    if (inherits(out,"univariateML_stop")) stop(out)
    if(fully_held && window && !anyNA(truncate_spec))attr(out$estimates,"certified")<-NULL
    obj <- univariateML_construct(out$estimates,name,list(logLik=out$logLik,call=deparse(call),n=if(censored)nrow(x) else length(x)),
      fixed=out$fixed,restricted=out$restricted,data=x)
    if (!is.null(out$certified) && !(fully_held && window && !anyNA(truncate_spec))) attr(obj,"certified") <- out$certified
  }
  # Constructor obtains base coefficient names from metadata; add the atom's name.
  names(obj) <- c(meta$names,extra)
  attr(obj,"zero") <- zero;attr(obj,"truncate") <- truncate
  if(window) {
    attr(obj,"truncate_spec")<-truncate_spec
    attr(obj,"truncate_estimated")<-is.na(truncate_spec)
  }
  support <- univariateML_support(obj)$bounds
  if(id=="lgser" && zero=="modified")support[1]<-0
  if(zero=="truncated")support[1]<-max(1,support[1])
  if(window) {
    support <- c(max(support[1],truncate[1]),min(support[2],truncate[2]))
    if(!isTRUE(attr(obj,"continuous")))support<-c(ceiling(support[1]),floor(support[2]))
  }
  attr(obj,"support")<-support
  if(censored) attr(obj,"censored") <- TRUE
  if(zero!="none" || alias) attr(obj,"modifier_variant") <- ml_zero_variant(id,if(alias)"truncated" else zero)
  else if(window && id %in% c("pois","binom","norm"))attr(obj,"modifier_variant")<-paste0("t",id)
  if(frozen && window)ml_modifier_function(obj,"d")
  obj
}
ml_modifier_function <- function(obj,type) {
  if(univariateML_name(obj)=="mlzeta" && !is.null(attr(obj,"truncate"))) {
    window<-attr(obj,"truncate");window<-c(max(1,ceiling(window[1])),floor(window[2]));s<-unname(obj[["s"]])
    if(window[1]!=1 || !is.finite(window[2]) || window[2]<1 || window[2]!=floor(window[2]) || window[2]>.Machine$integer.max)
      stop("finite zeta window requires [1,N], integer N in [1,INT_MAX]")
    evaluate<-function(x,lower=TRUE,log=FALSE)ml_zeta_window_(type,x,window[2],s,lower,log)
    return(switch(type,d=function(x,log=FALSE)evaluate(x,TRUE,log),
      p=function(q,lower.tail=TRUE,log.p=FALSE)evaluate(q,lower.tail,log.p),
      q=function(p,lower.tail=TRUE,log.p=FALSE)evaluate(p,lower.tail,log.p),
      r=function(n)ml_zeta_window_("q",runif(n),window[2],s,TRUE,FALSE)))
  }
  variant <- attr(obj,"modifier_variant")
  if (!is.null(variant)) {
    name <- univariateML_name(obj)
    values <- stats::setNames(as.list(as.numeric(obj)),names(obj))
    if(name=="mllgser") {values$prob<-values$theta;values$theta<-NULL}
    if(name=="mlgeom" && attr(obj,"zero")=="inflated") values$size <- 1
    if(variant %in% c("tpois","tbinom","tnorm")) {
      window<-attr(obj,"truncate")
      values$a<-if(variant=="tnorm")window[1] else ceiling(window[1])-1
      values$b<-if(variant=="tnorm")window[2] else floor(window[2])
    }
    if(variant %in% c("tpois","tbinom","tnorm") && any(is.infinite(window))) {
      # The closed-window API admits infinite design bounds. Parent
      # coefficients were validated at construction or by the fitter; check
      # joint constraints with the settled evaluator, without the retired
      # wrapper's finite-only design-domain check.
      alt_name<-paste0("ml",variant);alt_names<-univariateML_metadata[[alt_name]]$names
      alt<-univariateML_construct(unlist(values[alt_names]),alt_name,
        list(n=0,call="modifier"),fixed=alt_names)
      median<-suppressWarnings(qml(.5,alt))
      if(length(median)!=1L || is.na(median))stop(structure(
        list(message=paste0("`",name,"`: median quantile evaluation failed: invalid held coefficients or numerical failure"),call=NULL,stage="median"),
        class=c("univariateML_evaluation_error","error","condition")))
      attr(alt,"frozen")<-TRUE
    } else alt <- ml_fixed_model(paste0("ml",variant),values,quote(modifier),TRUE)
    return(univariateML_to_function(alt,type))
  }
  window <- attr(obj,"truncate")
  base <- obj;attr(base,"truncate") <- NULL
  d <- univariateML_to_function(base,"d");p <- univariateML_to_function(base,"p");q <- univariateML_to_function(base,"q")
  integer <- !isTRUE(attr(obj,"continuous"))
  physical<-univariateML_support(base)$bounds
  if(integer && max(physical[1],ceiling(window[1]))>=2^53)
    stop("Conditional integer window cannot be resolved at the required floating-point accuracy")
  a <- if(integer) max(physical[1],ceiling(window[1]))-1 else max(physical[1],window[1])
  b <- if(integer) min(physical[2],floor(window[2])) else min(physical[2],window[2])
  upper_endpoint<-b
  if(!is.finite(b) && integer && "prob" %in% names(base) &&
     (base[["prob"]]==1 || "size" %in% names(base) && base[["size"]]==0))upper_endpoint<-0
  complement<-function(v)ifelse(v<log(.5),log1p(-exp(v)),log(-expm1(v)))
  diff <- function(big,small) { z <- big+complement(pmin(0,small-big));z[is.infinite(small)&small<0] <- big[is.infinite(small)&small<0];z }
  mass <- function(lo,hi) {
    flo <- p(lo,log.p=TRUE);fhi <- p(hi,log.p=TRUE)
    slo <- p(lo,lower.tail=FALSE,log.p=TRUE);shi <- p(hi,lower.tail=FALSE,log.p=TRUE)
    use_lower<-fhi<log(.5)
    big<-ifelse(use_lower,fhi,slo);small<-ifelse(use_lower,flo,shi)
    if(any(hi>lo & is.finite(big) & is.finite(small) & small-big > -1e-4))
      stop("Conditional interval probability cannot be resolved at the required floating-point accuracy")
    ifelse(use_lower,diff(fhi,flo),diff(slo,shi))
  }
  z <- mass(a,b)
  if(!is.finite(z)) stop("Truncation event has zero or unresolved probability")
  if(64*.Machine$double.eps*max(1,abs(z))>1e-10)
    stop("Conditional normalization cannot be resolved at the required floating-point accuracy")
  dfun <- function(x,log=FALSE) {
    v <- d(x,log=TRUE)-z;v[x<window[1] | x>window[2]] <- -Inf
    if(log)v else exp(v)
  }
  pfun <- function(q,lower.tail=TRUE,log.p=FALSE) {
    k <- if(integer) floor(q) else q;k <- pmax(a,pmin(b,k))
    lc<-mass(a,k)-z;ls<-mass(k,b)-z
    from_lower<-lc<=ls
    v<-if(lower.tail)ifelse(from_lower,lc,complement(ls)) else ifelse(from_lower,complement(lc),ls)
    v[k<=a] <- if(lower.tail)-Inf else 0;v[k>=b] <- if(lower.tail)0 else -Inf
    if(log.p)v else exp(v)
  }
  add <- function(a,b) { m <- pmax(a,b); v <- m+log1p(exp(pmin(a,b)-m));v[is.infinite(m)&m<0] <- -Inf;v }
  qfun <- function(p,lower.tail=TRUE,log.p=FALSE) {
    lp <- if(log.p)p else log(p)
    ls <- if(log.p)complement(p) else log1p(-p)
    if(!lower.tail){tmp<-lp;lp<-ls;ls<-tmp}
    fa<-pml(a,base,log.p=TRUE);fb<-pml(b,base,log.p=TRUE)
    sa<-pml(a,base,lower.tail=FALSE,log.p=TRUE);sb<-pml(b,base,lower.tail=FALSE,log.p=TRUE)
    lf<-add(fa,lp+z);sf<-add(sb,ls+z)
    endpoint<-!is.na(lp) & !is.na(ls) & (lp==-Inf | ls==-Inf)
    at_min<-!is.na(lp) & !endpoint & lp<=d(a+1,log=TRUE)-z
    at_max<-rep(FALSE,length(lp))
    if(is.finite(b))at_max<-!is.na(ls) & !endpoint & ls<d(b,log=TRUE)-z
    use_lower<-lf<log(.5)
    prefix<-ifelse(use_lower,ifelse(lp<=ls,fa,fb),ifelse(ls<=lp,sb,sa))
    unresolved<-is.finite(prefix) & pmin(lp,ls)+z-prefix<log(1e-4)
    remaining<-!is.na(lp) & !is.na(ls) & !endpoint & !at_min & !at_max
    if(any(unresolved & remaining,na.rm=TRUE))
      stop("Conditional quantile cannot be resolved at the required floating-point accuracy")
    v<-rep(NaN,length(p));v[is.na(p)]<-p[is.na(p)]
    low<-remaining & !is.na(use_lower) & use_lower
    high<-remaining & !is.na(use_lower) & !use_lower
    v[low]<-q(lf[low],log.p=TRUE)
    v[high]<-q(sf[high],lower.tail=FALSE,log.p=TRUE)
    v[at_min]<-a+1;v[at_max]<-b
    v[lp==-Inf] <- if(integer)a+1 else a;v[ls==-Inf] <- upper_endpoint
    v
  }
  switch(type,d=dfun,p=pfun,q=qfun,r=function(n)qfun(stats::runif(if(length(n)>1)length(n) else n)))
}

ml_print_modifiers <- function(object) {
  zero<-attr(object,"zero")
  if(!is.null(zero) && zero!="none")cat("Zero: ",zero,"\n",sep="")
  window<-attr(object,"truncate")
  if(!is.null(window))cat("Truncate: [",format(window[1]),", ",format(window[2]),"]\n",sep="")
  if(isTRUE(attr(object,"censored")))cat("Observations: right censored\n")
  invisible(NULL)
}
