stats_route_ids <- c("beta", "gamma", "hyper", "lnorm", "cauchy", "logis", "unif",
                     "exp", "binom", "pois", "weibull", "geom", "nbinom", "norm")

test_that("all former stats routes use core d/p/q and preserve sampler streams", {
  for (id in umvl_filter_families(stats_route_ids)) {
    g <- jsonlite::fromJSON(file.path(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"), id, "golden.json"))
    fit <- do.call(get(paste0("ml", id)), c(list(as.numeric(g$data)), golden_fit_args(id)))
    expect_lte(abs(sum(dml(g$data, fit, log = TRUE)) - as.numeric(logLik(fit))) /
                 max(1, abs(as.numeric(logLik(fit)))), 1e-10)
    archived <- fit; attr(archived,"density") <- paste0("stats::d",id)
    expect_identical(univariateML_to_string(archived,"d"),paste0("core_d",id))
    expect_equal(dml(g$data,archived,log=TRUE),dml(g$data,fit,log=TRUE))
    args <- as.list(coef(fit))
    ref_args <- args
    if (id == "hyper") ref_args <- list(m=args$m, n=args$N-args$m, k=args$k)
    for (kind in c("d", "p", "q")) {
      expect_identical(univariateML_to_string(fit, kind), paste0("core_", kind, id))
      core <- getFromNamespace(paste0("core_", kind, id), "univariateML")
      ref <- get(paste0(kind, id), asNamespace("stats"))
      at <- if (kind == "q") c(0, .001, .1, .5, .9, .999, 1) else
        c(-Inf, -1, 0, .5, 1, 2, 10, Inf)
      for (lower in c(TRUE, FALSE)) for (logged in c(TRUE, FALSE)) {
        flags <- if (kind == "d") list(log=logged) else list(lower.tail=lower, log.p=logged)
        input <- if (kind == "q" && logged) log(at) else at
        got <- suppressWarnings(do.call(core, c(list(input), args, flags)))
        expected <- suppressWarnings(do.call(ref, c(list(input), ref_args, flags)))
        # R 4.6.1 qhyper(-Inf, log.p=TRUE) returns NaN; the core returns
        # the support endpoint, as its independent truth table requires.
        if (id=="hyper" && kind=="q" && logged)
          expected[is.infinite(input) & input<0] <- do.call(ref,c(list(0),ref_args,list(lower.tail=lower)))
        expect_equal(got, expected, tolerance=1e-10, info=paste(id, kind, lower, logged))
      }
      for (input in list(c(a=0,b=1),matrix(c(0,1),nrow=1,dimnames=list("row",c("a","b"))))) {
        expect_identical(attributes(suppressWarnings(do.call(core,c(list(input),args)))),
                         attributes(suppressWarnings(do.call(ref,c(list(input),ref_args)))),info=id)
      }
      for (bad in list(factor(1), 1+1i, as.raw(1), "1", list(1), NULL)) {
        expect_error(do.call(core,c(list(bad),args)),"Non-numeric argument")
        expect_error(do.call(core,c(list(1),replace(args,1,list(bad)))),"Non-numeric argument")
      }
      expect_identical(do.call(core, c(list(numeric()), args)), numeric())
      expect_identical(do.call(core, c(list(c(NA_real_, NaN)), args)), c(NA_real_, NaN))
      # Recycling includes coefficients and does not require a multiple length.
      varying <- args; varying[[1]] <- c(args[[1]],args[[1]]*.9)
      if (id %in% c("hyper","binom")) varying[[1]] <- floor(varying[[1]])
      input <- if (kind == "q") c(.2,.5,.8) else c(0,1,2)
      expected <- vapply(seq_along(input),function(i) {
        row <- args; row[[1]] <- varying[[1]][(i-1L) %% 2L+1L]
        suppressWarnings(do.call(core,c(list(input[i]),row)))
      },numeric(1))
      expect_equal(suppressWarnings(do.call(core,c(list(input),varying))),expected,info=id)
      expect_identical(do.call(core,c(list(1),replace(args,1,list(numeric())))), numeric())
    }
    set.seed(198); got <- rml(17,fit); seed <- .Random.seed
    set.seed(198); expected <- do.call(get(paste0("r",id),asNamespace("stats")),c(list(17),ref_args))
    expect_identical(got,expected,info=id)
    expect_identical(.Random.seed,seed,info=id)
    if (id %in% c("hyper","binom","pois","geom","nbinom"))
      expect_warning(do.call(getFromNamespace(paste0("core_d",id),"univariateML"),c(list(1.5),args)),"non-integer x")
  }
})

test_that("window modifiers resolve their base through the core routes", {
  for (id in umvl_filter_families(c("geom","nbinom"))) {
    args <- if(id=="geom") list(prob=.3) else list(size=2,prob=.3)
    fit <- do.call(get(paste0("ml",id)),c(args,list(truncate=c(2,7))))
    base <- do.call(get(paste0("ml",id)),args)
    mass <- pml(7,base)-pml(1,base)
    expect_equal(dml(c(2,4,6),fit),dml(c(2,4,6),base)/mass,tolerance=1e-10,info=id)
    expect_equal(vapply(c(3,4,5),pml,numeric(1),obj=fit),(pml(c(3,4,5),base)-pml(1,base))/mass,tolerance=1e-10,info=id)
    expect_equal(qml(c(.2,.5,.8),fit),qml(pml(1,base)+c(.2,.5,.8)*mass,base),
                 tolerance=1e-10,info=id)
  }
})

test_that("archived concentrated beta fit evaluates its independent likelihood", {
  if (!length(umvl_filter_families("beta"))) skip("beta not selected")
  # Independent point evaluation recorded by the seam audit, not stats::dbeta.
  x <- c(.24999999999950001,.24999999999975001,.25,
         .25000000000025002,.25000000000075001)
  fit <- univariateML_construct(c(shape1=2.5337590343562099e23,shape2=7.6012771030666019e23),
    "mlbeta",list(logLik=135.27888712858683,n=5,call=NULL))
  attr(fit,"density") <- "stats::dbeta"
  expect_equal(sum(dml(x,fit,log=TRUE)),135.27888712858683,tolerance=1e-10)
})
