# P1 contract from generated README metadata and frozen golden data. No RNG,
# reference packages or numerical oracle is involved in this sweep.
subsets <- function(x) lapply(0:(2^length(x)-1), function(mask)
  x[as.logical(intToBits(mask)[seq_along(x)])])

test_that('parameter kinds, required messages and adapter signatures conform', {
  for (id in names(univariateML_metadata)) {
    meta <- univariateML_metadata[[id]]
    adapter <- getFromNamespace(paste0(id, '_'), 'univariateML')
    args <- formals(adapter)
    required <- vapply(Filter(function(p) p$kind %in% c('design','threshold'), meta$params), `[[`, '', 'name')
    expect_identical(meta$required, required, info=id)
    for (h in meta$holds) {
      if (h$status == 'not-implemented' && grepl('The adapter does not accept', h$reason, fixed=TRUE))
        expect_length(intersect(unlist(h$held), names(args)), 0L)
    }
    for (p in meta$params) {
      if (!p$name %in% names(args)) {
        expect_true(any(vapply(meta$holds, function(h)
          h$status=='not-implemented' && p$name %in% unlist(h$held), logical(1))), info=paste(id,p$name))
      } else if (p$name %in% required) {
        expect_identical(args[[p$name]], quote(expr=), info=paste(id,p$name))
      } else expect_null(args[[p$name]], info=paste(id,p$name))
    }
    for (p in required) {
      supplied <- golden_fit_args(id)
      supplied[p] <- NULL
      err <- tryCatch(do.call(get(id), c(list(x=c(1,2,3)), supplied)), error=identity)
      expect_true(inherits(err, 'error'), info=paste(id,p))
      text <- conditionMessage(err)
      info <- Filter(function(q) q$name==p, meta$params)[[1]]
      expect_match(text, paste0('requires `',p,'`'), fixed=TRUE)
      expect_match(text, paste0('(',info$kind,')'), fixed=TRUE)
      expect_match(text, 'Hold a value', fixed=TRUE)
      expect_match(text, paste0(p,' = NULL'), fixed=TRUE)
      if (!is.null(info$advice)) expect_match(text, info$advice, fixed=TRUE)
      # NULL must reach estimation or a declared unsupported-fit reason,
      # never the missing-argument diagnostic.
      supplied[p] <- list(NULL)
      result <- tryCatch(do.call(get(id), c(list(x=c(1,2,3)), supplied)),error=identity)
      if(inherits(result,'error')) expect_false(grepl('requires `',conditionMessage(result),fixed=TRUE), info=id)
    }
  }
})

test_that('every holdable subset fits golden data or gives a declared reason', {
  root <- Sys.getenv('UMVL_DISTRIBUTIONS_DIR')
  skip_if(!nzchar(root), 'no distribution goldens')
  for (id in names(univariateML_metadata)) {
    meta <- univariateML_metadata[[id]]
    g <- jsonlite::fromJSON(file.path(root,sub('^ml','',id),'golden.json'))
    estimates <- setNames(as.numeric(g$estimates),names(g$estimates))
    adapter <- getFromNamespace(paste0(id,'_'),'univariateML')
    holdable <- intersect(meta$names,names(formals(adapter)))
    for (held in subsets(holdable)) {
      # Non-coefficient controls remain as in the golden.
      # Required coefficients are explicit NULL
      # unless held in this subset; regular coefficients use their default.
      args <- golden_fit_args(id)
      args[intersect(names(args),meta$names)] <- NULL
      args[meta$required] <- rep(list(NULL),length(meta$required))
      args[held] <- as.list(estimates[held])
      label <- paste(id,paste(held,collapse=','))
      fit <- tryCatch(do.call(get(id),c(list(x=as.numeric(g$data)),args)),error=identity)
      if(inherits(fit,'error')) {
        # A nonempty holds entry covers supersets of its held set unless
        # an exact free-required exception (which enumerates the whole set).
        exceptions <- Filter(function(h) {
          hheld <- unlist(h$held)
          setequal(hheld,held) || (length(hheld) && all(hheld %in% held) &&
            !grepl('Estimating',h$reason,fixed=TRUE))
        },meta$holds)
        reasons <- vapply(exceptions, `[[`, '', 'reason')
        limits <- vapply(meta$limits,function(l) if(!is.null(l$message)) l$message else if(!is.null(l$model)) paste0('ml', l$model) else if(!is.null(l$name)) sub(';.*$', '', sub('^shifted ', '', l$name)) else '', '')
        declared <- c(reasons,limits)
        declared <- declared[nzchar(declared)]
        expect_true(any(vapply(declared,function(reason)
          grepl(tolower(reason),tolower(conditionMessage(fit)),fixed=TRUE),logical(1))),
          info=paste(label,conditionMessage(fit)))
      } else {
        expect_identical(unname(as.numeric(fit[held])),unname(estimates[held]),info=label)
        free <- setdiff(meta$names,held)
        expect_true(all(is.finite(fit[free])),info=label)
        expect_true(is.finite(attr(fit,'logLik')),info=label)
        # P0 uses 1e-6*n, allowing the existing finite-difference fits'
        # numerical stopping error, without changing any golden values.
        expect_gte(attr(fit,'logLik'),g$logLik-1e-6*g$n,label=label)
      }
    }
  }
})
