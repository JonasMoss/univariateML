test_that("synthetic mappings evaluate expressions and translate d/p/q/r", {
  ns <- asNamespace("univariateML")
  env <- new.env(parent = ns)
  env$univariateML_args <- list(mlsynthetic = c(mean = "mu + delta", sd = "sigma"))
  env$univariateML_shifts <- list(mlsynthetic = "offset")
  env$univariateML_name <- function(obj) "mlsynthetic"
  env$univariateML_to_string <- function(obj, type) paste0("stats::", type, "norm")
  env$univariateML_dpqr_args <- get("univariateML_dpqr_args", ns)
  environment(env$univariateML_dpqr_args) <- env
  to_function <- get("univariateML_to_function", ns)
  environment(to_function) <- env
  obj <- c(mu = 1, delta = 2, sigma = 2, offset = 4)
  expect_equal(env$univariateML_dpqr_args(obj, "mlsynthetic"), list(mean = 3, sd = 2))
  expect_equal(to_function(obj, "d")(8), stats::dnorm(8, 7, 2))
  expect_equal(to_function(obj, "p")(8), stats::pnorm(8, 7, 2))
  expect_equal(to_function(obj, "q")(.7), stats::qnorm(.7, 7, 2))
  set.seed(42); observed <- to_function(obj, "r")(5)
  set.seed(42); expect_equal(observed, stats::rnorm(5, 7, 2))
})

test_that("restriction attributes survive construction and are displayed", {
  construct <- getFromNamespace("univariateML_construct", "univariateML")
  fit <- construct(c(mean = 0, sd = 1), "mlnorm",
                   list(logLik = -1, call = "mlnorm(x)", n = 3),
                   fixed = "mean", restricted = c(sd = "lower"))
  expect_identical(attr(fit, "restricted"), c(sd = "lower"))
  expect_output(print(fit), "Restricted:.*sd = lower")
  expect_output(summary(fit), "Restricted:.*sd = lower")
})

test_that('synthetic codegen accepts expressions and rejects invalid signatures', {
  root <- Sys.getenv('UMVL_DISTRIBUTIONS_DIR')
  skip_if(!nzchar(root),'no repository codegen')
  repo <- normalizePath(file.path(root,'../../../..'))
  # distributions is univariateML-cpp/include/cppunivariate/distributions.
  work <- tempfile('parameter-codegen-'); dir.create(work)
  on.exit(unlink(work,recursive=TRUE),add=TRUE)
  dossier <- file.path(work,'distributions'); dir.create(dossier)
  synthetic <- file.path(dossier,'synthetic'); dir.create(synthetic)
  readme <- c('---','id: synthetic','name: Synthetic',
    'support: {type: real, lower: -inf, upper: inf}',
    'params:',
    '  - {name: mu, kind: threshold, role: location, domain: "(-inf, inf)", simulate: 0}',
    '  - {name: sigma, role: scale, domain: "(0, inf)", simulate: 1}',
    'r:', '  density: stats::dnorm', '  sampler: stats::rnorm',
    '  shift: mu', '  args: {mean: "mu + sigma", sd: sigma}',
    '  control: {}','fit_args: {mu: 0}', 'examples: mlsynthetic()', '---','A synthetic normal model.')
  writeLines(readme,file.path(synthetic,'README.md'))
  out <- file.path(work,'R'); dir.create(out)
  writeLines('mlsynthetic_ <- function(x, mu, sigma=NULL) NULL',file.path(out,'RcppExports.R'))
  withr::local_dir(file.path(repo,'bindings/r'))
  withr::local_envvar(c(UMVL_DOSSIER_DIR=dossier,UMVL_PKG_R_DIR=out))
  expect_error(sys.source('scripts/codegen_metadata.R',new.env()),NA)
  metadata <- new.env(); sys.source(file.path(out,'ml00_metadata.R'),metadata)
  expect_identical(metadata$univariateML_args$mlsynthetic[['mean']], 'mu + sigma')
  expect_identical(metadata$univariateML_shifts$mlsynthetic, 'mu')
  expect_error(sys.source('scripts/codegen_wrappers.R',new.env()),NA)
  writeLines(readme[!grepl('^examples:', readme)], file.path(synthetic,'README.md'))
  expect_error(sys.source('scripts/codegen_wrappers.R',new.env()), 'missing examples:')
  writeLines(readme, file.path(synthetic,'README.md'))
  writeLines('mlsynthetic_ <- function(x, mu=NULL, sigma=NULL) NULL',file.path(out,'RcppExports.R'))
  expect_error(sys.source('scripts/codegen_wrappers.R',new.env()),'required coefficient.*no adapter default')
  writeLines('mlsynthetic_ <- function(x, mu, sigma=1) NULL',file.path(out,'RcppExports.R'))
  expect_error(sys.source('scripts/codegen_wrappers.R',new.env()),'non-required coefficient.*NULL')
  writeLines('mlsynthetic_ <- function(x, mu) NULL',file.path(out,'RcppExports.R'))
  expect_error(sys.source('scripts/codegen_wrappers.R',new.env()),'not an adapter argument.*holds: not-implemented')
  writeLines(sub('mu + sigma','unknown + sigma',readme,fixed=TRUE),file.path(synthetic,'README.md'))
  expect_error(sys.source('scripts/codegen_metadata.R',new.env()),'invalid r.args expression')
})
