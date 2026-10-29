#!/usr/bin/env Rscript
# P1 migration. Run from the repository root; idempotent. No fits or golden
# numbers are regenerated. P0 sources: 2026-10-29-p0-{group}.md, amendments A3/A7.
source('univariateML/scripts/load_distributions.R')
root <- 'cppunivariate/include/cppunivariate/distributions'
kind_map <- list(
 design = list(binom='size', bbinom='size', cmb='size', tbinom=c('size','a','b'),
   zib='size', zmbinom='size', ztbinom='size', hyper='k', nhyper='r',
   tnorm=c('a','b'), tpois=c('a','b'), gpd='mu', huber='epsilon'),
 threshold = list(hnorm='mu', frechet='mu', fatigue='mu', pareto2='min',
   pareto3='min', pareto4='min', fpareto='min', nsbeta=c('min','max'), genbeta='scale'),
 endpoint = list(unif=c('min','max'), dunif=c('min','max'), pareto='b', power='alpha',
   zipf='N', triang=c('a','b')))
# C5 translations (hcauchy/ht mu) and C6 (hyper/nhyper N) wait for P4.
# R5 exempts held locations under the almost-every-sample rule. P0 locscale
# proves the collapse disappears away from ties at the held location;
# exceptional ties remain R6 diagnostics. Holding scale also removes the
# lst/tlambda collapse, but P0 did not prove all remaining shape boundaries.
restriction <- function(lower=NULL, upper=NULL, mechanism, exempt=list(), reason) {
  Filter(Negate(is.null), list(lower=lower, upper=upper,
    restriction=list(mechanism=mechanism, exempt_when_held=exempt, reason=reason)))
}
restrictions <- list(
 lst=list(df=restriction(lower=1, mechanism='collapse', exempt=list(list('mu')),
   reason='df >= 1 prevents free-location scale collapse; ties at a held location need a separate diagnostic.')),
 frechet=list(lambda=restriction(lower=1, mechanism='collapse', exempt=list(list('mu')),
   reason='lambda >= 1 prevents free-threshold collapse; held mu is inverse Weibull on x - mu.')),
 ht=list(nu=restriction(lower=1, mechanism='collapse', exempt=list(list('mu')),
   reason='nu >= 1 prevents free-threshold collapse below half minimum multiplicity; the current fixed-zero endpoint is exempt.')),
 gev=list(xi=list(
   restriction(lower=-1, mechanism='endpoint', reason='xi >= -1 prevents an upper-endpoint singularity; the current fitter still uses -1/2 until P3.'),
   restriction(upper=1, mechanism='collapse', reason='xi <= 1 prevents collapse, including with mu or sigma held (P0 extreme).'))),
 gpd=list(xi=restriction(lower=-1, mechanism='endpoint', reason='xi >= -1 prevents an upper-endpoint singularity; the current fitter still uses -1/2 until P3.')),
 tlambda=list(lambda=list(
   restriction(lower=-1, mechanism='collapse', exempt=list(list('mu')),
     reason='lambda >= -1 prevents free-location scale collapse; ties at held mu need a separate diagnostic.'),
   restriction(upper=1, mechanism='branch', reason='lambda <= 1 selects the principal branch; no exemption is proved.'))),
 ged=list(nu=restriction(lower=1, mechanism='spike', reason='nu >= 1 selects the Laplace member; spikes persist with sd held (P0 locscale).')),
 sged=list(nu=restriction(lower=1, mechanism='spike', reason='nu >= 1 selects the Laplace member; holding sd does not remove spikes (P0 locscale/skewreal).')))
yaml_lines <- function(key, value) strsplit(yaml::as.yaml(setNames(list(value), key)), '\n', fixed=TRUE)[[1]]
for (path in sort(Sys.glob(file.path(root,'*','README.md')))) {
  fm <- .umvl_read_front_matter(path); f <- fm$front; id <- f$id
  lines <- readLines(path); end <- which(lines=='---')[2]
  if(id=='fatigue') lines <- sub('support: {type: real, lower: 0,', 'support: {type: real, lower: -inf,', lines, fixed=TRUE)
  params <- lapply(f$params, function(p) {
    p$simulate <- p$simulate %||% p$default
    p$advice <- p$advice %||% p$required_reason
    p[c("default","fixable","fixed","required","required_reason")] <- NULL
    if (is.null(p$advice)) p$advice <- switch(paste(id,p$name,sep='.'),
      'hnorm.mu'='mu = 0 is the ordinary half-normal; NULL estimates the translated endpoint.',
      'frechet.mu'='Holding mu fits inverse Weibull on x - mu; NULL estimates mu with lambda >= 1.',
      'fatigue.mu'='Supply a finite threshold below every observation; NULL is unsupported because of endpoint spikes and a normal escape.',
      'huber.epsilon'='epsilon = 1.345 is a conventional tuning choice; NULL fits epsilon with normal and Laplace escapes.',
      'tnorm.a'='NULL is not supported until the endpoint closure (R7a); use -Inf for an unbounded side.',
      'tnorm.b'='NULL is not supported until the endpoint closure (R7a); use Inf for an unbounded side.',
      'gpd.mu'='Supply a threshold strictly below the data; estimating it has no unrestricted MLE.', NULL)
    p$kind <- "regular"
    for (k in names(kind_map)) if (p$name %in% kind_map[[k]][[id]]) p$kind <- k
    if (!is.null(restrictions[[id]][[p$name]])) p$fit_domain <- restrictions[[id]][[p$name]]
    p
  })
  start <- which(lines=='params:')
  stop_at <- min(which(grepl('^[A-Za-z_]+:',lines) & seq_along(lines)>start))-1
  lines <- append(lines[-seq.int(start,stop_at)],yaml_lines('params',params),after=start-1)
  end <- which(lines=='---')[2]
  # Derive structural missing-hold exceptions from exact adapter names;
  # this corrects P0 proposals affected by the old partial-name matching bug.
  cpp <- paste(readLines(file.path(dirname(path),'adapter.cpp')),collapse='\n')
  signature <- regmatches(cpp, regexpr(paste0('ml',id,'_\\([^{}]+\\) *\\{'),cpp))
  args <- regmatches(signature, gregexpr('[A-Za-z_][A-Za-z_0-9]*\\s*(?==|,|\\)\\s*\\{)',signature,perl=TRUE))[[1]]
  args <- trimws(args)
  pn <- vapply(f$params, `[[`, '', 'name')
  holds <- lapply(setdiff(pn,args), function(p) list(held=list(p),status='not-implemented',
    reason=paste0('The adapter does not accept `',p,'`; this hold is not implemented (P2).')))
  # P0 structural rejections even where the formal argument exists. Unlike
  # sample-dependent limits these are P2 implementation gaps (A7).
  rejected <- switch(id, pareto2=c('shape','scale'), pareto3=c('shape','scale'),
                     pareto4=c('shape1','shape2'), nsbeta=c('shape1','shape2'), character())
  for (p in rejected) holds <- c(holds,list(list(held=list(p),status='not-implemented',
    reason=if(id=='pareto4') 'Holding shapes is not supported for Pareto IV.' else
      'Holding additional parameters is not supported for this family.')))
  if(id=='bbinom') holds <- c(holds,list(list(held=list('alpha','beta'),status='not-implemented',
     reason='Hold at most one of `alpha` and `beta` fixed.')))
  # Enumerate exact held subsets for unsupported NULL required coefficients.
  # P0 structural no-MLE diagnoses for thresholds; design estimation waits P2/P3.
  required <- vapply(Filter(function(p) p$kind %in% c('design','threshold'),params), `[[`, '', 'name')
  unsupported <- required[!vapply(required, function(p)
    grepl(paste0('Nullable<[^>]+> +',p,'( *=| *,| *\\))'), signature), logical(1))]
  unsupported <- union(unsupported, switch(id,tnorm=c('a','b'),fatigue='mu',gpd='mu',character()))
  holdable <- intersect(pn,args)
  for(mask in 0:(2^length(holdable)-1)) {
    held <- holdable[as.logical(intToBits(mask)[seq_along(holdable)])]
    free <- setdiff(unsupported,held)
    if(!length(free)) next
    reason <- switch(id,
      fatigue='Estimating `mu` is not supported: the three-parameter fatigue likelihood has endpoint spikes and a normal escape, with no weakest shape restriction.',
      gpd='Estimating `mu` is not supported: the free-threshold GPD likelihood has no MLE (endpoint and logistic escapes).',
      tnorm='Estimating truncation bounds with NULL is not supported until the endpoint closure (R7a).',
      if(id %in% c('pareto2','pareto3','pareto4','fpareto','nsbeta','genbeta'))
        'Estimating the threshold is not supported: endpoint singularities or unattained limit models prevent an unrestricted MLE (P0, A4).'
      else paste0('Estimating `',free[1],'` with NULL is not implemented for this design.'))
    holds <- c(holds,list(list(held=as.list(held),status=if(id %in% c('fatigue','gpd')) 'no-mle' else 'not-implemented',reason=reason)))
  }
  # Remove previously generated migration blocks for idempotence.
  starts <- which(grepl('^(holds|fit_args):',lines[seq_len(end)]))
  for (start in rev(starts)) {
    next_key <- which(grepl('^[A-Za-z_]+:',lines) & seq_along(lines)>start & seq_along(lines)<end)
    stop_at <- if(length(next_key)) min(next_key)-1 else end-1
    lines <- lines[-seq.int(start,stop_at)]; end <- which(lines=='---')[2]
  }
  fit_args <- f$fit_args %||% list()
  additions <- switch(id, binom=list(size=NULL), zib=list(size=NULL),
    zmbinom=list(size=NULL), ztbinom=list(size=NULL), hnorm=list(mu=NULL),
    frechet=list(mu=NULL), fatigue=list(mu=0), huber=list(epsilon=1.345),
    tnorm=list(a=-Inf,b=Inf), gpd=list(mu=0), list())
  for (p in setdiff(names(additions),names(fit_args))) fit_args[p] <- additions[p]
  end <- which(lines=='---')[2]
  extra <- c(yaml_lines('holds',holds), if(length(fit_args)) yaml_lines('fit_args',fit_args))
  lines <- append(lines,extra,after=end-1)
  writeLines(lines,path)
  # Only add fit_args to frozen records. Keep every existing byte of numbers.
  golden <- file.path(dirname(path),'golden.json')
  if (length(additions) && file.exists(golden)) {
    txt <- paste(readLines(golden),collapse='\n')
    if (!grepl('"fit_args":',txt,fixed=TRUE)) {
      json <- jsonlite::toJSON(fit_args, auto_unbox=TRUE, null='null',digits=17)
      txt <- sub('\n}',paste0(',\n  "fit_args": ',json,'\n}'),txt,fixed=TRUE)
      writeLines(txt,golden)
    }
  }
}
