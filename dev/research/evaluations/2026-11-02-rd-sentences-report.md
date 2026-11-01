# R help limit sentences and mlst diagnostics

TASK-101.6.4.1, job `rd-sentences`, completed 2026-11-02 on the local workstation.
Tested base revision: `13d3ad0fed44815f6bc3b6ef677b6d3d87006c59`, with the
uncommitted source changes listed below. No commits or board edits were made.

The generator now renders both legacy clauses and full site-facing sentences
without splicing their punctuation into another sentence. Fitting code,
adapters, defaults, goldens and other family dossiers were not changed.
This is a rendering change, not a new mathematical existence or uniqueness
claim. Numerical detection remains explicitly hedged when `numerical: true`.

## Templates

Every paragraph consists of an optional scope sentence, a branch-specific lead,
the stopping sentence, then the parameter path (`when`) and data condition
(`detect`) as separate sentences. Missing path/condition fields are omitted.

- Scope: “This limit applies with `<parameters>` held fixed.”
- `kind: nonexistence`: “The likelihood is unbounded and no finite maximum
  likelihood estimator exists.”
- `numerical: true`: “The numerical search does not establish a finite maximum
  above its tolerance relative to the limit (<name>).” The relative-to phrase
  is omitted if no name is available.
- Ordinary limit with a package target: “The maximum likelihood estimator does
  not exist: the likelihood increases toward the <name> distribution.”
- Ordinary limit without a package target: “The maximum likelihood estimator
  does not exist: the likelihood approaches its supremum only in a limit
  (<name>).”
- Stopping sentence: “`mlX()` stops with an error; use [mlY()] to fit the limit
  model.” Without a package target, it ends after “error.” No empty `ml()`
  link is generated.

`rd_sentence()` trims surrounding whitespace, capitalizes an initial prose
letter, and adds a period only if final `.`, `!` or `?` is absent. It preserves
math and existing code spans. Literal brace groups in prose, such as `{0,1}`,
receive an inline code span; otherwise CMP/CMB lose braces when their former
whole-condition code spans are removed. Full conditions are now prose, rather
than long code spans. The path is retained in all three branches, including
non-existence and numerical records, which previously dropped it.

The other uses of hold and existence reasons in `codegen_wrappers.R` already
pass them directly rather than splicing them into prose. No other codegen
script splices these limit fields.

## Diagnostic causes and fixes

In the st dossier, the displayed density used raw `\[`/`\]`, which the existing
Markdown-to-Rd math converter did not recognize. It now uses `$$` delimiters,
producing `\deqn{...}`; `\mathbb{R}` also has explicit argument braces. Its
top-level Markdown heading generated an empty Rd section, so that heading is
now plain prose. Only this introductory markup was changed.

The baseline scan reproduced all 26 mlst diagnostics: 22 unknown math macros,
one empty-section diagnostic and three lost-brace diagnostics. The all-family
scan also exposed ten existing title-period diagnostics in helper topics.
`codegen_links.R`, the existing preprocessing pass over assembled R sources,
now removes a terminal period from implicit roxygen titles, including multiline
titles. Authored helper R sources remain unchanged. Final diagnostics are zero.

The first updated scan revealed two literal-set brace diagnostics (CMP and CMB).
The focused brace protection above resolved these at the shared rendering cause;
neither dossier was edited. The complete final test/check run followed that fix.

## Validation

All builds and R checks ran locally under `/tmp/umvl-cpu/run` or `run4`, with
one BLAS/OpenMP thread per worker and `nice -n 10` supplied by those wrappers.
No simbox, package installation, site build or sub-agent was used. Temporary
files and logs stayed under `build/`. Environment: R 4.6.1 (2026-06-24),
Linux 7.0.0-38-generic, x86_64. CPU slot waits were allowed to finish.

| Check | Final result |
|---|---|
| `just regen` | Passed; also rerun as a dependency of final `just test` |
| `just test` (R dev tier, four workers) | FAIL 0, WARN 0, SKIP 151, PASS 20102; elapsed 106.918 s, excluding slot wait |
| All-family `tools::checkRd`, excluding Non-ASCII | No output; zero diagnostics |
| Artifact-pattern scan over every generated Rd | No output; no sentence-splicing matches |
| Focused renderer assertions | Passed: all three branches, clauses versus sentences, missing target/detect, punctuation, braces, math and existing code spans |
| `tools::Rd2txt` manual reading | All limit paragraphs for burr, lomax, st, gamma and cauchy reproduced below |
| `git diff --check` | Passed |

The exact requested all-family scan was run after the final tests:

```sh
cd univariateML-r
Rscript -e 'for (f in Sys.glob("man/*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }'
```

The artifact scan was:

```sh
rg -n -P '(?<![./])\.\.(?![./])|[.]\s*, the maximum|as (The|A|With)\b|When [A-Z]|When ,|held fixed: when With|\\link\[=ml\]' univariateML-r/man --glob '*.Rd'
```

It returned exit 1 with no output, meaning no matches. A literal `..` search
also finds legitimate relative-link paths (`../`); `...` is normal R usage.
The punctuation pattern excludes those cases, rather than claiming these
characters disappear from help files. Reference-author initials followed by
commas are likewise not sentence-splicing defects.

No full-tier, C++ or JS suite was needed for this documentation-only change;
the final requested R dev tier completed green. Skips are the suite's declared
dev-tier skips, not failures. These checks validate documentation generation
and existing regression behavior, not the mathematical claims in the dossiers.
Open items for this job: none.

## Before and after: rendered limit paragraphs

These are the actual `tools::Rd2txt` paragraphs, with wrapping normalized for
this report. The baseline was generated before edits and retained separately
from subsequent regeneration. Plain-text math is the representation supplied
by Rd2txt. All limit records are included, not just the first one per family.
Cauchy already has capitalized, punctuated fields at this revision; Lomax is
the actual legacy clause-style example among these five, and st also has
math-only paths. The synthetic checks additionally compare both prose styles
in every branch.

### mlburr

Before:

```text
When All observations are equal and c and s are free. With s held, c is free and either
every observation equals s, or both shapes are free and the common observation is below
s., the maximum likelihood estimator does not exist: the likelihood approaches its
supremum only in a limit (unbounded likelihood) as The free power c tends to infinity
while the free scale follows a concentrating path at the common observation. With scale
held, concentration is also possible at a common observation below s by letting k grow,
or at s itself.. ‘mlburr()’ then stops with an error.

With ‘shape2’ held fixed: when With c held, the numerical comparison for the transformed
observations selects the exponential limit of the Lomax likelihood. This is not an exact
sample criterion., the maximum likelihood estimator does not exist: the likelihood
increases toward the Weibull distribution as With c held, k tends to infinity and s
k^{-1/c} stays at a positive Weibull scale.. ‘mlburr()’ then stops with an error; use
‘mlweibull()’ instead.

When A numerical comparison finds no candidate finite fit improving on this limit by
more than 10^{-8}n log-likelihood units. This comparison is not a global certificate.,
the maximum likelihood estimator does not exist: the likelihood increases toward the
Weibull distribution as The shape k tends to infinity while s k^{-1/c} tends to a
positive Weibull scale.. ‘mlburr()’ then stops with an error; use ‘mlweibull()’ instead.

When A numerical comparison finds no candidate finite fit improving on this limit by
more than 10^{-8}n log-likelihood units. This comparison is not a global certificate.,
the maximum likelihood estimator does not exist: the likelihood increases toward the
Pareto distribution as The power c tends to infinity while kc tends to a positive Pareto
shape and s stays at its endpoint.. ‘mlburr()’ then stops with an error; use
‘mlpareto()’ instead.

With ‘scale’ held fixed: when Only scale is held and all observations are at least s.
The all-equal-to-s case has unbounded likelihood instead., the maximum likelihood
estimator does not exist: the likelihood increases toward the Pareto distribution as The
power c tends to infinity while kc tends to a positive Pareto shape and s stays at its
endpoint.. ‘mlburr()’ then stops with an error; use ‘mlpareto()’ instead.
```

After:

```text
The maximum likelihood estimator does not exist: the likelihood approaches its supremum
only in a limit (unbounded likelihood). ‘mlburr()’ stops with an error. The free power c
tends to infinity while the free scale follows a concentrating path at the common
observation. With scale held, concentration is also possible at a common observation
below s by letting k grow, or at s itself. All observations are equal and c and s are
free. With s held, c is free and either every observation equals s, or both shapes are
free and the common observation is below s.

This limit applies with ‘shape2’ held fixed. The maximum likelihood estimator does not
exist: the likelihood increases toward the Weibull distribution. ‘mlburr()’ stops with
an error; use ‘mlweibull()’ to fit the limit model. With c held, k tends to infinity and
s k^{-1/c} stays at a positive Weibull scale. With c held, the numerical comparison for
the transformed observations selects the exponential limit of the Lomax likelihood. This
is not an exact sample criterion.

The maximum likelihood estimator does not exist: the likelihood increases toward the
Weibull distribution. ‘mlburr()’ stops with an error; use ‘mlweibull()’ to fit the limit
model. The shape k tends to infinity while s k^{-1/c} tends to a positive Weibull scale.
A numerical comparison finds no candidate finite fit improving on this limit by more
than 10^{-8}n log-likelihood units. This comparison is not a global certificate.

The maximum likelihood estimator does not exist: the likelihood increases toward the
Pareto distribution. ‘mlburr()’ stops with an error; use ‘mlpareto()’ to fit the limit
model. The power c tends to infinity while kc tends to a positive Pareto shape and s
stays at its endpoint. A numerical comparison finds no candidate finite fit improving on
this limit by more than 10^{-8}n log-likelihood units. This comparison is not a global
certificate.

This limit applies with ‘scale’ held fixed. The maximum likelihood estimator does not
exist: the likelihood increases toward the Pareto distribution. ‘mlburr()’ stops with an
error; use ‘mlpareto()’ to fit the limit model. The power c tends to infinity while kc
tends to a positive Pareto shape and s stays at its endpoint. Only scale is held and all
observations are at least s. The all-equal-to-s case has unbounded likelihood instead.
```

### mllomax

Before:

```text
When both coefficients are free and the numerical profile search finds no improvement
over the best exponential larger than 10^{-8}n log-likelihood units, the numerical
search does not establish a finite maximum above its tolerance. ‘mllomax()’ stops; use
‘mlexp()’ to fit the limit model.
```

After:

```text
The numerical search does not establish a finite maximum above its tolerance relative to
the limit (Exponential). ‘mllomax()’ stops with an error; use ‘mlexp()’ to fit the limit
model. kappa tends to infinity while lambdakappa tends to a positive exponential rate.
Both coefficients are free and the numerical profile search finds no improvement over
the best exponential larger than 10^{-8}n log-likelihood units.
```

### mlst

Before:

```text
When , the numerical search does not establish a finite maximum above its tolerance.
‘mlst()’ stops; use ‘mlsn()’ to fit the limit model.

When , the numerical search does not establish a finite maximum above its tolerance.
‘mlst()’ stops; use ‘mlht()’ to fit the limit model.

When , the numerical search does not establish a finite maximum above its tolerance.
‘mlst()’ stops; use ‘ml()’ to fit the limit model.

When , the numerical search does not establish a finite maximum above its tolerance.
‘mlst()’ stops; use ‘mlhnorm()’ to fit the limit model.

When , the numerical search does not establish a finite maximum above its tolerance.
‘mlst()’ stops; use ‘ml()’ to fit the limit model.

When the data point to this limit, the maximum likelihood estimator does not exist: the
likelihood approaches its supremum only in a limit (unbounded scale collapse) as
omega\to0 and nu(n-k)<k at the collapse location for finite shape. ‘mlst()’ then stops
with an error.

When the data point to this limit, the maximum likelihood estimator does not exist: the
likelihood approaches its supremum only in a limit (scale-collapse equality) as
omega\to0 and nu(n-k)=k at the collapse location. ‘mlst()’ then stops with an error.
```

After:

```text
The numerical search does not establish a finite maximum above its tolerance relative to
the limit (Azzalini skew-normal). ‘mlst()’ stops with an error; use ‘mlsn()’ to fit the
limit model. nu\toInf.

The numerical search does not establish a finite maximum above its tolerance relative to
the limit (Half-t). ‘mlst()’ stops with an error; use ‘mlht()’ to fit the limit model.
alpha\to+Inf.

The numerical search does not establish a finite maximum above its tolerance relative to
the limit (reflected half-t). ‘mlst()’ stops with an error. alpha\to-Inf.

The numerical search does not establish a finite maximum above its tolerance relative to
the limit (Translated Half-Normal). ‘mlst()’ stops with an error; use ‘mlhnorm()’ to fit
the limit model. nu\toInf and alpha\to+Inf.

The numerical search does not establish a finite maximum above its tolerance relative to
the limit (reflected half-normal). ‘mlst()’ stops with an error. nu\toInf and
alpha\to-Inf.

The maximum likelihood estimator does not exist: the likelihood approaches its supremum
only in a limit (unbounded scale collapse). ‘mlst()’ stops with an error. omega\to0 and
nu(n-k)<k at the collapse location for finite shape.

The maximum likelihood estimator does not exist: the likelihood approaches its supremum
only in a limit (scale-collapse equality). ‘mlst()’ stops with an error. omega\to0 and
nu(n-k)=k at the collapse location.
```

### mlgamma

Before:

```text
When all observations are equal and both shape and scale are free, the likelihood is
unbounded and no finite maximum likelihood estimator exists. ‘mlgamma()’ stops with an
error.
```

After:

```text
The likelihood is unbounded and no finite maximum likelihood estimator exists.
‘mlgamma()’ stops with an error. The free shape tends to infinity while scale tends to
zero at fixed mean. All observations are equal and both shape and scale are free.
```

### mlcauchy

Before:

```text
When Excessive mass at that location or vanishing residual spread., the maximum
likelihood estimator does not exist: the likelihood approaches its supremum only in a
limit (scale collapse) as Scale tends to zero at the fitted or held location..
‘mlcauchy()’ then stops with an error.
```

After:

```text
The maximum likelihood estimator does not exist: the likelihood approaches its supremum
only in a limit (scale collapse). ‘mlcauchy()’ stops with an error. Scale tends to zero
at the fitted or held location. Excessive mass at that location or vanishing residual
spread.
```

## Reproducible focused assertions

Run this R code from the worktree root after generation. It evaluates only the
renderer definitions, without reexecuting the whole generator.

```r
env <- new.env()
env$models <- list(exp = list(name = "exponential"))
env$`%||%` <- function(a, b) if (is.null(a)) b else a
for (expr in parse("bindings/r/scripts/codegen_wrappers.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]]) %in% c("nl", "rd_math", "and_list", "rd_sentence", "limit_sentence"))
    eval(expr, env)
}
for (branch in c("ordinary", "nonexistence", "numerical")) {
  lim <- list(model = "exp", when = "the shape tends to infinity",
              detect = "all observations are equal", fixed = "scale")
  if (branch == "nonexistence") lim$kind <- branch
  if (branch == "numerical") lim$numerical <- TRUE
  clause <- env$limit_sentence(lim, list(id = "example"))
  lim$when <- "The shape tends to infinity."
  lim$detect <- "All observations are equal."
  stopifnot(identical(clause, env$limit_sentence(lim, list(id = "example"))),
            grepl("use [mlexp()]", clause, fixed = TRUE),
            !grepl("..", clause, fixed = TRUE))
}
lim <- list(name = "reflected half-t", numerical = TRUE, when = "$\\alpha\\to-\\infty$")
txt <- env$limit_sentence(lim, list(id = "st"))
stopifnot(!grepl("ml()", txt, fixed = TRUE), !grepl("When", txt, fixed = TRUE),
          grepl("does not establish", txt, fixed = TRUE),
          identical(env$rd_sentence("Already punctuated!"), "Already punctuated!"),
          identical(env$rd_sentence(NULL), character()))
stopifnot(identical(env$rd_sentence("data in {0,1} with $x^{2}$ and `{a,b}`"),
                    "Data in `{0,1}` with \\eqn{x^{2}} and `{a,b}`."))
cat("Renderer checks passed: all three branches, both prose styles, optional fields, punctuation.\n")
```

## Tested source fingerprints

SHA-256 fingerprints of the final source files (the report itself was written
after the final checks):

```text
4269c3ce73f29648260e2b8726f7ee433f6c9c7d6d9afe4ac8f7e40881944b02  bindings/r/scripts/codegen_wrappers.R
99b7863eb6edf0f91d0c5af6e927ff0b987ca60d8d19e59841722aec45ab1cb8  bindings/r/scripts/codegen_links.R
f48f47578f84bdc6f3619f7f32432295e75508b437eab98112aaa7bf90b4f6df  univariateML-cpp/include/cppunivariate/distributions/st/README.md
```
