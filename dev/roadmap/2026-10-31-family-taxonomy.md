# Family taxonomy

2026-10-31. Jonas: classification and organisation come before the theory
articles; "make something plausible, it is easier to see what is missing
once something is up". This is a first version to be revised once it is
visible in the catalog.

## Principles

- Organise families by how they are built, how they relate, and what
  happens when they are fitted, using the vocabulary of
  `dev/research/ml-behaviour.tex` (existence, escape, limit models, holds,
  restrictions) rather than ad hoc tags.
- Derive whatever the front matter already says; write by hand only what
  it cannot, from short controlled vocabularies.
- Relations are claims, so they are checked: special cases and
  transformations symbolically from `log_density`, limits as now.
- The theory articles (TASK-101.6.3, later) explain these tags; they do
  not introduce new ones.

## Derived dimensions

| Dimension | Values | From |
|---|---|---|
| support | `real`, `positive`, `unit`, `bounded`, `counts`, `finite-counts`; flag `dependent` when bounds involve coefficients | `support` |
| coefficients | count; kinds (`regular`, `design`, `threshold`, `endpoint`) | `params` |
| existence | any of `always`, `degenerate-only`, `escapes`, `restricted`, `needs-held`, `partial` | `limits`, `fit_domain`, `holds` |

Existence tags: `always` when nothing below applies; `degenerate-only`
when the MLE fails only for degenerate samples (constant data, all zero);
`escapes` when a `limits` record names a limit model; `restricted` when
`fit_domain` restricts a coefficient; `needs-held` when `holds` marks a
subset `no-mle` (a threshold or design coefficient must be held);
`partial` when `holds` marks a subset `not-implemented`.

## Written dimensions

- `group`: one primary group for the catalog. Initial list: normal and
  Student; skewed and robust location-scale; gamma and Weibull;
  beta and generalised beta; Pareto and Lomax; extreme value; other
  positive; bounded continuous; Poisson, binomial and negative binomial;
  finite sampling; discrete heavy-tailed; modified counts (truncated,
  zero-modified, zero-inflated).
- `construction`: any of `base`, `location-scale`, `transformation`,
  `skewing`, `folding`, `truncation`, `modification`, `compounding`,
  `discretisation`, `generalisation`.
- `relations`, in the child's README:
  - `special_case_of`: the parent family and the parent coefficients as
    expressions in the child's, for example in `exp`:
    `{family: gamma, at: {shape: 1, scale: 1/rate}}`.
  - `transform_of`: the parent, a transform from `log`, `logit`,
    `inverse`, `power`, `negate`, `abs`, `shift`, and the coefficient map;
    for example in `lnorm`: `{family: norm, transform: log, at: {mean: meanlog, sd: sdlog}}`
    meaning $\log X$ follows the parent.
  - Limit edges stay in `limits`.
- `fit`: `method` from `closed-form`, `scalar-solve`, `profile-search`,
  `multistart`, `integer-search`, `delegation`; `certified` from `yes`,
  `partial`, `no`, `unknown`, with a one-line note. Seeded from the fitter
  inventory and the dossier `## Details`; `unknown` rather than a guess.
- `areas`: up to three from reliability and survival, actuarial,
  economics and income, finance, extremes and environment, counts and
  ecology, proportions, measurement and errors, robust modelling, waiting
  times and queues, rank-frequency, sampling and quality control.
- `classification` (`classical`, `specialist`, `uncommon`) with its reason,
  as added in TASK-101.6.4.

## Checks

- The R loader validates vocabularies and that relation targets exist.
- A Python check substitutes each `special_case_of` map into the parent's
  `log_density` and compares with the child's (SymPy; numeric comparison at
  random interior points where simplification fails). `transform_of`
  adds the Jacobian. Unverified edges are listed, not dropped.

## Site

The catalog groups by `group` and filters on every dimension. Family pages
show the tags and a "Related families" section with parents, children
(derived by inverting the edges), transforms and limit models. The family
graph is the first theory visualisation (JS 3).
