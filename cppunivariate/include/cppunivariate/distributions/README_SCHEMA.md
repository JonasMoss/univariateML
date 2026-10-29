# Per-distribution `README.md`

Every distribution folder `distributions/<id>/` holds everything about that
distribution: the fitter (`mle.hpp`), d/p/q/r (`dpqr.hpp`), the Rcpp adapter
(`adapter.cpp`), C++ tests (`test.cpp`), R tests (`test.R`), golden data
(`golden.json`) and this one declarative file, `README.md`. The folder name
is the id. Design: `roadmap/2026-10-28-per-distribution-layout.md`.

`README.md` is YAML front matter plus prose. It is read by
`univariateML/scripts/load_distributions.R` (R metadata, wrappers and docs,
goldens), by the validation harness, and later by the Python shell.

## Front matter

| key | required | meaning |
|---|:-:|---|
| `id` | yes | must equal the folder name |
| `name` | yes | display name, e.g. `Lomax`; the R title is "<name> distribution maximum likelihood estimation" |
| `support` | yes | `{type: real\|integer, lower, upper, closed: [bool, bool]}`; bounds may be `inf` / `-inf` |
| `params` | yes | list of `{name, role, domain, default}`; names are the R coefficient names (see `univariateML/CANONICAL_NAMES.md`), in output order; `domain` is an interval string such as `"(0, inf)"` |
| `r.density`, `r.sampler` | yes | d/r functions: `pkg::fn` for an external package, or a bare name such as `dzipf` for package-defined functions; `p`/`q` are found by swapping the first letter |
| `r.args` | when the reference functions do not take the coefficient names | map from reference-function argument to coefficient name, e.g. `{mean: mu, sd: sigma, a: mu}` for the translated half-normal as `extraDistr::dtnorm`; a coefficient may feed several arguments. `dml`/`pml`/`qml`/`rml` and `simulate_default` apply it |
| `r.control` | yes (may be `{}`) | one entry per argument of the Rcpp adapter `ml<id>_` other than `x`, mapping name to a one-line description; codegen fails if the names differ from the adapter's |
| `fit_args` | when a parameter is `required` | map of arguments for the default fit `ml<id>(x, ...)` used by `just goldens` (recorded as `fit_args` in `golden.json` and replayed by the golden regression test); must name every `required` parameter, and may add arguments the fitter needs, such as hyper's `N` |
| `log_density` | yes | log density or log pmf in the natural parameters, sympy syntax, variable `x`; a `spec.py` in the folder may replace it when a string is not enough |
| `limits` | when the family is not Hellinger-closed | list of `{model, name, when, detect, status}`: `model` is the id of a package model the error message points to ("use `mlX`"); when the package has no such model, omit `model` and give `name` (e.g. `reflected translated half-normal`). `when` is the parameter path (math in `$...$`), `detect` the data condition if there is a simple one, `status` the C++ status that reports it; optional `fixed: [name]` restricts a limit to fits holding those parameters fixed; see `notes/ml-behaviour.tex` |
| `references` | no | one bibliographic entry per item |
| `examples` | no | R example code; default `ml<id>(simulate_default("ml<id>", 100))` |

Argument names follow `roadmap/2026-10-28-cran-argument-names.md`:
`reltol` (relative step), `abstol` (absolute step), `gradtol` (gradient
norm), `iterlim`, and `<param>0` for optional start values.

## Prose

Only what cannot be read off the code or the front matter:

- before the first `##` heading: one or two plain sentences on what the
  distribution is and what it is used for (the R `\description`);
- optional `## Details`: anything users need that the generated text does
  not say (interpretation, history, caveats);
- optional `## See also`: extra pointers beyond the density and the limit
  models, which are linked automatically.

YAML 1.1 reads bare `n`, `N`, `y`, `Y` (and `yes`, `no`, `on`, `off`) as
booleans, keys included: quote them (`name: "N"`, `"n": "N - m"`).

Write math as `$...$` / `$$...$$`; the R codegen turns it into
`\eqn{}` / `\deqn{}`. Use backticks for code. Do not restate the density,
parameter tables, moments or the algorithm.

## Optional parameter keys

Optional keys in each `params` entry:

- `fixable: true`: the user may hold this parameter fixed. The adapter
  `ml<id>_` takes an argument named like the coefficient; `NULL` (the
  default) estimates it, a value holds it at that value. The generated docs
  list it (a description in `r.control` overrides the generic text). The
  adapter returns `fixed`, the names of the parameters it held, which
  become the object's `fixed` attribute: `logLik()` degrees of freedom,
  AIC/BIC, `model_select` and `bootstrapml` count and hold only those.
  A held adapter default is also allowed for backward compatibility; then
  `NULL` explicitly frees it. Document this default in `r.control`; `default`
  remains the simulation value, and do not also set `fixed`.
- `fixed: <value>`: the fitter always holds this parameter fixed. When the
  adapter has an argument of the same name (listed in `r.control`), the user
  sets the held value and `<value>` is its default; otherwise it is always
  held at `<value>` (fatigue's `mu = 0`: the three-parameter MLE does not exist).
- `required: true`: a design quantity with no default (hyper's draws `k`).
  The adapter argument of the same name has no default (codegen checks
  this), it is listed in `r.control`, and the generated wrapper stops with
  "`ml<id>` requires `<name>` (<role>); it has no default." when it is
  missing. It is always held, so the adapter lists it in `fixed`. Cannot be
  combined with `fixable` or `fixed`; `default` is used only for
  simulation, and `fit_args` supplies its value for goldens.
- `bound: lower` / `bound: upper`: the parameter is a support endpoint
  whose MLE is the sample minimum / maximum when estimated. May be combined
  with `fixable: true` (for example hnorm); a held endpoint is used exactly.
  The harness holds it at its golden value in the oracle and omits its score and Hessian row.
- `kink: true`: hold this coordinate at its golden value for score and
  Hessian checks only; the oracle still optimizes it. Use for a parameter
  at a sample log-density kink, such as the triangular mode.
- `integer: true`: the harness omits derivatives and holds the discrete
  coefficient fixed.

The harness (`just quality`) also holds every parameter listed in a
golden's `fixed` field.

All other parameters are regular. At a closed domain endpoint, check the
one-sided score condition and the observed information on the free tangent
block. The oracle uses domain transforms and holds only flagged parameters.
A non-smooth sample location can prevent the ordinary Hessian check; report
this explicitly rather than substituting Fisher information.

An optional `spec.py` exports `log_density(symbols, x)`, returning a SymPy
expression using the supplied name-to-symbol dictionary and observation.
Use this only when `log_density` cannot be expressed as a string. Reports
include every unchecked id and reason. `limits` entries are listed only.


## Package-defined d/p/q/r

An optional `dpqr.R` in any distribution folder contains exported R wrappers
and roxygen documentation (one family help page). Assembly copies it to
`R/dpqr-<id>.R`. Scalar C++ evaluation lives in `dpqr.hpp`; Rcpp exports
(`d<id>_`, etc.) live in `adapter.cpp`. Bare `r.density: dzipf` and
`r.sampler: rzipf` resolve in the package namespace. Wrappers recycle all
parameter and observation vectors, propagate missing values, and warn for
invalid parameters and noninteger density arguments. CDFs floor observations;
quantiles use the smallest support point reaching the requested probability.
RNG adapters use R's RNG so `set.seed()` controls sampling.
