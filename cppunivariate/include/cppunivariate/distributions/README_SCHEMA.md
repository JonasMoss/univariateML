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
| `r.density`, `r.sampler` | yes | reference d/r functions, `pkg::fn` from an external package (the R package defines no d/p/q/r of its own); `p`/`q` are found by swapping the first letter |
| `r.args` | when the reference functions do not take the coefficient names | map from reference-function argument to coefficient name, e.g. `{mean: mu, sd: sigma, a: mu}` for the translated half-normal as `extraDistr::dtnorm`; a coefficient may feed several arguments. `dml`/`pml`/`qml`/`rml` and `simulate_default` apply it |
| `r.control` | yes (may be `{}`) | one entry per argument of the Rcpp adapter `ml<id>_` other than `x`, mapping name to a one-line description; codegen fails if the names differ from the adapter's |
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

Write math as `$...$` / `$$...$$`; the R codegen turns it into
`\eqn{}` / `\deqn{}`. Use backticks for code. Do not restate the density,
parameter tables, moments or the algorithm.

A limit may additionally declare `fixed: [name, ...]`: it applies only
when those parameters are held fixed. Limits without `fixed` describe the
all-free fit; each fixed subset must document which escapes remain.

## Optional parameter keys

Optional keys in each `params` entry:

- `fixable: true`: the user may hold this parameter fixed. The adapter
  `ml<id>_` takes an argument named like the coefficient; `NULL` (the
  default) estimates it, a value holds it at that value. The generated docs
  list it (a description in `r.control` overrides the generic text). The
  adapter returns `fixed`, the names of the parameters it held, which
  become the object's `fixed` attribute: `logLik()` degrees of freedom,
  AIC/BIC, `model_select` and `bootstrapml` count and hold only those.
- `fixed: <value>`: the fitter always holds this parameter at `<value>`
  (fatigue's `mu = 0`: the three-parameter MLE does not exist).
- `bound: lower` / `bound: upper`: the parameter is a support endpoint
  whose MLE is the sample minimum / maximum. The harness holds it at its
  golden value in the oracle and omits its score and Hessian row.
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
