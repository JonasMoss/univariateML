# Per-distribution `README.md`

Each core folder `univariateML-cpp/include/cppunivariate/distributions/<id>/`
holds the canonical dossier (`README.md`), fitter (`mle.hpp`), d/p/q/r
(`dpqr.hpp`), C++ tests (`test.cpp`), golden data (`golden.json`) and truth
fixtures. Rcpp adapters (`adapter.cpp`), R tests (`test.R`) and other R
checks live separately in `bindings/r/distributions/<id>/`. The folder name
is the id in both layers. Design: `dev/roadmap/decisions.md` (Migration layout).

`README.md` is YAML front matter plus prose. It is read by
`bindings/r/scripts/load_distributions.R` (R metadata, wrappers and docs,
goldens), by the validation harness, and later by the Python shell.

## Front matter

| key | required | meaning |
|---|:-:|---|
| `id` | yes | must equal the folder name |
| `name` | yes | display name, e.g. `Lomax`; the R title is "<name> distribution maximum likelihood estimation" |
| `support` | yes | `{type: real\|integer, lower, upper, closed: [bool, bool]}`; bounds may be `inf` / `-inf` or coefficient expressions (see below) |
| `params` | yes | list of `{name, role, domain, simulate}`; names are the R coefficient names (see `bindings/r/CANONICAL_NAMES.md`), in output order; `domain` is an interval string such as `"(0, inf)"` |
| `r.density`, `r.sampler` | yes | d/r functions: `pkg::fn` for an external package, or a bare name such as `dzipf` for package-defined functions; `p`/`q` are found by swapping the first letter |
| `r.args` | when the reference functions do not take the coefficient names | map from reference-function argument to an R expression in the coefficients, e.g. `{mean: mu, sd: sigma}` for a stats reference; a coefficient may feed several arguments; e.g. `"n": "N - m"`. Expressions must parse as one expression and use only coefficient variables. `dml`/`pml`/`qml`/`rml` apply it |
| `r.control` | yes (may be `{}`) | one entry per non-coefficient argument of the Rcpp adapter `ml<id>_` other than `x` (coefficient descriptions are optional), mapping name to a one-line description; codegen fails if the names differ from the adapter's |
| `fit_args` | when a parameter has kind `design` or `threshold` | map of arguments for the default fit `ml<id>(x, ...)` used by `just goldens` (recorded as `fit_args` in `golden.json` and replayed by the golden regression test); must name every required parameter (including explicit YAML `null` to estimate), and may add arguments the fitter needs, such as hyper's `N` |
| `log_density` | yes | log density or log pmf in the natural parameters, sympy syntax, variable `x`; a `spec.py` in the folder may replace it when a string is not enough |
| `limits` | when the family is not Hellinger-closed | list of `{model, name, when, detect, status}`: `model` is the id of a package model the error message points to ("use `mlX`"); when the package has no such model, omit `model` and give `name` (e.g. `reflected translated half-normal`). `when` is the parameter path (math in `$...$`), `detect` the data condition if there is a simple one, `status` the C++ status that reports it; optional `fixed: [name]` restricts a limit to fits holding those parameters fixed; see `dev/research/ml-behaviour.tex` |
| `references` | no | one bibliographic entry per item |
| `examples` | yes | R example code; generation fails when absent. Use real data appropriate to the family or `rml(n, mlX())`. |

Argument names follow `dev/roadmap/2026-10-28-cran-argument-names.md`:
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
- `## Details` also carries the fitting method where it is not obvious
  (adopted 2026-10-30, pilot: `exp`, `gamma`, `gb2`): what the fit
  establishes (closed form, unique root, proved stopping rule, or only a
  numerical search), the boundary and limit comparisons, the failure
  behaviour, and links to evidence. Depth follows the problem: a condition
  and a check for a closed form; objective, reduction, bracket and
  uniqueness for a scalar solve; starts, basins, faces, fallbacks and open
  guarantees for an involved search.
- optional `## Decision history`: dated material choices with commit and
  report links, including superseded approaches when they explain the
  present design; unknown rationale is stated as unknown. It stays in the
  dossier (and the future site): the R codegen does not render it in help.

YAML 1.1 reads bare `n`, `N`, `y`, `Y` (and `yes`, `no`, `on`, `off`) as
booleans, keys included: quote them (`name: "N"`, `"n": "N - m"`).

Write math as `$...$` / `$$...$$`; the R codegen turns it into
`\eqn{}` / `\deqn{}`. Use backticks for code. Do not restate the density,
parameter tables, moments or the algorithm.

## Parameter rules (P1)

Each parameter has `kind: design | threshold | endpoint | regular`, defaulting
to `regular`. Design quantities and thresholds are required: the adapter has
no default for them. All other coefficient arguments default to `NULL`.
A supplied value holds a coefficient; explicit `NULL` requests estimation
where supported. Missing required coefficients stop with the kind, both ways
out, and the optional nonempty scalar parameter `advice` text. `simulate`
is a required numeric scalar (infinity is accepted), a user-facing default for models built without data, and the simulation value for validation. With data it never selects a held fitting default.

The old parameter keys `default`, `fixable`, `fixed`, `required`, and
`required_reason` are rejected. `advice` replaces `required_reason`.
Every coefficient must be an exact adapter argument or be covered by a
`holds` entry with status `not-implemented`. Exact names are required;
a coefficient name cannot partially match a start-value argument.

`fit_domain` belongs to a parameter. It describes an R5 fitting restriction,
separate from the reference `domain`, e.g.:

```yaml
fit_domain:
  lower: 1
  restriction:
    mechanism: collapse
    exempt_when_held: [[mu]]
    reason: Free location allows scale collapse; held mu removes this path.
```

Use `upper` for an upper bound. Multiple restrictions on the same coefficient
are a list of these records (GEV and Tukey lambda). Mechanisms are `collapse`,
`spike`, `branch`, and `endpoint`. `exempt_when_held` lists sufficient held sets;
`[]` means no exemption. Exemptions use P0's almost-every-sample existence
results; exceptional ties still require diagnostics. The metadata records the
fitting bounds; adapters implement restrictions and endpoint closure.

Top-level `holds` lists structural exceptions only (A7):

```yaml
holds:
  - {held: [shape], status: not-implemented, reason: The adapter does not accept this hold.}
  - {held: [], status: no-mle, reason: The all-free likelihood is unbounded.}
```

An unsupported held coefficient covers supersets containing it. An exception
for estimating a required coefficient enumerates its exact held set. Adapter
arguments that reject a combination also need a structural entry. Sample-
dependent non-existence belongs in `limits` and prose, never `holds`.
`limits` may additionally give `message`, a diagnostic substring for generic
conformance checks. `fixed` inside a limit still means that the limit applies
with those coefficients held; the removed key is the parameter-level `fixed`.

`r.shift` names a translation coefficient: evaluate densities and CDFs at
`x - shift`, and add the shift to quantiles and draws. Without `r.args`, the
shift coefficient is removed from the reference arguments. With `r.args`,
expressions explicitly determine the arguments. The translated
half families use internal generated functions taking `mu` directly; no shift map is needed.

README `fit_args` and golden JSON `fit_args` replay the same fit, preserving
explicit nulls. JSON represents infinite bounds as strings `"Inf"` and
`"-Inf"`; readers convert them to numeric bounds. No migration regenerates
the golden data, estimates, or log likelihoods.

Fitted objects retain `fixed` (held coefficients, used by degrees of freedom
and bootstrap replay), and carry `restricted` if returned by an adapter:
a named character vector from coefficient to `"lower"` or `"upper"`.
Print and summary display both.
`model_select` attaches `skipped`, a named character vector of families and
reasons for skipping missing required coefficients, to both return forms.

The conformance test checks adapter signatures, required diagnostics and each
holdable subset against the golden data or a declared reason. These checks
do not prove existence or global optimality on other samples.

## Support expressions and endpoint closure (R7a)

`support.lower` and `support.upper` accept numbers, `inf`/`-inf`, or quoted
R expressions in coefficient names. Supported operations are arithmetic,
comparisons, `if`, `min`, `max`, `floor`, `ceiling`, `log`, `sqrt`, `lgamma`,
`lbeta`, `dnorm` and `dt`; no assignment or arbitrary calls are accepted.
Examples: `lower: mu`, `upper: "if (xi < 0) mu - sigma/xi else Inf"`.
Expressions use original data coordinates, before `r.shift` is applied.

Coefficient-dependent continuous endpoints are closed (`closed: [true, true]`
when both depend on coefficients). Infinite bounds are never observations.
The density at a finite closed endpoint is the limit from inside. Optional
`endpoint_log_density: {lower: "expression", upper: "expression"}` declares
that log limit, allowing coefficient names and `x` (the endpoint), `pi`, and
`Inf`. Declare singular limits explicitly: a nearby density cannot distinguish
zero, a finite limit, and infinity reliably. Smooth undeclared limits use
linear extrapolation of two reference log densities just inside the endpoint.
CDFs and quantiles use the same resolved endpoints in both tails and log modes.

For discrete laws, declare the actual included lattice bounds. Truncated
Poisson retains the strict conditioning rule `a < x <= b`: its effective
closed lower endpoint is `max(0, floor(a)+1)` and upper is `floor(b)`. Closing
this lattice support does not add the excluded atom at `a`.

Generated metadata retains numeric envelope `bounds` for pre-fit validation
and backward-compatible object attributes, and `expressions`, `endpoint_closed`
and `endpoint_log_density` for evaluation at fitted coefficients. Internal
`univariateML_support(fit)` resolves the fitted support without changing the
construction of fit objects.

## Generated d/p/q/r

Assembly runs `bindings/r/scripts/codegen_dpqr.R` before Rcpp attribute
compilation. It writes R wrappers and Rcpp exports into `univariateML-r/`
for every non-`stats::` reference, using README coefficient names and order.
It does not change the selected reference. The resolver still applies `r.args`
and `r.shift` when invoking a selected reference. Random generators use R's
RNG through `src/rng_r.h`; an EMG R-host specialization in `src/rng_emg.h`
preserves its established normal/exponential primitive stream. Sample size
follows base R conventions. For a family
with a coefficient named `n`, sample size is named `nn`
to avoid duplicate R formals.

All generated d/p/q/r functions are internal, with required coefficients and no
coefficient defaults or roxygen topics. Use exported `dml`/`pml`/`qml`/`rml`
with a fitted or fixed object. Family-specific evaluation facts belong in a short
`## d/p/q/r` prose section, rather than export metadata.

All-held fits use the generated internal C++ density directly. Stats-dispatched
families also receive a private `core_d<id>` entry point for this purpose;
ordinary `dml`/`pml`/`qml`/`rml` dispatch remains as declared by `r.density`.
