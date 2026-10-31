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
| `references` | no | one bibliographic string per item; verified DOIs may be appended; strings pass to R help unchanged |
| `summary` | no | one sentence for the catalog and page lead; does not replace the R description |
| `classification` | yes | `classical`, `specialist`, or `uncommon`; requires `classification_reason`, a one-line account of use or standardness, never a popularity score |
| `classification_reason` | with classification | qualitative reason for the classification |
| `group` | yes | primary catalog group; controlled vocabulary below |
| `construction` | yes | nonempty list of construction tags; vocabulary below |
| `relations` | yes | immediate identities and construction edges; `{}` if none declared |
| `fit` | yes | primary `method`, `certified` strength and scoped `note` |
| `areas` | yes | one to three application areas; vocabulary below |
| `aliases` | no | list of verified alternative names; an empty list is allowed |
| `key_references` | no | one to three distinct one-based indices into `references`; selects original, standard, or estimator sources without duplicating or changing reference strings |
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

## Presentation metadata (TASK-101.6.4)

Each coefficient may have `symbol`, a single-quoted TeX symbol without math
fences, e.g. `symbol: '\mu'`. Symbols must be distinct within a family and
must not be `x` (the observation). They describe the declared parameterization,
not a change of coordinates. Related families use matching notation where their
parameterizations agree. API names and symbolic `log_density` remain unchanged.
The site substitutes symbols during TeX printing and in parameter tables/notices;
the numerical harness continues to use API names.

The optional catalog fields and symbols are accepted by the R loader but do not
enter R help. `references` remains a list of strings, including its order; the
key selection affects only the site. Bibliographic corrections and verified DOI
additions therefore reach R help through the existing reference pipeline.

A `limits` record may use `kind: nonexistence`, omit `model`, and name a
sample-dependent unbounded likelihood. Supply `when`, `detect`, `name`, and
`status` as usual. This records an R6 failure rather than inventing a limit law;
R help describes the non-existence directly. Use `when` and `detect` as clauses
without terminal punctuation. Existing records remain valid. `fixed` scopes
held coefficients; conditions must also state which coefficients remain free
when that determines non-existence. Numerical inability to certify a boundary
is not itself a proof of non-existence.

## Family taxonomy (first version, 2026-10-31)

These presentation fields are required for the canonical catalog dossiers and
do not change R help or fitter behaviour. For compatibility, minimal codegen
clients may omit the entire taxonomy block; any supplied taxonomy requires
all five fields plus classification/reason and undergoes full validation.
Classification and its reason are populated for every family. Application and
prevalence assignments are editorial first-version judgements, not a usage survey.

- `group`: one value from `Pareto and Lomax`, `Poisson, binomial and negative binomial`, `beta and generalised beta`, `bounded continuous`, `discrete heavy-tailed`, `extreme value`, `finite sampling`, `gamma and Weibull`, `modified counts`, `normal and Student`, `normal convolutions`, `other positive`, `skewed and robust location-scale`.
- `construction`: a nonempty unique list from `base`, `compounding`, `discretisation`, `folding`, `generalisation`, `location-scale`, `modification`, `skewing`, `transformation`, `truncation`.
- `areas`: a nonempty unique list of at most three from `actuarial`, `counts and ecology`, `economics and income`, `extremes and environment`, `finance`, `measurement and errors`, `proportions`, `rank-frequency`, `reliability and survival`, `robust modelling`, `sampling and quality control`, `waiting times and queues`.
- `fit`: `{method, certified, note}`; method is `closed-form`, `scalar-solve`,
  `profile-search`, `multistart`, `integer-search`, `delegation`, or `convex-solve`.
  The last is a plan extension for the grouped-normal joint concave solve. This names
  the primary route; held subsets can use others. `certified` is a quoted string
  `yes`, `partial`, `no`, or `unknown`; YAML booleans are invalid. Use `unknown`
  only when the fitting evidence is genuinely absent. `note` states scope and evidence;
  certification is about fitting, not inference regularity or coverage.
- `relations`: a map (empty `{}` allowed) with lists `special_case_of`, `transform_of`, `truncation_of` and/or
  `modification_of`, `convolution_of`, `mixture_of`, and/or `discretisation_of` (defined below). Ordinary one-parent edges have `family` (an existing different id) and `at`,
  mapping **every parent coefficient** to a numeric constant or pure arithmetic
  expression in child coefficient names. Neither display symbols nor undeclared
  variables are allowed. Example: exponential has
  `{family: gamma, at: {shape: 1, scale: 1/rate}}`.
  A transform additionally has `transform`: `log`, `logit`, `inverse`, `power`,
  `negate`, `abs`, `shift`, `exp`, `expit`, or `affine`. It means `T(X)` has the parent law. `power`
  requires a `power` expression (currently positive exponent and positive support);
  `shift` requires a `shift` expression and means `X - shift`. `abs` means
  the child is the absolute value of a full-real parent, summing both branches.
  Other transforms include the absolute Jacobian. Existing `limits` remain
  separate asymptotic claims; this check does not verify them.

`just relations` checks every declared edge from `log_density`, including
mapped support bounds. Continuous endpoint inclusion is interpreted modulo
null sets; discrete endpoint inclusion must match exactly. Symbolic equality
is distinguished from numerical agreement (24 seeded interior samples, relative
log-density tolerance `2e-9`). Numerical checks are regression evidence, not proof.

Derived site tags: `support_tags` contains one of `real`, `positive`, `unit`,
`bounded`, `counts`, `finite-counts`, plus `dependent` when bounds reference
coefficients. Coefficient tags contain their count and distinct kinds.
Existence tags are additive: `escapes` for a named limit law, `restricted` for
any fit-domain restriction, `needs-held` for `no-mle` holds, `partial` for
`not-implemented` holds. `degenerate-only` applies when all nonexistence
records explicitly concern constant/all-zero/collapsed samples and none of the
other tags apply. `always` means no declared obstacle; it is a metadata
default, **not an existence theorem**. Unclassified unnamed failure records
are conservatively tagged `partial` rather than asserting `always`.

### Taxonomy round two (2026-10-31)

`normal convolutions` is an additional group for EMG and Bhattacharjee:
their independent-component construction is more informative than their
full-real support. Folding retains the parent family's primary group.
Groups are navigation choices and need not partition mathematical constructions.

Certification is assigned from the actual default and documented held fits:
`yes` requires closed forms or a unique solution with a monotone equation,
certified bracket, concavity argument or exhaustive bounded search for every
supported subset; `partial` identifies specific certified subsets or regions;
`no` means the nontrivial fitting routes remain numerical searches;
`unknown` is reserved for genuinely missing evidence. Unsupported subsets and
numerical precision limitations must be named. All-held density evaluation alone
does not justify `partial`. A converged local score is not a global certificate.
Each note cites a file and section; no tag establishes inference validity.

Additional density relations (the complete-map rule still applies):

- `truncation_of`: the child is the parent conditioned on the child's declared
  support. No extra bound fields are needed. Its density is the parent density
  divided by its mass on that support. Supports must have the same base measure;
  the conditioning support must be a nonempty subset. Integer bounds are the
  effective included lattice bounds, so strict lower truncation stays strict.
- `modification_of`: integer zero modification, with `type` equal to
  `zero-truncated`, `zero-modified`, or `zero-inflated`. If parent mass at zero
  is q, zero-truncated positive masses are f(k)/(1-q). Zero-modified additionally
  requires `mass`, an expression in child coefficients: zero has that mass p,
  positives have (1-p)f(k)/(1-q). Zero-inflated uses zero mass p+(1-p)q and
  positive masses (1-p)f(k). These formulas are checked at interior parameters;
  zero-denominator boundary extensions require separate family tests.
- An `abs` transform may have `shift`, meaning the child is shift + |parent|.
  Both inverse branches and the shifted support are checked. For example,
  hnorm maps to norm with mean=0, sd=sigma and shift=mu.

The checker integrates the mapped parent density on continuous truncation
supports and sums it on finite integer supports at 35-digit working precision.
Zero modifications use the mapped parent's zero mass and test both zero and
positive observations. The 24 seeded parameter neighbourhoods and `2e-9`
relative log-density tolerance provide regression evidence, not a global proof
or validated interval arithmetic. New edge types appear in both directions in
Related families and as declared-relation catalog facets.

### Construction edges and conditions (2026-10-31)

Any edge may have `when`, a scalar logical expression in child coefficients
using comparisons and `&&` / `||`. It restricts the identity, not the family
or fit domain. Verification accepts only valid child draws satisfying it;
24 accepted draws are required within 480 attempts. Empty or unreachable
conditions fail. The graph uses dotted lines and retains the condition in
its title. Maps admit pure `exp` and `expit` expressions as well as arithmetic.

- `convolution_of`: `components` contains exactly two `{family, at}` maps,
  `operation` is `sum` or `difference`, and optional `shift` defaults to zero.
  All components are independent: X = shift + A + B or shift + A - B.
  `family` repeats the first component id for compatibility with one-parent
  consumers; there is no top-level `at`. Both components must have the child's
  base measure. Every component coefficient must be mapped.
- `mixture_of`: `family` is the conditional parent, `at` its complete map,
  `mixed` names its one randomized coefficient, and `mixing` is a complete
  `{family, at}` map for a continuous mixing law. The reserved symbol `mix`
  may occur only in the `mixed` entry of the parent map. It is the random
  variate drawn from the mixing law, not necessarily the coefficient itself.
  Optional `shift` means X = shift + Y. For slash, Y given mix is
  Normal(mean=mu, sd=sigma/mix), with mix Uniform(0,1) and mixed=`sd`.
- `discretisation_of`: complete ordinary `family` and `at` map; X=floor(Y)
  for a continuous parent. The child's integer support must agree with the
  floor image. Its mass is the integral of the parent density over [k,k+1].
- `exp` and `expit` mean exp(X) and 1/(1+exp(-X)) have the parent law.
  `affine` requires `shift` and positive `scale`, meaning
  (X-shift)/scale has the parent law. Inverse images and absolute Jacobians
  are checked. Reverse log/exp and logit/expit arrows must not both be declared.

Construction checks use 35-digit mpmath direct quadrature or infinite discrete
summation, with 24 seeded accepted coefficient draws and relative log-density
tolerance `2e-8`. Existing edges retain `2e-9`. This is numerical regression
evidence, not a proof, tail-error certificate or interval integration. Domain,
map and support errors fail explicitly. Conditional parent maps are checked
at every mixing quadrature node. Graphs show each distinct component and
mixing family; repeated identical-family components remain one graph arrow
with both maps preserved in its title.

The immediate-edge check rejects special-case paths, a same-transform path
with special-case steps, and an analogous one-construction path for the three
new construction kinds. Conditional steps never establish an unconditional
redundancy. This conservative graph policy does not attempt symbolic implication
between conditions or general composition of multivariate constructions.

For mixtures, the checker additionally composes a child shift or special-case
map with one mixture and an optional parent special-case embedding. It compares
all composed parent coefficients, the mixing-family map and the total shift
symbolically. Thus Yule--Simon's shifted beta-geometric mixture is inherited
from its beta-negative-binomial relation, beta mixing of negative binomial,
and the geometric size-one member; no redundant direct mixture arrow is kept.

## Discrete quantile endpoints (TASK-134, 2026-10-31)

For an explicit finite design support, quantiles at lower-tail probability zero
and one return its nominal minimum and maximum, respectively, even when a
parameter makes one of those endpoints have zero mass. Upper-tail probabilities
reverse these endpoints; log probabilities use the identical rule. Interior
probabilities invert the actual CDF. Positive truncation has minimum one;
zero modification includes zero even at zero modification mass. An unbounded
law that collapses to finitely many atoms uses its actual last atom as maximum.

This keeps endpoint queries tied to the supplied design bounds and preserves
parameter continuity of those endpoint queries through collapse. It matches
R's `qbinom(c(0,1), 10, prob)` convention: both `prob = 0` and
`prob = 1` return the nominal endpoints zero and ten (checked in R 4.6.1). Interior
quantiles need not be continuous at an atomic probability jump. A nominal
endpoint is a quantile convention, not an assertion of positive endpoint mass.

An inclusive coefficient bound does not override the requirement that a
conditioning event have positive probability. The tpois/tbinom dossiers exclude
zero-probability conditioning events; their archived positive-truncation weak
limits are not public members. The ztpois/ztbinom dossiers separately include
those limits.
