# Parameter rules: design (2026-10-29)

Status: rules agreed with Jonas on 2026-10-29: R1–R8 and choices C1–C4 as
proposed. Jonas left C5 and C6 open; this design picks them (marked
below). Breaking changes are not a constraint; they are handled at
release. Evidence: `notes/evaluations/2026-10-29-parameter-conventions.md`
and `2026-10-29-parameter-table.csv` (98 families, 235 coefficients, 618
hold subsets in total, at most 5 coefficients per family).

## Rules

**R1. Coefficients.** A family's coefficients are the reference density's
parameters, under its names (own d/p/q/r: the literature's standard
form). Two extensions are allowed and must be declared:
- a translation coefficient (`kind: threshold`, `r.shift`) for a family
  whose reference has no location, when the translated family is needed
  (C5);
- a reparametrization when a held design quantity is a function of the
  reference parameters, e.g. the hypergeometric population size
  $N = m + n$ (`r.args` expressions, C6).

**R2. Holding.** Every coefficient can be held (`p = value`) or
estimated (`p = NULL`), in any combination. A combination with no MLE for
any sample stops and gives the reason.

**R3. Defaults.** Each coefficient either defaults to "estimate" or is
required: the user passes a value or `NULL`. No coefficient defaults to a
held value.

**R4. Required coefficients** are those of kind `design` (fixed by how the
data were collected) and `threshold` (a shift of a family whose standard
form starts at a fixed point; C1: required, no default). Kind `endpoint`
(support bounds of a family defined by its support) and `regular` are
estimated by default.

**R5. Restrictions.** When the family has no MLE for almost any sample,
restrict a shape parameter, for existence only (not for regularity; C2).
Mechanisms:
- *collapse*: with an observation repeated $k$ times, the likelihood
  behaves as $\sigma^{c(n-k)-k}$ as $\sigma \to 0$, where $c$ is a
  collapse exponent. With tie multiplicity below $n/2$, $c \ge 1$ is the
  weakest restriction valid for every $n$.
- *spike*: the density concentrates at an observation as a shape tends to
  an edge (GED $\nu \to 0$). No weakest bound exists; the restriction
  ends at a named member.
- *branch*: the family changes character past a point (Tukey
  $\lambda > 1$).
- *endpoint*: an upper endpoint approaches the sample maximum (GEV, GPD
  $\xi < -1$); the existence bound is attained.

A restriction may apply only while some coefficients are free (Fréchet
$\lambda \ge 1$ only with `mu` free).

**R6. Data-dependent non-existence** stops with "use `mlX`" (D1) or the
reason, never with a non-maximizer. This is unchanged for escapes, but it
conflicts with CLAUDE.md's "Wrapper failure handling" (nlminb legacy:
return whatever BFGS landed on after an iteration-limit or line-search
failure). TASK-77 showed the cost: `mlstd` returned its start values
after both line searches failed. Proposal (P1): R6 wins. A failed
optimizer stops with its status unless the score check (the `||g|| <
1e-3` gate the doctests use) passes at the returned point.

**R7. Domains** are the values the reference accepts. A fit at a natural
endpoint (binomial `prob = 0`) is an attained member, returned silently.
A fit on an R5 restriction is flagged (C3): a warning of class
`univariateML_restricted` and a `restricted` attribute.

**R8. Equivariance.** Fits commute with the family's location-scale maps
(TASK-77).

**Choices made in this design.**
- **C5:** hnorm, hcauchy and ht are all translated, through `r.shift` on
  extraDistr's `dhnorm`, `dhcauchy` and `dht`. hnorm then drops its
  `dtnorm` mapping and the `mlhnorm` hardcode in
  `univariateML_to_function`. The alternative, no translation, loses the
  translated half-normal that skew-normal escapes point to.
- **C6:** hyper and nhyper take the population size `N` as a design
  coefficient, with `n = N - m` mapped through `r.args`. The alternative
  (hold `n`) cannot express the usual design of a known population size
  and an unknown composition.
- **C4:** where the reference takes either rate or scale, the coefficient
  is the scale.

## Parameter kinds

| kind | default | coefficients |
|---|---|---|
| `design` | required | binom, bbinom, cmb, tbinom, zib, zmbinom, ztbinom `size`; hyper `k`, `N`; nhyper `r`, `N`; tnorm `a`, `b`; tpois, tbinom `a`, `b`; gpd `mu` |
| `threshold` | required | hnorm, hcauchy, ht, frechet, fatigue `mu`; pareto2, pareto3, pareto4, fpareto `min`; nsbeta `min`, `max`; genbeta `scale` (upper endpoint) |
| `endpoint` | estimated | unif, dunif `min`, `max`; pareto `b`; power `alpha`; zipf `N`; triang `a`, `b` |
| `regular` | estimated | all others (shape, scale, rate, location, probability) |

`NULL` for a required coefficient estimates it where supported:
- design: binomial `size` (with the Poisson escape), truncation bounds
  (the sample extremes), hypergeometric `N` (with `m` held);
- threshold: hnorm (the MLE exists); Fréchet and ht (under $c \ge 1$);
  hcauchy ($c = 1$, to be confirmed in M2); others after M2.

Unsupported cases stop with the reason.

## Restrictions after the rules

| family | restriction | mechanism | applies while |
|---|---|---|---|
| lst | `df >= 1` | collapse, $c$ = df | `mu` free |
| frechet | `lambda >= 1` | collapse, $c = \lambda$ | `mu` free (`mu = NULL`) |
| ht | `nu >= 1` | collapse, $c = \nu$ | `mu` free (new) |
| gev | `xi <= 1` | collapse, $c = 1/\xi$ | always (the endpoint moves with `sigma`; M3) |
| gev, gpd | `xi >= -1` (now $-1/2$) | endpoint | always |
| tlambda | `lambda >= -1` | collapse, $c = -1/\lambda$ | `mu` free |
| tlambda | `lambda <= 1` | branch | always |
| ged, sged | `nu >= 1` | spike, Laplace member | always |
| pareto2–4, fpareto, gpd, nsbeta, genbeta | shape $\ge 1$ ($c$) | collapse at the endpoint | threshold `NULL` (M2) |

## Schema (`README.md` front matter)

- `kind:` per parameter (`regular` when absent).
- `default:` is renamed `simulate:` (the simulation value for goldens and
  `simulate_default`); the fitting default follows from `kind`.
- `fixable`, `fixed` and `required` are removed (all coefficients are
  holdable; `required` follows from `kind`). `required_reason` becomes
  `advice:`, the family-specific text appended to the missing-coefficient
  message (e.g. "`mu = 0` is `mlinvweibull`").
- `fit_domain:` per parameter (the R5 restriction) with
  `restriction: {mechanism, while_free: [...], reason}`.
- `holds:` exceptions to R2, each with
  `{held: [...], status: no-mle | not-implemented, reason}`.
  The target is no `not-implemented` entries.
- `r.shift: <coefficient>`: d/p/q/r evaluated at `x - mu`, quantiles and
  draws shifted back.
- `r.args` values may be expressions in the coefficients (`n: "N - m"`).
- Codegen checks:
  - every coefficient is an adapter argument;
  - required coefficients have no adapter default;
  - all other coefficients default to `NULL`;
  - `kind` and `fit_domain` agree with `limits:`.

Alternatives considered: declaring `required` per parameter (status quo)
lets families drift apart again; deriving it from `kind` makes R4
mechanical and leaves one judgement per coefficient (its kind).

## R interface

- **Wrappers** distinguish a missing argument from `NULL`; the current
  `is.null(supplied[[p]])` check treats `NULL` as missing. A missing
  required coefficient stops with the kind, both ways out and the
  advice, e.g.:

  "`mlfrechet` requires `mu` (threshold). Hold it, e.g. `mu = 0` (the
  two-parameter Fréchet, `mlinvweibull` on `x - mu`), or estimate it with
  `mu = NULL` (restricted to `lambda >= 1`)."
- **Attributes:**
  - `fixed`: the held coefficients, design ones included; unchanged
    semantics, degrees of freedom count the free ones;
  - `restricted`: new, a named character vector (coefficient to
    `"lower"`/`"upper"`); `print` and `summary` show both.
- **`bootstrapml`** replays holds from `fixed`. With C6 every held
  quantity is a coefficient, so the `mlhyper`/`mlnhyper` gap closes.
- **`model_select`** passes `...` to every fitter and drops failures
  silently. Families whose required coefficients are not supplied should
  be skipped, and listed in a `skipped` attribute.

## C++ core

- Every `*_mle` takes every coefficient as a hold argument (NaN = free,
  the existing `Rcpp::Nullable` to NaN convention), in coefficient order,
  then `tol`, `maxit`.
- `*Result` gains `unsigned restricted`: a bitmask, bit $2i$ for lower and
  $2i+1$ for upper, of coefficients on an R5 restriction. Status enums are
  unchanged; a restricted fit has status `Ok`.
- Restriction constants live in the family's `mle.hpp`; a test checks
  them against `fit_domain`.

## Tests

- **Conformance** (new, generic, from metadata, every family):
  - required set = design ∪ threshold; missing stops naming it; `NULL`
    accepted where supported;
  - hold sweep: for every subset of coefficients (618 in total), hold the
    subset at the golden estimates on the golden data. Either the fit
    returns the held values exactly, its free coefficients finite and
    its logLik at least that of the golden fit minus tolerance, or it
    stops with a reason declared under `holds:`;
  - restrictions: a sample generated beyond each `fit_domain` bound
    gives a `univariateML_restricted` warning and the attribute.
- **Equivariance** (TASK-77).
- **Quality harness:** the oracle honours `fit_domain` and holds.

## Work plan (mathematics first)

**P0, analysis (Codex research jobs by family group).**
- **M1:** for every family and hold subset: an MLE always, data-dependent
  (which escape), or never; held subsets with a different existence
  story than the free fit.
- **M2:** threshold `NULL` fits under $c \ge 1$: gpd, pareto2–4, fpareto,
  nsbeta, genbeta, ht, hcauchy; fatigue's mechanism (unknown).
- **M3:** GEV and GPD on $[-1, -1/2)$: existence, attainment, profile
  shape; whether GEV's `xi <= 1` holds with `mu` held.
- **M4:** escape frequency of huber with free `epsilon` (now the default)
  on normal and Laplace samples.

Output: one table per group in `notes/evaluations/`, which feeds the
`holds:` entries.

**P1, infrastructure.**
- Schema keys, codegen derivation and checks, wrapper missing vs `NULL`,
  the `restricted` attribute and warning, `r.shift`, `r.args`
  expressions, the conformance test.
- READMEs are migrated by script: `kind` assigned from the table above,
  `default` renamed to `simulate`, current gaps listed as `holds:`
  `not-implemented`.
- Behaviour unchanged except R3/R4 defaults. Goldens get `fit_args` for
  the newly required coefficients.

**P2, holds.** Implement the `not-implemented` subsets group by group from
M1:
- location-scale;
- positive scale-shape;
- generalized beta and Burr;
- extreme value;
- discrete counts;
- binomial-type;
- own families.

Each group is one Codex job, and the conformance list shrinks to zero.

**P3, restrictions.**
- C2 bounds ($-1$) for gev and gpd;
- `restricted` flags in the eight restricted fitters;
- threshold `NULL` fits from M2.

**P4, parametrization.**
- C4: gamma, invexp, invweibull and llogis to scale;
- C5: hnorm, hcauchy and ht through `r.shift`;
- C6: hyper and nhyper with `N`;
- metadata slips (zmnbinom and ztnbinom `size` role; display names).

**P5, documentation.**
- `README_SCHEMA.md` and CLAUDE.md "Adding a new distribution";
- NEWS (all default changes in one list);
- `notes/ml-behaviour.tex` (R5 mechanisms).

## Interaction with TASK-78 (own d/p/q/r for every family)

TASK-78 (needs a decision, from another session) would replace the
extraDistr and actuar references with the package's own d/p/q/r.
- **R1:** if TASK-78 goes ahead, R1 anchors on the former reference's
  parametrization and names, so the coefficients do not change.
- **`r.shift` and `r.args` expressions:** they become properties of the
  generated wrappers and the uniform C++ d/p/q/r signatures (TASK-78
  step 0), not mappings onto external functions.
- **Codegen:** P1 and TASK-78 step 0 both rewrite codegen (README keys,
  generated wrappers, Rcpp exports). Do them as one schema change, or P1
  first, so the README keys are migrated once.
- **Tests:** the conformance test does not depend on the references.

## Consequences for users (for NEWS at release)

- Calls that will stop without the new required coefficient:
  - `mlbinom`, `mlzib`, `mlzmbinom`, `mlztbinom` (`size`);
  - `mltnorm` (`a`, `b`);
  - `mlgpd`, `mlhnorm`, `mlhcauchy`, `mlht`, `mlfrechet`, `mlfatigue`
    (`mu`);
  - `mlhyper`, `mlnhyper` (`N`).
- `mlhuber` estimates `epsilon` by default.
- gamma, invexp, invweibull and llogis report `scale`, not `rate`.
- gev and gpd accept $\xi \in [-1, -1/2)$.
- Fits on a restriction warn.
