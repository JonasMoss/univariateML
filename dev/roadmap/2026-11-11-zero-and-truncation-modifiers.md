# Zero modification and truncation as modifiers (2026-11-11)

Status: proposal, not scheduled. Asked for by Jonas on 2026-11-11: make zero
modification a knob on count families that start at 0, do the same for
truncation, organise tags and holds so that what exists is tracked, and
retire the separate zt/zm families in C++. Builds on the family recount in
[censoring and truncation](2026-11-05-censoring-truncation.md#families-that-become-redundant).

## Why this works

The zero-modified likelihood factorises:

$$
L(\theta, p_0) = p_0^{n_0}(1-p_0)^{n-n_0}
  \prod_{x_i > 0} \frac{f(x_i \mid \theta)}{1 - f(0 \mid \theta)} .
$$

With $p_0$ free, $\hat p_0 = n_0/n$ and $\theta$ is the zero-truncated fit on
the positive counts. Zero truncation is truncation to $\{1, 2, \dots\}$, and
the zero-truncated model is the zero-modified one with $p_0 = 0$ held. So zero
modification is a Bernoulli hurdle on top of truncation, and the per-family
work is the zero-truncated fit. The existing fitters already work this way:
`zero_detail::modified_mle(x, kind, ...)` in `distributions/zmpois/profile.hpp`
switches on `kind` (Poisson, geometric, NB, binomial, logarithmic), and
`zero_detail` itself sits in `distributions/ztpois/`.

Zero inflation (zip, zib, zinb) does not factorise. It is zero modification
restricted to $p_0 \ge f(0 \mid \theta)$: the zero-modified fit answers it when
it satisfies the constraint, and otherwise the zero-inflated MLE lies on
$\pi = 0$, which is the base family. It is part of the knob, but each family needs that boundary step.

## Proposed knobs

Two modifiers, each a property of the fit rather than a new family.

**`zero`** for counts starting at 0, with values

| value | model | extra coefficient |
|---|---|---|
| `"none"` (default) | base family | none |
| `"truncated"` | conditioned on $X \ge 1$ | none |
| `"modified"` | mass $p_0$ at 0, zero-truncated law on the rest | `p0` (holdable, R2) |
| `"inflated"` | mass $\pi + (1-\pi) f(0)$ at 0 | `pi` (holdable) |

**`truncate`** for any family: a window `c(lower, upper)` in data coordinates.
Default `NULL` (no truncation). A bound given as `NA` is estimated; its MLE is
the sample extreme for every parent, because $P(A)$ shrinks with the window.
Kind `design` under R4: it has to be supplied, so it never defaults to
"estimate".

For a while both knobs and the old families coexist; `zero = "truncated"` and
`truncate = c(1, Inf)` must give identical fits, and a test checks that.
Combining them (zero modification of an upper-truncated count) is refused at
first with `not-implemented`.

Window convention (decided, Jonas 2026-11-11): the retained set is
$A = [a, b] \cap \operatorname{supp}(f)$, closed for continuous and integer
families alike. For integers $A = \{\lceil a \rceil, \dots, \lfloor b \rfloor\}$
within the support and $P(A) = F(\lfloor b \rfloor) - F(\lceil a \rceil - 1)$;
zero truncation is `truncate = c(1, Inf)`. An infinite bound means that side is
unbounded. Empty $A$ and data outside $A$ are invalid input; a single retained
integer is a point mass with zero information (the existing tpois regime).
Reasons: a free bound's MLE $x_{(1)}$ is attained only with a closed window
(half-open would create a convention-made nonexistence), and integer windows
read naturally. extraDistr and the current `tpois`/`tbinom` use $(a, b]$; NEWS
states the change. Censoring and binning cells stay half-open (see the
censoring entry): they must partition the line, truncation windows need not.

## Where the knob appears

Eligibility is derived from front matter, never written by hand:

- `zero`: `support.type: integer`, literal `support.lower: 0`, and
  `construction` without `truncation` or `modification` (no double
  modification). This gives pois, binom, nbinom, geom, bbinom, bnbinom, cmb,
  cmp, dgamma, dweibull, pig. It excludes hyper and dunif (lower bound depends
  on coefficients), skellam, dlaplace and dnorm (zero is interior), and the
  modified families themselves. lgser keeps its zero-modified version (adds an
  atom at 0) as a declared exception; zeta and yulesimon could join the same
  way later.
- `truncate`: every family whose `dpqr.hpp` has tail-accurate log CDF and log
  survival, which per the accuracy contract is every family. Families that
  already are truncations (tnorm, tpois, tbinom, zipf) become the base family
  with `truncate`.

An eligible family always gets the argument in the generated R wrapper and in
`dml`/`pml`/`qml`/`rml`. When a value is not implemented, the wrapper stops with
a specific message ("`zero = \"modified\"` is not implemented for `mlcmp`;
see TASK-191 or its successor") and C++ returns a `NotImplemented` status. Ineligible families
do not get the argument, so passing it is R's usual unused-argument error.

## Metadata

A new front-matter block declares what is implemented; eligibility comes from
the rules above, status from this block, and anything eligible but absent is
`not-implemented`.

```yaml
modifiers:
  zero:
    truncated: {status: implemented, certified: "yes", note: "concave in log lambda (ztpois)"}
    modified:  {status: implemented, certified: "yes", note: "hurdle over truncated"}
    inflated:  {status: implemented, certified: "partial", note: "zip fitter"}
    limits:
      - {when: "truncated: $\\lambda \\to 0$", detect: "all positive counts equal 1", name: "point mass at 1", status: PointMass}   # status name illustrative
  truncate:
    status: implemented
    certified: "yes"
    note: "truncated exponential family, same natural parameter; concave"
```

- Statuses: `implemented`, `not-implemented`, `no-mle` (structural), matching
  `holds`.
- Modifier-specific escapes live under the modifier, with the same fields as
  top-level `limits`. The current zt/zm `limits` move here. Limits of the base
  family carry over to the truncated one through
  $T(\overline M) \subseteq \overline{T(M)}$; new ones ($\partial_T M$) are
  declared explicitly.
- Holds compose: a hold set of the base family plus `p0` held or free. A
  base-family `holds` entry applies under every modifier unless the modifier
  block overrides it.

## Tags and the capability table

Existing derived tags (support, coefficient kinds, existence) and written ones
(group, construction, areas, fit) stay. Additions:

- Derived modifier tags: `zero-modifiable` (eligible) and `zero-modified`
  (some non-`none` value implemented), `truncatable` and `truncation`
  (implemented). The catalog filters on them.
- `construction: truncation` and `modification` stop being family tags once
  the families retire; they become modifier facts.
- `group: modified counts` empties as the families retire. The modified
  variants appear on the base family's page, not as catalog entries.

The capability table is generated, not hand-written: one row per family and
fit configuration (hold subset × modifier value), with columns eligible,
status, certified, escape count and golden coverage. Source: front matter plus
the conformance results. Output: `dev/validation/capabilities.csv` and a site
facet. This gives one place to see what exists for both holds and modifiers,
and it replaces the hand-kept lists in this file and the 2026-11-05 entry.

## C++ layout

- Move `zero_detail` out of `zmpois/` and `ztpois/` into
  `core/zero_modified.hpp` (layout move, separate commit from any numerical
  change).
- Replace the `kind` switch with a per-family traits struct: log pmf, log of
  the zero mass, and a zero-truncated fitter, with the family's own status
  enum. A base family opts in by providing the traits in its folder
  (`distributions/pois/zero.hpp`).
- Generic `truncated<Traits>` fitter in `core/truncation.hpp`, with exponential
  family traits giving the concavity certificate (pois, binom, geom, zeta, norm,
  nbinom with size held). Zero truncation is this fitter with window
  $[1, \infty)$; families with a cheaper zero-truncated route keep it.
- Retire the directories `ztpois`, `ztbinom`, `ztgeom`, `ztnbinom`, `zmpois`,
  `zmbinom`, `zmgeom`, `zmnbinom` (and later `tnorm`, `tpois`, `tbinom`,
  `zipf`) once the knob reproduces their goldens. `zmlogarithmic` becomes
  lgser's declared exception. `zip`, `zib`, `zinb` become the `inflated`
  implementations of their base families.

## R names

CRAN 1.5.0 exported `mlzip`, `mlzipf` and `mllgser` only; the zt/zm/t wrappers
were never published. d/p/q/r are internal and generated, so extraDistr and
actuar names (`dztpois`, `dzmnbinom`, `dtpois`, `dzip`) do not bind us; users
know them, though.

Decision (Jonas, 2026-11-11): knobs only, in both C++ and R. No aliases;
`mlzip`, `mlzipf` and the zt/zm/t wrappers go, as a breaking change in the 2.0
release. Checked 2026-11-11: no CRAN package calls `mlzip` or `mlzipf` (GitHub
code search over the `cran` mirror; the reverse imports are ale, autogam,
EBcoBART, kdensity, peppwR, piecenorms, RealSurvSim, svines). NEWS gives the
replacement call for each removed wrapper.

## Truncation widens the parameter domain

`zipf` is `zeta` truncated to $\{1, \dots, N\}$, but its domain is
$s \ge 0$ while `zeta` needs $s > 1$: on a finite window the normaliser is
finite for every $s$. So `mlzeta(x, truncate = c(1, N))` must fit on a wider
domain than `mlzeta(x)`. In general a bounded window admits every parameter for
which the density is integrable on the window, and the truncated family can be
larger than $T(M)$ for the base family $M$. The same happens for a normal
truncated to a bounded window, whose closure adds the truncated exponential and
the uniform as limits.

Requirement: each family's `modifiers.truncate` block declares its domain under
a bounded window (`domain_bounded`), and the limits of the truncated family are
assessed separately rather than inherited. zipf retires only when
`mlzeta(x, truncate = c(1, N))` reproduces its goldens, including fits with
$s \le 1$.

## Current coverage (counts starting at 0)

| base | truncated | modified | inflated |
|---|---|---|---|
| pois | ztpois | zmpois | zip |
| binom | ztbinom | zmbinom | zib |
| nbinom | ztnbinom | zmnbinom | zinb |
| geom | ztgeom | zmgeom | — |
| lgser (from 1) | n/a | zmlogarithmic | — |
| pig, cmp, bbinom, bnbinom | — | — | — |
| cmb, dgamma, dweibull | — | — | — |

Candidates by use: pig, cmp, bbinom, bnbinom first (zero-inflated and hurdle
versions of these appear in applied count modelling; not checked here); cmb,
dgamma and dweibull marginal.

## Suggested order

1. Layout: `core/zero_modified.hpp`, traits for the four existing bases, with
   all existing zt/zm goldens unchanged (layout only).
2. Metadata: `modifiers` block, derived eligibility and tags, generated
   capability table; the knob in generated wrappers with `not-implemented`
   errors.
3. Generic truncation fitter with exponential-family certificate; zero
   truncation through it, checked against the existing zt fitters.
4. New bases: pig, cmp, bbinom, bnbinom, each with zero-truncated limits
   worked out first.
5. Retire the zt/zm/zi/t directories and zipf, and remove their R wrappers.

## Trigger to revisit

TASK-150 (accuracy contract, uniform
controls) is done, since that also changes every wrapper signature.

## Verification

- Every zt/zm/zi/t golden reproduced by the knob to the accuracy contract
  before its directory retires.
- `zero = "truncated"` and `truncate = c(1, Inf)` agree on every eligible
  family.
- `zero = "modified"` with every coefficient held at the base fit and
  `p0 = f(0 | theta_hat)` evaluates to the base fit's log likelihood; with
  `p0 = 0` held it reproduces `"truncated"`.
- The capability table regenerates without diffs after `just regen`, and
  every `not-implemented` cell raises the documented error in a conformance
  test.
