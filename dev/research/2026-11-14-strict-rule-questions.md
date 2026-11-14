# Strict layer: the five open rule questions (2026-11-14)

Decision memo for Jonas on the open rule questions in section 2 of the
[strict-2 report](evaluations/2026-11-13-strict-2-report.md), with its outcome codes
(`E`, `L`, `N:`, `F:`) and appendix table (`c167a975` baselines). Sources:
[`outcome.hpp`](../../univariateML-cpp/include/cppunivariate/core/outcome.hpp), the
[consolidation plan](../design/consolidation.md), the
[accuracy contract](../design/accuracy-contract.md),
[`fit-consistency.py`](../validation/fit-consistency.py), the classes in `fit-snapshot.py`
and the dossiers named below. No fits were run; *sketch* marks my own unchecked arguments.

## Three facts used throughout

**F1. One observation is constant data.** `[m, m, m, m]` has the fourth power of the
likelihood of `[m]`, hence the same outcome (the existing `one-constant` rule).

**F2. Equivariance.** A family is *affine-equivariant* if $a + bX$ ($b > 0$) stays in it
with coefficients mapped by a bijection of the fitting domain (location and scale free;
shape restrictions do not matter), and *power-scale equivariant* if $aX^b$ does (positive
data, scale and power free). The image sample's likelihood is the original times a
constant at mapped coefficients, so kind, reason and uniqueness agree. Hence:

- near-constant $[c, c, c^+]$ is an exact image of the well-scaled
  $[q_{50}, q_{50}, q_{75}]$ (power-scale: $x \mapsto ax^b$,
  $b = \log(q_{75}/q_{50})/\log(c^+/c)$); only the coordinates are bad;
- every two-point sample is an image of every other: its outcome is a family constant.

Under the golden holds, affine: cauchy, emg, frechet, gev, huber (epsilon held), lst, sn,
slash, sstd, st, std, tlambda, triang. Power-scale: burr, expweibull, fpareto and pareto4
(min held at 0, power `shape2` free), gb2, gengamma, invburr, invtrgamma, trgamma. Not
equivariant beyond scale: betapr, fatigue (mu held), genbeta (scale held), gompertz,
invparalogis, kumar, paralogis, sgomp, gamma, beta. Read off the coefficient structure;
each claim needs the check proposed at the end.

**F3. Collapse at a tie.** For a location-scale family with standardized density positive
and finite at the tie and tails $g(z) \asymp |z|^{-1-\alpha}$, putting the location on a
value of multiplicity $k$ (sample size $n$) and letting the scale go to 0 gives
$L \asymp \sigma^{e}$ with $e = \alpha(n-k) - k$ (stated in the cauchy, slash, lst, st,
std and frechet dossiers). Unbounded if $e < 0$ for some admissible $\alpha$; a finite path
limit at $e = 0$; tails lighter than every power collapse only on constant data. Classes:
`ties` = `[q50]*4 + [q25, q75]` ($n = 6$, $k = 4$: two thirds, not half), near-constant
($3, 2$), and `half-ties` ($6, 3$), which `fit-consistency.py` does not tabulate.

| family (fitting domain) | $\alpha$ | ties (6, 4) | near-constant (3, 2) | half-ties (6, 3) |
|---|---|---|---|---|
| cauchy, slash | 1 | $-2$: unbounded | $-1$: unbounded | $0$: critical |
| lst, st ($\nu \ge 1$) | $\nu$ | $-2$ at $\nu = 1$ | $-1$ | $0$ |
| tlambda ($\lambda \ge -1$) | $-1/\lambda$, $\lambda < 0$ | $-2$ at $\lambda = -1$ | $-1$ | $0$ |
| std, sstd ($\nu > 2$) | $\nu$ | $> 0$, $\to 0$ as $\nu \downarrow 2$ | $> 0$, $\to 0$ | $> 0$ |

tlambda (*sketch*): at $\lambda = -1$, $Q(p) = 1/(1-p) - 1/p$, so the density
$1/(p^{-2} + (1-p)^{-2}) \le 1/8$ has an $x^{-2}$ tail. Ties and near-constant are both
*critical in the limit* for std and sstd, which is why these two differ from cauchy, lst,
slash and st.

## Q1. Near-constant data

**Today** (continuous class; for counts `[c, c, c+1]` is an ordinary sample): 52 E, 20 L;
`N:unbounded` in cauchy, slash, st (F3), frechet and gev (their dossiers: minimum repeated
over $n/2$ times), all correct; 22 numerical failures, gb2 `unsupported`, tlambda
`invalid-input`. gamma, weibull, logis, gumbel (and norm on master) return `E` with a
`logLik` off the density.

**Mathematics.** 16 of the 24 failing families are equivariant (F2): their problem *is* the
well-scaled three-point one. The maximizer is representable to the contract (the location
is off by one spacing, about $10^{-16}|c|$, inside the floor $\max(|\mu|, \sigma)$), and
standardizing is exact ($x_i - c$ by Sterbenz). Only `logLik` suffers, by the contract's
steep-maximizer case (decided 2026-11-14). A numerical failure there is a formulation
defect. By F3, lst and tlambda must end `N:unbounded`, std and sstd are critical, the rest
are E or L by their own comparisons. The 8 others (betapr, fatigue, genbeta, gompertz,
invparalogis, kumar, paralogis, sgomp) have extreme coefficients, sometimes representable
(gamma: shape about $10^{31}$), sometimes not. kumar: the dossier proves a unique finite
MLE with $\hat b = -n/S(a)$, $S(a) = \sum_i \log(1 - e^{-a t_i})$; spread $\epsilon$ needs
$a \approx 1/\epsilon$, so $\hat b \approx c^{-a}$ overflows binary64, and
`failure(evaluation)` ("an unrepresentable result") is correct (today: `no-convergence`).
gompertz and sgomp probably have a coefficient of order $e^{-1/\epsilon}$ unless a Gumbel
face wins (*sketch*, not settled); the other five are not settled. The contract permits
`conditioning` and `tolerance` as honest accuracy stops.

**Proposed rule `near-constant`** (continuous class): never `no-convergence`,
`evaluation`, `invalid-input`, `invalid-argument` or `unsupported`; E, L, N, B,
`conditioning`, `tolerance` and `unresolved` are allowed. Second part, for families that
declare equivariance: the same kind and reason as a new class `tie3` =
$[q_{50}, q_{50}, q_{75}]$, except that near-constant may also stop on `conditioning`.

**Exceptions:** `evaluation` where the dossier shows a public coefficient of the maximizer
outside binary64 on this class, declared with the argument (kumar now; gompertz, sgomp once
checked). The wrong-`logLik` estimates are no reason to allow numerical failures: they fail
the density check, and the contract's remedies (log likelihood at the returned
coefficients, or a conditioning stop) both pass this rule.

**Would fail:** all 24 (the 22 numerical failures, gb2, tlambda); kumar passes once it
reports `evaluation` under its exception.

**Recommendation:** both parts. Alternatives: (a) forbid only `no-convergence`, which
passes the ten `evaluation` cells, nine of them formulation defects in equivariant
families; (b) require E, L or N, against the contract's `conditioning`; (c) no rule,
leaving it to the density and refinement checks, which judge only estimates and limits.

## Q2. Sample-size restrictions

**Today:** `one` is `invalid-input` in burr, frechet, gev, invburr, invtrgamma, pareto4,
sn, sstd, tlambda, trgamma (whose `constant` is `N:unbounded`) and in fpareto, gb2, kumar
(whose `constant` is refused too); `two` is refused in frechet, gev, tlambda. Stated
reasons: frechet's joint fitter "requires at least three observations", burr's all-free
certificate two, tlambda's attainment theorem three.

**Mathematics.** By F1, $n = 1$ is constant data, and in all 13 families the constant
outcome is `N:unbounded` (in the table, or in the dossier for burr, kumar, gb2, and fpareto
with its beta shapes held as in the golden fit). One observation is therefore decided
everywhere; routing it to the constant-data path suffices. "Nonexistence for one
observation with a free scale" is not general (exp, rayleigh, hcauchy, invexp and gpd have
an MLE): what decides is whether the family can concentrate at a point (F3 with $k = n$).
A valid sample is not `invalid-input` by definition; `unsupported` is defined for hold
subsets, but "not implemented" is the honest content of a size limit.

**Proposed rule:** `one-constant` and `valid-data` stand; `valid-data` also forbids
`unsupported` on a supported hold subset, except below a declared `existence.min_n`
($\ge 2$, with reason). The `Unsupported` comment becomes "a hold subset, or a sample below
the documented minimum size, the family does not implement".

**Exceptions:** none for $n = 1$; `min_n` only where the mathematics is open (Q3).
**Would fail:** all 13 until $n = 1$ takes the constant path; frechet and gev on `two`
(decided by their dossiers, Q3); tlambda on `two` unless it declares `min_n: 3`.

**Recommendation:** the mathematical outcome for $n = 1$, a declared `unsupported` below a
documented minimum otherwise. Alternatives: (a) the mathematical outcome everywhere, no
`min_n`, which blocks on open two-point questions; (b) keep `invalid-input`, which calls
valid data invalid; (c) a new reason `SampleSize`, a second name for "not implemented".

## Q3. Two observations, three or more free coefficients

**Today:** E or L in most; `invalid-input` in frechet, gev, tlambda; `no-convergence` in
expweibull, gengamma, st; `evaluation` in sstd.

**Mathematics.** All seven are equivariant (F2), so the two-point outcome is a family
constant: the outcome of one problem without data (on the log scale for power-scale
families), over the shapes $\theta$ in the fitting domain,

$$\sup_{\theta,\; u,\; D > 0}\; D^2\, g_\theta(u)\, g_\theta(u + D).$$

No closed form is needed: it is decided once, by proof or high-precision computation
recorded in the dossier, and the fitter returns it without a search. "Never a numerical
failure" is therefore achievable here. Known:

- **gev:** `E`, unique, at $\xi = -1$ (shifted reverse exponential), by the fit note
  ("all-free two-point samples with minimum fraction at most one half have a unique
  reverse-exponential maximum") and the half-minimum pair bound. The core has this path;
  the refusal comes before it.
- **frechet:** two points are an exact half-minimum tie, for which the dossier proves a
  finite unattained supremum $A = n\log 2 - n - 2\sum_{x_i > m}\log(x_i - m)$:
  `N:unattained`.
- **tlambda** (*sketch*): no dossier claim for $n = 2$. For symmetric unimodal $g$,
  $D\,g(D/2) \le 1$, with equality only for the uniform ($\lambda = 1$, in the fitted
  branch, $\sigma = d/2$); asymmetric placements not covered. Likely `E` at $\lambda = 1$.
- **expweibull, gengamma, st, sstd:** not settled; st and sstd have skew-normal, half-t
  and half-normal faces, so the constant may be a limit.
- Non-equivariant families (genbeta with scale held, fatigue, kumar, gamma-type) depend on
  $x_2/x_1$; none fails on `two` today.

**Proposed rule `two-point`:** for families declaring equivariance, `two` ends in the
outcome declared in `existence.two_point` (kind, reason, uniqueness), or, until declared,
in `unsupported` under a declared `min_n`; never a numerical failure. Other families:
`valid-data` only. No further exceptions.

**Would fail:** frechet and gev now (outcome known, so no `min_n`); tlambda, expweibull,
gengamma, st, sstd until declared (their numerical failures in any case).

**Recommendation:** the rule above. Alternatives: (a) "never a numerical failure" for all
families with three or more coefficients, asserting more than is known for non-equivariant
ones; (b) no rule; (c) `min_n: 3` for all seven, ignoring gev's and frechet's answers.

## Q4. An identically zero likelihood inside a closed support

**Today:** rayleigh `zeros` ends in `N:unattained` (support $[0, \infty)$ closed, density 0
at 0 for every $\sigma$); so does nhyper `zeros` (golden holds $N = 50$, $r = 3$; the
support starts at $r$, so `outside-support` already flags it).

**Mathematics.** If the density at an observation vanishes for every admissible
coefficient vector, the likelihood is zero everywhere, so `unattained` ("finite supremum,
not attained") is false: 0 is attained everywhere ("every scale ties", rayleigh dossier),
formally an estimate with `unique = false`. The data are impossible under every member,
which is what out-of-support data means; for nhyper, below $r$ is outside literally.

**Proposed rule (`outside-support`, extended):** data on a fixed closed endpoint whose
`endpoint_log_density` (already in the schema) is the constant `-Inf` count as outside and
end in `failure(invalid-input)`. rayleigh declares `endpoint_log_density: {lower: '-Inf'}`;
the `InvalidInput` comment adds "or data with zero density under every admissible
coefficient vector".

**Exceptions:** endpoints where the density vanishes only for some coefficients (gev upper
for $\xi > -1$, fpareto lower for $\text{shape2}\cdot\text{shape3} > 1$, frechet lower at a
free $\mu$): the likelihood is positive somewhere.

**Would fail:** rayleigh and nhyper `zeros` (and rayleigh samples with one zero among
positives, which no class contains).

**Recommendation:** `invalid-input`. Alternatives: (a) a new reason (`Impossible`), more
precise for two families but one more for the bindings to render; (b) keep `unattained`,
false by its own definition; (c) declare rayleigh's support open at 0, the same result
without the endpoint rule but changing what the support says to d/p/q.

## Q5. Ties

**Today** (4 of 6 at the median): `N:unbounded` in cauchy, lst, slash, st; `N:unattained`
in std; `no-convergence` in sstd; `unresolved` in emg; `unsupported` in gb2;
`invalid-input` in tlambda; E or L elsewhere.

**Mathematics** (F3 at $(6, 4)$):

- cauchy, slash, lst, st, tlambda: $e = -2$, `N:unbounded`. tlambda's attainment theorem
  needs every multiplicity below $n/2$ and its collapse limit record covers the rest, so
  `invalid-input` is wrong.
- std, sstd: $e = 2\nu - 4 \to 0$ as $\nu \downarrow 2$, critical in the limit. std's
  dossier treats the boundary (`N:unattained`). sstd's proves escape exhaustion only for
  $3k_{\max} < 2n$, and here $3k = 2n$: open, so a failure is honest, though `unresolved`
  fits better than `no-convergence`.
- emg (*sketch*): the density is at most $\min(\lambda, 1/(\sigma\sqrt{2\pi}))$; making it
  large needs $\lambda \to \infty$ and $\sigma \to 0$, which kills the other observations
  exponentially. Bounded on nonconstant data: E or a face (normal, translated
  exponential). `unresolved` is honest but undecided.
- gb2: `DoubleParetoBoundary`, a proved face (beta shapes to 0, power to infinity:
  log-Laplace) without a package family, is mapped to `unsupported`. The right mapping is a
  limit with the external name (certified as far as the comparison goes) or
  `N:unattained`. Whether that face is the supremum here was not checked.

**Proposed rule `tie-collapse`:** families declare `existence.collapse` (the exponent in
$n$, $k$ and coefficients; its infimum over the fitting domain; attained or a limit). At
the class's $(n, k)$: $e^* < 0$ requires `N:unbounded`, $e^* > 0$ forbids it, and
$e^* = 0$ only forbids `invalid-input`, `invalid-argument` and `unsupported`. Without the
field, with free location and scale and no `spike` restriction: never `N:unbounded` on
nonconstant data. Tabulate `half-ties`, the critical class for $\alpha = 1$ (cauchy's
dossier: no finite MLE).

**Exceptions:** critical cases (std, sstd on ties and near-constant; $\alpha = 1$ families
on `half-ties`), decided by dossier comparisons. **Would fail:** tlambda (needs
`N:unbounded`), gb2 (`unsupported`; also extended `valid-data`). emg and sstd pass the
sign rule and stay visible through Q1 and a listing of numerical failures on valid data.

**Recommendation:** the declared-exponent rule. Alternatives: (a) hard-code $\alpha$ in
`fit-consistency.py`, faster but a second copy of dossier facts; (b) only "ties never
`invalid-input` or `unsupported`", which misses the bounded/unbounded split; (c) "ties
never a numerical failure", which fails sstd on a case its own theorem excludes.

## What the rules need

New optional keys under `existence` (a schema change, validated by the loader;
equivariance checked numerically at seeded points, as `just relations` checks edges):

| key | content | used by |
|---|---|---|
| `equivariance` | `affine` or `power-scale`, and the coefficients that must be free | Q1, Q3 |
| `collapse` | exponent in $n$, $k$ and coefficients; infimum over the fitting domain; attained or a limit | Q5, Q1 |
| `two_point` | kind, reason, uniqueness, evidence | Q3 |
| `min_n` | value ($\ge 2$) and reason | Q2, Q3 |
| `strict_exceptions` | class, outcome, argument | Q1 (kumar), Q5 critical cases |

Also: a `tie3` snapshot class `[q50, q50, q75]`, `half-ties` in the consistency table, and
the two widened comments in `outcome.hpp`.

**Not settled here:** two-point outcomes of expweibull, gengamma, st, sstd (tlambda
sketched); near-constant in gompertz, sgomp, betapr, fatigue, genbeta, invparalogis,
paralogis; which face wins for emg and gb2 on ties; std's critical-class `N:unattained`
(dossier comparison, not re-checked); every equivariance claim.
