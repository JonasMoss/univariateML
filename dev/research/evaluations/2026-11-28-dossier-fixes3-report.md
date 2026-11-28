# TASK-436 dossier wording fixes

Six dossiers were corrected against the cited proofs and fitter behavior.
The change covers the dossier text, five reviewed proof ledger rows, and the
derived atlas. The baseline was `66338de2`.

## 1. Coverage review 5

### Generalized gamma

**Old.** The power path specified $Q\to+\infty$, $\sigma Q\to d>0$
and a finite endpoint for $\mu$. The Pareto path specified
$Q\to-\infty$, $\sigma|Q|\to d>0$ and the same endpoint condition.

**New.** Both paths take $\mu\to\mu_0$ and require
$\varepsilon(\log x_i-\mu_0)\le0$ for the full sample trace, where
$\varepsilon=\operatorname{sign}Q$. If any observation is at the limiting
endpoint, require $k\exp(A\varepsilon(\mu_0-\mu)/k)\to0$, where
$k=Q^{-2}$ and $A=1/(\sigma|Q|)$. Holding $\mu=\mu_0$ suffices.
[Generalized gamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/README.md#L26)

The written exhaustion proof gives the supported power and Pareto envelopes.
`Shape3GenGamma.moving_mode_supported_face` gives the additional term
explicitly. A finite scaled-exponential limit $c$ subtracts $c$ from the
limiting log density per endpoint contact. The full trace therefore requires
zero penalty at contact. Away from the endpoint on the supported side, the
strict signed gap already makes the exponential term vanish.
[Written proof](2026-11-27-crit-open-math-report.md#gengamma-held-all-free)
· [Lean contact theorem](../../lean/UnivariateML/Family/Shape3GenGamma.lean#L184)

### Skew Student

**Old.** Limits 55 and 60 omitted a `fixed` declaration. Their detector
text already said “With only skew held at one” for the normal supremum
and the positive-scale two-degree-of-freedom limit.

**New.** Both records declare `fixed: [xi]`. The detector text retains
the exact unit-skew requirement, with location, standard deviation and
degrees of freedom free.
[Skew Student dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#L101)

Unit skew reduces the likelihood to symmetric Student. The written
normal-seam and lower-face arguments identify the fitted normal and
positive ordinary-scale $t_2$ limits. The seam expansion establishes
local behavior. The claim of an unattained supremum additionally requires
a successful whole-profile boundary comparison. The fitter calls
`student_t_mle_certified` for this route only when skew is held at exactly
one and the other three coefficients are free.
[Normal seam](2026-11-04-t-cert-data/derivation.md#normal-seam-expansion-including-nuisance-refitting)
· [Lower face](2026-11-04-t-cert-data/derivation.md#collapse-and-the-lower-face)
· [Delegation guard](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/mle.hpp#L742)

### Pareto II

**Old.** The free-threshold limit named “A free-endpoint spike or an
allowed scale collapse” and listed lower power below one as a detector.
The existence statement said only that a finite free-threshold fit
places the threshold at the sample minimum.

**New.** With positive scale and shape held, the lower endpoint power is
one. The density decreases strictly with excess, so the unique threshold
is at the sample minimum with finite inside-support density. As the
threshold recedes to minus infinity the likelihood tends to zero.
The free-threshold divergence text specifies scale collapse with
negative exponent $\alpha(n-k)-k$ when scale is free, or concentration
of a constant sample when scale is held and shape is free.
[Pareto II dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md#L60)

The fixed-regular-coefficients proof gives the unit-power contact result.
`Endpoint3ParetoII.threshold_contact_iff` characterizes the maximizing
threshold and `Base7ParetoII.held_scale_shape_failure` also proves the
receding-threshold limit. The collapse and fixed-scale proofs supply the
remaining divergence branches. The existing `scale+shape/failure`
ledger row already describes the unit-power result.
[Contact theorem](../../lean/UnivariateML/Family/Endpoint3ParetoII.lean)
· [Receding threshold theorem](../../lean/UnivariateML/Family/Base7ParetoII.lean#L20)
· [Scope of the written proof](2026-11-25-thresh-math-report.md#fixed-regular-coefficients)
· [Free-scale collapse](2026-11-25-thresh-math-report.md#fixed-shapes-and-free-scale)
· [Held-scale comparison](2026-11-25-thresh-math-report.md#fixed-scale-and-free-upper-tail-shape)

## 2. Coverage review 7

### GEV

**Old.** “$\xi \to 1$ and $\sigma \to 0$, with the lower end of the
support at the sample minimum.”

**New.** “Set shape xi=1 and the lower support endpoint at the sample
minimum minus a positive gap. Along the nuisance-optimal collapse path
the gap and scale decrease to zero and the lower endpoint approaches the
sample minimum strictly from below.” The half-tie existence statement
now describes the same fixed-shape approach.
[GEV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md#L76)

At shape one and positive gap $e$, `GEV.collapsePath` sets
$(\mu,\sigma)=(m-e+s,s)$ for sample minimum $m$ and optimized scale $s$.
The lower endpoint $\mu-\sigma$ is $m-e$. The optimized scale is
$n/\sum_i (x_i-m+e)^{-1}$, which decreases as $e$ decreases and tends to
zero because a minimum occurs. `GEV.positive_collapse_path` proves the
path stays feasible and gives the limiting value at the half stratum.
[Lean path and limit](../../lean/UnivariateML/Family/GEV.lean#L217)
· [Profiled scale formula](../../lean/UnivariateML/Family/Frechet.lean#L48)
· [Half-supremum comparison](../../lean/UnivariateML/Family/EVGEV.lean#L229)

## 3. Lean robust review

### Huber

**Old.** The fit note ended “unresolved comparisons and work exhaustion
stop.”

**New.** “After the coefficients pass their accuracy checks, an
unresolved maximizer-set comparison returns an uncertified estimate
with unknown cardinality. An unresolved free-shape profile comparison
or exhaustion of its work budget stops.”
[Huber dossier](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md#L78)

`RBHuber.all_free_comparison` compares exact fitted face heights with
finite likelihoods. The numerical behavior follows from `huber_mle`
and its outcome mapping. After polishing the coefficients,
`set_unresolved` gives `unknown_cardinality(false)` when the location
interval check stays open. The free-shape cover can stop before
producing an accuracy-checked candidate. Its unresolved witnesses are
insufficient for a finite estimate and its exhausted work allowance
gives a budget stop.
[Exact comparison theorem](../../lean/UnivariateML/Family/RBHuber.lean#L574)
· [Outcome mapping](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L34)
· [Profile and budget branches](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L883)
· [Maximizer-set check](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L1197)

### EMG

**Old.** The detector ended “otherwise an unexcluded normal face
produces an unresolved stop.”

**New.** “If the normal face remains unexcluded, a finite mode that meets
the public coefficient enclosure and candidate comparison checks can
be returned uncertified. Otherwise the comparison stops unresolved.”
[EMG dossier](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md#L40)

`RBEMG.held_location_failure` proves nonattainment when at least one residual
is nonzero and every finite point lies strictly below the larger
compatible face height. In the numerical fitter, the `Unresolved`
branch can recover a finite enclosed point only after the public
coefficient check and an estimate-valued candidate comparison.
`comparison_outcome_of` flags that estimate as uncertified.
[Exact held-location theorem](../../lean/UnivariateML/Family/RBEMG.lean#L950)
· [EMG public recovery](../../../univariateML-cpp/include/cppunivariate/distributions/emg/mle.hpp#L130)
· [Comparison outcome](../../../univariateML-cpp/include/cppunivariate/core/stop_detail.hpp#L250)

## Ledger and validation

| Family | Held mask | Joined rows | Direction and grade |
|---|---|---|---|
| `gengamma` | all free | `path:1`, `path:2` | one-sided, written |
| `sstd` | `xi` | `limit:55`, `limit:60` | one-sided, written |
| `gev` | all free | `path:2` | one-sided, written |

The three path rows replaced the existing rows and match the dossier
`when` text literally. The two skew Student rows were added under the
held-skew mask. The previous all-free atlas keys were generated from
detectors whose text already specified held unit skew. Both claims retain
their written grade at the corrected keys. No surviving key lost grade
and the status baseline check passed without exceptions.
[Updated ledger](../existence-atlas/proofs.csv)
· [Coverage 5 proposed rows](2026-11-27-atlas-coverage5-data/proofs.csv)
· [Coverage 7 proposed rows](2026-11-28-atlas-coverage7-data/proofs.csv)

All checks ran on the workstation.

- Atlas generation ran once with default arguments and once with
  `--overview`. The saved outputs passed `--check`. The atlas has
  87 families and 520 regimes.
- `just gen` passed. The before/after diff of the generated R metadata
  and roxygen source shows the affected limit scopes, detector and path
  text, and existence paragraphs.
- `just test-changed --no-long-double` passed for the 18 selected
  families and their applicable checks. Native C++ and double-only C++
  each passed 35/35. R development tests had 6,303 passes, 39 skips,
  zero failures and zero warnings. The JavaScript build and all
  312 selected JavaScript tests passed.
- `python3 dev/tools/check_links.py` returned the same 66 findings as
  before the edits, including 47 classed as inherited and 19 classed
  as new by its historical baseline. The job introduced no broken links.
- **Open site check.** `just site-check` generated catalogue data for
  86 families and prepared the site binding. It exited 127 at
  `astro build` because this worktree has no `site/node_modules` and
  Astro is unavailable on its PATH. The final site build and checks
  require the worktree's site dependencies.
