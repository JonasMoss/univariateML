# Shape-face certificates and outcome grades

TASK-386, based on `4a014db6`. The fixes and regression tests are in the job worktree.

## Feller–Pareto and GB2 held power

Archive cases 001625 and 001628 contain the same binary64 observations and held power. The previous Feller–Pareto route reported an unresolved comparison and retained a fixed-scale witness. Every finite fit on this slice loses to the inverse transformed-gamma face. The new route computes normalized log offsets from observation differences before subtracting the threshold. It applies the strict third-central-moment inequality with a floating-point margin, uses the sign to select the face, and solves its gamma shape from a centered exponential remainder that preserves the second-order gap.

Lean proves the stationary-point exclusion in `held_power_narrow_span_no_mle`. The written argument uses the beta-prime exterior bounds to turn this exclusion into a global face comparison and orders the two faces by the signed cubic moment. The coefficient calculation uses the monotone gamma score and an error bound propagated through the public log scale. [Lean proof](../../lean/UnivariateML/Family/ShapeFacesFellerPareto.lean) · [Global comparison and archive enclosures](2026-11-25-fpareto-math-report.md)

The archive regression now expects a certified inverse transformed-gamma limit at log likelihood approximately 127.81430798409595 in both arithmetic modes. It also checks the held power and restored face coefficients. A direct GB2 call checks the same outcome. A second Feller–Pareto regression checks the ordinary shifted transformed-gamma limit on data whose differences disappear if the excesses are rounded first.

## Generalized beta moving power

The existing compact cover already summed the tail term over all observations, including repeated maxima. The finite-power profile score separately dropped the term when the powered log distance exceeded 700. Multiplication by a large second beta shape can still make the omitted term substantial. The new regression with two maximum observations at powered log distance 705 also exposed overflow in the ratio inside the beta normalizer. The score now keeps the exponentially small term, and the normalizer computes the log ratio directly if the ratio itself overflows. The power likelihood and its reported rate now use the same accumulated mean log distance from the sample maximum.

Along a moving-power path with `log(q)/a → m` and `q exp(-a m) → L`, the weak law alone does not determine the joint sample likelihood. If there are `k` observations at the sample maximum, positive `L` subtracts `k L` from the limiting sample log likelihood. The optimized face uses the path with `L = 0`. The regression compares penalized and approaching paths and checks the score and the certificate's log tail statistic with both contacts present. Expected changes concern extreme profile scores and the consistency of the face-rate payload. [Moving-power proof](2026-11-25-genbeta-limits-math-report.md#1-held-endpoint-nonconstant-data)

## Lomax displaced roots

The scalar cover used to remove a previously isolated maximum after comparing two point-value error intervals. That comparison did not bound the maximum over the old bracket. The cover now retains a padded tangent upper bound over the signed root bracket. A new feasible lower bound must exceed that upper bound before the old bracket can be discarded. Budget diagnostics also use the bracket upper bound. The tangent inequality follows from the strict curvature bound used to isolate the root. The Lean existence criterion is independent of this numerical comparison. [Scalar cover criterion](2026-11-25-uniq-cover-math-report.md#scalar-and-knot-covers) · [Runtime profile bounds](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md#global-profile-certificate)

The regression constructs overlapping value bounds with separated point values and then separates the full bounds. No real failing sample was identified for the old displacement rule. The changed-family run also exercised inverse Pareto, Pareto II, and the Burr delegates. No snapshot outcome change is expected when the competing root values are separated by the previous arithmetic margin plus the bracket allowance.

## Folded normal

For joint fits, the moment inequalities now certify a unique half-normal when they close. Strict fourth-moment, exact fourth-moment equality with a sufficiently large sixth moment, a normal candidate bound, or a separated feasible likelihood witness can exclude the half-normal. The exact equality check uses a bounded integer chart after power-of-two scaling. Competing positive branches still require a global value comparison, so a locally checked positive fit is returned as uncertified. The convex-chord comparison to a likelihood tolerance does not prove global coefficient accuracy or cardinality. [Moment and root analysis](2026-11-25-foldnorm-math-report.md#uniform-winner-tests)

For held positive physical location, the fitter now brackets the variance on `[D, m2 + mu²]`. Its signed score numerator has derivative strictly larger than 0.07. A residual bound therefore controls variance error, including at a rounded zero of the score. Squared residuals use original observation differences before normalization, and log likelihood is evaluated at the returned public coefficients. This replaces the held-location search and curvature polish. [Written score bound](2026-11-25-frechet-math-report.md#held-physical-location-free-scale) · [Lean uniqueness proof](../../lean/UnivariateML/Family/FoldedNormal.lean)

Tests cover the exact half-normal sample `{0,0,1}`, a strictly positive half-normal winner, exact fourth-moment equality with a positive winner, and adjacent binary64 observations at a large held location. The certification pin for the existing three-observation positive-location fixture changes from true to false in C++, R and JavaScript. Its unknown cardinality and inference guard remain asserted. Snapshot changes are expected in joint certification flags and in accurately restored held-location scales.

## Certified return audit

| Route | Basis for the returned grade |
|---|---|
| GB2 and Feller–Pareto transformed gamma and inverse transformed gamma | The held-power narrow-skew route uses stationary exclusion and strict face ordering. Held-power beta-prime delegates carry their analytic constant-data or signed profile decision and preserve a margin decision. General exploratory face selection is uncertified. |
| GB2 unit-shape Weibull and inverse Weibull | Held power uses the strict Lomax exponential comparison. Free power uses the Burr signed exterior and compact profile cover. The public outcome now takes the grade from the delegate and requires a limit payload. |
| GB2 and Feller–Pareto Pareto and power | The restricted endpoint route uses a pointwise density bound with the contact factor. Unit-shape delegates use the applicable Burr global or held-scale comparison. Numerical face proposals remain uncertified. |
| Shifted Feller–Pareto limits | An exact excess transform carries the delegate's proof. A rounded transform retains its limit proposal as uncertified. The independently computed narrow-skew certificate uses original observation differences. |
| GB2 generic finite search, lognormal and double Pareto | Local score refinement alone does not cover all basins. The generic finite result is uncertified except when the conditional beta enclosure closes. The numerical lognormal and double-Pareto returns carry uncertified grades. |
| Exponentiated Weibull | The held-scale power envelope is strict with both shapes free and all observations at or below scale. Ties have a factor `1/(e−1)` per contact. The tied outcome now preserves the fitter's grade. All-free power and inverse-Weibull selections remain numerical. Held-exponent finite fits have the affine concavity proof, and the one-free-exponent fit is a strict scalar maximum. |

The audit regressions check the flag on the ordinary GB2 golden fit, preservation of the grade on both Burr limit statuses, the same Feller–Pareto Weibull limit under exact and inexact threshold subtraction, and propagation of the tied exponentiated-Weibull grade. The GB2 golden's coefficients and likelihood retain their existing numerical pins. [Burr certificate](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md#details) · [Beta-prime held-shape proof](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md#non-unit-held-shape-certificate) · [Exponentiated-Weibull held masks](2026-11-25-expweibull-math-report.md#held-masks-and-sample-predicates)

## Validation

All runs used the workstation through the slot wrapper. The narrow-skew, moving-power, displaced-root and folded-normal changes were checked in order with `just test-changed`. The folded-normal JavaScript assertion was corrected and its focused rerun passed 237 tests.

The final `just test-changed --no-long-double` selected the full suites because the JavaScript regression and proof inventory are shared files. Its `just cpp-test` passed 136 of 136 tests and its double-only suite passed 181 of 181. The full R development-tier and JavaScript stages passed after correcting the R outcome pin. R reported 38,294 passes, 157 skips, zero failures and zero warnings. JavaScript passed 601 of 601. The regenerated atlas passed `existence_atlas.py --check`, `existence_keys.py`, `strict-uniqueness.py` and `just site-check`.

## Open mathematics and arithmetic

- Broad or nearly symmetric held-power GB2 samples can fail the skew inequality. A complete global comparison there still needs a signed tail and compact stationary or value cover. Inexact Feller–Pareto excess transport outside the narrow route would need bounds carried from the original observations into the delegated comparison.
- The narrow face reports the centered profiled likelihood. Direct density re-evaluation on extremely concentrated faces can be sensitive to rounding the two public coefficients. Independent enclosures for that public-coordinate likelihood remain open.
- A joint folded-normal positive-location winner still needs global isolation or value bounds for the other positive branches. The moment tests do not decide exact equality between distinct maxima. Quartic equality outside the bounded integer chart still needs an exact predicate if it cannot be resolved by the guarded sign comparison.
