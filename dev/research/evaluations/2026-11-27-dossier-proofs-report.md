# TASK-387 · Limit detector grades and dossier corrections

Worktree base `06b34bf3`. The dossiers now use `firmness` for the parameter
path and `detect_firmness` for the winning condition. The latter asserts
that the detected value is the supremum over finite fits and all compatible
boundaries. Finite equality with an excluded face still permits an attained
MLE. The schema accepts Lean declaration names, repository proof-section
links and published citations in the optional `proof` and `detect_proof`
lists. The atlas checks file and anchor existence and Lean map membership.
It preserves scoped atlas evidence and carries modifier records under
`modifier_limits` in the family’s all-free row. [Schema](../../../univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md) · [Atlas](../existence-atlas/overview.md)

A mixed detector grades the strict, certified branch as proved and states
what an unresolved comparison returns. In beta prime, the near-constant
skewness comparison can choose gamma or inverse gamma as an uncertified
limit. A successful finite search can also be retained uncertified. The
signed-tail and compact-profile exclusions have their own proof. A
delegate must reproduce the reachable joint sample likelihood, including
endpoint penalties. [Beta-prime comparison](2026-11-25-betapr-math-report.md#compact-scalar-decision-for-all-free-data)

## Runtime follow-ups

| Route | Finding and next check |
| --- | --- |
| Held-scale `trgamma` and `invtrgamma` | The reachable fixed-endpoint trace loses to finite parameters throughout the bounded regime. The reciprocal diagnostic can substitute the ordinary Pareto endpoint value, which exceeds that trace by one log-likelihood unit per threshold observation. Replay `x=(1,2), scale=1` through the inverse diagnostic and ensure that the trace, with the endpoint penalty, remains a diagnostic. [Proof and source check](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free) |
| `invtrgamma` all-free and `pareto4` Weibull/Pareto routes | The inverse and shifted outcome switches issue certified limits unconditionally for their named statuses. The dossier grades the corresponding broad detector records numerical. Trace the actual coverage and arithmetic margin per route. Carry the branch evidence into the outcome, or expose a proved subcase in the dossier when the strict certificate already closes. [Inverse switch](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/mle.hpp#L296) · [Shifted switch](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/mle.hpp#L167) |
| `invburr` held power and `gb2` Burr submodels | The switches issue certified limits, while the broad records still describe numerical detectors. Held-power inverse Burr delegates a strict comparison on reciprocated data. The remaining check is the certificate’s transport through the original-data transform and the distinction between signed exclusion and a numerical profile preference. [Inverse Burr source](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp#L325) · [GB2 source](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp#L837) |
| `gengamma`, `huber`, `snorm` | Named-limit statuses are mapped to certified outcomes while the current detector records remain numerical. The relevant source branches include strict face selection or separate comparison-tie handling. Document the scope of each certificate and check that unresolved comparisons cannot reach the same certified status. [Generalized gamma](../../../univariateML-cpp/include/cppunivariate/distributions/gengamma/mle.hpp#L319) · [Huber](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L55) · [Two-piece normal](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/mle.hpp#L311) |
| Truncated normal and zero-inflated negative binomial | Their surviving numerical modifier entries share statuses with certified outcomes. The switches handle ties or unresolved comparisons separately. The follow-up is to attach each strict comparison to its applicable hold mask and keep only the numerical outcomes on a margin grade. [Truncated normal](../../../univariateML-cpp/include/cppunivariate/distributions/norm/tnorm_mle.hpp#L826) · [Inflated negative binomial](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/zinb_mle.hpp#L246) |
| Numerical face families with conditional certification | `fpareto`, `gb2`, `ged`, `sged`, `sstd`, `st`, and `expweibull` expose some certified branches under statuses also used by broader detector descriptions. A follow-up should match each return site’s signed cover, hold scope and margin before assigning proof to the whole detector. |
| Critical scale collapse | Free-location `slash` returns an unresolved failure when it uses the margin. Held-location equality has its scalar proof. For `st`, the generic critical case does not establish nonattainment, although the same status name can represent a proved route through a different reason code. For `sstd`, the generic critical cases are numerical and the symmetric held-mean/held-shape equality has its own proof. Keep these branches distinct in any outcome cleanup. [Slash](../../../univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp#L448) · [Skew-t](../../../univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp#L47) · [SSTD](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/mle.hpp#L871) |
| Exact maximizing intervals | Laplace already returns the median interval on a successful nonunique fit. Current Huber returns the eroded interval when its interval certificate passes, including the exact held-scale and held-tuning gap test. The remaining fitter work is the held-tuning/free-scale route when its rounded-coefficient enclosure cannot certify the mathematically nontrivial interval. The proof supplies the common scale and equal-tail gap condition. [Huber proof](2026-11-25-lean-huber-report.md#theorems) · [Current implementation](../../../univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp#L1095) |

## Conversion

The [conversion counter](2026-11-27-dossier-proofs-data/count_conversion.py)
reads the original dossiers and atlas at the recorded base and compares
them with the current tree.

| Original base-limit flag | Proved | Numerical | Path only |
| --- | ---: | ---: | ---: |
| `true` | 10 | 110 | 0 |
| `false` | 2 | 0 | 0 |
| Absent | 140 | 15 | 2 |
| Total | 152 | 125 | 2 |

The two path-only records are the PGW laws. Of 279 base records, 166 now
have a path pointer and 159 have a detector pointer, including some
narrower evidence preserved under numerical grades. The ten old true
flags regraded proved are beta-binomial 1, beta prime 4–7, Bhattacharjee
1, Burr 2, Gompertz 2 and inverse Burr 3–4. Strict dominance or exact
inequality is the proved branch. A numerical fallback retains its
qualification. For Lomax, inverse Pareto and Pareto II, an exact
no-witness theorem is narrower than the implemented margin comparison.
Those records retain numerical grades.

The same schema applies to modifier limits. Of 33 original nested
records, 24 now have proved detectors, six retain numerical grades, one
records an invalid-input diagnostic without a winning condition, and two
were removed. Both removed records were copied negative-binomial
Poisson escapes under geometric inflation. The old paths required size
to increase without bound, while geometric inflation fixes the delegate
size to one in both bindings. The dossier and generated capability count
now reflect that hold. [R delegate](../../../bindings/r/R/modifiers.R#L101)
· [JS delegate](../../../bindings/js/modifiers.mjs#L57)

| Modifier | Grade and basis |
| --- | --- |
| Binomial, zero truncation/modification and general truncation | The signed tail and finite prefix prove the certified Poisson comparison. Zero modification adds the Bernoulli zero-indicator likelihood at the same zero probability. The point-mass cases have upper bound one and their displayed paths approach it. [Finite certificate](2026-11-25-tbinom-math-report.md#a-finite-decision-certificate) |
| Binomial, zero inflation | Proved on the stated signed-tail and complete-prefix branches. The face-tie record certifies escape while leaving the face-rate ordering at the arithmetic margin. [Declared scope](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md) |
| Geometric and negative binomial, held-size finite windows | The conditional log likelihood is concave in log odds for the count, with score given by the observed minus fitted mean. At the limiting upper mean the strictly monotone score gives the upper face. At the lower endpoint, the point-mass path reaches the upper bound one. [Negative-binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md) |
| Negative binomial, zero truncation/modification and inflation | The first two retain the supported positive-count certificate. The two inflation comparisons retain the old numerical grade pending branch-specific provenance. [Certificate scope](2026-11-06-zinb-cert-report.md#implementation-and-mathematical-scope) |
| Truncated normal | The two concentration records give divergence. The four exponential or uniform comparisons retain the numerical grade of the old flags. The exact exponential-tilt optimization and arithmetic qualifications have recorded evidence. [Tilt comparison](2026-11-21-tnorm-limit-report.md#coefficient-rule-and-comparison) |
| Poisson | The three point-mass paths attain probability one in the limit. The held zero-rate record identifies an invalid public input. [Poisson dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pois/README.md) |

R help and the site use the detector grade. R’s lookup of the optional
`detect` text uses exact indexing so it cannot partially match
`detect_firmness`. The low-level R reader retains the former boolean
type check for the existing legacy-input fixture. Canonical validation
in the site and atlas rejects that field. The numerical call-path
inventory reads source headers and has no limit-schema input.

## Review of previously unbacked base detectors

These are the 48 records with an original proved path, no true numerical flag
and at least one atlas scope without a proved detector. Record numbers
are one-based within each dossier. Forty-four now have a proved detector
and four have a numerical detector.

| Family | Record | Old grade | New detector grade | Proof or reason |
| --- | ---: | --- | --- | --- |
| [`bnbinom`](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md) | 1 · nbinom | Proved path, implicit detector | `proved` | Supporting bounds and complete finite cover certify exclusion; the legacy fallback explicitly lacks certification. [bnbinom-math](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| [`bnbinom`](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md) | 2 · pois | Proved path, implicit detector | `proved` | Supporting bounds and complete finite cover certify exclusion; the legacy fallback explicitly lacks certification. [bnbinom-math](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test) |
| [`cmb`](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) | 2 · point mass | Proved path, implicit detector | `proved` | Fixed-size exposed hull faces and free-size saturation bounds. [cmb-math](2026-11-25-cmb-math-report.md#held-size), [cmb-math](2026-11-25-cmb-math-report.md#free-size) |
| [`cmb`](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) | 3 · adjacent two-point law | Proved path, implicit detector | `proved` | Fixed-size exposed hull faces and free-size saturation bounds. [cmb-math](2026-11-25-cmb-math-report.md#held-size), [cmb-math](2026-11-25-cmb-math-report.md#free-size) |
| [`cmb`](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) | 4 · endpoint two-point law | Proved path, implicit detector | `proved` | Fixed-size exposed hull faces and free-size saturation bounds. [cmb-math](2026-11-25-cmb-math-report.md#held-size), [cmb-math](2026-11-25-cmb-math-report.md#free-size) |
| [`emg`](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md) | 1 · norm | Proved path, implicit detector | `proved` | The strictly excluding comparison suffices; unexcluded faces remain unresolved. [emg-math](2026-11-25-emg-math-report.md#exact-finite-comparison-and-per-input-certificates) |
| [`emg`](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md) | 2 · translated exponential | Proved path, implicit detector | `proved` | The strictly excluding comparison suffices; unexcluded faces remain unresolved. [emg-math](2026-11-25-emg-math-report.md#exact-finite-comparison-and-per-input-certificates) |
| [`frechet`](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) | 2 · shape divergence | Proved path, implicit detector | `proved` | Held-scale constant path at unit normalized gap, including a compatible held location. [frechet-math](2026-11-25-frechet-math-report.md#fréchet-single-holds) |
| [`frechet`](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) | 3 · gumbel | Proved path, implicit detector | `proved` | Strict profile cover for Gumbel; pair bound including Gumbel at a half tie. [frechet-math](2026-11-25-frechet-math-report.md#collapse-and-the-gumbel-comparison) |
| [`frechet`](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) | 4 · free collapse | Proved path, implicit detector | `proved` | Strict profile cover for Gumbel; pair bound including Gumbel at a half tie. [frechet-math](2026-11-25-frechet-math-report.md#collapse-and-the-gumbel-comparison) |
| [`genbeta`](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) | 2 · lgamma | Proved path, implicit detector | `numerical` | Near-constant face ranking does not exclude finite competitors. |
| [`genbeta`](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) | 3 · power | Proved path, implicit detector | `numerical` | Near-constant face ranking does not exclude finite competitors. |
| [`gev`](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md) | 1 · held-minimum half tie | Proved path, implicit detector | `proved` | Pair bound and the attained reverse-exponential comparator, with the balanced two-value exception. [gev-math](2026-11-25-gev-math-report.md#location-held) |
| [`lst`](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) | 1 · scale collapse | Proved path, implicit detector | `proved` | Negative exponent gives divergence at a fixed admissible shape. [tcert-math](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons) |
| [`lst`](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) | 2 · scale collapse | Proved path, implicit detector | `proved` | Negative exponent at the held location or at a sample atom. `UnivariateML.TDist.collapse_unbounded`, `UnivariateML.TDist.joint_unbounded` |
| [`lst`](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) | 3 · tie equality boundary | Proved path, implicit detector | `proved` | Fixed-location and geodesic free-location proofs. Below one the unresolved fallback applies. `UnivariateML.TDist.heldLocation_equality`, [derivation](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one) |
| [`lst`](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) | 4 · norm | Proved path, implicit detector | `proved` | Strict signed normal-tail and interval exclusion, with explicitly uncertified fallback. [tcert-math](2026-11-25-tcert-math-report.md#normal-face), [tcert-math](2026-11-25-tcert-math-report.md#cover-and-inclusion) |
| [`pareto4`](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md) | 5 · shifted Pareto with its endpoint held at min + scale | Proved path, implicit detector | `proved` | Positive-excess translation transports the strict held-scale Burr supremum, including its endpoint penalty. `UnivariateML.BurrHeld.scale_pareto_supremum`, [burr-math](2026-11-25-burr-math-report.md#inverse-and-shifted-burr) |
| [`sged`](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 6 · constant sample scale collapse | Proved path, implicit detector | `proved` | On all-zero residuals the positive standardized density contributes a fixed factor per observation, and each inverse-scale factor diverges. [sged dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#density-limits) |
| [`sged`](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 12 · constant sample scale collapse | Proved path, implicit detector | `proved` | On all-zero residuals the positive standardized density contributes a fixed factor per observation, and each inverse-scale factor diverges. [sged dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#density-limits) |
| [`sged`](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 14 · constant sample scale collapse | Proved path, implicit detector | `proved` | On all-zero residuals the positive standardized density contributes a fixed factor per observation, and each inverse-scale factor diverges. [sged dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#density-limits) |
| [`sged`](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 20 · constant sample scale collapse | Proved path, implicit detector | `proved` | On all-zero residuals the positive standardized density contributes a fixed factor per observation, and each inverse-scale factor diverges. [sged dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#density-limits) |
| [`sged`](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 22 · constant sample scale collapse | Proved path, implicit detector | `proved` | On all-zero residuals the positive standardized density contributes a fixed factor per observation, and each inverse-scale factor diverges. [sged dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#density-limits) |
| [`sged`](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 26 · constant sample scale collapse | Proved path, implicit detector | `proved` | On all-zero residuals the positive standardized density contributes a fixed factor per observation, and each inverse-scale factor diverges. [sged dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#density-limits) |
| [`sged`](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | 30 · scale collapse | Proved path, implicit detector | `proved` | On all-zero residuals the positive standardized density contributes a fixed factor per observation, and each inverse-scale factor diverges. [sged dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#density-limits) |
| [`slash`](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md) | 1 · point mass collapse | Proved path, implicit detector | `numerical` | The free-location margin does not exclude finite winners. The held-location proof remains scoped in the atlas. |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 7 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 13 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 18 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 22 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 26 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 31 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 35 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 41 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 44 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 48 · tie collapse | Proved path, implicit detector | `proved` | The strict atom exponent forces divergence on the compatible fixed-skew path. A held physical sd uses ordinary scale proportional to the square root of df minus two. [sstd dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md#strict-scale-collapse) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 51 · scale collapse | Proved path, implicit detector | `proved` | With location, shape and skew fixed, the monotone inverse-scale score and collapse exponent settle divergence and equality. [tcert-math](2026-11-25-tcert-math-report.md#fernandezsteel-sstd) |
| [`sstd`](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | 52 · symmetric scale-collapse equality | Proved path, implicit detector | `proved` | The symmetric fixed-mean and fixed-shape scale score gives the finite unattained critical supremum. [tcert-math](2026-11-25-tcert-math-report.md#fernandezsteel-sstd) |
| [`st`](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md) | 6 · unbounded scale collapse | Proved path, implicit detector | `proved` | Fixing slant and shape gives the indicated tail power and a positive prefactor at every nonzero residual. [st dossier](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md#details) |
| [`st`](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md) | 7 · scale-collapse equality | Proved path, implicit detector | `numerical` | Critical exponent gives a finite trace; runtime reports an unresolved comparison. |
| [`std`](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | 1 · scale collapse | Proved path, implicit detector | `proved` | Choose a fixed admissible shape of negative exponent. [tcert-math](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons) |
| [`std`](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | 2 · scale collapse | Proved path, implicit detector | `proved` | Ordinary-scale transfer and negative-exponent joint theorem. `UnivariateML.TDist.std_heldLocation`, `UnivariateML.TDist.std_joint_unbounded`, `UnivariateML.TDist.exponent_all_tied` |
| [`std`](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | 3 · tie equality boundary | Proved path, implicit detector | `proved` | Scalar and geodesic comparisons at held shape. `UnivariateML.TDist.std_heldLocation`, [derivation](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one) |
| [`std`](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | 4 · fixed-sd collapse | Proved path, implicit detector | `proved` | Strict compact and exterior comparisons on the certified branch. [tcert-math](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons), [tcert-math](2026-11-25-tcert-math-report.md#cover-and-inclusion) |
| [`std`](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | 5 · norm | Proved path, implicit detector | `proved` | Strict signed normal-tail and interval exclusion, with explicitly uncertified fallback. [tcert-math](2026-11-25-tcert-math-report.md#normal-face), [tcert-math](2026-11-25-tcert-math-report.md#cover-and-inclusion) |
| [`std`](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | 6 · lst | Proved path, implicit detector | `proved` | Strict compact and exterior comparisons on the certified branch. [tcert-math](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons), [tcert-math](2026-11-25-tcert-math-report.md#cover-and-inclusion) |
| [`tlambda`](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md) | 1 · scale collapse | Proved path, implicit detector | `proved` | The held negative-shape profile increases to its critical trace, and a strict majority at shape minus one forces divergence. [tlambda dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md#details) |
| [`tlambda`](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md) | 2 · infeasible held support | Proved path, implicit detector | `proved` | Support radius and zero endpoint density exclude a positive likelihood. [tlambda dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md#details) |

## Held masks and inherited wording

| Dossier and record | Previous declaration | Revised declaration | Proof |
| --- | --- | --- | --- |
| Hypergeometric 1 | `fixed: [m]` | `fixed: [k, m]`. Both held counts are positive and all observations are zero. A zero count yields a flat compatible likelihood. | [Population-ray theorem](../../lean/UnivariateML/Family/Hypergeometric.lean#L737) |
| Pareto II 1 and Pareto III 1 | Threshold omitted | `fixed: [min]`. Positive excesses are the transfer domain. Pareto II’s margin remains numerical. Pareto III’s scale may also be held at the common excess. | [Positive-excess transfer](../../lean/UnivariateML/Family/Transfers.lean#L400) · [Pareto III](2026-11-27-lean-transfers3-report.md#pareto-iii) |
| Pareto IV 2–4 and 5 | Threshold omitted in 2–4, only scale in 5 | `fixed: [min]` in 2–4 and `fixed: [min, scale]` in 5 | [Shifted Burr](2026-11-25-burr-math-report.md#inverse-and-shifted-burr) |
| Feller–Pareto 1–9 | Shape held in 1–2, threshold omitted throughout | `[min, shape3]` in 1, `[min, shape1]` in 2, and `[min]` in 3–9 | [Held-threshold slices](2026-11-25-fpareto-math-report.md#uniform-exclusion-of-finite-stationary-points) |
| Nonstandard beta 1 | Endpoints omitted | `fixed: [min, max]` and the common observation is strictly interior | [Four-parameter beta](2026-11-27-lean-transfers3-report.md#four-parameter-beta) |

Several TASK-320 statements already had their corrected wording at
`06b34bf3`. Inverse Weibull listed both constant-data routes, uniform
required compatibility and zero candidate width, Laplace qualified its
even-sample interval by the free-scale nonconstant condition, fatigue
used leading-order approximations, folded normal already asserted
held-location uniqueness, and beta-binomial already stated proved
necessity with an interior count. The new proof pointers and the scoped
cardinality text use those corrected statements. [Weibull](2026-11-25-lean-weibull-report.md#6-inverse-weibull)
· [Uniform](2026-11-25-lean-endpoint-report.md#dossier-findings)
· [Laplace](2026-11-25-lean-laplace-report.md#dossier-findings)
· [Fatigue](2026-11-25-lean-fatigue-report.md)
· [Folded normal](2026-11-25-frechet-math-report.md#held-physical-location-free-scale)
· [Beta-binomial necessity](2026-11-25-bbinom-math-report.md#global-necessity-proof)

The TASK-258 transfer appendix supplies the added fixed-shape and
reciprocal statements below. Where the dossier already gave the
transformed likelihood and the same existence and uniqueness conditions,
the existing text supplies that result. The exact location-set notes
for Huber and Laplace use their present fitter behavior and the held-shape
proofs. [Transfer appendix](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

## Changed mathematical text

The excerpts show the affected claim in each dossier. Blank prior
coverage is described as absent.

### bbinom

Before

```text
unidentified. With beta held and size free,
```

After

```text
unidentified. At known sizes two through eight with an interior count and positive dispersion sign, the attained two-shape fit is unique. With beta held and size free,
```

[bbinom3-math](2026-11-25-bbinom3-math-report.md#exact-proof-for-sizes-four-through-eight)

### bbinom

Before

```text
reason: >-
    Write $p
```

After

```text
reason: >-
    At size two the interior histogram is saturated when the fitted dispersion
    is positive. At size three, and at sizes four through eight, every finite
    stationary concentration has negative profile curvature, so the profile
    has at most one stationary maximum.
    Write $p
```

[bbinom2-math](2026-11-25-bbinom2-math-report.md#universal-uniqueness-at-size-three)

### betapr

Before

```text
the sample is nonconstant if both shapes are free.
```

After

```text
the sample is nonconstant if both shapes are free. Constant data with both shapes free give unbounded likelihood at held scale.
```

[lean-transfers3](2026-11-27-lean-transfers3-report.md#dossier-findings)

### betapr

Before

```text
Holding both shapes gives a strictly decreasing log-scale score
```

After

```text
With scale free and both shapes held, the log-scale score is strictly decreasing and has a unique root, even for constant data.
```

[lean-transfers3](2026-11-27-lean-transfers3-report.md#dossier-findings)

### betapr

Before

```text
the sign of the skewness selects this face when the sample is right-skewed
```

After

```text
the sign of the skewness selects this face as an uncertified limit when the sample is right-skewed
```

[betapr-math](2026-11-25-betapr-math-report.md#compact-scalar-decision-for-all-free-data)

### bhatt

Before

```text
No corresponding scoped statement in this section.
```

After

```text
When both scales vary and the residual radius is positive, compare the normal fit and all stationary widths on the compact dimensionless-width interval with the uniform envelope. A finite fit exists exactly when the largest finite candidate ties or exceeds that envelope. With half-width held, endpoint factors enter the path likelihood, including a factor of one half per observation at a held endpoint with location held. At range equal to the full held width and free location, the endpoint factors are their relative multiplicities.
```

[bhatt-math](2026-11-25-bhatt-math-report.md#finite-stationary-width-criterion-proved)

### bhatt

Before

```text
No corresponding scoped statement in this section.
```

After

```text
Real-sample crossings give an attained normal/positive-width tie with both scales free and exactly two optimizing widths in another sample at held Gaussian scale one.
```

[bhatt-math](2026-11-25-bhatt-math-report.md#exact-crossings)

### bhatt

Before

```text
No corresponding scoped statement in this section.
```

After

```text
With dimensionless width held, the Gaussian convolution kernel is strictly log-concave. A nonconstant sample then has one joint location–scale optimum. A physical half-width hold links dimensionless width to scale.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### binom

Before

```text
Integer maximizers need not be unique.
```

After

```text
With probability held, integer maximizers need not be unique. Joint positive-mean multiplicity remains open.
```

[lean-binomsize](2026-11-27-lean-binomsize-report.md#strict-poisson-dominance-and-the-existence-criterion)

### burr

Before

```text
No corresponding scoped statement in this section.
```

After

```text
With a positive power held, an exact real sample constructed on the powered scale gives two distinct joint shape–scale maxima. Holding the first shape instead gives strict log-data concavity and a unique joint power–scale fit on nonconstant data.
```

[burr-math](2026-11-25-burr-math-report.md#held-power-has-an-exact-global-tie)

### burr

Before

```text
No all-free finite/finite tie stated.
```

After

```text
All-free Burr XII can also have two strict finite global maxima on exact real observations. Both limiting likelihoods are strictly below them.
```

[burr2-math](2026-11-25-burr2-math-report.md#two-finite-global-maximizers-above-both-faces)

### dblpareto

Before

```text
side. Holding both rates always gives a finite fit, with a possible kink interval.
```

After

```text
side. With both rates free, one zero deviation and one positive deviation
    give a finite unattained one-sided limit, while two zero deviations give
    unbounded likelihood. With one rate held, zero free-side deviation gives a
    finite unattained supremum, including on constant data. Holding both rates
    always gives a finite fit, with a possible kink interval.
```

[dblpareto-math](2026-11-25-dblpareto-math-report.md#all-eight-masks)

### dnorm

Before

```text
No corresponding scoped statement in this section.
```

After

```text
Integrating the normal density over each observation cell preserves log concavity in inverse scale and scaled location. The joint scale-zero face for adjacent occupied cells is attainable only as a limit.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### dweibull report

Before

```text
The numerical restoration section studies a conditional shape refit.
```

After

```text
**2026-11-27 correction.** The coefficient-restoration example studies a
conditional shape refit at rounded probability. The current fitter retains
the joint shape, evaluates the likelihood at the rounded public pair, and
returns `Unrepresentable` if the restored probability reaches an endpoint.
The example measures sensitivity to a refit. [Source review](2026-11-27-lean-dweibull-report.md#source-discrepancy).
```

[lean-dweibull](2026-11-27-lean-dweibull-report.md#source-discrepancy)

### emg

Before

```text
No corresponding scoped statement in this section.
```

After

```text
For nonconstant all-free data, the dimensionless shape profile has finitely many stationary values. A finite MLE exists exactly when one of them equals or exceeds both optimized boundary values. The shifted-exponential limit with threshold held at an observed value contributes a half-density factor at each observation there.
```

[emg-math](2026-11-25-emg-math-report.md#exact-finite-comparison-and-per-input-certificates)

### emg

Before

```text
No corresponding scoped statement in this section.
```

After

```text
With the dimensionless rate held, Gaussian convolution gives a strictly log-concave location–scale kernel. A nonconstant sample has one joint nuisance fit. A held physical rate instead links the dimensionless rate to scale.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### emg

Before

```text
The shifted-exponential limit with threshold held at an observed value contributes a half-density factor at each observation there.
```

After

```text
The shifted-exponential limit with threshold held at an observed value contributes a half-density factor at each observation there. On exact real samples with all coefficients free, two finite shapes can tie globally.
```

[emg-math](2026-11-25-emg-math-report.md#two-finite-global-co-winners)

### exp

Before

```text
Fixed-shape log-concavity and anchored slice absent.
```

After

```text
The standard log density is affine on its positive support. The anchored log likelihood is strictly concave in inverse scale and has a unique maximum for positive total exposure.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### fatigue

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At fixed threshold and shape $\alpha\le2$, the likelihood is strictly log-concave in log scale. The reciprocal-scale pair at a constant excess requires $\alpha>2$.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### foldnorm

Before

```text
Joint uniqueness remains open.
```

After

```text
Joint cardinality is sample-dependent, with a proved real sample tying the half-normal and a positive-location fit.
```

[foldnorm-math](2026-11-25-foldnorm-math-report.md#exact-and-near-ties)

### foldnorm

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At a fixed standardized location ratio $\mu/\sigma\le1$, inverse-scale fitting is strictly concave and has a unique maximum if any observation is positive. A held physical location instead uses the inverse-variance result.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### frechet

Before

```text
No corresponding scoped statement in this section.
```

After

```text
With only physical scale held, every nonconstant sample attains on the fitted range $\lambda\ge1$. The free-scale Gumbel face is unavailable on this hold. At held scale one, an exact real sample has two finite maximizing shapes.
```

[frechet-math](2026-11-25-frechet-math-report.md#fréchet-single-holds)

### frechet

Before

```text
At exactly $n/2$ repeats, a finite collapse face must be compared with
    finite candidates. A Gumbel limit can also prevent attainment.
    A complete existence criterion and joint uniqueness are not established.
```

After

```text
At exactly $n/2$ repeats, collapse has a finite unattained supremum.
    With less mass at the minimum, a Gumbel limit can prevent attainment.
    With only physical scale held, each nonconstant sample attains on the fitted
    shape range and exact real samples can have distinct finite co-winners.
```

[frechet-math](2026-11-25-frechet-math-report.md#collapse-and-the-gumbel-comparison)

### gamma

Before

```text
Fixed-shape log-concavity and anchored slice absent.
```

After

```text
At fixed gamma shape the log-data location criterion is strictly concave. The physical standard gamma density is log-concave when shape is at least one.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### ged

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At held standard deviation every valid binary64 sample attains. At exact real uniform-width contact, the finite fit and the limiting value can win separately or tie. With shape held below one, the maximizing locations are all data knots minimizing the residual-power sum.
```

[ged-math](2026-11-25-ged-math-report.md#held-standard-deviation)

### genbeta

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At $q=1$, with the endpoint held, the density depends on the product of the two remaining shape and power coordinates. A finite optimal product gives a curve of maximizing pairs.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### gengamma

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At fixed finite $Q$, holding scale permits a unique location estimate even for constants. Holding location permits a unique scale unless every log observation equals that location.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### gev

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At fixed shape $-1<\xi\le0$, the standard log density is strictly concave. A positive feasible, noncollapsing location–scale slice has one maximizing point.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### gompertz

Before

```text
Fixed-shape log-concavity and anchored slice absent.
```

After

```text
At fixed ratio $a/b$, the standard log density is strictly concave. The anchored inverse-scale likelihood has a unique maximum if the total exposure is positive.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### gpd

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At fixed $-1<\xi<0$ the standard log density is strictly concave. At $\xi=-1$ with positive scale held, every support-compatible location maximizes the uniform likelihood on that slice.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### huber

Before

```text
When the maximising location is an interval (the sample lies in the linear tails), its midpoint represents the closed maximizer box and `unique` is false. The interval is the central sample gap eroded at each end by $\epsilon\sigma$.
```

After

```text
When the fitter certifies a nontrivial maximizing location interval, it returns the closed interval at the common scale and uses its midpoint as representative. For held tuning and nonconstant data, a nontrivial interval requires equal tail counts and a central gap wider than twice the transition width. The interval is the central gap eroded at each end by $\epsilon\sigma$.
```

[lean-huber](2026-11-25-lean-huber-report.md#theorems)

### invburr

Before

```text
No corresponding scoped statement in this section.
```

After

```text
Reciprocating the exact real held-power Burr sample, and inverting fitted scale, gives two distinct finite global maxima. The power hold can be any fixed positive value.
```

[burr-math](2026-11-25-burr-math-report.md#held-power-has-an-exact-global-tie)

### invburr

Before

```text
No all-free finite/finite tie stated.
```

After

```text
All-free inverse Burr can also have two strict finite global maxima on exact real observations. Both limiting likelihoods are strictly below them.
```

[burr2-math](2026-11-25-burr2-math-report.md#two-finite-global-maximizers-above-both-faces)

### invgauss

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At fixed positive shape-to-mean ratio $\lambda/\mu$, the log-data likelihood is strictly log-concave in log mean, with a unique maximum for every positive sample.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### invparalogis

Before

```text
No corresponding scoped statement in this section.
```

After

```text
Reciprocation transports both exact real-sample global ties, with all coefficients free or with scale held at one. It inverts each maximizing scale and preserves the shape hold.
```

[paralogis-math](2026-11-25-paralogis-math-report.md#reciprocal-transfer-and-verification)

### invpareto

Before

```text
No corresponding scoped statement in this section.
```

After

```text
The reciprocal transformation preserves the entire maximizing set. Attained joint fits with at most two support values are unique, and exact three-value real samples can have two finite winners.
```

[lomax-uniq-math](2026-11-25-lomax-uniq-math-report.md#transfer-and-lean)

### invtrgamma

Before

```text
when: The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held.
```

After

```text
when: The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held. The reachable trace has a loss of one log-likelihood unit per endpoint observation and is strictly beaten at a finite parameter in every bounded held-scale case.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### invtrgamma

Before

```text
message: fixed-endpoint Pareto face
```

After

```text
message: fixed-endpoint Pareto face
    detect: The held-scale cover can report this numerical comparison diagnostic. The reachable trace loses on the bounded branch.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### invtrgamma

Before

```text
With only $a$ held, the reciprocal certificate compares the interior with the fixed-endpoint Pareto limit.
```

After

```text
With only $a$ held, the bounded profile strictly beats the reachable Pareto trace. The numerical comparison can still report the trace as a diagnostic.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### invtrgamma

Before

```text
With only scale held, the reciprocal transformed-gamma certificate
    assesses attainment against the fixed-endpoint Pareto limit. Constant
    samples below scale have unbounded likelihood when both shapes are free.
```

After

```text
With only scale held, a finite fit strictly beats the reachable
    fixed-endpoint Pareto trace whenever the likelihood is bounded. Constant
    samples at or below the held scale give unbounded likelihood with both shapes free.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### kumar

Before

```text
No corresponding scoped statement in this section.
```

After

```text
The fixed-shape curvature calculation is the generalized-beta case with its first beta shape fixed at one. At second beta shape one the upper endpoint has positive density.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### laplace

Before

```text
The fit selects the upper endpoint of that interval.
```

After

```text
A successful non-unique fit returns the closed location interval at its common scale, using the upper endpoint as representative.
```

[lean-laplace](2026-11-25-lean-laplace-report.md#dossier-findings)

### lomax

Before

```text
No corresponding scoped statement in this section.
```

After

```text
An attained joint fit is unique on samples with at most two support values. An exact three-value real sample gives two distinct finite global maximizers.
```

[lomax-uniq-math](2026-11-25-lomax-uniq-math-report.md#exact-tie-construction)

### lst

Before

```text
At $k=\nu(n-k)$, held location gives an unattained supremum. With free location and shape at least one, equality is unattained except for the balanced two-point Cauchy maximum curve; joint equality below shape one stops numerically.
```

After

```text
At $k=\nu(n-k)$, held location gives an unattained supremum. With free location, $k$ counts the largest atom. At shape at least one equality is unattained except for the attained balanced two-point Cauchy curve. Below shape one, joint equality can return an uncertified finite result or a proved symmetry outcome. The equality-specific numerical stop applies if the search has no successful candidate.
```

[lean-tdist](2026-11-27-lean-tdist-report.md#dossier-findings)

### lst

Before

```text
exponent $\nu(n-k)-k$; the fit domain $\nu \ge 1$ of df puts its infimum
      at the attained member $\nu = 1$.
```

After

```text
exponent $\nu(n-k)-k$. With location free, estimated shape ranges over
      $\nu\ge1$ and the infimum is attained at $\nu=1$. With location held and
      $k<n$, free shape ranges over $\nu>0$ and gives infimum $-k$ as $\nu\downarrow0$.
```

[lean-tdist](2026-11-27-lean-tdist-report.md#dossier-findings)

### nhyper

Before

```text
No corresponding scoped statement in this section.
```

After

```text
With a positive stopping count held and both urn counts free, the population-tail bound reduces the comparison with the negative-binomial face to finitely many eligible populations. A finite tie with the face gives attainment. Strict exclusion on the finite list and in the tail proves escape.
```

[nhyper-math](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape)

### nsbeta

Before

```text
the transformed beta log likelihood is strictly concave in its two shapes.
```

After

```text
the transformed sufficient-statistic mean is interior, giving coercivity, and the beta log likelihood is strictly concave in its two shapes.
```

[lean-transfers3](2026-11-27-lean-transfers3-report.md#dossier-findings)

### paralogis

Before

```text
No corresponding scoped statement in this section.
```

After

```text
Exact real observations can give two global maximizers both in the all-free problem and at held scale one. At fixed positive shape the log-scale likelihood is strictly concave, including on constant samples.
```

[paralogis-math](2026-11-25-paralogis-math-report.md#held-scale)

### pareto3

Before

```text
strictly above $\mu$. Constant excesses have unbounded likelihood.
```

After

```text
strictly above $\mu$ with shape and scale free. Constant excesses then give unbounded likelihood.
```

[lean-transfers3](2026-11-27-lean-transfers3-report.md#dossier-findings)

### pareto3

Before

```text
Strict concavity gives the stated conditional results.
```

After

```text
Coercivity on the eligible logistic slices gives attainment, and strict concavity gives uniqueness.
```

[lean-transfers3](2026-11-27-lean-transfers3-report.md#dossier-findings)

### pareto4

Before

```text
detect_firmness: "argued"
  proof: ["dev/research/evaluations/2026-11-25-burr-math-report.md#inverse-and-shifted-burr"]
  fixed: ["min", "scale"]
```

After

```text
detect_firmness: proved
  proof: ["dev/research/evaluations/2026-11-25-burr-math-report.md#inverse-and-shifted-burr"]
  detect_proof: ["UnivariateML.BurrHeld.scale_pareto_supremum", "dev/research/evaluations/2026-11-25-burr-math-report.md#inverse-and-shifted-burr"]
  fixed: [min, scale]
```

[burr-math](2026-11-25-burr-math-report.md#inverse-and-shifted-burr)

### pgw

Before

```text
Existence block absent.
```

After

```text
existence:
  uniqueness: unknown
  statement: >-
    For positive data, holding all three coefficients gives evaluation. With
    two held, exponent and scale estimation are unique. Shape estimation
    is unique except for a sample constant at the held scale, where the
    likelihood is unbounded. At held exponent the joint fit is unique
    exactly for nonconstant data, with divergence on constants. With only
    scale held, nonconstant data attain exactly when that scale exceeds the
    sample minimum. At equality the threshold limit incurs a factor $2^{-m}$
    for $m$ tied minima. Constant data at or below held scale are unbounded;
    constants above it select the threshold. At held shape, constant data
    select the Gompertz face. Nonconstant data attain exactly when a retained
    stationary value at least equals that face; the face strictly wins when
    $(x_{\min}/x_{\max})^b\ge1/2$, where $b$ is the held shape. All-free
    constant samples are unbounded. Other all-free samples attain exactly
    when a retained stationary value ties or exceeds both optimized faces.
  reason: >-
    Log-scale compactness bounds exhaust competitive nonconstant escapes in
    the threshold and power-transformed Gompertz charts. Their tail cutoffs
    reduce the exact comparison to finitely many stationary profile values.
```

[pgw-math](2026-11-25-pgw-math-report.md#attainment-by-held-coefficients)

### pgw

Before

```text
detectors. Their reachability as likelihood suprema and exhaustiveness over
joint parameter paths remain open in the
[PGW analysis](../../../../../dev/research/evaluations/2026-11-11-pgw-theory-report.md#proved-boundary-paths).
```

After

```text
detectors. The [attainment proof](../../../../../dev/research/evaluations/2026-11-25-pgw-math-report.md#bounds-and-exhaustive-competitive-escapes) exhausts the competitive nonconstant escapes.
```

[pgw-math](2026-11-25-pgw-math-report.md#bounds-and-exhaustive-competitive-escapes)

### sged

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At held shape one and fixed skew, maximizing location is a weighted median set and every member has the same maximizing scale.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### sgomp

Before

```text
No corresponding scoped statement in this section.
```

After

```text
For nine ones and one exact real outlier defined by a profile crossing, the maximizing set consists of the exponential member and one positive-shape fit.
```

[sgomp-math](2026-11-25-sgomp-math-report.md#exact-separated-maxima-and-the-crossing)

### sgomp

Before

```text
Fixed-shape log-concavity and anchored slice absent.
```

After

```text
At fixed shape $\eta$, the standard log density is concave. At positive total exposure the anchored inverse-scale fit is unique, including the exponential member.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### skellam

Before

```text
uniqueness is not asserted. A constant sample $k$ has the maximum
```

After

```text
uniqueness remains open for integer samples. A single observed magnitude gives a unique fit, including samples that contain both signs. A constant sample $k$ has the maximum
```

[skellam2-math](2026-11-25-skellam2-math-report.md#reflection-and-a-uniqueness-theorem)

### skellam

Before

```text
No corresponding scoped statement in this section.
```

After

```text
Exact real weights on integer support can give two global winners, even at mean zero. Integer-sample ties remain open.
```

[skellam2-math](2026-11-25-skellam2-math-report.md#weighted-coexistence-and-integer-near-ties)

### sstd

Before

```text
Strict divergence was stated without treating the held physical standard deviation.
```

After

```text
Fix skew at its supplied value, or at one when it is free. As the ordinary
scale $a$ decreases to zero, take the raw mode equal to the concentration
point with free mean, or shift it by $-aM_\nu$ to preserve held mean, using
the moment equations above. The $k$ observations at the concentration
point have density of order $a^{-1}$ and the other densities are of order
$a^\nu$. At fixed shape their product diverges for $k>\nu(n-k)$.
If the physical standard deviation is held and shape is free, take
$\nu\downarrow2$. Then $a$ is proportional to $\sqrt{\nu-2}$ to
leading order, $(\nu-2)\log a\to0$, and the same product diverges for
$k>2(n-k)$. With shape and standard deviation held, neither construction
applies.
```

[tcert-math](2026-11-25-tcert-math-report.md#fernandezsteel-sstd)

### std

Before

```text
for the ordinary t scale $a$; on the domain $\nu > 2$ the infimum
```

After

```text
for the ordinary t scale $a$; for $k<n$, on the domain $\nu > 2$ the infimum
```

[lean-tdist](2026-11-27-lean-tdist-report.md#dossier-findings)

### std

Before

```text
$2n-3k$ is approached only as $\nu \downarrow 2$.
```

After

```text
$2n-3k$ is approached only as $\nu \downarrow 2$. At $k=n$ the exponent is $-n$ at every admissible shape.
```

[lean-tdist](2026-11-27-lean-tdist-report.md#dossier-findings)

### tlambda

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At held shape $2$ the density is uniform. At $2<\lambda\le8/3$ the standardized log density is strictly concave on the closed support.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### trgamma

Before

```text
when: The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held.
```

After

```text
when: The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held. The reachable trace has a loss of one log-likelihood unit per endpoint observation and is strictly beaten at a finite parameter in every bounded held-scale case.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### trgamma

Before

```text
message: fixed-endpoint power face
```

After

```text
message: fixed-endpoint power face
    detect: The held-scale cover can report this numerical comparison diagnostic. The reachable trace loses on the bounded branch.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### trgamma

Before

```text
With only $a$ held, the fixed-endpoint power limit is compared with the interior profile.
```

After

```text
With only $a$ held, the bounded profile strictly beats the reachable power trace. The numerical comparison can still report the trace as a diagnostic.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### trgamma

Before

```text
With only scale held, global profile bounds compare the interior with the
    fixed-endpoint power limit; constant samples above scale additionally
    have unbounded likelihood when both shapes are free.
```

After

```text
With only scale held, a finite fit strictly beats the fixed-endpoint power
    trace whenever the likelihood is bounded. Constant samples at or above
    the held scale give unbounded likelihood with both shapes free.
```

[trgamma-math](2026-11-25-trgamma-math-report.md#held-scale-and-both-shapes-free)

### triang

Before

```text
No corresponding scoped statement in this section.
```

After

```text
At fixed interior mode fraction the positive standardized density is strictly log-concave. The inverse-scale log likelihood gives a unique positive location–scale fit on a feasible, noncollapsing slice. A held physical endpoint gives an affine constraint in inverse scale and scaled location.
```

[transfer-math](2026-11-25-transfer-math-report.md#appendix-dossier-ready-existence-and-uniqueness-text)

### zeta

Before

```text
All-ones samples have an unattained point-mass supremum
```

After

```text
On infinite support, all-ones samples have an unattained point-mass supremum
```

[atlas-audit](2026-11-27-atlas-audit-report.md#3-contradictions-and-scope-mismatches)

### zeta

Before

```text
Finite windows `truncate = c(1, N)` admit every real exponent, since the
```

After

```text
At `truncate = c(1, 1)`, every exponent maximizes. For $N>1$, finite windows `truncate = c(1, N)` admit every real exponent, since the
```

[atlas-audit](2026-11-27-atlas-audit-report.md#3-contradictions-and-scope-mismatches)

The Lomax, Burr and inverse Burr crossings use exact real observations.
Burr’s held-power example permits any fixed positive power. The
paralogistic, inverse paralogistic, Bhattacharjee, Fréchet held-scale and
folded-normal examples also vary exact real observations. The
representative held scales in the crossings are one. Shifted Gompertz
uses one exact real outlier with no holds. Skellam coexistence instead
uses real observation weights on integer support. Near-ties on rounded
inputs do not give exact equality. [Tie taxonomy](2026-11-25-ties-math-report.md#classification-of-the-constructions)

## Open mathematical comparisons

- **Slash, free-location half tie.** The proof still needs a strict global
  exclusion or an exact competing finite witness in arbitrary unpaired
  data. A bound on improvements greater than the margin leaves possible
  finite equality.
- **Generalized beta at the near-constant shortcut.** The face ranking
  leaves a compact finite-parameter comparison and exact equality with
  the larger face unresolved.
- **Skew-t and SSTD critical atoms.** At equality the fixed-path
  calculation gives a finite trace. A general proof must bound the
  simultaneous nuisance, shape and skew paths and compare them with
  finite fits, including equality. The strict exponent and symmetric
  results cover their stated cases.
- **Binomial size and beta-binomial cardinality.** Both-free binomial
  multiplicity at positive mean needs an exact tie or an exclusion
  argument. General beta-binomial cardinality beyond the proved
  identifiable sizes through eight needs a stationary-curvature argument
  or an exact counterexample.
- **Ordinary Skellam samples and PGW.** For Skellam, the real-weight
  construction leaves rationality of the crossing weights and ties for
  ordinary integer samples unresolved. In PGW, the exact finite
  comparison does not provide root isolation at degenerate stationary
  points or exact equality of transcendental critical values.

## Verification

| Check | Workstation result |
| --- | --- |
| `just test-full` | 36 truth checks passed. R full reported 68,060 passes, zero failures, zero warnings and one skip for missing Zipf reference values. Simbox was unreachable and the run used the local four-slot lock. |
| `just test-changed master --no-long-double` | All 86 fitted families selected through shared consumers. Native C++ 136/136. Double-only C++ 181/181. R development 38,294 passes, zero failures and zero warnings, with 157 tier skips. JS build and 601/601 JS tests passed. |
| `just gen`, `just regen`, generated-output comparison | 116 Rd pages compared with the original source generation, with 47 changes to family help. All 232 equivalent detector sentences were byte-identical to the baseline. Generated wrapper functions were identical after removing roxygen comments. Site data changes were confined to `limits`, `unlinked_limits`, `existence` and `modifiers`. |
| `just site-check` | Passed following the modifier corrections. Built the full site, exercised the browser and MathML checks, and compiled all 86 example snippets. |
| Atlas, strict keys and loader | `existence_atlas.py --check`, `existence_keys.py`, and the R loader schema fixture passed. The atlas retained 87 families and 520 base regimes with the existing top-level status totals. A negative probe rejected an unknown grade, a missing Lean name, a missing file and a missing anchor. |
| `check_links.py` | The sole new-link diagnostic is the known beta-prime false positive `P+Q` at line 461. The checker returns 1 for that diagnostic. It also reports inherited findings. |
| `git diff --check` | Clean. |

The final native and double-only builds used workstation GCC 15.2. After
the late geometric declaration fix, the site and R generation were
repeated. The final JS build and suite used the revised modifier
metadata. The generated capability inventory reflects the scope
correction without changing which routes are implemented.
