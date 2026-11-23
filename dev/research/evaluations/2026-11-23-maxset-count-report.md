# Typed maximizer sets for count families and modifier routes

TASK-229 part B1 · `codex/maxset-count` · baseline `33a473f4` using the frozen snapshots of `83ed77a9`

The count fitters and routes publish their proved maximizing unions, isolated points and relations. The four snapshot comparisons preserved selected coefficients, likelihoods, result kinds and limiting law values for every shared key in the replay. The strict replay passed 112 TASK-229 keys with zero new failures. [Strict output and complete FIXED list](2026-11-23-maxset-count-data/fit-consistency.txt)

## Base families

| Family | Published set and proof |
|---|---|
| Hypergeometric | The support runs from `max(0, m+k-N)` to `min(m,k)`. For a zero atom the union fixes either count to zero. A positive atom requires one count equal to the population and the other equal to the observation. The Boxes carry the count order and the cap `2^53 - 1`. A scalar unidentified interval is an integer Box. An exact adjacent ratio of one gives two Points at the unimodal crossing. With one draw, the Bernoulli likelihood fixes the empirical success fraction. The Count Ratio encodes all admissible multiples of its primitive numerator and denominator. `hyper/mle.hpp:801`, `:821`, `:835`. |
| Beta-binomial | Size zero is the sole zero atom with strictly positive shapes. At size one the Bernoulli success fraction is `alpha/(alpha+beta)`, yielding a Part Over Sum relation on positive shapes. The held-shape successive ratio crosses one at the two adjacent sizes only when its integer-ratio identity is exact. `bbinom/mle.hpp:546`, `:942`, `:962`. |
| Conway–Maxwell binomial | The zero atom is the union of size zero and probability zero. A positive endpoint atom fixes size at the observation and probability at one. At size one the dispersion drops from the likelihood because both binomial coefficients are one. The mixed Bernoulli fit saturates the empirical likelihood and every larger finite design gives mass above one. With dispersion one the exact binomial ratio proves the two adjacent Points. The equivalent binomial comparison law stays with the size-one fit. `cmb/mle.hpp:496`, `:587`, `:1193`, `:1456`. |
| Conway–Maxwell Poisson | Rate zero gives the zero atom at every nonnegative dispersion, also when rate zero is held. Every positive rate gives mass to one. `cmp/mle.hpp:35`. |
| Negative binomial | Size zero and probability one are the two zero-atom faces. The Boxes have size in `[0, Inf)` and probability in `(0, 1]` before slicing. Positive size with probability below one gives positive mass to one. `nbinom/certificate.hpp:56`. |
| Poisson–inverse Gaussian | Shape zero and positive mean give exactly the zero atom, including a held zero shape. The Box opens the mean axis when it is free. `pig/mle.hpp:379`. |
| Discrete Weibull | With the first shape held and zeros observed, the zero mass is one minus that shape for every positive second shape. The Box fixes the first shape and opens the second. `dweibull/mle.hpp:418`. |
| Geometric, logarithmic and Poisson | The migrated plateaus for these families occur on the modifier routes below. |

Hyper's nonconstant swap path returns undetermined uniqueness and retains comparison candidates. For a held population the first grid pass now retains the leading two unordered designs. A second orbit inside the existing comparison margin also produces undetermined uniqueness when the selected design is on the diagonal. The selected vector and number of likelihood evaluations are preserved. Exact rational enumeration at `N = 3` and `N = 5` finds three and four maximizers, respectively, for the sample `(0, 1)`. At `N = 3` the selected design is diagonal. `hyper/mle.hpp:611`. [Exact enumeration](2026-11-23-maxset-count-data/hyper-orbits.txt) · [Native comparison payloads](2026-11-23-maxset-count-data/hyper-orbit-fits-native.jsonl) · [Double-only comparison payloads](2026-11-23-maxset-count-data/hyper-orbit-fits-double.jsonl)

Beta-binomial finite-size comparisons with unresolved cardinality and CMB numerical size comparisons that retain an accurate point also expose undetermined uniqueness. Their candidate payloads use the comparison path. Exact ties use the typed path. Certificate replay already gates on certification and therefore handles these uncertified estimates without a replay change.

## Modifier routes

The modifier dispatcher supplies the fitted-domain support and requested holds before sending the set to the shared renderer. A singleton slice carries the unique estimate. The first representative of a non-singleton slice is the reported estimate.

| Route | Published set and proof |
|---|---|
| Zero-inflated binomial and negative binomial | For zeros, add the inflation-one Box to the parent zero-atom union. The mixture is a zero atom exactly when either condition holds. Mixed binary data at size one fix the success fraction `prob * (1 - pi)` and give a complemented Product relation. A larger design assigning positive probability to one also assigns some probability above one. `bindings/common/distributions/binom/zib_fit.hpp:58`, `:70` and `nbinom/zinb_fit.hpp:49` in the same adapter tree. |
| Zero-inflated Poisson and geometric | ZIP uses the inflation-one Box over strictly positive rates. The geometric route projects the negative-binomial union through the held size-one slice. Size one gives the geometric law at every admissible probability, so the geometric union consists of probability one or inflation one. `bindings/common/distributions/pois/zip_fit.hpp:52` and `bindings/common/modifiers.hpp:226`. |
| Zero-modified binomial, negative binomial, geometric, logarithmic and Poisson | When positives are absent, the likelihood is `p0^n` throughout the feasible positive-component domain. The Box fixes the supplied or fitted zero mass and slices any held parent coefficients. The logarithmic route publishes `theta` in the coordinates and rows. With all positives equal to one, the binomial route needs size one or probability zero. The negative-binomial route needs probability one at any allowed size, including the size-zero logarithmic extension. In both cases the zero mass contributes its separate Bernoulli likelihood, so the same sets apply to mixtures of zeros and ones. `zmbinom_fit.hpp:55`, `zmnbinom_fit.hpp:50`, `zmgeom_fit.hpp:30`, `zmlogarithmic_fit.hpp:30`, `zmpois_fit.hpp:30`, and `bindings/common/modifiers.hpp:119`. |
| Zero-truncated binomial and negative binomial | For constant ones, the positive-conditioned binomial atom is attained at size one or probability zero. The positive-conditioned negative-binomial atom is attained at probability one for every allowed size. Positive-conditioning for Poisson and geometric has one free coefficient and the atom at one gives a singleton. `ztbinom_fit.hpp:50` and `ztnbinom_fit.hpp:39`. |
| Binomial and Poisson windows | In the binomial interior the retained support is the intersection of the window and `[0, size]`. Where that intersection is a singleton, the probability axis is open at both parent endpoints and the surviving endpoint atoms are included as separate pieces or a closed endpoint. The zero atom survives every window containing zero. The probability-one atom is feasible at size equal to the observed count. The Poisson singleton retains every positive rate and also rate zero when the retained count is zero. `bindings/common/modifiers.hpp:171`, `:190`. |
| ZIPF, geometric and held-size negative-binomial windows | Every finite exponent gives mass one on `[1, 1]`. For geometric and held-size negative-binomial singleton supports, every interior probability gives conditional probability one. The probability-one atom is included exactly when the retained count is zero. Size zero gives a singleton effective support in the negative-binomial route whenever its window contains zero. `bindings/common/modifiers.hpp:74`, `:139`. |

The independent expected sets are built in `typed_count_expected` from sample counts, holds and support. Strict compares unions structurally, checks selected and representative membership, and verifies hold intersections. The three routes without frozen aliases have nine supplemental cases in each precision mode. [Independent recipe](../../validation/flat_maxima.py) · [Supplemental checker](2026-11-23-maxset-count-data/check_extra_routes.py) · [18 passing cases](2026-11-23-maxset-count-data/extra-routes.txt)

## Snapshot and ledger results

| Capture | Records per mode | Typed payload moves | Moves to undetermined uniqueness | Added fixtures |
|---|---:|---:|---:|---:|
| Ten B1 public count families, native and double-only | 967 | 54 | 26 | 3 |
| All sixteen frozen route aliases, native and double-only | 2,006 | 84 | 0 | 3 |

The public additions are the two orientations of the hyper one-draw ratio and the beta-binomial shape ray. The route additions are the ZIB Product and the two hurdles with mixed binary data and a held zero mass. The typed moves replace legacy diagnostic and representative payloads. The uniqueness moves are hypergeometric numerical orbit comparisons, with any corresponding withdrawal of the certification flag and comparison diagnostics. The CMB equal-likelihood binomial face retains its model, coefficients, likelihood and bounds alongside the typed own-law set. A late rerun recaptured the CMB records in both modes to verify that comparison payload.

The diff helper ran against the stored baselines in all four modes. Its public captures are scoped to the ten B1 families. The per-key logs give every move within each capture and distinguish the added fixtures from modified records. [Native public moves](2026-11-23-maxset-count-data/moves-native.txt) · [Double-only public moves](2026-11-23-maxset-count-data/moves-double.txt) · [Native route moves](2026-11-23-maxset-count-data/moves-routes-native.txt) · [Double-only route moves](2026-11-23-maxset-count-data/moves-routes-double.txt)

All 112 B1 keys appear in the FIXED list. The two other TASK-229 keys are `phenomenon:flat-maximum|native|genbeta|class|constant-flat-power-product` and its `double` counterpart. Their ledger description is missing admissible witness rows and the diagnostic. Genbeta belongs to B2. [FIXED keys](2026-11-23-maxset-count-data/fit-consistency.txt)

## Test assertions and runs

- The hyper R tests check canonical Boxes and Count Ratio, preserve point likelihood checks, and read comparison candidates and `NA` for the nonconstant swap. The new held-population regression covers the diagonal and off-diagonal examples.
- The beta-binomial C++ tests inspect Box and Points tags in place of legacy prose. The R tests check set tags and representative membership, preserve the selected first row and exact adjacent sizes, and add the size-one ratio check.
- The CMB C++ test checks a nonempty typed payload. The CMB R tests check maximizing rows, set tags and the canonical diagnostic. The adjacent-observation test checks the equivalent binomial comparison. The shared assertion in the binomial R file checks the full open dispersion interval where it formerly expected two specific witness values.
- The JS CMB assertions check the dispersion Box, the separate binomial comparison, the zero-atom representatives, the two exact adjacent sizes and their point likelihood. The other new R assertions cover the ZIB complemented Product, a held hurdle zero mass, geometric inflation projection, and the zero and positive held-size window cases.

| Check | Machine | Result |
|---|---|---|
| `just test-changed --no-long-double`, C++ stages | Workstation, GCC 15.2, four slots | Native 135/135. Double-only 180/180. |
| Initial R dev stage of `test-changed` | Workstation, four slots | Two legacy CMB payload assertions failed. The corrected binomial and CMB R files passed the focused retest. |
| `just test-full` | Workstation, four slots, after simbox exit 75 | Truth-generator tests 36/36. R had 66,951 passes, zero failures, zero warnings and one existing ZIPF skip. |
| `just js-build` and `just js-test` | Workstation, four slots | Build passed and tests passed 593/593. |
| `just site-check` | Workstation, four slots | Passed, including 85 generated C++17 snippets and the browser checks. |
| Four snapshot captures, comparisons, strict and supplemental probes | Workstation, GCC 15.2, single slots | 112 FIXED, zero NEW and 18 supplemental route cases passed. |

The R assertion corrections were the final changes. The full R run exercised both corrected files and completed the requested late retest. The JS and site stages ran separately after the original runner stopped at the legacy assertions. [Initial runner](2026-11-23-maxset-count-data/targeted-suite.txt) · [Focused assertion retest](2026-11-23-maxset-count-data/r-assertion-retest.txt) · [Final suites](2026-11-23-maxset-count-data/final-suites.txt)

## Open

- Publishing an exact set for a nonconstant hyper swap requires an exhaustive comparison of surviving orbits. The held-population pass detects additional plausible orbits but uses floating-point likelihoods. Where the interval or exact arithmetic comparison is missing, the public result is undetermined with candidates. Free-population swaps also require exclusion of additional orbits across the population search and its tail.
- For near-rounded finite-size comparisons in beta-binomial and CMB, publication requires exact equality or an independent separation of the plausible sizes. The existing accurate candidate remains available in the comparison payload when cardinality is undetermined.
