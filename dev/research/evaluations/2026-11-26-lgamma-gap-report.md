# Log-gamma Jensen gap near the lower endpoint

TASK-363 · 2026-11-26 · Baseline `2d89fd4a117a7ee7850558e6a04b4c4d74b87589`

Both 11-observation samples now return certified singleton fits. The four-anchor sample previously certified a shape and rate each 1.1145% below the MLE. The sixteen-anchor sample previously stopped with `TransformFailure`. Here the anchor is repeated ten times in each sample.

## Baseline reproduction and reference

Before editing the fitter, the workstation C++ probe ran the public `lgamma_mle` from the baseline checkout. The independent reference used mpmath at 360 decimal digits. It took logarithms of the exact binary64 inputs and evaluated the direct gamma log likelihood with the log transformation Jacobian. It solved the appropriate shape score in log-shape coordinates and used the closed-form rate wherever the rate was free. The evidence retains 70 significant digits for shape, rate, total log likelihood, and the Jensen gap. [Probe](2026-11-26-lgamma-gap-data/probe.cpp), [reference calculation](2026-11-26-lgamma-gap-data/check.py), [recorded results](2026-11-26-lgamma-gap-data/results.json)

| Data with outlier $1+2^{-52}$ | Compiled baseline result | Reference shape | Reference rate | Reference log likelihood |
| --- | --- | ---: | ---: | ---: |
| Ten 4s | Certified singleton `(0.22246354268378057, 0.17652087739466427)` | 0.224970924694730048 | 0.178510440570710308 | −10.593818115507188265 |
| Ten 16s | `TransformFailure` | 0.221229924637113022 | 0.087771011671805567 | −31.542867214932793868 |

The four-anchor sample has exact-input gap approximately 3.211079606477078536. The earlier binary64 source replay gave 3.25306352469398656. That raised the gap used by the root solver and lowered the reference likelihood in the one-sided restoration check by about 0.10274. The check could therefore pass even though the rate-matched point lost about 0.00057667 log-likelihood units against the true peak. For the sixteen-anchor sample the rounded relative deviation was exactly −1, making the old gap infinite. [Prior numerical analysis](2026-11-25-lgamma-math-report.md#rare-observation-near-the-lower-endpoint)

## Computation and accuracy

Let $y_i=\log x_i$, $y_0$ be the transformed anchor, $d_i=(y_i-y_0)/y_0$, $M=1+\overline d$, and $u_i=(d_i-\overline d)/M$. In exact arithmetic, $y_i/\bar y=1+u_i$ and $\overline u=0$, so the Jensen gap is the mean of $u_i-\log(1+u_i)$. At a nonconstant sample the gap is positive. The all-free profile score $\log k-\psi(k)-s$ is strictly decreasing and has one zero. The unique rate at that shape is $k/\bar y$. [Existence and uniqueness derivation](2026-11-25-lgamma-math-report.md#exact-likelihood-and-decisions-by-hold-mask)

The correction is in `gamma/transformed.hpp:65` and is gated to `GammaMap::Log`. For $|u_i|<0.1$ the existing series starts at $u_i^2/2$ and avoids subtracting nearly equal first-order terms. Elsewhere the code subtracts the independently computed $\log(y_i/y_0)-\log M$ from $u_i$, using the stored `logdelta` and `log1p` of the mean deviation. This identity removes the amplification of absolute rounding error in $u_i$ by the factor $1/(1+u_i)$ when forming the logarithm. The far term is at least $0.1-\log(1.1)$, approximately 0.00469, so a small absolute error in the independent log ratio stays small relative to the term. The terms are reduced with the existing compensated sum. [Gap computation](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/transformed.hpp#L65)

The corrected gap drives both the monotone shape solve and the ideal likelihood at the mean-matched rate. The actual public-point likelihood uses its own penalty evaluation. The ideal and actual likelihoods are compared with the existing tolerance and floating-point allowance. This also repairs the certification check on the held-shape route, where the rate was already computed accurately from the mean. [Restoration calculation](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/transformed.hpp#L169), [updated dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/README.md#L108)

## Route sweep and regression tests

The 24-case grid uses anchors $4,16,2^{16},2^{128},2^{512},2^{1023}$ and counts 10, 15, 31, and 127. At baseline, four grid cases returned roots outside tolerance and 20 stopped. The expanded sweep varied the outlier among 1, 2, and 8 binary64 ulps above one, put it first and last, and exercised each hold mask with held shape 2 and held rate 3. Each mode ran 144 cases per mask. Scaled error means the maximum of the two relative coefficient errors and the absolute log-likelihood error divided by $\max(1,|\ell|)$.

| Hold mask | Baseline result | Largest corrected scaled error, native or double-only |
| --- | --- | ---: |
| Neither | 96 stops and 48 fits outside tolerance, 28 of those certified | $2.30\times 10^{-14}$ |
| Shape | All within tolerance, 20 falsely uncertified by restoration | $3.08\times 10^{-15}$ |
| Rate | All within tolerance and certified | $3.23\times 10^{-13}$ |
| Both | All within tolerance and certified | $1.57\times 10^{-15}$ |

All 576 corrected fits in each precision mode were within tolerance and certified as singletons. The C++ and R regressions pin the full 24-case all-free grid, including both 11-observation examples, with 17 significant digits from the reference. They check coefficients, log likelihood, certification, and uniqueness. The R regression uses the default controls. [C++ test](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/test.cpp#L224), [R test](../../../bindings/r/distributions/lgamma/test.R#L162)

To replay the sweep, compile `probe.cpp` as C++17 against `univariateML-cpp/include` and the available Boost headers, then pass the executable and an output JSONL path to `check.py`. For the baseline include tree, extract `univariateML-cpp/include` from the recorded commit with `git archive` under `build/scratch/`. For the double-only replay add `-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

## Final verification

All final runs used the workstation. The native and double-only C++ runs used GCC 15.2.

| Check | Result |
| --- | --- |
| `/tmp/umvl-cpu/run4 nice -n 10 just test-changed --no-long-double` | 19/19 C++ tests in each mode across 11 selected families. R dev tier had 3,062 passes, 20 skips, zero failures and zero warnings. JS build passed and the selected JS run passed 284/284. |
| `fit-snapshot.py --family lgamma` and `diff_baseline.py` | 108/108 identical results in each of native and double-only, with no added or removed records. The two modifier baselines contain zero `lgamma` records. The driver has no `lgamma` entry in its modifier registry, so an explicit modifier replay for this family raises a missing-key error. |
| `fit-density.py` on both fresh snapshots | Zero new failures. All 97 comparisons per mode passed, with maximum scaled discrepancy $1.3\times 10^{-15}$ in each mode. |
| `fit-consistency.py` on both fresh snapshots | Zero new failures. |
| `strict-uniqueness.py` | Passed, covering 218 proof sites. The existing log-gamma singleton proof covers the corrected fit. |
| `just site-check` | Passed on retry with `BROWSER_EXECUTABLE` pointing to Quarto's installed headless Chromium. The first run found no Chromium in the default Playwright cache. The retry passed both browser checks, all 86 family pages and all 86 C++ snippets. |

## Open

A uniform floating-point enclosure for the full log-gamma coefficient certificate remains open. The exact identities and the monotonicity argument establish the shape and rate of the maximizer, and the compiled sweep measures the default-tolerance margin. An enclosure at tighter tolerances would need error bounds across the series switch, the independent log ratio and the digamma evaluation, propagated through the shape bracket and transformed mean to the two public coefficients. [Numerical follow-up from the math report](2026-11-25-lgamma-math-report.md#what-this-changes-for-the-fitter)
