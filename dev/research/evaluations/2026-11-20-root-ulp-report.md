# Root-bracket rounding (TASK-214.31)

The 40 listed root checks pass, 20 in each precision mode. Replay judged 1,768 cases across the affected families with replay coverage and found zero failed checks. The 2,459 snapshots per mode show zero changes in outcome kind, reason or certification flag.

The comparisons use master `1252894e` and the frozen fit snapshots. The earlier width analysis decomposes the error into the computed bracket, score and data rounding divided by the score slope, and reconstruction of the public coefficient. [Width analysis](2026-11-15-root-widths-report.md) · [Frozen snapshots](2026-11-15-fit-snapshot-data/) · [Accuracy contract](../../design/accuracy-contract.md)

## Widths by path

Let $\varepsilon=2^{-52}$, $k$ be the public shape and $\rho$ the distance from the returned point to its farther computed bracket endpoint, expressed in the units of the claim. Code paths below are relative to `univariateML-cpp/include/cppunivariate/`.

| Path | Old claim | New rule and derivation | Source |
|---|---|---|---|
| Fréchet with location and scale held | $4\varepsilon$ full width, computed in $\log(k\,\mathrm{unit})$ | Bisect in $\log k$ using the log ratios of the excesses to the held scale. Report $\rho+8\varepsilon$, including excess, score and exponential rounding. An adjacent-double stop contributes the full endpoint spacing to $\rho$. | `distributions/frechet/mle.hpp:206`, `core/transformed_location.hpp:11`, `core/transformed_location.hpp:77` |
| Inverse Weibull with scale held | $4\varepsilon$ full width in standardized log shape | Use the same public log-shape bracket and radius with the Gumbel score on the raw log ratios. The direct ratios avoid cancellation between the anchor-centered data and held-scale shift. | `core/transformed_location.hpp:77` |
| Log-logistic with scale held | $4\varepsilon$ full width in standardized log shape | Use the public log-shape bracket and the farther endpoint plus $8\varepsilon$ with the logistic score. The fallback measures endpoint spacing when the target width is unreachable. | `core/transformed_location.hpp:77` |
| Weibull with both coefficients free | Default relative full width $10^{-14}$ in $1/k$ | Feed the same Gumbel equation centered values of $-\log(x/a)$. Use `log1p((x-a)/a)` close to the anchor and `log(x/a)` for other normal ratios. The ratio removes the errors proportional to the large common logs in the huge and tiny samples. Ratios outside the normal range use the log difference to preserve range. | `distributions/weibull/mle.hpp:58`, `core/transformed_data.hpp:43`, `distributions/gumbel/mle.hpp:109` |
| Gamma free-shape bracket | $4\varepsilon$ full relative width | Report $\rho+2\varepsilon+(E_s+E_G)/m$ with sample-dependent Jensen-gap error, digamma-score error, and log-shape slope $m$. The geometric midpoint is reconstructed by two square roots and a multiplication. | `distributions/gamma/mle.hpp:95`, `distributions/gamma/mle.hpp:113`, `distributions/gamma/mle.hpp:253` |
| Nakagami Square map with second moment held | Up to $4\varepsilon$ full relative width | Report the same relative-radius formula with $E_s=(70+n/2)\varepsilon s$. The direct digamma difference below shape 100 carries absolute error on the scale of its terms. The slope converts that error to relative shape units. | `distributions/gamma/transformed.hpp:111`, `distributions/gamma/mle.hpp:95` |

For the gamma equations, $G(k)=\log k-\psi(k)$ and $m=|dG/d\log k|$ at the upper endpoint. The large-shape series gives the slope and uses $E_G=3\varepsilon G(lo)$. At a direct-score endpoint the allowance uses $\varepsilon\max|\log k|+2\varepsilon\max|\psi(k)|+\varepsilon G(lo)/2$ over the bracket endpoints, with the larger of the series and direct allowances when the direct branch is present. For the concentrated Jensen gap, write $s=P-C$ for the mean of $\phi(t)=t-\log(1+t)$ minus $\phi(\bar t)$. The allowance scales with $P+C$, the serial sum length, and the mean correction sensitivity $|\phi'(\bar t)|\,\overline{|t|}$. For Nakagami, $70\varepsilon s$ is the practical transformed-data and score allowance from the width analysis, with an additional $n\varepsilon s/2$ for the serial reduction.

The bisection radii measure the bracket defined by computed signs. The added margins use first-order score sensitivity and practical libm and Boost digamma error estimates. The location, gamma and Nakagami radii in these snapshots are at most $2.665\times10^{-15}$, $8.794\times10^{-15}$ and $2.699\times10^{-14}$ respectively. The synthetic Nakagami case near shape 80 exercises a larger radius of about $4.8\times10^{-13}$.

## Snapshot schema and replay

The optional `result.root_radius` field is a hexadecimal binary64 value. It measures additive distance in log shape for the three held-scale location paths and relative distance about the returned shape for the gamma bracket and held Nakagami Square path. Each fitter computes its own value. Replay consumes it directly in its existing two-endpoint sign test. Archived records without the field use their earlier widths. The Weibull endpoint comparison uses the same width as before.

The baseline replay reproduced all 40 known failures. The updated replay found zero failed checks across 2,348 comparisons and fixed all 40 named entries. Of the informational profile-root comparisons, 39 changed from failing to holding and four went in the other direction. The four are inverse Weibull resample `n3-s2` in both modes, inverse gamma archive `003753` in double-only mode, and Nakagami archive `005020` in native mode. The judged restoration checks for all four held with likelihood gaps below $2.1\times10^{-30}$. [Replay log](2026-11-20-root-ulp-data/replay.log) · [Records](2026-11-20-root-ulp-data/replay-records.jsonl.gz) · [Baseline verdict comparison](2026-11-20-root-ulp-data/baseline-replay-diff.txt)

| Family | Source | Case | Mask | Native | Double-only | Evidence |
|---|---|---|---:|---|---|---|
| frechet | archive | 001946 | 6 | holds | holds | numerical |
| frechet | archive | 001949 | 6 | holds | holds | numerical |
| frechet | archive | 001955 | 6 | holds | holds | numerical |
| frechet | archive | 010365 | 6 | holds | holds | numerical |
| gamma | archive | 001983 | 0 | holds | holds | numerical |
| gamma | archive | 001986 | 0 | holds | holds | numerical |
| gamma | archive | 001994 | 0 | holds | holds | numerical |
| invweibull | archive | 004257 | 2 | holds | holds | numerical |
| invweibull | archive | 004259 | 2 | holds | holds | numerical |
| invweibull | archive | 004260 | 2 | holds | holds | numerical |
| llogis | archive | 004562 | 2 | holds | holds | numerical |
| llogis | archive | 004563 | 2 | holds | holds | numerical |
| llogis | archive | 004565 | 2 | holds | holds | numerical |
| llogis | archive | 004566 | 2 | holds | holds | numerical |
| llogis | archive | 004574 | 2 | holds | holds | numerical |
| llogis | archive | 010575 | 2 | holds | holds | numerical |
| llogis | archive | 010576 | 2 | holds | holds | numerical |
| naka | archive | 005059 | 2 | holds | holds | numerical |
| weibull | class | huge | 0 | holds | holds | interval |
| weibull | class | tiny | 0 | holds | holds | interval |

## Fit snapshots

The family scope covers the six named families, the direct consumers of the two transformed headers including the gamma-map delegates, and additional dependants including Burr and Pareto III. Both four-slot runs reproduced the saved single-slot snapshots byte for byte. Changed rows compare the existing fit payload against the frozen baseline, with the added radius excluded. The maximum is taken over matched finite coefficients.

| Family | Cases per mode | Changed native | Changed double-only | Max relative move native | Max relative move double-only |
|---|---:|---:|---:|---:|---:|
| burr | 220 | 0 | 0 | 0 | 0 |
| expweibull | 28 | 0 | 0 | 0 | 0 |
| frechet | 208 | 28 | 28 | 7.65e-15 | 7.65e-15 |
| gamma | 124 | 0 | 0 | 0 | 0 |
| genbeta | 220 | 0 | 0 | 0 | 0 |
| invburr | 220 | 23 | 23 | 9.72e-15 | 9.72e-15 |
| invgamma | 124 | 25 | 25 | 1.93e-13 | 1.93e-13 |
| invtrgamma | 180 | 0 | 0 | 0 | 0 |
| invweibull | 112 | 45 | 45 | 3.76e-14 | 3.76e-14 |
| lgamma | 108 | 0 | 0 | 0 | 0 |
| llogis | 116 | 37 | 37 | 5.67e-14 | 5.67e-14 |
| lnorm | 128 | 16 | 16 | 5.68e-14 | 5.68e-14 |
| logitnorm | 115 | 10 | 10 | 5.68e-14 | 5.68e-14 |
| naka | 124 | 20 | 20 | 9.45e-15 | 9.45e-15 |
| pareto3 | 104 | 8 | 15 | 2.17e-16 | 9.72e-16 |
| trgamma | 204 | 0 | 0 | 0 | 0 |
| weibull | 124 | 13 | 11 | 3.45e-14 | 3.45e-14 |

There were zero outcome kind, reason or certification changes. Inverse-gamma `tiny` had the largest move in both modes. Its `beta` relative error fell from $1.92\times10^{-13}$ to about $6.5\times10^{-16}$ against an 80-digit reference. The absolute-log scale reconstruction in log-logistic archive `004533` stepped across one spacing of the large logarithm. The resulting relative scale error moved from $3.87\times10^{-14}$ to $-1.80\times10^{-14}$. The Weibull huge and tiny shape errors fell to about $4.2\times10^{-15}$ and $4.3\times10^{-15}$ from $3.0\times10^{-14}$ and $2.1\times10^{-14}$.

[Native diff](2026-11-20-root-ulp-data/diff-native.md) · [Double-only diff](2026-11-20-root-ulp-data/diff-double.md) · [Native reference comparisons](2026-11-20-root-ulp-data/moved-native.txt) · [Double-only reference comparisons](2026-11-20-root-ulp-data/moved-double.txt)

## Tests and goldens

Four doctests were appended. Gamma archive `001983` checks the reported radius against an 80-digit shape root. Nakagami checks a held-second-moment sample with shape near 80. Log-logistic checks the adjacent-double log-shape stop and the endpoint signs on a concentrated sample. Weibull checks shape invariance under exact power-of-two scaling at exponents $-996$ and $996$. The six affected dossiers describe their claims and the Weibull dossier names `tol`.

The golden files had zero edits. Among the frozen golden input rows, only log-logistic coefficients moved, by 6 ulp in shape and 2 ulp in scale in both modes. The 50-digit references are shape `0.92746170367810178843191620715092797218261789251379` and scale `0.49602537138022452932405870464990964217648054542142`. [Reference results](2026-11-20-root-ulp-data/moved-native.txt)

| Check | Machine | Result |
|---|---|---|
| `just cpp-test` | Workstation, GCC 15.2.0 | 110/110 |
| `just cpp-test-no-long-double` | Workstation, GCC 15.2.0 | 154/154 |
| `just test-full` | Workstation, R 4.6.1 | 36 truth checks passed. R reported 0 failures, 29 warnings, 1 skip and 65,756 passes. |
| Targeted doctest binary | Workstation, both precision modes | 206/206 and 86,063 assertions per mode |
| Fit snapshots and certificate replay | Workstation, both precision modes | 2,459 rows per mode, 40/40 named checks hold, zero new failures |
| `just quality` | Workstation | 17/17 family checks passed |
| `just js-build` and `just js-test` | Workstation | Build succeeded and 586/586 tests passed |
| `just site-check` | Workstation | Passed, including 85 family examples compiled and linked |
| `python3 dev/tools/check_links.py` | Workstation | 0 new broken links, 45 inherited broken links |

Simbox returned 75 on the suite attempts and SSH timed out on retry. The full suites ran on the workstation. The `just test-changed --no-long-double --dry-run` selected all 85 families and the unfiltered recipes because of the shared headers and validation changes. The explicit unfiltered runs above use the full R tier, which contains the dev tier. [Suite evidence](2026-11-20-root-ulp-data/suite-results.txt) · [Targeted doctests](2026-11-20-root-ulp-data/targeted-doctest.txt)

## Reproduction

Pass the 17 family IDs in the provenance files to `fit-snapshot.py --family` with `--jobs 4`, adding `--no-long-double` for the second snapshot. Compare against the frozen files with `diff_baseline.py`. Run the replay with the 12 implemented replay IDs and the resulting native and double-only directories. The comparison and replay also accept the compressed snapshots below.

```sh
python3 dev/validation/cert-replay.py "$native" "$double" \
  --family frechet,invweibull,llogis,weibull,gamma,naka,invgamma,lgamma,lnorm,logitnorm,pareto3,genbeta \
  --records "$records"
```

[Native snapshot](2026-11-20-root-ulp-data/native.jsonl.gz) · [Double-only snapshot](2026-11-20-root-ulp-data/double.jsonl.gz) · [Comparison script](2026-11-20-root-ulp-data/diff_baseline.py) · [Known-entry extractor](2026-11-20-root-ulp-data/known_replay.py) · [Reference script](2026-11-20-root-ulp-data/moved_cases.py)

## Open

Pareto III restoration for archive `005612` remains open in the standard replay in both modes. Its high-precision evaluation raises an integer-conversion overflow on the same record in the baseline. Independently solving its held-shape logistic location score at 80 digits gives a likelihood gap of $2.01\times10^{-32}$ at the returned scale. Closing the standard record requires a stable restoration evaluator for that case. All named TASK-214.31 verdicts are settled. [Independent calculation](2026-11-20-root-ulp-data/pareto3-005612.txt)
