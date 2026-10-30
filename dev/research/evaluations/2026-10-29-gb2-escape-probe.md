# GB2: how often does the fit escape? (probe, 2026-10-29)

`mlgb2` on 20 samples per cell, seed 2026, script
`2026-10-29-gb2-escape-probe.R` (run from the repo root after `just test`).
Outcome = interior fit, or the face named in the D1 stop message
(`mltrgamma` = generalized gamma, `mlinvtrgamma` = inverse generalized
gamma, `mllnorm`, `mlpower`, or the double Pareto face, which has no
package fitter). Numerical multibasin search, not a certificate.

| Generator | n = 50 | n = 500 |
|---|---|---|
| GB2(a=2, b=1, p=.4, q=.3) | double Pareto 10, interior 8, inv. GG 2 | interior 19, double Pareto 1 |
| GB2(3, 1, 2, 1.5) | inv. GG 6, interior 6, double Pareto 4, GG 3, lognormal 1 | interior 20 |
| Burr (p = 1, q = 2, a = 3) | GG 11, interior 4, double Pareto 3, inv. GG 2 | interior 20 |
| lognormal(0, .5) | GG 10, inv. GG 5, interior 4, double Pareto 1 | inv. GG 8, interior 7, GG 5 |
| gamma(2) | GG 14, double Pareto 2, inv. GG 2, interior 2 | GG 10, interior 10 |
| Weibull(1.5) | GG 11, double Pareto 4, interior 3, power 2 | GG 11, interior 9 |
| Lomax(1, 3) | double Pareto 8, GG 7, interior 4, inv. GG 1 | interior 18, GG 2 |

Observations:

- At n = 50 the GB2 MLE fails to exist for most samples, including samples
  from GB2 itself (12 and 14 of 20). The double Pareto face, which no other
  package model reaches, wins 10 of 20 times for GB2(2, 1, .4, .3) data.
- Truth on a face gives roughly a coin flip at n = 500: gamma and Weibull
  data (members of the generalized gamma face) escape to that face in about
  half the samples, as in the Self–Liang boundary mixture.
- Lognormal data sit on the corner shared by the GG and inverse GG faces:
  at n = 500 they split between the two sides and the interior; the
  lognormal itself almost never wins (it can only tie, being a limit of both
  faces).
- Fits take 0.2–3 s at n = 500 (slowest for lognormal data).
