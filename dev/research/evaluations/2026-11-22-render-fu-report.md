# TASK-214.18 — Comparison candidates and non-unique fits

All 920 archived fits per precision mode agree field for field with the stored baselines and the replay of master at `cc7bfa80`. Four additional near-binomial samples retain their uncertified estimates and now carry the enclosed finite candidate alongside the binomial comparison. The targeted C++, R, JavaScript and site checks passed on the workstation.

## Status on master

| Entry | Concrete case and finding |
| --- | --- |
| Cauchy | With observations \((1,1,5,5)\) and scale 1, master already returns a certified non-unique estimate with both locations. The outstanding point-value issue was in the core, which recomputed the likelihood only for the selected comparison witness. |
| Bhattacharjee | With \(x=(-1.0000001,1.0000001)\), held location 0 and held scale 1, master returns the successful finite search uncertified. The existing comparison test at `bhatt/test.cpp:761` also shows that master recognizes a marked enclosed root. Master lacked a point likelihood for each marked witness, and a failed fallback could discard the certificate's comparison status. |
| EMG | With \(x=(-2,-1,0,1,2)\) and held location 0, the normal comparison remains unresolved. The rounded closure-transition sample \(x=(-10,2.746841848406631)\) with location 0 and scale 1 held also has only the normal face and an unenclosed finite witness. Master can return a polished candidate from a comparison. Transport of the certificate's coefficient boxes was open. |
| Skew-t | Settled. With \(x=(2,2,2)\), held location 2, scale 1 and degrees of freedom 5, the factor \(2T_6(0)=1\) makes the likelihood flat for every real alpha. Master returns `certified = TRUE`, `unique = FALSE`, and representative alphas \(-1\) and \(1\) at the same point likelihood. See `st/certificate.hpp:30`. |
| Beta-binomial | At size 3, the counts \((414,860,592,137)\) for observations \(0,1,2,3\) already produce an uncertified estimate on master through the finite-search fallback. That path returns an empty candidate table. Retaining the enclosure and its point likelihood in the fixed-size comparison was open. |
| GPD and other dossiers | The rounded sample \((a,a,1)\) with \(a=0.07880675592383729\) exercises the unresolved comparison in `bindings/r/distributions/gpd/test.R:166`. The payload already existed. The dossier's “tie stop” wording was stale. |
| NEWS and Gompertz | Settled. `bindings/r/NEWS.md:10` already gives the revised comparison rule. Empty Gompertz data and a sample containing NaN both return invalid input in each mode. |

The [case replays](2026-11-22-render-fu-data/cases.jsonl) include inputs, flags, candidates and likelihoods. [Replay driver](2026-11-22-render-fu-data/replay.py)

## Candidate handling

- **Cauchy.** `cauchy/mle.hpp:403` retains a point log likelihood and an accuracy result for each ambiguous root alongside the likelihood bounds. It projects the represented location back to the scale chart, charges the distance to the farther bracket endpoint plus a floating-point allowance, and checks the requested tolerance. `bindings/common/distributions/cauchy/fit.hpp:38` reads the stored value for the returned location. For two distinct observations with equal counts, write \(u\) for the displacement from the midpoint and \(d\) for the half-gap. The product of the density denominators is \((u^2-(d^2-\sigma^2))^2+4d^2\sigma^2\). For \(d>\sigma\), its only minima are the reflected pair \(u=\pm\sqrt{d^2-\sigma^2}\). Both isolated brackets must pass the accuracy check before the pair is returned as certified and non-unique. The sample \((-3,-2,2,3)\) still returns an uncertified estimate after overlapping comparisons. At \((-1,1)\), scale 1, the quartic coalescence still exhausts subdivision.
- **Bhattacharjee.** `bhatt/certificate.hpp:551` matches each retained physical-coordinate inclusion to its witness and evaluates the point likelihood there. An acceptable search result remains available. If the search fails and an enclosed finite candidate leads the comparison, the shared renderer can return it uncertified. Ordinary witnesses retain their bounds.
- **EMG.** `emg/certificate.hpp:178` retains public-coordinate boxes for validated winner roots and transports them through physical rescaling at line 370. `emg/mle.hpp:103` checks the caller's tolerance against each projected box and evaluates the point likelihood at a passing candidate. Existing successful polishing remains available. For an unresolved normal comparison, the comparison rule must select an enclosed finite point before an estimate can be returned. Both examples above remain unresolved.
- **Beta-binomial.** `bbinom/mle.hpp:631`, line 687 and line 790 record checked roots from the fixed-shape or the mean/dispersion enclosure. Each checked root carries the likelihood at its public shapes in the original count orientation. Line 859 returns a selected marked point uncertified. The ordinary finite-search accuracy gate still applies when the enclosure cannot supply the selected point. Of 26 additional near-binomial frequency tables per mode, four now carry both comparison candidates. Three keep the binary64 shape coordinates exactly. The fourth changes by about \(3.7\times10^{-13}\) relatively after refinement. [Frequency table search](2026-11-22-render-fu-data/search-bbinom.py) · [Outcomes](2026-11-22-render-fu-data/near-binomial.txt)
- **Dossiers.** `gpd/README.md:208` and line 230 now describe an unresolved comparison with candidate witnesses. The sweep distinguishes non-unique estimates, unresolved comparisons and limit outcomes in the other affected dossiers.

The enclosures use exact-arithmetic arguments conditional on computed comparisons and the families' practical floating-point margins. The replays are numerical evidence of the resulting paths and payloads.

## Verification

| Check | Machine | Result |
| --- | --- | --- |
| Snapshot comparison against stored baselines and `cc7bfa80` | Workstation, GCC 15.2 | 920 native and 920 double-only cases. Zero differing, missing or additional cases. The final rechecks replayed Cauchy in native precision and Bhattacharjee in both modes. |
| `just test-changed --no-long-double` | Workstation | 24 selected families. C++ 41/41 native and 41/41 double-only. R dev tier 7,773 passes, 0 failures, 0 warnings, 49 skips. |
| `just js-build` and `just js-test` within the targeted recipe | Workstation | WASM build passed. Selected JavaScript checks 329/329. |
| `just site-check` | Workstation | Passed for 85 family pages and their generated C++17 snippets. |

New coverage checks both Cauchy coordinates and stored likelihoods in C++, and certified flag-only rendering in R. The EMG test checks that a tight projected box earns a point mark and a wider box retains its bounds. The beta-binomial test replays the size-three comparison and checks the selected point likelihood. See `cauchy/test.cpp:292`, `bindings/r/distributions/cauchy/test.R:306`, `emg/test.cpp:546`, and `bbinom/test.cpp:647`. The edits use family-specific fields, so validation used the targeted tier. [Snapshot comparisons](2026-11-22-render-fu-data/snapshots.txt) · [Suite results](2026-11-22-render-fu-data/suite-results.txt)

## Open

**Unidentified maximizer payloads.** The earlier public stop split has been superseded. On master the attained flat cases below return certified estimates with `unique = FALSE`. They differ in whether they carry a separate maximizer payload.

| Public route | Concrete sample | Current representation |
| --- | --- | --- |
| `mlbbinom` with size 0, size 1, or size free with one shape held | \((0,0)\), \((0,1,1)\), and \((0,0)\) with alpha 2, respectively | A listed family point and a description of the free shape set or Bernoulli shape ray. At fixed positive size with all counts zero and positive shapes, the point mass remains a limit. |
| `mlbinom` and `mlpois` with singleton truncation \([2,2]\) | \((2,2)\), holding size 5 for binomial | Binomial returns probability \(1/2\). Poisson returns rate 1. Each has likelihood 1 and an empty maximizer payload. |
| `mlcmb` with size 0 | \((0,0)\) | Returns probability \(1/2\), nu 1 and likelihood 1 with an empty maximizer payload. The current size-zero branch uses `Ok`. The retained `Unidentified` status arm maps to `Unresolved` if reached. |

Proposed rule for Jonas. Whenever an admissible parameter set attains a flat maximum, return a valid canonical representative in `maximizers`, preserve the holds, and describe the full maximizing set in `outcome_message`. The corresponding branches are `bbinom/mle.hpp:893`, `binom/tbinom_mle.hpp:687`, `pois/tpois_mle.hpp:12` and `cmb/mle.hpp:285`. [Public replays](2026-11-22-render-fu-data/cases.jsonl)
