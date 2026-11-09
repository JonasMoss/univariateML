# bnbinom NB and Poisson face certificates

TASK-169, branch `codex/bnbinom-face`, base `b682b747713d6073a56f162b5135657c00806064`.
[Source fingerprints](2026-11-09-bnbinom-face-data/source.json) and
[provenance](2026-11-09-bnbinom-face-data/provenance.json).

The all-free fitter now combines uniform signed NB/Poisson exterior bounds
with a complete compactified stationary cover. On the frozen 270-sample
study it proves **24 additional face decisions: 21 NB and three Poisson**.
Certified finite fits remain at 88, certified face/limit decisions rise from
seven to 31, and uncertified results fall from 175 to 151. No search failed.

All requested workstation suites finished green, including both full tiers
and the full double-width C++ run. The requested simbox/GCC 13 check is
**open** because remote system commands fail with I/O errors; no remote
suite started. Its failed launchers remain waiting because automatic review
rejected cancellation under the user's no-interruption instruction.

A same-workstation, same-compiler replay of the exact base fitter confirms
that **all 270 model/status decisions, coefficients and log likelihoods are
identical**. The change upgrades certification and its diagnostics. Goldens,
d/p/q/r, truth files, controls and held-parameter routes are unchanged.
No commits, board edits, package installs or sub-agents were used.

## What is proved

The [derivation](2026-11-09-bnbinom-face-data/derivations.md) supplies explicit
constants and the global-cover argument. With `s=alpha+beta`, `p=alpha/s`,
`q=1-p`, and `F2=sum x_i(x_i-1)`, the exact gamma identity gives

```
ell_BNB(r,sp,sq) - ell_NB(r,p) = C(r,p)/s + E,
C(r,p) = [n r(r-1) q/p + F2 p/q - 2r sum x_i]/2,
|E| <= R(r,p)/s^2,
R(r,p) = sum_i [H(r)/p^2 + H(x_i)/q^2 + H(r+x_i)],
H(u) = u^3/6 + u^2/4 + u/12.
```

This remainder holds for every positive shape, and is uniform on bounded
size/probability rectangles. In the alpha coordinate, `c1=p C` and the
remainder is bounded by `p^2 R/alpha^2`. Near a finite NB maximizer with
strictly negative `c1`, continuity supplies a rectangle with a negative
coefficient upper bound. Outside it, the certified NB profile deficit
beats the absolute remainder. Small-shape, extreme-probability and
Poisson-corner exclusions make the relevant region effective and compact.
Taking the maximum of the rectangle cutoffs supplies a uniform finite
`A`; `alpha>=A` suffices for every remaining size/beta pair. Exact
size/beta symmetry supplies the swapped labeling.

For the Poisson corner, let `lambda=r beta/alpha`, `h=1/r+1/beta`. For
`h<=h0`, the one-sided uniform bound is

```
ell_BNB <= ell_Pois(lambda) + B(lambda) h + R_P(lambda,h0) h^2,
B(lambda) = n [(lambda-mean)^2 + variance-mean]/2,
R_P(lambda,h0) = lambda^2/2 sum_i x_i [1+(x_i-1)_+ h0/4]^2.
```

Its coefficient is negative near the fitted mean for strictly underdispersed
samples. Away from that neighborhood the Poisson profile deficit dominates.
A bounded lambda cover supplies a finite cutoff on both large shapes; the
remaining bounded-size NB face is strictly below Poisson and is covered
with the NB remainder.

A negative first-order coefficient alone does not establish a global face
win. The implementation covers `(r, logit(p), 1/s)` after ordering
`r<=beta`, including the NB boundary `1/s=0`. Signed bounds handle that
boundary. Original-coordinate likelihood, score and Hessian bounds exclude
every remaining possible stationary maximum. A finite point beating or
attaining the face supremum would supply an interior stationary maximum
and survive the exclusions. Only an empty cover certifies nonattainment
and the winning face.

These are exact-arithmetic proofs conditional on the computed strict
comparisons, with stated practical floating-point margins. New signed
arithmetic uses `4096 epsilon` absolute-term or relative margins; inherited
likelihood, score, Hessian, root-height and matrix margins remain active.
This is not outward-rounded interval arithmetic.

## Implementation and fallback

The private [signed face header](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/signed_face_certificate.hpp)
is called from [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp)
for an all-free legacy face candidate. Successful proofs use the existing
`certified` flag, retain numerical witnesses, and provide a face-height upper
bound. The existing R/JS bindings carry the certification labels.

Each signed cover shares `128 min(iterlim,100000)` evaluations across its
exterior cutoffs and compact cover. Exhaustion, an unresolved sign or
arithmetic range loss returns **no proof**, preserving the existing
uncertified search result. It introduces no certificate-budget or numerical
refusal. Actual search failures and pre-existing proved comparison
priorities retain their handling. Zero first-order signs give no new signed
proof; independent existing face theorems remain active.

The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md)
records these bounds and retains `certified: 'partial'` and `holds: []`.
C++ cases were appended; existing NB limit assertions remain. R and JS
certification-label expectations were updated, and reduced-budget fallback
checks were added.

## Seeded study

The [replay](2026-11-09-bnbinom-face-data/coverage.cpp) reads the exact archived
sample vectors from TASK-116.54, preserving its 216 BNB, 36 NB and 18 Poisson
samples, IDs and seeds. Fits use the unchanged defaults, `tol=1e-8` and
`iterlim=600`. Generation/loading is outside fit timing.
[Coverage results](2026-11-09-bnbinom-face-data/coverage.json),
[CSV](2026-11-09-bnbinom-face-data/coverage.csv), and
[baseline comparison](2026-11-09-bnbinom-face-data/comparison.json).

| Outcome | Before | After | After fraction |
|---|---:|---:|---:|
| Certified finite | 88 | 88 | 32.6% |
| Certified face/limit | 7 | 31 | 11.5% |
| Uncertified legacy | 175 | 151 | 55.9% |
| Search failure | 0 | 0 | 0% |

The 31 certified limit decisions comprise 21 NB, nine Poisson and one
pre-existing point-mass limit. This matches the original study's category
convention. No existing certificate was lost.

| Generating family | Samples | Certified finite | Certified face/limit | Uncertified |
|---|---:|---:|---:|---:|
| BNB | 216 | 88 | 24 | 104 |
| NB | 36 | 0 | 1 | 35 |
| Poisson | 18 | 0 | 6 | 12 |

Promotions comprise 20 BNB-generated NB faces, one NB-generated NB face,
one BNB-generated Poisson face and two Poisson-generated Poisson faces.
The default allowance therefore still leaves most NB-generated samples
uncertified; the added theorem is not a claim of complete default coverage.

The [same-compiler control](2026-11-09-bnbinom-face-data/baseline-control.json)
uses the [exact base header](2026-11-09-bnbinom-face-data/baseline-mle.hpp),
GCC 15.2.0, identical sample vectors and compilation flags. Every numerical
output column matches exactly. The historical GCC 13/simbox comparison has
relative coefficient differences up to `1.54e-6` and absolute log-likelihood
differences up to `1.52e-8`; those differences also occur with the unchanged
base fitter on this workstation.

| Workstation, one core | Mean fit | Median fit | Maximum fit | Total fit time |
|---|---:|---:|---:|---:|
| Base fitter | 4.919 s | 1.972 s | 148.227 s | 1328.085 s |
| Signed face fitter | 6.776 s | 4.038 s | 124.165 s | 1829.531 s |

The added comparison raises total fit time by about 38% in these single
passes. Shared workstation load differed between runs; these are descriptive
wall-clock timings, not a repeated performance experiment. The historical
simbox total was 518.511 s and is not a same-machine timing control.

## Independent checks and larger budgets

The [90-digit oracle](2026-11-09-bnbinom-face-data/high-precision.py) checks
56 general gamma-remainder cases, 100 NB remainder cases and 48 Poisson
upper-bound cases, including reflected labels. It also checks four Poisson
first-order limits. All passed. These finite grids corroborate the formulas;
the uniform proofs above establish the global bounds.
[Oracle output](2026-11-09-bnbinom-face-data/high-precision.json).

For 20 zeros and 20 fives, independent NB score solving gives
`r=0.4937679732673033775`, `p=0.1649319445182054901` and
`c1=-5.8034538667565849367`. Direct differentiation and the sample-moment
formula agree beyond 80 decimal places.

The [cover probes](2026-11-09-bnbinom-face-data/probe.json) certify
`c(0,0,5,5)` in 19,986 evaluations and `c(0,2,2,2)` in 18,400 at the default
allowance. The latter fails the older supporting-mixture test:
`D(0)=exp(1.5)>4`. The sufficient common concentration cutoffs are
`7.56e22` and `9.01e15`; these are derived tail bounds, not search caps.
The adaptive cover uses local exclusions rather than a grid up to those
conservative common cutoffs.

For `0:5`, [independent solving](2026-11-09-bnbinom-face-data/open-example.json)
gives `c1=-1.9779550299320218`. The cover exhausts the 600 and 6000
allowances. At `iterlim=100000`, the [larger-budget probe](2026-11-09-bnbinom-face-data/large-budget.json)
closes after 8,950,106 evaluations in 212.0 s. This is a standalone cover
probe, not a timing claim for the entire public fit at that allowance.

## Validation

All following runs used the workstation. The source fingerprints matched
after the runs. [Validation ledger](2026-11-09-bnbinom-face-data/validation.json)
and [compact logs](2026-11-09-bnbinom-face-data/validation.txt).

| Command/check | Result |
|---|---|
| Targeted C++, native and double-width | 51 cases / 207,086 assertions per mode, green |
| `just cpp-test` | 106/106 shards |
| `UMVL_TEST_TIER=full just cpp-test` | 129/129 shards |
| `just cpp-test-no-long-double` | 129/129 shards |
| `just test` | FAIL 0, WARN 0, SKIP 154, PASS 22,450 |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 45,285 |
| `just js-build` and `just js-test` | Green; 582 tests passed |
| `just site-check` | Green |
| 270-sample replay and base control | Finished; no status changes or lost certificates |

Reproduction scripts are kept beside the evidence: `targeted.sh`,
`coverage.sh`, `baseline.sh`, `probe.sh`, `large-budget.sh`,
`final-workstation.sh`, `final-simbox.sh`, `js.sh` and `site.sh`.
Local builds and computations used the supplied slot wrappers, one BLAS/OpenMP
thread per worker and their inherited nice priority. Local waits were allowed
to finish in the foreground.

The [changed-Markdown link audit](2026-11-09-bnbinom-face-data/links.json)
passes. The repository-wide checker reports 12 findings, all in files
byte-identical to the base commit: a missing pre-existing PDF, inline
code/math parsed as links, and two site-route references.
[Unfiltered audit output](2026-11-09-bnbinom-face-data/global-links.txt).
Those unrelated files and the shared checker were left unchanged.

## Open

1. **Default-budget coverage and cost.** The study retains 64 uncertified NB
   faces, ten uncertified Poisson faces and 77 pre-existing uncertified finite
   results. Certifying more face decisions requires completing the remaining
   global covers or resolving their signed comparisons within the allowance.
   The `0:5` experiment identifies a concrete budget-limited case whose
   larger cover succeeds. A tighter stationary cover in the compactified
   coordinates is the main practical requirement; the signed exterior
   theorem alone does not remove this cost.
2. **GCC 13/simbox validation.** The required second-machine double-width run
   could not start. SSH connects, but `/usr/bin/date` and `/usr/bin/stat`
   return `Input/output error`; the reservation helper keeps retrying.
   [Health evidence](2026-11-09-bnbinom-face-data/simbox-health.txt).
   Missing evidence is a completed GCC 13 run of `final-simbox.sh` once its
   filesystem works. There is no claim that this run passed. Automatic
   approval review rejected cancelling the two failed launcher waits because
   the user prohibited interrupting waiting commands; explicit approval was
   requested and no signal was sent.

3. **Artifact-pane registration.** The report was created at the requested
   the viewer's registered readable roots. [Delivery evidence](2026-11-09-bnbinom-face-data/viewer.txt).
   Display requires registering this worktree with the viewer. No file was
   copied into another checkout, no extra agent was opened, and no viewer
   configuration was changed.
