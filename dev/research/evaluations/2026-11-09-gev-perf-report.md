# Faster certified GEV and Fréchet all-free fits — TASK-167

Numerical changes and available final validation completed 2026-11-09T23:07:48+00:00, in
`codex/gev-perf`, based on `b1aa1929aa45`. Changes remain uncommitted.

Both workstation GEV timing targets are met: the frozen n=500 golden takes
**0.131 s** against the 0.3 s target, and the retained n=10000 sample takes
**3.241 s** against the 5 s target. Fréchet takes **0.081 s** and **2.091 s**.
The certificate remains the global all-free cover. Both goldens satisfy their
frozen 1e-10 relative coefficient and likelihood tolerances. All 189 seeded
GEV samples remain certified `Ok`, with identical restriction flags.

Every completed final suite passed. One external validation item remains open:
the **GCC 13 no-long-double suite on simbox** could not start after that host
began returning filesystem I/O errors and then became unreachable. The full
GCC 13 C++ suite passed before the fault; the workstation GCC 15
no-long-double suite passed 129/129. The precise missing check and recovery
command are recorded below.

The change is confined to the common
[GEV joint certificate](../../../univariateML-cpp/include/cppunivariate/distributions/gev/joint_certificate.hpp),
the GEV dossier's mathematical justification, an appended C++ test, and this
evidence bundle. Fréchet uses this kernel through its existing all-free
transport. Public fitter signatures, defaults, status enums, stop routing,
certification flags, work-cap constants and held-subset scope are unchanged.
The [source manifest](2026-11-09-gev-perf-data/source-state.json) records hashes, front-matter
preservation, append-only testing and unchanged fixtures.

The certified baseline below is the already certified implementation at the
base commit, preserved in [baseline-joint.hpp.gz](2026-11-09-gev-perf-data/baseline-joint.hpp.gz). Both variants use
GCC 15.2, C++17 and `-O2` on the workstation, one thread and nice level 10,
through the prescribed CPU slots. Golden times average three complete fits;
large-sample times use one complete fit. These are wall times on a shared
workstation, with ordinary load variability. Profiling timers are excluded
from this timing table.

| Sample | Certified baseline, s | Optimized, s | Speedup | Ledger evaluations before → after |
|---|---:|---:|---:|---:|
| GEV, n=500, golden | 2.111 | 0.131 | 16.1× | 515 → 511 |
| Fréchet, n=500, golden | 1.032 | 0.081 | 12.7× | 394 → 390 |
| GEV, n=10000, seed 11651 | 25.869 | 3.241 | 8.0× | 701 → 673 |
| Fréchet, n=10000, seed 11651 | 8.569 | 2.091 | 4.1× | 452 → 448 |

All four default fits returned `Ok`, without budget or comparison-tie stops.
The n=10000 samples are exactly the previous report's seed-11651 construction:
generating GEV shape 0.3 and Fréchet shape 1.5. The
[benchmark recipe](2026-11-09-gev-perf-data/benchmark.sh) reconstructs isolated header variants
and regenerates the same inputs. Raw native results are
[baseline.jsonl](2026-11-09-gev-perf-data/baseline.jsonl) and [final.jsonl](2026-11-09-gev-perf-data/final.jsonl); aggregate checks are in
[summary.json](2026-11-09-gev-perf-data/summary.json).

The main saving is per-evaluation work. Inner sign brackets and uniform
boundary-score checks now compute only nuisance score and curvature. They
use monotone corner bounds for the value integrand, avoiding shape and mixed
jets during these checks. Full jets remain in the outer exclusions,
contractions and stationary-point isolation.

For the small-radius jets, the integral representation proves that `L`,
`−L_ξ` and `L_ξξ` increase in the nonnegative pivot ratio and decrease in
shape throughout feasible rectangles. Two scalar corners therefore enclose
all three quantities, including the zero-shape seam. Their convergent series
retain the existing explicit remainders and 4*epsilon stopping threshold.
A power recurrence replaces repeated `pow` calls. This also preserves
variable dependencies and modestly reduces the surviving cover.

Interval products select the extremizing endpoints from operand signs, and
scalar multiplication uses its known sign. The two centered Taylor tests
reuse their identical midpoint jet. Sorting and the endpoint-root cache were
already shared by the caller, so those are retained. Point-profile proposals
and their feasible lower witnesses retain their algorithms.

The original shape-series work-cap check is retained in the nuisance-only
path. Without it, some small-iterlim golden fits reach a later numerical stop
instead of the previous iteration stop. After retaining the check, all
**28** paired control cases match status, restriction, budget, comparison-tie
and limit-model flags: both goldens, iteration limits 1, 2, 5, 10, 20, 50 and
100, at tolerances 1e-10 and 1e-16. See [controls.cpp](2026-11-09-gev-perf-data/controls.cpp),
[controls.sh](2026-11-09-gev-perf-data/controls.sh), [baseline-controls.txt](2026-11-09-gev-perf-data/baseline-controls.txt) and
[final-controls.txt](2026-11-09-gev-perf-data/final-controls.txt).

The [corner-bound proof](2026-11-09-gev-perf-data/corner-proof.md) supplies the new
exact-arithmetic argument and remainder bounds. The
[existing joint-cover proof](2026-11-08-gev-allfree-data/joint-proof.md)
supplies inner uniqueness, support coverage, the reverse-exponential face,
stationary existence, interval-Newton inclusion and global candidate comparison.
The package guarantee remains exact arithmetic conditional on computed
comparisons with stated practical floating-point margins: 128*epsilon in
pivot jets, 256*epsilon in mapped coefficients, and 4096*epsilon in outer
comparisons. No comparison margin or coefficient stopping target was relaxed.

Profiling uses RAII timers and counters injected into **copied headers under
build/scratch**, by [profile.py](2026-11-09-gev-perf-data/profile.py); production headers contain no
instrumentation. Each cell below is **calls / inclusive seconds**. Nested
scopes overlap, and the per-observation series timers add overhead. Their
absolute wall times also reflect changing shared-machine load. Use the native
table for speedups and these scopes to locate cost. Instrumented/native status,
coefficient and ledger agreement passed for all eight fits; see
[profile-summary.py](2026-11-09-gev-perf-data/profile-summary.py) and [profile-summary.json](2026-11-09-gev-perf-data/profile-summary.json).

GEV profiling:

| Scope | n=500 before | n=500 after | n=10000 before | n=10000 after |
|---|---:|---:|---:|---:|
| Entire joint certificate | 1 / 1.286220 | 1 / 0.294628 | 1 / 32.070800 | 1 / 5.870340 |
| Outer slab | 251 / 1.263130 | 249 / 0.286648 | 347 / 31.748000 | 333 / 5.684700 |
| Inner region enclosure | 242 / 0.963234 | 240 / 0.202328 | 331 / 20.348200 | 317 / 4.206790 |
| Endpoint-gap root | 248 / 0.553623 | 246 / 0.145468 | 337 / 18.586500 | 323 / 3.719150 |
| Initial opposite-sign bracket | 248 / 0.179805 | 246 / 0.027715 | 337 / 4.711040 | 323 / 0.650731 |
| Root refinement / inner Newton | 447 / 0.339438 | 445 / 0.040630 | 826 / 12.653700 | 812 / 1.016720 |
| Uniform region Newton | 241 / 0.083741 | 240 / 0.040453 | 325 / 2.022230 | 311 / 0.628398 |
| Full pivot value/score/curvature jet | 2,807 / 1.244900 | 745 / 0.107753 | 3,415 / 30.775300 | 963 / 1.690670 |
| Nuisance-only jet | 0 / 0.000000 | 1,772 / 0.102954 | 0 / 0.000000 | 2,214 / 1.987500 |
| Small-radius per-observation series jet | 659,202 / 0.938088 | 82,369 / 0.056542 | 12,602,540 / 22.330400 | 1,278,755 / 0.462838 |
| Reverse-exponential face envelope | 1 / 0.015697 | 1 / 0.002232 | 1 / 0.258550 | 1 / 0.048278 |
| Negative-shape value envelope | 36 / 0.152796 | 36 / 0.016627 | 48 / 7.631390 | 48 / 0.412024 |
| Joint coefficient-isolation step | 5 / 0.001760 | 5 / 0.001140 | 6 / 0.029696 | 6 / 0.018807 |
| Public precision gate | 6 / 0.000317 | 6 / 0.000287 | 7 / 0.004458 | 7 / 0.004442 |
| Public coefficient enclosure | 7 / 0.000364 | 7 / 0.000336 | 8 / 0.005099 | 8 / 0.005077 |
| Pivot coefficient evaluation | 8 / 0.000432 | 8 / 0.000373 | 9 / 0.005697 | 9 / 0.005716 |
| Point-profile proposal | 260 / 0.039490 | 258 / 0.081777 | 349 / 1.266170 | 335 / 2.165720 |

Fréchet profiling:

| Scope | n=500 before | n=500 after | n=10000 before | n=10000 after |
|---|---:|---:|---:|---:|
| Entire joint certificate | 1 / 0.488340 | 1 / 0.217796 | 1 / 10.311700 | 1 / 3.296350 |
| Outer slab | 189 / 0.484875 | 187 / 0.212541 | 217 / 10.240300 | 215 / 3.246080 |
| Inner region enclosure | 189 / 0.394394 | 187 / 0.159785 | 217 / 8.460360 | 215 / 2.601490 |
| Endpoint-gap root | 193 / 0.219835 | 191 / 0.119947 | 221 / 4.548850 | 219 / 2.139120 |
| Initial opposite-sign bracket | 193 / 0.095808 | 191 / 0.023820 | 221 / 1.898530 | 219 / 0.394364 |
| Root refinement / inner Newton | 193 / 0.095616 | 191 / 0.023556 | 221 / 1.901260 | 219 / 0.393117 |
| Uniform region Newton | 202 / 0.049704 | 200 / 0.029462 | 218 / 0.932914 | 217 / 0.350898 |
| Full pivot value/score/curvature jet | 1,634 / 0.457789 | 620 / 0.082099 | 2,012 / 9.519720 | 684 / 0.992893 |
| Nuisance-only jet | 0 / 0.000000 | 924 / 0.058591 | 0 / 0.000000 | 1,050 / 0.908858 |
| Small-radius per-observation series jet | 174,771 / 0.269189 | 39,182 / 0.035230 | 4,630,710 / 5.185150 | 761,063 / 0.214220 |
| Reverse-exponential face envelope | 0 / 0.000000 | 0 / 0.000000 | 0 / 0.000000 | 0 / 0.000000 |
| Negative-shape value envelope | 0 / 0.000000 | 0 / 0.000000 | 0 / 0.000000 | 0 / 0.000000 |
| Joint coefficient-isolation step | 5 / 0.001332 | 5 / 0.000964 | 6 / 0.027215 | 6 / 0.018613 |
| Public precision gate | 7 / 0.000253 | 7 / 0.000318 | 8 / 0.004827 | 8 / 0.004185 |
| Public coefficient enclosure | 8 / 0.000288 | 8 / 0.000354 | 9 / 0.005423 | 9 / 0.004796 |
| Pivot coefficient evaluation | 10 / 0.000358 | 10 / 0.000408 | 11 / 0.006700 | 11 / 0.005304 |
| Point-profile proposal | 204 / 0.029812 | 202 / 0.076050 | 232 / 0.782250 | 230 / 1.386040 |

The reflected-gap derivative kernel `gap_evaluate` has zero calls on these four
positive-shape workloads: the negative value envelope excludes those slabs
before gap derivatives are needed. Negative-chart derivative and coefficient
bounds are exercised by the independent checker and the seeded negative-shape
fits. The raw scope logs retain every depth count:
[baseline-profile.txt](2026-11-09-gev-perf-data/baseline-profile.txt) and [final-profile.txt](2026-11-09-gev-perf-data/final-profile.txt).

Outer counts and maximum depth, measured from each initial chart slab:

| Workload | Slabs before → after | Maximum subdivision depth before → after |
|---|---:|---:|
| GEV, n=500 | 251 → 249 | 11 → 11 |
| Fréchet, n=500 | 189 → 187 | 11 → 11 |
| GEV, n=10000 | 347 → 333 | 12 → 12 |
| Fréchet, n=10000 | 217 → 215 | 12 → 12 |

Thus GEV n=10000 keeps a similar-depth cover while full pivot-jet calls fall
from 3,415 to 963 and small-radius jet entries from 12,602,540 to 1,278,755.
Remaining large-fit cost is dominated by point-profile proposals, nuisance
sign/refinement evaluations and full jets. These scan all observations;
coefficient isolation and the attained-face envelope are comparatively small.
The ledger-averaged native GEV cost falls from 36.9 to 4.82 ms per charged
unit. This average includes several inner kernel calls per unit.
No sample-independent five-second bound is claimed. The tight enclosures and
full-data scans still determine difficult-sample runtime; the slowest retained
seeded n=500 fit takes 0.716 s.

The 189-sample grid uses seed 20261029, generating shapes −0.9, −0.6, −0.3,
0, 0.1, 0.3, 0.7, 1 and 2, seven sample sizes, and three replicates per pair,
with the previous report's min/range normalization. Generating shape 2 retains
the fitter's existing restricted branch. Every before/after pair is certified
`Ok`, with identical restriction and stop flags. Maximum literal relative
coefficient drift across all 567 coefficients is
**4.33e-14**, below 1e-10. See
[coefficient-drift.json](2026-11-09-gev-perf-data/coefficient-drift.json). Frozen golden and independent-reference
comparisons also pass.

| n | Cases | Total seconds before → after | Total ledger units before → after | Slowest optimized fit, s |
|---|---:|---:|---:|---:|
| 3 | 27 | 0.101 → 0.013 | 4,712 → 4,242 | 0.001158 |
| 4 | 27 | 0.225 → 0.026 | 6,771 → 6,149 | 0.004116 |
| 5 | 27 | 0.261 → 0.031 | 7,060 → 6,437 | 0.003151 |
| 10 | 27 | 0.630 → 0.080 | 7,540 → 6,871 | 0.012091 |
| 30 | 27 | 2.278 → 0.265 | 9,995 → 9,159 | 0.029329 |
| 100 | 27 | 8.062 → 1.150 | 11,228 → 10,384 | 0.117809 |
| 500 | 27 | 64.994 → 7.479 | 15,144 → 14,268 | 0.716330 |

Grid totals are **76.552 → 9.044 s**, **62,450 → 57,510** ledger units,
and maximum per-fit work **1,453 → 1,401**. The default budget remains 12,800.
Per-sample timings, coefficients, likelihoods, status and restriction flags are
in [baseline-seeded.txt](2026-11-09-gev-perf-data/baseline-seeded.txt) and [final-seeded.txt](2026-11-09-gev-perf-data/final-seeded.txt).
[summarize.py](2026-11-09-gev-perf-data/summarize.py) and [compare-coefficients.py](2026-11-09-gev-perf-data/compare-coefficients.py) reproduce these checks.

Independent numerical evidence passed at two precisions. The existing
50-digit checker verifies 54 interval-derivative comparisons, nine independently
bracketed inner roots, 54 public-coefficient comparisons, three face bounds,
both golden stationary references and the negative-shape five-point reference.
The new 70-digit checker verifies **1,728** value and derivative inclusions over
24 rectangles, including the zero-shape seam, 1e-18 shapes, tiny observations
and the series switch at absolute radius 0.25. These are numerical checks of
computed enclosures; the integral and cover arguments provide the proof.
Reproduction and outputs: [independent.sh](2026-11-09-gev-perf-data/independent.sh), [joint-independent.json](2026-11-09-gev-perf-data/joint-independent.json),
[corner-probe.cpp](2026-11-09-gev-perf-data/corner-probe.cpp), [corner-check.py](2026-11-09-gev-perf-data/corner-check.py) and
[corner-independent.json](2026-11-09-gev-perf-data/corner-independent.json). Appended C++ tests independently accumulate
nuisance score and curvature in long double. The targeted GEV/Fréchet run passed
66 cases and 48,085 assertions ([targeted.log.gz](2026-11-09-gev-perf-data/targeted.log.gz)).

Final validation results:

| Command | Machine | Result |
|---|---|---|
| `just cpp-test` | workstation, GCC 15 | 106/106; 76.08 s test time |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13 | 129/129; 228.99 s test time |
| `just cpp-test-no-long-double` | workstation, GCC 15, Release | 129/129; 258.34 s test time |
| `just test` | workstation fallback | 22,283 passes, 154 expected skips, zero failures/warnings; 171.894 s elapsed |
| `just test-full` | workstation fallback | 45,118 passes, zero skips/failures/warnings; 434.165 s elapsed |
| `just js-build` | workstation, EMCC_CORES=1 | green |
| `just js-test` | workstation | 569/569; 231.741 s |
| `just site-check` | workstation | 99 pages and C++17 snippets; MathML, links and browser checks green |
| `just cpp-test-no-long-double` | simbox, GCC 13 | **Open: host filesystem/network unavailable; suite never started** |

Each successful command has a retained `*.log.gz` file in the data directory.
[suites.tsv](2026-11-09-gev-perf-data/suites.tsv) records machine and total wall time including builds and
CPU-queue waits; those totals are not fit timings. Normal CPU queues were
waited out in the foreground. Runs used the prescribed slots, nice level 10,
and single-thread BLAS/OpenMP. The
[initial suite recipe](2026-11-09-gev-perf-data/final-suites.sh) used simbox where requested.
After its filesystem fault, [remaining-suites.sh](2026-11-09-gev-perf-data/remaining-suites.sh) ran the R suites
locally and completed JS/site validation. The unrelated site-generated
screenshot was restored byte-for-byte. No command remains running.

**Open validation item: GCC 13 no-long-double on simbox.** The host accepted
SSH but failed scratch-directory creation with `Input/output error`. A read-only
probe showed two free remote slots and no active compute jobs, while its
acquisition helper kept retrying. Only this job's sleeping, noncomputing helper
was ended; no remote locks or other jobs were changed. Its original driver
exited 143 before starting a test. The already successful suites were preserved.
Subsequent probes continued to fail and finally timed out on SSH. Evidence is
[simbox-acquisition.json](2026-11-09-gev-perf-data/simbox-acquisition.json), [remote-health-20261109-184211.json](2026-11-09-gev-perf-data/remote-health-20261109-184211.json),
[final availability probe](2026-11-09-gev-perf-data/remote-health-20261109-225900.json) and the intervening timestamped
health JSON files. Missing evidence is specifically a green GCC 13
no-long-double run of the final numerical source. After filesystem recovery,
the required check is:

```sh
/tmp/umvl-cpu/simbox-run env CMAKE_BUILD_PARALLEL_LEVEL=4 CTEST_PARALLEL_LEVEL=4 just cpp-test-no-long-double
```

No mathematical or implementation item remains open within the requested
all-free performance scope. The prior general held-nuisance G2/F1 certificate
scope is retained. Goldens, d/p/q/r, truth sources/tables, shared core, special
functions, bindings, generators, NEWS, STATUS, KNOWN_ISSUES and roadmap files
are unchanged. No dependency was installed, no sub-agent was used, and no
board edit, Git write command, commit or publication was performed.


worktree is outside its configured readable roots. Opening it in the pane requires
registering this worktree as a readable root in Nook. The completed Markdown report
and all evidence remain in this worktree; no app configuration or other checkout
was changed.
