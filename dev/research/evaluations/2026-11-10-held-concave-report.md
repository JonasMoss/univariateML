# Held-nuisance cover: enlarged concave basin — TASK-172

Follow-up to [TASK-170](2026-11-10-gev-held-report.md), whose open entry F1
left 5 of 40 seeded held-scale Fréchet fits uncertified, all at n=500, and
whose G2 entry left three GEV datasets exhausting the default allowance.
Branch `claude/held-concave` from `c60d92ea`. All runs used the workstation,
one thread each through the shared slot wrapper, GCC 15.2, `-O2`.

## Diagnosis at c60d92ea

The TASK-170 report attributed F1 to weak "constrained profile/value bounds".
[A counting wrapper](2026-11-10-held-concave-data/breakdown.cpp) around the
box evaluation shows where the shared cover (`gev_held_detail::cover`) spent
its work on the seeded Fréchet samples
([output](2026-11-10-held-concave-data/breakdown-c60d92ea.txt)).

A larger allowance closes all five: samples 37, 39, 57 and 59 at
`iterlim=400` and sample 77 at `iterlim=1600`. The cover was slow, not stuck.

| Sample | Generating lambda | n | Evaluations to close | Boxes with lambda >= 4 | Boxes within 0.1 of the root | Boxes per decade of distance |
|---|---:|---:|---:|---:|---:|---:|
| 33 | 2 | 100 | 7,252 | 3.5% | 75% | 688 |
| 35 | 2 | 100 | 6,123 | 3.5% | 75% | 566 |
| 53 | 2 | 100 | 3,679 | 3.1% | 78% | 328 |
| 55 | 2 | 100 | 6,238 | 3.4% | 75% | 572 |
| 73 | 1 | 100 | 10,560 | 1.8% | 85% | 1,242 |
| 37 | 2 | 500 | 30,954 | 4.1% | 71% | 2,914 |
| 39 | 2 | 500 | 48,735 | 2.6% | 74% | 4,904 |
| 57 | 2 | 500 | 28,331 | 3.2% | 74% | 2,806 |
| 59 | 2 | 500 | 43,438 | 2.4% | 75% | 4,405 |
| 77 | 1 | 500 | 87,303 | 1.4% | 86% | 10,734 |

"Within 0.1" is the larger of the relative shape gap and the log-gap
distance from the box to the search estimate. "Per decade" is the mean box
count over the four decades of that distance from 1e-5 to 1e-2. The default
allowance is 12,800 evaluations; about 40% of evaluations are the midpoint
evaluations of the Taylor test and are not counted as boxes.

Three conclusions follow.

- The infinite-shape tail is cheap. Its cutoff is large (about 2,500 to 2,900
  at n=500 with generating shape 2, against 190 to 340 at n=100) but costs a
  few hundred boxes. The low end of the location-root enclosure is cheap too.
- Most of the work is the approach to the excluded basin. That basin started
  at relative radius 2e-6 and could only shrink. Score-sign and value tests
  weaken near a maximum, so the cover paid a roughly constant number of boxes
  for every decade of distance between 0.1 and 1e-6.
- The cost per decade is about five times larger at n=500 than at n=100.
  The cause is not identified. One guess, that the split rule's domain-relative
  aspect ratio is to blame, was tested by splitting relative to the root's
  scale instead; that needed 88,000 to 190,000 evaluations on the five cases
  and was dropped.

## Change

After the root is enclosed, the cover enlarges the excluded basin. The
basin's criterion is pointwise (a negative definite Hessian, or at a shape
edge an outward shape score with negative nuisance curvature), so it is
verified on a larger box by pieces; the enclosed root is then the unique
maximizer of the whole box. The argument is in the
[constrained-cover proof](2026-11-10-gev-held-data/proof.md), section
"Enlarged basin". The radii double until an attempt fails. An attempt checks
only the ring around the box verified so far and examines at most 400 pieces
(1/32 of the default certificate allowance, and no more at a larger
`iterlim`), charged to the shared cap. The change is in `gev/held_certificate.hpp` and applies to all three
hold scopes. Default fits, statuses, controls and goldens are unchanged.

### Choices tried

Evaluations to close the five Fréchet cases at the default allowance, for a
first version that rechecked the whole box on every doubling (`FAIL` means
the 12,800 allowance ran out):

| Pieces per attempt | Growth factor | 37 | 39 | 57 | 59 | 77 |
|---:|---:|---:|---:|---:|---:|---:|
| 100 | 2 | 9,569 | FAIL | 10,894 | 11,086 | FAIL |
| 100 | 1.41 | 8,431 | 10,944 | 9,608 | 9,105 | 7,563 |
| 200 | 2 | 9,669 | FAIL | 10,994 | 11,186 | 6,317 |
| 300 | 2 | 7,129 | 8,131 | 8,588 | 6,844 | 6,417 |
| 400 | 2 | 7,229 | 8,231 | 8,688 | 6,944 | 6,517 |
| 1000 | 2 | 7,829 | 8,831 | 9,288 | 7,544 | 7,117 |
| 3000 | 2 | 9,829 | 10,831 | 11,288 | 9,544 | 9,117 |

The verified box reaches relative radius about 0.065 for interior maxima and
0.08 to 0.16 at the shape-one edge. The piece count rises steeply just below
300, so 100 or 200 pieces stop one doubling early, and more than 400 only
adds the cost of a longer failed attempt.

Two refinements followed. The final version checks only the new ring on each
doubling instead of the whole box; at the default this changes the counts
above by fewer than 25 evaluations, because successful attempts were already
cheap. A limit of 1/32 of the allowance, growing with `iterlim`, was tried
and dropped: at `iterlim=1600` a failed attempt may then use 6,400 pieces,
and sample 33 (n=100), which closes in 2,178 evaluations at the default,
needed 6,724. The limit is therefore capped at 400 pieces.

After the change the five cases close in 7,226, 8,224, 8,683, 6,941 and
6,495 evaluations (samples 37, 39, 57, 59, 77), 51% to 68% of the default
allowance ([output](2026-11-10-held-concave-data/breakdown-after.txt)). Small
fits pay for the growth attempts: the four generating-shape-0.5 samples,
which sit on the shape-one edge, used 168 to 908 evaluations before and 848
to 1,327 after.

## Seeded study after the change

Same [source](2026-11-10-held-concave-data/study.cpp) as TASK-170 apart from
the diagnostics path, same seed `17020261110`, `tol=1e-10`, `maxit=100`:
100 datasets and 160 fits.
[Raw fits](2026-11-10-held-concave-data/study.csv),
[aggregates](2026-11-10-held-concave-data/summary.json),
[diagnostics](2026-11-10-held-concave-data/diagnostics.csv).

| Scope | Fits | Certified at c60d92ea | Certified after | Uncertified after | Stops |
|---|---:|---:|---:|---:|---:|
| GEV, location held | 60 | 33 | 35 | 25 | 0 |
| GEV, scale held | 60 | 35 | 38 | 22 | 0 |
| Fréchet, scale held | 40 | 35 | 40 | 0 | 0 |

Overall 113 of 160 certified, up from 103. Newly certified: Fréchet samples
37, 39, 57, 59 and 77 (all n=500); GEV sample 7 under both holds (n=100,
generating shape -0.9), sample 74 under both holds (n=100, shape 1) and
sample 58 with scale held (n=500, shape 0.5). No fit lost its certificate and
no status changed. The largest relative change in a returned log likelihood
is 1.7e-15. These fractions describe this fixed grid, as in TASK-170.

The per-fit times in `study.csv` were taken on a loaded workstation and are
not comparable with TASK-170's. For timing, the binary built from `c60d92ea`
and the changed one ran the whole study at the same time, one core each
([per-fit times](2026-11-10-held-concave-data/timing-side-by-side.csv)):

| Scope | CPU at c60d92ea (s) | CPU after (s) | Median fit at c60d92ea (ms) | Median fit after (ms) |
|---|---:|---:|---:|---:|
| GEV, location held | 124.7 | 78.4 | 539.6 | 212.7 |
| GEV, scale held | 115.8 | 61.6 | 380.8 | 157.6 |
| Fréchet, scale held | 12.7 | 6.1 | 16.3 | 12.2 |

The whole run took 310 s of CPU before and 183 s after. Three fits were
slower by more than 20% and 20 ms, the largest going from 41 to 101 ms; they
are small fits that pay for one failed growth attempt.

## What stays open

**F1: the work still grows with the sample size.** The five study cases now
close in the default allowance, but the cost of covering the region outside
the enlarged basin still grows with n. [Quantile samples](2026-11-10-held-concave-data/large.cpp)
`x_i=(-log((i+.5)/n))^(-1/lambda)` with lambda 1.25, 2 and 4 certify at the
default for n=500, need `iterlim=200` for n=1000 and `iterlim=400` for n=2000
([output](2026-11-10-held-concave-data/large.txt)). The cause of the growth
per decade is not identified. A cover over the shape alone, with the unique
location root enclosed on each shape interval, remains the candidate for
removing it; so does finding why more boxes survive both score tests at
larger n.

**G2 is unchanged in kind.** The local-inclusion failures at small n and on
the active support face do not reach the basin and are not affected. The three
datasets that exhausted the allowance (8 with location held, 76 and 96 under
both holds) still do
([diagnostics](2026-11-10-held-concave-data/diagnostics.csv)).

**Uncertified fits on this route are less accurate than `tol`.** At
`c60d92ea` the five uncertified Fréchet fits returned the search's estimate.
Compared with the certified root obtained at a larger `iterlim`, its shape was
off by 6e-9 to 3e-8 relative and its location by 4e-9 to 2e-8 absolute, with
`tol=1e-10`; the log likelihoods agree to about 1e-12
(`breakdown-c60d92ea.txt`, fits at `iterlim` 100 against 400 or 1600). A
certified fit replaces the search estimate with the enclosed root, so the five
now meet `tol`. Any fit that stays uncertified here, such as n=1000 at the
default, still returns a search estimate of that accuracy with status `Ok`.
This belongs with the accuracy-contract work (TASK-150.2) and is not addressed
here.

## Validation

All on the workstation at the final source of this branch, GCC 15.2, one
slot each. The workstation was shared with four Codex jobs.

| Check | Result |
|---|---|
| `ctest -R` for `distribution_frechet`, `distribution_gev`, `distribution_gev_slow`, `no_throw_tests`, `diagnostics_tests` (every target that includes `gev/held_certificate.hpp`) | 5/5 passed |
| `ctest -L dev` (the `just cpp-test` tier) | 106/106 passed |
| `just quality gev frechet` | both pass |
| `python3 dev/tools/check_links.py` | no hits in the changed or new files |
| Seeded study, `large.cpp`, `breakdown.cpp` | rerun at the final source; `study.csv` has the same status, certificate and log likelihood on all 160 fits as the run before the 400-piece cap |

Not run: `just test` and `just test-full` (the R tiers), the full C++ tier
beyond the GEV slow shard, `just cpp-test-no-long-double`, the JavaScript
build and tests, and `just site-check`. The change is confined to one C++
header and adds no binding code, but the R tests of the `certified`
attribute for held fits were not exercised.

New regressions: a strictly concave quadratic closes in at most 100
evaluations (251 before the change, 28 after) and a two-well objective is
certified from the winning well and refuted from the losing one
(`gev/test.cpp`); a deterministic Fréchet quantile sample of size 500 with
scale held is certified at the default `iterlim` (uncertified before)
(`frechet/test.cpp`). No existing test was changed.

## Reproduction

From the repository root, with `B=$(Rscript -e 'cat(system.file("include",package="BH"))')`
and `D=dev/research/evaluations/2026-11-10-held-concave-data`:

```sh
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$B" $D/study.cpp -o build/scratch/held-study
build/scratch/held-study > $D/study.csv
python3 $D/summarize.py
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$B" $D/breakdown.cpp -o build/scratch/breakdown
build/scratch/breakdown 100 > $D/breakdown-after.txt
g++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$B" $D/large.cpp -o build/scratch/large
build/scratch/large > $D/large.txt
```

`breakdown-c60d92ea.txt` is the same program built against the headers of
`c60d92ea` and run as `breakdown 1600 400 100`, so that its tallies describe
a cover that closes. `timing-side-by-side.csv` comes from running the
`c60d92ea` build of TASK-170's `study.cpp` and this build of `study.cpp` at
the same time. It was taken before the 400-piece cap was added; at
`maxit=100` the cap equals the earlier limit, so the fits are identical.
