# Loose coefficient comparison follow-ups

TASK-150.8. Worktree baseline `460a0ff2`. The fixed-size, held-inflation ZINB golden now meets its `1e-12` request in both builds. The seven comparisons with deliberately relaxed solver tolerances use those requests. The previously skipped coefficient assertions meet the reference threshold after re-solving the frozen targets.

## ZINB held inflation

The old scalar loop in `zinb_certificate.hpp:156–171` could leave the branch after a sign was ambiguous under the likelihood comparison margin. It accepted a signed neighborhood immediately if its probes passed, and otherwise allowed an existing bracket up to `1e-8 * min(lo, 1-hi)`. The golden fixture has `n = 500`, `k = 298` positive counts and sum `S = 1076`. Its old log-score margin was `1.4343e-9` and the local slope was `-30.7978`, allowing roughly `1.35e-10` relative displacement in probability. The old result was `0.3442803382861217`, with relative error `1.1402e-10` in both builds.

The updated `zinb_certificate.hpp:148–195` uses the existing margin for turning-point signs and computes the interior sign from the equivalent scaled equation

```text
F(p) = (1-pi) p^r [n r (1-p) - S p] - pi [S p - k r (1-p)].
```

Inside the branch the log-score and this equation have the same sign in exact arithmetic. The affine products use fused multiply-add, and the score has a local rounding allowance based on term magnitudes. The signed bracket must close to `min(tol, 1e-10) * lo`. Its width bounds the error of the returned midpoint because the maximizing probability is at least `lo`. An ambiguous midpoint can advance the bracket only through signed probes. If those cannot advance it, the stop retains a witness and records the diagnosed spacing and noise-over-slope estimate when available. On the golden, the local score allowance is `1.2388e-12`, the scaled slope is `-579.7497`, and the implied relative allowance is `6.21e-15`. These are practical floating-point margins.

The 70- and 100-digit reference rounds to `0.34428033824686793`. Both builds returned `0.34428033824682741`, with relative error `1.177e-13`. The four held-size masks now check that rounded reference at `1e-12`. The added `1e-20` regression requires a tolerance stop and a retained witness. The focused probe measured relative binary64 spacing of `1.612e-16`. [Implementation](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/zinb_certificate.hpp), [modifier tests](../../../univariateML-cpp/tests/modifiers/zinb.cpp), [independent roots](2026-11-21-loose-2-data/references.csv).

## Comparisons

Errors below are maxima over the specified assertion site, using the rounded reference and relative error for nonzero targets. Exact zero uses absolute error. The C++ reference threshold is `mltest::kFitReferenceTol` with zero `Approx` scale. The R threshold is componentwise `1e-10`. The two large-scale location tests also check the contract floor of `1e100`; both floor-normalized errors are `6.95e-111`. The old assertions in those two cases still check recovery of a small location in the original units. The coefficient-level table retains old and new references, estimates and errors for every measured comparison. [Measurements](2026-11-21-loose-2-data/measurements.csv).

| Site | Class | Old check | New check | Native error | Double-only error |
|---|---|---|---|---:|---:|
| `bhatt/test.cpp:112–118` | frozen | kBhattGoldenTol | kBhattGoldenTol + relative reference | 1.15e-15 | 2.94e-14 |
| `bhatt/test.cpp:273` | frozen | Approx(1e-8) | relative reference | 1.16e-14 | 3.22e-14 |
| `bhatt/test.cpp:636` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:643` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:650` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:657` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:664` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:669` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:671` | frozen | Approx(1e-10) scale 1 | relative reference | 1.78e-15 | 1.78e-15 |
| `bhatt/test.cpp:676` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:678` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:685` | recovery | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:695` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:702` | frozen | Approx(1e-10) scale 1 | relative reference | 8.85e-16 | 8.85e-16 |
| `bhatt/test.cpp:709` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:716` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:723` | frozen | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `bhatt/test.cpp:730` | recovery | Approx(1e-10) scale 1 | relative reference | 0 | 0 |
| `gb2/test.cpp:191–194` | frozen | kBurrGoldenTol | relative reference | 3.71e-16 | 3.71e-16 |
| `gb2/test.cpp:447–448` | frozen | Approx(1e-7) | relative reference | 2.44e-11 | 2.44e-11 |
| `gengamma/test.cpp:308` | analytic | Approx default | exact | 0 | 0 |
| `kumar/test.cpp:58` | frozen | Approx(1e-8) | relative reference | 5.58e-16 | 5.58e-16 |
| `sgomp/test.cpp:21` | analytic | Approx default | relative reference | 0 | 1.22e-16 |
| `sstd/test.cpp:365–366` | analytic | Approx(1e-8) | relative reference | 5.89e-12 | 5.89e-12 |
| `sstd/test.cpp:377` | frozen | Approx(1e-8) | relative reference | 7.8e-13 | 7.8e-13 |
| `st/test.cpp:178` | frozen | Approx(1e-8) | relative reference | 5.71e-11 | 5.71e-11 |
| `tests/modifiers/zinb.cpp:201` | frozen | Approx(2e-7) | relative 1e-12 + golden check | 1.18e-13 | 1.18e-13 |
| `bnbinom/test.cpp:252` | frozen | Approx(1e-8) | relative 1e-8 | 3.84e-09 | 3.84e-09 |
| `bnbinom/test.cpp:277` | analytic | Approx(1e-8) | relative 1e-8 | 3.23e-09 | 3.23e-09 |
| `expweibull/test.cpp:240` | frozen | Approx(1e-6) | relative 1e-6 | 3.98e-14 | 2.12e-10 |
| `ged/test.cpp:475` | frozen | Approx(1e-5) | relative 1e-6 | 3.47e-10 | 3.47e-10 |
| `lst/test.cpp:521` | frozen | Approx(1e-7) | contract scale floor + retained check | 3.48e-10 | 3.48e-10 |
| `pig/test.cpp:64` | frozen | Approx(1e-6) | relative 1e-9 | 1.61e-10 | 1.61e-10 |
| `std/test.cpp:326` | frozen | Approx(1e-7) | contract scale floor + retained check | 3.48e-10 | 3.48e-10 |
| `tests/testthat/test_golden_regression.R:254–265` | frozen | 2e-6 | 2e-6 + relative 1e-10 | 1.15e-15 | 2.94e-14 |
| `gb2/test.R:233` | frozen | expect_equal(1e-7) | componentwise relative 1e-10 | 1.13e-12 | 1.13e-12 |
| `gb2/test.R:235` | frozen | expect_equal(1e-7) | componentwise relative 1e-10 | 3.81e-14 | 3.81e-14 |
| `bhatt/test.R:107` | analytic | expect_equal default | exact | 0 | 0 |
| `bhatt/test.R:110` | analytic | expect_equal default | exact | 0 | 0 |
| `bhatt/test.R:239` | analytic | expect_equal default | exact | 0 | 0 |
| `bhatt/test.R:242` | analytic | expect_equal default | exact | 0 | 0 |
| `bhatt/test.R:279` | analytic | expect_equal default | exact | 0 | 0 |
| `bhatt/test.R:324` | analytic | expect_equal default | exact | 0 | 0 |
| `sgomp/test.R:90` | analytic | expect_equal default | exact | 0 | 0 |
| `sgomp/test.R:40–41` | analytic | expect_equal default | relative 1e-10 and exact eta | 0 | 1.22e-16 |

The changed reference values are listed below. GB2 uses the same targets in C++ and R, with different requests to the solver. Bhatt's recovered near-normal branches reuse the corresponding independently solved free-width roots.

| Site | Coefficient | Previous value | Independent value | Native error | Double-only error |
|---|---|---:|---:|---:|---:|
| `gb2/test.cpp:447–448` | shape2 | `0.5783436321365117` | `0.5783436321372848` | 1.43e-12 | 1.43e-12 |
| `gb2/test.cpp:447–448` | shape3 | `1.7944752190424211` | `1.794475219041364` | 2.44e-11 | 2.44e-11 |
| `gb2/test.cpp:447–448` | shape1 | `3.3085427329308605` | `3.308542732930816` | 1.15e-11 | 1.15e-11 |
| `gb2/test.cpp:447–448` | shape2 | `0.43155118384080654` | `0.4315511838408103` | 8.23e-13 | 8.23e-13 |
| `st/test.cpp:178` | alpha | `0.16456272666935215` | `0.16456272669138336` | 5.71e-11 | 5.71e-11 |
| `bnbinom/test.cpp:252` | size | `1.2350155733754016` | `1.2350155733754018` | 2.13e-10 | 2.13e-10 |
| `bnbinom/test.cpp:252` | alpha | `0.7885962837901387` | `0.7885962837901386` | 1.81e-09 | 1.81e-09 |
| `bnbinom/test.cpp:252` | beta | `22.077541746455292` | `22.077541746455296` | 3.84e-09 | 3.84e-09 |
| `expweibull/test.cpp:240` | exponent | `7502738932447.385` | `7502738932447.38` | 3.98e-14 | 2.12e-10 |
| `gb2/test.R:233` | shape2 | `0.5783436321365117` | `0.5783436321372848` | 1.45e-13 | 1.45e-13 |
| `gb2/test.R:233` | shape3 | `1.7944752190424211` | `1.794475219041364` | 1.13e-12 | 1.13e-12 |
| `gb2/test.R:235` | shape1 | `3.3085427329308605` | `3.308542732930816` | 3.81e-14 | 3.81e-14 |
| `gb2/test.R:235` | shape2 | `0.43155118384080654` | `0.4315511838408103` | 1.17e-14 | 1.17e-14 |

## Findings

- The seven prior comparison misses agree with their requested tolerances. Their reference solves and the SGED and genbeta references were replayed at 70 and 100 digits using the prior script. The replay agreed with all 15 selected stored coefficients. The new script solved 36 quantities from the exact binary64 inputs in both precisions and required agreement after rounding. The numerical roots witness the stationary branches exercised by the tests. [Prior evaluation and reference command](2026-11-21-loose-tests-report.md#reference-construction), [new reference script](2026-11-21-loose-2-data/references.py).
- **Genbeta, `tol = 1e-20`.** Both R builds emitted `univariateML_tolerance` with `attainable_tol = 2.1742455025865251e-16`. The independent root for the held-scale golden has relative nearest-double distances of approximately `2.37e-17`, `1.97e-17` and `1.05e-16` in its free coefficients. The tolerance stop is correct. The previous conditional assertions checked its kind and witness only if a stop arrived. The test now requires the typed stop and retains the witness checks.
- **SGED, `tol = 10`.** Both R builds emitted `univariateML_numerical` on the held-sd, held-power and held-skew fixture. The weighted location loss is strictly convex and has a finite optimum. The original test exercised a broad message check on error and an accuracy check on a returned mean. The test now checks the observed error class and kind, and retains the candidate comparison. [Native R observations](2026-11-21-loose-2-data/r-probe-native.csv), [double-only R observations](2026-11-21-loose-2-data/r-probe-double.csv).

## Snapshot replays

The workstation replays used the canonical `geom` and `nbinom` families, which include the ZINB path through their inflation implementations, and the stored `nbinom+zero=inflated` modifier route. In both modes all 138 canonical cases matched and 150 of 151 route cases matched. The other route case, `008660`, held size `1.5215075359537307` and inflation `0.2573876163521669`. Only its iteration count changed, from 35 to 32. The bracket now controls accuracy in public probability, so near `p = 0.852` the previous extra restriction by `1-p` is unnecessary. The returned probability, held values, likelihood and outcome fields are bit-identical. [Filtered baseline diffs](2026-11-21-loose-2-data/snapshot-diffs.txt), [stored baselines](2026-11-15-fit-snapshot-data/).

## Verification

All final runs used the workstation with GCC 15.2, R 4.6.1 and the installed BH headers. Simbox returned exit 75 for the R double-only probe. The workstation replay compiled both R packages with `just regen` and loaded their compiled libraries with `pkgload::load_all(compile = FALSE)`. The C++ and R double-only probes set `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

`/tmp/umvl-cpu/run4 just test-changed --no-long-double` exited zero. Its shared-file selection ran the unfiltered recipes and covered the standalone ZINB modifier and the R golden regression.

| Check | Result |
|---|---|
| Native C++ | 110/110 passed, 282.15 s CTest time |
| Double-only C++ | 154/154 passed, 301.66 s CTest time |
| R development tier | 36,455 passed, 29 warnings, 157 skips, zero failures |
| JavaScript/WASM build and tests | Build succeeded, 587/587 tests passed |
| Link check | Zero new broken local links, 47 inherited |

To reproduce the new references and join the saved observations from the repository root:

```sh
data=dev/research/evaluations/2026-11-21-loose-2-data
/tmp/umvl-cpu/run python3 "$data/references.py" >build/scratch/references-check.csv
cmp "$data/references.csv" build/scratch/references-check.csv
/tmp/umvl-cpu/run python3 "$data/measurements.py" >build/scratch/measurements-check.csv
cmp "$data/measurements.csv" build/scratch/measurements-check.csv
```

`probe.py` emits the standalone C++ observation source. Compile it with `-std=gnu++17 -O2 -DNDEBUG`, the repository include directory and BH, then rerun the compilation and probe with the double-only macro. `probe.R` uses the same inputs through the installed or loaded package entry points. [C++ probe source generator](2026-11-21-loose-2-data/probe.py), [R observation script](2026-11-21-loose-2-data/probe.R).

## Open

- **SGED held-sd location recovery.** For the two points $0$ and $x = 10^{100}$, the exact convex-power join is $x/(1+\xi^{2\nu/(\nu-1)})$, about `1e-304`. Adding the moment shift gives public mean `1.0048721515485897`. The centered search in `sged/certificate.hpp:17–25,59–68,167–218` loses that join displacement at its left endpoint. Its public-coordinate guard also uses `min(tol, 1e-7)` and a roundoff allowance proportional to the centered span. Reaching the public candidate needs an endpoint-anchored score or a closed two-point solve, followed by a public-coordinate accuracy check. The fitter investigation belongs outside the owned test files for this job.
