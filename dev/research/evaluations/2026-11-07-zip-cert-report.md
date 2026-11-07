# ZIP certificates — TASK-116.21

Completed on 2026-11-07 in `codex/zip-cert`, based on `017b9f8ec3bed7610ccd41c62738bce1ea987316`. No commits, board edits, pushes or dependency installs were made.

## Result and scope

ZIP now certifies every supported free/held subset in the package's sense: a global maximizer in exact arithmetic conditional on the computed comparisons, with a practical floating-point margin, or a structured limit, tie, budget or numerical stop. The dossier's `fit.certified` is `'yes'`; its note and existence discussion describe the conditional certificate and nonuniqueness.

| Free coefficients | Result |
|---|---|
| `lambda, pi` | Existing clipped profile/Lambert-W fit retained unchanged. With positive observations its maximizing pair is unique; all-zero data retain the existing representative `(1,1)`. |
| `pi`, rate held positive | Exact clipped profile, now evaluated as `max(0, 1 - (1-z/n)/(-expm1(-lambda)))`. All-zero data give inflation one even at a tiny held rate. |
| `lambda`, inflation held at zero | Poisson mean for positive count sum; all-zero data stop on the zero-rate Poisson face. |
| `lambda`, inflation held strictly between zero and one | With positive observations and zeros, isolate and compare every possible rate maximum. Without zeros the positive-count mean is the unique maximum. All-zero data stop on the zero-rate face. |
| `lambda`, inflation held at one | Only all-zero data are compatible; retain rate one as an unidentified global representative. |
| None | Stable likelihood evaluation of compatible held coefficients; nonfinite arithmetic stops. |

The default free-fit branch and every golden fixture are unchanged. The new optional control is `iterlim = 200` in R and the common binding, and appended `maxit = 200` in C++. R and JS use the same binding-neutral routing and expose the same structured conditions. JS types were regenerated for this control.

## Certificate and proof

Let `z` be the zero count, `k=n-z>0`, `S=sum(x)>0`, and hold `0<pi<1`. Writing `C=sum(log(x_i!))`, the full log likelihood at rate `t` is

\[
\ell(t)=z\log\{\pi+(1-\pi)e^{-t}\}+k\log(1-\pi)-kt+S\log t-C.
\]

Its score is `S/t-k-z/(1+pi*exp(t)/(1-pi))`. It is positive below `u=S/n` and negative above `v=S/k`; both likelihood tails tend to minus infinity. All maxima therefore occur at stationary rates in `(u,v)`.

Rewriting the stationary equation gives

\[
h(t)=-t+\log(nt-S)-\log(S-kt)-\log\{\pi/(1-\pi)\}=0,
\qquad
h'(t)=-1+\frac1{t-u}+\frac1{v-t}.
\]

Put `d=v-u`. For `d<4`, `h` is strictly increasing. For `d>4`, its only turning points are `(u+v +/- sqrt(d*(d-4)))/2`. Thus there are at most three stationary rates and at most two maxima. The likelihood score has the opposite sign to `h`, so only increasing branches can contain maxima. Their endpoint signs identify every such branch; bisection isolates its unique root. Tangent cases are explicitly refused numerically. This proves the finite bracket and exhaustive peak enumeration, conditional on the computed comparisons. No starting rate or mesh is needed.

For a root enclosure `[a,b]`, `|ell'(t)| <= n+S/a`. The midpoint likelihood plus `(b-a)*(n+S/a)+E` is consequently an upper bound on that peak, and its evaluated likelihood minus `E` is a lower bound. ZIP uses

\[
E=4096\epsilon\{1+n+|\ell(t)|+kt+S|\log t|\}.
\]

Midpoint likelihoods use stable log-sum-exp zero probabilities, the existing stable Poisson log mass, and compensated summation. A returned rate enclosure has relative width at most `1e-13`. Its lower likelihood bound must exceed every other peak's upper bound. Overlap gives a `tie` retaining both peaks and their bounds. Per-root bisection work is `min(iterlim,100000)`; exhaustion gives a `budget` with finite witnesses and a global upper bound, and advice to raise `iterlim`.

The computed topology is refused when `abs(d-4) <= 4096*epsilon*(1+n+d)`, or a turning-point `h` value is within `4096*epsilon*(1+n+v+abs(log(pi)))` of zero. These `numerical` stops retain a feasible finite witness and an unclosed infinite upper bound. These margins are practical floating-point safeguards, not outward-rounded error bounds. The exact-arithmetic argument is proved; floating-point performance is tested numerically rather than asserted as an interval-arithmetic theorem.

The canonical derivation and numerical contract are in [ZIP's dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zip/README.md). The old log-rate mesh and local maximizations were replaced because they did not establish exhaustive stationary-point coverage.

### Reuse and preserved behavior

[ZIP's new helper](../../../univariateML-cpp/include/cppunivariate/distributions/zip/certificate.hpp) extracts the held-inflation ZIP-face machinery previously local to ZINB, using the equivalent normalized quadratic. Its lower turning-point offset is evaluated without subtracting nearly equal quantities. The requested cross-family edit in [ZINB](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/global_certificate.hpp) delegates that branch to the helper, retaining every peak even when their likelihood bounds overlap. ZINB retains its existing arithmetic-padding policy and subsequent face comparisons. No global core/shared header was edited.

Two related ZIP held-fit arithmetic defects were corrected: the clipped inflation profile lost its all-zero maximum at held rate `1e-20`, and direct exponentiation could underflow a held Poisson-member zero probability at rate 1000. Dedicated cases cover both. The settled d/p/q/r already evaluate these log probabilities stably; no d/p/q/r, special-function, truth, code-generation-script or golden changes are needed or made.

## Nonuniqueness counterexample

For 99 zeros and one count of ten, two global maxima tie at an inflation value approximately

- `pi_* = 0.72549710522911909850293147041482049`;
- rates `0.52230412466233525659` and `9.98272792836634535365`;
- total log likelihood `-35.13856131179308265101` at each.

Existence of an exact tied value follows by continuity: throughout inflation `[0.7,0.75]`, the first turning-point `h` value stays positive and the second stays negative, so both strict maxima persist continuously. The high-rate minus low-rate likelihood is about `-2.4543261` at 0.7 and `+2.0990710` at 0.75. The intermediate value theorem gives an exact tie between them. The sign inequalities are independently checked at 80 digits with large margins in [the reproducer](2026-11-07-zip-cert-data/oracle.py) and [its output](2026-11-07-zip-cert-data/oracle.json). The displayed decimal is a numerical locator; exact equality at its rounded binary64 representation is not claimed. That input correctly produces a practical `tie` stop. Inputs displaced by `+/-1e-6` select the appropriate different winners.

## Validation

The source revision is **`017b9f8ec3bed7610ccd41c62738bce1ea987316`**, plus the uncommitted files identified by [SHA-256 manifest](2026-11-07-zip-cert-data/source-sha256.txt). All hashes were rechecked after site generation. The historical PNG regenerated by the site check was restored byte-for-byte to its original tracked contents.

Final runs completed with these results:

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 dev shards passed |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129 full shards passed |
| `just cpp-test-no-long-double` | simbox | 129/129 passed |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21016 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43836 |
| `just js-build` | workstation | passed |
| `just js-test` | workstation | 440/440 passed; no skips |
| `just site-check` | workstation | passed; 99 family pages and 99 compiled C++17 snippets |

The dev-tier skips are tier/environment skips, not failures; the full R run has none. An initial metadata check rejected unquoted YAML `yes`; quoting it corrected that issue before the final reruns. Compiler/build warnings outside the changed ZIP code were not counted as R test warnings. No final test failure or permitted failure remains. Raw result summaries are retained [here](2026-11-07-zip-cert-data/final-summary.json), [for native/R suites](2026-11-07-zip-cert-data/suite-summary.txt), and [for the site](2026-11-07-zip-cert-data/site-check-summary.txt).

Additional numerical checks:

- [All four golden hold masks](2026-11-07-zip-cert-data/golden-check.json) match every coefficient and likelihood at scaled tolerance `1e-10`. Maximum scaled coefficient error is `8.65e-15`; maximum scaled likelihood error is `3.34e-15`. Default coefficients and likelihood match the fixture exactly.
- The [70-digit sweep](2026-11-07-zip-cert-data/sweep.json) passed all 192 cases without a stop: four zero/positive-count compositions, positive counts `1,2,5,10,100,2147483647`, and eight held inflations from `1e-8` to `1-1e-8`. It evaluates the original score and likelihood independently in mpmath, using the proved branch partition. Maximum relative rate error was `4.60e-14`; maximum absolute likelihood error was `1.21e-13`. Acceptance thresholds were `1e-9` relative rate and `1e-7*(1+abs(logLik))` likelihood.
- Appended C++, R and shared JS tests cover both winning branches, the tied modes, winner switches, a forced `iterlim=1` budget stop, turning-point numerical stops, zero limits, stable held probabilities, and the tiny-rate inflation correction. The initial ZIP-only doctest run passed 11 cases/18304 assertions; the subsequently appended tiny-rate case is included in all final C++ tiers.

The [workstation environment](2026-11-07-zip-cert-data/environment-workstation.txt) is Linux x86-64, GCC 15.2.0, Python 3.14.5, mpmath 1.3.0, R 4.6.1/BH 1.90.0.1 and Node 22.22.1. [simbox](2026-11-07-zip-cert-data/environment-simbox.txt) used GCC 15.2.0 and R 4.6.1/BH 1.90.0.1. All local compute used the shared slot wrappers; simbox runs used `simbox-run`, `nice -n 10`, four build/test workers and single-threaded BLAS/OpenMP. No fallback to local long suites was needed. Queue waits are excluded from fitter timings.

## Timings

Core C++ fits, `-O2 -std=c++17`, 99 zeros and one ten, 10,000 fits per cell, seven alternating baseline/current runs on the workstation. The baseline fitter is reconstructed read-only from `017b9f8e`; medians are microseconds per fit. This is one deterministic workload, not a general speed guarantee.

| Inflation | Before | After | Speedup |
|---|---:|---:|---:|
| Free (default fit) | 1.72743 | 1.71236 | 1.01x |
| Held 0.001 | 17.4795 | 1.71583 | 10.19x |
| Held 0.1 | 17.7978 | 3.20671 | 5.55x |
| Held 0.9 | 24.6361 | 2.04168 | 12.07x |

The helper evaluates only constant-cost score equations during root isolation and performs at most two full-data peak likelihood evaluations. The former mesh repeatedly evaluated full-data likelihoods. The default branch's timing is essentially unchanged. [All samples and medians](2026-11-07-zip-cert-data/timing-summary.json) and the [benchmark runner](2026-11-07-zip-cert-data/run-timing.sh) are retained; the earlier single-run measurements are also kept as raw evidence.

## Reproduction

Run from this worktree root, using its installed tools; no dependency installation is required:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
zip_evidence=dev/research/evaluations/2026-11-07-zip-cert-data
/tmp/umvl-cpu/simbox-run bash "$zip_evidence/run-final-suites.sh"
/tmp/umvl-cpu/run4 just js-build
/tmp/umvl-cpu/run just js-test
/tmp/umvl-cpu/run4 just site-check
/tmp/umvl-cpu/run bash "$zip_evidence/run-timing.sh"
/tmp/umvl-cpu/run python3 "$zip_evidence/oracle.py"
```

The final-suite runner records each native/R suite separately and stops on any failure. If `simbox-run` returns 75, rerun that runner locally through `run4`, as specified by the launch command's fallback policy. To reproduce the independent sweep or golden comparison, compile `probe.cpp` or `golden-check.cpp` under `run` with `-O2 -std=c++17`, `-IunivariateML-cpp/include` and the installed BH include directory, then invoke `sweep.py` or `golden-check.py` under `run` with the resulting executable path as its argument. These checks are deterministic; there are no simulation seeds.

## Open items

None blocks the requested J1 scope. Near-degenerate topology, unresolved peak comparisons and exhausted work intentionally stop with the documented evidence rather than returning an unchecked estimate. The certificate's floating-point qualification is explicit; outward-rounded interval arithmetic is not implemented or claimed.
