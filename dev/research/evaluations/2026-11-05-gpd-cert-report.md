# TASK-116.2: certified GPD scale and shape fit

The production GPD fitter now certifies the global scale/shape maximum with the threshold held and shape restricted to $\xi\ge-1$, or stops with a structured budget, numerical, tie or non-existence diagnostic. Held-shape and held-scale fits are covered. The default golden coefficients remain within their existing tolerance, and the golden fixture is byte-for-byte unchanged. All required final dev and full suites passed.

Joint uniqueness is false in general. The canonical dossier now gives an exact three-observation counterexample construction, with a numerical illustration and a checked finite-candidate tie stop.

Tested source is base `fe4573e9b02a950a1440039a8395c30ff2f25926` on `codex/gpd-cert`, plus the uncommitted GPD changes. The [final validation record](2026-11-05-gpd-cert-data/test-summary.json) contains the source and evidence fingerprints, commands, machine, results and numerical error maxima. The fingerprints were verified after the final suites finished. No commit was made.

The implementation and mathematical contract are in [mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/mle.hpp), [certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/certificate.hpp) and the [canonical dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md). The [binding diagnostics](../../../bindings/common/distributions/gpd/fit.hpp) transport `core/stop_detail.hpp` records to R conditions.

For positive excesses, let $M$ be their maximum, $v_i=y_i/M$, $b=\xi M/\sigma$ and $t=\log(1+b)$. The integral extension of $D(b)=\overline{\log(1+bv)/b}$ removes the singularity at zero. Joint profiling gives $\sigma=MD$, $\xi=bD$ and mean log likelihood $-\log M-\log D-bD-1$ when the shape is admissible. Shape zero is an interior point of this continuous profile.

Monotone integral formulas enclose $D$, $-D'$ and $D''$. They give score and curvature bounds on every queued interval. A lower curvature bound supplies an upper chord envelope; score bounds exclude nonstationary intervals. A retained maximum requires strict concavity and opposite signed endpoint scores. Overlapping neighborhoods cover roots on subdivision boundaries. Analytic tail bounds cover both omitted tails, and the attained uniform member is compared explicitly. The algorithm assumes no general unimodality theorem.

| Fitted subset, always with threshold held | Certificate or exact result |
|---|---|
| Scale and shape free | Complete scalar profile coverage, signed root isolation, analytic tails and uniform comparison. |
| Shape held above −1 | Strictly decreasing scaled scale score and a signed scale bracket; threshold-collapse exceptions stop. |
| Shape held at zero or −1 | Mean excess or maximum excess, respectively; representability is checked for the mean. |
| Scale held | Exact uniform maximum when the scale covers the positive excesses; otherwise the scalar shape certificate. All-zero excesses give a structured unidentified-shape tie. |
| All coefficients held | The existing finite likelihood evaluation contract. |

Certificates are exact-arithmetic arguments conditional on the computed enclosures and comparisons, with the documented practical binary64 allowance $\eta=512\epsilon\{1+\log(n+1)\}$ and a mean-log-likelihood separation margin of $10^{-8}$. The dossier states the elementary-function assumptions. This is the requested package certificate standard; no outward-rounded interval or universal libm error theorem is claimed.

Root enclosures have width below $2\,10^{-7}$ in the transformed coordinate, or directly enclose scale to relative width $3\,10^{-7}$ and shape to width $3\,10^{-7}\max(1,|\xi|)$. Bisection/Newton refinement supplies the returned representative. Restoration and likelihood consistency checks protect the stored physical coefficients. The documented certificate accuracy is approximately $10^{-6}$; the independent checks below separately assess the much more accurate representatives and unchanged golden values.

The global subdivision budget is $128\min(\texttt{iterlim},100000)$. Root passes and proposal refinements have their own finite bounds. Tail guards at −32 and 64 require analytic exclusion of the omitted regions; an unresolved guard stops rather than returning an estimate at a cap. Normalization, coefficient restoration and ambiguous comparisons also stop. A small optimizer score never overrides a failed global certificate.

Threshold ties with free scale and shape stop for unbounded likelihood. With positive shape held, the equality case in the collapse exponent instead records its finite unattained supremum. These have distinct `nomle` and `limit` conditions. Budget and numerical stops expose bounds and candidates when available; near-equal finite maxima and unidentified shape use `tie`.

Final review found and fixed a representability gap in the closed exponential subset: for excesses equal to one and two minimum subnormals, the mathematical mean is not representable accurately by a double. It now stops numerically. The mean of one and three minimum subnormals is exactly representable and still succeeds. Both C++ and R tests protect these cases. Every required suite was rerun after this fix.

The nonuniqueness proof uses the sample $(a_*,a_*,1)$, where $a_*$ is the attained maximum of an explicit continuous scalar function given in the dossier. Monotonicity in the smaller observation bounds every admissible profile by zero. Both the uniform member $(\sigma,\xi)=(1,-1)$ and a distinct interior member attain zero log likelihood. This proves nonuniqueness within the package's closed uniform convention without numerical root finding.

The [numerical illustration](2026-11-05-gpd-cert-data/uniqueness-validation.json) locates a critical candidate near $a=0.07880675592383729$, with interior coefficients $\sigma\approx0.2058062908136120$, $\xi\approx0.5808198884419236$. The binary64 critical sample stops as a margin-aware finite tie. Changing $a$ by $-10^{-5}$ selects the interior maximum; changing it by $+10^{-5}$ selects the uniform maximum. The decimal constants are numerical approximations, distinct from the exact max-defined construction. The [reproducer](2026-11-05-gpd-cert-data/check_uniqueness.py) checks all three outcomes.

The [independent numerical check](2026-11-05-gpd-cert-data/check_certificate.py) covers 22 workloads: the unchanged golden, six quantile shape regimes, three samples around the exponential stationary point, a 10,000-observation seed-1162 sample, held subsets, threshold ties with scale held, uniform and constant samples, extreme rescalings and a representable subnormal mean. It solves the stationary equations at 80 decimal digits on the exact binary64 input values. These local high-precision solves corroborate coefficient and likelihood accuracy; the analytic enclosures provide the global certificate.

The largest observed relative scale error was 3.12e-13; shape error divided by max(1, absolute shape) was 8.56e-16; mean log-likelihood error was 4.19e-13. The unchanged golden coefficients pass the existing relative tolerance of 1e-12. The final targeted C++ run passed 20 cases and 4,978 assertions; the full-tier R GPD target passed 199 assertions.

Every final command below finished green on the local workstation. The four-slot reservations used one BLAS/OpenMP thread per worker and nice priority 10. Dev skips are the tier exclusions; the full suites had no skips. There were no permitted failures.

| Command | Final result |
|---|---|
| `just cpp-test` | 106/106 shards passed |
| `just cpp-test-no-long-double` | 127/127 shards passed |
| `UMVL_TEST_TIER=full just cpp-test` | 127/127 shards passed |
| `just test` | FAIL 0, WARN 0, SKIP 154, PASS 20641 |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43456 |

The retained before/after benchmark compiles the complete base-revision fitter and the replacement in the same C++17 `-O2` binary. Entries are means of five calls through one local CPU slot at nice priority 10. Compilation and slot waiting are excluded. The [JSON record](2026-11-05-gpd-cert-data/certificate-validation.json) retains every workload, result and timing.

| Workload | n | Baseline ms | Certified ms | Work count |
|---|---:|---:|---:|---:|
| Golden | 500 | 0.5729 | 3.4132 | 84 |
| Negative-shape quantiles, generating shape −0.9 | 400 | 0.4139 | 2.1594 | 132 |
| Exponential stationary-point sample | 400 | 0.2481 | 3.3963 | 82 |
| Seed 1162, generating shape 0.3 | 10000 | 11.0906 | 72.2379 | 158 |
| Scale held at 1 | 6 | 0.0113 | 0.0269 | 19 |
| Scale held at 1, threshold ties | 7 | 0.0107 | 0.0345 | 26 |
| Held scale covers all excesses | 4 | 0.0050 | 0.0001 | 0 |
| Shape held at 0.3 | 500 | 0.0331 | 0.0899 | 32 |

The iterative certificate costs more than the previous sampled search on these workloads. It evaluates three integral derivatives and proves coverage of every competing region. The held-scale uniform shortcut is exact and avoids the former numerical optimization. Work counts exclude separately bounded root refinements and initialization. Timings describe this shared workstation run, not an isolated hardware benchmark.

The [environment record](2026-11-05-gpd-cert-data/environment.txt) records GCC 15.2.0, Python 3.14.5, R 4.6.1, Rcpp 1.1.2, BH 1.90.0.1, testthat 3.3.2 and roxygen2 8.0.0. The numerical reference uses mpmath 1.3.0. Native and double-width-extended C++ builds both passed. The standalone comparison driver builds without R.

Reproduce the numerical checks from the worktree root:

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-05-gpd-cert-data/check_certificate.py \
  --output dev/research/evaluations/2026-11-05-gpd-cert-data/certificate-validation.json
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-05-gpd-cert-data/check_uniqueness.py
```

Run the suite commands in the table through `/tmp/umvl-cpu/run4`. The [final audit checker](2026-11-05-gpd-cert-data/check_final.py) consumes their `build/scratch/` logs, verifies the tested fingerprints and unchanged golden, checks source scope, syntax, whitespace and local report links, and freezes the retained test summary.

Only GPD sources, its binding diagnostics/tests and the requested evaluation evidence changed. No d/p/q/r implementation, special-function seam, truth fixture, code generator, golden, other family, NEWS, status or roadmap source changed. No dependency installation, simbox, sub-agent, board edit, commit or publication was used. The normal R test builds staged their generated package only inside this worktree's temporary directory.

Runtime note: the required CPU entry points were initially absent. Small shared runtime entry points were restored at `/tmp/umvl-cpu/run` and `run4`; they currently symlink to this worktree's ignored `build/scratch/umvl-cpu-wrapper.py`. Their lock files reserve at most four slots and all subsequent builds, tests and numerical jobs used those reservations. No other checkout was touched.

Artifact-pane display: the prescribed `show` command returned HTTP 403 because this worktree is outside the viewer's configured readable roots. Display requires registering this worktree with the viewer or adding it to `CODEX_MATH_WEB_VIEW_ROOTS`. The report and evidence are retained at the requested worktree paths.
