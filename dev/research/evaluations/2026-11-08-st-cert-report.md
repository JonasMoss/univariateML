# TASK-116.55: Azzalini skew-t certificates

The fitter now uses six additional globally justified routes: conditional location, symmetric conditional scale, one-sign joint slant/scale, constant-data joint location/slant with held df, the corresponding free-df half-normal corner, and the exact symmetric all-nuisance Student reduction. The existing alpha-only certificate also has a stronger practical floating-point allowance for thin-tail score ratios. These routes are wired into the C++ fitter and the common R/JS binding. All required final validation runs passed.

General joint skew-t optimization is still numerical. The dossier therefore retains `fit.certified: 'partial'` and names the actual certified conditions in `fit.note`. The open entries below identify the missing algorithms and bounds, including two compactness ingredients derived during this job. They do not claim that numerical profiling certifies those subsets.

The source base is `ddb399172bd99091c8d1b04dbf7fe329c8adc43e`. Work is uncommitted; no merge or rebase was performed. Evidence, scripts and logs are in [the job data directory](2026-11-08-st-cert-data/). The [final source manifest](2026-11-08-st-cert-data/source-manifest.json) records hashes, append-only test checks and frozen-file comparisons.

## Implemented scope

The mask uses bits 1 = xi, 2 = omega, 4 = alpha, 8 = nu; a set bit means free. All held coefficients must be finite and proper. Held nu may be any positive value; free nu retains the existing nu >= 1 restriction. “Open” means no global certificate for the general subset, beyond existing structural stops and any explicitly listed special case.

| Mask | Free coefficients | Global certificate or exact decision |
|---:|---|---|
| 0 | none | Direct likelihood evaluation, already supported. |
| 1 | xi | Every proper held omega, alpha, nu; exhaustive stationary-component enclosures. |
| 2 | omega | Held alpha exactly zero: unique finite root, unbounded collapse, or unattained equality-collapse supremum. Nonzero held alpha remains open. |
| 3 | xi, omega | Open, including the general symmetric fixed-nu problem. |
| 4 | alpha | Existing global scalar certificate, with repaired weighted-ratio arithmetic; exact one-sign half-t stops and all-zero-residual ties. |
| 5 | xi, alpha | Constant data: sharp equal suprema on the two half-t faces. Nonconstant data remain open. |
| 6 | omega, alpha | All nonzero residuals have one sign: monotone-alpha reduction to the certified symmetric scale problem. Mixed signs remain open. |
| 7 | xi, omega, alpha | Open. |
| 8 | nu | Open. |
| 9 | xi, nu | Open. |
| 10 | omega, nu | Open. |
| 11 | xi, omega, nu | Held alpha exactly zero: delegate to `lst_mle_certified` on the identical restricted domain. Other held alpha remain open. |
| 12 | alpha, nu | Open. |
| 13 | xi, alpha, nu | Constant data and held scale: sharp equal half-normal suprema. Nonconstant data remain open. |
| 14 | omega, alpha, nu | Open. |
| 15 | xi, omega, alpha, nu | Existing default numerical search and face comparisons; globally uncertified. |

Implementation is in [st/certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/st/certificate.hpp), routed by [st/mle.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp). [The common binding](../../../bindings/common/distributions/st/fit.hpp) transports the stop details, including the newly proved unattained collapse decision. Routing is not confined to R. No shared numerical header was changed and no certificate helper is left unused.

## Mathematical justification

The complete arguments, including the unimplemented compactness constructions, are in [proofs.md](2026-11-08-st-cert-data/proofs.md). Certificates use the package convention: **certified in exact arithmetic, conditional on the computed comparisons, with a stated practical floating-point margin**. They are not outward-rounded special-function proofs.

For location, write m = nu + 1, z = (x-xi)/omega and h = alpha z sqrt(m/(nu+z²)). The z log-density derivative is

```
g(z) = -m z/(nu+z²)
       + [t_m(h)/T_m(h)] alpha nu sqrt(m)/(nu+z²)^(3/2).
```

For positive alpha, this derivative is positive on negative z. For nonnegative z the CDF ratio is at most 2 t_m(0), which makes the derivative strictly negative outside a finite residual bound. In particular, R² = 4 t_m(0) alpha nu/sqrt(m) gives the compact location interval [min(x)-omega R, max(x)]. Reflection handles negative alpha; zero alpha uses the sample range. The likelihood vanishes at escaping location, so this contains every global maximum.

Inside the interval, the exact extrema of z/(nu+z²), monotonicity of h, Student pdf unimodality and CDF monotonicity enclose every observation's location-score contribution. The search discards only strictly signed score boxes, retains connected stationary-component hulls, and compares their likelihood enclosures using integrated score bounds. It assumes neither concavity nor uniqueness. The normalized/public subdivision target is 2e-13; the accepted public component width is at most 4e-11 (1+abs(xi)). Wider or unresolved components stop. Budget and numerical stops preserve valid witnesses rather than return the last iterate.

For symmetric scale with xi, nu held, let d_i = x_i-xi and k count zero residuals. The log-scale score is

```
S(omega) = -n + (nu+1) sum d_i²/(nu omega²+d_i²).
```

It is strictly decreasing whenever a residual is nonzero. Its limits are e = nu(n-k)-k at zero scale and -n at infinity. Thus e > 0 gives exactly one global root. Padded sign brackets and bisection narrow its log-scale width to 2e-11. Exhausting a solve gives numerical failure with a finite witness. If e < 0, or all residuals vanish, likelihood is unbounded. If e = 0 and some residual is nonzero, the score is strictly negative at every finite scale and the sharp unattained log supremum is

```
n log t_nu(0) + (nu+1)(n-k) log(nu)/2
  - (nu+1) sum_nonzero log|d_i|.
```

For free alpha and omega with all nonzero residuals of one sign, likelihood is strictly monotone in alpha toward that sign's infinity. The joint supremum equals the symmetric scale supremum plus (n-k) log(2). Observations at the held endpoint retain factor one, not the closed half-t factor two. This proves the finite-scale half-t stop and the equality-collapse stop; reflection preserves the original held location. Equality is not extrapolated to moving-location problems.

For constant data c with omega, nu held and xi, alpha free, every finite density is strictly below 2 t_nu(0)/omega. Approaching c from either side while alpha z tends to positive infinity attains that bound only as a limit. Both half-t faces therefore have the same sharp supremum. If nu is also free, strict Jensen applied to

```
t_nu(0) = phi(0) E sqrt(V/nu),  V ~ chi-squared(nu)
```

gives t_nu(0) < phi(0). The sharp joint bound becomes n log(2 phi(0)/omega). A sequence nu = j+4, xi = c-sign*omega/j, alpha = sign*j² reaches it. Uniform Student second moments make the skew-CDF tail vanish along this sequence. Neither bound is attained at finite coefficients; the two half-normal faces tie in the free-nu case. Structured tie payloads carry both face models and global likelihood bounds. These are ties of limiting suprema, not two finite fitted skew-t laws.

When alpha is held exactly zero and xi, omega, nu are all free, the skew-t likelihood equals the location-scale Student likelihood identically. Both domains restrict free df to at least one. The implementation delegates to the parent's certified all-free fitter and transports finite, normal-face, collapse, tie and budget decisions. Student candidate tags and bounds are retained, the normal face is expressed as skew-normal with alpha zero, and the parent's df lower-bound flag becomes the skew-t nu flag. A small estimated alpha does not activate this identity.

This delegation inherits the parent's precision, including its enclosure-width clamp of 1e-7 to 1e-6. It does **not** establish a universal relative 1e-10 coefficient guarantee for that route. The tested cases meet this family's 1e-5 coefficient golden tolerance. The new scalar routes have the tighter widths stated above. This inherited precision limitation remains explicit rather than being hidden behind the certified flag.

### Nonuniqueness counterexamples

For x = (-2,2), held omega = nu = 1 and alpha = 0, maximizing likelihood minimizes xi⁴-6xi²+25. There are exactly two global location maxima, xi = ±sqrt(3). The new location certificate returns a tie with both witnesses.

For x = (-1,1), held nu = 1 and alpha = 0, with xi and omega free,

```
L = omega² / {pi² [4 omega² + (xi²+omega²-1)²]}
  <= 1/(4 pi²).
```

Every xi²+omega² = 1 with omega > 0 attains equality. There is an entire continuum of finite global maximizers. The two collapse endpoints have the same limiting value, so equality of a collapse exponent cannot by itself justify a no-MLE declaration in the joint location/scale problem. This is an exact counterexample for an open subset, not a claim that its current numerical route is certified.

## Floating-point handling and stop behavior

During development, the initial final-score-only padding failed thin-tail checks: 33 of 37 point probes exceeded that allowance. At nu = alpha = 1e6 and z = -1, the observed score error was about 9.31e-4 against an allowance of 2.84e-8. The defect was cancellation in log-pdf minus log-CDF followed by multiplication by a score weight; the exact interval inequality was not the problem.

The repaired alpha and location score helpers combine the weight in log space. For computed log-pdf p, log-CDF c and log-weight w, they use

```
E = 128 epsilon * (1 + |p| + |c|
                  + sum |working log-weight terms| + |p-c+w|)
```

before exponentiation, enclosing the positive weighted ratio by exp(p-c+w±E). Final term-sum padding remains. Nonfinite bounds cause a numerical stop. The [80-digit stress results](2026-11-08-st-cert-data/thin-score-checks.json) contain 31 finite enclosures, all containing their reference values, and six explicitly unresolved probes; no incorrect finite enclosure remains. [The failed initial allowance](2026-11-08-st-cert-data/thin-score-before.json) is retained as evidence, not counted as a successful final check.

Stopped location witnesses are reevaluated at their rounded public coefficients. Their likelihood bounds include the normalized/public likelihood difference and rounding allowance. Invalid or unrepresentable witnesses are omitted; if none remain, the result is numerical failure rather than an invalid budget payload. The regression at location near 1e300 and scale eight ulps checks the serialized bounds against the public density evaluator.

New loops use the existing iterlim-scaled solver or certificate work budgets; there is no fixed search cap that iterlim cannot raise. Reaching a cap, an unresolved comparison, an unrepresentable coefficient or failed arithmetic cannot produce an unchecked successful estimate on the new routes. The common binding distinguishes limit, tie, budget, numerical and proved no-MLE stops. The new equality-collapse reason reports the unattained supremum with bounds; the older unresolved moving-location equality case keeps its separate unresolved stop.

## Independent checks and frozen goldens

[The independent checker](2026-11-08-st-cert-data/check.py) uses 60-digit mpmath calculations and a numerical root census. It checked 12 location fits and four symmetric scale fits. Maximum location error was 2.6199853945088413e-12; maximum relative scale error was 3.965404890554209e-12. Across 400 score boxes and 2,000 sampled points, the largest unpadded term violation was 7.605803995652019e-16, within the implemented score padding. These finite experiments check arithmetic and implementation; the global justification is the mathematical enclosure argument, not the census. Detailed values are in [checks.json](2026-11-08-st-cert-data/checks.json).

For the frozen golden sample, holding omega, alpha and nu at their stored binary64 values gives the independent conditional location optimum

```
xi = -0.0365390228229098411950770372957525307076247348476245659876553
log likelihood = -774.302307891845502396934690741359336335483926927409851307923
```

The stored all-free xi is -0.036539023191020196523748353456539916805922985076904296875. The conditional optimum differs by +3.6811035532867132e-10; the stored point's conditional score is about 1.6333081378903756e-7. The certified conditional result has error 7.209573814060201e-14 against the high-precision value. This comparison is conditional, not an independent four-free-coordinate optimum. The difference is within the family's documented coefficient golden tolerance of 1e-5, though larger than 1e-10. [The calculation and exact values](2026-11-08-st-cert-data/golden-conditional-check.json) are recorded without changing the golden.

The audit confirms that golden.json, dpqr.hpp and both truth files are byte-identical to the base revision. Default all-free status, certification flag, coefficients and likelihood have identical printed values before and after on both timing samples. The default numerical algorithm was not replaced. All native, R and JS golden regressions passed.

## Timings

These are single-call elapsed milliseconds on the shared workstation, compiled as C++17 with optimization, with one reserved CPU and BLAS/OpenMP restricted to one thread. They are reproducible workloads, not statistical speed estimates; contention and cache state affect unchanged routes. In particular, the change in all-free elapsed time is not evidence of an algorithmic speedup.

The seven-point sample is (-3,-1,-0.2,0.1,0.8,2,4). The 500-point sample uses z = tan(2.5((i+0.5)/n-0.5)), multiplying positive z by 1.5. Holds are xi = 0, omega = 1, alpha = 2, nu = 4; controls are tol = 1e-6, maxit = 200. Consequently the general scale and moving-location joint rows do not activate the symmetric or constant-data exceptions. The mask legend is the scope table above.

| Mask | n=7 before | n=7 after | n=500 before | n=500 after |
|---:|---:|---:|---:|---:|
| 0 | 0.155 | 0.092 | 1.533 | 1.692 |
| 1 | 1.130 | 11.030 | 61.294 | 794.558 |
| 2 | 0.887 | 0.578 | 53.981 | 70.022 |
| 3 | 1.694 | 1.075 | 89.657 | 112.986 |
| 4 | 8.106 | 5.197 | 313.234 | 325.199 |
| 5 | 22.102 | 16.909 | 698.495 | 755.363 |
| 6 | 10.429 | 6.558 | 682.480 | 670.233 |
| 7 | 31.168 | 20.982 | 1338.396 | 1322.493 |
| 8 | 10.185 | 9.423 | 184.501 | 127.059 |
| 9 | 8.864 | 8.921 | 587.452 | 665.064 |
| 10 | 8.923 | 9.534 | 718.027 | 835.099 |
| 11 | 23.774 | 24.561 | 2456.195 | 2389.304 |
| 12 | 81.654 | 85.176 | 3563.385 | 4149.725 |
| 13 | 97.971 | 104.187 | 7473.875 | 7984.323 |
| 14 | 116.107 | 125.530 | 8206.079 | 7455.486 |
| 15 | 665.226 | 695.218 | 35561.335 | 19289.615 |

The exhaustive location certificate costs substantially more than the previous local solve: about 9.8 times on seven points and 13.0 times on 500. It now excludes every competing stationary component or stops explicitly. Additional workloads exercise the new special routes:

| Workload | Before ms | After ms | Final outcome |
|---|---:|---:|---|
| Seven-point location, alpha=0, omega=1, nu=4 | 0.168667 | 0.103190 | Certified finite location. |
| Seven-point scale, alpha=0, xi=0, nu=4 | 0.047569 | 0.013419 | Certified finite scale. |
| Constant data, xi/alpha free, omega=1, nu=4 | 12.897868 | 0.003384 | Sharp two-half-t supremum tie. |
| Symmetric Student, xi/omega/nu free, heavy seven-point data | 5.129595 | 5.913074 | Certified Student fit on nu=1. |
| Symmetric Student, xi/omega/nu free, (-2,-1,0,1,2) | 0.658046 | 0.399100 | Certified normal limit. |
| Constant data, xi/alpha/nu free, omega=1 | 171.791952 | 0.002792 | Sharp two-half-normal supremum tie. |
| (0,1,2,4), scale free, xi=0, alpha=0, nu=4 | 0.039053 | 0.011691 | Certified scale 1.9145628671728367. |
| (0,1,2,4), alpha/scale free, xi=0, nu=4 | 6.210103 | 0.007236 | Certified right-half-t limit at that scale. |

Raw results include coefficients, status and certification flags: [before](2026-11-08-st-cert-data/timing-before.txt), [after](2026-11-08-st-cert-data/timing-after.txt), [extra before](2026-11-08-st-cert-data/extra-timing-before.txt), [extra after](2026-11-08-st-cert-data/extra-timing-after.txt), and [parsed summary](2026-11-08-st-cert-data/timing-summary.json). The before/after programs are identical and use the base versus final headers. [timing-baseline.sh](2026-11-08-st-cert-data/timing-baseline.sh) reconstructs the base with a read-only git archive into this worktree's scratch directory; [postfix-evidence.sh](2026-11-08-st-cert-data/postfix-evidence.sh) reproduces the final independent checks and timings. The caller must reserve a CPU; neither script acquires a nested lock.

## Open entries

1. **Nonzero-held-alpha scale-only:** the skew score contribution destroys the monotonicity used in the symmetric proof. Missing are a global scalar score enclosure, quantitative small/large-scale exclusions and finite-versus-collapse comparisons at equality. The alpha=0 proof does not cover this case.

2. **Mixed-sign alpha/scale with xi,nu held:** for e > 0, this job derived a compact rectangle containing all global maxima. Uniform density bounds exclude both scale tails relative to an incumbent. On the remaining scale interval, a positive lower bound for every nonzero residual's skew argument makes the existing alpha-tail inequalities uniform. Explicit formulas are in the proofs. Thus attainment and compactness in this strict case are settled mathematically. Missing are the tested two-dimensional score/likelihood search, connected stationary sets, public-coordinate accuracy in both dimensions, and floating-point validation of the rectangle arithmetic. At e = 0, the finite-versus-collapse comparison is additionally unresolved. Selected conditional fits are insufficient.

3. **Nonconstant xi/alpha with omega,nu held:** a location bound uniform over alpha was derived. If L0 is a finite incumbent, B = n log(2 t_nu(0)/omega) and

   ```
   D = omega sqrt(nu expm1(2(B-L0+1)/(n(nu+1)))),
   ```

   then location outside [min(x)-D,max(x)+D] has log likelihood at most L0-1. Fixed-nu half-t face suprema are also explicit at the sample extremes, with the correct reachable endpoint factor; a free face scale has a monotone profile. Missing are quantitative moving-endpoint layer bounds as alpha diverges and the global finite two-dimensional search. Conditional alpha brackets are not uniform as xi crosses a datum. Lack of a location bound is not the blocker.

4. **General moving xi/omega joint subsets, including fixed-nu symmetry:** global stationary sets and collapse layers remain to be enclosed and compared. The Cauchy semicircle counterexample proves that isolated-root enumeration or equality-collapse nonexistence logic cannot handle all cases. Adding alpha requires the endpoint layers above; adding nu requires the remaining df analysis. The exact all-free symmetric Student delegation covers only its stated mask and holds.

5. **Remaining nu-free subsets:** missing are a finite-nu global search, quantitative computed df-tail and joint-layer bounds, the nu=1 restriction face, and hold-compatible normal/half-t/half-normal integration. The sn and ht families already have global certificates on their own domains. They are not missing parents. However, held-endpoint ht ordinarily permits every positive nu, while free St nu remains at least one, so its unconstrained parent result cannot simply certify the constrained St face. Closure theory establishes escape structure, not the required implemented comparisons. Constant-data and exact symmetric routes above settle only their specified exceptions.

6. **Inherited Student precision:** the exact reduction preserves the parent's certificate and accuracy policy. A universal relative 1e-10 assertion would require tighter parent coefficient enclosures or an explicit accuracy stop. That shared-parent change is outside this family's scope and was not made. The present route is tested to the existing family golden accuracy, with this limitation recorded.

These entries explain why the family cannot be marked `yes`. They are not unused proposed helpers: only the proved implemented routes are attached to the fitter. No new structural `holds:` exceptions were declared.

## Final validation

All commands ran separately to completion with CPU reservations. Source and tests were frozen throughout these final runs, including the final rounded-public-location witness regression. Long suites used simbox as requested; there was no final-run simbox fallback. The workstation compiler was GCC 15.2.0 and simbox GCC 13.3.0, recorded in the [local](2026-11-08-st-cert-data/compiler-workstation.txt) and [remote](2026-11-08-st-cert-data/compiler-simbox.txt) metadata.

| Command | Machine / reservation | Final result |
|---|---|---|
| `just cpp-test` | Workstation, `/tmp/umvl-cpu/run4` | 106/106, 141.93 s test time. |
| `UMVL_TEST_TIER=full just cpp-test` | Simbox, `/tmp/umvl-cpu/simbox-run`, four build/test workers | 129/129, 196.68 s. |
| `just cpp-test-no-long-double` | Workstation, `/tmp/umvl-cpu/run4` | 129/129, 494.47 s. |
| `just cpp-test-no-long-double` | Simbox, `/tmp/umvl-cpu/simbox-run`, four build/test workers | 129/129, 175.13 s. |
| `just test` | Simbox, `/tmp/umvl-cpu/simbox-run`, four workers | FAIL 0, WARN 0, SKIP 154, PASS 22181. |
| `just test-full` | Simbox, `/tmp/umvl-cpu/simbox-run`, four workers | FAIL 0, WARN 0, SKIP 0, PASS 45013; 288.4 s test duration. |
| `just js-build` | Workstation, `/tmp/umvl-cpu/run`, one pinned CPU | Passed. |
| `just js-test` | Workstation, `/tmp/umvl-cpu/run`, one pinned CPU | 559/559; zero failures, cancellations or skips; 218.57 s. |
| `just site-check` | Workstation, `/tmp/umvl-cpu/run`, one pinned CPU | Passed, including 99 pages and 99 generated C++ examples. |
| Independent checks, final timings | Workstation, `/tmp/umvl-cpu/run`, one pinned CPU | Passed. |
| `audit-final.py` | Workstation, `/tmp/umvl-cpu/run`, one pinned CPU | Passed. |

Simbox commands explicitly set `CMAKE_BUILD_PARALLEL_LEVEL=4` and `CTEST_PARALLEL_LEVEL=4`. Local one-CPU commands use [one-cpu-command.sh](2026-11-08-st-cert-data/one-cpu-command.sh) inside the reservation; the JS build additionally sets `EMCC_CORES=1 BINARYEN_CORES=1`. The reservation wrappers set nice priority and single-thread BLAS/OpenMP. All temporary runtime paths were inside `build/tmp`. Lock-queue time is excluded from the reported suite test durations.

Final logs: [C++ dev](2026-11-08-st-cert-data/cpp-dev.txt.gz), [C++ full](2026-11-08-st-cert-data/cpp-full.txt.gz), [local double-only](2026-11-08-st-cert-data/cpp-no-long-double-workstation.txt.gz), [simbox double-only](2026-11-08-st-cert-data/cpp-no-long-double-simbox.txt.gz), [R dev](2026-11-08-st-cert-data/r-dev.txt.gz), [R full](2026-11-08-st-cert-data/r-full.txt.gz), [JS build](2026-11-08-st-cert-data/js-build.txt), [JS tests](2026-11-08-st-cert-data/js-test.txt), [site](2026-11-08-st-cert-data/site-check.txt), and [audit](2026-11-08-st-cert-data/audit-final.txt). Historical logs or failed initial score experiments in the directory are not substituted for these final runs.

Append-only C++, R and JS regressions cover location ties and reflection, finite scale and one-sign faces, endpoint factors, equality collapse, constant-data face ties, Student transport, budget/numerical stops, thin-tail cancellation and rounded public witnesses. The st C++ dossier now has 27 test cases. The final audit confirms exactly seven changed tracked source/test/dossier files, append-only changes to all three test files, unchanged frozen family files and identical default replay outputs. The site-generated unrelated screenshot was restored to its base bytes. Settled d/p/q/r, special functions, truth/codegen, shared numerical headers, release/status files and boards were not changed. No package installs or subagents were used.
