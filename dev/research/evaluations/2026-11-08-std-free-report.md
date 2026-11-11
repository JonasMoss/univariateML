# TASK-116.45: Student t free-shape held subsets

Completed the second review in `std-free` on 2026-11-11, based on `5ecd032cf0998f50abce3353e610dd99559221c2`. No commits, merges, rebases, board edits or installs were made.

## Result

Both shape-only subsets now have global certificates. Joint free-shape subsets can additionally certify the normal face using a signed profile expansion and a complete compact exclusion cover. General finite joint fits retain the converged legacy result marked `certified=false`; an unresolved certificate does not refuse ordinary data. Both dossiers remain `fit.certified: partial` and describe this coverage. General finite joint coefficient certificates remain open as specified below.

The three reviewed legacy expectations are restored unchanged: held-location normal mid-quantiles report `use mlnorm` in both families, and the std outlier/central-cluster example reports `use mllst`. The two normal examples now certify their global face; the lower-shape example retains its original limit class/message with `certified=false`. A finite candidate beaten by a reachable face supports the legacy nomination. A strictly better finite witness can disprove a face; failure of a numerical optimizer still stops.

Native results append a `certified` member. R exposes a logical estimate attribute and a logical limit-condition field; JS exposes the same flag on successful fits and limit errors. Two minimal edits to shared nonheader JS transport files serialize error extras and preserve `certified` on thrown errors. No shared C++ header changed. The original shared JS test file is an exact byte prefix, with appended regressions only. All frozen golden files are unchanged.

## Mathematics implemented

The full arguments, score and curvature formulas, tail bounds and remaining constrained-profile bounds are in [derivation.md](2026-11-08-std-free-data/derivation.md).

With held physical scale s, let qᵢ = ((xᵢ − m)/s)². Ordinary LST scale uses r = ν; held std standard deviation uses r = ν − 2. The latter preserves sd while ordinary scale changes with shape. The scalar certificate therefore uses the actual-hold likelihood, rather than treating held sd as held ordinary t scale.

The certificate covers every surviving finite shape interval. Enclosed score signs and positive curvature exclude maxima; negative curvature and opposite endpoint score signs isolate a unique local maximum. Curvature bounds give root inclusion and coefficient error bounds. Every included maximum is compared with the eligible faces. Unresolved certificates try the legacy search and preserve successful finite/limit results uncertified; failed searches retain structured stops. The root solve targets `min(tol, 1e-10)` and accepts only a relative enclosure width at most `min(tol, 1e-7)`.

The lower faces differ:

- **LST with location and scale held:** the uniform density bound `f ≤ sqrt(ν)/(2s)` excludes a whole neighborhood of ν = 0 using any finite likelihood witness. Exact step-two gamma recurrences extend the existing bounds below ν = 1/2.
- **Std with mean and sd held:** writing w = ν − 2 and k for observations at the held mean, the collapse exponent is `n − 3k/2`. Its sign distinguishes vanishing likelihood from unbounded likelihood. At critical equality, the finite face is `L0 = −n log(2s) − (3/2) sum_nonzero log(qᵢ)`. A signed `w log(w)` bound excludes the punctured lower neighborhood below L0 and permits comparison with all finite candidates and normal.
- **Normal:** uniform unsigned inverse-shape bounds exclude the tail when a finite witness beats the actual-hold normal value. Signed inverse-shape derivative bounds can instead certify the normal winner for the scalar problem.

Certificate work is bounded by `128 * min(iterlim, 100000)` evaluations, including refinement. The shape value `1e12` is an arithmetic refusal boundary; it is never returned as a fitted cap. Rounded cutoffs must establish their inequalities or the certificate stops.

These are certificates in exact arithmetic conditional on computed comparisons, with the existing practical binary64 margins: 64-epsilon operation margins and pairwise reduction margins. They do not claim outward-rounded interval arithmetic. The analytical bounds establish uniform validity; the high-precision computations below are independent numerical checks.

## Signed normal-face certificate

The derivation and all constrained lower-face bounds are in [signed-derivation.md](2026-11-08-std-free-data/signed-derivation.md). At the Gaussian fit respecting the holds, let z be the residual divided by the actual physical scale. The inverse-shape coefficients are

```text
lst: c1 = sum(z^4 - 2 z^2 - 1)/4,
std: c1 = sum(z^4 - 6 z^2 + 3)/4.
```

For std use w=1/(nu-2); its first coefficient is also the coefficient in 1/nu. When scale is free, the Gaussian score gives sum(z^2)=n, so both profiles have c1=n(b2-3)/4 about the held centre. Reoptimization changes only second-order terms. With physical scale held, that identity need not hold; the actual supplied scale is retained.

The uniform value bounds are `h A + h^2 R` for LST (h=1/nu) and `w B + w^2 R` for held-sd std. On a checked nuisance neighborhood, Gaussian loss is at least kappa p^2/2. If M bounds the coefficient derivative, completing the square gives the uniform profile bound `h c1 + h^2 (R + M^2/(2 kappa))`. Uniform nuisance score signs establish root containment for the entire tail. A strictly negative enclosed c1 supplies a finite cutoff rounded upwards. Zero or uncertain sign makes no new claim.

The remaining finite domain is covered completely: unique conditional scale roots are enclosed uniformly over shape intervals when location is held; full location/shape boxes cover every branch when scale is held. Strict score signs and likelihood upper bounds exclude boxes. Reachable lower faces are included or excluded by explicit uniform bounds, including the ordinary-scale std t2 face and the moving-location held-sd collapse region. Normal is certified only after every box clears. A cap or an unresolved comparison leaves the legacy decision available, uncertified. Stable inverse-shape scores and inherited scale-root boxes remove the artificial large-shape dependency that made the first cover too slow. Uniform midpoint/quarter-point bisection contracts inherited scale boxes that interval Newton alone cannot shrink. Scalar root refinement refreshes its curvature bound and accumulates scaled derivatives before applying the reduction margin. The positive remainder series has an explicit analytic tail; precision margins remain unchanged. The [before-refinement trace](2026-11-08-std-free-data/unresolved-trace-before-refinement.txt) records stalled inherited scale brackets about 10 log-scale units wide at nearly point-valued shape intervals. The [current trace](2026-11-08-std-free-data/unresolved-trace-current.txt) identifies remaining root-width floors and the coverage-budget case. For example, near nu=326 the included root width is 7.29e-8 versus a required 3.26e-8. [trace-unresolved.sh](2026-11-08-std-free-data/trace-unresolved.sh), with `UMVL_DIAGNOSTIC_BEFORE=1` for the archived version, reproduces the traces without editing production headers.

A negative c1 alone is insufficient. The following independently evaluated finite witnesses beat normal despite c1<0:

- **lst**: c1=-1.25001875; finite witness gain=18.9940333.
- **std**: c1=-1.42857143; finite witness gain=0.896285042.

LST uses three copies of each of ±.001 and two of each of ±1, held mu=0, witness nu=.2 and sigma=.001. Std uses 26 zeros and seven copies of each of ±1, held sd=sqrt(7/20), witness mean=0 and nu=3. Native regressions require refusal of the normal proof; high-precision gains are numerical checks, not uniqueness proofs. Zero-coefficient examples retain uncertified legacy fits. A strictly positive enclosed coefficient rules out normal, without proving a particular finite maximizer.

The bounds are certified in exact arithmetic conditional on computed comparisons, with the existing practical 64-epsilon interval and reduction margins. Outward rounding is not claimed. Certificate work is 128*min(iterlim,100000); inner solves have min(iterlim,100000) iterations. Unjustifiable sub-epsilon coefficient precision remains a refusal.

The normal-relative value enclosure also remains in place. On the earlier two-point near-normal example it shrinks a width of 7.58e-7 to 1.15e-12. Value domination alone rejects that numerical point; the new full signed/compact proof now certifies its normal face. The earlier numerical-stop observation is historical, not current public behavior.

### Proved nonuniqueness

**Std, held sd, free mean and shape.** For two equally occupied atoms with half-separation b > 15 held standard deviations, a centered symmetric unimodal variance-one density satisfies `f(b) ≤ 3/(2b³)`. Every centered Student fit, including its optimized normal limit, is therefore beaten by a shape-three witness at an atom. The lower-shape face vanishes uniformly, locations can be restricted to the sample range, and the normal limit is excluded. A finite off-center global maximizer exists; reflection supplies a distinct global maximizer.

For `[-100, 100]`, sd = 1, the required comparison reduces to the exact integer inequality `1000000000000 > 14400720009`. The returned shape-three points are actual likelihood witnesses, not asserted maximizers.

**LST, joint location/scale at held shape below one.** For `[-100, -99, 0, 99, 100]`, ν = .5, strict atom inequalities imply existence. The best centered scale is uniquely determined by `5a² + 39602a − 98010000 = 0`, where a = νσ² and `1979 < a < 1981`. The witness μ = 99.5, σ = .5 beats the entire centered profile. An exact rational comparison proves that the fourth power of its likelihood ratio exceeds 5. Reflection again proves at least two global maxima.

The implemented helper checks exact binary reflection, encloses the whole centered scale profile, and tests reflected finite witnesses. When that sufficient comparison closes, it returns a proved nonuniqueness stop. Otherwise it makes no global claim. The global likelihood upper bound remains infinity when no numerical global upper bound has been computed.

The exact comparisons are reproduced by [oracle.py](2026-11-08-std-free-data/oracle.py) and retained in [exact-comparisons.txt](2026-11-08-std-free-data/exact-comparisons.txt). The latter example disproves uniqueness under the strict atom condition at held subunit shape; existence alone is insufficient.

## Frozen golden samples

Independent 80-digit comparisons on the exact binary64 frozen inputs show substantial separation from the conditional normal face:

| Held coefficients | Std finite-witness gain over normal | LST finite-witness gain over normal |
|---|---:|---:|
| Location only | 34.190237 | 140.564391 |
| Physical scale only | 34.324684 | 460.047549 |
| Both | 34.327412 | 460.349154 |

For std with mean held and sd free, the optimized positive ordinary-scale t₂ face is also below the finite witness by **9.480632**. These are numerical face comparisons, not joint global certificates. The monotone conditional t₂ scale root and all input values are reproduced by [golden-faces.py](2026-11-08-std-free-data/golden-faces.py), with [80-digit results](2026-11-08-std-free-data/golden-faces.json).


All six held free-shape golden fits return `Ok` within the default 200 iterations. The standalone [native check](2026-11-08-std-free-data/golden-native-default.csv), reproduced by [golden-native.py](2026-11-08-std-free-data/golden-native.py), has maximum relative golden error 6.16e-6 against the unchanged 3e-5 tolerance; [golden-default.json](2026-11-08-std-free-data/golden-default.json) records coefficients, flags, errors and timings from [golden-default.R](2026-11-08-std-free-data/golden-default.R). Both scalar routes certify; the four general joint finite routes remain uncertified. Their missing proof is exhaustive finite joint root inclusion with coefficient enclosures, rather than a solver budget or a near-normal value enclosure. The new normal exclusion cover certifies a face, not finite maximizing coefficients.

## Accuracy evidence

[bounds.py](2026-11-08-std-free-data/bounds.py) passed **621/621** independent 80-digit checks of the enclosed formulas and normal-relative identity; [bounds.json](2026-11-08-std-free-data/bounds.json) records their numerical scope.

The independent scalar roots on exact binary64 frozen inputs are:

| Family | Shape | Conditional log likelihood |
|---|---:|---:|
| std | 3.977657952082522942249896734409 | −579.480782421475318178988871518 |
| lst | 3.024242713628303514240410197295 | −898.624781104657356037271792338 |

See [oracle.json](2026-11-08-std-free-data/oracle.json) for scores, curvature and further digits. Root-finding checks coefficients and likelihoods; exhaustive coverage and face inequalities provide globality. The conditional std root differs from the frozen approximate all-free shape by relative **1.023086432e-6**, within the existing **3e-5** golden tolerance. Holding approximate nuisance coefficients changes the exact conditional root. LST's relative difference is approximately 3.95e-9. No golden was regenerated or edited; final golden regressions are green.

The new signed inequalities passed **115/115** independent 80-digit checks. [Data: signed-bounds.json](2026-11-08-std-free-data/signed-bounds.json) records scope and finite-winner counterexamples.

The independent [seeded scalar oracle](2026-11-08-std-free-data/scalar-seeded-oracle.json) verifies a newly closed root at 80 digits: nu=18.4926708646815561, with relative error 3.31e-12 in the certified result. For a flat root near 326, the uncertified legacy coefficient differs by 2.86e-5 relative to the nearby high-precision stationary root. That numerical comparison makes no global claim; the flag does not promise coefficient accuracy.

## Seeded coverage

There are 32 seeds per source, n=64, two families and three hold patterns: **1344 fits on 448 recorded input samples**. Sources are normal, t3/t5/t10/t30, variance-one uniform and variance-one triangular. Std t inputs are standardized to variance one; LST uses ordinary t scale one. Location holds are zero and physical scale holds are one. Controls are tol=1e-10 and iterlim=200. This is descriptive coverage of these samples, not a population guarantee. Inputs and every outcome/time are retained in [seeded-samples.csv](2026-11-08-std-free-data/seeded-samples.csv) and [seeded-coverage.csv](2026-11-08-std-free-data/seeded-coverage.csv); the generator is [seeded-coverage.cpp](2026-11-08-std-free-data/seeded-coverage.cpp).

Each fraction below has denominator 32. F=certified finite, L=certified face, U=uncertified finite or face, S=stop. Stops are reported separately; unresolved certificate attempts followed by successful legacy fits count under U. Timings are median/p95 milliseconds on one workstation slot.

| Family | Source | Held | F | L | U | S | ms median/p95 |
|---|---|---|---:|---:|---:|---:|---:|
| lst | normal | location | 0.000 | 0.781 | 0.219 | 0.000 | 3.252/14.061 |
| lst | normal | scale | 0.000 | 0.750 | 0.250 | 0.000 | 4.508/79.336 |
| lst | normal | both | 0.250 | 0.719 | 0.031 | 0.000 | 1.486/33.392 |
| lst | t10 | location | 0.000 | 0.312 | 0.688 | 0.000 | 30.237/59.812 |
| lst | t10 | scale | 0.000 | 0.250 | 0.750 | 0.000 | 79.420/177.387 |
| lst | t10 | both | 0.656 | 0.219 | 0.125 | 0.000 | 15.292/109.795 |
| lst | t3 | location | 0.000 | 0.031 | 0.969 | 0.000 | 14.780/33.421 |
| lst | t3 | scale | 0.000 | 0.000 | 1.000 | 0.000 | 89.684/187.566 |
| lst | t3 | both | 1.000 | 0.000 | 0.000 | 0.000 | 2.565/28.006 |
| lst | t30 | location | 0.000 | 0.531 | 0.469 | 0.000 | 8.403/13.687 |
| lst | t30 | scale | 0.000 | 0.531 | 0.469 | 0.000 | 5.065/77.465 |
| lst | t30 | both | 0.469 | 0.531 | 0.000 | 0.000 | 2.388/18.507 |
| lst | t5 | location | 0.000 | 0.156 | 0.844 | 0.000 | 13.624/14.545 |
| lst | t5 | scale | 0.000 | 0.031 | 0.969 | 0.000 | 79.503/84.894 |
| lst | t5 | both | 0.938 | 0.031 | 0.031 | 0.000 | 2.575/8.965 |
| lst | triangular | location | 0.000 | 1.000 | 0.000 | 0.000 | 2.671/4.533 |
| lst | triangular | scale | 0.000 | 0.938 | 0.062 | 0.000 | 4.448/71.454 |
| lst | triangular | both | 0.062 | 0.906 | 0.031 | 0.000 | 1.383/27.230 |
| lst | uniform | location | 0.000 | 1.000 | 0.000 | 0.000 | 1.546/2.265 |
| lst | uniform | scale | 0.000 | 1.000 | 0.000 | 0.000 | 3.139/3.729 |
| lst | uniform | both | 0.000 | 1.000 | 0.000 | 0.000 | 1.229/1.389 |
| std | normal | location | 0.000 | 0.781 | 0.219 | 0.000 | 1.506/16.169 |
| std | normal | scale | 0.000 | 0.688 | 0.312 | 0.000 | 29.656/167.718 |
| std | normal | both | 0.188 | 0.688 | 0.125 | 0.000 | 13.966/845.743 |
| std | t10 | location | 0.000 | 0.312 | 0.688 | 0.000 | 21.424/36.025 |
| std | t10 | scale | 0.000 | 0.219 | 0.781 | 0.000 | 348.768/457.302 |
| std | t10 | both | 0.625 | 0.156 | 0.219 | 0.000 | 193.485/3448.880 |
| std | t3 | location | 0.000 | 0.031 | 0.969 | 0.000 | 17.033/36.309 |
| std | t3 | scale | 0.000 | 0.000 | 1.000 | 0.000 | 174.205/374.584 |
| std | t3 | both | 1.000 | 0.000 | 0.000 | 0.000 | 19.758/219.166 |
| std | t30 | location | 0.000 | 0.531 | 0.469 | 0.000 | 7.311/15.602 |
| std | t30 | scale | 0.000 | 0.375 | 0.625 | 0.000 | 153.363/162.668 |
| std | t30 | both | 0.406 | 0.375 | 0.219 | 0.000 | 29.196/333.370 |
| std | t5 | location | 0.000 | 0.156 | 0.844 | 0.000 | 15.593/16.698 |
| std | t5 | scale | 0.000 | 0.094 | 0.906 | 0.000 | 161.831/172.319 |
| std | t5 | both | 0.812 | 0.094 | 0.094 | 0.000 | 24.080/124.561 |
| std | triangular | location | 0.000 | 1.000 | 0.000 | 0.000 | 0.968/2.045 |
| std | triangular | scale | 0.000 | 0.906 | 0.094 | 0.000 | 16.197/150.145 |
| std | triangular | both | 0.031 | 0.938 | 0.031 | 0.000 | 4.772/77.203 |
| std | uniform | location | 0.000 | 1.000 | 0.000 | 0.000 | 0.426/0.686 |
| std | uniform | scale | 0.000 | 1.000 | 0.000 | 0.000 | 12.835/25.509 |
| std | uniform | both | 0.000 | 1.000 | 0.000 | 0.000 | 3.496/7.147 |

Across all 1344 fits, 206 certified finite maxima and 674 certified faces account for 65.48%; 462 finite fits and 2 faces remain uncertified. There were 0 stops. Among the 448 shape-only fits, 28 numerical refusals and one budget exhaustion used the uncertified fallback. The 7 initially unresolved LST normal covers now close; the 2 std lower-shape nominations remain uncertified. [uncertified-diagnostics.csv](2026-11-08-std-free-data/uncertified-diagnostics.csv) records the remaining cases.

The summary separates uncertified finite/face counts and stop statuses, and checks that no sampled uncertified normal decision has a strictly positive first-order coefficient. [Data: seeded-summary.json](2026-11-08-std-free-data/seeded-summary.json)

## Matched timings

Workstation GCC 15.2, `-O2 -std=c++17`, same BH include paths, one CPU slot, three repetitions. The baseline overlays only the two original std/lst fitter headers from the base revision. These shared-workstation measurements are numerical timing evidence, not accuracy evidence.

For normal mid-quantiles, controls are tol=1e-10 and iterlim=200. Values are median seconds [minimum, maximum]; held location=0, held physical scale=1.

| Family | n | Held | Before | After | Ratio |
|---|---:|---|---:|---:|---:|
| lst | 100 | location | 0.0181527 [0.0179163, 0.0185381] | 0.0728661 [0.0728386, 0.0729579] | 4.01× |
| lst | 100 | scale | 0.148888 [0.148534, 0.148933] | 0.128716 [0.127843, 0.131055] | 0.865× |
| lst | 100 | both | 0.000194465 [0.000191238, 0.000199609] | 0.0277587 [0.0277538, 0.0277676] | 143× |
| lst | 1000 | location | 0.176142 [0.176011, 0.178112] | 1.07573 [1.04429, 1.09447] | 6.11× |
| lst | 1000 | scale | 14.1304 [13.9413, 14.4083] | 2.16981 [0.259957, 2.20482] | 0.154× |
| lst | 1000 | both | 0.00194605 [0.0019409, 0.00194915] | 0.0546621 [0.0546616, 0.0551095] | 28.1× |
| std | 100 | location | 0.02062 [0.0206178, 0.0206501] | 0.0392108 [0.0390604, 0.0392445] | 1.9× |
| std | 100 | scale | 0.327724 [0.32737, 0.33408] | 0.83183 [0.82829, 0.834263] | 2.54× |
| std | 100 | both | 0.000227872 [0.000223556, 0.000230896] | 0.17363 [0.172211, 0.178499] | 762× |
| std | 1000 | location | 0.2024 [0.202208, 0.203771] | 0.042149 [0.0421176, 0.0422086] | 0.208× |
| std | 1000 | scale | 23.6083 [23.0767, 33.102] | 4.07351 [3.85025, 4.54818] | 0.173× |
| std | 1000 | both | 0.00242619 [0.00241846, 0.00244643] | 0.697003 [0.683998, 0.702096] | 287× |

Raw matched normal timings: [Data: normal-timing-before.csv](2026-11-08-std-free-data/normal-timing-before.csv), [Data: normal-timing-after.csv](2026-11-08-std-free-data/normal-timing-after.csv); reproducer [Data: normal-face-timing.cpp](2026-11-08-std-free-data/normal-face-timing.cpp).

For the original 500 t5 mid-quantiles, held location=0 and physical scale=1.3, tol=1e-8 and iterlim=200:

| Family | Free | Before | After | Ratio |
|---|---|---:|---:|---:|
| lst | nu | 0.00103118 [0.00100007, 0.00199428] | 0.0250619 [0.0250096, 0.0251059] | 24.3× |
| lst | sigma,nu | 0.0943101 [0.0930729, 0.204312] | 0.105215 [0.103769, 0.107046] | 1.12× |
| lst | mu,nu | 3.73404 [3.66912, 7.75002] | 4.20078 [4.19348, 4.27058] | 1.12× |
| std | nu | 0.0011877 [0.00112793, 0.00122011] | 0.179419 [0.163487, 1.64234] | 151× |
| std | sd,nu | 0.10698 [0.100887, 0.109421] | 0.115747 [0.109825, 0.126733] | 1.08× |
| std | mean,nu | 6.64088 [6.58258, 12.1388] | 7.33953 [7.29781, 7.68401] | 1.11× |

Scalar certification has a material cost relative to the former numerical search; the table reports it explicitly. The added normal certificate can avoid the much slower legacy location searches when its proof closes. Raw t5 timings: [Data: timing-before.csv](2026-11-08-std-free-data/timing-before.csv), [Data: timing-after.csv](2026-11-08-std-free-data/timing-after.csv); reproducer [Data: timing-final.cpp](2026-11-08-std-free-data/timing-final.cpp).

## Final validation

All current final commands finished green on the workstation, through separate `/tmp/umvl-cpu/run4` reservations, sequentially and in the foreground. BLAS/OpenMP were pinned to one thread and the wrappers used nice priority. Simbox was unavailable for this review; earlier GCC 13 runs are not evidence for this source. The workstation double-only build exercises GCC 15.2 explicitly. Elapsed times below exclude CPU-reservation waits.

| Command | Result | Elapsed seconds |
|---|---|---:|
| `just cpp-test` | 106/106 shards | 1418.38 |
| `just test` | 21924 pass; 0 fail/warn; 154 skip | 224.62 |
| `just test-full` | 44747 pass; 0 fail/warn; 0 skip | 924.78 |
| `UMVL_TEST_TIER=full just cpp-test` | 129/129 shards | 428.66 |
| `just cpp-test-no-long-double` | 129/129 shards | 404.90 |
| `just js-build` | Exit 0; 99 families | 74.59 |
| `just js-test` | 541/541 tests | 261.88 |
| `just site-check` | 99 pages/snippets; links, MathML and browser checks | 130.06 |
| `just relations` | 157 records; 0 failed | 167.95 |

Focused std/lst native and full-tier R checks also passed. Durable summaries: [Data: final-test-summaries.txt](2026-11-08-std-free-data/final-test-summaries.txt); machine/count/timing/audit record: [Data: validation.json](2026-11-08-std-free-data/validation.json). All **18** tested source hashes match [Data: source-sha256.json](2026-11-08-std-free-data/source-sha256.json). `git diff --check` passes. Settled dpqr/special/truth/codegen, default goldens, shared core headers and prohibited status/roadmap files are unchanged; the incidental site screenshot was restored.

## Open

1. **Remaining scalar precision/coverage cases**: the seeded diagnostics identify numerical root-enclosure refusals and budget exhaustion at the default controls. These retain explicitly uncertified legacy fits; tighter certified coefficient enclosures or additional coverage work are still needed.
2. **General finite free-shape joint certificates**, std {sd,nu}/{mean,nu} and lst {sigma,nu}/{mu,nu}: complete joint maximizing-root inclusion, branch completeness and coefficient enclosures. The signed normal-face proof now closes a sufficient class; zero/uncertain signs, occupied held locations excluded by the free-scale helper, unrepresentable lower cutoffs, critical lower faces and unresolved compact boxes retain the converged legacy finite/limit decision uncertified. Frozen joint goldens remain in this category.
3. **Generic joint location/scale at held 0<nu<1**: exhaustive nuisance-root isolation and critical collapse-face comparisons beyond the proved symmetry ties. Strict atom inequalities prove existence, not uniqueness.
4. **Settled density normalizer.** A full-suite iteration exposed large-shape cancellation through `lst/tails.hpp::logmode` → `special::lbeta`. For `[-1,2]`, μ=.2, σ=1.5307277909393948, ν=97432810384.283554, the recorded package density gave log likelihood −3.6877369241232691 versus the independent 80-digit value −3.6880288874065606: a spurious gain of **0.00029196328329145787**. A stable large-shape normalizer and corresponding truth cases are needed in a separate permitted change. The settled density/special/truth seams were left untouched. [density-cancellation.py](2026-11-08-std-free-data/density-cancellation.py), [its output](2026-11-08-std-free-data/density-cancellation.json), and [the original R observation](2026-11-08-std-free-data/oracle-diagnostic-before.txt) retain the evidence. This reproducer compares the recorded package value against a fresh high-precision evaluation; the recorded floating-point error is platform-specific.


The family-local oracle uses independent `stats::dt` with the original assertions and tolerances. This avoids the recorded settled-density cancellation artifact without altering that seam. All required final suites above are green; all work remains uncommitted.
