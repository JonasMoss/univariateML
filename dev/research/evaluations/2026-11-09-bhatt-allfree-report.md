# TASK-116.58: Bhatt golden reference, global width certificate and search fallback

The golden fit now has an independent 50-digit reference and a closed global width certificate. A successful existing search survives an inconclusive certificate with per-fit **certified = false**. Certificate-budget exhaustion, unresolved face comparisons and failed coefficient enclosures do not themselves turn that successful search into a stop. The family advertises **fit.certified: 'partial'**.

All final validation runs completed green. Work remains uncommitted on **codex/bhatt-allfree**, based on **2ed5514b**; master was neither merged nor rebased. The requested report filename retains the job date; validation continued into 10 November.

## Independent golden reference

I read the reference tool and ran **just golden-reference bhatt**, followed by **just golden-reference --write bhatt**, through the CPU reservation wrapper. The tool refined the README density by damped Newton at 50 digits and wrote the golden itself.

| Quantity | Stored binary64 value |
|---|---:|
| mu | -0.0038000689215917497 |
| sigma | 1.0578973332904953 |
| a | 1.733605923867751 |
| log likelihood | -896.3104598272386 |

The recorded score norm is **4.54e-59**, with negative definite observed information. The metadata serializes 25 significant digits. Relative differences from the earlier independent 70-digit stationary point are **1.30e-25**, **2.20e-25**, and **1.18e-26** or less for mu, sigma and a. This comparison establishes agreement to the serialization precision.

Newton and negative information establish a local stationary maximum. Accordingly, the tool's **global: local only** record is retained. The C++ certificate supplies separate global evidence for this sample.

Parsed comparison with HEAD verifies that the frozen sample and every other pre-existing metadata field, apart from estimates and logLik, are unchanged. The reference block and formatting are the tool's output. No estimates were edited by hand. The tool's input fingerprint remains **00685f345b19642a6bd60577982d4753b2fd0e2b638be61bdec4d09e36dcd695**.

Evidence: [reference check](2026-11-09-bhatt-allfree-data/reference-check.txt), [write run](2026-11-09-bhatt-allfree-data/reference-write.txt), [70-digit comparison](2026-11-09-bhatt-allfree-data/reference-comparison.json), and [source/input integrity](2026-11-09-bhatt-allfree-data/source-integrity.json).

## Global width argument and acceptance

For positive width, put $k=a/\sigma$, $u=\mu/\sigma$ and $b=1/\sigma$. Then

$$
L(u,b,k)=n\log b-n\log(2k)+
\sum_i\log\{\Phi(bx_i-u+k)-\Phi(bx_i-u-k)\}.
$$

Normal interval mass is jointly log concave in its endpoints. Consequently, $L+n\log k$ is jointly concave in $(u,b,k)$. Profiling nuisance coordinates gives a concave adjusted profile $F(k)=P(k)+n\log k$. Holding physical location imposes $u=b\mu$, an affine constraint; holding sigma fixes b. Positive held a instead gives affine endpoints in $(u,b)$ and a concave held-width problem.

Enclosed nuisance roots provide global nuisance optima and supporting lines for F. The lower envelope of supporting lines, minus $n\log k$, bounds each width interval. Each linear piece minus $n\log k$ is convex, so its maximum occurs at a piece endpoint. Interval endpoints and supporting-line intersections therefore provide an exhaustive upper bound. This argument does not assume unimodality of P.

Subdivision excludes losing intervals. Negative definite Hessian boxes and uniform nuisance inclusions isolate winner regions. Certified finite acceptance requires coverage or exclusion of the remaining width domain, coefficient enclosures meeting the requested tolerance, and separation from both compatible faces and every surviving winner.

For the small-width end, Hoeffding's inequality, optimized over compatible normal coordinates, gives

$$
P(k)\le L_{N,\max}-\frac n2\log(1-k^2),\qquad 0<k<1.
$$

For the whole small-sigma neighbourhood, let r be the sample half-range when location is free, or the maximum distance from held location. For $a\ge r$, the peak bound gives likelihood at most $(2r)^{-n}$. For $a<r$, set $t=r/a>1$. Retaining one extreme observation's tail factor gives

$$
\prod_i f(x_i)\le (2r)^{-n}t^n Q(k(t-1)).
$$

The logarithmic derivative in t is $n/t-kR(k(t-1))<0$ for $k\ge2n$, since the inverse Mills ratio satisfies $R(v)\ge\sqrt{2/\pi}$ for $v\ge0$. This bounds the entire tail neighbourhood. An earlier cutoff K is available when the finite incumbent dominates the uniform face: $Q(v)\le e^{-v^2/2}/2$ and $\log t\le t-1$ bound the entire $k\ge K$ tail by

$$
L_U+\max\{0,n^2/(2K^2)-\log2\}.
$$

Diverging a or sigma loses likelihood by the density-height bounds. Escaping location with bounded scales also loses likelihood. For nonconstant data, joint scale collapse loses likelihood; the remaining nonzero faces are normal, attained at a = 0, and uniform, approached as sigma tends to zero. These bounds complete the domain decomposition used by the certificate.

The implementation follows the package standard: **certified in exact arithmetic, conditional on the computed comparisons, with a stated practical floating-point margin**. Bounds use padded binary64 comparisons, including 32-epsilon interval padding, 64-epsilon score/input allowances, 256-epsilon tangent/tail and RMS allowances, and 1024-epsilon physical likelihood allowances, with magnitude and interval-mass conditioning factors. These are floating-point qualifications to the mathematical arguments.

All numerical loop caps remain tied to min(iterlim, 100000): solver iterations use multiplier 1 and certificate evaluations multiplier 128. The changes add no fixed numerical-search cap.

### Golden enclosure

The final diagnostic records **601 certificate units, 151 profiles and one winner**. At tolerance 1e-10, its physical coefficient enclosures are:

| Coefficient | Lower | Upper |
|---|---:|---:|
| mu | -0.0038000689573057652 | -0.0038000688858777368 |
| sigma | 1.057897333258164 | 1.0578973333228245 |
| a | 1.7336059237618298 | 1.7336059239736763 |

They contain the independent reference and meet the documented tolerance, using sigma as the location floor. The winner region in k is **[1.6386767355680685, 1.6387791560646821]**; the root error diagnostic is **3.053e-11**.

After the common likelihood normalization, the winner lower bound is **-215.20445779080137**, the normal upper bound **-216.28040353584288**, and the uniform upper bound **-346.57359027978015**. Every other width interval is excluded or covered by the isolated winner region before acceptance.

Evidence: [diagnostic source](2026-11-09-bhatt-allfree-data/diagnostic.cpp), [launcher](2026-11-09-bhatt-allfree-data/diagnostic.sh), and [final diagnostic](2026-11-09-bhatt-allfree-data/final-diagnostic.txt).

## Proved normal domination with sigma held

There is a direct global normal-winner criterion with sigma fixed:

$$
\sum_i(x_i-\mu)^2\le n\sigma^2
$$

for held location, or the same inequality about the sample mean for free location.

Here is the argument. Let W be standard normal truncated to [-k,k], with variance v < 1, and let $C(k)=E_{U[-k,k]}e^{-U^2/2}$. Under exponential tilt t, take two independent copies of W and set $H=(W_1+W_2)/2$, $R=(W_1-W_2)/2$. Their joint density is proportional to $e^{-H^2-R^2+2tH}$ on $|R|\le k-|H|$. Conditional $E[R^2\mid |H|]$ decreases with $|H|$, whereas $E[H\mid |H|]=|H|\tanh(2t|H|)$ increases for positive t. Thus

$$
\frac{d}{dt}\operatorname{Var}_t(W)
=4\operatorname{Cov}_t(R^2,H)\le0,
\qquad
\log Ee^{zW}\le vz^2/2.
$$

Symmetry gives the same moment-generating-function bound for negative tilts. Also $-\log C(k)>v/2$ for k > 0. To see this, put w = k squared and let m(s) be the second moment under density proportional to $e^{-sU^2/2}$ on [-1,1]. Since $m'(s)=-\operatorname{Var}_s(U^2)/2<0$,

$$
-\log C(k)=\tfrac12\int_0^w m(s)\,ds>\tfrac12wm(w)=v/2.
$$

The density ratio therefore obeys

$$
\log\frac{f(x;\mu,\sigma,a)}{\phi_\sigma(x-\mu)}
\le\frac v2\{(x-\mu)^2/\sigma^2-1\}.
$$

Summation proves the held-location criterion. Completing the normal square gives, for free location,

$$
L(\mu,\sigma,a)-L_N(\bar x,\sigma)
\le\frac v2\{\operatorname{SSE}_{\bar x}/\sigma^2-n\}
-\frac{(1-v)n(\mu-\bar x)^2}{2\sigma^2}.
$$

The strict central-mass inequality gives the unique width winner a = 0 under the stated condition. The numerical RMS comparison requires separation from its padded upper bound. This closes the old held-mu, held-sigma = 4 example, and additional free-location cases, with certified = true.

### Remaining mathematical scope and uniqueness

Allowing sigma to decrease invalidates this fixed-scale comparison. For twenty zeros and observations -10 and 10, the unrestricted normal witness beats the uniform face by

$$
L_N-L_U=11\log\{22/(\pi e)\}>0.
$$

It is also a strict local maximum: with total variance $v_0=\sigma^2+a^2/3$ and $\eta=a/\sqrt{v_0}$, the fourth Hermite sum is 176 and the profiled expansion is $L=L_N-44\eta^4/45+O(\eta^6)$. Neither fact excludes every positive-width, free-sigma competitor. **Open:** an all-free normal-neighbourhood comparison sufficient to close this global certificate. The successful normal search is returned with a = 0 and certified = false.

Joint finite global uniqueness is not asserted for arbitrary samples. Nonunique supremizing centres do occur: with data {-1,0,1} and a = 2 held, every centre in (-1,1) approaches the same supremum -3 log 4 as sigma tends to zero. This is a closure nonuniqueness example; it is not a claim of multiple attained finite global maxima.

## Implementation and regression behavior

Routing is in the C++ fitter and binding-neutral Bhatt fit header. The R adapter transports the standard per-fit flag. C++ has a result bool; **FitResult.extra** carries **certified** as 0/1; JS exposes 0/1; R estimates have a logical **certified** attribute.

If certification is inconclusive, the wrapper runs the existing search and retains a successful result unchanged with certified = false. This includes the rounded seed-7, size-24 regression, default certificate-budget cases, and a requested tolerance of 1e-30 when the positive-held-width search succeeds. The generic tiny-tolerance bypass was removed. The actual normal delegate retains its own numerical/representability failure checks.

Stops remain for failed searches and refuted results. Two appended regressions demonstrate substantive refutations:

- With twenty observations at 1e16 and endpoints 1e16 - 10 and 1e16 + 12, recovering the normalized normal mean rounds it to 1e16. Keeping the normalized likelihood inflates the reported likelihood at the physical point by **11/1341**. Its physical mean score is **242/1341 > 1e-3**. The physical likelihood check rejects that result.
- With held a = 3.1 at a huge location, rounded uniform endpoints can encode half-width 4. The parent hold and reported limit model disagree, so the result is rejected. For unrestricted width and location, uniform endpoints instead retain the original sample extrema; a separate regression checks the correct width and likelihood.

An evaluated finite or compatible-face lower bound above the searched point's upper bound also refutes that point. Failed attempts retain structured physical witnesses. A small point-Newton correction relative to an existing valid enclosure ends unproductive refinement; certified finite acceptance still requires an accurate enclosure.

The original R and C++ P3 normal-member tests, the explicit a = 0 R assertion, and the entire original face-oracle test with its likelihood bound are restored unchanged. Integrity checks verify both named P3 blocks and the complete face-oracle block byte for byte. The inaccurate old-golden rejection fixture keeps its original numerical inputs as literals, so the golden refresh does not change that regression's input.

Two obsolete stop expectations in R/JS now test successful returns: the held-sigma example has a proved certified normal winner; the tiny-target held-width example retains an uncertified search. Existing iteration-failure and normal-delegate failure tests remain. No test was rewritten to accept a new stop. New C++ cases are appended.

See the [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/README.md) and [integrity audit](2026-11-09-bhatt-allfree-data/final-integrity.py).

## Seeded default-control census

The census uses sigma = 1, mu = 0 and a in {0.1, 0.5, 1, 3, 10}, plus standard normal and Uniform[-1,1] samples. Sizes are 10, 30, 100, 300 and 1000, with ten replicates per cell: **350 fits** at tol = 1e-8 and maxit = 500. Seeds are 11658000 + row; the retained C++ generator specifies GCC15/libstdc++ draws.

Here C/U/L means **certified finite / uncertified finite / uniform-limit stop**, with counts out of ten in each cell. Each count therefore represents a fraction with denominator ten.

| Sample model | n = 10 | n = 30 | n = 100 | n = 300 | n = 1000 |
|---|---:|---:|---:|---:|---:|
| Bhatt a = 0.1 | 0/0/10 | 3/5/2 | 5/5/0 | 4/6/0 | 3/7/0 |
| Bhatt a = 0.5 | 0/0/10 | 2/5/3 | 2/8/0 | 1/9/0 | 1/9/0 |
| Bhatt a = 1 | 0/0/10 | 2/3/5 | 5/5/0 | 7/3/0 | 5/5/0 |
| Bhatt a = 3 | 0/0/10 | 2/0/8 | 8/2/0 | 10/0/0 | 10/0/0 |
| Bhatt a = 10 | 0/0/10 | 0/0/10 | 1/3/6 | 2/7/1 | 0/10/0 |
| Normal | 0/0/10 | 0/5/5 | 3/7/0 | 6/4/0 | 2/8/0 |
| Uniform | 0/0/10 | 0/0/10 | 0/0/10 | 0/0/10 | 0/0/10 |

Overall there are **84 certified finite returns (24.00%)**, **116 uncertified finite returns (33.14%)**, and **150 uniform-limit stops (42.86%)**. Of the limit decisions, **139 are certified and 11 uncertified**. Counting finite and limit decisions by their flag gives **223/350 certified (63.71%)** and **127/350 uncertified (36.29%)**. There are **zero numerical, budget, unresolved or failed-search stops** in this census. The uncertified finite returns comprise **55 normal members and 61 positive-width fits**.

These are empirical frequencies on the specified seeds. The census binary precedes the final rejection-only physical guards and the tiny-target fallback correction. The ordinary-target optimizer/certificate algorithm is unchanged; the added fixed-sigma branch is not entered by these unrestricted calls. Regenerating every sample and replaying the final physical guards verifies **350/350** decisions without changes or likelihood discrepancies. Census wall times predate those final guards; the golden timing below uses final headers.

Evidence: [generator](2026-11-09-bhatt-allfree-data/census.cpp), [launcher](2026-11-09-bhatt-allfree-data/census.sh), [350 raw rows](2026-11-09-bhatt-allfree-data/review-census.csv), [summary JSON](2026-11-09-bhatt-allfree-data/review-census-summary.json), [35-cell summary CSV](2026-11-09-bhatt-allfree-data/review-census-summary.csv), and [final replay](2026-11-09-bhatt-allfree-data/guard-replay-summary.json).

## Timings and cost

The initial default-fit observation was **0.8415 s before** and **5.8655 s after** enabling the global route. The final paired benchmark runs the existing search and public certified fit in the same process, pinned to CPU 3, single-threaded through the slot wrapper.

| Repeat | Existing search, wall s | Certified fit, wall s | Search CPU s | Certified CPU s |
|---|---:|---:|---:|---:|
| 1 | 0.8225 | 4.3923 | 0.8221 | 4.3902 |
| 2 | 0.7979 | 4.4111 | 0.7978 | 4.4084 |
| 3 | 0.7921 | 4.4195 | 0.7917 | 4.4175 |
| Median | **0.7979** | **4.4111** | **0.7978** | **4.4084** |

The final paired median is about **5.53 times** the search cost, adding **3.61 CPU/wall seconds**. The diagnostic records 151 nuisance profiles and 601 certificate units: supporting-bound work, Newton/Hessian evaluations, winner-region isolation and coefficient enclosures supply the additional evidence. Development stack samples locate work in those Newton/Hessian routines; they are not an aggregate profiler. Successful inconclusive cases can also pay for the search twice, as proposer and fallback.

Unpinned final-source observations ranged from **4.38 to 18.39 s**. They are retained; pinning the final paired benchmark reduces scheduling variation. All times exclude CPU-slot waiting. The census excludes sample generation as well.

| n, 70 fits each | Median s | 90th percentile s | Maximum s |
|---|---:|---:|---:|
| 10 | 0.0154 | 0.0868 | 0.1351 |
| 30 | 0.0882 | 0.6907 | 5.4711 |
| 100 | 0.8323 | 7.0157 | 23.8812 |
| 300 | 2.9656 | 60.6672 | 123.5834 |
| 1000 | 64.8682 | 340.6509 | 1206.7561 |

The full census median is **0.7066 s**, its 90th percentile **72.5136 s**, and total fit wall time **10031.39 s**. The long tail is a material performance limitation, including inconclusive certificates. No fit was omitted, time-capped or censored.

Evidence: [initial before](2026-11-09-bhatt-allfree-data/before.csv), [initial after](2026-11-09-bhatt-allfree-data/after.csv), [final paired timing](2026-11-09-bhatt-allfree-data/final-timing.csv), [configuration](2026-11-09-bhatt-allfree-data/final-timing-config.json), [unpinned observations](2026-11-09-bhatt-allfree-data/final-timing-unpinned.csv), and [development stack](2026-11-09-bhatt-allfree-data/review-census-slow-stack-final.txt).

## Final validation and machine record

All final runs below completed on the **workstation, GCC 15.2.0**, with R 4.6.1 and Node 22.22.1 for the test runner. The JS build used its Emscripten toolchain. The R/C++/JS tier commands and site check each ran as a separate foreground command through **/tmp/umvl-cpu/run4**; actual worker affinity was limited to three CPUs. The native preflight, quality check and targeted research computations used the single-slot wrapper. BLAS/OpenMP were pinned and runs used the wrapper's nice priority.

| Check | Result | Log |
|---|---|---|
| Native Bhatt preflight | 44 cases, 5044 assertions passed | [native](2026-11-09-bhatt-allfree-data/final-preflight.txt) |
| Targeted R, full-tier Bhatt file | PASS 313; FAIL/WARN/SKIP 0 | [R Bhatt](2026-11-09-bhatt-allfree-data/review-r-targeted.txt) |
| just test | PASS 22156; FAIL/WARN 0; SKIP 154 | [R dev](2026-11-09-bhatt-allfree-data/review-r-dev-final.txt) |
| just test-full | PASS 44968; FAIL/WARN/SKIP 0 | [R full](2026-11-09-bhatt-allfree-data/review-r-full-final.txt) |
| just cpp-test | 106/106 | [C++ dev](2026-11-09-bhatt-allfree-data/final-cpp-dev.txt) |
| Full-tier just cpp-test | 129/129 | [C++ full](2026-11-09-bhatt-allfree-data/final-cpp-full.txt) |
| just cpp-test-no-long-double | 129/129 | [no-long-double](2026-11-09-bhatt-allfree-data/final-cpp-no-long-double.txt) |
| just js-build | completed successfully | [JS build](2026-11-09-bhatt-allfree-data/final-js-build.txt) |
| just js-test | 555/555; no failures or skips | [JS tests](2026-11-09-bhatt-allfree-data/final-js-test.txt) |
| just site-check | 99 C++ snippets and HTML checks passed | [site](2026-11-09-bhatt-allfree-data/final-site-check.txt) |
| just quality bhatt | pass | [quality](2026-11-09-bhatt-allfree-data/review-quality.txt) |
| Census and final guard replay | 350 rows, zero mismatches | [replay](2026-11-09-bhatt-allfree-data/guard-replay.csv) |
| Source/input integrity and diff check | pass | [integrity](2026-11-09-bhatt-allfree-data/final-integrity.txt) |

Simbox first returned filesystem I/O errors and later became unreachable. Final suites therefore used the authorized local fallback. A historical pre-review GCC13 no-long-double pass is retained in the data directory; it is not claimed as validation of the final revision. Evidence: [I/O failure](2026-11-09-bhatt-allfree-data/review-simbox-unavailable.txt), [later health check](2026-11-09-bhatt-allfree-data/review-simbox-final-health.txt), and [final run summary](2026-11-09-bhatt-allfree-data/validation-summary.json). Earlier development logs and pre-precision-fallback R runs are historical; the table names the final logs.

Changes are confined to Bhatt, its bindings/tests, and this report/evidence directory. No shared core header, special function, d/p/q/r, truth fixture, code-generator source, protected status/news/roadmap file, or other worktree was changed. The site check's incidental tracked screenshot was restored to its original bytes. The [final integrity record](2026-11-09-bhatt-allfree-data/source-integrity.json) fingerprints all ten changed tracked source files and verifies the frozen input and restored tests.
