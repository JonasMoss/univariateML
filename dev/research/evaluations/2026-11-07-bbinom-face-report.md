**TASK-116.40: beta-binomial binomial-face decision**

Worktree `codex/bbinom-face`, based on `30d60e58a168463bbd0a01efc15d8490b0b01091`. Runs completed on 2026-11-08. No commits, board edits, package installs or publications were made.

The fixed-size, both-shapes-free fitter no longer declares nonexistence using a positive likelihood preference for the binomial. A binomial-limit stop now requires a signed bound near the face and strict exclusion of every remaining dispersion interval. Size-two overdispersed tables have a separate algebraic global fit. Unresolved comparisons and exhausted work retain valid witnesses; failed mean solves and non-finite final evaluations stop. Frozen goldens were not edited, and their C++, R and JS replays pass.

The dossier retains `fit.certified: partial`. The face nonexistence certificate and size-two algebraic fit are established below. The existing general finite-profile likelihood allowance does **not** establish the coefficient accuracy contract or uniqueness for every supported subset. That remaining requirement is recorded explicitly in open entry 1.

For the proof, let there be $h$ observations, known trial size $N\ge2$, $S=\sum x_i$, $Q=\sum x_i^2$, $p_0=S/(hN)$, and at least one interior count. Use $p=\alpha/(\alpha+\beta)$ and $s=1/(\alpha+\beta)$. Drop the data-only binomial coefficients temporarily and write

$$
\ell(p,s)=V(p,s)-D(s),\qquad
V=\sum_{j=0}^{N-1}\{A_j\log(p+js)+C_j\log(1-p+js)\},\quad
D=h\sum_{j=0}^{N-1}\log(1+js).
$$

Here $A_j=\#\{x_i>j\}$ and $C_j=\#\{x_i<N-j\}$. Let $L(s)=\max_p\ell(p,s)$ and $F=L(0)$. The existing [escape analysis](2026-10-31-open-existence-report.md) establishes that this sample's only positive-likelihood escape is the binomial face. Endpoint-only samples and sizes zero and one retain their separate handling.

Joint concavity of $V$ gives its tangent upper bound at $(p_0,0)$. Its $p$ slope there is zero. Since $-D''(s)\le B=h\sum j^2$, Taylor's inequality gives, **uniformly over all means**,

$$
L(s)-F\le g_0s+\frac12Bs^2,\qquad
B=\frac{h(N-1)N(2N-1)}6,\qquad
g_0=\frac{NhQ-(N-1)S^2-NhS}{2hNp_0(1-p_0)}.
$$

Thus $g_0<0$ excludes $0<s\le-g_0/B$ strictly. This does not assume that a negative boundary score decides the global comparison. Both ordinary and reflected moments are accumulated directly as positive sums; reflecting a large second moment by subtraction could lose the small statistic that controls this sign.

A second bound handles zero score and enlarges small negative-score strips. For a proposed rectangle $p\in[l,u]$, $0\le s\le t$, the lower score bound at $l$ and upper score bound at $u$ are

$$
\sum_j\left\{\frac{A_j}{l+jt}-\frac{C_j}{1-l}\right\},\qquad
\sum_j\left\{\frac{A_j}{u}-\frac{C_j}{1-u+jt}\right\}.
$$

If these are respectively positive and negative, strict concavity in $p$ puts every profiled mean inside the rectangle. Define

$$
a_j^- =\frac{A_j}{(u+jt)^2},\quad a_j^+=\frac{A_j}{l^2},\quad
c_j^-=\frac{C_j}{(1-l+jt)^2},\quad c_j^+=\frac{C_j}{(1-u)^2}.
$$

Then $M=\sum(a_j^-+c_j^-)>0$ bounds $-\ell_{pp}$ below,
$J=\sum j^2(h-a_j^--c_j^-)$ bounds $\ell_{ss}$ above, and
$K_-=\sum j(c_j^--a_j^+)$, $K_+=\sum j(c_j^+-a_j^-)$ enclose $\ell_{ps}$. Consequently

$$
L''(s)=\ell_{ss}+\frac{\ell_{ps}^2}{-\ell_{pp}}
\le C=J+\frac{\max(|K_-|,|K_+|)^2}{M}.
$$

A negative $C$ and nonpositive $g_0$ prove $L(s)-F\le g_0s+Cs^2/2<0$ throughout the positive strip. The implementation keeps the first-order strip if curvature cannot enlarge it. Rectangle refinement uses the solver cap, and its scans consume the enumeration-cell ledger.

Away from the proved strip, the existing tangent/chord inequality is used with a **strict** face comparison. At a segment midpoint $(p_m,m)$, concavity of $V$, a chord above the convex function $-D$, and $|p-p_m|\le1$ give

$$
\ell(p,s)\le\ell(p_m,m)+|\ell_p(p_m,m)|+
\max_{v\in\{a,b\}}\{V_s(p_m,m)(v-m)-D(v)+D(m)\}
$$

for every $s\in[a,b]$. Thus this remains an upper bound even when the mean solve has a residual score. Each remaining interval must have its upper bound below $F$ by the comparison margin. The existing dispersion-score tail bound excludes any maximizer beyond $hH_{N-1}/\#\{0<x_i<N\}$. Together, the strip, interval cover and tail prove $L(s)<F$ for every finite positive $s$. The supremum $F$ is approached at $(p_0,0)$ and is unattained at finite shapes. Only then does the fitter return `BinomialLimit` with the binomial witness and the binding advice to use `mlbinom`.

These are exact-arithmetic certificates conditional on computed comparisons, with practical floating-point margins, not outward-rounded intervals. The face-score numerator uses extended intermediates and a $64\epsilon_L$ absolute-sum margin. An exact zero is used only when the integer products and their sum fit that type's mantissa. Rectangle comparisons use a $1024\epsilon$ scale margin. The tangent/chord bound includes its existing $128\epsilon$ margin; strict face comparisons additionally use $1024\epsilon(1+|F|+hN)$. A finite candidate must exceed the face by that comparison margin. Failure to separate values never becomes a binomial-limit declaration. Reported witness likelihoods include the binomial coefficients.

For $N=2$, let the cell frequencies be $(n_0,n_1,n_2)$ and $\Delta=4n_0n_2-n_1^2$. When $n_1>0$ and $\Delta>0$, the empirical probabilities themselves are a beta-binomial law, with

$$
\alpha=\frac{(n_1+2n_2)n_1}{\Delta},\qquad
\beta=\frac{(2n_0+n_1)n_1}{\Delta}.
$$

They attain the saturated multinomial upper bound, proving global optimality. Mean and overdispersion identify both positive shapes, proving uniqueness in this case. The implementation checks the sign with a practical floating-point margin before using this route.

The [independent rational and 70-digit check](2026-11-07-bbinom-face-data/check.py) gives two counterexamples to the old face preference:

| Size | Cell frequencies | Exact shapes | Finite log-likelihood gain | Old preference |
|---|---|---|---|---|
| 2 | `(25001, 50000, 25001)` | $\alpha=\beta=25000$ | $0.0000199996000093$ | $0.00100002$ |
| 3 | `(12501, 37499, 37499, 12501)` | $\alpha=\beta=18749.5$ | $0.000106664770459$ | $0.001$ |

The symmetric size-three table also lies exactly on the beta-binomial surface, so its empirical likelihood is a global upper-bound witness independently of the fitter. It exercises the general profile, rather than the size-two algebraic route. The workstation native check obtains zero relative coefficient error in the first case and at most $2.515\times10^{-12}$ in the second. These are sample-specific accuracy checks, not a universal coefficient certificate. Exact arithmetic, likelihood gains and native results are retained in [the evidence directory](2026-11-07-bbinom-face-data/).

The near-critical table `(1000, 3001, 2999, 1000)` at size three has exact face-score numerator $-2$. Curvature now enlarges its tiny first-order strip enough to certify the binomial limit. The reflected data give the same likelihood and complementary probability. A separate ledger test exhausts the final charged cell of a finite fit: the reference uses 58,710 cells, and a cap of 58,709 returns a budget stop retaining both finite and binomial witnesses. Non-finite recomputations stop before returning an estimate or appending an invalid witness.

Global uniqueness fails in supported structural cases. At $N=1$, data `(0,1,1)` have the same maximum at every $(\alpha,\beta)=(2t,t)$, $t>0$. At $N=0$, all-zero data have likelihood one for every positive shape pair. Both are attained, unidentified fits, not nonexistence. The dossier's endpoint nonexistence statement now explicitly assumes $N\ge2$. No general finite-size uniqueness theorem is claimed.

The five-search literature check is recorded in [literature.json](2026-11-07-bbinom-face-data/literature.json). [Tarone (1979)](https://doi.org/10.1093/biomet/66.3.585) supplies background on local binomial-variance score tests against beta-binomial alternatives; its publisher abstract does not establish a global MLE criterion. [Smith (1983)](https://doi.org/10.2307/2347299) was identified as a relevant estimation paper, but its full text was not obtained and no theorem from it is used. No verified necessary-and-sufficient global condition was obtained within the search budget. This is not a claim that none exists. The implemented inequalities above are derived directly from the likelihood.

All final required commands finished green. Long suites used simbox; workstation runs used the CPU-slot wrappers, one-thread BLAS/OpenMP settings and worktree temporary directories. The whole no-long-double suites passed before the last bbinom-only refinements; the final-source bbinom shard was then rebuilt and rerun on both machines.

| Check | Machine | Result |
|---|---|---|
| C++ dev | Workstation; also simbox | 106/106 on each |
| C++ full, final source | Simbox | 129/129 |
| Whole no-long-double suite | Workstation; simbox | 129/129 on each |
| Final no-long-double bbinom shard | Workstation; simbox | 1/1 on each |
| R dev | Simbox | 21,519 passes; 0 failures/warnings; 154 tier skips |
| R full, final source | Simbox | 44,334 passes; 0 failures/warnings/skips |
| JS build and test | Workstation | Build green; 495/495 tests |
| Site check | Workstation | Green, including 99 C++17 example compilations |
| Independent/native checks | Workstation; native also simbox | Passed |

[Workstation summaries](2026-11-07-bbinom-face-data/workstation-suites.txt) and [simbox summaries](2026-11-07-bbinom-face-data/simbox-suites.txt) retain the run results. The final-source full C++ run took 197.59 seconds on simbox; final R full took 309.797 seconds. Site validation regenerated an unrelated tracked reference screenshot; it was restored byte-for-byte from the baseline. No shared header, density, truth table, code generator, golden, NEWS, status file or roadmap was changed. The shared JS test file was changed append-only.

Timing compares the baseline commit with the final fitter, using `-O2`, C++17, one pinned workstation CPU, and seven interleaved batches of 100 fits per case. Compilation is excluded. Raw trials and medians are in [timing.json](2026-11-07-bbinom-face-data/timing.json); samples are in [timing.cpp](2026-11-07-bbinom-face-data/timing.cpp).

| Case | Before, ms/fit | After, ms/fit |
|---|---:|---:|
| Finite fit, size ten | 0.252 | 0.237 |
| Negative-score face, size ten | 0.0991 | 0.0485 |
| Zero-score face, size two | 0.0297 | 0.0153 |

These small shared-workstation measurements do not establish a general speedup. Unpaired measurements varied substantially, so the table uses interleaved medians.

Open items:

1. **General finite-profile coefficient and mode certification.** Outside the algebraic size-two route and independently verified special examples, finite and held-shape profiles retain the existing $10^{-8}h$ global likelihood allowance. Even in exact arithmetic this does not exclude a better mode within that allowance or bound every coefficient by relative `tol`. Completing that guarantee needs enclosures of all relevant stationary maximizers, coefficient/root-width bounds and certified comparisons between overlapping candidate enclosures, or a suitable structural uniqueness theorem. Goldens and numerical oracles do not supply that proof. This is why the certified flag remains `partial`.
2. **Unresolved seams.** If the score sign cannot be established, curvature cannot close a neighbourhood, or finite/face comparisons overlap at practical precision, the fitter stops with a tie, unresolved, numerical or budget outcome and available witnesses. No higher-order expansion for degenerate curvature or general theorem resolving all such seams was established. The code does not convert those stops into nonexistence. A global necessary-and-sufficient existence condition and uniqueness for arbitrary $N\ge3$ also remain unproved here.

3. **Artifact-pane presentation.** The prescribed `show` command returned HTTP 403 because Nook's readable roots do not include this worktree. Display needs this working directory registered with the viewer, or an update to `CODEX_MATH_WEB_VIEW_ROOTS`. No viewer configuration or files outside the worktree were changed, and no extra agent was created. The report is saved at the requested repository path.

The fixed-size one-shape-free subsets do not use the binomial-face preference: their only escaping laws are endpoint point masses, and their existing compact shape bounds and likelihood certificates remain. The unsupported fit with size and both shapes free remains TASK-113 and was not implemented.

To reproduce the independent check and timings from the repository root, use the installed dependencies and the slot wrappers:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-bbinom-face-data/check.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-bbinom-face-data/build-timing.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-bbinom-face-data/timing.py
```

The retained [full-suite launcher](2026-11-07-bbinom-face-data/final-suites.sh) runs through `/tmp/umvl-cpu/simbox-run bash <script>`. The [final reflection verification](2026-11-07-bbinom-face-data/verify-moment-reflection.sh) repeats final-source full C++, the updated no-long-double family shard and full R. Their logs go under `build/scratch/`; the native witness checker is [native-check.cpp](2026-11-07-bbinom-face-data/native-check.cpp).
