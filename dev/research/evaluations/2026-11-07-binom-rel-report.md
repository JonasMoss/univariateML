# Binomial relatives: finite-winner certificates (TASK-116.28)

J7a is implemented for **ztbinom, zmbinom, tbinom and zib**. A numerical size proposal can now succeed only after a retained-support bound excludes the entire remaining size tail and evaluated points or bounded blocks cover every smaller feasible integer. Exact reductions bypass this search. Unresolved infinite-size comparisons stop rather than asserting non-existence or returning the numerical proposal.

The four `fit.certified` fields remain `partial`: signed tail and winning-face/tie decisions belong to J7b. Their `fit.note` fields now describe the finite-winner certificates and this remaining scope.

The base is `7c52f905f21bee7d997d59211d4093e2593ec727`, branch `codex/binom-rel`. The final source fingerprint is `02373121061f90606b09d92e8e5e132e19c6a8be7232bf7238bb05697d5f7101`; individual hashes and machine results are retained in [source-sha256.txt](2026-11-07-binom-rel-data/source-sha256.txt) and [validation.json](2026-11-07-binom-rel-data/validation.json). No commits were made.

| Family | Targeted free subset | Result |
|---|---|---|
| ztbinom | size and probability | Mandatory finite-winner certificate; unresolved positive-Poisson comparisons |
| zmbinom | size and probability, with zero probability held or fitted | Positive-count certificate plus the zero-indicator likelihood |
| tbinom | size and probability, with any subset of the two bounds free | Mandatory certificate for finite and infinite upper windows; sizes below the upper endpoint are covered |
| zib | size and probability, with inflation held or fitted | Constrained fixed-size profiles, retained-support bounds and complete integer coverage; unresolved ZIP comparisons |

Existing held-probability integer-tail bounds and fixed-size reductions remain in use. Probability solves additionally reject unresolved tiny probabilities. Public fitter signatures and protected golden files are unchanged.

The implementation is in [ztbinom/certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/ztbinom/certificate.hpp), called by the four family fitters. The previous grids and refinements are private proposal routines. Their stabilization rules and local peak refinements supply no global-search premise.

For a conditioning support with lower integer endpoint \(L\), retain \(L,\ldots,K\), where \(K\ge\max_i x_i\) lies within the original window. For \(N\ge K\), put

$$
c_N(k)=\sum_{j=0}^{k-1}\log(1-j/N),\qquad
\lambda=\frac{Np}{1-p}.
$$

The binomial kernel, after removing factors common to its conditioning support, is \(\lambda^k e^{c_N(k)}/k!\). Since \(c_N(k)\ge c_N(K)\) for retained \(k\), its full normalizer is at least \(e^{c_N(K)}\sum_{k=L}^K\lambda^k/k!\). Consequently,

$$
\ell_N(\lambda)\le T_{L:K}(\lambda)+D_{N,K},\qquad
D_{N,K}=\sum_i[c_N(x_i)-c_N(K)].
$$

Here \(T_{L:K}\) is the retained finite-support Poisson log likelihood on the original observations. Each term of \(D_{N,K}\) is a sum of \(-\log(1-j/N)\), so it is nonnegative, decreases with \(N\), and tends to zero. An upper bound for \(\sup T_{L:K}\), plus the correction at a cutoff, therefore bounds **every** larger size. Increasing \(K\) tightens the bound for infinite original windows as well as finite ones.

The retained conditional likelihood is concave in log rate. Its derivative is the count sum minus sample size times the retained mean; its second derivative is minus sample size times the retained variance. Mean bracketing encloses its maximum, and tangent bounds enclose each subdivided interval.

For ZIB, retain \(0,\ldots,K\) and write its finite Poisson normalizer as \(Z_K\). The base zero mass is at most \(e^{-c_N(K)}/Z_K\). Because \(e^{-c_N(K)}\ge1\), the inflated zero mass is at most

$$
e^{-c_N(K)}[\pi+(1-\pi)/Z_K].
$$

Positive masses have their corresponding \(e^{c_N(x)-c_N(K)}\) bound. The same total correction \(D_{N,K}\) applies with inflation held or maximized.

The retained ZIB likelihood factors into a positive conditional likelihood and a Bernoulli zero-indicator likelihood. The former has a concave tangent bound. On a log-rate interval \([a,b]\), the latter is maximized over the attainable zero-mass interval

$$
[\pi+(1-\pi)/Z_K(b),\ \pi+(1-\pi)/Z_K(a)]
$$

for held inflation, or \([1/Z_K(b),1]\) for free inflation. Its maximizer is the empirical zero fraction clamped to that interval. This preserves the nonnegative-inflation constraint. Every rate maximum lies where the base retained mean is between the full and positive sample means: the score is \(S-\mu n_{\rm eff}\), with \(n_+\le n_{\rm eff}\le n_{\rm obs}\). These brackets and subdivision bounds cover all retained-profile branches.

The finite prefix uses a second bound. For an integer block \(A\le N\le B\), define

$$
\delta_N(k)=c_N(k)-c_A(k),\quad
d_N(j)=\delta_N(j+1)-\delta_N(j),\quad
r_N(k)=\delta_N(k)-\delta_N(h)-(k-h)d_N(h),
$$

with an integer \(h<A\). The increments \(d_N(j)\) increase with \(j\). Also \(\partial_N d_N(j)=j/[N(N-j)]\) increases with \(j\). Summing the differences on either side of \(h\) proves

$$
0\le r_N(k)\le r_B(k)\qquad(A\le N\le B).
$$

The subtracted affine term is absorbed into log rate and a common normalizing factor. The support at \(A\) is a subset of each later support. Hence

$$
\sup_{A\le N\le B,\,p}\ell_N(p)
\le \sup_p\ell_A(p)+\sum_i r_B(x_i).
$$

This remains valid when conditioning support changes below a finite upper endpoint. For ZIB, \(r_B(0)\ge0\) also bounds the structural-zero term, so the same inequality holds with inflation held or fitted. Blocks that cannot be excluded are bisected to singleton sizes. Cached fixed-size profiles avoid reevaluating their left endpoints. Neither integer unimodality nor the proposal grid is assumed.

Once the tail is excluded, this block coverage proves that the returned finite winner maximizes over all feasible sizes. If the incumbent improves during coverage, the existing tail exclusion remains valid. The first implementation attempted exhaustive prefix profiling; it exhausted the default budget on the zmbinom golden sample. The convex-correction bound supplies the missing efficient coverage.

For zmbinom the additive factor is \(J=n_0\log p_0+n_+\log(1-p_0)\). The positive-data ztbinom result, candidates and likelihood bounds are transported with this factor. Finite candidates append `p0`; positive-Poisson candidates become zero-modified Poisson candidates with the same `p0`. No binding refits a face.

Fixed-size conditional probability profiles retain their exponential-family mean solves. A successful ztbinom solve must pass the total log-odds score check \(|S-n_{\rm obs}E[X]|<10^{-7}\). ZIB validates its positive-mean root before deciding whether inflation is feasible; otherwise a poorly resolved root can choose the wrong constrained branch.

For held inflation, ZIB enumerates probability roots between the full and positive mean bounds, partitioned at the turning-point quadratic. That quadratic is now formed in \(u=Np\):

$$
n_{\rm obs}n_+u^2-
[S(n_{\rm obs}+n_+)+Sn_0/N]u+
S^2+Sn_0.
$$

This avoids losing its linear coefficient by differencing polynomial values at probabilities zero, one half and one. Discriminants within \(4096\epsilon(1+b^2+4ac)\) of zero stop numerically. Root brackets must resolve relative to probability. The zero-mass score uses `exp(N*log1p(-p))`; forming `1-p` first can change the score when it rounds to one. With no zeros, free inflation is zero and held inflation adds a constant, giving the ordinary-binomial mean fit.

The [reproducer](2026-11-07-binom-rel-data/check.cpp) shows the consequence on `x = 0:4`, held size \(10^{100}\), inflation \(0.2\). The old rounded-zero-mass score selects \(Np=2.0833333333333335\), with log likelihood \(-8.7544446539693883\). The stable profile gives \(Np=2.3371516880031913\), log likelihood \(-8.6975323585688784\), agreeing with the independently fitted held-inflation ZIP face to displayed likelihood precision. This is numerical comparison evidence for a proper finite held size, not a proof that an infinite-size face wins.

The certificate is valid **in exact arithmetic conditional on the computed comparisons**, with practical floating-point margins. Tail and block comparisons use \(10^{-9}n_{\rm obs}+4096\epsilon(1+|\ell_{\rm best}|)\); retained interval bounds additionally use \(4096\epsilon(1+|U_\ell|+n_{\rm obs}K)\). Here \(U_\ell\) is the retained interval likelihood bound. These are not outward-rounded interval calculations or an unconditional floating-point error proof. Golden regressions and independent comparisons validate accuracy on the recorded workloads.

Work caps use the [work-budget units](../../design/work-budgets.md): certificate subdivision and coverage use \(128\min(\mathrm{iterlim},100000)\) evaluations, and charged enumeration work uses \(100000\min(\mathrm{iterlim},100000)\) cells. Solver limits remain separate. Tbinom retains its existing API without `iterlim`, using the documented no-control exception: 65,536 certificate evaluations and 51,200,000 cells. Arithmetic/representation guards stop numerically; they never become capped estimates. Exhausted certificates retain the finite incumbent and an infinite global supremum upper bound.

Exact delegates retain their proved limit decisions. The remaining size-search face comparisons retain a finite witness and the fitted comparison face, with primary kind `unresolved`. R and JS both report these stops with kind and primary class `unresolved`; the review removed an R-only extra `univariateML_limit` class and updated the shared binom R assertion for the ztbinom example accordingly. Four appended R tests check the class.

Uniqueness is not asserted jointly. Exact counterexamples include:

- **ztbinom:** size one with probability \(0.2\) or \(0.8\) gives the same point mass at one; all-one data maximize both.
- **zmbinom:** all-zero data with `p0 = 1` leave both base coefficients unidentified.
- **zib:** for data \((0,1,1)\) and size one, \((p,\pi)=(2/3,0)\) and \((1,1/3)\) both give the saturated probability \(P(X=1)=2/3\).
- **tbinom:** on \(0<X\le2\), data \((1,2)\) attain the saturated likelihood for every integer \(N\ge2\) with \(p=2/(N+1)\), since \(P(2)/P(1)=(N-1)p/[2(1-p)]=1\). Real bounds giving the same integer window are also equivalent.

All final acceptance runs finished green. The final C++ full run includes all 106 development shards; an earlier explicit development run also passed 106/106. Workstation runs used the prescribed slot helpers; simbox runs used `simbox-run`. Jobs used nice priority 10 and one BLAS/OpenMP thread per worker.

| Final check | Machine/compiler | Result |
|---|---|---|
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13.3 | 129/129; 203.20 s |
| `just cpp-test-no-long-double` | simbox, GCC 13.3 | 129/129; 178.98 s |
| `just cpp-test-no-long-double` | workstation, GCC 15.2 | 129/129; 253.52 s |
| `just test` | simbox, R 4.6.1 / GCC 15.2 | FAIL 0, WARN 0, SKIP 154, PASS 21,279; 100.6 s |
| `just test-full` | simbox, R 4.6.1 / GCC 15.2 | FAIL 0, WARN 0, SKIP 0, PASS 44,097; 306.1 s |
| `just js-build` and `just js-test` | workstation, Emscripten | 471/471, no skips; tests 148.42 s |
| `just site-check` | workstation | 99 family pages, MathML and 99 compiled C++17 examples |

Both machines used Boost 1.90 headers from installed BH. Existing compiler warnings in the untouched binom/lst headers remain. The R warning counts above are zero. The [final check extracts](2026-11-07-binom-rel-data/final-checks.txt) preserve the summaries.

The four protected golden files are byte-identical to the base. Their family C++ coefficient and likelihood checks pass at relative tolerance \(10^{-10}\). All family test additions and shared JS test additions are append-only; the shared binom R test is byte-identical to the base. No core, special-function, d/p/q/r, truth, code-generation, NEWS, status or roadmap sources changed. Site-check's regenerated historical screenshot was restored.

The [70-digit independent check](2026-11-07-binom-rel-data/check_bound.py) passed all 162 retained-support inequalities, including zero inflation; [bound.json](2026-11-07-binom-rel-data/bound.json) records the minimum slack. This is numerical evidence, separate from the exact-arithmetic proofs above. Appended C++ tests also compare block bounds against integer profiles, exercise finite/infinite/very large windows, validate stop witnesses and cover large-held-size probability failures and reductions.

The expensive tbinom case of 100 observations equal to 100 and one zero, with bounds \((-1,2000]\), certifies size **3350**, log likelihood **−421.73150333619617**. Its tail cutoff is **423,624,704**. Complete prefix coverage takes **33,393,020 charged cells**, within the 51,200,000-cell budget. [finite-certificates.txt](2026-11-07-binom-rel-data/finite-certificates.txt) records this and the smaller finite-winner cases. This replaces the old optional pass's unchecked-success fallback.

Before/after timings are means of ten fits of the deterministic small samples in [timing.cpp](2026-11-07-binom-rel-data/timing.cpp), compiled with workstation GCC 15.2, C++17 and `-O2`, under one slot at nice priority 10. The baseline used unmodified base headers; their hashes are retained. These small-workload measurements do not establish large-sample throughput.

| Family | Before, ms/fit | After, ms/fit |
|---|---:|---:|
| ztbinom | 0.845360 | 0.870863 |
| zmbinom | 0.843437 | 0.497242 |
| zib | 0.757360 | 0.419243 |
| tbinom | 13.685300 | 2.156380 |

**Open — J7b:** the unsigned retained-support correction cannot establish that a Poisson/positive-Poisson/ZIP face wins or resolves a finite-versus-face tie. A signed tail argument, uniform over the relevant profiled nuisance parameters, is still needed. Current stops explicitly report the unresolved comparison; no finite-size existence criterion, integer unimodality theorem or general uniqueness theorem is claimed. This is the remaining scope identified in the [triage route](2026-11-07-partial-triage-report.md), rather than a fallback to numerical successful estimates.

