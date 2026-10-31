# Fixed-size beta-binomial existence follow-up

31 October 2026. Source revision `3b597c0c868ec16118ee5df8d615691b08bf116e`. Scope: fixed integer size N, both positive shapes free, independent observations. **Result: necessity for N >= 3 remains open in this investigation.** No counterexample or complete uniform inequality was found. The fitter, dossier, tests and goldens are unchanged under the requested no-proof branch. In particular, its `1e-8*n` comparison remains a numerical convention and can misclassify a near-tie; it is not an exact decision. Nothing below establishes that an optimizer's numerical certificate is an exact-arithmetic global proof.

## Existing facts and the open step

Write `p=alpha/(alpha+beta)`, `s=1/(alpha+beta)`, `T=sum(x_i)`, `p0=T/(n*N)` and `S2=sum((x_i-T/n)^2)/n`. If a count is strictly between 0 and N, then `0<p0<1` and the only positive-likelihood escape is the binomial face. **Proved:** a finite MLE exists iff some `0<p<1, s>0` has `L(p,s)>=B`, the best binomial likelihood; a tie is sufficient for attainment. The polynomial criterion and compactness proof are in [the preceding report](2026-10-31-open-existence-report.md#1-beta-binomial-fixed-size-exact-algebraic-decision). The binomial coefficients cancel from comparisons.

**Proved:** the face derivative at `p0` is `n*(S2-N*p0*(1-p0))/(2*p0*(1-p0))`. Positive dispersion excess implies `L(p0,s)>B` for sufficiently small positive s and hence finite attainment. The sign is equivalently the exact integer sign of `N*n*sum(x_i^2) - N*T^2 - T*(n*N-T)`; this expression equals `n^2*N*(S2-N*p0*(1-p0))`. A nonpositive derivative alone gives no global conclusion: a profile can fall initially and rise later, and derivative zero does not determine its next term.

**Proved for N=2, including equality:** let `f0,f1,f2` be empirical frequencies, `u=f2+f1/2`, and represent a model probability vector by `q2=p^2+v`, `q1=2*p*(1-p)-2*v`, `q0=(1-p)^2+v`, where `v>0` for finite positive shapes. The feasible set of all probability vectors with `q2 >= (q2+q1/2)^2` is convex: it is a superlevel set of a concave function. At the binomial point `q*` with `p=u`, the directional derivative of empirical log likelihood in the binomial-mean direction is zero, and in the direction `(1,-2,1)` is `(f2-u^2)/(u^2*(1-u)^2)`. For any feasible q, the derivative towards q equals this second derivative times `q2-u^2-2*u*(p-u) = v+(p-u)^2`. Thus, if `S2 <= 2*u*(1-u)`, equivalently `f2<=u^2`, concavity bounds the likelihood at q by the face likelihood. If the inequality is strict, the bound is strict for `v>0`. If equality holds, the observed frequency vector is exactly `q*`; strictness for any distinct q follows from strict concavity of log (all three entries of q* are positive). Therefore every finite beta-binomial likelihood is strictly below B in both cases. Endpoint-only and point-mass exceptions are handled separately in the preceding report; N=0 and 1 are unidentified in the two-shape parameterization.

**Conjecture, N >= 3:** the same strict inequality holds whenever `S2 <= N*p0*(1-p0)` and an interior count occurs. Neither equality nor strict underdispersion is settled here. A valid counterexample needs an exact sample and finite p,s with likelihood at least B; a numerical candidate alone is not a certificate.

## A global reduction to a quadratic majorant

**Proved reduction.** Fix N, p0 in (0,1), p in (0,1), and s>0. For k=0,...,N define

```
d_k = sum_{j=0}^{k-1} log(p+j*s)
    + sum_{j=0}^{N-k-1} log(1-p+j*s)
    - sum_{j=0}^{N-1} log(1+j*s)
    - k*log(p0) - (N-k)*log(1-p0).
```

For empirical frequencies `w_k`, the average log likelihood ratio to B is `sum(w_k*d_k)`, provided their mean is `N*p0`. All real frequency vectors with this mean and variance at most `N*p0*(1-p0)` form the polytope

```
w_k >= 0;  sum w_k = 1;  sum k*w_k = N*p0;
sum k^2*w_k <= (N*p0)^2 + N*p0*(1-p0) = M2.
```

Its maximum ratio is a finite linear program. By linear-programming duality, that maximum is at most zero exactly when there exist real a,b and c>=0 satisfying

```
a + b*k + c*k^2 >= d_k  for all k=0,...,N,
a + b*N*p0 + c*M2 <= 0.
```

The polytope is nonempty (the Binomial(N,p0) frequencies satisfy equality), closed and bounded, and its linear objective is finite; standard finite LP strong duality applies. This gives a concrete sufficient-and-necessary condition for the **stronger fixed-triple, all-real-frequency** assertion. Proving such a majorant for every triple would prove the non-strict global inequality for samples, but would not alone exclude an attained tie. No construction uniform in the parameters was established. Conversely a positive LP value is only a numerical candidate: it must be converted to integer frequencies and rational p,s, then checked exactly, including the variance condition.

## Numerical counterexample search

**Numerical, not a proof:** in addition to the earlier 1,778 strictly underdispersed three-point integer samples described in the preceding report, we solved the LP above for 2,500 independently generated triples per N in `3,4,5,6,8,10,15,20,30,50,100` (27,500 attempted LPs). Generator: NumPy `default_rng(117)`; `p0` uniform on `[.01,.99)`, `p=clip(p0+Normal(0,.2),.001,.999)`, `s=exp(Uniform(-5,4))`. SciPy `linprog(method='highs')` optimized over all `N+1` count categories, rather than just endpoint-containing supports. At every size at least one LP succeeded; no successful solve exceeded `1e-7` in average log ratio. Largest reported values, by N in that order, were `-0.000103754`, `-0.000630968`, `-0.000589272`, `-0.000967550`, `-0.000540258`, `-0.001313620`, `-0.001096661`, `-0.002850142`, `-0.006130141`, `-0.011901681`, `-0.045325739`. Failed LPs, if any, were skipped and not counted separately; the run is not a certificate. Floating-point log-gamma differences, solver tolerances and the random sampling leave especially weak coverage close to s=0 and parameter boundaries. The sampled weights need not be rational; the LP permits equality in the variance constraint and samples without an interior observation.

Reproduction (SciPy and NumPy; one CPU slot; save as a script within the worktree):

```python
import numpy as np
from scipy.optimize import linprog
from scipy.special import gammaln
rng = np.random.default_rng(117)
for N in (3,4,5,6,8,10,15,20,30,50,100):
    x = np.arange(N+1); best = -1.; successes = 0
    for z in range(2500):
        p0 = rng.uniform(.01,.99)
        p = np.clip(p0+rng.normal(0,.2),.001,.999)
        s = np.exp(rng.uniform(-5,4)); a=p/s; b=(1-p)/s
        ll = (gammaln(a+x)-gammaln(a)+gammaln(b+N-x)-gammaln(b)
              -gammaln(a+b+N)+gammaln(a+b))
        d = ll - (x*np.log(p0)+(N-x)*np.log1p(-p0))
        r = linprog(-d, A_eq=[np.ones(N+1),x], b_eq=[1,N*p0],
                    A_ub=[x*x], b_ub=[N*p0*(1-p0)+(N*p0)**2],
                    bounds=(0,None), method='highs')
        if r.success:
            successes += 1; best=max(best,-r.fun)
            if -r.fun > 1e-7:
                print('candidate',N,p0,p,s,-r.fun,r.x,flush=True)
    print(N,successes,best,flush=True)
```

## Free-size implications (TASK-113)

**Argued:** for any candidate fixed N with an interior count, a proven exact fixed-size rule would classify its binomial face versus finite attainment, including ties. It would not order likelihoods across different sizes, bound the infinite integer tail, or settle the closure as N tends to infinity. Endpoint-only and point-mass faces also require comparison, and a finite fit tied with an unattained face is attained. The currently unresolved fixed-size rule supplies no authorization to discard any face. The existing dossier identifies negative-binomial and Poisson limits and the missing tail bound; this note does not prove a complete closure or tail inequality.

## References

- D. M. Smith (1983), “Maximum Likelihood Estimation of the Parameters of the Beta Binomial Distribution,” *Journal of the Royal Statistical Society: Series C* 32(2), 196–204, [doi:10.2307/2347299](https://doi.org/10.2307/2347299). Bibliographic identity checked against [publisher record](https://academic.oup.com/jrsssc/article/32/2/196/6985066); cited for the established MLE literature, not as proof of the present conjecture.
- D. A. Griffiths (1973), “Maximum likelihood estimation for the beta-binomial distribution and an application to the household distribution of the total number of cases of a disease,” *Biometrics* 29(4), 637–648; [PubMed record](https://pubmed.ncbi.nlm.nih.gov/4785230/). Cited as background, not as a claim that it resolves the conjecture.

## Validation

The calculations above use the stated revision; no source, test, golden, or generated package file was intentionally edited. Commands and test results are recorded below after the requested runs finish.

- Numerical scan: Python 3.14.5, NumPy 2.4.6, SciPy 1.18.0, seed and tolerances as above; `TMPDIR`, `JUST_TEMPDIR`, and `XDG_RUNTIME_DIR` set to `build/tmp`, BLAS/OpenMP pinned to one thread. Invoked with `/tmp/umvl-cpu/run nice -n 10 python3 build/scratch/bbinom_lp.py`. The self-contained listing above is the durable reproducer; `build/scratch` is ignored.
- `/tmp/umvl-cpu/run4 nice -n 10 just cpp-test`: **pass, 104/104**, GNU C++ 15.2.0, BH headers found through the recipe's fallback. The initial system Boost configuration failed as anticipated by that fallback; the build emitted compiler warnings in unrelated existing files.
- `/tmp/umvl-cpu/run4 nice -n 10 just test`: **pass**, testthat `[ FAIL 0 | WARN 0 | SKIP 151 | PASS 18887 ]`, R 4.6.1; skips are primarily full-tier cases, plus missing optional `sn`. During `regen`, roxygen reported `ml02_wrappers.R:3300: @details failed to evaluate inline markdown code` for the negative-hypergeometric generated default `` `r = 3` ``; the recipe continued and tests passed. This documentation-generation error is outside the owned files and was not fixed. Shell startup also reported a missing `.cargo/env`.
- Full tier and `just quality bbinom` were not run: the requested conditional final runs apply if the fitter changes; this investigation made no fitter, test or density changes. The suites validate existing software behavior, not the conjecture or the LP's global coverage.
- The artifact pane's `show` command rejected this worktree because it was absent from the viewer's configured readable roots (HTTP 403); the report remains available at its worktree path.
