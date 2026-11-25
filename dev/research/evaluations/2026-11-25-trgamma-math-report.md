# Transformed gamma and inverse transformed gamma likelihoods

TASK-354 · 2026-11-25 · Base `f22e71d9`

**Proved.** Every supported hold mask has an exact existence criterion. With scale held and both shapes free, every bounded case attains its maximum. The geometric-mean slice has a unique maximum, and the dyadic sample `(2^-1000, 1/2)` at scale one has two separated finite local maxima. With all coefficients free, the profile has finitely many stationary points on every nonconstant sample. There is an exact real sample with an attained global maximum equal in likelihood to the excluded power face. Reciprocation gives the corresponding Pareto equality.

**Numerical.** The archived successful fits on both golden samples meet the default free-coefficient tolerance against independent roots. A source-level replay at held power \(2^{-40}\) finds relative scale error \(1.60\times10^{-5}\) despite passing the represented-likelihood guard.

## Coordinates and prior results

Write \(k,p,a\) for gamma shape, power and public scale. Let \(y_i=\epsilon\log x_i\) and \(b=\epsilon\log a\), with \(\epsilon=1\) for trgamma and \(-1\) for invtrgamma. On the oriented log axis the per-observation log likelihood is
\[
 \log p-\log\Gamma(k)+kp(\bar y-b)-\overline{e^{p(y-b)}} .
\]
Subtract \(\overline{\log x}\) for the physical likelihood in either orientation. Reciprocal samples and reciprocal held scales preserve all likelihood comparisons, and the public scales of corresponding fits are reciprocal. All claims below concern nonempty samples of positive finite observations and valid positive finite holds.

The [October profile and closure derivation](2026-10-29-trgamma-derivation.md) exhausts the proper nondegenerate limits and gives the exact nuisance elimination. The [November certificate report](2026-11-07-trgamma-seam-report.md) proves the uniform refitted normal bound, compact secant covers and the held-endpoint approach from above. It supersedes the October discussion of seam stopping. The [outer-profile report](2026-11-25-gbshape-math-report.md#generalized-gamma) proves two local maxima on an exact free-scale sample and nonattainment on balanced two-point data. These are also results for the positive Stacy side itself.

## Existence on all eight masks

**Proved.** For held scale use \(z_i=y_i-b\). A common value means equality of the raw positive observations. The constant-side tests use exact comparisons of the common observation with the held scale, reversed for invtrgamma.

| Held | Exact condition and likelihood outcome |
|---|---|
| None | Constant gives unbounded likelihood. Otherwise the finite stationary-point and face comparison below decides attainment, including equality. |
| \(k\) | Nonconstant gives a unique finite fit. Constant gives unbounded likelihood. |
| \(p\) | Nonconstant gives a unique finite fit. Constant gives unbounded likelihood. |
| \(k,p\) | Unique finite scale on every sample, including singletons. |
| \(a\) | Unbounded exactly when all \(z_i\) are equal and the common value is nonnegative. In every other case at least one finite maximizer exists. |
| \(a,k\) | Unbounded exactly when all \(z_i=0\). Otherwise a unique finite power exists, including constant off-endpoint data. |
| \(a,p\) | Unique finite shape from \(\psi(k)=p\bar z\), including all observations at the endpoint. |
| \(a,k,p\) | The given parameter point is the sole feasible point and has a finite exact log likelihood. |

The one-free-coordinate results follow from the closed-form scale, the strictly increasing digamma, and the power curvature \(-1/p^2-\overline{z^2e^{pz}}\). The held-shape power score is positive near zero. If \(\max z>0\) its exponential term drives it negative, and otherwise it tends to \(k\bar z<0\) unless all \(z=0\). At fixed shape and free scale the standardized profile has curvature \(-1/t^2-kK''(t)<0\). Its score runs from positive to negative on nonconstant data. The fixed-power gamma reduction gives the other unique two-free-coordinate fit. At free scale, constant data can put the latent argument at \(k\). The likelihood then grows as \(\log p\) at held shape, or as \(\tfrac12\log k\) at held power. At held scale with common \(z>0\), choose \(k=e^{pz}\) and let \(p\) grow. At common \(z=0\), any fixed \(k>0\) suffices. The bounded held-scale cases follow from the trace and tail comparisons below.

## Free scale and both shapes free

For nonconstant data let \(s^2=\overline{(y-\bar y)^2}\), \(u_i=(y_i-\bar y)/s\), \(M=\max u_i\), \(t=ps\), and \(K(t)=\log\overline{e^{tu}}\). For comparison in standardized log units define
\[
 L(t)=\log t+k\log k-k-\log\Gamma(k)-kK(t),\quad
 f(k)=\log k-\psi(k)=K(t).
\]
The nuisance optimum is unique and \(b=\bar y+[K(t)-\log k]/p\). The endpoints have optimized values \(N=-\tfrac12[1+\log(2\pi)]\) and \(P=-1-\log M\). The normal uses oriented location \(\bar y\) and scale \(s\). For the power face, the fitted endpoint is the raw maximum in trgamma, and the fitted rate is \(1/(sM)\). In invtrgamma the endpoint is the raw minimum and the law is Pareto with the same oriented rate. The larger face is determined by \(M\) relative to \(\sqrt{2\pi/e}\), with equality when these coincide. Common additive terms, including \(-\log s\), cancel from all these comparisons.

### Signed exteriors and finite root count

**Proved.** Define \(g(w)=f(1/w)\) and \(w=tK'(t)>0\). Then
\[
 \operatorname{sgn} L'(t)=\operatorname{sgn}\{K(t)-g(tK'(t))\}.
\]
Indeed \(g\) is strictly increasing, and \(tL'=1-kw\). With \(\kappa_3=\overline{u^3}\) and \(\mu_4=\overline{u^4}\), the Taylor series of \(K\) and the signed Stirling expansion \(g(w)=w/2+w^2/12+O(w^4)\) give
\[
 K-g(w)=-\frac{\kappa_3}{12}t^3
       +\frac{1-\mu_4}{24}t^4+O(t^5).
\]
Thus the near-normal score is positive at negative log skewness, negative at positive log skewness, and negative at zero skewness with \(\mu_4>1\). Jensen gives \(\mu_4\ge1\), with equality only when every \(u_i^2=1\). Centering then forces balanced two-point data. In that case \(K=\log\cosh t\) and \(w=t\tanh t\). Their sixth-order coefficients in \(K\) and \(g(w)\) are \(1/45\) and \(1/90\), respectively, and the lower coefficients agree. The score is strictly positive near zero. The balanced case also has a direct global dispatch to power from the prior proof.

For a bounded signed seam, use Taylor enclosures of \(K,K'\) on a shrinking interval and the digamma remainder
\[
 w/2+w^2/12-w^4/120 < g(w) < w/2+w^2/12.
\]
The remainder for the exponential uses \(\max|u_i|\), and the logarithm and reciprocal of its positive mean have ordinary Taylor bounds in a neighborhood of one. Subtract the symbolic leading terms before bounding remainders. For each of the three nonzero leading orders above, halving eventually gives the proved sign in exact arithmetic. The third-moment comparison and recognition of exact equality are separate arithmetic tasks.

At the other end, put \(v=tM\). Then \(K\ge v-\log n\), and digamma recurrence and monotonicity give
\[
 f(1/v)=v-\log v-\psi(1+1/v)<v-\log v+\gamma_E .
\]
For \(v>ne^{\gamma_E}\) this gives \(k<1/v\). Since \(K'\le M\), the profile is strictly increasing on that whole tail and strictly below \(P\). The resulting compact power interval contains every stationary point. The profile is real analytic there by the implicit-function theorem and \(f'<0\). Its derivative is not identically zero by the tail sign. Its zeros therefore form a finite set, including any degenerate roots.

**Exact per-input criterion.** Find the positive stationary roots in the compact interval and let \(C\) be the largest value there, with \(-\infty\) used for an empty set. A finite MLE exists exactly when \(C\ge\max(N,P)\). If equality holds, all finite roots at \(C\) are maximizers and the equally high face is excluded from the parameter set. If \(C<\max(N,P)\), report the limit or limits attaining \(\max(N,P)\). This is a finite characterization in exact real arithmetic. A practical certificate can exclude whole cells with the existing secant bounds and isolate only surviving basins. The criterion does not provide an exact numerical equality oracle for arbitrary stationary heights.

### Exact samples for all three strict outcomes

**Proved.**

- **Power only.** Any balanced sample on two distinct levels, for example \((1,2)\), has no finite MLE. The prior balanced-case bound gives \(L(t)<P\) for every finite \(t\) and \(P=-1>N\). Use power, or Pareto on the reciprocal sample.
- **Lognormal only.** Take 100 copies of 1 and one copy of 2 in trgamma. The standardized levels are \(-1/10,10\), with third moment \(9.9\). For \(0<t\le1\), the cubic Taylor lower bound for the exponential gives \(\overline{e^{tu}}\ge1+t^2/2+9.9t^3/6>e^{t^2/2}\), using \(e^{t^2/2}\le1+t^2/2+e^{1/2}t^4/8\). Positive Stirling remainder gives \(L(t)\le\log t-\tfrac12\log(4\pi e K)<N\). For \(t\ge1\), \(K\ge10t-\log101>5t\). Strict Jensen applied to the inverse of a Gamma\((k+1,1)\) variable gives \(\psi(k+1)>\log k\), so \(f(k)<1/k\) and the fitted \(k<1/K<1\). Using \(\log\Gamma(1+k)\ge-\gamma_E k\) and dropping \(k(\log k-1)<0\) gives \(L(t)\le\log[t/(K-\gamma_E)]-1<N\). Therefore use lognormal. Invtrgamma uses the exact reciprocal sample, with the outlier \(1/2\).
- **Finite above both faces.** Take \((2^{-3},1^{[10]},4)\), with brackets denoting multiplicity. In units of \(\log 2\) the mean is \(-1/12\), the skewness is negative and \(M=25/\sqrt{155}>\sqrt{2\pi/e}\). Thus \(N>P\), and a finite power near zero has value above \(N\). Finite attainment follows. The argument leaves the number of finite winners to the stationary comparisons.

### Exact finite/power equality over the reals

**Proved.** Define a path of twelve observations by taking base-two exponentials of the following log levels, with \(0\le h\le3\):
\[
 \begin{cases}
 (-(2+h),,2,\,(-2,2)^{[5]}),&0\le h\le1,\\
 (-3,,2,\,(-r,r)^{[5]}),\quad r=3-h,&1\le h\le3.
 \end{cases}
\]
At \(h=0\) this is balanced two-point data, with every finite value below \(P\). On the first leg \(M\le1\): writing \(d=2+h\), the numerator of \(1-M^2\) with denominator \(144s^2\) in base-two log units is \(10(d-2)^2\). On the second leg \(M=25/\sqrt{155+120r^2}\), and the third central moment in those log units is \(-19/12+(13+10r^2)/48-1/864<0\) for \(0\le r\le2\). At the unique \(h_e\) where \(M=\sqrt{2\pi/e}\) the faces are equal and a fixed small finite power beats both by the signed seam. By continuity that same power beats \(P\) at some \(h_0<h_e\).

On \([0,h_0]\) the gap \(P-N\) is uniformly positive. The Stirling upper bound \(L(t)\le\log t-\tfrac12\log(4\pi e K)\) tends uniformly to \(N\) on this path as \(t\downarrow0\). The far-tail score exclusion is uniform because \(M\) has a positive minimum. Thus a common compact interval contains every finite point that could meet or exceed \(P\). The maximum of \(L(t)-P\) on that compact interval is continuous in \(h\), strictly negative at zero and strictly positive at \(h_0\). Define \(h_*\) as the first \(h\in[0,h_0]\) where that maximum is nonnegative. Equivalently, it is the first parameter on the path with any finite point at or above \(P\). This specifies an exact sample with a finite global maximizer at \(C=P>N\). Its classification is a real crossing. Representability of all its levels in binary64 is undecided. For invtrgamma take reciprocal levels, obtaining a finite/Pareto equality.

**Numerical.** The checker scans 45 logarithmically spaced powers at each of up to seven second-leg coordinates and solves the stationary-height and score equations at 45 decimal digits. It finds \(r\approx1.049917674365736\), \(t\approx1.534976745063013\), with \(P-N\approx0.03029008622\). This localizes a branch contact. The global equality follows from the compact-path proof, which does not identify this root as the first contact or count all finite winners there.

For free-scale local multiplicity the earlier [exact 39-level sample and sign intervals](2026-11-25-gbshape-math-data/gamma-results.json) isolate two separate maximum-containing intervals on the positive side. Both carry the unique nuisance optimizers. Separated local maxima require comparison before any uniqueness status is issued.

## Held scale and both shapes free

Let \(m=\bar z\), \(Z=\max z\) and \(\rho=\#\{z_i=0\}/n\). The exact profile is
\[
 F(p)=\log p+G(pm)-\overline{e^{pz}},\qquad
 G(v)=k(v)v-\log\Gamma(k(v)),\qquad \psi(k(v))=v.
\]
**Proved.** At zero the profile tends to minus infinity. When \(Z\le0\) and \(m<0\), the optimized held-endpoint path trace is
\[
 H=\log(-1/m)-1-\rho.
\]
The large-power excess is \(\gamma_E/(-pm)+O(p^{-2})-\overline{\mathbf 1_{z<0}e^{pz}}\) by the prior held-scale proof. It is strictly positive eventually on every such fixed sample, even when \(\rho>0\). Hence no bounded held-scale case can tie its held-endpoint trace at the global value. If an observation is outside this support, the large-power profile tends to minus infinity in the bounded cases by the prior bounds. The lognormal path would require a moving scale and cannot occur here. Holding either shape excludes both proper faces on any of the other masks; the concentration cases are covered by the table.

The held-scale score is also eventually negative for every bounded case. Near zero it is positive because of \(1/p\). For \(m<0\),
\[
 pF'(p)=k\psi(1+k)-\overline{pz e^{pz}},\qquad k\sim1/(-pm).
\]
The negative contribution from \(\psi(1+k)\to-\gamma_E\) eventually dominates the exponentially small positive contributions from all \(z<0\). Any positive \(z\) makes an additional negative contribution. For \(m=0\) with some \(z\ne0\), the score is strictly decreasing and eventually negative. For \(m>0\) in a bounded case, \(Z>m\) and the exponential at \(Z\) dominates \(1/p+m k\) by \(k<e^{pm}+1\). The strict endpoint signs and analyticity again imply a finite stationary-root list on a per-input compact interval. For global value certification the existing objective tail bounds suffice.

**Proved uniqueness slice.** If \(\prod_i x_i=a^n\) and some \(x_i\ne a\), then \(m=0\) in either orientation. The shape is the single root \(\psi(k)=0\) at every power and \(F''(p)=-1/p^2-\overline{z^2e^{pz}}<0\). The unique fit is certifiable by a monotone score bracket. The equality in the test can be decided by exact products on dyadic data. For example \(x=(1/2,2)\) at \(a=1\) is in this slice.

**Proved separated local maxima.** Hold \(a=1\) and take \(x=(2^{-1000},2^{-1})\). Put \(q=p\log 2\). The score multiplied by power, in units of \(q\), is
\[
 k\psi(1+k)+q(e^{-q}+1000e^{-1000q})/2,qquad
 \psi(k)=-1001q/2.
\]
At \(q=1/1000,1/100,1,20\), respectively, it has signs \(+,-,+,-\). For each rational argument the checker encloses \(k\) by rational endpoints, applying digamma recurrence to \(k+64\) and bounding the remainder after \(\log v-1/(2v)-1/(12v^2)\) between 0 and \(1/(120v^4)\). Substitution by interval arithmetic gives the following outer score enclosures.

| \(q\) | Score enclosure |
|---:|---:|
| \(0.001\) | \([0.65978036,0.65978038]\) |
| \(0.01\) | \([-0.05289162,-0.05289161]\) |
| \(1\) | \([0.18279168,0.18279170]\) |
| \(20\) | \([-0.00005764,-0.00005762]\) |

Both disjoint intervals with positive left and negative right scores contain an interior local maximum. The inverse sample \((2,2^{1000})\) and its held scale are also exact binary64. This example requires basin comparison. It establishes local multiplicity and leaves global equality undecided.

**Proved trace discrepancy by source inspection.** In the inverse held-scale route, the diagnostic postprocessing reevaluates a stored Pareto limit at its ordinary endpoint density. If the raw minimum equals the held endpoint, this adds \(n\rho\) to the total held-scale trace. For \(x=(1,2)\), \(a=1\), the fitted trace rate is \(2/\log2\) and the ordinary Pareto value is exactly 1 log-likelihood unit above the reachable trace. The discrepancy applies when the held-face diagnostic is populated, as on an exhausted certificate. A diagnostic evaluated at the ordinary Pareto value cannot serve as a reachable held-family face witness there.

## Public coefficient checks

**Numerical.** At 65 decimal digits, the checker solves the scores on the oriented physical log data with the requested coefficients held fixed. It compares each free parameter in public coordinates against the root. On both 500-observation golden samples across the eight masks in the November 7 archive, the largest relative error was \(1.95\times10^{-11}\) over native and double-only records. Older archived held coefficients sometimes differ from the requested inputs, by at most \(1.95\times10^{-11}\) on these rows. The oracle used the requested holds. For the newer November 15 all-free snapshots the worst error was \(5.81\times10^{-14}\). These checks assess the archived roots and coordinate restoration on those samples. They use archived outputs from the reported revisions.

**Numerical source replay.** For the exact binary64 input \(x=(1,2)\), \(k=1\), \(p=2^{-40}\), with scale free, the unique scale is
\[
 a_* = \left[(1+2^p)/2\right]^{1/p}
     =1.4142135623731722948\ldots .
\]
The current two-held-shapes formula takes a logarithm of the rounded centered mean of the powered data and divides by \(p\). Replaying the branch with correctly rounded binary64 transcendental operations gives \(\hat a=1.4142361519174376\), relative error \(1.5973\times10^{-5}\). Even a correctly rounded centered mean loses information here. The likelihood discrepancy plus its source-level padding is about \(8.76\times10^{-13}\) against a \(5.91\times10^{-9}\) allowance. The checks thus pass in the arithmetic replay while the free scale misses the default coefficient contract. This is a prediction from the source and stipulated arithmetic, with no fresh compiled invocation. The reciprocal sample \((1/2,1)\) uses the same oriented profile, with reciprocal public scale.

For a stable restoration use an oriented log anchor \(c\) and compute
\[
 b=c+\frac{\log1p(\overline{\operatorname{expm1}(p(y-c))})-\log k}{p}
\]
in the small-power regime. Center near the log mean, preserving the first moment explicitly if it is rounded, and use exponential remainders for the rest. Compensated or extended arithmetic can carry the numerator before division. Independently bound its propagated error in \(b\) and the final public scale. In free-power routes, propagate the entire signed root bracket through the conditional shape and scale, including reciprocal restoration, and compare the resulting public boxes to the requested \(\mathrm{tol}\). A bound on the likelihood is a separate check.

The final Python run passed on the workstation through the one-slot lock. It used 50-digit intervals for the held-scale signs, 45 digits for the branch contact, 65 digits for root comparisons and 80 digits for the scale reference. The math-only scope used no fresh builds. [Checker](2026-11-25-trgamma-math-data/check.py) · [Results](2026-11-25-trgamma-math-data/results.json)

```sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-25-trgamma-math-data/check.py
```

## What this changes for the fitter

- Dispatch constant samples by raw equality and the held-scale side tests in the table. Use a proved unbounded outcome exactly on those masks. Dispatch balanced two-level data with equal counts to the certified power or Pareto limit when all coefficients are free. Allow the held-scale geometric-mean mask to use a monotone bracket.
- Add signed seam and power-tail tests to the free-scale cover. Retain both faces when their comparison remains unresolved. Return the named limit as certified nonattainment only after excluding finite attainment. A proved finite/face equality returns the finite fit, with the face in diagnostics. At held scale the face cannot win a bounded case. Keep the endpoint factor in all held-face payloads, including reciprocal diagnostics.
- Isolate the winning finite region and certify its public coefficients. Return `certified = TRUE, unique = TRUE` for an isolated, proved singleton. A certified finite fit with unresolved cardinality has `unique = NA`. Proved finite multiplicity uses the complete typed set and `unique = FALSE`. A small likelihood gap or two local peaks cannot establish that set. If the global cover, sign comparison or its budget fails to close, keep a successful accuracy-checked search result with `certified = FALSE, unique = NA` and the remaining bounds. Stop on actual search failure, invalid input, unrepresentable required accuracy, or proof that the candidate loses when a checked replacement cannot be returned. [Outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)
- Restore scale stably at tiny held power and check scale error directly. The exact sample \((1,2)\) with held \((1,2^{-40})\) is a regression input for both the coefficient check and the likelihood guard. An implementation job can run the same case through both public routes and both precision modes.

## Open

- General finite global uniqueness in the two joint masks remains open. A strict cell cover can certify individual winners. A general result needs either a dominance theorem for distinct stationary values or an exact finite/finite equality with a complete global cover. The two local-peak constructions alone leave this undecided.
- The real finite/power crossing leaves the number of finite winners there and the arithmetic of its input levels open. Exact finite/face equality on binary64 and attained finite/lognormal equality still need a construction or an exclusion theorem. Root cardinality and equality of two excluded faces are separate comparisons.
- A production root enumerator still needs a certified procedure for degenerate stationary roots, and either an exact third-log-moment predicate or a bounded fallback for rounded near-zero skewness. The analytical cutoff proves finite cardinality without providing those numerical primitives.
- The archived coefficient checks and source replay leave fresh native and double-only execution of the tiny-power regression to the implementation job. Its repair needs a coefficient error bound after public restoration on every successful mask.
