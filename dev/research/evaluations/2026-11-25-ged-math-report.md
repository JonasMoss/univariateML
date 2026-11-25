# Generalized error likelihoods with held coefficients

TASK-357 · Base 338f045bfe4d9631410ccfcce8f16e35e8e68b62

**Proved.** With standard deviation held, every valid binary64 sample has an attained fit. At exact real uniform-width contact a finite fit can win, lose, or equal the contact value. With standard deviation free a compact shape comparison decides between finite attainment and the uniform escape after checking collapse. Exact examples include an all-free median interval, asymmetric two- and three-knot ties, a real global tie between separated shape regions, and a Laplace fit that attains the uniform value.

The conditional profiles, free-scale signed tail, held-scale contact value, and small-held-scale Laplace criterion build on TASK-116.33 and the unit-skew specialization of the SGED analysis. [GED dossier](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md) · [Prior GED certificate](2026-11-07-ged-free-report.md) · [SGED contact analysis](2026-11-25-sstdsged-math-report.md#2-sged-held-width-and-a-free-mean)

## Existence on the eight masks

Let \(p=\nu\), \(t=1/p\), \(b=\sigma\sqrt{\Gamma(t)/\Gamma(3t)}\), and \(T_p(m)=\sum_i |x_i-m|^p\). Write \(T_p^*\) for its value at the held mean or its minimum when mean is free. The sample is nonempty and finite and the holds are valid. Free shape ranges over \([1,\infty)\).

| Holds | Exact rule |
|---|---|
| None | Constant data give unbounded scale collapse. Otherwise apply the free-sd comparison with half the range. |
| Mean | All observations equal to the held mean give unbounded scale collapse. Otherwise apply the free-sd comparison with the largest held-mean residual. |
| Sd | Apply the held-sd width and contact rules with half the range. Away from exact real contact, a finite MLE exists. |
| Mean, sd | Apply the held-sd width and contact rules with the largest held-mean residual. Away from exact real contact, a finite MLE exists. |
| Shape | Constant data give unbounded scale collapse. Otherwise return the fixed-shape maximizing set with its common scale. |
| Mean, shape | All observations equal to the held mean give unbounded scale collapse. Otherwise the fitted scale is unique. |
| Sd, shape | The fixed-shape maximizing location set always exists. |
| Mean, sd, shape | Evaluate the density at the supplied coefficients. |

**Proved.** A free mean is in the sample range. Above shape one strict convexity gives a unique power-loss minimizer. At shape one the full closed median interval minimizes it. Below one the loss increases away from the sample range and is strictly concave on every open gap, so exactly the distinct knots minimizing \(T_p\) maximize. When scale is free and \(T_p^*>0\), the log-scale score \(-n+pT_p^*/b^p\) decreases strictly and has one zero, at
\[
\sigma_*=(pT_p^*/n)^{1/p}\sqrt{\Gamma(3/p)/\Gamma(1/p)} .
\]
Every minimizing location shares this scale. At zero loss the likelihood is unbounded as free scale tends to zero.

For \(p\ge1\), \(b/\sigma\) is bounded above and below by positive constants, including its infinite-shape limit. The log peak is bounded above by \(-\log\sigma\) plus a constant, uniformly in shape. So scale diverging to infinity gives likelihood zero. If the smallest possible maximum residual \(\rho\) is positive, the penalty of a residual at least \(\rho\) also drives the likelihood to zero uniformly as scale tends to zero. These facts follow directly from the log density \(-\log(2b\Gamma(1+t))-|x-m|^p/b^p\). After profiling within the compact location range and using the positive scale formula, the objective is continuous at finite shapes. The only remaining excluded competitor is \(p=\infty\). Shape one is attained and carries the restriction flag when estimated.

## Free standard deviation

**Proved.** For \(\rho>0\) and \(R_p=(T_p^*/n)^{1/p}\), the per-observation gap over \(F=-n\log(2\rho)\) is
\[
G(p)=-\log\Gamma(1+t)-(1+\log p)t-\log(R_p/\rho).
\]
Indeed \(\rho n^{-1/p}\le R_p\le\rho\), the conditional sd tends to \(\rho/\sqrt3\), and free mean tends to the midrange because displacement from it increases the maximum residual. The dossier's signed bound is strictly \(G(p)<[\log n-\log p-1+\gamma]/p\) by strict convexity of log-Gamma. Set \(P=\max(1,n e^{\gamma-1})\) and \(M=\max_{[1,P]}G\). For \(M<0\), return the nonattained uniform. For \(M=0\), return the finite maximizing set with the equal face as a diagnostic. For \(M>0\), return the finite maximizing set. The face endpoints are the sample extremes with mean free and \(m\pm\rho\) with mean held. Its closed uniform likelihood equals the profile limit. Score and value bounds on a compact interval implement strict comparisons. [Free-shape certificate](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md#free-shape-bounds)

If \(T_p^*/(n\rho^p)\ge q>0\) for all \(p\ge1\), then
\[
G(p)\le -\log\Gamma(1+t)+t(\log t-1-\log q).
\]
One can use the farthest fraction with held mean, or \(2\min(k,l)/n\) with free mean, where \(k,l\) count the extremes. The bound is strictly convex in \(t\) since \(\psi_1(1+t)=\sum_{j\ge1}(t+j)^{-2}<1/t\) by integral comparison. Its endpoint values are zero and \(-1-\log q\). Thus \(q>1/e\) proves a uniform escape. This covers the balanced pair with both moments free and constant data displaced from a held mean. With held mean and residuals only \(0,\rho\), the bound is exact. Here \(q<1/e\) gives the unique shape one, and \(q=1/e\) gives a finite Laplace maximum equal to the face. Rational count fractions cannot equal \(1/e\).

### Exact finite–uniform equality and an all-free median interval

**Proved.** Use \((0,a,1)\) with mean held zero, or free both moments on \((-1,-a,0,0,a,1)\), for \(0.103\le a\le0.105\). The profiles agree per observation by symmetry and the unique median zero. The effective tail count is \(1/q=3\), so \(p\ge2\) loses to the face. On \([1,2]\) the sign of the shape score is the sign of
\[
\psi(1+1/p)+\log p-D(p),\qquad
D(p)=p\sum_i w_i\log |x_i|-\log(T_p^*/n),
\]
with weights proportional to nonzero residual powers. The normalizer term increases by the trigamma inequality, and \(D'(p)=p\,\mathrm{Var}_{w_p}(\log |x_i|)\ge0\). Here \(D(1)=\log(3/(1+a))+a\log a/(1+a)\) decreases with \(a<1\). Its value at \(0.105\) exceeds \(2-\gamma-\log2\), the normalizer term at \(p=2\). The interval checker verifies the strict inequalities. The Laplace candidate is therefore the only possible finite winner, with gap \(\log(3/(1+a))-1\). At \(a=3/e-1\) it ties the uniform and is attained with \(\sigma=\sqrt2/e\). Smaller and larger \(a\) in this band give a unique restricted estimate and the uniform escape, respectively. On rational data and holds, a Laplace candidate cannot tie this face since \(R_1/\rho\) is rational and equality requires \(1/e\). **Numerical** checks at 85 and 115 digits give gaps \(+3.93888\times10^{-18}\) and \(-8.63570\times10^{-18}\) per observation at adjacent binary64 inputs **0x1.a880a8a1b369dp-4** and **0x1.a880a8a1b369ep-4**.

**Proved, binary64 interval.** Take one copy of each of \(\pm1\) and three of each of \(\pm a\), with \(a=2^{-10}\), and free all coefficients. Symmetry fixes the profiling location above one. The effective tail count is four, so \(p\ge3\) loses. On \([1,3]\), \(D(p)\ge D(1)=\log(4/(1+3a))+3a\log(a)/(1+3a)>\psi(3/2)+\log3\), which bounds the normalizer term at \(p=3\). The Laplace value is strictly above the uniform. The complete maximizing set is \([-a,a]\times\{\sqrt2(1+3a)/4\}\times\{1\}\), a restricted nonunique estimate. The earlier held-sd example \((-1,1)\), \(\sigma=0.1\), gives \([-1,1]\) at shape one. The threshold in the prior small-sd theorem can include equality because its penalty lower bound is then nondecreasing and its peak is strictly decreasing. [Earlier Laplace theorem](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md#free-shape-bounds)

**Proved, right score.** For a free mean the right derivative of \(T_p^*\) at one is the minimum of \(\sum_i r_i\log r_i\) over the median interval, with \(r_i=|x_i-m|\) and \(0\log0=0\). Expand \(T_p(m)\) uniformly in the sample range using bounded \(r^p\log^2 r\). All cluster points of its minimizers at \(p\downarrow1\) are medians. Comparing the expansion at a minimizing location to its value at the best median proves the derivative and the convergence to that best median. In a nontrivial median gap the first-order moment has second derivative \(\sum_i 1/r_i>0\), and its first derivative tends to opposite infinities at the ends. Its minimizer solves equality of the products of distances on the two sides. For \((0,1,2,100)\) this is \(200/101\) inside \([1,2]\). Use the minimizing moment in the dossier's one-sided shape scores for either scale regime. A positive score excludes Laplace and a strictly negative one separates a right neighborhood. Zero requires further comparison.

## Held standard deviation

**Proved.** Define \(r=\sqrt3\sigma\), and let \(d\) be the held-mean radius or the half-range. With \(g_j(t)=\log\Gamma(1+jt)\), put \(B=(g_1-g_3)/2\), \(C=(g_3-3g_1)/2\), \(E(t)=\exp(-B(t)/t)\), and \(F_0=-n\log(2r)\). The profile is \(F_0+nC(t)-E(t)T_p^*/r^p\).

- If \(d>r\), at least one residual has size \(d\) or greater at every feasible location. The penalty is at least \(e^{-\gamma}(d/r)^p\), while \(C(t)\le(9/4)\zeta(2)t^2\). The tail tends to \(-\infty\) and a finite witness cuts it off.
- If \(d<r\), the profile tends to \(F_0\). Fix the held mean or the midrange so that the penalty is exponentially small in \(p\). The positive peak excess \(C(t)=\tfrac32\zeta(2)t^2+O(t^3)\) yields a finite witness above \(F_0\). The tail upper bound \(F_0+nC(1/P)\) for \(p\ge P\) eventually falls below it. When the loss is identically zero at the profiling location, the strictly increasing peak in \(t\) selects shape one uniquely.
- At \(d=r\), the sharp approach value is \(F_c=F_0-P_0\). For held mean \(P_0\) is \(e^{-\gamma}\) times the endpoint count. With mean free it is \(2e^{-\gamma}\sqrt{kl}\), where \(k,l\) count the extremes. The closed uniform density evaluated at those endpoints has log likelihood \(F_0\). [Prior contact derivation](2026-11-25-sstdsged-math-report.md#2-sged-held-width-and-a-free-mean)

At contact put \(A=0\) with mean held and \(A=(\log(k/l))^2/8\) with mean free. The endpoint-only minimum penalty \(P_{\rm end}\) is \(E\) times the endpoint count for held mean or \(E\) times the two-extreme loss \(F_p(k,l)\) of the dossier. The endpoint-optimal location tends to the midrange. Evaluating there shows that the full and endpoint-only profiles differ by \(O(e^{-c/t})\) for some \(c>0\), since the interior residual ratios stay strictly below one. The log-Gamma series and the two-extreme log-cosh formula give
\[
\log(P_{\rm end}/P_0)=(2\zeta(2)-A)t
 -\bigl(\tfrac{13}{3}\zeta(3)+A\bigr)t^2+O(t^3).
\]
In fact \(-B/t=-\gamma+2\zeta(2)t-\tfrac{13}{3}\zeta(3)t^2+O(t^3)\) and the log-cosh contribution is \(-At/(1-t)+O(t^3)\). Therefore for \(A>2\zeta(2)\) finite shapes eventually beat the face. They also beat it at equality by the negative quadratic coefficient and the positive peak. A converging upper envelope uses \(C(1/P)\), \(E\ge e^{-\gamma}\), and the increasing \(F_p\) bounded below by \(F_P\), or the fixed endpoint count. This proves attainment in both cases. For example, the earlier unit-skew sample with one copy of \(-1\) and 1000 of \(1\), \(\sigma=1/\sqrt3\), and mean free is on the strict side.

For \(A<2\zeta(2)\), choose \(0<t_0<1\) such that
\[
c_0=\frac{2\zeta(2)}{(1+3t_0)^2}-\frac{A}{1-t_0}>0,\qquad
\tfrac94 n\zeta(2)t_0<P_0c_0.
\]
The termwise trigamma bound \(g_1''(u)\ge\zeta(2)/(1+u)^2\) implies \(-B/t\ge-\gamma+2\zeta(2)t/(1+3t)^2\) by integrating \(g_1'(u)+\gamma\) from \(t\) to \(3t\) and using \(g_1'(u)+\gamma\ge u\zeta(2)/(1+3t)^2\). Also \(\log\cosh v\le v^2/2\) by integrating the bound on its derivative. Together they give \(P_{\rm end}\ge P_0(1+c_0t)\) for \(t\le t_0\). The peak bound proves strict tail exclusion there. Compare the compact maximum on \([1,1/t_0]\) to \(F_c\). A lower maximum gives an unattained contact limit, equality gives a finite fit plus the contact diagnostic, and a higher maximum gives the finite fit.

### Exact contact outcomes and the rounded endpoint

**Proved by signed bounds.** For \((-1,1)\), \(\sigma=1/\sqrt3\), with mean free or held zero, contact wins. With \(t_0=0.01\) the signed bound covers \(p\ge100\). On \([1,100]\) a cell \([u,v]\) has upper face-relative value per observation \(C(1/u)-E(1/v)+e^{-\gamma}\). This uses decreasing \(C(1/p)\) and \(E(1/p)\), the latter by the concavity of \(B\) anchored at zero. The interval check excludes all cells, independently confirming the prior unit-skew example.

**Proved, exact real tie.** Keep those holds and add three copies of each of \(\pm a\). For \(0\le a<1\), the gap per observation relative to contact is
\[
J_a(t)=C(t)-E(t)(1+3a^{1/t})/4+e^{-\gamma}/4.
\]
The same \(t_0=0.01\) satisfies the signed bound with endpoint fraction \(1/4\). The compact maximum \(m(a)=\max_{t\in[0.01,1]}J_a(t)\) is continuous and strictly decreasing since each finite value strictly decreases. At zero the Laplace witness gives \(\tfrac12\log6-(\sqrt6-e^{-\gamma})/4>0\). Extend the formula to \(a=1\) with the comparison level fixed. The balanced-pair exclusion gives \(m(1)<0\). Define \(a_c\) as the unique zero of this explicit compact maximum on \((0,1)\). That exact sample attains the contact value at a finite shape. Smaller and larger \(a\) in \([0,1)\) give a strictly winning finite fit and a strictly winning contact limit, respectively.

**Proved, binary64 classification.** With mean held compare the exact dyadic squares \(d^2\) and \(3\sigma^2\); with mean free compare \((\max x-\min x)^2\) and \(12\sigma^2\). Equality would make \(\sqrt3\) rational. Use exact differences or sign enclosures with exact fallback, since a rounded comparison can select the wrong side of a discontinuous penalty.

For the exact binary64 pair \(\pm\texttt{0x1.bb67ae8584caap+0}\) at sd one, \(d<r\). Put \(\delta=-\log(d/r)>0\) and \(q=p\delta\). For \(p\le2\) use \(C\le\tfrac12\log6\) and \(E\ge3/2\); for \(2\le p\le4\) use \(C\le\tfrac12\log(6/\pi)\) and \(E\ge e^{-\gamma}\). The interval check verifies that both ranges lose to \(F_0\). For \(4\le p\le70/\delta\) the peak bound proves the same if \(q^2e^{-q}>(9/4)\zeta(2)e^\gamma\delta^2\). The function on the left has its minimum on \([4\delta,70]\) at an endpoint, and both endpoint inequalities pass. Thus every maximizing shape exceeds \(70/\delta\) for either mean mask. **Numerical** checks at 85 and 115 digits use \(p=10^6,10^{12},10^{16},10^{18},10^{19}\) and bisect a derivative sign change in \(q\in[70,100]\). A local maximum is at \(p\simeq1.48370934\times10^{18}\), with per-observation excess \(1.09476\times10^{-36}\). The shape lower bound and existence proof are independent of that local-root calculation.

### Excluded small shape

**Proved.** The fitted shape restriction removes this face. To resolve the equality in the earlier unrestricted analysis, take held \(\sigma\) and a location with \(k\) zero residuals. Stirling gives \(\log b=\log\sigma-t\log t-\tfrac32t\log3+t+\tfrac14\log3+O(1/t)\). Hence each nonzero residual contributes \(Kt+O(1)\) to the penalty, where \(K=3\sqrt3/e\); the log peak is \(\tfrac32t\log3-\tfrac12\log t+O(1)\). The unrestricted log likelihood along the profiled small-shape end diverges to \(+\infty\) if \(\tfrac32 n\log3>(n-k)K\), and to \(-\infty\) when reversed or equal. At equality the remainder is \(-(n/2)\log t+O(1)\). For a free mean use the largest multiplicity, since the exact profile for \(p<1\) is over finitely many knots. With free sd and positive loss the per-observation expansion is \(-t\log((n-k)/n)-\tfrac12\log t+O(1)\), from the power mean and Stirling. It diverges positively for \(0<k<n\) and negatively for \(k=0\). The case \(k=n\) is scale collapse. [Earlier leading-order spike analysis](2026-10-29-p0-locscale.md#ged)

## Separated and tied maximizers

**Proved, binary64 knot ties without reflection.** Hold \(p=1/2\) and free mean. At knots \(0,9,25\) use multiplicities \((3,2,3)\). The losses are \(21,21,23\) since the positive square-root distances are \(3,4,5\). Exactly \(0,9\) maximize. With counts \((8,5,9)\) the losses are \(60,60,60\) and all three knots maximize. Both sets are complete with sd held at any positive value or free with the common analytic sd. Their unequal adjacent gaps exclude reflection. For rational \(p=a/b\), each distance power on dyadic data is the unique nonnegative solution to \(u^b=|x_i-x_j|^a\). Real algebraic sign comparison of the sums decides all knot comparisons including exact equality. At \(p=1/2\) grouping squarefree radicals after removing square factors gives a simpler equality predicate.

**Proved by interval comparisons, separated interior maxima.** Hold sd one and hold mean zero or free it. Use eight zeros, 23 copies of each of \(\pm a\), and 12 of each of \(\pm b_0\), with \(b_0=\texttt{0x1.6c84e093fc0ecp+0}\) exactly. Symmetry fixes the profiling location at zero above shape one. Let \(V(p,a)\) be the per-observation profile excess over \(F_0\). Both radii are below \(\sqrt3\); the peak and the penalty decrease in \(p\). An interval upper bound is the peak at its lower endpoint minus the penalty at its upper endpoint. The penalty increases in \(a\). The checker establishes the following.

- For the full band \(a\in[0.329,0.331]\), \(p=1\), \(2\le p\le8\), and \(p\ge100\) lose to the witness at \(p=25.4\).
- At \(a=0.329\) the witness at \(p=1.11\) beats every \(p\in[8,100]\). At \(a=0.331\) the witness at \(p=25.4\) beats every \(p\in[1,2]\).
- At the exact binary64 \(a=\texttt{0x1.51eb851eb851fp-2}\), each witness beats both endpoints of its own shape interval. Hence the exact binary64 sample has two separated interior local maxima.

For an exact global tie define \(L(a)=\max_{[1,2]}V\) and \(H(a)=\max_{[8,100]}V\), continuous by compactness. The magnitude of \(\partial_a V\) is \((46/78) E(1/p)p(a/\sqrt3)^p/a\). It is greater than \(1/100\) on the lower shape interval and less than \(1/1000\) on the upper. Use \(a<1/2,\ a/\sqrt3>1/7,\ E\ge3/2\) for the first bound, and \(a>1/4,\ a/\sqrt3<1/5,\ E<3,\ p/5^p\le8/5^8\) for the second. Comparing the envelopes at their maximizers gives the same slope separation for their increments. The exact real sample uses the unique \(a_*\in(0.329,0.331)\) with \(H(a_*)=L(a_*)\). Both intervals contain global maxima, and every other region has been excluded. The **numerical** local-branch crossing reproduces \(a\simeq0.32998660809120003716\) and shapes \(1.108145553391854,\ 25.39783214175462\). Those local roots alone do not count the maximizing points in each interval or prove a binary64 equality. [Earlier numerical construction](2026-11-07-ged-free-report.md#stops-nonuniqueness-and-arithmetic-checks)

## Public coefficients and reproduction

**Numerical.** The replay verifies input hashes before comparing stored native and double-only public fits against independent references at 85 and 115 digits. The grid is archive cases 002360, 002372, 002389, 002404, 002420, 002436, 002452, 002468 and the 500-point golden input. It covers every mask. Shape-one references use the exact median and scale; the others solve the free scores, with positive observed information checked for the interior free-shape roots. The largest scaled public coefficient errors were \(6.77\times10^{-16}\) native and \(9.40\times10^{-16}\) double-only, with the fitted-scale location floor. For free location and scale at held shape, recomputing the exact conditional scale at the emitted location moved it by at most \(3.54\times10^{-33}\) relative on this grid. [Accuracy contract](../../design/accuracy-contract.md) · [Stored snapshots](2026-11-15-fit-snapshot-data/)

**Proved rounding counterexample.** For \((1,1+\Delta)\) with \(\Delta=2^{-52}\) and \(p=2\) held, the ideal mean and sd are \(1+\Delta/2\) and \(\Delta/2\). A mean rounded to 1 meets the location tolerance, but refitting sd there gives \(\Delta/\sqrt2\), an error of \(\sqrt2-1\) relative. At the rounded mean with ideal sd the public log likelihood is one unit below the ideal likelihood. At any held \(p>1\), refitting after the same mean rounding multiplies ideal sd by \(2^{1-1/p}\). The conditional fitter initially computes sd using its public mean and has subsequent score and accuracy guards. This tests the refit formula and joint target without a fresh public binary run on the tight pair. [Conditional fitter](../../../univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp) · [Public accuracy polish](../../../univariateML-cpp/include/cppunivariate/distributions/ged/accuracy.hpp)

Both final scripts exited zero on the workstation under the one-slot lock. The 40-digit interval script shifts the Gamma argument above 16 by recurrence and uses Stirling through the inverse seventh power with the next positive term bounding the remainder. This enclosure follows by expanding the arctangent in Binet's integral with its alternating remainder bound. The radius-band valley, small-radius right exclusion, large-radius left exclusion, and binary64 valley used 23, 31, 853, and 23 cells, respectively. The balanced contact comparison used 15 cells. The numerical replay uses ordinary mpmath. Run from the worktree root.

    d=dev/research/evaluations/2026-11-25-ged-math-data
    export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
    export PYTHONDONTWRITEBYTECODE=1
    mkdir -p "$TMPDIR"
    /tmp/umvl-cpu/run python3 -u "$d/cover.py"
    /tmp/umvl-cpu/run python3 -u "$d/replay.py"

[Interval certificates](2026-11-25-ged-math-data/cover.py) · [Exact-input and numerical replay](2026-11-25-ged-math-data/replay.py)

## What this changes for the fitter

- Keep the exact collapse predicates. Return unbounded only when free scale can collapse at zero loss. A positive scale lost to arithmetic is a numerical or tolerance failure.
- With sd held, use the exact width order. Every valid binary64 input has a finite mathematical maximizer. A near-contact interior case can require a search in \(q=p\delta\) with a stable log-radius deficit and a common likelihood shift. The rounded \(\sqrt3\) pair refutes an estimate capped at the ordinary fallback grid. A winning symbolic contact carries its approach likelihood separately from closed-uniform evaluation.
- With sd free, attach the uniform after strict exclusion of all finite competitors. At proved finite–face equality return the attained estimate and attach the face diagnostic. When Laplace wins, preserve the full median interval and its common scale. Flag the restriction only when shape is estimated.
- Compare every distinct subunit knot by exact or enclosed loss. Return every proved winning point, including asymmetric ties, after validating its public sd. Certify uniqueness only with root and coefficient evidence. If global comparison is unresolved, preserve a successful locally accuracy-checked search with uncertified status and unknown uniqueness unless an independent bound refutes it. Preserve genuine search and accuracy failures. Emit a typed maximizer set only with equality and completeness evidence. [Outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)
- Enclose the joint location and sd before rounding either. Use the ideal joint scale as the coefficient reference. Evaluate the public likelihood under the existing steep-maximum convention where applicable.

## Open

- Exact equality of two distinct interior shape roots on binary64 data remains undecided. The real crossing proves coexistence in two disjoint intervals. Counting the winners within each still needs a signed derivative and curvature cover. General root counts at moving location knots and degenerate shape-one scores remain open.
- With sd free and an interior shape, exact equality with the uniform on general rational samples needs an equality predicate or exclusion theorem. The Laplace exclusion settles only shape one. Compact comparison specifies the mathematical outcome but overlapping enclosures may fail to decide it.
- Production near-contact arithmetic still needs a stable coefficient enclosure and practical error bounds for the log-radius deficit, normalized penalty and derivatives. The Python checks establish existence and a shape lower bound for the rounded pair. They do not validate a production search or its budget in that regime.
