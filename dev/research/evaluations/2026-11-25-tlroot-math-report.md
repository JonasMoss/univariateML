# Tukey lambda root isolation, half-tie cutoff, and shared-scale bounds

TASK-253. The free-scale U profile admits a sign-and-derivative certificate with explicit witnesses for a double stationary root and a finite maximum tied exactly with the junction. The half-tie cutoff reduces the log-scale search width from 34.43 to 11.38 on the balanced sample and from 76.36 to 6.93 on the separated sample. On the two 31-point probes, the cheap shared-scale bound barely changed the remaining likelihood gaps. At held shape \(-1\), profiling the exact density over scale closed the comparison in 26,359 boxes. The definitions of \(P,J,B,K\) and the support convention follow the prior work. [Prior math report](2026-11-25-tl-math-report.md) · [Tukey lambda dossier](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md)

## U profile with free scale

Use the prior fixed-support reduction for \(1<\lambda<2\). Set \(H=\log(P/J)\), \(S=H'\), and \(t_i=|x_i-\mu|/R\). For \(0<t_i<1\), let \(p\) be the smaller tail, \(q=1-p\), and \(D=p^{\lambda-1}+q^{\lambda-1}\). Then
\[
 F(\lambda,p;t)=p^\lambda-q^\lambda+t=0,\qquad
 H(\lambda)=-n\log\lambda+\sum_{t_i<1}\log(2/D_i).
\]
A center observation contributes \((\lambda-1)\log 2\) to the sum. The analytic extension has \(H(1)=0\), \(H(2)=-k\log 2<0\), and finitely many stationary roots on \([1,2]\) by the prior result.

### Acceptance conditions

**Proved.** Over a shape interval \(I\subset[1,2]\), first enclose the inverse in \([(1-t)/3,\min(1/2,1-t)]\). Indeed \(p\le1-q^\lambda\le\lambda p\) and \(p^\lambda\le p\). A trial \(p_0\) is a valid new lower endpoint if \(F(I,p_0;t)<0\) throughout and an upper endpoint if \(F(I,p_0;t)>0\) throughout, since \(F_p=\lambda D>0\). Two searches for the endpoints preserve the bracket when the sign is ambiguous. The same reasoning applies with \(t\) enclosed in an interval.

Evaluate \(H,S,S',S''\) on the resulting enclosure. The derivatives of the inverse follow by differentiating \(F=0\):
\[
\begin{aligned}
 p' &=-F_\lambda/F_p,\\
 p'' &=-[F_{\lambda\lambda}+2F_{\lambda p}p'+F_{pp}(p')^2]/F_p,\\
 p''' &=-[F_{\lambda\lambda\lambda}+3F_{\lambda\lambda p}p'
 +3F_{\lambda pp}(p')^2+F_{ppp}(p')^3
 +3(F_{\lambda p}+F_{pp}p')p'']/F_p .
\end{aligned}
\]
Differentiate \(-\log D\) along the same inverse. With \(c\) the midpoint and \(h=I-c\), Taylor's theorem encloses \(S(I)\) in \(S(c)+S'(c)h+S''(I)h^2/2\), and \(S'(I)\) in \(S'(c)+S''(I)h\). Intersect with the direct enclosures. Also intersect the direct \(H(I)\) with \(H(c)+S(I)h\), using the tightened score range. The quadratic term helps near a fold, where \(S'(c)\) is small.

- Discard \(I\) when its score range excludes zero. If \(S'\) has strict sign throughout, equal nonzero endpoint signs of \(S\) also discard \(I\). Opposite endpoint signs isolate exactly one simple root by continuity and strict monotonicity. Bisection retains the signs, with off-center trials when a midpoint sign is ambiguous.
- Subdivide the remaining intervals, retaining coverage. A general implementation can search interior dyadic cuts for a certified nonzero sign and raise arithmetic precision if needed. The score has finitely many roots, so every cut window contains nonzero points. With convergent enclosures, neighborhoods of simple roots eventually admit sign-change tests and the root-free complement is excluded. An exact endpoint zero can be accounted for by a multiplicity witness and omitted from the interior candidate list.
- A multiplicity-\(m\) witness supplies an exact \(\alpha\in I\) with \(S^{(j)}(\alpha)=0\) for \(0\le j<m\) and a strict sign bound on \(S^{(m)}(I)\). Taylor's theorem gives \(S(\lambda)=S^{(m)}(\xi)(\lambda-\alpha)^m/m!\) for some \(\xi\) between \(\alpha\) and \(\lambda\). This proves uniqueness in the collar, exact multiplicity and both one-sided signs. One can specify \(\alpha\) as the unique root of \(S^{(m-1)}\), with separate proofs of the lower derivative identities. An even multiplicity preserves the sign of the score through the root, so that stationary point cannot maximize \(H\).
- If every interior stationary candidate has a strictly negative \(H\) upper bound, the junction wins strictly. A finite root above zero beats the junction. A root proved to have \(H=0\) wins jointly with the junction provided every other candidate is proved to be at or below zero. Compare surviving finite values with each other before certifying a unique winner or a list of tied winners. An unresolved root interval can be omitted from the value comparison when its entire \(H\) upper bound is strictly below a proved competing level.

A finite collection of strict comparisons persists under sufficiently small perturbations of a continuous input. The transitions below have nearby inputs where the answer changes. Exact coalescence and exact equality with \(J\) therefore need more than strict numerical signs.

### Interval results and exact transition samples

**Numerical enclosures.** The prototype used 70 decimal digits in mpmath.iv with exact rational sample inputs. Ordinary root brackets had width below \(10^{-20}\). Table values are rounded from the interval output. The free-scale U test also exercised the sample geometries from the prior held-scale examples. [Prototype](2026-11-25-tlroot-math-data/roots.py) · [Sample definitions](2026-11-25-tlroot-math-data/scenarios.py) · [Interval output](2026-11-25-tlroot-math-data/roots.txt)

| Sample | Interior roots | Largest stationary \(H\) | U outcome |
|---|---:|---:|---|
| \((-1,1)\) | 0 | | Junction |
| \((-1,1,t^6)\), \(t=-63457199/64481201\) | 1 | \(0.732785029827\) | Finite, \(\lambda\approx1.262003364871\) |
| \((-1,0,0,1)\) | 0 | | Junction |
| \((0,0,10^{-20},1)\) | 1 | \(0.400668338371\) | Finite, \(\lambda\approx1.052574999211\) |
| \((0,0,1/2,1)\) | 0 | | Junction |
| \((0,10^{20})\) and \((0,0,10^{20})\) | 0 each | | Junction |
| \((-1,0,1)\) | 0 | | Junction |

Superscripts on sample entries denote repetitions. The separated sample still has the proved collapse outcome on the principal branch.

**Proved with interval-checked signs.** For \(X(t)=(-1,1,t^{19},0^{16})\), define \(t_{\rm d}\) as the unique real number in
\[
 (0.7425450262943313027831787956,\ 0.7425450262943313027831787957)
\]
for which \(\max_\lambda S(\lambda;t_{\rm d})=0\) on the shape collar \([477966,478031]/262144\). On the whole parameter rectangle, \(S''<0\), \(S_t<0\), and \(S'\) has signs \(+,-\) at the two shape endpoints. Thus each \(t\) has a unique interior score maximum. Its value varies continuously by maximization over a fixed compact set. It strictly decreases in \(t\) by comparison at the maximizing shape for the larger \(t\). Enclosing the two endpoint maxima gives signs \(+,-\), proving existence and uniqueness of \(t_{\rm d}\). At that exact input the collar contains one double root, near \(1.823419030778\), and the rest of \([1,2]\) contains no roots. The collar has \(H<-1.37907\), so the junction wins.

For one interior observation \(p_t=-1/(\lambda D)\) and \(H_t=D_p/(\lambda D^2)\). Differentiating the latter along the inverse in shape supplies the enclosure for \(S_t\). At the lower rational \(t\) endpoint minus \(10^{-18}\), two simple roots isolate near \(1.823419029019\) and \(1.823419032538\), both below \(J\). At the upper endpoint plus \(10^{-18}\), there are zero roots. The interval run accounts for the entire shape domain for both perturbations. [Fold witness and perturbations](2026-11-25-tlroot-math-data/roots.txt)

**Proved with interval-checked signs.** For \(X(t)=(-1,1,t^{110},0^{35})\), define \(t_{\rm j}\) as the unique real number in
\[
 (0.7500888321531553476973054941,\ 0.7500888321531553476973054942)
\]
for which \(\max_\lambda H(\lambda;t_{\rm j})=0\) on \([419455,419520]/262144\). The rectangle has \(S'<0\) with endpoint score signs \(+,-\) in shape order. This gives a unique interior maximum on the collar. For these shapes and \(0<p<1/2\), \(D_p=(\lambda-1)(p^{\lambda-2}-q^{\lambda-2})>0\), so \(H_t>0\). The collar maximum is consequently continuous and strictly increasing in \(t\). The two rational input endpoints have maximum signs \(-,+\), which proves existence and uniqueness of \(t_{\rm j}\) and exact equality at its collar root. The only other stationary root is near \(1.245974959750\), with \(H\approx-0.215917\). The finite maximum near \(1.600217784053\) ties the junction. At the lower \(t\) endpoint minus \(10^{-18}\) and the upper endpoint plus \(10^{-18}\), both roots isolate and the larger critical value is about \(-3.22335\times10^{-17}\) and \(+3.22335\times10^{-17}\), respectively. The implicit samples and their rational perturbations exercise comparisons finer than binary64 input spacing near the transitions. [Junction witness and perturbations](2026-11-25-tlroot-math-data/roots.txt)

## Exactly-half ties on the fitted branch

Use \(n=2k>2\), maximal multiplicity \(k\), and the prior collapse value \(B\) and upper scale \(S_{\max}=B^{-1/n}\). A larger lower cutoff shrinks the compact search. The location and shape sides of \(K\) follow the prior report.

### Sample-dependent cutoff

**Proved.** Choose \(A\in[1/2,1)\). For \(m\) disjoint pairs of distinct sample entries let \(d_j\) be half the pair separation. For every \(\mu\), each pair contributes at least one residual of magnitude \(\ge d_j\). The sorted halves pairing \((x_{(j)},x_{(j+k)})\) has \(k\) distinct pairs, since equality across \(k\) positions would require at least \(k+1\) ties. Any subset works. The extreme pair is a separate option.

For \(a=-\lambda\in[A,1]\), put \(H_i=|x_i-\mu|/\sigma\) and \(y_i=-\log p_i\) with smaller tail \(p_i\). Then
\[
 y_i\ge \frac{\log(1+aH_i)}a\ge\log(1+H_i).
\]
The first inequality uses \(p_i^{-a}=aH_i+(1-p_i)^{-a}\), and the second uses concavity of \(\log\) between \(1\) and \(1+H_i\). For \(y_i\ge 4\), retain the coefficient \(480/289\) from the prior negative-shape score proof and use \(1/a\le1/A\). The score contribution is at least \(\alpha y_i-b_A\), where \(\alpha=191/289\), \(b_A=480/(289A)\). For \(y_i<4\) it is at least \(-4\), which is also a valid bound when \(y_i\ge4\). Ensure \(y_i\ge4\) for the \(m\) selected residuals by taking \(\sigma\le\min_j d_j/(e^4-1)\). Since \(y_i>\log(d_j/\sigma)\) there, the total score in \(a\) is strictly positive at or below
\[
 T_{\rm score}(A,d)=\min\left\{
 \frac{\min_j d_j}{e^4-1},\
 \left(\prod_j d_j\right)^{1/m}
 \exp\!\left[-\frac{mb_A+4(n-m)}{m\alpha}\right]\right\}.
\]
Thus the shape band \([-1,-A]\) is dominated by the finite fit at \(-1\), whose likelihood is strictly below \(B\) by the prior face proof.

On \([-A,1]\) apply the prior Hölder bound on the selected residuals and the modal bound on the others. With \(\gamma=1+1/A\), \(C=A^{-\gamma}\), and \(E=m/A-(n-m)\),
\[
 L\le 2^{-(n-m)}C^m\left(\prod_j d_j^{-\gamma}\right)\sigma^E .
\]
When \(E>0\), the bound is at most \(B/2\) for \(\sigma\le T_{\rm tail}(A,d)\), where
\[
 T_{\rm tail}(A,d)=
 \left[\frac{B\,2^{n-m}}{2 C^m}\prod_j d_j^\gamma\right]^{1/E}.
\]
For each \(A\) take the minimum of the largest score cutoff and the largest tail cutoff over the tested subsets. The two subsets may differ since both bounds are uniform in \(\mu\). Take the maximum of these combined cutoffs over the chosen \(A\)'s and the prior \(\tau\). This proves strict exclusion at or below the returned threshold. For \(m=k\), \(E=k(1/A-1)>0\), so there is an eligible tail estimate on every grid point.

**Proved separated-sample asymptotic.** On \((0,0,\epsilon,1)\) with small positive \(\epsilon\), use \(A=1/2\) and distances \(\epsilon/2,1/2\). The tail bound is \(1024\sigma^2/\epsilon^3\), and \(B=1/(64\epsilon^2)\). The resulting cutoff is \(2^{-17/2}\sqrt{\epsilon}\). The extreme pair alone gives a positive score cutoff independent of \(\epsilon\), so the tail cutoff applies for sufficiently small \(\epsilon\). The new log-scale width is at most \(10\log 2\). The old cutoff is \(\epsilon^2/512\) for sufficiently small \(\epsilon\), and its log-scale width diverges.

**Numerical.** At 80 decimal digits, the script checks \(A=0.50,0.51,\ldots,0.99\), prefixes of pair distances sorted largest first, and the extreme pair. The widths are \(\log(S_{\max}/\tau)\) and \(\log(S_{\max}/\tau_{\rm new})\). Their ratio is also the volume ratio in \((\mu,\log\sigma,\lambda)\) since only the lower scale side changes. The full output also sweeps six exact rational \(\epsilon\)'s in the transition family. [Cutoff prototype](2026-11-25-tlroot-math-data/cutoff.py) · [Cutoff output](2026-11-25-tlroot-math-data/cutoff.txt)

| Sample | Old log-scale width | New log-scale width | Increase in cutoff, decades | Selected \(A\) |
|---|---:|---:|---:|---:|
| \((-1,0,0,1)\) | 34.43 | 11.38 | 10.01 | 0.70 |
| \((0,0,10^{-20},1)\) | 76.36 | 6.93 | 30.15 | 0.50 |
| \((0,0,1/2,1)\) | 34.77 | 11.38 | 10.16 | 0.70 |
| \((0,0,1,1)\) | 35.12 | 11.38 | 10.31 | 0.70 |

## Shared-scale envelopes on the 31-point probes

The sample is \(Q_{0.14}((i+1/2)/31)\), \(0\le i<31\). One probe holds \(\lambda=-1\) and the other varies it over \([-1,1]\). The Python port uses the recorded binary64 quantile inputs and operations, the original proposal likelihoods, starting boxes, longest-relative-side splitting and gap target \(3.1\times10^{-6}\). It replays the corner bound and tests the prior Hölder shared-scale envelope alone and in a cascade. The cascade skips the corner bound when the inexpensive bound suffices and otherwise takes the smaller of the two upper bounds for queue priority. The cheap bound uses the box's upper-shape modal constant and \(A=\max(1/2,-\lambda_{\min})\). Its breakpoint maximizer follows from the slope \(-n+\gamma\#\{i:\mathrm{breakpoint}_i>s\}\) away from knots. [Original probe](2026-11-07-tlambda-impl-data/box-probe.cpp) · [Original free-shape result](2026-11-07-tlambda-impl-data/box-probe.json) · [Original held-shape result](2026-11-07-tlambda-impl-data/negative-box-probe.json)

### Exact shared-scale bound for held shape \(-1\)

**Proved.** For a location interval \(I\), let \(d_i=\operatorname{dist}(x_i,I)\), \(s=\log\sigma\), and \(h_i(s)=\sqrt{4+d_i^2 e^{-2s}}\). The prior closed-form density \(g_{-1}\) decreases in absolute residual, hence
\[
 \log L\le U_I(s)=-ns-\sum_i\{\log h_i(s)+\log(h_i(s)+2)\}.
\]
Its derivative is \(n-\sum_i(2/h_i+4/h_i^2)\), which is nonincreasing because \(h_i\) decreases in \(s\) for \(d_i>0\) and is constant otherwise. Therefore \(U_I\) is concave. Endpoint derivative signs either identify an endpoint maximum or bracket the derivative root. At a trial \(s_0\) in the final bracket \([a,b]\), the tangent gives the upper bound
\[
 U_I(s_0)+\max\{U_I'(s_0)(a-s_0),\,U_I'(s_0)(b-s_0)\}.
\]
This permits subdivision of location alone and retains a common scale. The run used up to 30 derivative bisections per box.

### Counts and gaps

**Numerical.** The tree searches used binary64 with a \(2\times10^{-10}\) additive log-bound pad. A 70-digit audit checked the sample values and 93 inverse points, including positive support deficits down to \(10^{-10}\). The Python corner replay reproduced both historical pending counts. Its gap differed from the C++ record by \(2.0\times10^{-10}\) at fixed shape and \(7.7\times10^{-9}\) at free shape. Corner calls count box-bound invocations, including fast paths with no numerical inverse. Wall times are from the same workstation run. [Benchmark prototype](2026-11-25-tlroot-math-data/envelope.py) · [Benchmark output](2026-11-25-tlroot-math-data/envelope.txt)

| Shape | Bound | Boxes | Pending | Corner calls | Log gap or upper bound | Wall |
|---|---|---:|---:|---:|---:|---:|
| Fixed −1 | Corner | 127,999 | 51,548 | 127,999 | 0.017291404 | 2.50 s |
| Fixed −1 | Cheap shared scale | 127,999 | 63,982 | 0 | 12.858135 | 2.15 s |
| Fixed −1 | Cascade | 127,999 | 51,607 | 127,925 | 0.017276287 | 3.82 s |
| Fixed −1 | Exact density, profiled scale | 26,359 | 0 | 0 | \(\le3.1\times10^{-6}\) | 17.74 s |
| Free | Corner | 127,999 | 52,094 | 127,999 | 0.645258419 | 47.60 s |
| Free | Cheap shared scale | 115,175 | 57,576 | 0 | 39.261157 | 2.10 s |
| Free | Cascade | 127,999 | 52,538 | 127,563 | 0.641290713 | 30.41 s |

The free-shape cheap-only run reached adjacent representable box endpoints. The cascade skipped only 436 corner calls for free shape and 74 at fixed shape. The changed priorities also changed which boxes were evaluated, so the single free-shape wall-time comparison does not isolate the prefilter's cost. Profiling the exact held-shape density closed the requested likelihood-gap comparison conditional on the computed bounds. The free-shape comparisons remained above the target.

### Box-count bounds

**Proved.** The cheap envelope can have persistent slack under refinement. At held \(-1\), every box containing a specified feasible point has a bound at least as large as the envelope evaluated at that point. The constants are fixed, \(d_i\) can only decrease in a containing location interval, and the point's scale is available to the maximization. **Numerically**, at \((\mu,s)=(0,-1.1082259776925427)\) the pointwise envelope is \(-49.085657\), above the target near \(-61.860077\). At the free-shape point \((0,0.07,0.22)\), the pointwise envelope is \(-40.417861\), against the target near \(-54.760312\). Thus even the local limit of shrinking boxes at that free-shape point exceeds the target. This point was selected from the script's local grid and evaluated at 70 digits.

**Proved for exact profiling at \(-1\).** The standardized log density satisfies
\[
 \left|\frac{d\log g_{-1}(z)}{dz}\right|
 \le\frac{2|z|}{4+z^2}\le\frac12 .
\]
For any \(\mu\in I\), \(\big||x_i-\mu|-d_i\big|\le |I|\). Thus \(\max_s U_I(s)\le\ell_*+C_\mu |I|\), where \(C_\mu=n e^{-s_{\min}}/2\), \(s_{\min}\) is the initial lower log scale, and \(\ell_*\) is the true maximum. If the incumbent gap is \(\Delta=\ell_*-\ell_0<g\) for requested gap \(g\), uniform location bisection to depth
\[
 D=\max\!\left(0,\left\lceil\log_2\frac{C_\mu|I_0|}{g-\Delta}\right\rceil\right)
\]
suffices, visiting at most \(2^{D+1}-1\) boxes. An error allowance for approximating the profiled bound can be subtracted from \(g-\Delta\). **Numerically**, assuming \(\Delta=0\) gives \(D=39\) and a loose bound of \(1{,}099{,}511{,}627{,}775\) boxes. The observed 26,359 boxes supply the practical evidence for this instance.

**Proved qualitatively for the exact corner cascade.** Suppose \(\ell_*-\ell_0<g\) on the initial compact box. The exact corner bound inverts at the lower shape and smallest standardized distance, then bounds the density at that tail with the upper shape and lower scale. As boxes shrink to a parameter point its limsup is at most the point's log likelihood. This follows from inverse continuity at interior observations, eventual rejection at strict support violations, and \(p\to0\) with \(1-\lambda\) bounded away from zero at contact below 1. At \(\lambda=1\) the modal bound supplies the closed-support density for observations in support. If arbitrarily small boxes exceeded the target, compactness would give a subsequence tending to a point with log likelihood at least \(\ell_0+g\), a contradiction. Some finite subdivision therefore suffices with the exact bound in a fair refinement. This yields no practical depth or count for the free-shape probe.

## Reproduction

The final workstation runs passed on Python 3.14.5 with mpmath 1.3.0 through the one-slot lock. The root, cutoff and envelope runs took 108.954 s, 0.172 s and 108.436 s of script wall time. The envelope run includes the high-precision audits and the benchmark cases that deliberately stop with a positive unresolved gap.

~~~sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
dir=dev/research/evaluations/2026-11-25-tlroot-math-data
for script in roots cutoff envelope; do
  /tmp/umvl-cpu/run nice -n 10 python3 -u -B "$dir/$script.py" || exit
done
~~~

## What this changes for the fitter

- For an optional U fit with free scale and positive support radius, use the prior exact nuisance reduction and the interval score and derivative tests on \([1,2]\). Issue the junction status on a strict global comparison. A proved finite tie at \(J\) returns the finite maximizers and can record the equal junction limit. Report multiple finite maximizers only with equality and completeness evidence.
- For exactly-half ties on \([-1,1]\), replace the lower face of \(K\) by the maximum of the old and new cutoffs. Use the bounds in logs with a computation margin. A feasible value at or above \(B\) proves attainment. Strict global exclusion at level \(B\) proves CollapseLimit.
- The held-\(-1\) profile supplies an upper bound for each location interval through a scalar scale solve. It supports a global likelihood-gap test for a valid incumbent. Coefficient accuracy and uniqueness each require their own evidence.
- When a comparison remains unresolved or its certificate budget expires, retain a valid successful search result marked uncertified. Stop if the certificate refutes that candidate, proves strict escape or nonexistence, or if the search itself fails the existing convergence and score criteria.

## Open

- The U prototype accepts exact multiple roots and equality through explicit witnesses and resolves strictly separated near cases. A general exact predicate for \(S=S'=0\) or \(S=H=0\) on an arbitrary fixed sample still needs an exact identity, structural theorem or other equality certificate. The same missing evidence applies to higher multiplicity and tied finite critical values.
- The half-tie cutoff reduces \(K\). Exact equality of its optimum with \(B\) on arbitrary samples still needs a critical-value comparison or a problem-specific identity.
- Free shape still needs a tighter correlated bound or a usable modulus to give a practical count at \(3.1\times10^{-6}\). The held-\(-1\) result provides an instance where retaining scale in the exact density bound closes the comparison.
