# Generic uniqueness of attained maximum likelihood

TASK-412 · 2026-11-27 · Base cb3a93e2

**Proved.** Distinct global winners must have identical sample derivatives almost everywhere, under local smoothness in the data. Separating those derivatives proves generic uniqueness for all-free skew Student and Prentice generalized gamma, conditional on attainment. Held-df Student with both nuisances free has a stronger property: every attained fit is unique. With scale also held, the two-observation problem instead has two global maxima on an open set.

## The theorem

**Proved.** Let \(U\subset\mathbb R^m\) be open. Use countably many candidate families, each with a locally compact, second-countable parameter chart \(P\). On the open domain in \(P\times U\) require joint continuity of the value \(L(p,x)\) and its sample derivative \(D_x L(p,x)\). A chart can encode a sample-dependent candidate, such as a join at the sample minimum, provided it remains feasible nearby and its composed height meets these hypotheses. A smooth optimized face height is a chart with a single parameter point.

At each sample take the supremum over the feasible candidates. Outside a single Lebesgue-null set, all candidates attaining a finite supremum have the same sample gradient. Consequently, if distinct winners, or winners from distinct equivalence classes, must have different sample gradients outside some specified null set, almost every sample has at most one winner, or one winning class. This statement is conditional on attainment and does not require an open attainment set.

**Proof.** Cover the candidate–sample domains by countably many products \(K_j\times B_j\), with \(K_j\) compact and \(B_j\) an open ball with compact closure in a slightly larger product inside the domain. Local compactness and second countability give the cover. On each product, the derivative bound gives a common sample Lipschitz constant. Thus \(V_j(x)=\max_{p\in K_j}L(p,x)\) is Lipschitz, as is \(V_{jk}=\max(V_j,V_k)\) on the overlap. Take the union of their countably many null nondifferentiability sets by Rademacher's theorem. If two global winners at \(x\) belong to pieces \(j,k\), their smooth height germs are below \(V_{jk}\) and attain it at \(x\). Differentiating the local minima of the differences shows their gradients both equal \(D V_{jk}(x)\) wherever this derivative exists. Only the compact comparison envelopes need be locally Lipschitz. More generally the argument needs a countable compact-piece cover with common local Lipschitz bounds and differentiable sample germs at the winners compared. [Rademacher (1919)](https://doi.org/10.1007/BF01498415)

**An exact exceptional-set test.** Write \(S(x)\) for the full supremum. With jointly continuous sample derivatives,
\[
 E=\bigcup_{j,k}\{x\in B_j\cap B_k:V_{jk}(x)=S(x)\in\mathbb R,\ V_{jk}\text{ is nondifferentiable at }x\}
\]
is exactly the set with two global winners of different gradients. For the additional implication, if all maximizers of a compact comparison at \(x\) have common gradient \(g\), evaluating one of them at \(x+h\) gives a lower bound with first-order term \(g\cdot h\). For the upper bound choose a maximizer at \(x+h\); every cluster point as \(h\to0\) maximizes at \(x\), by compactness and continuity. The Taylor error is uniform on the pieces and the gradients of these maximizers converge to \(g\), proving differentiability. Where all distinct winners separate, \(E\) is the exact multiple-winner locus. The formula requires the global-height comparison and supplies neither a simple equation in the data nor the winning parameter values.

### Analytic branches, degeneracy and faces

**Proved.** At an interior stationary point with invertible parameter Hessian, analytic implicit differentiation gives an analytic stationary graph. Its height derivative is the sample derivative of \(L\), since the parameter score vanishes. On each connected overlap, equality of two analytic heights either holds identically or has measure zero by the real-analytic zero-set theorem. At a tie with different height derivatives, the difference defines a regular hypersurface and changes sign across it. If these are the only competing branches locally, the global winner switches there. The same argument allows an analytic face height. Second countability gives countably many stationary graph neighborhoods. [Mityagin (2020)](https://doi.org/10.1134/S0001434620030189)

Fixed-parameter separation as proposed does not suffice. Take one observation from the identifiable location family proportional to \(\exp(-[(x-p)^2-1]^2)\). Distinct translates have different density peak pairs. For fixed \(p\ne p'\) the log likelihood difference has nonzero cubic coefficient \(4(p-p')\), yet for every \(x\) the two nondegenerate maxima \(p=x-1,x+1\) have the same height, for example \((-1,1)\) at the exact sample \(x=0\). A test on optimized heights or on simultaneous winning gradients is required.

**Proved.** Sard supplies a sufficient test for negligible degeneracy if the *full* derivative of \(F=D_p L\) has full row rank on \(F=0\). The score-zero manifold then has dimension \(m\); its projection to the sample chart is singular exactly at a singular parameter Hessian. Indeed its tangent equation is \(D_pF\,v+D_xF\,h=0\), and projection has a vertical kernel precisely when \(D_p F\) does. Sard makes the projected singular locus null. The rank condition needs verification. The density proportional to \(\exp(-(x-p)^4)\) gives a degenerate maximum for every one-observation sample. The compact-comparison theorem bypasses degeneracy and does not require a count of births or deaths of branches. It uses only height germs available at the sample under comparison. [Sard (1942)](https://doi.org/10.1090/S0002-9904-1942-07811-6)

**Proved.** A face enters the comparison only after identifying its actual joint envelope trace and proving its chart heights are bounded by the original supremum at nearby samples. Weak or Hellinger convergence alone does not identify this trace, and multiplying separate one-point envelopes can combine incompatible paths. Use composed derivatives for moving contacts. This permits finite/finite, finite/face and face/face comparisons whenever each type has derivative separation. Distinguish a finite winner strictly above the faces, attainment at a finite/face equality, a finite supremum attained only in the closure, and an infinite supremum. Collapse, spikes, branch ends and support contacts require the joint analysis and the correct endpoint convention. A zero-product end cannot tie a positive finite fit. [Joint closure lemma and boundary mechanisms](../ml-behaviour.tex)

## Exceptions

### Parameter symmetries

**Proved.** For a group preserving likelihood and feasibility for every sample, an entire orbit wins whenever any member does. Use invariant coordinates distinguishing orbits or one canonical representative per orbit, stratifying fixed points when needed. If the theorem separates distinct canonical winners, then almost everywhere an attained fit has exactly one winning orbit. A finite group gives \(|G|/|\operatorname{Stab}(p)|\) winning labels in that orbit. Canonicalization alone leaves the cardinality of canonical maxima open. In beta-negative-binomial, the unordered size and beta parameters are recovered from their sum and product, with the discriminant giving one label on the diagonal and two elsewhere. The model has discrete data, so the continuous theorem makes no generic canonical claim there. [Canonical orbit proof](2026-11-25-bnbinom-math-report.md#canonical-parameters-and-the-exact-orbit-test)

Reflection of both parameters and data gives a different statement. On a reflection-invariant sample, an off-fixed-point winner supplies its distinct partner. Other orbits are possible, and an asymmetric tie would require a separate equality argument. A real crossing requires comparing optimized heights of competing branches. [Tie classification](2026-11-25-ties-math-report.md#classification-of-the-constructions)

### Kinks and support contacts

**Proved.** For a finite one-dimensional piecewise-analytic decomposition, a maximizing interval contains an open interval of an analytic piece with identically zero derivative. A constant piece contributes a flat maximum only if its height wins globally. In higher dimensions a ridge can lie in a stationary set of a nonconstant smooth objective, as for the balanced Cauchy pair. If the entire maximizer graph is definable in an o-minimal structure, cell decomposition with sample coordinates first gives a definable set of samples with positive-dimensional fibers. Such a set has positive measure exactly when it has a full-dimensional cell. In one parameter dimension the fibers then consist of finitely many intervals and points. Definability must include the comparisons and feasible contacts; piecewise analyticity alone does not imply this decomposition for unbounded charts. [Van den Dries, Tame Topology and O-minimal Structures](https://doi.org/10.1017/CBO9780511525919)

- **Laplace.** The slopes of the sum of absolute residuals give the entire median set. For distinct observations the set is a point at odd sample size and the closed central gap at even sample size. Free scale profiles at the mean absolute residual for nonconstant data and keeps the same locations. The exact sample \((0,2)\) gives \([0,2]\).
- **GED with held \(0<\alpha<1\).** The sum of the absolute residuals to power \(\alpha\) is strictly concave between observation levels and strictly improves as location moves inward outside the extremes. Thus the minimizing locations are observed knots. Free scale on nonconstant data preserves the ordering, since the optimized scale is proportional to the corresponding sum raised to power \(1/\alpha\) and the optimized height decreases with it. For at least three distinct levels, the difference of two knot costs is analytic and nonidentically zero on each ordered level chamber. Hold the two knots fixed and vary a third level within its interval relative to them. Its derivative in the difference blows up as it approaches an adjacent candidate knot, while the contribution from the other knot stays bounded. The identity theorem in that interval applies even if other fixed levels are crossed during continuation of that cost difference. Thus ties are null on this chamber, also with fixed positive counts. Two distinct observations instead always give exactly the two knots, for example \((-1,1)\). Shape one is Laplace. Free shape requires a further comparison.
- **Huber with held scale \(s\) and cutoff \(\kappa>0\).** The location loss is convex and its derivative is monotone. A flat interval requires every residual to remain on a linear tail in its interior, with balanced counts by side. For an even distinct sample the entire maximizing set is the interval \([x_{(n/2)}+\kappa s,x_{(n/2+1)}-\kappa s]\) when the central gap exceeds \(2\kappa s\), and otherwise it is a singleton. A strict gap is an open condition, exemplified by \((-2,2)\) with \(\kappa=s=1\), yielding \([-1,1]\). These holds matter when profiling additional coefficients.
- **Uniform and thresholds.** A closed uniform with width \(w\) held has precisely the feasible lower-endpoint set \([x_{\max}-w,x_{\min}]\) when the span is at most \(w\). This is a segment on an open set, for example \([-1,0]\) at \((0,2)\) with \(w=3\). Free width gives the tight interval for a nonconstant sample. More generally, strict improvement up to an included contact can give a unique moving candidate, while a constant feasible branch can give a plateau. Differentiate the composed contact height in the data, as the fixed-parameter likelihood can be infeasible on one side. [Examples with included contacts](2026-11-27-tlambda-atlas-math-report.md)

### Repeated observations

**Proved.** Fix a partition and order into distinct levels \(z_1<\cdots<z_m\) with counts \(k_j>0\). Use the open level chart and its \(m\)-dimensional measure. For a smooth full-support i.i.d. density, the sample derivative vector is \((k_j\,\partial_z\log f_p(z_j))_{j=1}^m\). The theorem applies in this chart. The positive counts cancel when comparing derivatives at a level, but they remain in all nuisance score equations; intersection bounds use \(m\) and not total sample size. A null set in full sample space can contain a whole repeated-level stratum. The Cauchy circle on a balanced two-level stratum is an example, also present with two distinct observations in the full two-observation experiment. [Exact Cauchy maximizing set](2026-11-25-ties-math-report.md#classification-of-the-constructions)

## Family checklist

1. Fix the sample chart, multiplicities, holds and coefficient equivalence. Cover prospective winners, including support contacts and seams.
2. Verify locally uniform Lipschitz heights and differentiable sample germs. For full support compute \(g_p(z)=\partial_z\log f_p(z)\). Count its intersections with \(g_{p'}(z)\), or use nuisance stationarity to rule out coincident derivative vectors at distinct winners.
3. Identify joint ends and attainment separately. Include compatible face heights to claim finite/face or face/face separation, with endpoint contributions in their derivatives.
4. Specify the compact-comparison null set and any additional exceptional loci. To establish pointwise uniqueness, isolate and compare all competitive branches or prove a stronger family theorem.

## Pilot 1. Student with df held

**Proved for std, held nu.** For \(\nu>2\), the maximizing set is a singleton if the largest atom count \(k\) satisfies \(k<\nu(n-k)\), and empty otherwise. A negative exponent gives divergence, and equality gives finite nonattainment. This is exact on every stratum. The prior atlas already records the result. Gabrielsen's joint location–scale result and multimodality with scale fixed concern different optimization problems. [Gabrielsen (1982)](https://doi.org/10.1093/biomet/69.3.677) · [Reviewed atlas result](2026-11-27-tfree-atlas-math-report.md#std-held-nu)

Write \(c\) for location and \(a=\mathrm{sd}\sqrt{\nu-2}>0\). Up to constants the density is \(a^\nu[(x-c)^2+a^2]^{-(\nu+1)/2}\). The negative log likelihood is
\[
 \frac{\nu+1}{2}\sum_i \log\frac{(x_i-c)^2+a^2}{a}
 -\frac{n(\nu-1)}2\log a .
\]
On a vertical hyperbolic geodesic, \(a=e^t\) and \(c\) is fixed. The summands are logs of sums of exponentials, strictly convex when \(x_i\ne c\). On a circular geodesic \(c=b+R\tanh t,\ a=R\,\mathrm{sech}\,t\), the expression inside each log becomes
\[
 [((x_i-b)^2+R^2)\cosh t-2R(x_i-b)\sinh t]/R.
\]
This is a sum of exponentials with nonnegative coefficients and at least one positive. On the circle \(-\log a\) is strictly convex. Thus on nonconstant data the objective is strictly convex on every connecting geodesic and has at most one minimizer. The analytic sample scores of two different centers agree at at most two levels by cross multiplication, and at a common center two scales agree only there. Geodesic convexity gives the stronger all-sample result.

The peak bound makes every product vanish as \(a\to\infty\). At bounded \(a\), escape of \(c\) to either infinity also makes it vanish. For \(a\to0,\ c\to y\), the product is bounded above by a constant times \(a^{\nu(n-k_y)-k_y}\), using a positive distance bound for the noncontacts and the peak for contacts. This bound has the sharp limiting multiplier from the limiting noncontact distances, achieved by \(c=y\). Positive exponents make a positive superlevel compact; a negative exponent gives divergence. At the unique critical atom in the equality case, every geodesic ray to that atom from a finite point has \(|c-y|=O(a^2)\), or zero vertically. The product tends to the sharp positive contact value. A strictly convex function on a ray with a finite limit lies strictly above that limit at every finite time. Applying this to the negative log likelihood proves that every finite point loses to the critical trace. [Critical ray derivation](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one)

### Student with df and sd held

**Proved for std, held nu+sd.** Attainment is unconditional by continuity and decay at infinite location. For \(c\ne d\) equality of the sample scores at \(x\) requires
\[
 (x-c)[(x-d)^2+a^2]=(x-d)[(x-c)^2+a^2].
\]
The difference has quadratic coefficient \(c-d\ne0\), proving generic uniqueness on every stratum with three or more distinct levels. With one level its location is the winner. For two equally replicated levels, center at the midpoint and write \(b\) for their half-gap. The product of the two denominators is \((c^2+a^2-b^2)^2+4a^2b^2\). The midpoint wins alone when \(b\le a\); otherwise the midpoint plus or minus \(\sqrt{b^2-a^2}\) gives the exact pair. For \(\nu=3,\mathrm{sd}=1,\ x=(-2,2)\), these are \(\pm\sqrt3\). The inequality \(b>a\) is open in the two-observation experiment. For arbitrary data all maximizers occur among the finite real roots of the rational location score, whose numerator has nonzero leading term. [Atlas comparison](2026-11-27-tfree-atlas-math-report.md#std-held-nusd)

**Unequal counts at two levels, proved.** Put the heavier level at zero with count \(k\), and the lighter one at \(D>0\) with count \(l<k\). Every maximum lies in \((0,D/2)\). Outside \([0,D]\) moving inward improves both residuals; the endpoint derivatives point inward, and reflection strictly improves points closer to the lighter level. At the midpoint the location derivative points toward the heavier level. With \(v=c/(D-c)\) and \(\rho=l/k\), the score equation on this half is \(a^2(1+v)^2(kv-l)+D^2v(k-lv)=0\), or
\[
 D^2/a^2=(1+v)^2(\rho-v)/[v(1-\rho v)],\qquad 0<v<\rho .
\]
The right side decreases from infinity to zero. Its log derivative is \(2/(1+v)-1/v-1/(\rho-v)+\rho/(1-\rho v)\), and both paired differences are negative for \(0<v<\rho<1\). There is exactly one maximizing location. Every one- and two-level case is therefore classified exactly.

## Pilot 2. Skew Student, all free

**Proved.** For each \(n\), for almost every real sample, an all-free sstd MLE, if attained, is unique. The theorem holds also in level measure on every fixed stratum with at least four distinct levels, or with three equally replicated levels. Three observations require both width scores in the separation proof. One- and two-level samples give no finite all-free MLE. Finite/face equality is null on fixed strata of at least four levels.

Use join \(c\), positive left and right denominator widths \(a,b\), \(S=a+b\), and \(q=\nu>2,\ p=q+1\). The ordinary widths are \(a/\sqrt q,b/\sqrt q\); the moment formulas map bijectively to public coefficients. With \(w=a\) or \(b\) on its side the peak is \(\sqrt q\,2g_q(0)/S\), and the sample score is
\[
 G(x)=-p(x-c)/(w^2+(x-c)^2).
\]
Here \(g_q\) is the ordinary unit-scale Student density. The likelihood is piecewise analytic, with matching first derivatives at the join. On compact positive-width pieces the sample scores are uniformly bounded and Lipschitz, including across joins. At a finite stationary fit \(\sum_iG(x_i)=0\), so nonconstant data occupy both sides strictly. Writing \(h_i=|G(x_i)|,\ r_i=|x_i-c|\), differentiation in each width gives
\[
 \frac{nw}{S}=\sum_{i\ \mathrm{on\ side}} h_i r_i. \tag{1}
\]
Contact observations contribute zero to either width score. [Chart and joint envelope](2026-11-27-sstd-atlas-math-report.md#joint-envelope)

**Four or more levels.** With a common join and different \(p\), equality away from the join gives one linear equation in squared distance on each side, so at most three levels agree. With common \(p\), both sides must have the same widths since each is occupied. For distinct joins the scores have opposite signs in the gap and disagree at either join. On an exterior half-line write \(r\) for the distance to the nearer join, \(D>0\) for the join gap, \(A,B>0\) for the near and far squared widths, and \(p_n,p_f\) for the respective shape factors. Equality is equivalent to
\[
 p_f(r^2+A)(r+D)-p_n((r+D)^2+B)r=0 .
\]
The leading, quadratic and constant coefficients are \(p_f-p_n,\ D(p_f-2p_n),\ p_fAD\). Descartes' rule gives at most one positive root when \(p_f\le p_n\), including the quadratic case, and at most two when \(p_f>p_n\), since the leading and constant signs then agree. The near and far roles reverse on the other exterior. Thus at most three distinct levels agree.

**Three distinct observations.** A finite winner cannot have a join contact. In that case the two exterior scores have the same magnitude by the location equation, and (1) makes the two standardized distances \(r/w\) equal. Their common square is \(3/(2q-1)\) by the summed width equation. At fixed join and denominator widths, twice the shape derivative per observation is
\[
 \psi((q+1)/2)-\psi(q/2)-\tfrac23\log((q+1)/(q-1/2)).
\]
The digamma difference is \(2\int_0^\infty e^{-qu}/(1+e^{-u})\,du>1/q\), from the digamma difference integral. The difference between \(1/q\) and the log term is positive, since its derivative is \(-1/q^2+1/[(q+1)(q-1/2)]<0\) and it tends to zero. Thus increasing \(q\) improves the likelihood. [Student density derivative and digamma integral](2026-11-27-sstd-atlas-math-report.md#six-level-sample-and-exact-separation)

Off contacts the common score signs specify a common split. By reflection take \(x_1<c<x_2<x_3\). Let \(h_1=h_2+h_3,\ \delta=x_3-x_2,\ u=(x_2-c)/\delta,\ \lambda=h_2/h_1\in(0,1)\) and \(M=\delta(u+1-\lambda)\). Equation (1) gives \(a/b=(c-x_1)/M\). With \(B=b^2\), the right score equations solve as
\[
 \frac{p}{h_1\delta}=\frac{(2u+1)\lambda(1-\lambda)}{\lambda+(2\lambda-1)u},
 \quad
 \frac{B}{\delta^2}=\frac{u(u+1)[1-\lambda+(1-2\lambda)u]}{\lambda+(2\lambda-1)u}.
\]
The common denominator cannot vanish at a finite solution since the first numerator is positive. The left score equation is \(p=h_1(c-x_1)(1+B/M^2)\). Substituting gives
\[
 (x_2-x_1)/\delta=u+F_\lambda(u),\qquad
 F_\lambda(u)=\frac{(2u+1)(u+1-\lambda)^2}{A_\lambda(u)},\qquad
 A_\lambda(u)=4u^2+(5-2\lambda)u+1-\lambda .
\]
Its derivative is
\[
 F'_\lambda(u)=\frac{(u+1-\lambda)(8u^3+12u^2+3u-1+\lambda)}{A_\lambda(u)^2}>-1
 \quad (u>0).
\]
Indeed \(A_\lambda(u)>u+1-\lambda\); when the second numerator factor is negative its absolute value is at most \(1-\lambda\). Therefore \(u+F_\lambda(u)\) is strictly increasing. The score vector gives at most one \(u\), hence one join, and the right equations and (1) identify all other parameters. [Exact algebra checker](2026-11-27-uniq-generic-math-data/check_algebra.py)

**One or two levels.** Constants permit unbounded collapse. With two levels and arbitrary positive counts, location stationarity equates the count-weighted score magnitudes on the two sides. By (1) their widths are proportional to their unstandardized distances. Thus both standardized distances are equal, with squared size \(1/q\), or exactly one in ordinary units. Keeping the join and ordinary widths fixed, both densities increase strictly with \(q\). Twice the derivative of \(\log g_q(1)\) is the digamma difference above minus \(\log(1+1/q)\), and \(1/q>\log(1+1/q)\). Thus no finite stationary point can win.

**Face checks, proved.** The envelope uses the two-piece \(t_2\) and normal, the two half-shape families and their corners, and collapse at sample atoms. Collapse vanishes at strict \(2(n-k)>k\), gives infinite supremum if \(2(n-k)<k\), and has an explicit joint trace at criticality. Other vanishing ends cannot compete with a positive finite product. Attainment is a finite stationary comparison with the full boundary height, admitting equality. [Exhaustive boundary and attainment results](2026-11-27-sstd-atlas-math-report.md#attainment-and-candidate-sets)

- For two positive widths at \(q=2\) the same score comparison applies, including the three-observation equations, since a winning finite candidate has \(q>2\) and, at three observations, no join contact.
- The normal has linear sample scores on each side. With a common join there is at most one noncontact equality per side with a Student. For distinct joins the exterior polynomials, with \(B,C>0\), are \((r+D)(r^2+B)-Cr\) when the Student is nearer and \(r((r+D)^2+B)-C(r+D)\) when the normal is nearer. These give at most two and one positive roots, respectively. For three observations at the same sign split used above, the normal right scores force \(\lambda=u/(2u+1)\). Its squared widths are \((c-x_1)/h_1,\ \delta(2u+1)/h_1\). Its width ratio, which again obeys (1), gives \((c-x_1)/\delta=(u+1-\lambda)^2/(2u+1)=F_\lambda(u)\) at this \(\lambda\). Strict monotonicity forces a common join, where that \(\lambda\) makes the Student denominator zero. Thus finite/normal separation also holds for three distinct observations.
- A right half with positive winning height has its join at the sample minimum, since moving the join there at fixed width improves supported noncontacts. The restricted half-model heights follow this moving contact. Their derivative at the minimum level is minus the sum of all other level derivatives by translation invariance, hence positive. The other levels contribute negative scores from the broad kernel. A finite candidate with that vector has its join strictly between the minimum and next level. With at least four levels the broad side has at least three comparisons on a single exterior half-line, whereas the polynomials above allow at most two. The left half follows by reflection. This includes the half-normal and half-\(t_2\) corners.
- At critical multiplicity with all coefficients free, the optimized collapse log trace is a stratum-dependent constant minus \(3\sum_{x_i\ne y}\log|x_i-y|\), with critical atom \(y\). At noncontacts the per-observation score is \(-3/(x_i-y)\). With distinct finite join \(c\) and atom \(y\), equality with the finite score is impossible strictly between them. On the exterior where \(c\) is nearer, cross multiplication gives \((p-3)r^2+pDr-3w^2=0\); on the exterior where \(y\) is nearer it gives \((p-3)r^2+(p-6)Dr-3(D^2+w^2)=0\), with \(r\) measured from the nearer point and \(D=|c-y|\). Each has one positive root, since the leading and constant signs are opposite. If \(c=y\), the equation is \((p-3)r^2=3w^2\). The collapse contact can add at most one equality level in any case. Four levels therefore separate these gradients too.

**Exceptional sets and exact witness.** Equal replication multiplies the three-observation log likelihood by a positive constant, so its separation, face comparisons and spacing-ratio result transfer without changes to the candidate parameters. The compact-envelope test describes the exact finite tie locus on the separation strata, with finite candidates in the comparison. For three observations there are at most countably many exceptional spacing ratios \((x_2-x_1)/(x_3-x_1)\) after sorting. Normalize the extremes to \(0,1\) by affine equivariance. Different stationary sample score vectors must differ at the middle observation because they each sum to zero and \(\sum_i x_iG(x_i)=-3\) by stationarity and (1). On a compact parameter piece, the second derivative in the middle observation is uniformly bounded below on either side of a join, with matching first derivative at the join. Adding a common quadratic to the heights therefore makes their compact comparison envelope convex. A one-variable convex function has only countably many nondifferentiability points, by monotonicity of the one-sided derivatives. Taking the countable union gives the claim.

For an exact dyadic tie take \(M=512\) and \((-512,(-7/4)^{[M]},(-1)^{[10M]},1^{[10M]},(7/4)^{[M]},512)\), where brackets denote counts. The prior proof bounds both regular shape faces, both half faces and the reflection-fixed family strictly below a finite witness; collapse vanishes. A global winner is finite and off the fixed set, so its reflection is another winner. The replay of the 35,819-box outward cover and the 80-decimal-digit interval comparisons passed. The number of winning orbits is unresolved. [Exact witness proof](2026-11-27-sstd-atlas-math-report.md#six-level-sample-and-exact-separation)

## Pilot 3. Prentice generalized gamma, all free

**Proved.** For each \(n\), for almost every positive sample, an all-free gengamma MLE, if attained, is unique. This holds in level measure on every fixed stratum with at least three levels, and there finite/face equality is also null. Constant samples have infinite supremum. All two-level samples have finite unattained supremum, so \(n\le2\) has no MLE.

In \(y=\log x\), drop the data-only Jacobian. For \(Q\ne0\) use \(p=Q/\sigma,\ k=Q^{-2},\ A=k e^{-p\mu}>0\), a bijection in each sign chart. Then
\[
 \log f(y)=\log|p|+k\log A-\log\Gamma(k)+kp y-Ae^{py},\qquad
 G(y)=kp-Ap e^{py}.
\]
At \(Q=0\) use the normal chart with score \((\mu-y)/\sigma^2\). Each chart is analytic in its own coordinates; the attained seam is handled as a separate candidate family. Smooth invertible level transformations preserve null sets locally. [Prentice coordinates](2026-11-10-gg-theory-report.md#coordinates-and-likelihood)

For distinct exponents the score difference is a constant plus two exponentials with distinct nonzero rates. Its derivative has at most one zero, by dividing by either exponential, so the difference has at most two zeros. With equal exponent it is constant plus one exponential, and identity forces \(A,k\) to agree. A line versus the nonconstant exponential plus a constant has at most two intersections since the second derivative of their difference is never zero. Two normal scores agree identically only with identical parameters. Three distinct levels therefore suffice.

**Faces and attainment, proved.** For nonconstant log data, set \(t=p s\), where \(s^2=n^{-1}\sum(y_i-\bar y)^2\), and \(K(t)=\log(n^{-1}\sum e^{t(y_i-\bar y)/s})\). The signed profile maximizes first in \(A\) at \(A=k/(n^{-1}\sum e^{py_i})\), then solves \(\log k-\psi(k)=K(t)>0\). The left side is strictly decreasing from infinity to zero, giving the unique optimum. Thus every nuisance route is bounded by that signed profile, whose tails are the upper-endpoint power and lower-endpoint Pareto traces and whose seam is the fitted normal. On the \(y\)-scale the two optimized face mean log heights are
\[
 -1-\log(y_{\max}-\bar y),\qquad -1-\log(\bar y-y_{\min}).
\]
They are the reachable fitted one-sided exponential heights. On each face, noncontact derivative per observation is constant across levels. A finite log-data score is strictly decreasing in \(y\), so it cannot agree at two noncontact levels. Face/face equality is exactly \(y_{\min}+y_{\max}=2\bar y\), a proper hyperplane on any level stratum with an interior level. On constant samples the normal collapses. [Profile and signed limits](2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate) · [Balanced-pair proof and Lean statement](2026-11-26-gengamma-twopoint-report.md#proof-and-implementation)

**Two levels with arbitrary counts, proved.** An affine change of log data reduces the levels to \(-1,1\). Write \(\epsilon\) for the smaller empirical fraction and \(K_w(t)=\log(w e^t+(1-w)e^{-t})-(2w-1)t\), with \(w\) the fraction at \(1\) and \(t\) now the exponent in these units. For either sign, \(K_w\) is concave as a function of \(w\) and zero at 0 and 1. Chords to \(w=1/2\) give \(K_w(t)\ge 2\epsilon\log\cosh t\). The mean log likelihood after optimizing \(A\) is \(\log|t|+B(k)-kK_w(t)\), where \(B(k)=k\log k-k-\log\Gamma(k)\). The bound \(B'(k)=\log k-\psi(k)<1/k\) follows by applying strict Jensen to \(\log(1/Z)\) for \(Z\sim\mathrm{Gamma}(k+1,1)\), since \(E[1/Z]=1/k\) and \(E[\log Z]=\psi(k+1)=\psi(k)+1/k\). Integrating from \(2\epsilon k\) to \(k\) gives \(B(k)-B(2\epsilon k)\le-\log(2\epsilon)\). Consequently every finite signed height is at most
\[
 \log|t|+B(2\epsilon k)-(2\epsilon k)\log\cosh t-\log(2\epsilon).
\]
The first three terms are a balanced-pair height and are strictly less than \(-1\). For completeness, when \(0<|t|\le3\), \(\log\cosh t/t^2\) decreases with \(|t|\) by the decreasing second derivative of \(\log\cosh t\) on the positive half-line. It is at least \((3-\log 2)/9>e/(4\pi)\). Stirling's lower bound for \(\Gamma\), followed by maximization in shape, gives balanced height at most \(\log|t|-\frac12\log(4\pi e\log\cosh t)<-1\). For \(|t|\ge3\) the optimizing balanced shape \(b\) satisfies \(b<1/\log\cosh t\) by the same bound on \(B'\). Using \(\log\Gamma(1+b)\ge-\gamma_E b\) by log convexity, and \(\log\cosh t\ge |t|-\log2\), the excess above \(-1\) is at most
\[
 \log(|t|b)+1-|t|b+b[\log(2b)+\gamma_E-1]<0.
\]
The bracket is negative because \(2b<2/(3-\log2)<1\) and \(\gamma_E<1\). At the normal seam the optimized height for weights \(w,1-w\) is \(-\tfrac12\log(2\pi e\,4w(1-w))<-1-\log(2\epsilon)\), since \(2\pi(1-\epsilon)/\epsilon>e\). Thus every finite height loses strictly to the best face, of height \(-1-\log(2\epsilon)\). The best face ends at the heavier level, with both faces winning for equal counts. [Balanced-pair derivation](2026-11-25-gbshape-math-report.md#generalized-gamma)

**Exact comparison and witness.** The signed profiles have finite stationary lists by their nonzero near-seam and far-tail score signs and analyticity on the intervening compact intervals. Take the largest height over the roots, the fitted lognormal and both faces, and keep all finite candidates at this level. This describes the full maximizing set by exact-real comparison. The compact-envelope test also describes the multiple-finite locus on strata with at least three levels. Neither comparison limits the tie locus to reflection-symmetric samples. [Finite root theorem](2026-11-25-trgamma-math-report.md#signed-exteriors-and-finite-root-count)

For the exact sample \((1/4,(1/2)^{[10000]},2^{[10000]},4)\), use base-two log units. Both face heights equal \(-1-\log 2\), strictly below the fitted normal. At signed power \(p=\log 3\) in these units and \(k=1\), the height after optimizing \(A\) is \(\log\log 3-1-\log[(10000(5/3)+41/9)/10001]\), above the normal since \((\log 3)\sqrt{2\pi/e}>1.6670>(10000(5/3)+41/9)/10001\). The rational checker verified the inequalities. The winning \(Q\) is finite and nonzero, and reflection of the log sample gives its distinct partner with opposite \(Q\). The total orbit count is unresolved. [Exact witness](2026-11-27-uniq-atlas-math-report.md#gengamma-held-all-free) · [Rational checks](2026-11-27-uniq-atlas-math-data/exact_checks.py)

## What this changes for the fitter

An almost-everywhere claim supplies no pointwise certificate for binary64 observations or repeated samples. A global enclosure excluding all competing branches, together with uniqueness in the remaining maximizing basin or a verified exclusion of the exceptional set, can certify singleton cardinality. Held-df joint Student already has pointwise uniqueness from its attainment theorem. A proved symmetry supplies labels from a known orbit, while complete reporting requires eliminating additional orbits and determining stabilizers. A tied excluded face contributes no additional finite maximizer.

If a value proof closes but the cardinality is unresolved, retain unknown uniqueness. If a global comparison, sign or certificate budget cannot close and the underlying search succeeds with its accuracy checks, retain the fit marked uncertified. Stop when a proof rules out that result or when the underlying search fails its documented checks. A numerical height tolerance cannot decide exact equality.

## Open

- sstd and gengamma: find simpler sample equations for the finite tie loci, settle asymmetric finite co-winners, and count all winning orbits for the two reflection witnesses. The compact-envelope test uses the global heights and is not an effective arithmetic predicate.
- sstd: the three-level width calculation leaves unequal three-level multiplicities open. At three distinct observations the finite/half-face comparison remains open. All-free effective score isolation and exact general height comparison remain open.
- std with df and sd held: identify the higher-level exceptional locus explicitly. Held df alone has no remaining cardinality frontier.
- gengamma: effective root isolation and exact height comparison on the exceptional set are separate from generic uniqueness. All two-level count patterns are covered by the nonattainment proof.

## Checks

Workstation checks used one slot at a time. The new symbolic checker verified the Student product, skew-Student intersections, and the three-observation elimination and derivative identities. The inherited generalized-gamma rational comparison and skew-Student outward witness cover passed. The four proposed atlas rows include the supplemental held-df-and-sd result. No builds, package tests or source changes were required for the research-only job. [Proof rows](2026-11-27-uniq-generic-math-data/proofs.csv)

Reproduce from the worktree root.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export PYTHONDONTWRITEBYTECODE=1
eval_dir=dev/research/evaluations
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-uniq-generic-math-data/check_algebra.py"
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-uniq-atlas-math-data/exact_checks.py"
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-sstd-atlas-math-data/check.py"
/tmp/umvl-cpu/run python3 "$eval_dir/2026-11-27-uniq-generic-math-data/validate.py"
```
