# Tukey lambda U-branch ties and free-shape likelihood bounds

TASK-348 · Base `f25d97e2`

The free-scale U branch can have exactly two finite global maximizers. An exact real sample below has that outcome, and two adjacent binary64 inputs select opposite winners. For algebraic samples, every rational shape is a nonzero-score cut, and likelihood comparisons at rational shapes are algebraic. The general exact root-multiplicity and critical-height decisions are effective conditional on Schanuel's conjecture. On the fitted branch, an explicit modulus controls boxes that cross moving support boundaries below shape one, and a shared location and scale gradient tightens boxes with positive likelihood throughout. [U-branch reduction](2026-11-25-tl-math-report.md#free-scale) · [Prior isolator](2026-11-25-tlroot-math-report.md#acceptance-conditions) · [Tie policy](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

## Free-scale U branch

Consider the optional range \(1<\lambda<2\) with free scale and positive support radius \(R\). With location free, the prior nuisance reduction gives the unique fixed-shape fit \(\mu=(x_-+x_+)/2\) and \(R=(x_+-x_-)/2\). With location held, \(R=\max_i|x_i-\mu|\). In either case \(\sigma=\lambda R\). Set \(t_i=|x_i-\mu|/R\), let \(k\) count contacts \(t_i=1\), and let \(p_i\le1/2\) solve \((1-p_i)^\lambda-p_i^\lambda=t_i\) for each noncontact. Write \(q_i=1-p_i\) and \(D_i=p_i^{\lambda-1}+q_i^{\lambda-1}\). With \(J=2^k(2R)^{-n}\), the prior scalar profile gives
\[
 V=P/J=\frac{2^{n-k}}{\lambda^n\prod_{t_i<1}D_i},\qquad
 H=\log V,\qquad S=H'.
\]
The analytic extension to \([1,2]\) has \(H(1)=0\), \(H(2)=-k\log 2<0\), and finitely many score roots. The attained uniform at shape one has likelihood \(J/2^k\).

### Exact arithmetic and strict comparisons

**Proved.** If the normalized sample is algebraic and \(a\in[1,2]\) is rational, then \(S(a)\ne0\). To see this, write \(a=A/B\) in positive integers. Introduce positive \(u,v\) with \(u^B=p_i\), \(v^B=q_i\). The equations \(u^B+v^B=1\) and \(v^A-u^A=t_i\) have a unique positive solution in the specified probability range, so \(u,v\) are algebraic. A singleton defined by polynomial equalities and inequalities over the real algebraic numbers has algebraic coordinates by real quantifier elimination. The inverse derivative is
\[
 p_i'=-\frac{p_i^\lambda\log p_i-q_i^\lambda\log q_i}{\lambda D_i}.
\]
At \(a\), all coefficients in \(D_i'/D_i\) are algebraic when it is expanded as a linear form in \(\log p_i,\log q_i\). The score is \(-n/a-\sum D_i'/D_i\). Reduce the logarithms to a basis over \(\mathbb Q\). Baker's theorem makes that basis together with 1 linearly independent over the algebraic numbers. The nonzero constant \(-n/a\) prevents a zero score. This applies to normalized binary64 data and holds when the distances are formed exactly. In particular an arbitrary rational partition cut, including either endpoint, has a sign that convergent enclosures can eventually determine. The argument gives no work bound for that sign. [Baker's linear forms theorem](https://www.cambridge.org/core/journals/mathematika/article/linear-forms-in-the-logarithms-of-algebraic-numbers-iii/C3A7D8C6F454E237D6A3FBAD8DBD006D)

At both endpoints \(p_i=(1-t_i)/2\) and \(D_{i,p}=0\). Useful starting signs are
\[
 S(1)=-n-\tfrac12\sum_{t_i<1}\log(p_iq_i),\qquad
 S(2)=-n/2-\sum_{t_i<1}(p_i\log p_i+q_i\log q_i).
\]
A positive \(S(1)\) proves finite attainment strictly above \(J\). A negative \(S(1)\) supplies a strict exclusion collar next to 1 once \(S<0\) is enclosed there. If there are only contacts and centers, \(H=-n\log\lambda+c(\lambda-1)\log2\) with \(c\) centers. Its strict convexity and the endpoint values give \(H<0\) on the open branch. For example \((-1,0,1)\) has an unattained U junction.

**Proved.** The same algebraic construction decides the sign of \(V(a)-1\), and compares \(V(a)\) and \(V(b)\) at any two rational trial shapes. If \(V(a)=1\) at an interior rational anchor on an algebraic sample, its nonzero score proves some nearby likelihood is strictly above \(J\). Conversely, a strict finite improvement has a rational witness by continuity. Thus enumerating rational trial shapes gives a one-sided test for strict finite improvement, including an exact equality encountered at a trial. For an example of the latter, take \((-1,1,t^{[4]})\), where brackets on superscripts denote multiplicity, and
\[
 d^2=32/27,\qquad
 t=\frac{1+d^2}{2}\sqrt{2-d^2}=\frac{59}{54}\sqrt{22/27}.
\]
At shape \(3/2\) the two positive numbers \(\sqrt p,\sqrt q\) have sum \(d\), sum of squares 1 and difference \(\sqrt{2-d^2}\). Their cube difference gives exactly \(t\), and \(V=(2/3)^6(2/d)^4=1\). The U fit is attained strictly above \(J\).

**Proved.** Consecutive stationary roots have strictly ordered heights. Between them the analytic score is nonzero and has constant sign. The same strict comparison applies across any chain with only even-multiplicity roots in between, by integrating the common sign except at finitely many points. The first stationary root is strictly above or below \(J\) according to the sign of \(S\) between 1 and that root. Once the relevant score signs and intervening root counts are certified, these comparisons require no height arithmetic. Even score multiplicity gives no local maximum. An unresolved pocket can also be discarded without counting its roots if its whole height upper bound is strictly below a competing lower witness.

### A scalar junction threshold

**Proved.** Consider samples whose absolute normalized distances are contacts, centers and \(m>0\) copies of one level \(t\in(0,1)\). The counts and support radius stay fixed as \(t\) varies. Set \(G(\lambda,t)=H(\lambda,t)/(\lambda-1)\), with \(G(1,t)=S(1,t)\). For \(1<\lambda<2\),
\[
 H_t=m\,\frac{D_p}{\lambda D^2}>0,qquad
 D_p=(\lambda-1)(p^{\lambda-2}-q^{\lambda-2}).
\]
This follows from \(p_t=-1/(\lambda D)\) and \(0<p<1/2\). At \(\lambda=1\), \(G_t=mt/(1-t^2)>0\). At 2 the value of \(G\) is the negative constant \(-k\log2\). The maximum \(M(t)=\max_{[1,2]}G(\lambda,t)\) is continuous for \(0\le t<1\). At zero the contact/center convexity argument gives \(M(0)\le-k\log2\), and \(S(1,t)\to+\infty\) as \(t\uparrow1\). At any zero of \(M\), its maximizing shapes are below 2. Strict pointwise monotonicity there, and compactness for all smaller \(t\), give a unique threshold \(t_*\). Below it the junction strictly dominates. Above it some finite shape beats the junction. At it the interior maximizers of \(G\) give all finite ties at \(J\), with a junction-only outcome if \(G\) maximizes just at 1. The latter case requires
\[
 t_*^2=1-2^{2(m+c)/m}e^{-2n/m}
\]
by \(S(1,t_*)=0\), and cannot occur at an algebraic \(t_*\). Ordered enclosures of a threshold and a sample level decide the strict cases. A witness of exact equality can specify the threshold by its defining conditions and an isolating interval.

### Exact fold and junction witnesses, and adjacent inputs

**Proved with interval-checked signs.** The earlier real fold is on \((-1,1,t^{[19]},0^{[16]})\). Its exact \(t\) is the unique zero of the maximum of \(S\) on \([477966,478031]/262144\) in the interval \((0.7425450262943313027831787956,0.7425450262943313027831787957)\). The maximum has \(S'=0\), \(S''<0\), and negative \(H\), so the score has a double root and the junction wins. The earlier exact junction tie uses \((-1,1,t^{[110]},0^{[35]})\). Its exact \(t\) is the unique zero of the maximum of \(H\) on \([419455,419520]/262144\) in \((0.7500888321531553476973054941,0.7500888321531553476973054942)\). It has one finite maximum equal to \(J\) and a lower stationary minimum. This \(t\) is the scalar threshold just proved. Both statements use the earlier complete cover. [Definitions and proof of the two real witnesses](2026-11-25-tlroot-math-report.md#interval-results-and-exact-transition-samples)

The new 70-decimal interval run lifts the hexadecimal observations to their exact dyadic values and covers the entire shape interval. The two finite roots, where present, have bracket widths below \(10^{-25}\). Heights in the table are rounded from the checked signs. [Binary64 replay script](2026-11-25-tlambda2-math-data/u_events.py) · [Interval output](2026-11-25-tlambda2-math-data/u_events.txt)

| Path | Exact varied observation \(t\) | Interior roots | Highest stationary \(H\) | U outcome |
|---|---|---:|---:|---|
| Fold | `0x1.7c2edc977bb4bp-1` | 2 simple | \(-1.3790710\) | Unattained junction |
| Fold | `0x1.7c2edc977bb4cp-1` | 0 | | Unattained junction |
| Junction | `0x1.800ba4b662a11p-1` | 2 simple | \(-3.169\times10^{-15}\) | Unattained junction |
| Junction | `0x1.800ba4b662a12p-1` | 2 simple | \(+4.100\times10^{-16}\) | Unique finite fit |

### Exactly two finite global maximizers

**Proved with interval-checked signs.** Use the sample
\[
 X(t)=\big((-1)^{[10]},1^{[10]},0^{[350]},t^{[1100]},1-2^{-512}\big).
\]
It has \(n=1471\), \(k=20\), \(\mu=0\), and \(R=1\) in the U reduction. Define \(t_f\) by the exact conditions below. The two shape collars are
\[
 I_1=[1058972,1059005]/1048576,\qquad
 I_2=[1667557,1667590]/1048576 .
\]
For \(j=1,2\), let \(a_j(t)\) be the unique score root on \(I_j\). Then \(t_f\) is the unique zero of \(\Delta(t)=H(a_2(t),t)-H(a_1(t),t)\) in
\[
 (0.7511416680063824861674766027,\ 0.7511416680063824861674766028).
\]
The interval run used 185 decimal digits. On both collars it proved opposite endpoint score signs in order \(+,-\), negative \(S'\), and positive \(H\), uniformly across a parameter range containing the defining interval and both binary64 neighbors below. Therefore the two roots are unique and analytic in that range. Differentiation at the roots cancels the shape-derivative term, giving \(\Delta'=H_t(a_2,t)-H_t(a_1,t)\). The displayed formula for \(H_t\) applies to the varied group alone. The run bounds \(\Delta'\) between 309.1 and 309.3 and gives opposite signs for \(\Delta\) at the two rational endpoints, about \(-6.02\times10^{-27}\) and \(+2.49\times10^{-26}\). Continuity and strict monotonicity prove existence and uniqueness of the specified exact \(t_f\).

The complement of the two collars has exactly one stationary root, with \(S'>0\) and \(H\) near \(-1.3742675\). Both collar roots are above \(J\), and shape 2 lies below \(J\). Thus the complete global maximizing set at \(X(t_f)\) consists of \((0,a_1,a_1)\) and \((0,a_2,a_2)\), in \((\mu,\sigma,\lambda)\) order. Their shapes are about \(1.00993045255108\) and \(1.59032233154165\), and the checked root brackets are narrower than \(10^{-12}\). The common total log gain over \(J\) is about 0.56828. Splitting the 1100 copies between \(-t_f\) and \(t_f\), 550 each, preserves the entire U profile. This gives the same two U maximizers on an exact sample whose largest tie is below half. On the principal fitted branch the split sample has an attained maximum by the dossier's strict tie condition. The unsplit sample and the unsplit fold and junction examples have a strict majority tie and are unbounded on that branch. [Finite-tie proof driver](2026-11-25-tlambda2-math-data/finite_tie.py) · [Interval output](2026-11-25-tlambda2-math-data/finite_tie.txt)

**Proved with interval-checked signs.** Translate \(X(t)\) by \(-1\). The exceptional observation is then exactly \(-2^{-512}\) in binary64. The two adjacent binary64 values of the varied raw observation \(x=t-1\) enclosing the crossing give the following outcomes. The script forms \(t=1+x\) exactly. Rounding the normalized exception to a contact would change the likelihood profile.

| Exact varied raw observation \(x\) | Total log height, late minus early | U outcome |
|---|---:|---|
| `-0x1.fda96fe9ff619p-3` | \(-7.3289963\times10^{-15}\) | Unique early root |
| `-0x1.fda96fe9ff618p-3` | \(+1.2527356\times10^{-15}\) | Unique late root |

The same collar argument supplies an exact real crossing predicate on another sample path if the two roots have signed isolators, the difference of their profiled height derivatives has a strict sign, and the endpoint height gaps have opposite signs. A full cover or upper bound on the remaining regions is required to turn the crossing into a global tie.

### Conditional decisions for general algebraic data

**Proved conditional on Schanuel.** For rational inputs, the three exact decisions are instances of the first-order decision procedure of Macintyre and Wilkie for the ordered real exponential field under Schanuel's conjecture. Introduce the positive probabilities and their logs using the inverse equations above and \(e^{\log p_i}=p_i\). Powers are products inside exponentials. With those variables, \(V\), \(S\) and \(S'\) are finite expressions in the field. For derivatives, apply \(\partial_\lambda+\sum_i p_i'\partial_{p_i}\), with the displayed formula for each \(p_i'\). Algebraic constants can instead be specified by their polynomials and rational isolators. The following are closed sentences once the input is fixed.

| Decision | Predicate with shapes in \((1,2)\) |
|---|---|
| Multiple score root | \(\exists a\; S(a)=S'(a)=0\) |
| Equal finite critical heights | \(\exists a<b\; S(a)=S(b)=0,\ V(a)=V(b)\) |
| Critical height equal to the junction | \(\exists a\; S(a)=0,\ V(a)=1\) |

Rational root brackets can be included in the predicates. Query successively for at least \(j\) ordered distinct roots to count them, which terminates by the prior analytic finiteness. Then use rational subdivisions to isolate them in disjoint brackets, query derivatives for their multiplicities, and compare \(V\) within the root brackets. Analyticity and \(H(2)\ne H(1)\) guarantee finite multiplicities. This gives a conditional exact decision, including global comparison, with neither a practical budget nor an unconditional equality oracle. [Wilkie on the conditional decision procedure](https://doi.org/10.1007/978-94-015-8923-9_11)

**Proved conditional on Schanuel.** There is also a direct exclusion for an algebraic sample with contacts and just one distinct absolute distance in \((0,1)\) and no centers. Let \(t\) be that distance of multiplicity \(m\). Put \(p+q=1\), \(X=p^\lambda\), \(Y=q^\lambda=X+t\), \(u=\log p\), and \(v=\log q\). At a stationary point at \(J\),
\[
 D=X/p+(X+t)/q=2\lambda^{-n/m}.
\]
Thus \(\lambda\) is algebraic over the field generated by \(p,X\) and the algebraic numbers. Stationarity gives a linear equation in \(u,v\) over that field with \(\lambda\) adjoined, whose right side is nonzero because of \(n/\lambda\).

If \(u,v\) are dependent over \(\mathbb Q\), a nontrivial multiplicative relation in \(p,1-p\) makes \(p\) algebraic. The score makes both logs algebraic over the field generated by \(X\) and algebraic numbers. Since \(\lambda\) is irrational by the rational-cut theorem, the exponents \(u,\lambda u\) are independent, yet all exponents and their exponentials lie in a field of transcendence degree at most one. This contradicts Schanuel. If \(u,v\) are independent, consider \(u,v,\lambda u,\lambda v\). Rank four contradicts Schanuel because the field generated by these and their exponentials has transcendence degree at most three, using the score to solve for one log. In lower rank a nontrivial rational relation gives a second linear equation in \(u,v\) with coefficients affine in \(\lambda\). Its row is nonzero by irrationality and independent of the score row because its right side is zero. Both logs are then algebraic over the field of \(p,X\), giving transcendence degree at most two. Rank three is excluded. At rank two, multiplication by \(\lambda\) preserves the two-dimensional rational span of \(u,v\). Its characteristic polynomial makes \(\lambda\) algebraic. Now \(D=2\lambda^{-n/m}\) is algebraic, and the equation for \(D\) makes \(X\) rational in \(p\) over algebraic coefficients. The transcendence degree drops to at most one, contradicting rank two. This excludes even a nonglobal stationary point at \(J\) on this sample class, so the scalar threshold for this class is transcendental under Schanuel. The prior junction example has centers and lies outside the exclusion's hypotheses. [Schanuel rank criterion](2026-11-25-ties-math-report.md#what-the-transcendence-results-can-decide)

## Correlated bounds on the fitted branch

### A probability and density modulus through moving support

**Proved.** Write \(b=1-\lambda\in[0,2]\) and use \(D(p)=p^{-b}+(1-p)^{-b}\) on the fitted branch. For a standardized residual magnitude \(z\ge0\), let \(p(z,\lambda)\) be the smaller inverse probability, extended to zero on and beyond a finite endpoint. Inside support,
\[
 z=\int_p^{1/2}D(v)\,dv,\qquad
 |p_z|\le1/2,\qquad
 |p_\lambda|
 \le \frac{\int_p^{1/2}D(v)(-\log v)\,dv}{D(p)}
 \le c_0=\frac{1+\log2}{2}.
\]
The first inequality uses \(D\ge2\). For the shape bound, each log in \(\partial_\lambda D(v)\) has magnitude at most \(-\log v\), and \(D(v)\le D(p)\) for \(p\le v\le1/2\) since \(b\ge0\). At a finite support boundary the inverse tends continuously to zero. For fixed shape, the clipped part in \(z\) is a terminal interval. For fixed \(z>0\), clipping starts at \(\lambda=1/z\) if that shape lies in range, so continuity and the interior derivative bound extend each inequality across clipping. At \(z=0\) the inverse is constant. The signed inverse \(u\) obeys the same two bounds by reflection, with continuity at zero residual.

Use \(A=1/\sigma, B=-\mu/\sigma\), so signed residuals are \(z_i=x_iA+B\). The change in the smaller probability between two parameter points is bounded by
\[
 w_i=\min\left\{\tfrac12,\ \tfrac12|x_i\Delta A+\Delta B|
                            +c_0|\Delta\lambda|\right\}.
\]
The affine residual uses a common location and scale for the entire sample. On the slab \(-1\le\lambda\le1-\delta\), \(0<\delta\le1\), set \(g(p,\lambda)=1/D(p)\), with \(g(0,\lambda)=0\). At fixed shape,
\[
 |\partial_p g|\le b p^{b-1}.
\]
For \(\delta\le b\le1\), integration gives a modulus \(|\Delta p|^b\le|\Delta p|^\delta\). For \(1\le b\le2\), the derivative is bounded by 2. At fixed \(p\), \(|\partial_\lambda g|\le p^\delta(-\log p)\le1/(e\delta)\), by \(g\le p^b\) and the log-weighted expression for the derivative of \(D\). These bounds extend to \(p=0\) by continuity. Hence the standardized density modulus is
\[
 |\Delta g_i|\le \omega_i=\max(w_i^\delta,2w_i)+|\Delta\lambda|/(e\delta).
\]
On a compact held-location shape interval with lower endpoint below \(-1\), replace 2 in the term \(2w_i\) by \(1-\lambda_{\min}\). The probability bound is unchanged there.

For a box with anchor \(c\), maximize \(|x_i\Delta A+\Delta B|\) over its permitted affine offsets. A rectangular box uses \(|x_i|h_A+h_B\), with \(h_A,h_B\) the coordinate radii. Holds can be incorporated directly as \(B=-\mu A\) or fixed \(A\). Using \(A_+\) for the box maximum of \(A\), a likelihood upper bound is
\[
 U_{\rm box}=A_+^n\prod_i\min\{1/2,\ g_i(c)+\omega_i\}.
\]
The anchor can be evaluated with a checked inverse, or by setting the corresponding density to zero where its support exclusion is proved. In particular the bound works even when some point in the box has zero likelihood.

### Uniform collar and an explicit subdivision bound

**Proved.** The exact sample \((-1,1)\) rules out a two-sided continuous likelihood modulus on the whole closed branch. At \(\mu=0,\ \sigma=\lambda\uparrow1\), its likelihood is zero because both observations contact the support endpoints below shape one. At \((0,1,1)\) the closed-support likelihood is \(1/4\), and this is the proved unique principal-branch maximum. [Distinct-pair proof](2026-11-21-tlambda-ties-report.md#distinct-pairs)

For free scale and smallest allowed support radius \(R_*>0\), the uniform optimum is \(U=(2R_*)^{-n}\). At positive likelihood with \(1-\delta\le\lambda\le1\) and \(\delta\le1/2\), support enforces \(\sigma\ge\lambda R_*\) and the modal bound is \(g\le2^{\lambda-2}\). Since \(2^{\lambda-1}/\lambda\) decreases on \((0,1]\),
\[
 L\le U\left[\frac{2^{-\delta}}{1-\delta}\right]^n\le U e^{2n\delta}.
\]
Thus an interior incumbent strictly above \(U\) can strictly exclude the strip by choosing \(\delta\) with \(\log U+2n\delta\) below its certified lower value. For an objective tolerance \(g>0\) and a lower witness at least \(U\), \(\delta\le g/(4n)\) suffices for a strip upper within \(g/2\) of that witness in logs. When the uniform itself is a contender for exact globality, the earlier strict terminal collar can exclude nearby sub-uniform shapes. With held scale, a feasible uniform strictly beats every sub-uniform shape by the modal bound and gives the previously proved location interval or singleton. If that uniform is infeasible, \(1-\delta>\sigma/R_*\) makes the strip infeasible. [Strict uniform collar and held-scale geometry](2026-11-25-tl-math-report.md#uniqueness-and-maximizer-sets-on-the-fitted-branch)

For a quantitative bound on the compact slab, let \(A_{\max}\) be a finite upper bound, \(M=A_{\max}/2\), and \(h_j\le w_j^0 2^{-D}\) after bisecting each coordinate \(D\) times from its initial radius \(w_j^0\). Let
\[
 C=\max_i (|x_i|w_A^0+w_B^0)/2+c_0w_\lambda^0,qquad
 E= w_A^0/2+A_{\max}\left(2C^\delta+w_\lambda^0/(e\delta)\right).
\]
Each per-observation factor in the box upper bound and the anchor value lies in \([0,M]\). The factor increase is at most \(E2^{-D\delta}\), by the modulus and \(2^{-D}\le2^{-D\delta}\). Telescoping the product gives
\[
 U_{\rm box}\le L(c)+nM^{n-1}E2^{-D\delta}.
\]
This holds with initial coordinate widths in place of radii when the anchor is chosen off-center to respect a held constraint. Let \(\ell_0\) be the incumbent lower log witness, \(L_*\) the true global supremum, and \(\epsilon_L=\exp(\ell_0+g)-L_*>0\). A depth \(D\) satisfying \(nM^{n-1}E2^{-D\delta}<\epsilon_L\) is sufficient, with at most \(2^{3D+1}-1\) binary tree nodes on the three-coordinate slab and a separate uniform strip bound. The same argument covers the prior compact comparison for exactly-half ties using its small-scale cutoff. The proof gives a finite quantitative count for positive slack. At a total log tolerance \(3.1\times10^{-6}\) and \(n=31\), the generic choice for the strip gives \(\delta=2.5\times10^{-8}\), so the bound on depth is too large to use as a default work allowance. [Half-tie compact comparison](2026-11-25-tl-math-report.md#compact-comparison-and-examples) · [Improved cutoff](2026-11-25-tlroot-math-report.md#sample-dependent-cutoff)

### Shared gradient bound inside support

**Proved.** If every datum has a signed inverse enclosure strictly inside \((0,1)\) throughout a convex box, form \(h(z,\lambda)=-\log D_\lambda(u(z,\lambda))\). Implicit differentiation using \(Q_u=D\) gives
\[
 h_z=-D_u/D^2,\qquad h_\lambda=-(\partial_\lambda D)/D-h_z\,\partial_\lambda Q_\lambda(u).
\]
The shape partial of \(D\) holds \(u\) fixed. The quantile shape partial is continuous through zero and can be enclosed using \(\partial_\lambda Q_\lambda(u)=\int_{1/2}^u\partial_\lambda D(v)\,dv\). Evaluate the interval gradient of the whole log likelihood
\[
 \nabla\ell=\left(n/A+\sum_i x_i h_{z,i},\ \sum_i h_{z,i},\ \sum_i h_{\lambda,i}\right)
\]
before allowing coordinate changes. If the three interval components are \(G_j\), the line segment from the anchor gives the upper bound
\[
 \ell(c)+\sum_j h_j \max_{v\in G_j}|v|
\]
for a centered rectangle. For a polytope of offsets, maximize \(\sum_j\max_{g\in G_j}g v_j\) over its vertices instead. This follows from integrating the gradient along the common segment and maximizing a convex piecewise linear function of the offset \(v\). Summing the gradient first preserves score cancellation. A smooth interval extension has gradient width of order the box radius on interior compact sets, giving quadratic bound excess at a stationary anchor. The probability modulus gives enclosures for the inverses from the anchor brackets without additional inversions. At clipped support or a possible zero density, the density modulus and the uniform comparison still apply.

**Numerical.** The 75-decimal check used 336 triples across four slabs \(\delta\in\{1,1/2,1/8,1/32\}\), residuals from 0 to 250, and perturbations \(10^{-2},10^{-4},10^{-8}\). The largest observed ratios of changes to bounds were 0.36336 for probability and 0.07280 for density. The local gradient probe uses the 31 quantiles of shape \(7/50\) at \((i+1/2)/31\), rounded symmetrically to a lattice of spacing \(2^{-44}\). The output records its exact integer numerators and anchor. Inverse signs and log bounds use 60-decimal interval evaluation around the 75-decimal point calculations. The independent comparator uses the same inverse intervals and evaluates each log term separately. All eight box corners were below the gradient upper. These are local box comparisons on the specified sample. [Bound checks](2026-11-25-tlambda2-math-data/bounds.py) · [Sample and output](2026-11-25-tlambda2-math-data/bounds.txt)

| Radius in each of \(A,B,\lambda\) | Shared gradient excess | Independent log-bound excess |
|---:|---:|---:|
| \(10^{-3}\) | 0.16551 | 0.69332 |
| \(10^{-4}\) | \(9.6454\times10^{-4}\) | 0.075231 |
| \(10^{-5}\) | \(9.2543\times10^{-6}\) | \(7.5937\times10^{-3}\) |
| \(10^{-6}\) | \(9.2171\times10^{-8}\) | \(7.6010\times10^{-4}\) |
| \(10^{-7}\) | \(9.2134\times10^{-10}\) | \(7.6017\times10^{-5}\) |

## Reproduction

All three final drivers exited zero on the workstation with Python 3.14 and mpmath 1.3.0. The U drivers import the unchanged interval kernel and point scout from the previous task's data folder. Their full covers account for every shape in \([1,2]\) on the checked inputs. The fitted-branch run checks the proved formulas and local box behavior.

~~~sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
d=dev/research/evaluations/2026-11-25-tlambda2-math-data
for s in u_events finite_tie bounds; do
  /tmp/umvl-cpu/run nice -n 10 python3 -u -B "$d/$s.py" || exit
done
~~~

## What this changes for the fitter

- An optional free-scale U route can form exact contact, center and interior groups from the original observations or hold. Use the scale reduction, rational shape cuts with checked signs, and the prior interval score and derivative isolator. Form the endpoint deficit from the distance to the appropriate original support endpoint divided by \(R\), so the translated \(-2^{-512}\) observation stays interior. Carry the nuisance map \(\sigma=\lambda R\) and coefficient restoration error into each shape bracket.
- With a complete cover, use score signs for consecutive-root comparisons and strict likelihood intervals for the remaining comparisons. Discard an unresolved root collar whose entire upper is strictly below the best lower witness. Use a dedicated certified unattained junction outcome if all finite contenders lose or strict pointwise exclusion proves it. A finite maximizer proved equal to \(J\) is returned as a finite fit, with the equal excluded face as diagnostics. Set uniqueness by the count of finite maximizers. At the exact finite/finite witness return the two coefficient points with `certified = TRUE` and `unique = FALSE`. At the two binary64 neighbors return their respective singletons once their coefficient enclosures pass the accuracy check.
- On the fitted branch, combine the existing compact nuisance bounds and half-tie cutoff with the uniform strip, the density modulus for support-crossing boxes, and the shared gradient on smooth boxes. Use the least justified upper available. A compact feasible point at or above the collapse value proves attainment, and a strict cover below it with the prior tail exclusions proves `CollapseLimit`. A value gap alone gives a supremum interval. To certify coefficients, separately prove the required inclusion, using the complete retained maximizer cover or isolation and dominance of the winning point.
- Use the existing work units for refinements. When the comparison budget expires, retain a successful accuracy-checked search fit with `certified = FALSE, unique = NA` and keep the unresolved candidates or faces as diagnostics. A strictly superior witness can refute that fit. An actual search failure, proved strict escape or proved nonexistence keeps its justified stop. [Five-case rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter) · [Wrapper contract](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- General unconditional exact predicates for multiple score roots and equality of nonconsecutive critical heights on algebraic samples need an identity or a terminating decision rule at unknown, generally irrational shapes. Rational cut arithmetic settles endpoint and rational-shape zeros. The interval covers settle the strict replays and the constructed real events. The conditional decision procedure and the exact real finite tie leave existence of a rational finite/finite tie undecided.
- For the U junction on general algebraic samples, a terminating unconditional decision still needs an exact separation theorem or an equality witness and test at a stationary shape. The direct conditional exclusion covers one noncentral distance without centers. The full event crosses on the real line and its dyadic neighbors are strict. The rationality of the implicitly specified real fold and tie inputs remains open.
- The modulus and shared gradient give a quantitative cover and a tighter local bound. A practical whole-domain count on the historical 31-point probe still needs a replay of the new cascade with production inverse and arithmetic margins. The proof does not decide general fitted-branch cardinality or exact equality with the half-tie collapse face. The latter requires an exact comparison at a surviving compact maximizer or a separate dominance theorem.
