# Sample-checkable likelihood existence for six all-free families

TASK-425 · base `61bfcadf`

All six families admit sufficient sample checks for finite attainment and for some escapes. The inexpensive escape checks use symmetry, extreme ranks, or tail multiplicity. The bounded checks for Burr also detect escapes on continuous samples. Exact examples show that each pilot attainment and escape screen misses some cases. On the specified sampling grid the fraction left undecided ranges from 0% to 99.5%.

The regimes are the repository's restricted fitted families. Shape one is an attained Cauchy member for `lst`, with a distinct zero-scale end. Shape two is excluded for `std` and `sstd`. The shape-one SGED member is attained. The SSTD normal end is the Fernández–Steel two-piece normal, including its half-normal corners. SGED has a uniform end and two half-GED ends. For nonconstant Burr data the only positive all-free boundary competitors are Weibull and Pareto. Log-logistic is the interior slice at first shape one. Reciprocation gives inverse Weibull (Fréchet) and power for inverse Burr. [Student closure](2026-11-25-tcert-math-report.md) · [SSTD atlas proof](2026-11-27-sstd-atlas-math-report.md#joint-envelope) · [SGED atlas proof](2026-11-27-sged-atlas-math-report.md#localization-and-comparison-rule) · [Burr closure](2026-11-03-burr-closure-report.md#2-an-envelope-and-the-complete-weak-closure)

Use mean log likelihood throughout and geometric mean likelihood when comparing density heights directly. All comparisons count multiplicities. On the Burr log scale omit the common Jacobian. The prior joint envelopes imply that a finite witness at or above all excluded faces proves attainment. The screens use strict comparisons because they can be resolved with a margin. Their gap means that neither **implemented sample screen** decides. It includes samples that a finer cover, a stronger theorem, or exact equality can settle.

## Shared Student calculations

### A shape comparison for two residual levels

**Proved.** Write
$$
 c(q)=\log\Gamma((q+1)/2)-\log\Gamma(q/2)-\tfrac12\log\pi,
 \qquad r=h(q+1)-1.
$$
Suppose the zero residual fraction is \(1-h\) and every other residual has magnitude \(d>0\). With denominator width \(a=\sqrt q\,s\), differentiation in \(\log a\) gives the optimum at \(a=d\sqrt r\) when \(r>0\). The optimized mean log likelihood is
$$
 J_h(q;d)=c(q)-\log d+\tfrac12\{r\log r-(r+1)\log(1+r)\}.
$$
It extends continuously to collapse at \(r=0\). The normal value is \(J_h(\infty;d)=-\frac12\log(2\pi e)-\log(d\sqrt h)\). Add \(\log 2\) for a half family.

For \(h\ge1/3\) and \(q>1/h-1\),
$$
 J_h'(q)=c'(q)-\frac h2\log\frac{q+1}{q+1-1/h}
 =\frac12\int_0^\infty e^{-qu}
 \left\{\frac{2}{1+e^{-u}}-\mathbb E e^{-uV}\right\}du,
 \qquad V\sim\operatorname{Unif}[1-1/h,1].
$$
Use the prior digamma identity for \(c'\) and integrate \(1/(q+v)\) over the displayed interval for the other term. The brace vanishes at zero, has derivative \((3-1/h)/2\) there and is strictly concave on \(u>0\) by concavity of the logistic term and convexity of the exponential average. At \(h=1/3\) it is strictly negative on \(u>0\). At \(h>1/3\) it is initially positive and can change sign once, from positive to negative. If its Laplace integral is nonnegative at one \(q\), multiplication by \(e^{-(q'-q)u}\) for \(q'>q\) makes the integral strictly positive. Compare the multiplier to its value at the sign change to bound the positive and negative parts. If there is no sign change the integral is positive throughout. Hence on a shape interval every strictly interior shape lies below the larger endpoint value, allowing a critical lower endpoint and infinity. [Digamma and shape identities](2026-11-04-t-cert-data/derivation.md#a-global-envelope-on-every-finite-shape-interval)

### Symmetric normal escape

**Proved.** For a nonconstant reflection-symmetric sample centered at zero with variance \(v=\overline{x_i^2}\), if \(\max_i x_i^2\le2v\), normal strictly wins in both `std` and `lst`. The mass off zero is at least \(1/2\), and each nonzero atom has mass at most \(1/2\). For \(q>1\) the fixed-shape geodesic maximizer exists, is unique and has center zero by symmetry. Concavity gives
$$
 \overline{\log(1+x_i^2/a^2)}\ge\tfrac12\log(1+2v/a^2)
$$
at every width. This yields the profile upper bound \(J_{1/2}(q;\sqrt{2v})\). On \(q\ge1\) its lower endpoint is \(-\log(\pi\sqrt{2v})\), below its normal value since \(\pi>e\). The shape comparison gives strict exclusion at finite \(q>1\). The \(q=1\) bound follows by taking \(q\downarrow1\) at fixed positive width. Normal is reachable. [Prior balanced two-point theorem](2026-11-05-t-root-data/derivation.md#balanced-two-point-all-free-fits-select-normal)

**Proved, an exact escape missed by that screen.** Take 99 copies each of \(-1,1\) and one each of \(-3/2,3/2\). The largest squared residual is \(9/4>2v=81/40\). To verify normal escape independently let \(m=\min x_i^2>0\), \(V=\operatorname{Var}(x_i^2)\), and use the fixed-shape center zero. The scale score gives \(s^2\ge m\). Taylor's inequality for \(\log(1+u x^2)\) about \(v\), with \(u=1/(qs^2)\), lowers the average log penalty from the equal-squared-residual benchmark by at most \(Vu^2/[2(1+um)^2]\). The corresponding likelihood increase is at most \(V/[4m^2(q+1)]\). The equal-residual profile has a normal-to-Student gap at least \(1/(4q)-1/(24q^3)\ge5/(24q)\), by the prior normalizer bound and \((q+1)\log(1+1/q)>1\). Thus \(V/m^2\le5/6\) suffices for symmetric samples without zero residuals whenever the fixed-shape center is zero. Here there are four support points with strict multiplicity even at \(q=1\), and the sample satisfies the variance bound and has kurtosis below three. [Normalizer bound](2026-11-04-t-cert-data/derivation.md#unsigned-bound-on-the-entire-normal-tail)

### A critical atom with a distant observation

**Proved.** Let \(q_0=1\) for `lst` or \(q_0=2\) for `std`. Suppose an atom \(b\) has \(k=q_0(n-k)\). Normalize by subtracting \(b\) and dividing by the geometric mean \(g\) of the nonzero distances. Set \(R=\max_i|x_i-b|/g\) in original sample coordinates and \(a_0=2^{-q_0-1}\). Define
$$
 B_{\rm short}=e^{c(n)}\max\{a_0^{(q_0+1)/n-1}R^{-(q_0+1)/n},R^{-1}\},
 \qquad B_{\rm long}=\sqrt{2n/\pi}/R.
$$
If both are strictly less than \(e^{c(q_0)}\), collapse wins with geometric mean likelihood \(e^{c(q_0)}/g\) in original units.

The fixed-shape Busemann inequality gives this value at \(q_0\), strictly above every positive width except for balanced two-point Cauchy data, which have \(R=1\) and fail the hypothesis. In normalized units let \(D_i=(x_i-\mu)^2+a^2\). At \(q>q_0\), multiplying the fixed-shape upper inequality for the kernel part by \((q+1)/(q_0+1)\) gives an upper mean log likelihood \(c(q)+(q-q_0)\log a/(q_0+1)\). This is strictly below \(c(q_0)\) for \(a\le a_0\) by strict concavity and \(c'(q_0)\le\log2\). For \(a\ge a_0\), \(D_bD_{\rm far}\ge a^2R^2\) and the other \(D_i\ge a^2\) bound the geometric mean above by \(e^{c(q)}a^{-1}\min(1,(a/R)^{(q+1)/n})\). For \(q\le n\), replace the exponent by \((q_0+1)/n\) and maximize over \(a\ge a_0\) to obtain \(B_{\rm short}\). For \(q\ge n\), one of the paired points is at least \(R/2\) from the center. The kernel is at most \((1+r^2/n)^{-n/2}\) at standardized distance \(r\) because \(q\log(1+r^2/q)\) increases with \(q\). Its one penalty together with the peak bound \(1/(s\sqrt{2\pi})\) gives \(B_{\rm long}\), also at the normal end. The critical collapse path is reachable. For example \((0,0,1/100,100)\) satisfies the criterion for `lst`. [Fixed-shape critical value](2026-11-05-t-root-data/derivation.md#equality-masses-an-exact-endpoint-not-an-early-winner)

## All-free regimes

### std held all free

**Attainment, proved.** Let \(N\) be the fitted normal mean log likelihood, \(D\) the best shape-two value including critical collapse when present, and \(b_2=\overline{(x_i-\bar x)^4}/v^2\). If \(3k_{\max}\le2n\), \(b_2>3\), and either \(N\) or a prescribed finite Student witness exceeds an upper bound for \(D\), a finite MLE exists. A finite member above \(N\) follows from the positive inward coefficient \((b_2-3)/4\). If the given witness above \(D\) also exceeds \(N\), it suffices directly. Otherwise the normal improvement beats both. At critical multiplicity \(D=c(2)-\log g\). Under strict multiplicity the prior order-statistic scale rectangle and hyperbolic tangent supply the enclosure for \(D\). At any proposed \((\mu,a)\), take its shape-two value plus the hyperbolic score norm times the maximum distance to the four rectangle corners. Geodesic convexity proves the bound for any proposal. The replay uses 128 fixed reweighting steps starting at three sample quartiles and a small shape/scale stencil for feasible values. [Inward normal coefficient](2026-11-04-t-cert-data/derivation.md#normal-seam-expansion-including-nuisance-refitting) · [Rectangle and tangent](2026-11-04-t-cert-data/derivation.md#uniform-nuisance-containment)

**Escape and closed subcase, proved.** For \(3k_{\max}>2n\) collapse is unbounded. The shared symmetric normal and critical-atom conditions apply. On the entire reflection-symmetric class supported on \(\{b-d,b,b+d\}\), \(d>0\), let \(h\) be the mass off \(b\). For \(h<1/3\) the likelihood is unbounded. For \(h=1/3\) the decreasing \(J_h\) selects collapse. For \(h>1/3\), compare \(J_h(2;d)\) and \(J_h(\infty;d)\). At fixed \(q>2\), reflection and geodesic uniqueness identify \(J_h\) with the exact optimized profile, so the endpoint theorem excludes all finite shapes, including when the faces tie. Ten zeros and three copies each of \(-1,1\) give a positive-width shape-two winner. At \(h=3/8\) the lower end beats normal by more than \(0.0391\) per observation. [Exact comparisons](2026-11-27-checkable-math-data/exact.txt)

**A wider sufficient certificate.** With strict multiplicity and \(D>N\), use a finite upper cover against a lower bound for \(D\), an unsigned normal tail bound and a signed lower collar. For some \(s>0\), enclose \(P(2+s)-D\) above by \(E\), with \(P\) the mean fixed-shape optimum. If \(E/s+s/8<0\), the relative chord inequality excludes the punctured lower collar uniformly. The curvature correction for \([2,2+s]\) is at most \(u(s-u)/8\) with \(u=q-2\). On remaining compact shape intervals use the nuisance tangent and shape chords. Against normal, use a negative inward coefficient with a signed normal collar and strict upper bounds on the finite complement. These sample-only interval tests take more evaluations than the replay screen. [Lower collar](2026-11-05-t-root-data/derivation.md#relative-endpoint-chords-include-the-cusp) · [Normal collar](2026-11-04-t-cert-data/signed-tail.md)

**Exact gap samples.** Five copies each of \(\pm3/16\) and two copies each of \(\pm1\) have kurtosis \(<3\). Shape \(5/2\), center zero and ordinary scale \(9/25\) beat normal and shape two by more than \(0.00708\) and \(0.00146\) per observation. The shape-two upper value follows by reflection and opposite signs of its monotone scale score on a bracket. This refutes a global kurtosis-at-most-three escape rule. The 200-observation example in [Symmetric normal escape](#symmetric-normal-escape) is an exact escape missed by the implemented screen. [Interval checker](2026-11-27-checkable-math-data/check_exact.py)

### lst held all free

**Attainment, proved.** If \(2k_{\max}<n\) and \(b_2>3\), a finite MLE exists. Normal is the only excluded positive boundary in this regime, and the inward derivative is positive. At \(2k_{\max}=n\) the same derivative proves attainment if \(N\) is at least the largest critical collapse value \(c(1)-\log g\) over critical atoms. At \(2k_{\max}>n\) the likelihood is unbounded. Shape one is feasible. A finite shape-one witness above every excluded end also proves attainment. Which finite shape wins is a further comparison. [Prior lower faces and Cauchy endpoint](2026-11-05-t-root-report.md#lower-faces-equality-masses-and-zero-normal-slope)

**Escape and closed subcase, proved.** The shared normal and critical-atom rules apply. The sample \((0,0,1/100,100)\) selects critical collapse and has no positive-scale Cauchy maximum. The two-level decision is closed. Unequal counts imply unbounded collapse, and equal counts select normal. Constants are unbounded. The signed normal collar with strict interval exclusion of all remaining finite shapes supplies a wider sufficient comparison.

**Exact gap samples.** The twelve rational observations
$$
 (-101,-99,-4,-3,-2,-1,1,2,3,4,99,101)/100
$$
have negative inward coefficient. The feasible \(q=3,\mu=0,s=1/3\) improves the fitted normal by more than \(0.49\) in total log likelihood and the mass test is strict. This is an attained example outside the screen. The same 200-observation normal escape as for `std` misses the implemented escape screen. [Earlier rational counterexample](2026-11-04-t-cert-data/derivation.md#an-exact-counterexample-to-a-global-kurtosis-decision)

### sstd held all free

**Attainment, proved.** Write \(u,v\) for left and right ordinary widths, \(T=u+v\), and \(K_q=2e^{c(q)}/\sqrt q\), with \(K_\infty=\sqrt{2/\pi}\). For strict mass \(3k_{\max}<2n\), use the following three computable boundary upper bounds. If a prescribed finite point exceeds all three, it proves attainment. At critical mass the same conclusion holds with the additional comparison against the exact critical value, when the positive-width bounds can be resolved. The replay applies the strict-mass rule. The strict supercritical case is unbounded.

1. **Two-piece normal.** At fixed join \(j\) put \(A=\overline{(j-x_i)_+^2}\) and \(B=\overline{(x_i-j)_+^2}\). Minimizing \(A/u^2+B/v^2\) at fixed \(T\) gives \(u:v=A^{1/3}:B^{1/3}\), with the endpoint convention if one side has zero residuals. Maximizing over \(T\) gives \(\log K_\infty-1/2-(3/2)\log(A^{1/3}+B^{1/3})\). On a join cell use its left endpoint in \(A\) and its right endpoint in \(B\) to get an upper bound. Projection of the join to the data range can only improve the likelihood.
2. **Two-piece shape two.** On a join cell \([j_l,j_u]\), retain distances \(d_i=(j_l-x_i)_+\) on the left and \(d_i=(x_i-j_u)_+\) on the right, dropping the loss on crossing observations. Let \(p_i\in[0,1]\) vanish wherever \(d_i=0\), require \(\sum p_i=n/3\) and set \(\omega=3\overline{p_i1_{\{\mathrm{left}\}}}\). Write \(E(t)=-t\log t-(1-t)\log(1-t)\), with continuous endpoint values. For any such weights,
   $$
   \log K_2-E(\omega)-\tfrac32\overline{p_i\log(d_i^2/2)+E(p_i)}
   $$
   is an upper bound on the cell. Terms with zero distance contribute zero. Apply \(\log(e^a+e^b)\ge\omega a+(1-\omega)b+E(\omega)\) to the log total width, with \(a=\log u,b=\log v\). Apply \(\log(1+e^t)\ge p_it+E(p_i)\) to each penalty. The chosen total and side weights cancel both log widths. The same argument at another fixed \(q\) uses total \(n/(q+1)\) and factor \((q+1)/2\). Affine correction toward zero or toward the active indicators converts proposed weights into feasible ones. In a cell with too few active observations, subdivide. This removes the need to search a width rectangle on every join cell. The inequalities extend to half limits by taking limits.
3. **Both halves.** Each optimal half join sits at the corresponding extreme. For \(q\in[a,b]\subseteq[2,\infty]\) use \(K_q\le K_b\) and the lower penalty
   $$
   \frac{a(1+1/b)}2\log(1+r^2/a),\qquad r=d_i/T.
   $$
   The first inequality follows from \(c'(q)>1/(2q)\) in the digamma integral. The second uses monotonicity of \(q\log(1+r^2/q)\). For the resulting upper objective the width score in \(\log T\) decreases strictly. Bracket its zero whenever \(a(1+1/b)\Pr_{\rm emp}(d_i>0)>1\). The upper value uses the smaller width for the log peak and the larger width for the penalties. Subdivide shape cells as needed, taking an explicit unbounded terminal cell.

These are absolute bounds on the excluded faces. The finite stencil chooses joins at inner observations and adjacent midpoints, tries fixed degrees of freedom, and proposes positive widths at each point using fixed steps on the concave log-width objective. Its attained finite value is a lower bound whatever the proposals do. [Complete face envelope and critical value](2026-11-27-sstd-atlas-math-report.md#vanishing-width-and-other-ends)

**Escape and closed two-level subcase, proved.** For exactly two support points at distance \(d\), let \(h\le1/2\) be the smaller mass. Project the join into the range. The two nonnegative standardized residuals have width-weighted average \(d/T\). With strictly positive widths, either the larger residual strictly exceeds \(d/T\), or both equal \(d/T>0\) and both groups pay the penalty. Thus every such finite product is strictly below the one with all the residual cost assigned to the smaller group at residual \(d/T\) and the same peak. That upper bound is the half profile with its endpoint at the more frequent observation. It is reachable as a half limit, in both orientations for equal counts. If \(h<1/3\) collapse is unbounded. At \(h=1/3\) the decreasing \(J_h\) gives the critical half collapse. At \(h>1/3\) the maximum is the larger of \(J_h(2;d)+\log 2\) and \(J_h(\infty;d)+\log2\), retaining both when equal. This gives positive-width half-\(t_2\) winners, for example \(h=3/8\), and half-normal winners, for example \(h=1/2\) or \(h=4/9\). Every nonconstant two-level sample therefore has no finite MLE.

More generally, at a critical extreme atom, split into triples containing two copies of the atom and one other observation. In each triple the half collapse is the strict upper limit. All triples reach their limits on a common path, since each non-tie lies on the same broad side. The resulting original-sample geometric mean likelihood is \(1/g\). This gives an all-free sufficient collapse decision including multilevel samples. Constants are unbounded. [Interval comparisons for \(h=3/8,1/2,4/9\)](2026-11-27-checkable-math-data/exact.txt)

**Wider sufficient certificates for the regular shape faces and halves.** For a strict-mass sample, use a feasible face value \(B_0\) as a lower bound for the nominated face. Project the join into the range. Widths of contenders can be restricted to a compact positive interval. The large-width bound is \(K_\infty/T\). At small width, at least \(n-k_{\max}\) residuals are separated by a positive order-statistic spacing from any join. Uniformly in \(q\in[2,\infty]\), \(F_q(r)\le(1+r^2/4)^{-3/2}\). For \(2\le q\le4\) this follows from the denominator and the exponent separately, and for \(q\ge4\) from monotonicity of \(q\log(1+r^2/q)\). Hence the product is \(O(T^{3(n-k_{\max})-n})\). These bounds give explicit cutoffs against \(B_0\).

- For a **regular two-piece \(t_2\)** candidate, use tubes in the shape-two collar bounded away from the half corners. At fixed denominator widths, a strictly negative enclosure of
  \(c'(q)-\frac12\overline{\log(1+r_i^2)}\), with residuals in those widths, proves each finite point in the tube loses to its projected shape-two point. This formula follows by differentiating the log kernel at fixed denominator widths. Require upper bounds strictly below \(B_0\) on the remaining compact cells, including rival faces.
- For a **regular two-piece normal** candidate, use tubes bounded away from the half corners with \(h=1/q\). At fixed ordinary widths the derivative at zero is \(\overline{(r_i^4-2r_i^2-1)}/4\). A strictly negative interval bound on the derivative throughout the tube proves finite domination by projection to \(h=0\). Require the same strict value cover on the complement.
- For a **right or left half**, use the prior signed half collar on the compact shape and broad-width intervals. In the right collar compare with the right half at the same broad width and shape. At fixed broad width the peak loses \(-\log(1-w)\) in log likelihood with thin fraction \(w\). Any improvement from moving the join inside the sample is \(o(w)\), with the explicit power bound from the contact proof. This gives strict domination in a sufficiently small collar. Require a strict value cover of the remaining cells and the other half against \(B_0\). This applies to interior half-\(t\) shapes as well as the corners if the comparisons close.

For value covers split the range of \(h=1/q\), log total width, join and skew fraction. Bound heights above and use minimum separated residuals with maximum side widths in each cell, with the half contact ratios bounded below by zero at contact. The shape-envelope inequality in the attainment proof gives one group of upper bounds, including cells meeting \(h=0\). The signed inequalities compare to the exact face at the projected nuisance parameters, while the unsigned cover compares to a feasible lower bound. Together these are sufficient checks without a full finite stationary-point search. Compact covers need not close within a fixed budget. The pilot uses the closed two-level and critical-extreme rules, and does not run these wider escape covers. [Normal derivative and signed half collar](2026-11-25-sstd2-math-report.md#1-uniform-comparisons-at-the-student-normal-end)

**Exact gap samples.** For attainment take
$$
 (-67,-47,-38,-27,-20,-19,-8,-8,-8,-7,-6,-6,-5,0,15,17,22,27,74,85).
$$
Normalize by \(y=(x+67)/152\). At \(q=41/20\), join \(77/200\), and ordinary side widths \(111713/10^6,143348/10^6\), the mean log likelihood exceeds all faces. The 80-digit checker attests the normal bound, 697 visited shape-two join cells and 49 half-shape cells. A single positive-width shape-two witness exceeds the upper values for **every** finite shape and join in the pilot's stencil even if its widths are optimized. Thus its attainment screen misses this exact sample. The escape example is \((0^{[6]},2\sqrt2,3^{[4]})\), where brackets denote multiplicities. Split into \((0,2\sqrt2)\) and \((0^{[5]},3^{[4]})\). The right half-normal wins in both blocks, with the same ordinary width \(d\sqrt h=2\) and join zero. Summing the strict finite inequalities proves escape of the full sample. It has three support points and no critical atom, so the pilot misses it. [Outward checks](2026-11-27-checkable-math-data/exact.txt)

### sged held all free

**Attainment, proved.** Let \(p\ge1\), \(t=1/p\), range \(R>0\), and
$$
 b_p=\exp\{t(\log t-1)-\log\Gamma(1+t)\}.
$$
At a fixed join the geometric mean likelihood after optimizing both side widths is
$$
 \frac{b_p}{(A^{1/(p+1)}+B^{1/(p+1)})^{1+1/p}},
 \qquad A=\overline{(j-x_i)_+^p},\quad B=\overline{(x_i-j)_+^p}.
$$
For the width fraction \(w\), minimize \(A/w^p+B/(1-w)^p\) to get the side-width ratio \(A^{1/(p+1)}:B^{1/(p+1)}\). Maximizing the scale factor \(-\log T-\mathrm{penalty}/T^p\) gives the formula. An interior join gives a feasible witness. The shape and join stencil in the replay includes \(p=1\) and inner sample knots and adjacent midpoints.

The half profiles at the extreme endpoints are \(b_p/M_p(d)\), with \(M_p(d)=(\overline{d_i^p})^{1/p}\) and \(d_i\) the distances to that endpoint. The endpoints maximize supported half likelihood at fixed width by monotonicity of the penalties. Bound a shape cell \([a,b]\) above by \(b_b/M_a(d)\). The power mean increases with \(p\), and \(b_p\) increases by \(\psi(1+t)>\log t\), the upper derivative in the convex secant inequality for \(\log\Gamma\) from \(t\) to \(1+t\). If \(m\) is the smaller extreme count fraction, all \(p\ge1/m\) give half heights below \(1/R\). Indeed \(M_p(d)\ge Rm^{1/p}\) and \(\log\Gamma(1+t)\ge-\gamma t\), so the logarithm of the ratio to \(1/R\) is at most \(t(\log(t/m)-1+\gamma)<0\). A finite stencil value above \(1/R\) and above both half upper covers on \([1,1/m]\) proves attainment. [Uniform and half face values](2026-11-27-sged-atlas-math-report.md#finite-shape-half-ends)

**Escape and closed two-level subcase, proved.** As in SSTD, the larger standardized extreme residual is at least \(R/T\). For a finite fit with positive widths the cost strictly exceeds the lower bound from putting the penalty only on the smaller extreme mass \(m\) at \(R/T\). The bound holds also for multilevel data by discarding the other penalties. Profiling \(T\) gives \(b_p m^{-1/p}/R\). The log ratio to \(1/R\) is
$$
 f_m(t)=t(\log t-1-\log m)-\log\Gamma(1+t), \quad f_m(0)=0.
$$
It is strictly convex since \(f_m''(t)=1/t-\sum_{j\ge1}(t+j)^{-2}>0\) by the strictly decreasing integral test. Its largest value on \(0\le t\le1\) is an endpoint. Thus \(m\ge1/e\) proves uniform escape even with other observations. For a two-level sample the half endpoint of shape one attains the other bound in the closure and wins if \(m<1/e\), oriented toward the less frequent observation. At formal equality the two limits share the supremum, but equality cannot occur with rational frequency \(m\). Every finite shape and positive skew width lies strictly below the winning value. Constants are unbounded.

Two inexpensive ways to extend uniform escape are available.

- Split the ordered sample into two nonempty contiguous groups, of means separated by \(D\) and smaller group fraction \(h\). Every finite SGED log density is concave in the observation for \(p\ge1\), as its negative log kernel is convex across the join. Jensen upper bounds its sample value by that on the group means with their group weights. On \(1\le p\le1/m\), this is at most
  \(\exp\{\max(f_h(1),f_h(m))\}/D\) by the two-point result and convexity in \(t\). If this is strictly below \(1/R\), the rest of the shape range is excluded by the extreme bound. This is a continuous-data sufficient condition.
- If the mean is the midpoint and the empirical law dominates uniform on the range in convex order, normalizing the range to \([0,1]\) gives \(\overline{\log f(x_i)}\le\int_0^1\log f(x)\,dx<0\). The first inequality follows by the convex order applied to \(-\log f\), and the second by Jensen and \(\int_0^1 f<1\) for every finite fit. The convex order check uses the empirical stop-loss values at all data knots against \((1-z)^2/2\). Between knots the empirical stop-loss is linear and the uniform one convex, so the endpoint comparisons suffice. With equal means these comparisons imply the convex order by writing piecewise linear convex functions as affine functions plus positive hinge combinations, then taking uniform limits on the range.

A half escape can also be proved by blocks if each is a two-level half-Laplace winner with the same endpoint, orientation and broad scale \(hD\). Summing the strict inequalities gives a common half winner for the whole sample. A wider interval certificate for a half face with a feasible value strictly above \(1/R\) uses the tail \(p\ge1/m\), the signed fixed-broad-width half collar, and strict value bounds on the remaining compact join, width, shape and skew cells. To localize width, the peak is bounded by a constant times \(1/T\), which excludes large width, and one extreme with distance at least \(R/2\) gives a penalty at least \(R/(2T)\) for \(T\le R/2\). The half collar works uniformly for \(p\) on the remaining compact interval, including \(p=1\). Compact cover tests and the block rule are outside the replay's inexpensive escape checks. [Prior half collar](2026-11-25-sstd2-math-report.md#2-signed-finite-shape-half-collars)

**Exact gap samples.** The sample \((0^{[7]},4,5)\) splits into \((0^{[3]},4)\) and \((0^{[4]},5)\), each with right half-Laplace scale one and join zero. The common half value \(1/e>1/5\) proves escape and excludes both uniform predicates. It has three levels, outside the implemented half screen.

For an attained gap sample define
$$
 X(a)=(0,((1-a)/2)^{[2]},((1+a)/2)^{[2]},1),\qquad
 a_*=\sup\{a\in[0,1]:\text{a finite fit for }X(a)\text{ has geometric mean likelihood }>1\}.
$$
This defines an exact real sample, with \(0<a_*<1\). At zero the centered optimized Laplace gives \(3/e>1\). Near one, grouping the lower and upper three observations gives two equally weighted means with distance \((1+2a)/3\) tending to one. The coarsening test passes there because \(f_{1/2}(1)\) and \(f_{1/2}(1/6)\) are strictly negative. Throughout the path a finite half value is strictly below one. On \(1\le p\le6\) its power mean is at least \((1/6+(2/3)2^{-p})^{1/p}\), by convexity of each symmetric pair of powers about \(1/2\). The 160 cells of width \(1/32\) enclose the largest resulting log upper bound below \(-0.0999\). The extreme-count bound treats \(p\ge6\) for both finite and half fits. Thus a sequence of improving feasible points approaching \(a_*\) stays in a compact join, width and shape box. Neither half nor the uniform can be its limiting parameters, since each finite-shape half value is strictly below one and the tail \(p\ge6\) was excluded. The limit is finite with value at least one. A strict improvement at \(a_*\) would persist for larger \(a\) by continuity, so the attained value is exactly one. This exact finite equality is missed by every strict-witness rule above. [Half upper check](2026-11-27-checkable-math-data/exact.txt)

### burr held all free

Use \(y_i=\log(x_i/x_{\min})\) and \(D=\bar y\). For a nonconstant sample the prior optimized Pareto and Weibull values in the log-data chart are
$$
 P=-\log D-1,\qquad
 W=\max_{c>0}\{\log c+cD-\log\overline{e^{cy_i}}-1\}.
$$
The Weibull profile is strictly concave because its second derivative is \(-1/c^2-\operatorname{Var}_{c}(y)\), with exponential weights. Its score has one root. A score sign bracket and concave tangent enclose \(W\) by a scalar calculation. [Boundary values](2026-11-03-burr-closure-report.md#3-boundary-optima-and-the-exact-attainment-criterion)

**Attainment, proved.** At the Weibull power \(c_W\) let \(v_i=e^{c_W y_i}/\overline{e^{c_W y_i}}\). If \(W\ge P\) and \(\overline{v_i^2}>2\), attainment follows from the positive inward derivative in \(1/k\). The expansion follows by substituting the Weibull nuisance coefficients into the Burr log likelihood and expanding \(\log(1+v_i/k)\). For a complementary finite witness, use \(z_i=c(y_i-b)\), \(S=\overline{\log(1+e^{z_i})}\) and \(U=\overline{\log(1+e^{-z_i})}\). The best \(k\) for a proposed \(c,b\) is \(1/S\), with value \(\log(c/S)-1-U\). If any prescribed \(c,b\) gives a value above \(P\) and an upper enclosure for \(W\), it proves attainment. The replay uses powers proportional to \(c_W\) and a fixed set of log-data quantile joins. The second test permits negative seam slopes. The seven log data \((0,5,7,8,8,15,15)/15\) from the closure proof give a strict finite winner despite a negative slope. [Weibull expansion and exact witness](2026-11-03-burr-closure-report.md#7-the-weibull-seam-and-certified-failures-of-local-classifiers)

**Pareto escape, proved.** For a nonconstant sample with at least half the observations at the minimum, the prior softplus chord and entropy argument gives strict Pareto dominance. This also gives a closed outcome on that class. A new sufficient spacing test is as follows. Define the exact seven-log reference
$$
 Y(q)=(0,\,0.23826q,\,7q/15,\,8q/15,\,8q/15,\,1,\,1),
 \qquad q=1/64.
$$
If the empirical distribution of \(y/\bar y\) dominates that of \(Y(q)/\overline{Y(q)}\) in convex order, Pareto strictly wins. The check compares the ascending integrated quantile sums at the cumulative rank knots of both samples. Lower sums certify this direction of convex order, since the integrals are piecewise linear and the means are equal. Equivalently, replicate both multisets to a common size and use ordinary majorization of the normalized arrays. The log-Burr density, evaluated in the log observation, is strictly concave because its nonlinear term is \(-(1+k)\log(1+e^{cy+b})\). Consequently every finite point on the more dispersed data is bounded above by its value on the reference with the same normalized mean log excess, and those values are strictly below their common Pareto benchmark. The reachable endpoint trace gives equality in the limit.

The reference escape itself is proved by three checks. The entropy bound eliminates \(k\le1/6\) since the reference has one minimum. The prior large-shape bound \(H(k)\le W+2n^2/k\), in mean units with \(k\ge2n\), gives strict exclusion for \(k\ge512\) since \(P-W>1.0058\). For the compact complement the checker uses the dual and chord bounds below over \([1/8,512]\). It certifies 12 accepted intervals with minimum separation exceeding \(0.2034\). [Minimum multiplicity proof](2026-11-03-burr-closure-report.md#6-a-global-minimum-tie-nonexistence-theorem) · [Large-shape bound](2026-11-27-lean-burrfree-report.md#results) · [Exact reference checks](2026-11-27-checkable-math-data/exact.txt)

**Weibull escape, proved.** Let \(m\) be the number at the minimum, \(k_0=m/(n-m)\), and \(A=\overline{v_i^2}/2-1\). The following finite list of inequalities suffices.

1. Prove \(W>P\). The Pareto end and all shapes \(0<k\le k_0\) then lose by the minimum-atom entropy inequality.
2. For some \(h>0\) check the signed collar against Weibull. In the coordinates \(\theta=(c,\eta)\), \(\eta\) the intercept shifted by \(\log k\), choose a ball of radius \(\rho<c_W\) about the Weibull nuisance optimum, and \(t_i=(y_i,1)^T\). Put \(a_i=v_i e^{-\rho\|t_i\|}\), \(b_i=v_i e^{\rho\|t_i\|}\). Use a positive eigenvalue lower bound \(\mu\) for
   \[
     M=\operatorname{diag}((c_W+\rho)^{-2},0)
       +\overline{\frac{a_i}{(1+hb_i)^2}\,t_it_i^T}.
   \]
   One choice is \(\det(M)/\operatorname{tr}(M)\). Set
   \[
   D_1=\|\overline{v_i(v_i-1)t_i}\|+h\overline{v_i^2|v_i-1|\|t_i\|}.
   \]
   The inequalities \(A<0\), \(hD_1<\mu\rho\), \(h(1/6+D_1^2/(2\mu))<-A/2\) suffice. Indeed the negative nuisance Hessian is at least \(M\) throughout the ball for \(0\le 1/k\le h\). The score at its center is bounded in norm by \(D_1/k\), giving inward radial scores and the nuisance improvement bound \(D_1^2/(2\mu k^2)\). The fixed-nuisance quadratic remainder above \(A/k\) is at most \(1/(6k^2)\) by integrating \(u(1-u)/(1+u/k)^3\) and discarding its negative part. It follows that \(H(k)<W\) for all \(k\ge1/h\). [Signed seam derivation](2026-11-03-burr-xi-data/derivation.md#3-signed-upper-tail-with-an-explicit-remainder)
3. Cover \([k_0,1/h]\) by finitely many adjacent knot intervals. At each knot \(k\) choose weights \(p_i\in[0,1]\) with \(\bar p=1/(1+k)\) and \(a=\overline{((1+k)p_i-1)y_i}>0\). The upper value is
   \[
      U_k=\log(k/a)-1-(1+k)\overline{E(p_i)} .
   \]
   Apply the softplus entropy inequality, cancel the intercept and maximize the resulting \(\log c-ac\). Also \(H(k)-\log k\) is convex since it is a supremum of affine functions of \(k\). On \([l,u]\) use \(\max(U_l,U_u)+(u-l)^2/(8l^2)\), by bounding the interpolation remainder of \(\log k\). Each upper value must lie strictly below a feasible Weibull lower bound. No solution of a finite three-parameter score equation enters the proof. [Dual and chord derivation](2026-11-03-burr-cert-data/derivation.md#5-fixed-k-dual-upper-certificates)

These checks certify a strict Weibull winner on log data \(U(9/10)\) below. The calculation uses 571 accepted compact intervals. The binary64 replay uses a finite knot budget and the same inequalities. A negative slope alone would supply only the signed collar. [Outward exact-sample run](2026-11-27-checkable-math-data/exact.txt)

**Exact gap samples.** For escape take \(x=\exp(Y(1/32))\), componentwise. Its single minimum misses the multiplicity test, and its normalized logs fail the reference convex order: increasing \(q\) increases the lower rank sums strictly at some knots, since the two largest logs stay one while the sum grows. It fails the Weibull screen since \(P>W\), yet the same checked Pareto cover has \(P-W>0.9867\) and minimum compact gap \(>0.1857\). The normalized log data have a strict Pareto winner.

For an attained gap use the prior exact finite/Weibull tie. Define
$$
 U(t)=(0,1-10t/15,1-8t/15,1-7t/15,1-7t/15,1,1),
 \quad
 t_*=\min\left\{t\in[9/10,1]:
  \max_{1/6\le k\le 2^{16}} H_{U(t)}(k)-W_{U(t)}=0\right\}.
$$
Use \(x=\exp(U(t_*))\). In this deformation the prior checks give \(W>P\), \(A<0\) and signed upper-tail exclusion uniformly, and use the exact small-\(k\) bound up to \(1/6\). At \(t=9/10\) the compact upper cover lies below \(W\), whereas at \(t=1\) a finite witness is strictly above. Continuity yields the stated least zero with a finite maximizer that equals \(W\). Neither the positive slope rule nor a strict witness can detect its attainment. The 65-digit prior check of the deformation and endpoints was replayed. Constants are unbounded by the prior concentration path. [Full exact tie proof](2026-11-25-burr2-math-report.md#a-finite-maximum-equal-to-a-winning-weibull-face)

### invburr held all free

**Attainment and escape, proved by transport.** For positive observations run the Burr checks on \(y_i=\log(x_{\max}/x_i)\). For physical parameters reciprocal transformation inverts the scale and adds the fixed log Jacobian \(-2\log x_i\) to each inverse Burr log density. Therefore objective comparisons, maximizers and reachability are preserved, including equality. Pareto becomes the power model with endpoint \(x_{\max}\), so the multiplicity check counts maxima and the convex order calculation uses normalized reciprocal logs. Weibull becomes inverse Weibull. The signed seam and finite compare checks use the same transplanted log objectives. Constants are unbounded, and the maximum-multiplicity class is closed.

**Exact gap samples.** The componentwise reciprocals of \(\exp(Y(1/32))\) and \(\exp(U(t_*))\) give the escape and attainment misses, respectively, with the same proofs. [Reciprocal transfer](2026-11-27-lean-burrfree-report.md#results)

## Summary

| Family | Held | Question | Result |
| --- | --- | --- | --- |
| std | All free | Attainment and kurtosis | Proved sufficient screen. Kurtosis-at-most-three escape refuted. |
| std | All free | Escape and closure | Partial overall. Closed for reflection-symmetric samples on three equally spaced levels and for several larger normal or collapse classes. |
| lst | All free | Attainment and kurtosis | Proved sufficient screen. Kurtosis-at-most-three escape refuted. |
| lst | All free | Escape and closure | Partial overall. Closed on at most two levels, with additional normal and critical-collapse classes. |
| sstd | All free | Attainment | Proved finite witness and boundary upper tests. |
| sstd | All free | Escape and closure | Partial overall. Closed on at most two levels. Conditional signed cover tests for positive-width competitors. |
| sged | All free | Attainment | Proved finite witness and half upper tests. Exact finite/uniform equality. |
| sged | All free | Escape and closure | Partial overall. Closed on at most two levels. Uniform coarsening and half-block extensions. |
| burr | All free | Attainment | Proved seam and finite witness tests. |
| burr | All free | Escape and closure | Partial overall. Pareto order test and signed Weibull cover. |
| invburr | All free | Attainment | Proved by reciprocal transfer. |
| invburr | All free | Escape and closure | Partial overall. Power order test and signed inverse-Weibull cover. |
| All six | All free | Gap | Both misses proved on exact samples. Gap frequencies estimated by simulation. |

The closed subcases resolve the winning face as well as nonexistence, allowing tied winning faces. No simple criterion is proved for every sample in any of the six regimes. There is no new cardinality claim.

## Simulation

**Numerical.** NumPy binary64 replay with seed 42520261127, 200 independent samples per cell, 54 cells, 10,800 samples in total. Simulation estimates the probability that the stated finite-budget screens leave a sample undecided. The continuous-data distribution of actual finite attainment within the gap was not estimated. There is no optimizer ground truth in this calculation. All displayed gap rates have Monte Carlo standard errors of at most 3.54 percentage points. The replay uses a \(10^{-9}\) log-value allowance for separated comparisons. The independently checked exact examples use outward interval arithmetic at 80 decimal digits for the Student and SGED checks and 65 for the reused Burr comparison, with exact rational dual-weight correction in the SSTD example.

The Student design parameters are \(q\) for the symmetric families and \((q,\xi)\) for SSTD. SGED uses \((p,\xi)\). The two skew generators choose sides with mass proportional to width and scale a half-kernel accordingly. Burr and inverse Burr use \((k,c)\). Common location or positive scale cancels in all comparisons. The inverse model was drawn independently and tested on its reciprocal logs.

| Family | Shape parameters | Gap at 10 | Gap at 30 | Gap at 100 |
| --- | --- | ---: | ---: | ---: |
| std | 3 | 90% | 44.5% | 22% |
| std | 6 | 88% | 49% | 14.5% |
| std | 20 | 87% | 65% | 40.5% |
| lst | 1 | 21.5% | 0.5% | 0% |
| lst | 3 | 61% | 20% | 0.5% |
| lst | 12 | 73.5% | 54.5% | 24% |
| sstd | 3, 1 | 97.5% | 60% | 11% |
| sstd | 6, 2 | 99.5% | 82.5% | 45% |
| sstd | 20, 0.5 | 99.5% | 91.5% | 87% |
| sged | 1, 1 | 78.5% | 15% | 0% |
| sged | 2, 2 | 90.5% | 68% | 21.5% |
| sged | 8, 0.5 | 88% | 50.5% | 53.5% |
| burr | 0.25, 2 | 69% | 19% | 0% |
| burr | 1, 1 | 32% | 3% | 0% |
| burr | 8, 2 | 21% | 6.5% | 10% |
| invburr | 0.25, 2 | 70.5% | 25.5% | 0% |
| invburr | 1, 1 | 35.5% | 4.5% | 0% |
| invburr | 8, 2 | 20.5% | 6% | 8.5% |

The simulated `std`, `lst` and `sstd` escape screens recognize tied or precisely symmetric structures of probability zero under the continuous designs. This explains their zero escape counts. SGED's coarsening check selected one uniform escape. Burr's cover and order screens selected escapes in 12 of the 18 Burr and inverse Burr design cells, with nonzero escape counts at every sample size for the two largest-shape designs. At sample size 100 the attainment screen succeeds on most replicates in 16 of the 18 parameter settings. For SSTD with \(q=20,\xi=1/2\), and SGED with \(p=8,\xi=1/2\), many draws remain in the gap even at that size. [Complete counts](2026-11-27-checkable-math-data/simulation.csv)

The stencils, budgets and width bracketing rules are explicit in the replay. The SSTD finite grid is \(q\in\{9/4,3,5,10,25,80\}\) and observed inner knots or adjacent midpoints. Its half shapes start at 49 geometric knots between two and 512 and include a terminal interval to infinity. The SGED finite grid uses the same join stencil and shapes \(1,5/4,3/2,2,3,4,6,8,12,24,48,96,192\). Burr's seam cover starts at 49 geometric knots after its signed cutoffs, with adaptive subdivision. Failed separation or budget exhaustion leaves a gap result. [Replay implementation](2026-11-27-checkable-math-data/pilot.py)

The final Python runs completed on the workstation, under one slot each. From this worktree, with NumPy and mpmath available, run the commands sequentially.

~~~sh
d=dev/research/evaluations/2026-11-27-checkable-math-data
export PYTHONDONTWRITEBYTECODE=1 OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1
/tmp/umvl-cpu/run nice -n 10 python3 "$d/pilot.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$d/check_exact.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$d/verify_replay.py"
~~~

## What this changes for the fitter

- A strict attainable witness rules out stopping at an excluded face. It does not identify the maximizing coefficients. Use the ordinary finite search and coefficient-enclosure path for an estimate. The same applies when the positive normal or Weibull slope is the witness mechanism.
- The closed multiplicity and symmetry checks can run before a finite search. Return `Unbounded` on strict collapse and a bounded scale-collapse nonexistence with its path likelihood when that path wins. Normal and positive-width \(t_2\) are different outcomes for `std`. For `lst` a selected positive-scale Cauchy remains an attained shape-one fit. The SSTD half-\(t_2\) outcome carries the orientation and the shape-two intersection. The SGED two-level half winner is half-Laplace with broad width \(mR\) and support endpoint at the more common value. Its uniform winner uses the sample range. Preserve both winning limits when tied.
- Burr Pareto and inverse Burr power selection use the full trace at the sample extreme. The scale and exponent in original units follow from the reachable face and mean log excess. Use the signed and compact tests together for Weibull or inverse Weibull. A separated face preference by itself establishes neither escape nor attainment.
- Apply exact tie predicates to represented sample values, and enclose all comparisons including scale or shape roots used in the bound. Implement bounded evaluations under the existing certificate allowance. An exhausted or inconclusive comparison leaves the successful accuracy-checked fit uncertified unless a separated witness disproves it. Preserve the score gate for failed searches, and keep a finite maximizing set when equality is proved. [Repository diagnostics](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Dossier follow-ups

- The SSTD all-free existence wording can say “For a nonconstant sample with at most two support points there is no finite MLE. With smaller mass below one third the likelihood is unbounded. At equality the critical half collapse wins. Above one third compare the explicit half-\(t_2\) and half-normal heights.” The existing strict-tie description of the joint envelope should also mention the already proved critical envelope and the critical-extreme decision. Universal all-free uniqueness has been refuted by the prior six-level example. [SSTD proof](#sstd-held-all-free) · [Prior finite nonuniqueness](2026-11-27-sstd-atlas-math-report.md#six-level-sample-and-exact-separation)
- The SGED two-point sentence can replace “balanced” by the exact smaller-mass threshold \(1/e\), with the shape-one half winner at smaller masses. Finite all-free nonuniqueness is already proved, and the new transition shows finite attainment at the uniform likelihood. [SGED proof](#sged-held-all-free) · [Prior reflected pair](2026-11-27-sged-atlas-math-report.md#a-finite-reflected-pair-across-eight-masks)
- For `std` and `lst` the all-free text can state the symmetric normal condition and distant-observation critical collapse test. The latter distinguishes the attained Cauchy endpoint from collapse under `lst`. The correct replacement for any blanket kurtosis-at-most-three rule is the positive-kurtosis inward implication with a separate lower-face test when needed. [Student proofs](#shared-student-calculations)
- Burr and inverse Burr can add the normalized-log order condition as an extension of their existing half-at-the-extreme rule. The cited all-free closure already lists the correct two proper escape competitors. [Burr proof](#burr-held-all-free) · [Inverse Burr proof](#invburr-held-all-free)

## Open

- **Student normal and lower faces on continuous data.** The chord and signed collar tests are valid conditional checks. The pilot does not include an economical global normal cover for generic asymmetric samples. The weak step is uniformly bounding the finite \(q\) complement cheaply when the normal-tail cutoff is conservative. At a zero inward coefficient the first signed bound gives no direction. The closed `std` symmetric three-level result does not settle generic positive-width \(t_2\) wins or critical ties of unrestricted geometry.
- **SSTD regular shape faces and interior half-\(t\) wins.** A concrete small statistic for either regular two-piece face is open. The signed tests need a tube enclosing all its competitive nuisance points with the same separated inward sign and a strict cover of the complement. The joint half-corner comparison is a separate obstruction if such a tube touches a vanishing width. The strict-value cover for an interior half winner is conditional. No exact all-free example with an interior half-\(t\) winner was certified here. The critical atom away from the extremes also remains a joint comparison problem.
- **SGED multilevel half decisions.** The block proof and the signed cover give sufficient tests. A general inexpensive rule needs a bound over join cells and shape that separates all other finite/skew competitors at the winning half height. The uniform threshold proved for two points does not give this comparison for general half-GED shapes. The finite/uniform equality construction shows why strict margins alone cannot give a closed all-sample test.
- **Burr and inverse Burr.** A reusable small-statistic Weibull or inverse-Weibull rule would need a uniform inequality over the compact first-shape complement of the signed tail. The dual cover works on separated samples and cannot resolve a true finite/face tie with strict gaps. The reference order test misses Pareto-winning samples outside its convex order cone. A covering family of references with rigorous and useful coverage over continuous models remains open.
