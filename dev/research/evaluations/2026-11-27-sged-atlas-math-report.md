# SGED boundary envelopes and held-parameter likelihood ties

TASK-402, base `fc24c05ba5cc8eac994292af2e71f9cde1f3592c`. The SGED attainment and failure results below apply to all nonempty finite real samples in the listed masks, with positive finite held sd, shape and skew. The estimated shape domain is \(p=\nu\ge1\), including one. The uniqueness statements concern finite maximizers. All 15 requested masks admit exact counterexamples to universal finite uniqueness. The examples prove multiplicity on their stated samples. The cardinality of the maximizer set for arbitrary samples is a separate problem.

The fixed-shape GED Lean results give the conditional scale and location graphs, including the median interval at one. They have no cross-shape comparison. The prior GED analysis already has a separated global tie for both held-sd masks. The SGED held-width analysis gives the optimized uniform contact penalties and the fixed-moment skew analysis solves the strict-support finite-shape ends. The new skew contact calculation covers the exact endpoint left unresolved by the latter analysis, including the change in that endpoint when shape is free. [GED Lean proof](../../lean/UnivariateML/Family/Ged.lean) · [GED shape comparisons](2026-11-25-ged-math-report.md) · [SGED uniform tails](2026-11-25-sstdsged-math-report.md) · [SGED fixed-shape and held-moment analysis](2026-11-08-sged-held-report.md) · [Tie classification](2026-11-25-ties-math-report.md)

## Common SGED proof

Use \(t=1/p\), join \(c\), total kernel width \(S=a+b\), right-minus-left fraction \(z=(b-a)/S\), and \(w_e=(1+ez)/2\) for \(e=\pm1\), so \(\xi^2=(1+z)/(1-z)\). Put
\[
 g_j=\log\Gamma(1+jt),\quad k=\Gamma(2t)/\Gamma(t),\quad
 v=\Gamma(3t)/\Gamma(t),\quad D=3v-4k^2,\quad W=\sqrt{v+Dz^2}.
\]
The moment equations are \(\mu=c+kzS\), \(\sigma=SW/2\). The log likelihood is
\[
 \ell=-n(\log S+g_1)-\sum_i
 \left(\frac{|x_i-c|}{S w_{\operatorname{sgn}(x_i-c)}}\right)^p,
\]
with zero loss at a join. Write \(L,H\) for the extremes, \(R=H-L\), \(A,B\) for their counts when distinct, and \(\rho=\max_i|x_i-\mu|\) when mean is held. All face comparisons below use joint log likelihood.

### Localization and comparison rule

**Proved.** Compactify the coordinates \((c,S,t,z)\) by the closed extended lines for \(c,S\), the closed shape interval \(0\le t\le1\) or the fixed positive \(t\), and \(-1\le z\le1\) or the held \(z\). Use the closure of feasible vectors in that product. On the shape coordinate range \(k,v,e^{g_1},W\) are bounded above and bounded away from zero, uniformly in \(z\) for \(W\). For the last assertion use
\[
 W^2=v(1-z^2)+4(v-k^2)z^2
\]
and the positive variance \(v-k^2\) of the half kernel, including its uniform limit. Holds persist whenever \(S,c\) are finite. With sd held the admissible \(S\) range is compact and positive.

As \(S\to\infty\) the peak bound forces \(\ell\to-\infty\), regardless of the other coordinates. For \(S\to0\), unless all observations converge to the join, a separated residual gives a penalty at least \((d/S)^{p_0}\) for some \(d>0\), where \(p_0=1\) if shape is free and \(p_0=p>0\) otherwise. This dominates the log peak. If all observations equal \(x_0\), collapse can give \(+\infty\) precisely when sd is free and the mean is free or equals \(x_0\). Use any eligible fixed finite shape and skew, and use \(c=x_0\) for free mean or \(c=x_0-kzS\) for held mean. In the second case the tied ratio is \(k|z|/w_{\operatorname{sgn}z}\), bounded as \(|z|\to1\). The same construction with \(p\to\infty\) has bounded tied loss since that ratio is at most \(k\) and \(k\to1/2\). If \(c\to\pm\infty\) at bounded positive width, the penalty diverges. These arguments include simultaneous divergences or degenerations by taking subsequences in the product.

After ruling out collapse, the only remaining excluded points of possible finite envelope are the finite-shape half ends and the uniform end analyzed below. The continuous finite log likelihood and the compact closure give a maximum of the upper semicontinuous envelope. Let \(F\) be the largest of the eligible face suprema, with missing faces and impossible support assigned \(-\infty\). Attainment holds **iff some feasible finite parameter has likelihood at least \(F\)**. A strictly superior finite witness forces an interior winner by the closure lemma. Equality at a finite witness gives attainment whether that witness is global or a better finite solution exists. If all finite values are strictly below \(F\), precisely the boundary points with envelope \(F\) are maximizing escapes. A supremum on the finite-shape half end approached only as \(p\) diverges is represented by its uniform boundary point. If \(F=-\infty\), every finite point is a witness. With collapse the supremum is infinite. This comparison uses a separately computed boundary benchmark, and can use finite local candidates instead of arbitrary witnesses when \(F\) is finite. Retain smooth stationary points, attained shape one, and all applicable joins. For a candidate-only search a finite equality witness that is not local entails a strictly better finite winner. A supremum of finite-domain values equal to \(F\) alone gives no equality witness. [Joint envelope and closure lemma](../ml-behaviour.tex)

### Finite-shape half ends

**Proved.** Free skew permits \(z\to1\) or \(z\to-1\). Give the right-hand formula and reflect for the left. At a boundary point with finite \(p,S,c\) and \(z=1\), a sample point below \(c\) gives \(-\infty\). For \(x_i>c\) the limiting penalty is \(((x_i-c)/S)^p\). Each occurrence of \(c\) has limiting penalty zero in the **upper envelope**, except with shape and both moments held. Hence for admissible support the right envelope is
\[
 -n(\log S+g_1)-\sum_{x_i>c}((x_i-c)/S)^p
 - \#\{i:x_i=c\}C^p.
\]
Here \(C=0\) except in that three-hold mask, where \(C=2kv/(v+D)\). Boundary values of \(S,c\) obey the held moment equations at \(z=1\).

The broad-side terms converge jointly and the contact penalties are nonnegative. For free mean set the join to its limiting value, with \(S\) solved from held sd when applicable. For held mean and free sd choose \(S=(\mu-c)/(kz)\) near the face and the join stays exactly at \(c\). With both moments held write \(q_i(z)=(x_i-\mu)W(z)/(2\sigma)+kz\). At contact and fixed \(p\), \(q_i(1)=0\) and \(q_i'(1)=kv/(v+D)>0\). The residual is on the thin side near the end, and its ratio \(-2q_i(z)/(1-z)\) tends to \(C\) for every path. All such ties have the same penalty. The corresponding likelihood is the trace of the constrained family at the half law.

With both moments held and shape free, the right half endpoint is \(c(p)=\mu-\sigma k/\sqrt{v-k^2}\). It is strictly decreasing in \(p\): \(v/k^2=(4/3)e^\Delta\) with \(\Delta=g_3+g_1-2g_2=\sum_{j\ge1}\log(1+t^2/[(j+t)(j+3t)])\) strictly increasing in \(t>0\), by termwise monotonicity. Approach any finite \(p_*\) from above, with skew sufficiently close to its endpoint at each shape that all contacts are on the broad side. Their ratios tend to zero. This joint feasible path also applies at \(p_*=1\). A fixed-shape slice through the same boundary point can therefore have a strictly smaller trace than the joint envelope. For the weak law at positive finite shape, integrate the broad-side density and use that the disappearing side has mass \(w_-\). The analogous left end follows by reflecting data and mean.

### Uniform end

**Proved.** At \(t=0\) with positive finite width, the interval is \([c-Sw_-,c+Sw_+]\), with midpoint \(\mu\) when mean is held and \(S=2\sqrt3\sigma\) when sd is held. This law follows from convergence almost everywhere of the kernels and preservation of their total mass. Any observation strictly outside the interval gives an envelope of \(-\infty\). The following formulas give the joint envelope on the support, with the entire sample counted with multiplicity.

- **Free sd.** The value at a supported interval is \(-n\log S\), including contacts. For any fixed interior skew, slightly inflate \(S\) so the limiting interval enlarged about its midpoint strictly contains the sample. Keep the midpoint fixed by the moment equation with held mean, or move the join to keep that midpoint if mean is free. Take \(p\) sufficiently large for the penalties at the expanded interval to vanish, then let the inflation go to zero. At a skew endpoint use a diagonal sequence of interior skews converging to it. The benchmark at the smallest supporting width is \(-n\log R\) for free mean, or \(-n\log(2\rho)\) for held mean, outside the collapse cases.
- **Held sd and free mean.** Write \(S_0=2\sqrt3\sigma\). If \(R>S_0\) the face is impossible. If \(R<S_0\), shift the support by an arbitrarily small amount towards strict interior containment at supported boundary points. At a skew endpoint first approximate with interior skews and adjust the join to preserve the shifted interval. A diagonal limit proves the unpenalized envelope \(-n\log S_0\). The maximal face cannot beat finite fits in this strict case. Choose an interval strictly containing the sample and a fixed feasible interior skew. The gamma peak excess has positive quadratic order in \(t\) and the penalty decays exponentially. At \(R=S_0>0\) the only supported interval is \([L,H]\). For limiting left weight \(w=w_-\) its value is
  \[
  -n\log R-e^{-\gamma}(A/w)^w(B/(1-w))^{1-w}.
  \]
  End weights are evaluated continuously, giving penalties \(Be^{-\gamma}\) at \(w=0\) and \(Ae^{-\gamma}\) at \(w=1\). For free skew the maximum uses the smaller count. For held skew use its weight.
- **Held mean and sd.** Write \(r=\sqrt3\sigma\). If \(\rho>r\) the end is impossible. If \(\rho<r\) the unpenalized value \(-n\log(2r)\) holds at every eligible limiting skew and is strictly beaten at a fixed feasible interior skew by the same peak argument. If \(\rho=r\), the envelope is \(-n\log(2r)-N_c e^{-\gamma}\) for \(N_c=\#\{i:|x_i-\mu|=r\}\). This holds also at limiting skew endpoints.

For the free-mean contact formula use the two-extreme optimized penalty (3) of the prior analysis. Projection of any join to \([L,H]\) lowers the penalties. Its denominator is \([w(w/A)^u+(1-w)((1-w)/B)^u]^{1/u}\), \(u=1/(p-1)\). Its log tends uniformly to \(w\log(w/A)+(1-w)\log((1-w)/B)\) by Taylor expansion. The weighted second moments of the logarithms stay bounded since \(w\log^2w\) is bounded and the positive parts of the logarithms are bounded. The numerator tends uniformly to \(e^{-\gamma}\) from the held-width gamma expansion. For sharpness use the optimizing join at fixed interior skew where the interior observations have vanishing penalties. A diagonal limit reaches either end weight. For free skew Jensen bounds the cost below by the smaller count times \((R/S)^p\), so an end weight gives the global optimum. For held mean and sd use the prior bound on each contact ratio
\[
 2k+\frac{\sqrt3 W-2k}{1+ez}\ge 2k,
\]
where \(e\) is the contact sign relative to the mean. The difference in the numerator is nonnegative and \(O(t^2)\) uniformly in skew, while \(\log(2k)=-\gamma t+O(t^2)\). At fixed interior skew each penalty tends to \(e^{-\gamma}\). For a limiting skew endpoint use a diagonal path, or take \(1-|z|=\sqrt t\) to make the extra ratio term \(o(t)\). At such a corner individual paths can have strictly greater losses. These calculations concern the joint envelope and preserve both moment constraints. [Optimized two-extreme loss and signed bounds](2026-11-25-sstdsged-math-report.md#2-sged-held-width-and-a-free-mean) · [Both-held contact and gamma expansion](2026-11-25-sstdsged-math-report.md#additional-signed-shape-tails)

### A finite reflected pair across eight masks

**Proved.** Use \(a_0=2^{-10}\) and the sample with one each of \(\pm1\) and three each of \(\pm a_0\). Put \(m=(1+3a_0)/4\). Where held, set \(\mu=0,p=1,\sigma=\sqrt2m\). Leave skew free. At the reflection-fixed coefficients the best geometric likelihood is at most \(Q=1/(2em)\) by the earlier GED global Laplace proof for this exact sample. In particular any reflection-fixed fit has zero mean and unit skew and belongs to that GED problem even when some coefficients are held. The unit-skew Laplace at \(\sigma=\sqrt2m\) achieves \(Q\), both at join zero and anywhere in the median interval when mean is free. [GED median-interval proof](2026-11-25-ged-math-report.md#exact-finiteuniform-equality-and-an-all-free-median-interval)

There is a strict finite improvement. For \(p=1\), take \(S=2m/\sqrt{1+z^2}\), which preserves the specified sd when held and is feasible otherwise. With mean free keep \(c=-a_0\). The derivative of the average penalty at zero is negative: it is \(2/(nS)\) times the left sum of distances minus the right sum, and the latter exceeds the former by \(na_0\). The log height has derivative zero. With mean held take \(c=-zS\); for small \(|z|\) this stays in the central gap and the average penalty becomes \((\sqrt{1+z^2}-2z^2)/(1-z^2)=1-z^2/2+O(z^4)\). The log height gains \(z^2/2+O(z^4)\), giving a strict improvement.

All excluded ends are strictly below \(Q\). Without held sd the uniform height is \(1/2\); with held sd the width \(2\sqrt6 m<2\) cannot cover the data. For each right half at finite \(p\ge1\), the best possible endpoint without moment constraints is \(-1\), as any smaller endpoint increases every distance at a fixed width. Profiling its width gives geometric likelihood \(b_p/M_p\), where \(M_p\) is the power mean of the distances to \(-1\) and \(b_p=\exp[-g_1-t(1+\log p)]\). Jensen and the observation at 1 give \(M_p\ge\max(1,2\cdot8^{-1/p})\). The function \(b_p\) increases with \(p\) since \(\psi(1+t)>\log t\) by strict convexity of log Gamma and the secant from \(t\) to \(1+t\). Also \(b_p<p^{-1/p}\) by \(g_1> -\gamma t\) and \(\gamma<1\). On \([1,4]\) the bound is \(b_4<1/\sqrt2\). On \([4,8]\) it is at most \(b_8\cdot 8^{1/4}/2<2^{-5/8}\), and above eight it is at most \(8^{1/8}/2=2^{-5/8}\). The reflected bound is the same. Each is strictly below \(Q\). The localization theorem now gives a finite maximizer away from the reflection-fixed set. Reflection preserves the sample and all holds in this construction, and produces a distinct finite maximizer of equal value. This proves at least one reflected pair, with no assertion about its total cardinality.

## SGED regimes

### sged held all free

**Proved.** For constants there is scale collapse. Otherwise the competing ends are the uniform on the sample range and the reflected finite-shape half ends, with the joint envelopes above. The comparison rule is iff, including finite equality. In the unrestricted family the previously proved GED small-shape spike at unit skew already gives an infinite supremum on every nonempty sample. The fitted domain has a finite nonunique example by the reflected-pair construction. Balanced distinct two-point counts give a strict uniform escape by the earlier log-concavity argument. [Excluded small shape](2026-11-25-ged-math-report.md#excluded-small-shape) · [Balanced two-point proof](2026-11-08-sged-free-report.md#balanced-two-point-uniform-supremum)

### sged held mean

**Proved.** Collapse occurs iff all observations equal the held mean. Otherwise compare the centered uniform of width \(2\rho\) with both finite-shape half ends, constrained by \(c=\mu-kzS\). There is no half-contact penalty in the envelope. The comparison rule yields attainment iff a finite witness reaches the largest boundary value. A unique farthest *side* permits a strictly better finite fit than the uniform by the previously proved outward-thin-side path, even when there are several observations at the same farthest point. A half may still win. The reflected-pair sample refutes universal uniqueness. [Held-mean signed and unsigned tails](2026-11-25-sstdsged-math-report.md#additional-signed-shape-tails)

### sged held mean+nu

**Proved.** All observations equal to the held mean give scale collapse for any held positive shape. Otherwise the only possible winning excluded ends are the two finite-shape half laws under \(c=\mu-kzS\). For each, maximize the displayed half envelope over positive \(S\), including feasible endpoint contacts at zero cost. Uniform escape is unavailable. Attainment is exactly the common finite-witness test. The reflected-pair sample with shape one proves nonuniqueness.

### sged held mean+nu+sd

**Proved.** There is no scale or shape escape. Compare the likelihood on \(-1<z<1\) with the two half values, with \(S=2\sigma/W\), \(c=\mu-kzS\). Unsupported faces have value \(-\infty\); each endpoint occurrence costs \([2kv/(v+D)]^p\). In particular, if the sample extends strictly beyond each half's support there is always a finite maximum. Finite equality attains. Nonuniqueness follows from the reflected construction. The prior scalar proof moreover gives *exactly* two reciprocal maximizing skews for the pair \((-11/10,11/10)\) at \((\mu,p,\sigma)=(0,1,1)\), because its stationary equation in \(z^2\) is strictly monotone with a single interior root and both skew ends diverge to \(-\infty\). [Prior exact scalar proof](2026-11-08-sged-held-report.md)

### sged held mean+sd

**Proved.** Compare the uniform on \([\mu-\sqrt3\sigma,\mu+\sqrt3\sigma]\) with the two half ends obeying both moment equations. At finite shape their envelope has zero contact penalty, by allowing feasible variation of shape in its approach. At infinite shape use the held-mean contact count. The comparison rule is iff, with no collapse. With strict uniform containment only a half end can prevent attainment. The reflected-pair sample proves nonuniqueness.

### sged held mean+sd+xi

**Proved.** The uniform is the sole excluded end, with the held-mean contact count. Any noncontact sample attains, through uniform infeasibility or strict finite dominance. At contact a finite witness at least equal to the envelope is necessary and sufficient. For a sample with exactly \(\xi=1\), the GED separated-shape tie below with held mean and sd proves nonuniqueness.

### sged held mean+xi

**Proved.** Collapse occurs iff all residuals to the held mean are zero. Otherwise the only excluded competitor is the centered uniform of width \(2\rho\), with no contact penalty. The comparison rule is iff. The earlier signed bound gives a finite high-shape cutoff for every fixed positive skew and nonzero \(\rho\). Unit skew and the new GED held-mean tie below refute finite uniqueness. [Signed bound](2026-11-25-sstdsged-math-report.md#additional-signed-shape-tails)

### sged held nu

**Proved.** Constants collapse. Otherwise compare the two finite-shape half ends with free \(c,S\). In either orientation the best endpoint is the corresponding sample extreme and has no contact penalty. This follows by translating any supported endpoint to the extreme to reduce every broad-side distance, then taking the unique conditional width when the sample is nonconstant. The comparison rule is iff and covers held subunit shapes as well. The prior four-knot example at shape \(1/2\) proves finite nonuniqueness by comparing two interior joins with both half faces. The reflected-pair sample supplies another example at shape one. The earlier two-point endpoint tie consists of two excluded half faces. [Fixed-shape mode reduction](2026-11-08-sged-free-report.md#o4-with-free-moments-exhaustive-modeskew-certificate) · [Subunit and shape-one knot reduction](2026-11-07-sged-skew-report.md)

### sged held nu+sd

**Proved.** Compare the half ends with \(S=2\sigma/W\) evaluated at \(z=\pm1\) and endpoints optimized as in the preceding regime. The only remaining variables are skew and join. In particular a constant sample selects the half peak at its common value, as maximizing over the join gives the peak and \(D>0\) makes the peak strictly increase in \(|z|\). Finite equality in general attains. The reflected-pair sample with held shape and sd proves nonuniqueness.

### sged held sd

**Proved.** Compare both finite-shape half ends with the width-constrained uniform, using the smaller endpoint count at exact width contact. The half endpoint is at its corresponding sample extreme, at every finite shape, with zero contact penalty. No other escape can win. With \(R<S_0\) a finite fit beats uniform. For constants the peak increases strictly with \(|z|\) and with \(t\) since \(g_3-3g_1\) and \(\Delta\) both strictly increase with \(t\), and the log excess is \((g_3-3g_1)/2+\frac12\log[1+3z^2(1-e^{-\Delta})]\). Hence the half-Laplace peaks win and are unattained. The reflected-pair sample with held sd refutes finite uniqueness.

### sged held sd+xi

**Proved.** Only the width-constrained uniform can win outside the domain, and its exact-contact penalty uses the held weights. Away from exact contact there is always a finite maximum. At contact the finite-witness test decides attainment. Unit skew with the GED held-sd median-interval example below proves nonuniqueness.

### sged held xi

**Proved.** Constants collapse. Otherwise only the unpenalized range uniform can prevent attainment. The earlier weighted-shape profile provides a signed finite cutoff and the finite-witness test including equality. The reflected construction requires free skew, but at held unit skew the same sample has the GED median interval at shape one and its common fitted sd, which exactly gives the finite maximizing set of that example. [Weighted profile cutoff](2026-11-08-sged-free-report.md#uniform-large-shape-tail) · [Exact median-interval proof](2026-11-25-ged-math-report.md#exact-finiteuniform-equality-and-an-all-free-median-interval)

The four other SGED masks were outside the requested open-question list. With both shapes held and only mean free, localization has no excluded winner and the complete conditional location set follows from the weighted convex loss or subunit knot test. With both shapes held and both moments free the nonconstant sample attains by localization and the conditional scale profile, while constants collapse. With both shapes and mean held, only scale collapse on zero residuals can win outside the domain, and the constrained scale comparison is the earlier inverse-scale or subunit cover. With all four held the domain is a singleton. [Fixed-shape certificate and scope](2026-11-06-sged-cert-report.md) · [Asymmetric subunit scale comparison](2026-11-07-sged-subunit-report.md)

## GED regimes

### ged held mean

**Proved, exact separated finite tie.** Hold mean zero and free sd and shape. Let \(N=2^{131072}\). For \(h\in[0.103,0.105]\) take \(N\) copies each of \(0,h/2,1/2\) and one copy of (1). Multiplicities specify the sample exactly. For the bulk alone, the log profile per observation \(G_b(p,h)\) relative to the uniform of radius \(1/2\) is the earlier \((0,h,1)\) profile after scaling. It is strictly decreasing on \([1,2]\) and has \(G_b(1,h)=\log(3/(1+h))-1\). Its bound for \(p\ge2\) is \((\log3-\log p-1+\gamma)/p\). Both properties are from the prior proof. [Bulk profile and signed bounds](2026-11-25-ged-math-report.md#free-standard-deviation)

The actual log profile per observation, still relative to the bulk uniform with log density zero, is
\[
 G_N(p,h)=-\log\Gamma(1+1/p)
 -\frac{1+\log p}{p}
 -\frac1p\log\frac{1+h^p+2^p/N}{3+1/N}.
\]
This follows from the unique conditional width. The extra observation gives \(G_N\le G_b+p^{-1}\log(1+1/(3N))\). The 40-digit interval checks below show the following strict inequalities, using \(1/(3N)<10^{-6}\) for the perturbations.

1. At \(h=0.103\), \(G_N(1,h)>0\), whereas \(G_N(p,h)<0\) for all \(p\ge2\), by the bulk bound with the perturbation added.
2. At \(h=0.105\), all \(1\le p\le2\) give \(G_N<-0.001\). At \(p=65536\), the logarithm of the loss fraction in the formula is negative since \(h^p<1\) and \(2^p/N<1\). Also \(\log\Gamma(1+1/p)\le0\) by convexity on \([1,2]\). Thus \(G_N>-(1+\log65536)/65536>-0.001\).
3. On the entire \(h\) band, \(G_N(2,h)<-0.005\) by the bulk bound, and \(G_N(1,h)>-0.002\) by its explicit formula. Thus shape two never wins on this band.

The actual uniform has radius one and log likelihood per observation \(-\log2\), strictly below the shape-one witness throughout. Set \(P=3N+1\). The earlier free-sd cutoff gives \(G_N(p,h)<-\log2\) for \(p\ge P\). The maxima of \(G_N\) on \([1,2]\) and \([2,P]\) are continuous functions of \(h\) by compactness. Their difference has opposite signs at the two ends of the \(h\) band. Define the exact sample using the smallest zero \(h_*\) of this continuous difference on that closed band. By item 3 neither maximizing branch uses their common endpoint. At \(h_*\) there are at least two distinct globally maximizing shapes, each with its unique conditional sd, and the uniform is strictly worse. This is a real sample specified by an exact zero with an existence proof, the same type of equality as the earlier held-sd separated-shape construction. It provides no binary64 equality claim.

For general nonzero residuals there is a finite candidate set for this mask. On the compact cutoff of the prior report, include the endpoints and the zeros of the analytic shape score. The analytic function extends across the cutoff endpoints and is nonconstant by its strictly negative large-\(p\) gap tending to zero relative to the actual uniform. Thus its derivative has finitely many zeros on the compact interval. Comparing their values, endpoints, and the face gives the full maximizing set as a mathematical characterization. An effective bound on the number of roots and a procedure for exact equality are open.

### ged held mean+sd

**Proved previously, finite nonuniqueness.** Hold mean zero and sd one. Use eight zeros, 23 copies of each of \(\pm a_*\), and 12 each of \(\pm b_0\), where \(b_0\) is `0x1.6c84e093fc0ecp+0` exactly and \(a_*\) is the unique crossing in \((0.329,0.331)\) of the shape envelopes on \([1,2]\) and \([8,100]\). The prior interval proof separates the valley and outer tail from a finite witness throughout the radius band, proves opposite winner order at the band endpoints, and shows strictly different slopes of the two envelopes. The exact real crossing gives at least two global maximizers. The checked dyadic radius in that proof gives two separated local maxima. [Separated and tied maximizers](2026-11-25-ged-math-report.md#separated-and-tied-maximizers)

### ged held sd

**Proved previously, finite nonuniqueness.** The preceding symmetric sample has unique location zero for \(p>1\) and also has median zero at shape one, so freeing mean preserves the complete shape profile and its separated global tie. A second exact example is \((-1,1)\) at held sd \(0.1\), which has the entire median interval at \(p=1\) as its maximizing location set by the small-sd theorem. [Separated-shape and median-interval counterexamples](2026-11-25-ged-math-report.md#separated-and-tied-maximizers) · [Small-sd example](2026-11-25-ged-math-report.md#exact-finiteuniform-equality-and-an-all-free-median-interval)

## Results

| Family | Held | Question | Result |
|---|---|---|---|
| SGED | all free | criterion, failure | proved on fitted domain |
| SGED | all free | uniqueness | refuted |
| SGED | mean | criterion, failure | proved |
| SGED | mean | uniqueness | refuted |
| SGED | mean+nu | criterion, failure | proved |
| SGED | mean+nu | uniqueness | refuted |
| SGED | mean+nu+sd | criterion, failure | proved |
| SGED | mean+nu+sd | uniqueness | refuted |
| SGED | mean+sd | criterion, failure | proved |
| SGED | mean+sd | uniqueness | refuted |
| SGED | mean+sd+xi | criterion, failure | proved |
| SGED | mean+sd+xi | uniqueness | refuted |
| SGED | mean+xi | criterion, failure | proved |
| SGED | mean+xi | uniqueness | refuted |
| SGED | nu | criterion, failure | proved |
| SGED | nu | uniqueness | refuted |
| SGED | nu+sd | criterion, failure | proved |
| SGED | nu+sd | uniqueness | refuted |
| SGED | sd | criterion, failure | proved |
| SGED | sd | uniqueness | refuted |
| SGED | sd+xi | criterion, failure | proved |
| SGED | sd+xi | uniqueness | refuted |
| SGED | xi | criterion, failure | proved |
| SGED | xi | uniqueness | refuted |
| GED | mean | uniqueness | refuted |
| GED | mean+sd | uniqueness | refuted |
| GED | sd | uniqueness | refuted |

## What this changes for the fitter

- Use the uniform and half formulas as separate likelihood benchmarks on the eligible masks. For an unattained winner, return the corresponding uniform or oriented half-GED boundary along with the **approach** likelihood. Keep equal-valued finite candidates when the global comparison closes. Distinguish a tied finite estimate and boundary diagnostic from a tie among excluded limits.
- With fixed shape and both moments held, use the thin-side contact formula in the endpoint likelihood rather than a closed-support density substitution. With shape free, cover skew and shape jointly near finite half contacts. A skew-only endpoint calculation at the limiting shape can underestimate the supremum there.
- With held sd test the exact width order before applying a contact formula. With free mean compare \(R^2\) to \(12\sigma^2\); with held mean compare \(\rho^2\) to \(3\sigma^2\). Positive exact dyadic inputs do not give real equality in either test. Near-contact dyadic inputs can require very large finite shapes. With skew free and strict containment, uniform loses but a half can still win.
- Preserve a successful, locally accuracy-checked search as uncertified when a comparison fails to separate or the certificate budget expires. Certify nonexistence only after proving collapse or an excluded winner without a finite equality witness. Unknown cardinality is appropriate until the finite set has been classified. Exact reflection gives another maximizing witness when a winner is proved away from the reflection-fixed set. Additional global ties still require a complete cover.

## Dossier follow-ups

- SGED's fixed-shape both-held contact paragraph can state the exact limiting penalty \(N_c[2kv/(v+D)]^p\) for a supported half end with \(N_c\) contacts. Its shape-one example remains valid. For the corresponding free-shape route the envelope contact term is zero at any finite shape, by the strictly monotone endpoint motion.
- The SGED existence summary can state the complete fitted-domain comparison on all listed masks, while distinguishing mathematical attainment from the current implementation's certification. In the fixed-shape free-skew two-point paragraph, the two endpoint optima are optima of the *closure*, not finite nonunique fits. The reflected sample above supplies a finite example.
- GED and SGED can cite the exact separated-shape held-mean tie independently of the previous held-sd tie. Its multiplicities make it a mathematical counterexample rather than a normal-size runtime fixture.

## Open

- These counterexamples leave sample-wide cardinality open. The reflected construction separates a finite winner from the symmetry-fixed set but does not isolate the maximizing branches or exclude a third branch. For the new GED held-mean sample the strict valley separates the two compact shape branches but does not count the roots within either branch. A held-mean finite shape tie on exact binary64 data remains open.
- The exact face comparisons are mathematical iff criteria. A practical signed comparison near arbitrary winning half faces still needs constrained shape/skew bounds where both coordinates move through contact. In particular, the envelope value at the joint corner does not determine the sign of the finite-versus-face difference in a punctured neighborhood. Finite/face equality and equality between unrelated critical branches require additional certified value or equality predicates.
- At held-sd contact with held skew and free mean, the sign can change with endpoint imbalance. The prior Taylor test controls the uniform collar there. A budgeted cover that also handles nearly degenerate finite critical points and public moment recovery is still needed. A proof of existence at a strict width deficit does not supply a practical upper bound on a near-contact maximizing shape.

## Reproduction

The [checker](2026-11-27-sged-atlas-math-data/check.py) ran green on the workstation through `/tmp/umvl-cpu/run nice -n 10 python3`. It replays the prior 40-digit GED interval covers and verifies the strict inequalities in the new held-mean and reflection constructions. At 90 digits it also checks the finite-shape contact ratio at \(p=1/2,1,11/4,15\) and \(1-z=10^{-12},10^{-25},10^{-40}\), and the joint uniform contact path at \(p=10^3,10^5,10^7\). The numerical contact replay supports the algebra and supplies no classification on its own. The [atlas rows](2026-11-27-sged-atlas-math-data/proofs.csv) use `one-sided` for counterexamples with general cardinality open. This task changed only the mathematical report and its evaluation data.
