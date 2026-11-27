# Maximum likelihood under fixed truncation for discrete families

TASK-457. Let \(n>0\), let \(m,M\) be the sample extrema, and let \(W\) be the integer points in the closed observation window. Round finite cuts inward before intersecting with the parent support. A whole-support window has the parent likelihood, including its endpoint conventions. Held coefficients are fixed throughout a limit. These results concern the mathematical models in the atlas. In particular, beta-binomial with free size and both shapes free is included even where the current base fitter restricts that route. Source tree at HEAD `239846bf`. [Modifier design](../../design/modifiers.md) · [Atlas conventions](../existence-atlas/README.md) · [Parent proofs](../existence-atlas/proofs.csv) · [Zero-truncation calculations](2026-11-27-zero-mod-math-report.md)

## General lemmas

**Proved.** A compatible parameter gives positive retained mass and positive mass at every occupied cell. Discard other parameters when any compatible parameter exists. If none exists, report incompatibility. Let \(l,u\) denote the effective support ends for each branch. A branch with one cell gives likelihood one for the compatible constant sample, and all coefficients on that branch are maximizers. Other branches can tie only if they too induce that atom. This includes included atoms even when the geometric intersection of the window with the *nondegenerate* support has several cells.

For two retained cells \(l,l+1\), write \(R=g_{l+1}/g_l\), including zero and infinity for included atoms. The likelihood is \(R^j/(1+R)^n\) where \(j\) is the upper count, interpreted by continuity. For a mixed sample it is strictly increasing up to \(R_*=j/(n-j)\) and strictly decreasing thereafter. Compare the supremum of accessible ratios at or below \(R_*\) with the infimum at or above, omitting an empty side, by this same expression. For constants use the appropriate extremum. An endpoint of the ratio image counts as attained exactly when it has a finite admissible preimage. The entire maximizing set is the union of those preimages, across branches, including singletons. This proves the rule also for disconnected images or integer designs, for which projecting in Euclidean distance onto the image would give the wrong comparison. For a connected scalar chart the image is an interval, whose ends are given by the attained extrema (including score zeros) and the two endpoint limits. A continuous interval of ratios is filled by the intermediate value theorem.

More generally, for any nonnegative weights \(w\), put
\[
 B(w)=rac{\prod_i w_{x_i}}{(\sum_{x\in W}w_x)^n}.
\]
All comparisons below use this joint likelihood, with exact limits denoted by that expression or by normalized probabilities. The saturated bound is \(\prod_{x:n_x>0}(n_x/n)^{n_x}\), with equality exactly at the empirical law, by Jensen's inequality. For a normalized geometric tilt take \(w_x=r^{x-l}\) on a finite window or a right ray (the latter requires \(r<1\)); reflect for a left ray. For a Poisson, binomial, negative binomial or logarithmic-series limit use the corresponding normalized or unnormalized weights restricted to \(W\). The weight of a singleton is one and a two-point weight vector \((1-p,p)\) gives \((1-p)^{n_l}p^{n_u}\). These evaluations include limiting laws produced by vanishing conditioning probabilities.

**Finite and infinite normalization.** On finite support, normalize ratios against any positive reference weight. If ratios at a reference position involve zero over zero, take the joint limit of the ratios before normalizing. On a ray, summable domination of those relative weights allows termwise normalization. If their pointwise limits sum to infinity, the denominator diverges by finite partial sums and each cell with bounded relative weight loses its mass. A divergence argument based on a successive ratio going to infinity also forces the preceding cell to lose its mass. Neither argument requires tightness. On any count support extract a diagonal subsequence of probabilities; its joint sample likelihood converges to the product of the limiting masses even if the limiting vector is defective. On a finite window every such vector is a probability vector. Conditional laws need not be conditional versions of parent weak limits.

**Finite versus excluded heights.** Let \(H\) be the supremum of likelihood limits over all sequences leaving every compact subset of the compatible parameter domain, with included boundary charts treated as admissible. Use zero if there is no such sequence. For the models below, continuity on each finite feasible chart and extension to compatible included boundaries give the following test. Attainment holds iff some finite candidate has \(B\ge H\), where candidates are stationary points on continuous charts (all points on flat charts), and the relevant included endpoints and integer slices. Necessity follows by considering a maximizer. If a witness equals \(H\), either it already wins or there is a point of strictly larger height. A superlevel above \(H\) is sequentially compact in the admissible domain, by contradiction using a sequence leaving a compact exhaustion. Thus that level contains a maximizer. If no finite candidate qualifies, the supremum is \(H\). Use the largest candidate height to retain every tie. This test can be implicit when isolating all roots or comparing an integer tail remains open. Listing score roots and their values is insufficient to prove singleton cardinality. [Likelihood closure](../ml-behaviour.tex) · [Equality and support-breakpoint checks](2026-11-27-mod-rev4-report.md#finite-candidates-and-nonuniqueness)

**Finite canonical charts.** For weights \(h_x\exp(\eta\cdot T_x)\), the derivative of the log normalizer is the mean statistic, with Hessian its covariance. Work in the span of statistic differences. A relative-interior empirical statistic makes the log likelihood coercive, since every unit direction has a cell strictly above the empirical projection, with a uniform positive gap by compactness. It gives one root in the identified coordinates. A boundary statistic lies on an exposed face. Along its outward normal all off-face weights disappear, and the reduced mean equations on the smallest face give the unique maximizing law on that face. Indeed the sample likelihood on the face bounds the unrestricted one by the factor \(P(\mathrm{face})^n\); the face solution is approached by the exposing path. Fixed natural coordinates can be absorbed into the carrier. Included coefficient endpoints must be compared even when the exposing path itself goes to infinity.

**Finite rational charts.** If the weights are rational functions of continuous coefficients, use the positive denominators to clear the score equations and compare the objective on every connected component of the resulting semialgebraic critical set. It is constant on each component by piecewise smooth path connectivity. There are finitely many such components. For algebraic holds, quantifier elimination also decides comparisons to semialgebraic face heights and the dimensions and cardinalities of finite winning sets. On two cells this procedure computes ratio images and all preimages with no score calculations in the other cells. This is a decision procedure at a fixed integer design, without a practical uniform complexity claim.

## Finite-population and beta-mixture families

### bbinom all free truncate:known

**Proved.** Write \(s,t\) for the shapes and \(N\) for size. At fixed \(N\), compatible support is \([l,u]=W\cap[0,N]\). Relative weights are given by
\[
 R_k=rac{N-k}{k+1}rac{s+k}{t+N-k-1}.
\]
Finite parameter faces at zero shapes are obtained by dropping to zero in this recurrence, with the following endpoint convention. If \(s\to0\) and zero is retained, its weight dominates unless the upper singular endpoint \(N\) is also retained with \(t\to0\). If \(t\to0\) and \(N\) is retained its weight dominates subject to the same exception. If both singular endpoints are retained with both shapes vanishing, their relative masses are \(t:s\) in the limit, with any split possible. This follows from the parent endpoint products \((t)_N,(s)_N\), of first order \(t,s\), and the interior products of order \(st\). When neither singular endpoint is retained, evaluate the ratio at the limiting finite shapes. For diverging shapes take \(s/t\) in the extended half-line: a positive finite ratio gives binomial odds \(s/t\); an infinite ratio gives \(\delta_u\), and zero gives \(\delta_l\) when \(t\to\infty\). This follows by termwise division, even where only one shape diverges. The descriptions with singular endpoints concern bounded shapes; the diverging-shape ratio takes precedence. At fixed size use the finite rational test against these faces, giving the iff and cardinality decision for algebraic holds. At two cells the ratio displayed above supplies the exact image slice by slice. With both shapes free it spans \((0,\infty)\) on every adjacent pair, since the ratio can be made arbitrarily small by increasing \(t\) or arbitrarily large by increasing \(s\), and varies continuously. For sample \((1,2)\) at \(N=2\) and \(W=[1,2]\), every \(t=(s+1)/2\) fits exactly. With size free these are still global maximizers.

**Growing size.** Put \(h=N/(t+N)\), extract limits of \(s,h\), and when \(s\) diverges extract that of \(sh\). With \(s\to s_0<\infty\), the limiting ratios are \(h_0(s_0+k)/(k+1)\). With \(s\to\infty\) they are \(c/(k+1)\), \(c=\lim sh\), including zero and infinity. A zero initial ratio when \(s_0=0,l=0\) gives an atom on a finite window, even if subsequent ratios are positive. On finite windows the resulting laws are truncated NB at \(s_0>0,0<h_0\le1\) (at \(h_0=1\) use weights \((s_0)_k/k!\)), logarithmic-series weights \(h_0^k/k\) for \(s_0=0,l>0,h_0>0\), Poisson at \(0<c<\infty\), or the appropriate endpoint atom. The finite-window \(h_0=1\) case includes uniform at \(s_0=1\). On a ray, for \(h_0<1\) and bounded \(s\), the ratios are bounded by \(\frac{N}{t+N-1}\frac{s+k}{k+1}\) when \(t\ge1\). This gives a uniform geometric tail, also when \(s\to\infty,h\to0\) with \(sh\) bounded. At \(h_0=1\) and \(s_0>0\) or \(l>0\), limiting weights have divergent sum because their order is \(k^{s_0-1}\). If \(h_0=1,s_0=0,l=0\), every fixed positive cell has weight relative to zero tending to zero. The limit is \(p_0\delta_0\) as a pointwise vector, where \(p_0=\lim\prod_{j=0}^{N-1}(1+s/(t+j))^{-1}\) along the subsequence. It contributes only on all-zero samples, already saturated by size zero. If \(c=\infty\) each fixed cell on a ray loses to its successor. These arguments extend the [positive-ray table](2026-11-27-zero-mod-math-report.md#bbinom-all-free-truncated).

### bbinom alpha truncate:known

**Proved.** Keep \(s>0\) in the preceding fixed- and growing-size comparisons. Size infinity permits NB/power weights at that \(s\) and the lower atom; the \(h_0=1\) branch on a ray has zero likelihood. Size zero still saturates compatible all-zero data. On a two-cell window mixed samples attain any empirical odds using the slice \(N=l+1\), in which varying \(t\) ranges over every positive ratio.

### bbinom alpha+size truncate:known

**Proved.** Keep \(N,s\) fixed. The odds \(R_k\) are strictly decreasing in \(t\); on two cells their image is \((0,R_k|_{t=0})\) with infinite upper endpoint when \(k=N-1\). On larger supports use the fixed-size rational criterion including the \(t\downarrow0\) and \(t\uparrow\infty\) faces.

### bbinom beta truncate:known

**Proved.** Keep \(t>0\) fixed; size infinity has \(h_0=1\). On a finite window use the power, harmonic, or atom rules above and on a ray every such path loses on samples with positives, including the defective case. Thus positive samples on rays need only the finite-design candidates and their within-design faces. When comparing all slices, any positive level admits only finitely many designs, by the growing-size subsequence argument. For two cells put \(j=N-l\). At each \(j\ge1\) the odds image is the open interval from \(lj/[(l+1)(t+j-1)]\) to infinity. The infimum over \(j\) is \(l/(l+1)\) for \(t\le1\) and \(l/[(l+1)t]\) for \(t>1\), since \(j/(j+t-1)\) is monotone. The union is open at that lower end even if a slice minimizes its lower limit. No numerical tail bound for larger windows is implicit in this statement.

### bbinom beta+size truncate:known

**Proved.** Keep \(N,t\) fixed. On two cells the increasing odds have image \((R_k|_{s=0},\infty)\). On larger supports the two shape endpoints and the fixed-size rational criterion apply.

### bbinom size truncate:known

**Proved.** Use the fixed-size two-shape criterion, including singleton slices and the exact mixed-pair tie above. A general singleton assertion for larger windows would require more than rational critical-set isolation.

### bnbinom all free truncate:known

**Proved.** With size \(r\), beta \(b\), alpha \(a\), and \(d=a+b+r\), the ratios are
\[
 R_k=rac{(k+r)(k+b)}{(k+1)(k+d)} .
\]
On two cells the all-free odds span \((0,\infty)\) by taking alpha large at fixed numerators or taking both numerators large at fixed alpha. At held alpha the lower endpoint is \(k^2/[(k+1)(k+a)]\) by monotonicity in both numerator shapes. With just one numerator held at \(b>0\), the image is \((0,(k+b)/(k+1))\) by monotonicity in the free numerator and alpha. These bounds are excluded, and continuity fills each interval. The formula is symmetric under interchange of \(r,b\). Classify limits after ordering the two numerator shapes so the smaller is denoted \(r\), then restore labels before imposing holds.

| Limit | Relative weights or atom |
| --- | --- |
| Bounded \(d\) | Use the limiting recurrence. At \(k=0,d\to0\), define \(rb/d\to0\). |
| \(d\to\infty, r\to r_0<\infty, b/d\to p\) | \(R_k=p(k+r_0)/(k+1)\). This gives NB weights or positive harmonic/log-series weights, with the lower atom if the initial ratio vanishes. |
| \(r,b\to\infty, rb/d\to c\) | \(R_k=c/(k+1)\), giving Poisson weights or an endpoint atom. |

On finite windows these rules cover all sequences; in particular the finite \(a=0\) recurrence and the \(p=1\) weights must be retained. On rays the bounded-\(d\) ratios have expansion \(1-(1+a)/k+O(k^{-2})\), uniform for bounded shapes, giving summable domination for \(a\) bounded below and a divergent limiting series for \(a=0\) unless the initial ratio vanishes. For bounded \(r\le R\), \(b/d\le p'<1\), \(d\ge D\), the upper bound \((k+R)(k+p'D)/[(k+1)(k+D)]\) gives a summable product for \((1-p')D>R+2\). If \(p=1\), the limiting products have order \(k^{r_0-1}\) and diverge, except again when the initial ratio vanishes. In those exceptions \(l=0\), and the pointwise limiting vector is \(p_0\delta_0\), where \(p_0\) is the subsequential limit of \(\Gamma(a+r)\Gamma(a+b)/[\Gamma(a)\Gamma(d)]\). Only a constant zero sample can retain positive height. On that sample the boundary height is obtained from maximizing mass at zero. With a free numerator shape send it to zero at other coefficients fixed; with free alpha send it to infinity at the other shapes fixed. Both give \(\delta_0\) by the displayed domination, establishing the saturated supremum when any coefficient is free. When both numerator shapes diverge and \(rb/d\) remains bounded, the ratio equals \(1+[rb-d-(a+1)k]/[(k+d)(k+1)]\); it is at most \(1-2/(k+1)\) for large fixed \(k\) onward since \(a/d\to1\). If \(c=\infty\) each fixed cell loses to its successor.

Use \(B\) on each of these normalized vectors and compare with finite scores. The score for a free coordinate \(\xi\) in either beta family is \(\sum_i \partial_\xi\log w_{x_i}-n\sum_k g_k\partial_\xi\log w_k\), with each log weight obtained by summing the log successive ratios from a reference cell. Thus the comparison is in terms of sample counts and the displayed laws. On finite windows the rational test is a decision procedure at algebraic holds, including the closure itself, which is the closure of a semialgebraic image. On rays the series and its derivative can be enclosed away from non-summable edges by these bounds. An exact finite-window nonuniqueness witness is \(W=[1,3]\), sample histogram \((10,6,4)\), and \((r,a,b)=(1,1,2),(2,1,1)\). The ratios are \(3/5,2/3\), so both attain the empirical law. The [positive-ray report](2026-11-27-zero-mod-math-report.md#bnbinom-all-free-truncated) also gives an exact integer-histogram nonuniqueness witness on \([1,\infty\)).

### bnbinom alpha truncate:known

**Proved.** In the table require \(d-r-b=a\) fixed. The bounded branch can drop either numerator shape, the bounded-smaller diverging branch has \(p=1\), and two diverging numerators give \(c=\infty\). The exact finite-window histogram above has \(a=1\) and still refutes uniqueness.

### bnbinom alpha+beta truncate:known

**Proved.** Vary \(r\) alone. The two-cell ratio increases from its value at \(r=0\) to \((k+b)/(k+1)\), with both ends excluded. Indeed the log derivative is \(1/(k+r)-1/(k+d)>0\). On larger windows compare these same endpoint laws with stationary values.

### bnbinom alpha+beta+size truncate:known

**Proved.** Evaluate the held law whenever compatible.

### bnbinom alpha+size truncate:known

**Proved.** With only \(b\) varying, the two-cell image and larger-window comparisons are those for a free numerator shape with the labels interchanged.

### bnbinom beta truncate:known

**Proved.** Keep the original \(b>0\) in the table. The growing-\(d\) branch has finite smaller shape; the Poisson branch with both numerator shapes diverging is unavailable. The finite-window criterion uses the same rational isolation, with no permitted label swap unless both labelings respect the hold.

### bnbinom beta+size truncate:known

**Proved.** With only \(a\) varying, the two-cell ratio decreases from its \(a=0\) value to zero. Larger windows compare the zero-alpha face and lower atom with finite candidates.

### bnbinom size truncate:known

**Proved.** Same restrictions as for beta held with its label interchanged. Every hold on a ray is covered by the normalization bounds and the finite-versus-excluded test, including all-\(l\) data. In particular when \(l>0\), holding alpha while freeing one or both numerator shapes sends the lower-cell mass to its supremum as each free numerator goes to zero, by strict monotonicity of all the ratios. With alpha free one can instead reach \(\delta_l\) by increasing it at fixed numerator shapes.

### cmb all free truncate:known

**Proved.** For each integer \(0\le N\le10^7\), take \([l,u]=W\cap[0,N]\). At interior \(p\), use canonical weights
\[
 w_k=\exp\{k\eta+\nu h_N(k)\},\qquad
 \eta=\log[p/(1-p)],\quad h_N(k)=\log {N\choose k}.
\]
Also include \(p=0,1\) precisely when they give a compatible atom at the true parent endpoint, and allow every dispersion there. For at least three cells the graph is strictly concave: its successive slopes are \(\log[(N-k)/(k+1)]\), strictly decreasing. Thus the upper polygon edges join adjacent retained cells, the lower edge joins the two retained ends, and its vertices are individual cells. The finite canonical lemma gives the unique two-coordinate root iff the empirical vector lies in the polygon interior, with boundary cases giving the empirical law on the exposed point or pair. A boundary height is attained only by a compatible included chart, such as a true endpoint atom. With exactly two cells, \(\log R=\eta+\nu[h_N(l+1)-h_N(l)]\). Every mixed sample attains on the full corresponding affine line. With size held at two, \(W=[1,2]\), sample \((1,2)\), this is \(\eta=\nu\log2\). With size free that line still wins by saturation. Apply this and the next three slice rules at every permitted size and retain all branches tying the greatest height. The size bound makes this an exact finite comparison even on a ray.

### cmb nu truncate:known

**Proved.** At each size keep \(\nu\) fixed and apply the \(\eta\) rule for nu+size below, then compare all size heights and inclusion bits.

### cmb nu+prob truncate:known

**Proved.** Evaluate the held canonical coefficients at every compatible size, including singleton and atom slices. The finite comparison gives both attainment and every tied integer design. An exact cross-size tie on the ray \([1,\infty)\), at held \(\nu=1,p=1-q\), is sample \((2)\) and \(q=(\sqrt3-1)/2\). The count-two masses at sizes 2 and 3 coincide by \(2q^2+2q=1\); for \(N\ge3\) the successor ratio is \(q(N+1)(1-q^N)/[(N-1)(1-q^{N+1})]<2q<1\).

### cmb nu+prob+size truncate:known

**Proved.** Evaluate the held law whenever compatible.

### cmb nu+size truncate:known

**Proved.** At least two cells give a unique conditional mean root iff the empirical mean is strictly between \(l,u\). At either extreme the corresponding atom wins, attainable iff the true endpoint is retained there and the associated \(p\) endpoint is admissible. A singleton gives all compatible probabilities.

### cmb prob truncate:known

**Proved.** Apply the held-odds rule at every size then compare their heights and inclusion bits.

### cmb prob+size truncate:known

**Proved.** At held interior odds compare the empirical mean of \(h_N\) with its minimum and maximum on the retained cells. In between, the dispersion root is unique. At an extreme restrict the odds weights \(e^{k\eta}\) to the minimizing or maximizing set, computing \(B\) there. An extreme is unattained unless the statistic is constant, in which case all \(\nu\) fit. Two different retained cells can share this statistic, for instance \([1,2]\) at \(N=3\). Held endpoint probability gives only the compatible atom, with dispersion flat.

### cmb size truncate:known

**Proved.** The fixed-design polygon, affine line and singleton rules give the iff and entire fit set.

### cmp all free truncate:known

**Proved.** Take \(h(k)=\log(k!)\) and weights \(e^{\eta k-\nu h(k)}\), with \(\nu\ge0\), \(\eta=\log\lambda\). Include \(\lambda=0\) iff zero is retained and compatible. On a finite window with at least three cells the permitted outward directions with \(\nu\ge0\) give singletons and consecutive pairs only: \(h\) is strictly convex and the lower chord of the graph is inaccessible as an exposing direction. Constant and adjacent samples saturate, with only the included zero atom attaining on constant zero. Otherwise match the mean by the unique geometric tilt at \(\nu=0\). The limiting geometric chart wins iff
\[
 E_{\rm geom}\log(X!) - \overline{\log(x!)}\le0.
\]
By strict concavity, the sign is the exact inward test, including equality. That winning chart is attained iff its rate is below one, since the unconditioned zero-dispersion parent must normalize. An excluded rate of one or more is approached at fixed rate by \(\nu\downarrow0\). For a positive inward score solve the full two moment equations. On exactly two cells, every mixed sample attains on \(\eta-\nu\log(l+1)=\log R_*\), \(\nu\ge0\). For constants the appropriate atom wins, with the same zero inclusion rule.

On a ray, the same constant/adjacent test and geometric sign apply (the mean-matched geometric ratio is \((\bar x-l)/(1+\bar x-l)\)). To prove exhaustion, pass to limits of \(\nu,\lambda\). For \(\nu\to\infty\) the log successive ratios are \(\eta-\nu\log(k+1)\), with successive gaps tending to minus infinity. Their threshold yields a singleton, adjacent pair, or drift to infinity. A retained pair or singleton has geometric domination above it because the next ratio tends to zero. Bounded \(\nu\) and \(\lambda\to0\) give the lower atom by the ratio bound, and \(\lambda\to\infty\) gives drift. At \(\nu\to0\) finite \(\lambda<1\) permits geometric domination, whereas \(\lambda\ge1\) gives an infinite limiting normalizer. Thus nonadjacent samples attain. Strict covariance on at least three cells gives singleton cardinality for each attained non-atomic fit. For the zero atom on a multi-cell window, every allowed dispersion ties. This extends the [positive-ray rules](2026-11-27-zero-mod-math-report.md#cmp-all-free-truncated).

### cmp lambda truncate:known

**Proved.** At held positive rate the expectation of \(h(X)\) strictly decreases in \(\nu\) unless \(h\) is constant, by its variance. Its upper end is at \(\nu=0\). The limiting expectation is finite on a finite window and on a ray with \(\lambda<1\). The endpoint is admissible exactly when \(\lambda<1\); on a finite window at \(\lambda\ge1\) it gives an excluded geometric tilt, which wins at or above its expectation. On a ray at \(\lambda\ge1\) the expectation diverges as \(\nu\downarrow0\) since every fixed mass vanishes. The lower end corresponds to the minimal-statistic set, namely \(\{0,1\}\cap W\) if it contains both, or the leftmost cell otherwise, with weights \(\lambda^k\) on that set. For positive dispersion tending to infinity, these limits follow by summable domination from any fixed positive dispersion. The minimum gives an excluded supremum, unless the statistic is flat, including the two-cell window \([0,1]\), or the held rate is zero. Solve the unique mean-statistic root strictly between the limits, or use the upper endpoint at or above its limiting expectation with the appropriate inclusion bit. Held zero rate evaluates the allowed atom with free dispersion flat.

### cmp lambda+nu truncate:known

**Proved.** Evaluate the held law whenever compatible; at \(\nu=0,\lambda>0\) the parent domain requires \(\lambda<1\).

### cmp nu truncate:known

**Proved.** For positive held dispersion the mean ranges from \(l\) to \(u\) (infinite on a ray), by the ratio limits, with monotone variance derivative. Use the conditional mean root, otherwise the appropriate point law, with only the true zero atom included. At \(\nu=0\) on a finite window the parent rate restriction \(\lambda<1\) remains: the upper mean is the uniform mean \((l+u)/2\), excluded. Equality and larger sample means escape to the uniform; at the lower extreme use the same atom rule. On a ray use the ordinary geometric rule.

### hyper all free truncate:known

**Proved.** For each integer design \(0\le k,m\le N\), use weights \({m\choose x}{N-m\choose k-x}\) on its effective support. For escaping \(N\) choose the effective lower end \(l\) constant on a subsequence, since \(0\le l\le\min x_i\). Put \(P=m-l,\ Q=k-l,\ D=N-m-k+l+1\), with \(D\ge1\). On a nonsingleton branch \(P,Q\ge1\), and
\[
 R_{l+j}=\frac{(P-j)(Q-j)}{(l+j+1)(D+j)}.
\]
Keep each integer factor either fixed or diverging along a subsequence and take \(C=\lim PQ/D\). If \(C=0\), the lower atom results. If \(C=\infty\), the upper atom results when the upper end stays finite (pass to a constant subsequence); when it diverges all fixed cells vanish. For \(0<C<\infty\), \(D\to\infty\) because \(N=D+P+Q+l-1\) diverges. At least one numerator diverges. With the other fixed at \(P_0\), the limit has ratios \(C(1-j/P_0)/(l+j+1)\) up to \(j=P_0-1\), the binomial of size \(l+P_0\) and odds \(C/P_0\), conditioned to the retained interval. If both diverge the ratios are \(C/(l+j+1)\), a truncated Poisson. On a ray the ratios when \(C\) is finite are bounded by a constant over \(l+j+1\), giving summable domination. These limits retain \(l=\max(\inf W,0,k+m-N)\) and all design holds. In particular \(D>1\) forces \(l=\max(\inf W,0)\). For free design the positive Poisson and binomial limits have \(l=\max(\inf W,0)\), and any positive rate or odds is reached by integer rounding along these regimes. For the Poisson use \(m,k\) of order \(\sqrt N\); for the binomial fix one of \(m,k\) and use an asymptotic fraction in \((0,1)\) for the other. Degenerate upper atoms need only be considered for constant samples, where free design can attain any point mass using \(m=k=N=x\). These facts and the finite-versus-excluded test give the iff. Comparing all exact rational design values with the envelope leaves an integer-tail problem for general data.

On two cells every positive rational target \(a/b\) is realized by \(P=1,Q=(l+1)a,D=b\), giving a valid integer design with both cells. With \(k\) held compatible, use \(Q=k-l, P=(l+1)a,D=Qb\); with \(m\) held interchange the labels. This settles all mixed samples on two cells for these three masks. Constants under a single hold were already shown to attain, and with both \(k,m\) held use the integer rounding rule below. For \(W=[1,2]\) and sample \((1,2)\), \((N,k,m)=(4,3,2),(4,2,3)\) give \(R_1=1\) and attain. The same witness works with \(N=4\) held.

### hyper N truncate:known

**Proved.** Enumerate \(0\le m,k\le N\) exactly. Every compatible sample attains, and retain all tied values, including the swap above.

### hyper N+k truncate:known

**Proved.** Enumerate \(m=0,\ldots,N\). Every compatible sample attains; compare rational heights and retain every tie.

### hyper N+m truncate:known

**Proved.** Enumerate \(k=0,\ldots,N\), with the same argument as when draws are held.

### hyper k truncate:known

**Proved.** Apply the growing-population proof with \(k\) fixed. At \(0<m/N\to p<1\) the limit is truncated binomial of size \(k\); at \(p=0\) the limit is the lower atom, and at \(p=1\) the upper atom of the effective support. This follows directly from the ratio expression, including the cases where one numerator is bounded and \(D\) is bounded. Enumerate compatible designs and compare with those heights. A constant \(x\le k\) can always be realized as an atom by \(N=k,m=x\).

### hyper k+m truncate:known

**Proved.** The eventual support as \(N\to\infty\) is \([l_0,\min(k,m)]\cap W\), with \(l_0=\max(0,\inf W)\). The only limit is \(\delta_{l_0}\), by the ratio formula. A compatible constant at any \(x\ne l_0\), or a nonconstant sample, attains when compatible because its escape height is zero. A constant \(l_0\) attains iff some allowed population yields a singleton effective support there, equivalently iff the lower support can reach \(l_0\) while the upper effective end equals \(l_0\). In fact the upper end is independent of \(N\), so it suffices that it equals \(l_0\). Otherwise the atom is excluded. On two cells, the odds are a positive constant divided by \(D\), strictly decreasing in \(N\) on the compatible mixed-sample set. The two neighbors of the target (or the single endpoint) are found by integer rounding and compared by the two-cell rule.

### hyper m truncate:known

**Proved.** Interchange \(k,m\) in the fixed-draws proof.

### nhyper all free truncate:known

**Proved.** The \(r=0\) branch is the atom at zero. For \(r\ge1\) use weights
\[
 {x-1\choose r-1}{N-x\choose m-r},\qquad
 R_x=\frac{x}{x-r+1}\frac{N-m+r-x}{N-x},\qquad r\le x<N-m+r.
\]
On a compatible branch \(r\le \min x_i\), so for \(N\to\infty\) take \(r,l\) fixed on a subsequence. Set \(h=(N-m+r-l)/(N-l)\) and extract its limit \(h_0\). All fixed ratios tend to \(h_0 x/(x-r+1)\) when the successive cells exist. If \(h_0=0\), the limit is the lower atom, including when the upper support stays finite. The key bound is \((N-m+r-x)/(N-x)\le h\) for \(x\ge l\). It gives summable domination when \(h_0<1\) and yields truncated shifted NB with size \(r\) and odds factor \(h_0\) for \(h_0>0\). If \(h_0=1\), the upper end before truncation diverges and the relative weights tend to \({x-1\choose r-1}\) up to a constant. These polynomial weights normalize on finite windows and diverge on a ray, giving zero sample height there. Every \(h_0\in[0,1]\) is approached by rounding \(N(1-h_0)\) for \(m\) with \(m\ge r\), or using the exact limiting proportions at the ends. Thus compare every finite design against these envelopes. On two cells with \(1\le r\le l\) fixed, the possible mixed-sample ratios are exactly the positive rationals up to \(l/(l-r+1)\), including that upper bound: take \(N-l\) the denominator of the required \(h\) and \(m-r=(1-h)(N-l)\). For free \(r\) the largest ratio bound is \(l\), given by \(r=l\). Thus if the target exceeds the bound the bound itself wins, otherwise the target is attained. For a constant at the lower cell take \(N=m,\ r=l\) when free; at fixed \(r\le l\) take \(N-m=l-r\) to make \(l\) the true upper end. A constant at the upper cell at fixed \(r\le l\) cannot attain the saturated height when the lower cell belongs to the window, since the support starts at \(r\). The upper odds bound then wins at \(m=r\), which attains even for that constant. If \(r=l+1\) the upper singleton saturates. For \(W=[1,2]\) and sample \((1,2)\), \(m=r=1\) gives equal weights at every \(N\ge2\); this is an exact full-likelihood tie also when \(m,r\) are held. Free design can saturate every compatible constant using \(N=m=r=x\) (with the \(r=0\) interpretation at zero).

### nhyper N truncate:known

**Proved.** Finite enumeration of \(0\le r\le m\le N\) gives attainment for every compatible sample and all tied designs.

### nhyper N+m truncate:known

**Proved.** Enumerate \(0\le r\le m\) and evaluate each compatible law.

### nhyper N+r truncate:known

**Proved.** Enumerate \(r\le m\le N\) and evaluate each compatible law.

### nhyper m truncate:known

**Proved.** Compare the finite design candidates with \(h_0=1\) polynomial weights for each compatible \(r\ge1\) and the attained atom branch if \(r=0\). On a ray all compatible positive samples attain, since all escaping heights vanish. At two cells enumerate the finitely many \(1\le r\le\min(l,m)\) and apply the integer-neighbor formula below in each slice, also checking singleton branches for constants. If \(m=1\) the mixed pair witness applies.

### nhyper m+r truncate:known

**Proved.** The same comparison at the fixed \(r\) gives attainment on rays whenever compatible, including the zero atom. On two cells and \(r>0\), the odds are proportional to \(1-(m-r)/(N-l)\). If \(m=r\) they are constant on the mixed-sample branch. Otherwise they increase strictly in \(N\) up to the excluded limit, so compare just the integer neighbors or endpoints of the ratio target, including compatible singleton branches for constants.

### nhyper r truncate:known

**Proved.** Use the free-population faces at that \(r\), with direct evaluation of the whole \(r=0\) branch. The mixed-pair tie also applies for \(r=1\).

## Location, scale and power families

### dlaplace all free truncate:known

**Proved.** Put \(q=\mathrm{scale}=e^{-r}\), \(r>0\), and use \(w_x=e^{-r|x-c|}\), \(c\) an integer. On a finite window clip \(c\) to \([l,u]\) with exterior centers equivalent to their clipped endpoint. On the right ray, all \(c\le l\) are equivalent to \(l\). For \(c\ge M\) on that ray, the numerator in these weights decreases strictly as \(c\) increases, while the sum \( [1+q-q^{c-l+1}]/(1-q)\) increases. Hence optimize only \(l,\ldots,M\), retaining the whole exterior plateau if \(l\) wins. Reflect for the left ray. On the whole line use the integer medians since the normalizer is independent of \(c\).

For each center, take \(D=|X-c|\) and \(\bar D=\sum_i|x_i-c|/n\). The derivative in \(r\) is \(n(E_rD-\bar D)\) and its derivative is \(-n\operatorname{Var}_rD\). For finite windows the value at \(r=0\) is the uniform mean distance \(\bar D_0\). On rays or the whole line this expectation tends to infinity as \(r\downarrow0\), since in the tail distances increase by one and the weights are geometric. As \(r\to\infty\) the expectation decreases to the minimum distance \(d_{\min}\), and the law goes to the sole nearest cell. Use the unique root if \(d_{\min}<\bar D<\bar D_0\) (with \(\bar D_0=\infty\) for unbounded windows), the excluded nearest atom at the lower endpoint, and the excluded uniform when \(\bar D\ge\bar D_0\) on a finite window with more than one cell. Normalize a singleton directly. These per-center comparisons plus the location reduction give the global iff and all ties. They also bound the escaped likelihood from simultaneous changes of center and rate because the projection bounds hold at each rate. For a ray and \(r\downarrow0\) every individual mass tends to zero uniformly in \(c\), by the geometric series on a whole side of the mode or on the exterior plateau. A moving center into an unbounded side with rate bounded away from zero gives zero sample height. Thus there is no extra positive-height joint escape beyond the displayed envelope.

On two cells the only ratios are \(q,1/q\). A mixed sample with unequal frequencies fits at \(q=\min(R_*,1/R_*)\) on the corresponding exterior plateau. A balanced pair escapes to the excluded uniform. A constant escapes to its nearest-cell atom. For an exact attained nonuniqueness witness use sample \((0,0,1)\) on \([0,1]\): \(q=1/2\), any \(c\le0\).

### dlaplace location truncate:known

**Proved.** Use the single distance-statistic rule with the actual held center, or its equivalent clipped center. A nonflat finite fit is unique by the strictly negative variance derivative.

### dlaplace location+scale truncate:known

**Proved.** Evaluate the held law whenever compatible.

### dlaplace scale truncate:known

**Proved.** Use the finite center comparison and plateau rule. A compatible sample always attains. For \(W=[0,1]\), sample \((0,1)\), both ratios \(q,1/q\) have the same height, so every integer location fits at any held scale. On the whole line retain every integer median.

### dnorm all free truncate:known

**Proved.** The cell indexed by \(x\) is \([x,x+1]\), whose endpoints have continuous mass zero. On an effective window use the integrated weights
\[
 w_x(\eta,\gamma)=\int_x^{x+1} e^{\eta y-\gamma y^2/2}\,dy,\qquad
 \eta=\mu/\sigma^2,\quad\gamma=1/\sigma^2.
\]
Integer cell observations therefore have likelihood given by a *product of integrals*. A continuous truncated-normal variance threshold does not give the gradient of this product. The two canonical scores are the differences of the average within-observed-cell means and second moments and their window-conditioned counterparts (multiply the second-moment difference by \(-1/2\)).

Here is the exhaustion argument. If \(\gamma\) stays bounded and \(|\eta|\to\infty\), ratios of fixed consecutive cell integrals diverge toward the favored side, by translating the integrand by one. A finite favored endpoint gives the associated atom (on a ray, bound successive ratios on the inward side geometrically by the slope at the endpoint). If the favored side is unbounded each fixed cell loses to a fixed neighbor. If \(\gamma\downarrow0\) and \(\eta\to\eta_0\) finite, compact-cell ratios converge to the geometric tilt \(e^{\eta_0 x}\). On a ray pointing against the slope, an exponential upper bound dominates all tails when the limiting slope is strict. Otherwise the limiting normalizer is infinite, including a whole-line window. If both coordinates have finite limits with \(\gamma>0\) the limit is admissible.

For \(\gamma\to\infty\), extract the limit of \(\mu\) on the extended line. If it lies weakly outside the continuous retained interval or tends to an infinite endpoint, the density concentrates in the closest edge cell if there is one, or leaves all fixed cells otherwise. To see this, compare an interval separated from the projection by a positive gap with a smaller interval closer to it. The squared-distance gap times \(\gamma\) tends to infinity, and the unbounded-side upper tail is bounded by integrating the tangent exponential outside any fixed positive distance from the projection. This also handles vanishing retained mass when the center lies outside. If \(\mu\) limits inside the retained interval, the same comparison excludes all cells except the one or two whose closures contain that limit. At a shared boundary \(j\), \((\mu-j)/\sigma\to z\in[-\infty,\infty]\) gives the probabilities \((1-\Phi(z),\Phi(z))\) on \(j-1,j\). Each split is reached. In particular constants and adjacent pairs attain the saturated *supremum*, excluded whenever three or more cells are retained. On two cells varying \(\mu\) at any fixed \(\sigma\) realizes every positive ratio: its log derivative in \(\eta\) is the difference between the neighboring within-cell means, strictly positive, and its limits are zero and infinity. Every mixed pair fits and has an entire scale-indexed fiber. For \(W=[0,1]\), sample \((0,1)\) and mean one, every \(\sigma>0\) fits.

For range at least two the only nonzero boundary candidates are geometric tilts. On the whole line the parent grouped-normal result supplies uniqueness and the sharper held-mean criterion. [Parent proof](2026-11-25-dnorm-math-report.md#existence-and-limiting-laws) On finite windows their optimal height is at the unique mean-matched tilt (uniform iff the sample mean equals the uniform mean); on either ray use its inward tilt and reflected result; on the whole line use zero. Compare this exact height against stationary values of the integrated objective. A positive inward derivative at the mean-matched face implies attainment. At \(\gamma=0\) the within-cell coordinate \(Y-X\) has the same density proportional to \(e^{\eta v}\), \(0\le v\le1\), in every cell. On expanding \(Y^2\), the offset terms cancel since the geometric fit matches \(\bar x\). Hence the inward derivative per observation is exactly \((\operatorname{Var}_{\rm geom}X-n^{-1}\sum_i (x_i-\bar x)^2)/2\). For a symmetric mean on a finite window the matched tilt is uniform, with variance \(((u-l+1)^2-1)/12\). On a right ray the matched variance is \((\bar x-l)(1+\bar x-l)\), and reflection gives the left-ray value. Lack of a positive sign leaves the possibility of a separated peak. In contrast with a point-observed normal, the conditional moments inside the cells enter both scores and the Hessian gains the average within-cell covariance; this argument establishes no joint concavity or global singleton rule.

### dnorm mean truncate:known

**Proved.** Restrict to \(\eta=\gamma\mu\). The broad limit is uniform on finite windows and gives zero height on rays and the whole line. The sharp limit is the atom at the projected cell or equal halves if the held mean is a shared internal cell boundary. Use these two heights and the scalar score on this line. On a ray or the whole line, if the sample occupies any cell outside the sharp face it necessarily attains by compactness. On the whole line a sample contained in the sharp face has strictly increasing likelihood as scale decreases, since every observed cell closure contains the mean and its standardized interval expands. Other whole-line held-mean samples have one finite maximizer by the parent strict-information proof. On two cells there is a sharper rule. If the held center is at the shared edge, the ratio is identically one. Otherwise the probability of the cell nearer the center increases strictly in \(\gamma\). Indeed, upon folding each interval about \(\mu\), the multiplicity ratio of nearer to farther cell is decreasing in distance. If \(\mu\) lies inside the nearer cell, the farther cell starts at the distance to the shared edge. On the overlap the nearer multiplicity drops from at most two to at most one then zero, and the farther multiplicity is one. If \(\mu\) lies outside both, the folded intervals are ordered and disjoint. Tilting the distance measure by \(e^{-\gamma d^2/2}\) therefore strictly favors the nearer cell, by the covariance of oppositely ordered functions, or by pairwise integration across the two cells. Its mass ranges from \(1/2\) to one, with both ends excluded. This settles every short-window fit and includes the exact flat example above.

### dnorm mean+sd truncate:known

**Proved.** Evaluate the held law whenever compatible.

### dnorm sd truncate:known

**Proved.** Restrict to fixed \(\gamma>0\). The only positive escaped heights are atoms at finite cuts. For more than one cell, all observations at a finite end escape; every other sample attains (compatible samples on the whole line always attain). On two cells the monotone odds proof gives the unique mixed fit. The whole-line parent proof gives a unique finite location; on larger proper windows use all roots of the first score to decide cardinality.

### dunif all free truncate:known

**Proved.** Write the parent endpoints as \(a,b\) and the cuts as \(L,U\). Every compatible effective interval has endpoints \(l=\max(L,a)\le m\), \(u=\min(U,b)\ge M\) and likelihood \((u-l+1)^{-n}\). Maximize \(l\) and minimize \(u\) independently: the free effective ends become \(m,M\). Retain all parent endpoints mapping to these effective ends, including all integers outside a clipping cut. Example \(W=[1,2]\), sample \((1,2)\): \(a=0,b=3\) and \(a=1,b=2\) both attain \(1/4\). A widening retained interval tends to zero height; moving a parent end behind a finite cut produces the same attained effective interval.

### dunif max truncate:known

**Proved.** Hold the effective upper end \(\min(U,b)\); the optimal effective lower end is \(m\), and every parent lower end mapping to \(m\) ties. If the fixed effective end excludes data there is no compatible fit.

### dunif max+min truncate:known

**Proved.** Evaluate the held interval whenever compatible.

### dunif min truncate:known

**Proved.** Hold the effective lower end \(\max(L,a)\); the optimal effective upper end is \(M\) with all its parent preimages. This also settles singleton and two-cell cases under every mask.

### dweibull all free truncate:known

**Proved.** Write \(t=-\log(\mathrm{shape1})\), \(\beta=\mathrm{shape2}\), and \(Y_k=k^\beta\), \(0^\beta=0\). Let \(A_k=Y_k-Y_l\), \(D_k=Y_{k+1}-Y_k\), and \(V=Y_{u+1}-Y_l\) (\(V=\infty\) on a ray). Then
\[
 g_k=\frac{e^{-t A_k}(1-e^{-tD_k})}{1-e^{-tV}} .
\]
For finite positive \(\beta\), \(t\to0\) gives weights \(D_k\) on finite windows, and zero sample height on rays. At \(t\to\infty\) the limit is \(\delta_l\). These statements are uniform with \(\beta\) in a compact positive interval.

For \(\beta\downarrow0,l>0\) use \(c=\lim t\beta\). For \(c>0\) finite the weights are \(k^{-c}-(k+1)^{-c}\), normalized on the window, by expansion of \((k^\beta-l^\beta)/\beta\). On a finite window \(c=0\) instead gives the increment weights \(\log((k+1)/k)\), and \(c=\infty\) gives the lower atom. On a ray \(c=0\) gives zero sample height, while other limits are proper by telescoping. For \(l=0\) on a finite window and \(\beta\downarrow0\), all positive masses vanish. Their sum relative to the zero cell is at most \(t[(u+1)^\beta-1]/(e^t-1)\to0\). On a ray with \(l=0\), every fixed positive cell vanishes by the same bound, with the zero mass equal to \(1-e^{-t}\). Only all-zero data get positive height from this defective limit, bounded by the saturated lower atom.

For \(\beta\to\infty\), neighboring increments \(D_{k+1}/D_k\to\infty\). Extract limits of \(tD_k\) on a subsequence. At most one such limit is finite positive, with all earlier increments tending to zero and all later ones diverging. The adjacent mass ratio is \(e^{-tD_k}(1-e^{-tD_{k+1}})/(1-e^{-tD_k})\). It tends to infinity when \(tD_k\to0\), and to zero when \(tD_k\to\infty\); a finite positive \(tD_k\) gives the intermediate split. This proves singleton or adjacent-pair limits on a finite window, also when the parent threshold lies above it. On a ray a finite threshold gives the same result directly by telescoping survival; if the threshold drifts, fixed masses vanish. For an explicit arbitrary split at \(k,k+1\), take \(tD_k\to-\log p\) for upper fraction \(p\in(0,1)\). Earlier relative exponents vanish and later ones diverge. All singletons are reached as well by placing the threshold strictly inside the cell.

With at least three cells, constant and adjacent data saturate at these excluded faces. On two cells the both-free ratio image is the entire positive half-line, by the attainable bracketing of the limiting atoms and continuity on the connected parameter domain. For an exact nonunique sample take \((1,2)\) on \([1,2]\). For every sufficiently large finite \(\beta\), the \(t=0\) ratio \(D_2/D_1\) exceeds one and the \(t=\infty\) ratio is zero. Each gives a distinct finite \(t\)-root for ratio one. For range at least two, compare the finite candidates against the power-increment family at \(t=0\), the discretised Pareto family and its terminal log-increment law where applicable, and zero. On the full parent window \(l=0,u=\infty\) this recovers the positive sample case of the [parent theorem](2026-11-25-dweibull-math-report.md#attainment-and-maximizer-sets). In other windows the comparison may require isolating separate profile maxima.

### dweibull shape1 truncate:known

**Proved.** At fixed \(t>0\) the two endpoints in \(\beta\) are those above: finite window lower zero atom for \(l=0\), log-increment weights for \(l>0\); on a ray the small-\(\beta\) fixed positive masses vanish. At large \(\beta\), the limit is \(\delta_l\) for \(l\ge1\); at \(l=0\) it is the Bernoulli with masses \(1-e^{-t},e^{-t}\), clipped if needed. Compare against all scalar critical values. On the full window the parent proof supplies uniqueness for samples with positives, and a flat fit otherwise. On \([0,1]\) the odds reduce to \((e^{-t}-e^{-t2^\beta})/(1-e^{-t})\), strictly increasing with range \((0,1/(e^t-1))\). For two cells with \(l\ge1\) use the extrema of the displayed adjacent mass ratio and its stated endpoints. There are finitely many extrema and finitely many preimages of any positive target. At zero the numerator and denominator have simple zeros before cancellation, so the ratio extends analytically across zero. At infinity the ratio tends to zero and its log derivative is eventually negative:
\[
 -tD_l'\left(1+\frac{1}{e^{tD_l}-1}\right)+\frac{tD_{l+1}'}{e^{tD_{l+1}}-1}<0 .
\]
Here \(D_l'\sim (l+1)^\beta\log(l+1)\) diverges, and the positive term tends to zero by exponential damping in \(D_{l+1}\). Nonconstant analyticity on the remaining compact interval excludes accumulation of critical points or target roots. This gives a finite comparison, including tied preimages.

### dweibull shape1+shape2 truncate:known

**Proved.** Evaluate the held law whenever compatible.

### dweibull shape2 truncate:known

**Proved.** For a nonsingleton window the derivative of \(\log g_k\) in \(t\) is \(-A_k+D_k/(e^{tD_k}-1)-V/(e^{tV}-1)\), dropping the last term on a ray. Its derivative is strictly negative because \(s^2 e^{ts}/(e^{ts}-1)^2=[s/(2\sinh(ts/2))]^2\) strictly decreases in \(s>0\) and \(D_k<V\). The score per observation tends to \(-A_k+(V-D_k)/2\) at \(t=0\) on finite windows, and to positive infinity on a ray. The upper-rate limit is \(-A_k\). A constant at \(l\) escapes to \(\delta_l\); otherwise attain exactly when the total lower-end score is strictly positive, and then the root is unique. Equality selects the excluded \(D_k\) weights. This also gives the complete two-cell decision at held \(\beta\).

### lgser all free truncate:known

**Proved.** Use \(w_k=\theta^k/k\), \(l\ge1\), \(0<\theta<1\). The derivative of the mean in \(\log\theta\) is its strictly positive variance on nonsingleton windows. At zero the mean tends to \(l\) and the law to \(\delta_l\). At one it tends on a finite window to the harmonic-weight mean
\[
 E_1 X=\frac{u-l+1}{\sum_{k=l}^u 1/k}.
\]
On a ray the harmonic weights have divergent sum so each fixed cell loses its mass and the mean tends to infinity. Thus on finite windows attainment is exactly \(l<\bar x<E_1X\); on rays it is \(\bar x>l\). The unique fit matches the mean. At or above the finite upper threshold the excluded harmonic law gives the likelihood by \(B(1/k)\). This applies on two cells as well; the odds span \((0,l/(l+1))\).

### lgser theta truncate:known

**Proved.** Evaluate the held law whenever compatible.

### yulesimon all free truncate:known

**Proved.** Survival at \(k\) is \(S_k=\prod_{i=1}^{k-1}i/(i+\rho)\); the cell probability is \(S_k\rho/(k+\rho)\). This telescopes since \(S_{k+1}/S_k=k/(k+\rho)\) and \(S_k\to0\) at positive \(\rho\) by the divergent harmonic sum. On a ray conditioned on \(X\ge l\) the log likelihood derivative in \(\log\rho\) is
\[
 n-\sum_i\sum_{j=l}^{x_i}\frac{\rho}{j+\rho}.
\]
It strictly decreases from \(n\) to \(-\sum_i(x_i-l)\), giving a unique root iff any observation exceeds \(l\), and otherwise the excluded \(\delta_l\). At \(\rho\downarrow0\) the likelihood vanishes on the ray. On finite windows use the rational weights obtained by the same products, divided by \(1-\prod_{j=l}^u j/(j+\rho)\). At zero the numerator is asymptotic to \(\rho/k\) and the denominator to \(\rho\sum_{j=l}^u 1/j\); the boundary law is harmonic. At infinity the lower atom results from the adjacent ratios \(k/(k+1+\rho)\). Use the rational critical comparison against those two heights, which is an exact finite polynomial decision including all ties. The objective is nonconstant if there are at least two retained cells: for all-\(l\) data it tends to one at infinity but is less than one at zero, and with any observation above \(l\) it tends to zero at infinity but is positive at zero. Thus the finite candidate set on nonflat windows is finite. On two cells the interval of odds is exactly \((0,l/(l+1))\), giving explicit unique mixed fits inside the interval and the two excluded endpoints otherwise.

### yulesimon rho truncate:known

**Proved.** Evaluate the held law whenever compatible.

## Mixed Poisson and difference families

### pig all free truncate:known

**Proved.** Put \(\lambda=\mathrm{shape}\), \(\mu=\mathrm{mean}\), \(z=\sqrt{2\lambda+(\lambda/\mu)^2}\), \(d=z-\lambda/\mu\), and \(v=\lambda/z=d(1-d/(2z))\), with \(0<d<z\). Drop the common factor \(e^{-d}\) from the parent probabilities to obtain
\[
 w_0=1,\qquad w_k=v^k Q_{k-1}(1/z)/k!,\qquad
 Q_n(y)=\sum_{j=0}^n \frac{(n+j)!}{j!(n-j)!2^j}y^j .
\]
Shape zero is an included atom, compatible only for all-zero data with zero retained. The finite Lévy endpoint is \(d=z>0\), equivalently infinite mean, and uses the same weights. The table exhausts the extended \((z,d/z,d)\) limits.

| Path | Limit after conditioning |
| --- | --- |
| \(z\downarrow0,\ l\ge1,\ d/z\to\rho_0>0\) | Tilted Sibuya with weights \(s^k(-1)^{k+1}{1/2\choose k}\), \(s=\rho_0(2-\rho_0)\), including \(s=1\) |
| \(z\downarrow0,\ l=0\) | Zero atom |
| \(z\downarrow0,\ l\ge1,\ d/z\to0\), or \(d\downarrow0\) with \(z\) bounded away from zero | Lower atom |
| \(z\to\infty,\ d\to d_0>0\) finite | Poisson of rate \(d_0\) restricted to the window |
| \(d\to\infty\) | Finite upper endpoint atom or, on a ray, every fixed mass vanishes |
| \(z\to z_0\in(0,\infty),\ d/z\to1\) | Restricted Poisson–Lévy |
| Finite positive \(z,d\) with \(d<z\) | Admissible |

For positive lower cut and \(z\downarrow0\), select the highest powers of \(1/z\) in \(Q_n\) to derive the Sibuya weights, or use the positive-law pgf in the [zero-truncation report](2026-11-27-zero-mod-math-report.md#pig-all-free-truncated). When \(z\) stays away from zero and \(d\to0\), or when \(z\to0\) and \(d/z\to0\), the positive weights concentrate at one before further conditioning; at a higher lower cut use ratios, which tend to zero. On rays in these rare-event cases the further-conditioned atom follows from uniform ratio bounds: \(Q_n(1/z)\) are moments of the positive mixing rate after size bias and exponential tilting, or directly satisfy \(Q_{n+1}(y)=(2n+1)yQ_n(y)+Q_{n-1}(y)\) for \(n\ge1\), with \(Q_0=1,Q_1=1+y\). The recurrence follows by coefficient comparison and gives \(Q_{n+1}/Q_n\le 1+(2n+1)y\) since \(Q_n\ge Q_{n-1}\) coefficientwise. Thus the successive weight ratios are at most \(v[1+(2k-1)/z]/(k+1)\) for \(k\ge1\), sufficient when \(v\to0\) and \(v/z\to0\), and for a uniform geometric tail when \(z\to\infty\) with bounded \(d\). At the Sibuya and finite Lévy limits proper conditional laws follow by the cited pgf convergence on the positive ray, then conditioning on an event with positive limiting mass. When zero is retained, ordinary parent pgf convergence applies at finite Lévy and Poisson limits. At \(z\downarrow0\) with zero retained the parent zero mass tends to one. At \(d\to\infty\), \(v\ge d/2\), \(z\to\infty\), and every fixed successive ratio diverges, giving the stated endpoint or drift.

On any finite window use the rational chart with \(y=1/z\), \(v>0,\ 0<2vy<1\). The face and score comparisons for this chart, and the two rational held charts below, are semialgebraic for algebraic holds. The boundary weights can be read from the table or found from the closure of the normalized weight image. This gives a finite-window attainment and cardinality decision.

For fixed \(z\), the weights are a scalar exponential family in \(\log v\), for \(0<v<z/2\). With more than one cell, comparison of the empirical mean with the lower end and the mean at \(v=z/2\) either gives its unique inner root or its best endpoint. Then compare the finite profile with the boundary laws using \(B\). On two cells the all-free odds range over \((0,\infty)\): at \(z\) fixed, \(R_l=v\,b_l(z)\) for a positive coefficient, and the upper limit diverges as \(z\to\infty\) since \(b_l\to1/(l+1)\). At \(W=[1,2]\), sample \((1,2)\), each \(z>3\) realizes ratio one with \(v=2/(1+1/z)<z/2\). Thus an entire profile fiber attains.

### pig mean truncate:known

**Proved.** For fixed \(\mu>0\), \(0<d<\mu\) and \(z=d(\mu-d/2)/(\mu-d)\), by solving \(\lambda=\mu(z-d)=d(z-d/2)\). At zero shape, positive lower cuts give the untilted Sibuya face; when zero is retained the zero atom is included. At infinity the face is Poisson at the held mean. Compare against the scalar critical values. On two cells substitute this expression for \(z\); the ratio is rational in \(d\). Its exact range and preimages use the rational chart rule, expressly checking the included zero atom when relevant.

### pig mean+shape truncate:known

**Proved.** Evaluate the held law whenever compatible.

### pig shape truncate:known

**Proved.** With \(\lambda>0\) held, the lower-mean limit is the lower atom and the upper-mean limit is restricted Poisson–Lévy. With \(\lambda=0\) all compatible means give the zero atom. For positive held shape and two cells the ratio is rational in \(z\in(\sqrt{2\lambda},\infty)\), since \(v=\lambda/z\). The rational chart gives its endpoints, extrema and finite preimages. On every ray the Lévy face is strictly improvable. Perturb \(t=z-d=\lambda/\mu\) inward at fixed positive shape, so \(z'(0)=v'(0)=0\) and \(d'(0)=-1\). Each observed weight has zero first derivative. The ray denominator is \(e^d-\sum_{k<l}w_k\), whose derivative is \(-e^z\), giving a strictly positive likelihood derivative. The same proof holds with both coefficients free along this path. Thus at held positive shape all-\(l\) samples escape to the lower atom and every other sample on a ray attains. On a finite window the denominator is a finite sum of stationary weights, so its first derivative vanishes too. Compare the exact finite-window face heights with finite candidates.

### skellam all free truncate:known

**Proved.** At positive rates use \(w_k=e^{\eta k}I_{|k|}(z)\), \(\eta=\frac12\log(\mu_1/\mu_2)\), \(z=2\sqrt{\mu_1\mu_2}\). Include either zero-rate axis when compatible (Poisson, reflected Poisson, or the zero atom). The positive-product escapes are exhausted as follows.

For \(z\to z_0\in(0,\infty)\), a finite \(\eta\) stays in the model, and \(\eta\to\pm\infty\) gives an atom at the favored finite cut or drift if that end is infinite. For \(z\to\infty\) the fixed-order ratios \(I_{j+1}/I_j\to1\). This and \(0<I_{j+1}/I_j<1\) follow from the integral with kernel \(e^{zt}(1-t^2)^{j-1/2}\) on \([-1,1]\): integration by parts makes the ratio the mean of \(t\), and that measure concentrates at one as \(z\to\infty\), by comparison to an interval closer to one. Hence finite limiting \(\eta\) gives a geometric tilt, proper on finite windows and on rays pointing strictly against the slope. The uniform bound on \(I_{j+1}/I_j\) gives geometric domination in the latter case, after reflection if needed. For non-summable tilts every fixed cell vanishes. Infinite \(\eta\) gives the favored-end atom or drift by the same ratio result. The geometric domination toward an unbounded unfavored end also applies for bounded positive product.

For \(z\to0\) use the power series \(I_j(z)=(z/2)^j/j!\,(1+O(z^2))\) for each fixed \(j\). For \(k\ge0\) the weights tend in relative form to \(\mu_1^k/k!\); for \(k\le0\) they take the reflected form \(\mu_2^{-k}/(-k)!\), with the same reference at zero. If the rates have finite limits, these give the admissible axes when compatible. If a positive lower cut excludes the limiting axis and \(\mu_1\to0\), the rare-event conditional limit is \(\delta_l\), with the reflected version for a negative upper cut. When both rates tend to zero, use the atom nearest zero. If either rate diverges, the other vanishes, so consecutive weights favor the diverging-rate direction by divergent ratios at every fixed count, giving the favored atom or drift. These arguments normalize on rays by the series bound \(I_{j+1}(z)/I_j(z)\le z/[2(j+1)]\), obtained term by term. For the rare-event paths this also handles conditioning on a strictly positive or strictly negative ray. On the whole line any drift gives zero sample height. No geometric law there has finite normalizer.

Compare exact axis likelihoods and all finite candidates with these escaped heights. The best geometric tilt uses its mean equation on finite windows and the inward geometric fit on rays, including the extended endpoint atom. Mixed samples on two cells attain the saturated height at every \(z>0\) with \(\eta=\log R_*+\log[I_{|l|}(z)/I_{|l+1|}(z)]\). In particular \((0,1)\) on \([0,1]\) proves nonuniqueness. Atoms on two-cell windows attain exactly when an axis has effective singleton support at that cell, which can be checked by intersection with its positive, negative, or zero support. Otherwise the atom approached by the odds direction is excluded. For at least three cells the geometric comparison does not assert that the joint score has only one maximizer.

### skellam mu1 truncate:known

**Proved.** With positive \(\mu_1\) held, sending \(\mu_2\to\infty\) gives the lower endpoint atom on windows with a finite lower end, or zero height otherwise. At \(\mu_2\downarrow0\) use the nonnegative Poisson axis if compatible; on windows strictly negative use the nearest-to-zero atom. If the axis intersects the window but excludes an occupied cell, its likelihood is zero. Compare with the finite scalar critical values. With \(\mu_1=0\) use the reflected truncated-Poisson rule (including the possible origin atom), which is a scalar exponential family with carrier \(1/(-k)!\). Reflection gives the other mask.

On two cells with positive held rate, the odds strictly decrease as \(\mu_2\) increases. To see this, each two-sided mass sequence at positive rates is strictly log-concave. The sequence of each Poisson summand (one reflected) has nonnegative order-two Toeplitz minors by the decreasing successive ratios and interval support. The minors of the convolution are sums of products of minors, by the two-by-two determinant expansion, and for neighboring cells at least one product is positive. In detail, for rows \(k,k+1\) and columns \(0,1\), choose intermediate columns \(h,h+1\) with \(h\le \min(-1,k-1)\). Then the two adjacent minors have strictly positive entries on the interior of the respective Poisson supports and are strictly positive by the Poisson ratios. Absolute convergence of the convolution justifies the expansion by finite intermediate sums. Differentiating in \(\mu_2\) shifts the probability from \(k+1\) into \(k\), so the derivative of \(\log R_l\) is \(p_{l+2}/p_{l+1}-p_{l+1}/p_l<0\). Its interval ends follow from the two held-rate limits. The \(\mu_2=0\) ratio endpoint is included when the Poisson axis supplies the required law; otherwise the corresponding atom is excluded. A held zero-rate branch uses its own support before applying the two-cell rule.

### skellam mu1+mu2 truncate:known

**Proved.** Evaluate the held law whenever compatible.

### skellam mu2 truncate:known

**Proved.** Reflect counts and exchange the rates in the held-\(\mu_1\) analysis, including the monotone two-cell ratio. The only possible excluded positive-height escape on a ray with fixed positive other rate is the favored finite-bound atom or the atom conditional on a disjoint axis; apply the height comparison to those cases.

## Summary

Here “finite comparison” includes its exact inclusion bits and ties; “stationary comparison” is an iff against an explicit escape envelope and can still need root isolation.

| Family | Attainment rule | Principal excluded competitors | Cardinality evidence |
| --- | --- | --- | --- |
| bbinom | Finite rational charts, then compare growing-size envelope | Zero-shape recurrences, endpoint mixture, binomial, NB/power, logarithmic, Poisson and atoms | Finite-design algebraic decision, exact two-cell tie, integer-tail singleton open |
| bnbinom | Finite rational decision or ray stationary comparison | Rare-shape recurrence, NB, logarithmic, Poisson and lower atom, plus finite-window endpoint and power recurrences | Exact swap witness; general ray canonical cardinality open |
| cmb | Per-size hull or scalar mean, then bounded integer comparison | Exposed endpoint or pair, and held-statistic extreme set | Exact per-design fit set and attained cross-design ties |
| cmp | Mean/dispersion moments and inward geometric score | Point and adjacent laws, plus excluded geometric tilts when rate is at least one on a finite window | Strict covariance in identified charts, flat atomic or short-window fibers |
| dlaplace | Integer center reduction and scalar distance moment | Nearest atom or finite uniform | Full winning centers and rate roots |
| dnorm | Collapse check, then stationary comparison to a geometric tilt or held faces | Point, adjacent or fixed-half laws, geometric and uniform | Exact short-window fibers and parent uniqueness; proper-window general cardinality open |
| dunif | Shortest effective interval | Only compatible attained clipping plateaus can tie the finite optimum | All parent endpoint preimages |
| dweibull | Fixed-shape signed score or face/stationary comparison | Power/log increments, discretised Pareto, point and adjacent laws | Strict rate curvature, exact two-cell tie; other proper-window profile cardinality open |
| hyper | Bounded enumeration or finite designs against integer escape envelope | Binomial, Poisson and retained endpoint atoms | Exact design ties; general unbounded-tail equality open |
| lgser | Conditional mean vs harmonic threshold | Lower atom, finite harmonic law | Strict variance |
| nhyper | Bounded enumeration or shifted-NB/finite polynomial envelope | Shifted NB, polynomial weights and lower atom | Exact tied populations; free-population tail equality open |
| pig | Finite rational decision or ray profile and held comparisons | Truncated Poisson, tilted Sibuya, Lévy and atoms | Exact two-cell fibers and finite rational decision; larger-ray profile cardinality open |
| skellam | Compare stationary candidates and included axes against escape envelope | Geometric tilts and endpoint atoms, including rare-axis limits | Exact monotone held two-cell rule, joint ties; larger-window cardinality open |
| yulesimon | Ray score or finite rational critical comparison | Finite harmonic law and lower atom | Unique ray root, exact finite algebraic decision |

## What this changes for the fitter

None of these truncation routes is implemented here. A certified route needs to distinguish a support conflict from a zero-mass retained event, preserve the inclusion bit for each chart, and compare normalized sample likelihoods at the same observation window. Compute normalizers from retained weights or interval masses with tail-relative error control; nearly equal parent CDFs can lose the entire retained probability. [Short-window evaluation](2026-11-22-short-window-report.md) · [Continuous normal limit payloads](2026-11-21-tnorm-limit-report.md) · [Exponential-family truncation](2026-11-27-trunc-expfam-math-report.md)

Use integer support checks and the singleton/two-cell dispatch before search. Monotone moment roots allow coefficient brackets. Face comparisons need exact inequalities or enclosures with practical floating-point margins, including a separate equality rule when a solution ties an excluded law. Integer enumeration requires an explicit tail certificate unless its design range is bounded. A successful search on ordinary data with an unclosed comparison should stay flagged uncertified; a stop requires proved failure or failure of the search itself. Report uniqueness only where identified curvature, exact enumeration or a complete set calculation supplies it.

## Open

- **bbinom.** For free size and samples beyond the short-window and saturated cases, decide equality between the best finite chart and the growing-size envelope. Need signed uniform large-\(N\) expansions or a sharper conditional order comparison with the NB/binomial/Poisson/power limits, including the held-shape subfamilies. Known-size rational isolation establishes a decision for algebraic inputs but does not give a uniform singleton theorem.
- **bnbinom.** For rays, bound the conditional scores on compact regions and isolate every candidate that can tie the maximum; the normalization bounds give the competing heights, and the label-swap proof only distinguishes permutations. Off-diagonal canonical ties remain open.
- **dnorm.** For larger proper windows, root isolation or a new curvature inequality for the difference of integrated normal log normalizers is needed. In particular the broad-face inward score by itself gives only a sufficient strict-improvement check. A sample-variance iff and larger-window singleton assertion remain open. The sign proof for held-mean two cells uses ordered folds and does not extend directly to a numerator that sums more than two cell log masses.
- **dweibull.** Beyond fixed shape and the parent fit, need a monotone shape profile or global score enclosures to exclude separated maxima and decide exact equality with a power or discretised-Pareto face. The finite-window subtraction term in the likelihood invalidates an argument based solely on concavity of the parent cell masses. For two cells with held probability and positive lower cut the finite-window scalar odds comparison has finitely many preimages, with exact root enclosures and a simpler count as the remaining tasks.
- **hyper and nhyper.** On general unbounded-design masks, locate which finite designs can beat or tie the named growing-population limits, or prove an eventual one-sided inequality. The formulas reduce the tail to rational products but do not decide its sign near equality. This includes constant and two-cell integer comparisons wherever the given set formulas can contain several parameter tuples.
- **pig.** On rays, a general proof that every relevant scalar or joint profile root has been found is missing. There the first derivative excludes the finite Lévy face when shape can be kept fixed, but finite windows cancel that derivative and need a higher-order comparison or a direct ordering. Compare held-mean and both-free Poisson and Sibuya heights against certified finite scores. Cardinality on larger rays remains open.
- **skellam.** Need an exclusion bound away from the geometric and axis charts and a global scalar or joint root count for larger windows. Strict log-concavity of the mass sequence proves the held two-cell monotonicity and supplies no such root count for the normalized product over arbitrary samples.
- **yulesimon.** Finite windows have an exact polynomial decision, including ties. A simpler explicit inequality on the sample and a uniform singleton theorem for those windows would require further control of the derivative after subtracting the finite retained-mass score.

## Checks and atlas proposals

**Numerical.** The workstation replay used mpmath at 80 decimal digits. It compared normalized finite-window masses on cells 2 through 7, and −2 through 3 for Skellam. The grid was \(100,10^3,10^4,10^5\) for the growing sizes, growing shapes and growing Bessel argument, and \(10^{-2},10^{-4},10^{-8},10^{-16}\) for the shrinking scale coordinate, shape or Bessel argument. The 36 records report maximum absolute probability discrepancies. The same script checks the stated algebraic odds and ties with exact rational or symbolic arithmetic. The numerical grid checks derivations and gives no proof of uniqueness, exhaustiveness or sign on untested inputs. [Replay script](2026-11-27-trunc-discrete-math-data/check.py) · [Numerical records](2026-11-27-trunc-discrete-math-data/checks.json)

The workstation single-slot replay and the repository’s alternate-input atlas validator passed. The proposal contains 211 rows covering 68 atlas masks. The criterion rows distinguish a closed-form sample dispatch from an exact comparison against a named escape height. The latter can carry an open root or integer-tail obligation. Cardinality rows assert a complete set only where the proof supplies one, and scoped witnesses record nonuniqueness separately. All rows point to the proof for their mask. [Proposed CSV](2026-11-27-trunc-discrete-math-data/proofs.csv) · [Row generator](2026-11-27-trunc-discrete-math-data/write_proofs.py)
