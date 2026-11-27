# Zero truncation, modification and inflation for seven count families

27 November 2026 · TASK-440 · base 4a4ba7de

| Family | Truncated attainment test | Main open comparison |
|---|---|---|
| bbinom | Finite-design rational scores and explicit growing-design faces | Uniform design cutoff and cross-design ties |
| bnbinom | Canonical critical values and explicit recurrence faces | Canonical root coverage and equality |
| cmb | Conditional hull and finite design comparison | Fast cross-design bounds and held-inflation roots |
| cmp | Strict moment fit or point/adjacent limit | Held nonzero inflation |
| dgamma | Conditional gamma scores and shape-zero face | Free-shape face comparison and uniqueness |
| dweibull | Rate profile and discretised Pareto face | Joint profile comparison and uniqueness |
| pig | Bessel profile and explicit Sibuya-type and Poisson faces | Global profile shape and equality |

This analysis uses the [modifier design](../../design/modifiers.md), [roadmap](../../roadmap/2026-11-11-zero-and-truncation-modifiers.md), [likelihood envelope](../ml-behaviour.tex) and [parent atlas](../existence-atlas/atlas.csv). The [ZIB](2026-11-02-zib-zero-report.md), [ZIP](2026-11-07-zip-cert-report.md), [ZINB](2026-11-07-zinb-theory-report.md), [ZT probability](2026-11-11-ztpois-pmf-report.md), [modifier](2026-11-12-modifiers-report.md), [retirement](2026-11-12-modifiers-2-report.md), [zero uniqueness](2026-11-22-zero-unique-report.md) and [negative hypergeometric](2026-11-22-nhyper-zero-report.md) reports supply precedents for the boundary step, stable evaluation, zero-only unidentified coefficients and ties.

## Likelihood and comparison rules

Let \(m>0\) be the number of positive observations, \(z=n_0/n\), \(u=f_\theta(0)\), \(g_k=f_\theta(k)/(1-u)\), \(T=\sum_{x_i>0}\log g_{x_i}\) and \(B(v)=n_0\log v+m\log(1-v)\). Use the continuous Bernoulli convention at endpoints. Truncation requires a nonempty positive sample. A held parent zero atom is incompatible with conditioning on positives. Throughout, positive likelihood also requires each observed count to be in support.

### Critical values and boundary ties

**Proved.** Keep the all-held evaluations and critical points in free continuous coordinates at each admissible design. Include constrained stationary points on included parameter faces, and entire flat maximizing sets where appropriate. One can include all stationary points of a restricted face even if some fail an inward score condition, since they are feasible witnesses. Define \(F\) as the largest escaped likelihood, with zero as the floor. Escapes leave every compact subset of the parameter domain. Path limits of the finitely many observed probabilities lie in a compact cube, and the set obtained by escapes is closed by diagonal approximation. Dominated escapes can be pruned if their dominator is either another reachable limit or a feasible point.

Given positive feasible likelihood, an MLE exists iff at least one critical candidate reaches \(F\). A point strictly above \(F\) makes every maximizing sequence eventually stay within some compact subset of the feasible domain. Continuity then gives attainment. A candidate equal to \(F\) already attains unless some point beats it, when the same compactness argument applies. Conversely every attained maximum is a candidate and reaches \(F\). All finite winners are the candidates at the largest value, and all co-winning escapes are the reachable face laws giving that same value. This is an exact test in terms of scores and faces. A terminating algorithm additionally requires root coverage and certified value comparisons.

For recurrence limits, normalize weights from \(w_1=1\). Divergence of the limiting series forces fixed-cell masses to vanish by taking arbitrarily long partial sums. For a summable limit, a summable bound or a separate normalization argument is required. If the ratio of each fixed cell to its successor tends to zero, each fixed mass vanishes. Named probability laws below have total mass one unless explicitly described as defective.

### Modification transfer

**Proved for all masks.** The objective is \(T+B(v)\). Free \(p_0\) fixes \(v=z\), including \(z=0\); held \(p_0=v\) fixes the same factor by evaluation. Holding \(v=0\) with zeros or \(v=1\) with positives is incompatible. For compatible samples with positives the complete maximizing set and every winning face are the truncated ones crossed with the chosen \(v\). On all zeros free \(p_0\) selects one. Held \(p_0>0\) leaves likelihood \(p_0^n\), flat in parent coefficients under their holds, subject to the positive component being defined if \(p_0<1\). At \(p_0=1\) that component is unused. A held \(p_0=0\) is incompatible with all zeros.

### Inflation transfer

**Proved for all masks.** With positives the exact free-inflation envelope and held-inflation objective are
\[
 Q=T+B(\max(z,u)),\qquad \hat\pi=\max(0,(z-u)/(1-u)),\qquad
 Q_j=T+B(j+(1-j)u)\quad(\pi=j<1).
\]
Holding \(\pi=1\) is incompatible with positives. For held \(\pi=0\), use the parent atlas. With no zeros free \(\pi\) is zero, and held \(\pi<1\) multiplies the parent likelihood by \((1-\pi)^n\), preserving its maximizing sets. An attained full modified maximizer is feasible under free inflation iff \(u\le z\), and under held inflation iff \(j+(1-j)u=z\). If any passes, all inflation maximizers are exactly the passing maximizers, with the displayed free inflation coefficient or the held value. This is strict Bernoulli maximization at \(v=z\).

The free-inflation branches use the conditional score on \(u\le z\) and the parent score on \(u\ge z\), with holds and included faces respected. They join with matching first derivatives at an interior seam because \(B'(z)=0\). For a held value use \(\nabla T+(1-j)B'(j+(1-j)u)\nabla u=0\) with constrained derivatives on included faces. Apply the critical-value test to both branches together. The CMB example below refutes the unrestricted shortcut from an infeasible conditional winner to \(\pi=0\).

For the escaped heights use parent pointwise closure, including defective limits. If \(u<1\), the objective is evaluated by the displayed formulas on that same limiting vector of masses. If \(u\to1\), every inflated positive mass tends to zero because it is at most its parent mass. New conditional rare-event faces with \(u\to1\) therefore require no additional inflation comparison. On all zeros free \(\pi\) yields every compatible parent at \(\pi=1\), plus every attained parent zero atom at any \(\pi\). Held \(\pi=1\) leaves the parent flat. For held \(\pi<1\), the total zero probability strictly increases in \(u\), giving precisely the parent zero-only maximizers and escapes.

### Free inflation in natural coordinates

**Proved.** On a convex natural chart with exponential weights, the parent and conditional Hessians are negative covariance matrices times sample size. Along any segment the objective \(Q\) has nonincreasing derivative in either branch, and its derivative agrees at switches. The branches are analytic inside the chart, so switches are locally isolated unless the entire segment lies on the seam, where the values agree. This proves concavity, strictly in directions with positive covariance on both branches. Included faces follow by continuity. A feasible stationary or KKT point is global on such a chart. If there is a flat saturated direction, keep the full maximizing set. This argument applies to free inflation on each CMB design and on CMP. The held-\(\pi\) objective needs separate analysis.

## Beta-binomial

The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md), [fixed-design proof](2026-11-25-bbinom-math-report.md) and [parent design comparison](2026-11-27-crit-open-math-report.md#bbinom-held-all-free) supply the baseline. The ordinary dispersion sign uses the zero-cell normalization and does not carry over.

### bbinom all-free truncated

**Proved.** Size \(N=0\) has no positive event. At \(N=1\) only the count one is compatible and every unheld shape is flat. For \(N\ge2\) use \(w_1=1\) and
\[
 \frac{w_{k+1}}{w_k}=\frac{N-k}{k+1}\frac{a+k}{b+N-k-1},\quad 1\le k<N,
\]
where \(a,b\) are alpha and beta. The finite-design closures apply at each allowed \(N\ge\max x_i\), with any held shape fixed.

| Fixed-design path | Conditional limit |
|---|---|
| \(a\to0,\ b\to b_0\in(0,\infty)\) | Same recurrence at \(a=0,b=b_0\) |
| \(a,b\to\infty,\ a/b\to c\in(0,\infty)\) | Truncated binomial with odds \(c\) |
| \(b\to\infty,\ a/b\to0\) | \(\delta_1\) |
| \(b\to0\), \(a\to\infty\) with bounded \(b\), or \(a/b\to\infty\) with \(b\to\infty\) | \(\delta_N\) |

At \(b\to0\) the last ratio diverges and the other ratios are bounded below by positive constants, giving \(\delta_N\) even when \(a\to0\). The other limits follow term by term on finite support. Extended shape limits exhaust sequences. For \(N\to\infty\) write \(h=N/(b+N)\).

| Growing-design path | Conditional limit | Compatible free-size masks |
|---|---|---|
| \(a\to a_0>0,\ h\to h_0\in(0,1)\) | Truncated NB of size \(a_0\), success probability \(1-h_0\) | All free or alpha held |
| \(a\to0,\ h\to h_0\in(0,1)\) | Logarithmic series with power parameter \(h_0\) | All free |
| \(a\to\infty,\ h\to0,\ ah\to c\in(0,\infty)\) | Truncated Poisson at \(c\) | All free |
| \(h\to0,\ ah\to0\) | \(\delta_1\) | All free or alpha held |
| \(h\to1\) with bounded \(a\), or \(ah\to\infty\) | Fixed positive masses vanish | Whenever the path respects the holds |

For bounded \(a\) and \(h_0<1\), eventually \(b\ge1\), and \(N/(b+N-1)\) times \((a+k)/(k+1)\) bounds the ratio and is uniformly below one beyond a fixed \(k\). The same bound works for \(h\to0\) and bounded \(ah\). At \(h\to1\) with \(a\to a_0\ge0\), the ratio limit \((a_0+k)/(k+1)\) gives divergent total weight, including at \(a_0=0\). If \(ah\to\infty\) each fixed count loses to its successor. Together with the finite-design faces this exhausts the paths under all six atlas masks. Size free with held beta has no growing-design positive-likelihood boundary. The \(N=1\) slice is an attained saturated fit on all ones and can tie an excluded \(\delta_1\). For an all-\(k\) sample with \(k\ge2\) and free size, if the holds permit an upper-atom path at \(N=k\), it is the unique winning law, unattained since all finite feasible slices have full positive support.

At a fixed \(N\ge2\), all ones cannot attain if a shape is free. Each ratio increases strictly in \(a\) and decreases strictly in \(b\), so \(g_1=1/\sum_k w_k\) increases on sending a free \(a\) to zero or a free \(b\) to infinity. With \(b\) held this gives the rare-alpha law; with \(b\) free it gives \(\delta_1\). Likewise for all \(N\), \(g_N=1/\sum_k w_k/w_N\) increases when the ratios increase, selecting \(\delta_N\) by \(a\to\infty\) or \(b\to0\).

Use the score of \(\sum_i\log w_{x_i}-m\log\sum_{k=1}^Nw_k\) and the tables in the critical-value test. At each finite size the likelihood is rational in the free shapes, with strictly positive denominators. Clearing denominators gives a semialgebraic critical set. The likelihood is constant on each connected component by piecewise smooth path connectivity and the zero derivative. For algebraic holds one can compare all the component values and finite-size face values by polynomial methods; include dimensions of flat sets and all ties. This handles known-size cardinality per input. It leaves the growing-design comparison.

**Proved exact nonuniqueness at size two.** For \((1,2)\), the ratio \(g_2/g_1=(a+1)/(2b)\) gives the whole maximizing ray \(b=(a+1)/2\), \(a>0\). Both empirical proportions are attained. This remains the full finite maximizing set when size is free and both shapes are free, since size one is incompatible and larger finite designs give positive mass to counts above two.

### bbinom all-free modified

**Proved.** Apply the modification transfer with the beta-binomial conditional tables, including the shape-flat \(N=1\) slice. In particular any compatible zero/one sample at size one gives no shape information after free zero modification, though it can identify a shape ratio in the ordinary Bernoulli model.

### bbinom all-free inflated

**Proved reduction.** With \((b)_N\) denoting a rising factorial, \(u=(b)_N/(a+b)_N\), so feasibility is \((b)_N\le z(a+b)_N\). At bounded size the parent faces are binomial, endpoint mixtures and endpoint atoms, and at growing size the positive-likelihood faces are NB and Poisson under the holds, as in the [parent closure](2026-11-27-crit-open-math-report.md#bbinom-held-all-free). Use their actual zero masses in the inflation transfer and compare with the two branches or the held score. A finite size score and mass comparison at algebraic holds, including held \(\pi\), is semialgebraic by the same clearing argument. For all zeros at held \(\pi<1\), size free attains at zero. At positive held size with a free shape, \(u\uparrow1\) at \(a\downarrow0\) or \(b\uparrow\infty\). Both shapes held at fixed size give evaluation.

## Beta-negative-binomial

Write \(r,a,b\) for size, alpha and beta, and \(d=a+r+b\). The [parent proof](2026-11-25-bnbinom-math-report.md) supplies the recurrence, swap and ordinary face comparison. Its NB score comparison and fitted Poisson expansion involve the unconditional likelihood and cannot decide the conditional face.

### bnbinom all-free truncated

**Proved.** At positive indices, \(c_k=(k+1)g_{k+1}/g_k=(k+r)(k+b)/(k+d)\). Order \(r\le b\) to classify sequences, then undo the ordering to apply the holds. A held numerator shape remains unchanged, and a held alpha requires \(d-r-b\) to be fixed. In the diverging-\(d\) NB or logarithmic row, alpha diverges; in the Poisson row with positive rate alpha also diverges. With held alpha and one numerator shape diverging while the other stays bounded, the ratio \(b/d\) in the ordered representation tends to one.

| Path | Conditional law |
|---|---|
| Bounded \(d\), \(a\to a_0>0\), at least one numerator shape tends to zero | Normalize the limiting recurrence at \(k\ge1\) |
| Bounded \(d\), \(a\to0\) | Fixed masses vanish |
| \(d\to\infty,\ r\to r_0<\infty,\ b/d\to p\in(0,1)\) | Truncated NB for \(r_0>0\); logarithmic series for \(r_0=0\), with power \(p\) |
| \(d\to\infty,\ r\) bounded, \(b/d\to0\) | \(\delta_1\) |
| \(d\to\infty,\ r\) bounded, \(b/d\to1\) | Fixed masses vanish |
| \(r,b\to\infty,\ rb/d\to c\in[0,\infty)\) | Truncated Poisson at \(c>0\), or \(\delta_1\) when \(c=0\) |
| \(r,b\to\infty,\ rb/d\to\infty\) | Fixed masses vanish |

For bounded \(d\), the ratio is \(1-(a+1)/k+O(k^{-2})\) uniformly, giving summable domination for \(a\) bounded below. At \(a=0\), the limiting weights are asymptotic to a positive multiple of \(1/k\) by the summable remainder. For \(d\to\infty\), bounded \(r\le R\), \(b/d\le p'<1\) and \(d\ge D\), the ratio is bounded by \((k+R)/(k+1)\) times \((k+p'D)/(k+D)\). Choose \((1-p')D>R+2\), giving summable products. At \(p=1\) the limiting series diverges even at \(r_0=0\). When both numerator shapes diverge and \(rb/d\) is bounded, \(a/d\to1\), and
\[
 c_k/(k+1)=1+\frac{rb-d-(a+1)k}{(k+d)(k+1)}\le 1-\frac{2}{k+1}
\]
uniformly for sufficiently large \(k\), since \((a-1)k\ge rb+d\) there. This proves domination. When \(rb/d\to\infty\), every fixed mass loses to its successor. The bounded rare-\(r\) law has weights proportional to \((b)_k/[k(a+b)_k]\) if \(b>0\), with normalizer \(\psi(a+b)-\psi(a)\) by integrating the series for \(-\log(1-x)\) against the beta density. The other rare face interchanges the labels, and the recurrence also handles the simultaneous rare corner.

The test for all eight masks uses the derivatives of \(\log f_k-\log(1-f_0)\), with \(u=\Gamma(a+r)\Gamma(a+b)/[\Gamma(a)\Gamma(d)]\). All-held evaluates. All ones gives the unattained \(\delta_1\) supremum whenever the holds allow that path. Equality of the positive ratios forces equality of the rational functions in \(k\); their linear denominators do not cancel since \(d>r,b\). This identifies \(d,a\) and the unordered pair \(r,b\). Include both labelings of an off-diagonal winner whenever compatible with the holds. The identification argument leaves distinct canonical winners possible.

On all ones, a free alpha can send \(a\to\infty\) at fixed positive numerator shapes to reach \(\delta_1\). At held alpha, every ratio strictly increases in either numerator shape, since \(1/(k+r)>1/(k+d)\) and \(1/(k+b)>1/(k+d)\). Thus \(g_1\) strictly increases when any free numerator shape decreases to zero. The unique winning law is the corresponding bounded rare-shape recurrence, taking every free numerator shape to zero. If any numerator shape stays bounded below away from zero, monotonicity supplies a strict likelihood deficit by fixing it at that positive lower bound and sending just the other free shape to zero. With all parent coefficients held there is no escape.

**Proved exact sample refuting labeled uniqueness.** Take \(K=10^{15}\), and for each \(1\le k\le K\) take \((K+3)!/[(k+2)(k+3)]\) copies of \(k\). The counts are integers because the two distinct factors both occur in the factorial. The weights are the law \(G\) at \((r,a,b)=(1,1,2)\), conditioned further on \(k\le K\): \(G_k=3/[(k+2)(k+3)]\) by the recurrence and telescoping, and the removed tail is \(p=3/(K+3)\). Write \(P\) for the empirical frequencies. Any candidate law \(H\) beating or tying \(G\) has \(D(P\Vert H)\le D(P\Vert G)=-\log(1-p)\le2p\), with \(D\) the relative entropy. Grouping one cell against its complement decreases relative entropy by Jensen's inequality. Binary relative entropy is at least \(2(P_k-H_k)^2\) by Taylor's theorem in its first argument, whose second derivative is \(1/[v(1-v)]\ge4\). Hence \(|H_k-G_k|\le\sqrt p+2p<10^{-7}\).

In particular, the three positive recurrence ratios \(c_1,c_2,c_3\) lie within \(10^{-5}\) of \(6/5,2,20/7\), respectively: the error in \(c_i\) is bounded by \([(i+1)+c_i(G)]10^{-7}/(G_i-10^{-7})\) for \(i=1,2,3\). For every such candidate \(d=2[1-(c_2-c_1)]/(c_3-2c_2+c_1)-3\) lies between \(3.99\) and \(4.01\). With \(A_i=(d+i)c_i-i^2\), \(s=r+b=A_2-A_1\) is between \(2.96\) and \(3.04\), and \(v=rb=2A_1-A_2\) is between \(1.95\) and \(2.05\). The bounds follow by substituting the endpoints, with an exact rational check in [check.py](2026-11-27-zero-mod-math-data/check.py). They separate \(a=d-s\) from zero and give \(s^2-4v>0\). They also bound each numerator shape above by \(s\) and below by \(v/s\). The positive likelihood level therefore lies in a compact interior box disjoint from the diagonal. It contains \(G\), so a finite maximum exists. Every maximizing pair has a distinct reflected pair. This example is specified by its exact integer histogram formula and does not require materializing that histogram.

### bnbinom all-free modified

**Proved.** Apply the modification transfer under all eight masks. The truncated label orbits persist on all compatible samples with positives; the parent coordinates on all zeros are flat under the holds.

### bnbinom all-free inflated

**Proved reduction.** Compare the gamma ratio \(u\) with \(z\), use the conditional and parent branches or held score, and compare the parent NB and Poisson faces from the [closure analysis](2026-11-25-bnbinom-math-report.md#faces-and-the-finite-versus-face-test). Each label has the same \(u\) so neither inflation feasibility nor holding \(\pi\) breaks a permitted swap. On all zeros with held \(\pi<1\), \(u\to1\) when a free numerator shape tends to zero or free alpha tends to infinity. The gamma ratio gives the former, and beta concentration at success probability one gives the latter. Finite positive parameters have \(u<1\). All-held evaluates.

## Conway–Maxwell–binomial

Use the supported size range \(0\le N\le10^7\) established in the [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) and [parent math report](2026-11-25-cmb-math-report.md#supported-model). Infinity is outside that range, so a large-design CMP approximation can aid bounding but cannot supply an escape from the supported model. Write \(r=p/(1-p)\), \(\eta=\log r\), \(h_N(k)=\log\binom Nk\).

### cmb all-free truncated

**Proved.** Discard \(N=0\), \(p=0\), and unsupported observations. At \(p=1\), only all \(N\) is compatible and dispersion is flat. At \(N=1\) the conditional law is \(\delta_1\) for all \(p>0\), and both coordinates are flat wherever free. For \(N\ge2\) and finite natural coordinates use weights \(\exp(k\eta+\nu h_N(k))\) on \(1\le k\le N\). Therefore:

| Fixed design, additional holds | Finite solution and escaped candidates |
|---|---|
| Both \(p,\nu\) held | Direct evaluation |
| \(\nu\) held, \(p\) free | Unique mean root for empirical mean in \((1,N)\); \(\delta_1\) excluded at mean 1; \(\delta_N\) attained at mean \(N\) |
| Interior \(p\) held, \(\nu\) free | Unique \(h_N\) moment root between its extremes. At the minimum or maximum the supremum is the normalized \(r^k\) law on the corresponding minimizing or maximizing set of \(h_N\) |
| Neither held, \(N=2\) | Mixed samples attain on the entire straight line \(r/2^\nu=\#\{2\}/\#\{1\}\); the two constants use the endpoint rules above |
| Neither held, \(N\ge3\) | Unique moment root in polygon interior; a proper hull face gives its empirical point or two-point law, with the upper endpoint \(N\) attained at \(p=1\), and every other boundary empirical point unattained |

The normalization derivatives are positive covariances except along unidentified directions. For fixed \(p\) the candidate minimum set is \(\{N\}\) for \(N\ge2\); the maximum set consists of the middle integer(s) intersected with positive support. The fixed odds determine their weights, even when the empirical sample occupies only one of two central cells. For two free coordinates the strict decrease of \(h_N(k+1)-h_N(k)\) gives a polygon with consecutive upper edges and a lower edge joining \(1,N\). A direction into infinity exposes a proper face. Restricted directions along its tangent yield exactly the empirical probabilities at a winning exposed-face likelihood, by the multinomial bound. Interior empirical statistics give coercivity, since every unit natural direction has some cell with statistic strictly farther in the direction than the sample mean statistic, uniformly by compactness. The one-dimensional cases use the monotone moment score. At \(N=2\) the log weight ratio of cell 2 to cell 1 is \(\eta-\nu\log2\), giving the line in the table. These are the parent hull proofs with the cell zero removed. In particular the lower hull edge changes.

For free size apply the table at every \(N\) from the sample maximum through \(10^7\), retaining attainment bits and all tied designs. The maximum of these finitely many heights gives the iff and exact set. With neither canonical coefficient held, constants attain at \(N=k,p=1\) (ones also have the flat slice at \(N=1\)). Mixed samples supported on \(\{1,2\}\) attain at \(N=2\) and can tie excluded faces from other designs. Other adjacent pairs, and pairs \(\{1,M\}\) with \(M\ge3\), have the saturated two-point supremum with no attained fit. For the latter the excluded lower edge at \(N=M\) gives the saturated value. For either type no finite feasible slice can have just those two cells in its support. All remaining nonconstant samples have an interior empirical statistic for every feasible design, so a finite maximum exists. This supplies a test for all eight masks and counts their fit sets by retaining the solutions and rays from each winning design.

**Proved exact cross-design tie.** Take sample \((2)\), held \(\nu=1\) and \(q=1-p=(\sqrt{3}-1)/2\), with size free. The conditional count-two masses at \(N=2,3\) are equal by \(2q^2+2q=1\). For \(N\ge3\) the ratio from size \(N\) to \(N+1\) is \(q(N+1)(1-q^N)/[(N-1)(1-q^{N+1})]<2q<1\). These are exactly the two maximizing sizes.

### cmb all-free modified

**Proved.** Apply the modification transfer to the conditional hull at each feasible size. Retain every winning slice, including the exact two-design tie on the previous positive sample or on the same positive histogram with any compatible zeros. On all zeros the parent coordinates are flat over the admissible sizes and holds. Size zero does not supply a positive component.

### cmb all-free inflated

**Proved.** At each allowed design, use \(u=1/\sum_{k=0}^N r^k\binom Nk^\nu\) and both branches or the held score. The parent polygon uses \(0,\ldots,N\); the exact parent faces are the exposed vertices and edges under two free coordinates, the endpoint or central sets under fixed odds, and the atoms under fixed dispersion and diverging odds. The proof by natural parameters is the same as in the truncated table using this larger support, or the [parent held-size proof](2026-11-25-cmb-math-report.md#held-size). Use their actual limiting zero masses in the envelope. For free \(\pi\) each natural chart is concave by the transfer proof. For \(N\ge3\) it is strictly concave on the free canonical directions. For \(N=2\) retain the section of the conditional odds line on \(u\le z\) when it saturates, and otherwise use the parent branch. At \(N=1\) the sole positive mass is \(p\); choose any allowed \(p\ge1-z\) on the free-\(\pi\) saturated region or maximize \(p\) in the unsaturated branch. Free dispersion is irrelevant. The included atom at \(p=1\) is evaluated directly. For held \(0<\pi<1\) use the score test with the same parent faces. Design comparison retains every tie.

For held \(\pi<1\) on all zeros, free size or free \(p\) allows \(N=0\) or \(p=0\), respectively, wherever compatible with the other holds. If size and interior \(p\) are held, \(N=1\) leaves dispersion flat, and \(N\ge2\) with free dispersion sends \(\nu\to-\infty\), with zero mass \(1/(1+r^N)\). This follows from the strict decrease of \(u\) in dispersion when an interior cell is present. With known size and \(p=1\), the zero mass is zero at any positive size and dispersion is flat. With no free coordinate evaluate the held law. A zero-likelihood deterministic incompatibility should be reported as such.

**Proved counterexample to the proposed boundary shortcut.** Use \(x=(0,2,2,2,2,2,2,2)\) and hold \(\nu=1,\ p=317/500\), with \(q=183/500\). Let \(g_N=\binom N2p^2 q^{N-2}/(1-q^N)\). By \(2q^2+2q<1\), \(g_2>g_3\). By the ratio bound in the tie example, all \(N>3\) have \(g_N<g_3\). The unique conditional winner \(N=2\) violates \(u=q^2>z=1/8\). But \(q^3<z\), so \(N=3\) can saturate with \(\pi>0\). Its likelihood \(z(1-z)^7 g_3^7\) strictly exceeds \(q^2p^{14}\), the size-two boundary value. This is a rational inequality verified with exact integers in [check.py](2026-11-27-zero-mod-math-data/check.py). Every \(N>3\) is beaten by the strict conditional inequality even if it could saturate, and \(\pi=0\) at \(N=3\) loses the strict Bernoulli comparison. Hence the global inflated fit is uniquely size three with positive inflation.

## Conway–Maxwell–Poisson

The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md#details) supplies the parent moment proof. Write \(a=\log\lambda\), with weights \(\exp(ka-\nu\log(k!))\). Positive likelihood requires \(\lambda>0\). At \(\nu=0\) one needs \(\lambda<1\).

### cmp all-free truncated

**Proved.** If \(\nu\) is held, the strictly increasing conditional mean spans \((1,\infty)\) as the rate traverses its domain. This follows from the variance derivative and from the limits \(\lambda\downarrow0\) and \(\lambda\uparrow\infty\) for positive dispersion, or from the shifted geometric mean at zero dispersion. Thus all ones escapes to \(\delta_1\), and every other sample has a unique finite root. With positive \(\lambda\) held, the expected \(\log(X!)\) decreases strictly in \(\nu\), by the negative variance derivative. It tends to zero as \(\nu\to\infty\), when the conditional law tends to \(\delta_1\). At the lower endpoint, it has the shifted-geometric expectation if \(\lambda<1\), or diverges if \(\lambda\ge1\), since each fixed mass vanishes there. All ones escapes to \(\delta_1\); otherwise finite attainment is unique at the scalar moment root or the included geometric face. Both parameters held give evaluation.

With both free, a constant or a sample supported on two adjacent counts gives the corresponding saturated face, strictly beating every finite positive-rate fit. Along \(\nu\to\infty\) the successive ratio is \(\exp(a-\nu\log(k+1))\). Choosing \(a/\nu\) between the two thresholds around a count gives its point mass, using the lower-support endpoint for one. Choosing \(a=\nu\log(j+1)+c\) gives any split at \(j,j+1\). Along an arbitrary diverging dispersion subsequence, those ratios are strictly ordered in log with successive gaps diverging at every fixed count. If the mode diverges all fixed cells vanish; otherwise one can only retain a count or two adjacent counts. Geometric domination outside the retained cells follows from the monotone ratios. With bounded dispersion, rates tending to zero give \(\delta_1\), rates diverging force fixed cell masses to vanish, and at \(\nu\to0\) a finite rate below one gives the included geometric law by domination. At finite rate at or above one the limiting weights sum to infinity. These cases exhaust the escapes, so nonadjacent sample range gives attainment and strict concavity gives uniqueness. This is the parent's moment and adjacent-face proof with conditional statistics.

For an explicit geometric boundary test, match the conditional mean \(\bar x_+\) at \(\lambda=1-1/\bar x_+\). The geometric MLE stays on the boundary iff the inward dispersion score \(E_g\log(X!)-\overline{\log(x!)}_+\le0\). Otherwise solve the two conditional moment equations. The positive covariance matrix and support inequalities justify the KKT test and uniqueness even at score equality.

### cmp all-free modified

**Proved.** Apply the modification transfer using the CMP moment tests, the conditional geometric KKT case and the saturated point and adjacent faces. The zero-rate parent provides no positive component. All compatible parent coordinates are flat on all zeros.

### cmp all-free inflated

**Proved.** Use \(u=1/\sum_{k\ge0}\lambda^k/(k!)^\nu\). For free \(\pi\) the natural chart is convex, including the geometric boundary segment \(a<0,\nu=0\), and \(Q\) is strictly concave on its free directions with positives. Its unique finite winner exists iff there is a feasible branch KKT solution, with all-held evaluation as usual. Compute it by the truncated moment/KKT tests with \(u\le z\) or the parent tests with \(u\ge z\). For a held positive rate or dispersion the same scalar branch tests work. An attained geometric solution can meet the seam without creating a tie. For held \(0<\pi<1\) use the score and face comparison. Parent escaping laws on the positive-likelihood level are the point and adjacent faces, including \(\{0,1\}\) at held rate with \(\nu\to\infty\), with odds \(\lambda\). The successive-ratio argument above, now including \(k=0\), proves exhaustion; finite geometric parameters are included. For free inflation the face weights must maximize the inflated objective, which can leave a set of original \(\{0,1\}\) weights all giving the same total zero mass. Use the resulting probabilities to report the limit.

On all zeros with held \(\pi<1\), free rate attains at zero, leaving any free dispersion flat. At positive held rate, free dispersion approaches the excluded Bernoulli law with zero probability \(1/(1+\lambda)\) as \(\nu\to\infty\), strictly improving since every term at \(k\ge2\) decreases. Held rate zero gives an atom and any free dispersion is flat. Both held evaluate.

## Discretised gamma

Let \(Y\sim\mathrm{Gamma}(a,r)\), with shape \(a>0\), rate \(r>0\) and \(X=\lfloor Y\rfloor\). The [parent dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md) and [shape-profile report](2026-11-25-dgamma-math-report.md) give concentration and the parent variance proof. The original shape profile and its rate equation use the full gamma distribution, so they change under conditioning.

### dgamma all-free truncated

**Proved.** The continuous kernel becomes \(y^{a-1}e^{-ry}\) on \(y\ge1\). Each conditional cell probability is its integral on \([k,k+1)\) divided by its integral over \([1,\infty)\). Write \(E_A\) and \(\operatorname{Var}_A\) for the continuous conditional moments on the entire positive event, and \(E_k,\operatorname{Var}_k\) for cell moments. With fractions \(w_k\) in the positive sample the average scores are
\[
 T_a/m=\sum_k w_k E_k\log Y-E_A\log Y,\qquad
 T_r/m=E_A Y-\sum_k w_k E_kY ,
\]
where subscripts on \(T\) denote derivatives. The formula follows by differentiating the conditional log integrals.

The complete positive-likelihood escapes for free coordinates are as follows. At \(a\downarrow0,\ r\to r_0>0\), the kernel becomes \(y^{-1}e^{-r_0y}\), by dominated convergence. This is a proper excluded shape-zero law. For bounded \(a\) and \(r\downarrow0\), the denominator diverges while fixed-cell integrals stay bounded: integrating up to \(1/r\) bounds the denominator below by \(e^{-1}\log(1/r)\). For bounded \(a\) and \(r\uparrow\infty\), the conditional law tends to \(\delta_1\), by comparing the kernel for \(y\ge2\) to an interval strictly below 2 and bounding the remaining exponential integral.

When \(a\to\infty\), pass to a limit \(a/r\to b\) in the extended positive axis. If \(1<b<\infty\), relative variance \(1/a\) of \(Y/(a/r)\) gives concentration at \(b\); the limit is a point law or a split at a positive integer boundary. Since the positive event has mass tending to one, the parent normal-limit construction gives every such split with its chosen weights. If \(b=\infty\), the same relative concentration makes fixed-cell masses vanish and the positive event have probability tending to one. For \(b\le1\), for each \(\epsilon>0\) the log derivative \((a-1)/y-r\) is at most \(-c_\epsilon a\) beyond \(1+\epsilon/2\) for all sufficiently large shapes. Compare the resulting exponential upper bound beyond \(1+\epsilon\) with the integral on \([1+\epsilon/4,1+\epsilon/2]\). The kernel is eventually decreasing on that interval and its integral is at least its right endpoint value times the interval length. This gives concentration at the lower truncation boundary and the conditional count-one law. These cases exhaust subsequences and respect holds by keeping the held parameter fixed.

For **both free**, constants or adjacent pairs have their saturated supremum unattained, by the concentration construction and full finite support. For range at least two, compare all joint critical points with the best shape-zero value. The latter is attained at a positive finite rate since at \(r\downarrow0\) fixed masses vanish and at \(r\uparrow\infty\) at least one observed cell \(k>1\) vanishes. Thus one can use the rate score at \(a=0\) to specify the face candidates. Any strict improvement proves attainment. Exact equality at a finite critical point is included in the rule.

For **held rate**, the parent quantile argument carries over to the conditional law of \(\log Y\) on \([0,\infty)\). Its log density is \(a t-r e^t+\text{constant}\), strictly concave including at \(a=0\). For its CDF \(F\), quantile \(H\) and density \(f\), \(I(v)=f(H(v))\) is strictly concave, since \(I''=(\log f)''(H)/f(H)<0\). Its endpoint values are nonnegative. For a cell starting at CDF level \(v_0\) with probability \(p\), strict concavity gives \(I(v_0+pv)>pI(v)\) for \(0<v<1\). This follows by mixing \(v\) and \(v_0/(1-p)\), and also holds when they coincide by positivity. The conditional quantile derivative on the cell is \(p/I(v_0+pv)<1/I(v)\). Every quantile spacing contracts, so the cell variance is strictly smaller than the full conditional variance. Consequently the shape score is strictly decreasing. The likelihood tends to zero as \(a\to\infty\) at held rate. There is a unique finite solution iff the right shape score at \(a=0\) is positive. Equality or negative score selects the excluded shape-zero law strictly. In particular all ones selects that law, since the lowest cell has strictly smaller mean log count-latent value than the whole positive event.

For **held shape**, all ones escapes to \(\delta_1\), and otherwise both rate tails have zero likelihood and a maximum exists. For \(a\ge1\), the conditional density in \(y\) is log concave. The quantile spacing proof with \(I\) concave gives a strict variance inequality because the conditional positive-event density is positive at its finite lower endpoint. More explicitly, the inequality is strict for \(v_0/(1-p)<1\) since \(I\) is positive there; any finite cell has \(v_0+p<1\). This proves strict rate concavity and uniqueness. At \(a=1\) this is also the shifted geometric case by memorylessness. For \(0<a<1\) use the critical-value rule; universal uniqueness is open. **Both held** evaluate.

**Proved failure of rate concavity below shape one.** For the exact singleton sample \((2)\), put \(c=a-1<0\). Integration from \(k\) upward, with \(u=r(y-k)\), gives \(\log\int_k^\infty y^c e^{-ry}dy=-rk-\log r+c\log k+c/(rk)+O(r^{-2})\). This expansion and its first two differentiated remainders follow by expanding \((k+u/r)^c\) at \(1/r=0\) under the exponential integral. The first three derivatives in \(1/r\ge0\) are bounded by constants times \(1+u^3\), since \(c<0\) and \(k\ge1\). Subtraction of the third cell tail from the second changes the expansion and its rate derivatives by exponentially small terms. Thus the conditional log mass at two has second derivative \(2c(1/2-1)/r^3+O(r^{-4})>0\) at large rates. The curvature failure is compatible with a unique optimum.

### dgamma all-free modified

**Proved.** Apply the modification transfer with the conditional Gamma kernel on \([1,\infty)\). In particular the held-rate shape-zero test and the held-shape atom case apply to positive counts alone, regardless of the number of zeros.

### dgamma all-free inflated

**Proved reduction.** Evaluate \(u\) as the gamma probability on \([0,1)\). The feasibility comparison uses this actual CDF. The parent concentration proof and small-shape bound in the [parent dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md#existence-and-identification) give only point or adjacent laws of positive likelihood on escapes with positives. The bounded-shape rate tails are zero atom or loss to infinity. If \(a\to0\) every fixed positive cell vanishes uniformly over rate, even on a path where \(u\) has a limit strictly between zero and one. This eliminates the shape-zero conditional law from the positive-likelihood inflation comparison. Concentration faces with positives are available only with both coefficients free. For a held coefficient the two ends have zero likelihood with positives, so a finite optimum exists for every admissible held or free inflation coefficient. For all zeros with held \(\pi<1\), free rate can send \(r\to\infty\) at fixed shape or free shape can send \(a\to0\) at fixed rate. Both give supremum one in zero probability, unattained. Both held evaluate.

With both parent coefficients free and free inflation, the exact criterion is that the positive sample range is at least two. At range zero or one, a parent concentration face supported on the occupied positive cells assigns zero mass zero and supplies the empirical positive proportions. Taking the limiting inflation coefficient equal to the observed zero fraction gives the saturated likelihood, which every finite parent with positives misses. With range at least two, every escaped likelihood is zero, since no parent face covers the positive sample. The same two arguments also decide the range-at-least-two case for any held admissible inflation, and the short-range case if held inflation is at least the observed zero fraction. In the latter case the minimum attainable total zero probability in the closure is the held inflation coefficient and it maximizes the Bernoulli factor on the allowed interval.

## Discrete Weibull

Use \(q=\mathrm{shape1}\in(0,1)\), \(t=-\log q>0\), and \(\beta=\mathrm{shape2}>0\). The [parent proof](2026-11-25-dweibull-math-report.md#attainment-and-maximizer-sets) and [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md#why-a-single-profile-bracket-is-global) give joint concavity before conditioning. Subtracting the log positive mass adds \(t\); the parent Hessian argument in \((\log t,\beta)\) does not establish the conditional joint result.

**Proved failure of joint concavity in the parent coordinates.** For the exact singleton sample \((2)\), hold an interior \(\beta\) and let \(t\to\infty\). The Hessian of the leading term \(-e^{\log t}(k^\beta-1)\) in \((\log t,\beta)\) has determinant \(-t^2 k^\beta(\log k)^2\), negative at \(k=2\). The second derivatives of \(\log(1-e^{-tD_k})\) are exponentially small times polynomials in \(t\) at that fixed \(\beta\). Hence the full determinant is negative for large \(t\). This excludes direct reuse of the parent joint-concavity proof.

### dweibull all-free truncated

**Proved.** Conditional survival at integer \(k\ge1\) is \(S_k=\exp[-t(k^\beta-1)]\), and \(g_k=S_k-S_{k+1}\). Put \(A_k=k^\beta-1\) and \(D_k=(k+1)^\beta-k^\beta\). At held \(\beta\), the score of \(\log g_k\) in \(t\) is \(-A_k+D_k/\operatorname{expm1}(tD_k)\), strictly decreasing. It tends to positive infinity near zero and to \(-A_k\) at infinity. All ones escapes to \(\delta_1\); every other sample has a unique finite root. At held \(t\), the term \(-t(k^\beta-1)\) is concave, and
\[
 \log(tD_k)=\log t+\beta\log(k+1)+\log(1-e^{-\beta\log((k+1)/k)})
\]
is strictly concave. The function \(v\mapsto\log(1-e^{-e^v})\) is increasing and concave because its positive derivative \(e^v/(\exp(e^v)-1)\) decreases. Thus the remaining term \(\log(1-e^{-tD_k})\) is strictly concave in \(\beta\). At \(\beta\downarrow0\) all fixed probabilities vanish, and at \(\beta\to\infty\) the limit is \(\delta_1\). Therefore all ones escapes, and otherwise the finite maximizer is unique. Both held evaluate.

With both free, for \(\beta\to0\) let \(c=\lim t\beta\). Since \((k^\beta-1)/\beta\to\log k\), at \(0<c<\infty\) the conditional limit has survival \(k^{-c}\), a discretised Pareto. At \(c=0\) fixed masses vanish, and at \(c=\infty\) the limit is \(\delta_1\). For \(\beta\) in a positive compact interval, \(t\to0\) sends mass to infinity and \(t\to\infty\) gives \(\delta_1\). If \(\beta\to\infty\) and \(t\) stays bounded below, then \(S_2\to0\), giving \(\delta_1\). If instead \(t\to0\), the positive conditioning changes masses by a factor tending to one. As in the parent proof, the threshold \(t^{-1/\beta}\) gives only point or adjacent laws, or loss of mass to infinity. Explicitly successive exponents \(t k^\beta\) separate by ratios tending to infinity, so at most one finite positive survival level strictly between zero and one persists. Every positive point or adjacent split is reached by choosing a threshold in the occupied cell, or \(t=-\log(1-w)/(j+1)^\beta\) for lower fraction \(w\) on \(\{j,j+1\}\). The masses telescope to one on each finite limit.

Constants and adjacent samples thus have the saturated supremum, unattained. For larger range the only nonzero boundary competitor is the best discretised Pareto. Its log likelihood is strictly concave in \(c\) by the same rate score argument with \(A_k=\log k,\ D_k=\log((k+1)/k)\); its maximum is a unique positive root. Compare all stationary points of the smooth scalar profile in \(\beta\) with this height, profiling \(t\) by the strictly decreasing score. The implicit function theorem applies because its derivative in \(t\) is strictly negative. The critical-value rule gives exact attainment and equality. One sufficient strict-improvement test evaluates the right derivative at \(\beta=0\) holding \(t\beta=c_*\), the winning Pareto parameter. The derivative of each log mass is
\[
 \frac{c_*}{2}\left[-(\log k)^2+
 \frac{(\log(k+1))^2-(\log k)^2}
      {\operatorname{expm1}(c_*\log((k+1)/k))}\right].
\]
This follows from expanding \((k^\beta-1)/\beta\) to first order. A positive sum implies attainment, but a nonpositive sum alone does not exclude a separated finite peak.

### dweibull all-free modified

**Proved.** Apply the modification transfer with the conditional survival and Pareto comparison. In particular, a constant sample at a positive count can escape to that atom when both parent coefficients are free, even on data also containing zeros.

### dweibull all-free inflated

**Proved reduction.** Here \(u=1-e^{-t}\), so the free-\(\pi\) saturated region is \(t\le-\log(1-z)\). At each fixed \(\beta\) and with free \(t\), \(\pi=0\) and the saturated branch both have strictly concave criteria in \(t\). The parent positive terms have negative curvature and the zero term \(\log(1-e^{-t})\) is concave. The splice has matching derivatives. At held \(0<j<1\), the zero term is \(\log(1-(1-j)e^{-t})\), also concave, and positive terms differ from parent logs by a constant. Thus one can profile \(t\) by a single score or an endpoint test on either route. With \(t\) held, \(u\) is constant and the \(\beta\) optimum and escapes are exactly the conditional ones for either inflation route. With both free, compare the resulting profile and parent point or adjacent faces, including the \(\{0,1\}\) Bernoulli face with finite \(t\) and \(\beta\to\infty\). That is the only positive-likelihood escape when \(t\) is held. With \(\beta\) held and \(t\) free both parent endpoint likelihoods vanish on positives, giving an attained unique \(t\) for each inflation route. For the Pareto conditional path \(t\to\infty\), \(u\to1\), so no inflation likelihood with positives persists. For all zeros at held \(\pi<1\), free \(t\to\infty\) gives the excluded zero atom. A held \(t\) makes every allowed \(\beta\) equally good.

For both parent coefficients free, free inflation attains iff the positive sample range is at least two. The concentration faces with zero mass zero saturate every shorter positive range with the empirical zero fraction as the inflation coefficient. Full finite support excludes finite equality. For longer positive ranges no parent escape gives positive likelihood. That compactness conclusion also holds at any admissible held inflation. For a short positive range and held inflation at least the empirical zero fraction, the same faces at zero parent zero mass maximize both the constrained Bernoulli factor and the positive likelihood.

## Poisson–inverse Gaussian

Write \(\mu>0,\lambda\ge0\) for the mean and shape in the [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md). The [parent proof](2026-11-25-pig-math-report.md) gives the shape polynomial, joint pgf and boundary proof. Its fixed-Bessel-argument reduction sets the parent mean to the empirical mean. The conditional normalizer changes this reduction and invalidates using the parent variance sign for truncation.

### pig all-free truncated

**Proved.** Shape zero gives no positive component. At positive shape write
\[
 t=\lambda/\mu,\quad z_b=\sqrt{2\lambda+t^2},\quad d=z_b-t,\quad
 0<d<z_b,\quad \lambda=d(z_b-d/2).
\]
Use a subscript on \(z_b\) to distinguish the Bessel argument from the sample zero fraction \(z\). Then \(u=e^{-d}\) and for \(k\ge1\)
\[
 g_k=\frac{(\lambda/z_b)^k Q_{k-1}(1/z_b)}{k!\operatorname{expm1}(d)},
 \qquad
 Q_{k-1}(y)=\sum_{j=0}^{k-1}
 \frac{(k-1+j)!}{j!(k-1-j)!2^j}y^j .
\]
These identities come from the parent mass formula. At fixed \(z_b\), multiply the conditional \(d\)-score by \(d\). With \(S=\sum x_i\) the result is
\[
 S\frac{z_b-d}{z_b-d/2}-\frac{md}{1-e^{-d}}.
\]
It strictly decreases from \(S-m\) to a negative value. Hence if any count exceeds one there is exactly one inner root and a smooth scalar profile in \(z_b\) by the implicit function theorem. For all ones, its supremum at each \(z_b\) occurs as \(d\downarrow0\); the overall atom at one is unattained.

Put \(\rho=d/z_b\) and pass to extended limits. The conditional pgf from the parent pgf is
\[
 \frac{\exp\{z_b[1-\sqrt{1-\rho(2-\rho)s}]\}-1}{e^d-1}.
\]
When \(z_b\downarrow0\) and \(\rho\to\rho_0\in(0,1]\), this tends to \([1-\sqrt{1-\rho_0(2-\rho_0)s}]/\rho_0\), the Sibuya law of index \(1/2\) at \(\rho_0=1\) and its exponentially tilted, normalized version for \(\rho_0<1\). When \(\rho\to0\) here, rationalize \(1-\sqrt{1-\rho(2-\rho)s}\) to give the point law at one. For \(d\to0\) and \(z_b\) bounded away from zero, the same rationalization and expansion in \(d\) give \(\delta_1\). For \(z_b\to\infty\) and \(d\to d_0\in(0,\infty)\), rationalization gives a truncated Poisson of rate \(d_0\). For \(d\to\infty\), the Bessel mass formula bounds each fixed positive mass by a constant times \(d^k/e^d\) for \(z_b\ge d\) large, giving zero likelihood. With \(z_b,d\) bounded and of strictly positive limits, the only excluded endpoint is \(d/z_b\to1\), the truncated Poisson–Lévy law with shape \(d^2/2\). Its inward derivative at fixed \(z_b\) in \(t=z_b-d\) is \(m/(1-e^{-d})>0\), since \(\lambda=(z_b^2-t^2)/2\) is stationary there. The derivative is the same at fixed positive \(\lambda\), since \(z_b=\sqrt{2\lambda+t^2}\) also has zero derivative there. This excludes the Lévy face as a winner for both masks in which it is reachable. The pgf convergence at \(0<s<1\) gives coefficient convergence by the diagonal subsequence argument for probability vectors, and each listed proper limit has pgf one at \(s=1\).

For both free and any count above one, compare stationary values of the \(z_b\) profile with the best truncated Poisson and Sibuya-type values. The former uses the truncated Poisson mean equation. The latter depends on \(\rho\) through \(\rho^{S-m}(1-\rho/2)^S\), by the binomial series, and uniquely maximizes at \(\rho_*=2(S-m)/(2S-m)\). The inward log likelihood derivative in \(z_b\) at fixed \(\rho_*\) equals
\[
 \#\{i:x_i\ge2\}-m\rho_*/2.
\]
Indeed the ratio of the next-highest to highest coefficient of \(Q_{k-1}\) is one for \(k\ge2\), and \(d/\operatorname{expm1}(d)=1-d/2+O(d^2)\). A positive derivative removes that face if it would otherwise win. Compare against the Poisson height as well. The critical-value rule includes any exact tie.

With **mean held**, \(\lambda\downarrow0\) gives the Sibuya law at \(\rho=1\) and \(\lambda\to\infty\) gives truncated Poisson at the held mean. These follow by substitution in \(z_b,d\). Compare these two values with all stationary points of the one-dimensional Bessel expression. There is a closed decision for **all ones**: the finite likelihood is strictly below the larger of the two boundary values, which give mass \(1/2\) and \(\mu/(e^\mu-1)\) respectively. To prove it, \(0<d<\mu\) and the mass of one is \(d/[(2-d/\mu)(e^d-1)]\), using \(\lambda=\mu^2(1-d/\mu)^{-1}(d/\mu)^2/2\). Its log derivative has sign of \(1/(2\mu-d)-J(d)\), for \(J(d)=1/(1-e^{-d})-1/d>0\). The threshold \((d+1/J(d))/2\) is strictly increasing because
\[
 J(d)^2-J'(d)=\frac{1}{1-e^{-d}}\left(\coth(d/2)-2/d\right)>0.
\]
The last inequality follows from \(\tanh x<x\) for \(x>0\). The derivative can only change from negative to positive. Therefore one or both endpoints win, with both when \(e^\mu=1+2\mu\). This equation has one positive root by strict convexity and its derivative at zero. This is an exact two-face tie for the singleton sample \((1)\) under that hold.

With **positive shape held**, \(\mu\downarrow0\) gives \(\delta_1\), and \(\mu\to\infty\) gives the truncated Poisson–Lévy face with that shape. On all ones the unattained lower-mean atom wins. Otherwise the atom gives zero likelihood and the Lévy endpoint admits a strict improvement by the derivative above, proving finite attainment. The parent mean-uniqueness proof compares a different normalization derivative. Compare all conditional score roots for cardinality. **Both held** evaluate when the positive component exists.

### pig all-free modified

**Proved.** Apply the modification transfer with the conditional Bessel likelihood, including the Sibuya/Poisson tie at held mean on all ones and the atom under positive held shape. A held shape zero has no positive component and is available only for the pure zero modified law when no positive component is needed.

### pig all-free inflated

**Proved reduction.** Saturation requires \(d\ge-\log z\) for \(0<z<1\); evaluate the held objective with \(u=e^{-d}\). The [parent joint closure](2026-11-25-pig-math-report.md#joint-paths-to-the-boundary) and the transfer give ordinary Poisson and Poisson–Lévy candidates with positives, subject to the holds. At the Lévy face, the inward derivative at fixed \(z_b\), or at fixed positive shape, increases each original mass on the observed sample to first order: \(z_b'\) and \(\lambda'\) vanish and \((-d)'=1\). Thus for any fixed \(\pi<1\) the inflated likelihood strictly increases to first order. Holding its face-optimal \(\pi\) during this comparison also excludes the face when \(\pi\) is free. A held positive shape therefore gives finite attainment with positives. Held mean allows only the positive-rate Poisson at that mean; both free allow optimizing its rate and its inflation by the same envelope. The rare \(d\to0\) endpoints are zero atoms before conditioning and cannot maximize with positives. On all zeros with held \(\pi<1\), a free shape attains at zero, with arbitrary compatible mean. If positive shape is held, the zero mass tends to one only as \(\mu\downarrow0\). Both held evaluate.

## What this changes for the fitter

- **bbinom.** Separate empty-positive support, the size-one flat slice and positive fixed designs. Use rational critical scores and exact known-design face checks where feasible. For size free, a proved tail bound must cover every larger design before declaring a finite certificate or excluded NB, Poisson or logarithmic face. Beta held removes the growing-design competitor. Preserve attained/face ties.
- **bnbinom.** Normalize positive weights from count one, retain the rare-shape, logarithmic and ordinary truncated faces under the holds, and count compatible size/beta labelings. Apply strict inward witnesses or a global cover before choosing between an ordinary finite solution and a face. A single canonical root found by a search supplies no root count.
- **cmb.** Run moment and hull tests on support starting at one. The lower edge moves to \(\{1,N\}\), and the size-two conditional rank drops to one. Use the actual finite design domain and retain all maximizing designs. In inflation solve the joined chart at each competing design. The exact eight-observation example rules out dispatching all such inputs through the parent boundary.
- **cmp.** Use shifted moments, a scalar or joint monotone/concave solve and the included geometric KKT check. Distinguish a geometric optimum from excluded point and adjacent fits. For free inflation the branch-compatible moment checks certify the attained chart and its unique point. Held nonzero inflation needs the held score and a separate cover.
- **dgamma.** Use upper gamma normalization directly. At held rate the signed right score at zero shape decides an attained root versus an excluded shape-zero conditional law, including equality. At held shape use a zero-likelihood tail bracket for non-ones and the concavity certificate at shape at least one. At free shape compare to the best conditional shape-zero law as well as the concentration cases.
- **dweibull.** Compute the conditional survival via \(t(k^\beta-1)\) and stable differences. Use the proved scalar brackets under holds. Test the Pareto competitor and saturated faces before any claimed joint certificate. In inflation the Pareto face disappears; at fixed shape the \(t\)-profile stays strictly concave, and fixed \(t\) only changes the zero factor.
- **pig.** Compute the conditional Bessel mass with the \(\operatorname{expm1}(d)\) normalization, or use stable scaled ratios. At fixed Bessel argument use the monotone inner root. Test the two scalar faces and the Sibuya inward derivative. At held mean on all ones compare the exact boundary masses, preserving a two-face tie. At held positive shape the Lévy inward derivative rules that face out under both truncation and inflation.
- **All routes.** Map infeasible observations or empty positive support to invalid input. Map proved unattained atoms, two-point laws and named limits to the corresponding limit statuses and give usable model advice where a matching fitter exists. Keep finite/face equality as an attained fit with the face attached. Use an explicit nonunique outcome for proved flat sets and distinct tied fits, and unknown uniqueness when cardinality is unresolved. With no verified generic face sign or exhausted certificate work, retain a successful accuracy-checked search result flagged uncertified. If the search itself fails or a proof rules it out, report the appropriate search failure or proved limit. A capped or otherwise unchecked proposal supplies no successful fit. Apply the project work budgets and verify public coefficients and likelihood at their represented values.

## Open

- **bbinom.** For free size and alpha free or held, the closure test still needs usable conditional analogues of the signed binomial and NB/Poisson comparisons, uniform in the integer tail, with a decision at finite/limit equality. The known-size semialgebraic test supplies all roots for algebraic holds, but it gives neither a small work bound nor a universal singleton theorem across sizes. The corresponding inflation branch and parent-face comparison also needs an integer-tail exclusion.
- **bnbinom.** The recurrence determines the normalizer and identifies the faces, but does not bound or count canonical likelihood roots. A conditional NB/Poisson boundary expansion, the two bounded rare-face score signs, and a uniform sign or admissible global cover at equality remain needed. Ordinary NB score arithmetic cannot decide the conditional equality. The inflation problem with held nonzero \(\pi\) also needs joint score coverage.
- **cmb.** Exact finite design comparison may be computationally prohibitive at the supported cap. Efficient bounds need the conditional normalizer and must preserve cross-design equality. Held nonzero inflation lacks a general root-count or terminating equality certificate even with design held. Extending CMB to unbounded design would need a separate conditional growing-design closure and is outside this supported-domain result.
- **cmp.** The pure and free-inflation natural-chart tests close the attained-fit question. For held nonzero inflation with both zeros and positives, a root-count or global bound on the held objective and an exact finite/face equality test remain open.
- **dgamma.** For shape below one and held shape the conditional density is log convex in the latent variable, so the rate quantile contraction does not apply. The missing results are rate-root counting on that range, joint conditional shape profiling and a global comparison at the shape-zero boundary. Positive-likelihood inflation with a held coefficient attains, but general uniqueness and shape/face ties for inflation are not settled.
- **dweibull.** The extra \(t\) term removes the parent joint-curvature certificate. Prove unimodality of the conditional profile and a global sign test at the best discretised Pareto, or find an exact sample where a local face sign misclassifies the global maximum. The free-both inflation profile needs an analogous global comparison; scalar concavity alone does not rule out separated maxima.
- **pig.** The fixed-Bessel-argument score supplies a unique inner root, but the remaining profile requires a root-count and uniform comparison with Sibuya-type and truncated Poisson faces. Except for the exact all-ones result, held mean needs the sign and possible equality between finite and face heights. Held positive shape needs a conditional mean-uniqueness proof, and the inflation case needs the corresponding global comparisons against Poisson where reachable.

## Reproduction

**Numerical.** [check.py](2026-11-27-zero-mod-math-data/check.py) uses 70 and 100 decimal digits, counts 1 through 8, parameters \(0.3,0.7,1\) where applicable, and approach scales \(10^{-4},10^{-6},10^{-8},10^{-10}\). It checks conditional shape-zero gamma, discrete Weibull/Pareto, PIG/Sibuya-type, beta-negative-binomial rare-shape and logarithmic limits, and beta-binomial rare alpha. The maxima of absolute mass error divided by the approach scale were respectively \(0.23724952,0.12011459,0.25,0.30248521,0.39661065\) on the five printed grids. These replays do not certify off-grid inequalities. The rational CMB inequality is exact, with a displayed likelihood ratio of about \(1.00241176\). For the held-mean PIG tie the same two precisions locate the mean \(1.256431208626169676982738\) and verify that the finite mass of one is below \(1/2\) at the 99 points with \(d/\mu=j/100\), \(1\le j\le99\). The equality itself is the proved equation above. The BNB compact-box inequalities are checked in exact rational arithmetic without expanding the histogram. The two irrational holds in the CMB and PIG ties specify real-parameter examples; their rounded representations need not tie.

The final Python checks run in sequence using one workstation slot.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
data=dev/research/evaluations/2026-11-27-zero-mod-math-data
/tmp/umvl-cpu/run nice -n 10 python3 "$data/check.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/inventory.py"
```

 The [proof inventory](2026-11-27-zero-mod-math-data/proofs.csv) records the statements proved here.

## Individual hold masks

Each entry applies the indicated family proof under the named holds.

### bbinom alpha truncated

Keep fixed alpha in each slice and in the growing-size truncated NB face. [bbinom truncated proof](#bbinom-all-free-truncated)

### bbinom alpha modified

Apply the hurdle factor to the held [conditional fit](#bbinom-alpha-truncated). [bbinom modified proof](#bbinom-all-free-modified)

### bbinom alpha inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bbinom inflated proof](#bbinom-all-free-inflated)

### bbinom alpha+size truncated

Use the fixed-design beta score and its endpoint laws at beta zero and infinity. [bbinom truncated proof](#bbinom-all-free-truncated)

### bbinom alpha+size modified

Apply the hurdle factor to the held [conditional fit](#bbinom-alphasize-truncated). [bbinom modified proof](#bbinom-all-free-modified)

### bbinom alpha+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bbinom inflated proof](#bbinom-all-free-inflated)

### bbinom beta truncated

Only bounded-design alpha escapes compete. The growing-size tail vanishes. [bbinom truncated proof](#bbinom-all-free-truncated)

### bbinom beta modified

Apply the hurdle factor to the held [conditional fit](#bbinom-beta-truncated). [bbinom modified proof](#bbinom-all-free-modified)

### bbinom beta inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bbinom inflated proof](#bbinom-all-free-inflated)

### bbinom beta+size truncated

Compare the fixed-design alpha score with the rare-alpha law and upper atom. [bbinom truncated proof](#bbinom-all-free-truncated)

### bbinom beta+size modified

Apply the hurdle factor to the held [conditional fit](#bbinom-betasize-truncated). [bbinom modified proof](#bbinom-all-free-modified)

### bbinom beta+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bbinom inflated proof](#bbinom-all-free-inflated)

### bbinom size truncated

Compare both fixed-design scores with the rare-alpha, binomial and endpoint faces. [bbinom truncated proof](#bbinom-all-free-truncated)

### bbinom size modified

Apply the hurdle factor to the held [conditional fit](#bbinom-size-truncated). [bbinom modified proof](#bbinom-all-free-modified)

### bbinom size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bbinom inflated proof](#bbinom-all-free-inflated)

### bnbinom alpha truncated

Use both numerator scores. The swap is permitted. The bounded rare-shape faces remain. [bnbinom truncated proof](#bnbinom-all-free-truncated)

### bnbinom alpha modified

Apply the hurdle factor to the held [conditional fit](#bnbinom-alpha-truncated). [bnbinom modified proof](#bnbinom-all-free-modified)

### bnbinom alpha inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bnbinom inflated proof](#bnbinom-all-free-inflated)

### bnbinom alpha+beta truncated

Vary size alone and keep the bounded rare-size face at its zero endpoint. [bnbinom truncated proof](#bnbinom-all-free-truncated)

### bnbinom alpha+beta modified

Apply the hurdle factor to the held [conditional fit](#bnbinom-alphabeta-truncated). [bnbinom modified proof](#bnbinom-all-free-modified)

### bnbinom alpha+beta inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bnbinom inflated proof](#bnbinom-all-free-inflated)

### bnbinom alpha+beta+size truncated

Evaluate the supplied parameters. [bnbinom truncated proof](#bnbinom-all-free-truncated)

### bnbinom alpha+beta+size modified

Apply the hurdle factor to the held [conditional fit](#bnbinom-alphabetasize-truncated). [bnbinom modified proof](#bnbinom-all-free-modified)

### bnbinom alpha+beta+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bnbinom inflated proof](#bnbinom-all-free-inflated)

### bnbinom alpha+size truncated

Vary beta alone and keep the bounded rare-beta face at its zero endpoint. [bnbinom truncated proof](#bnbinom-all-free-truncated)

### bnbinom alpha+size modified

Apply the hurdle factor to the held [conditional fit](#bnbinom-alphasize-truncated). [bnbinom modified proof](#bnbinom-all-free-modified)

### bnbinom alpha+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bnbinom inflated proof](#bnbinom-all-free-inflated)

### bnbinom beta truncated

Apply the two-coordinate score under the beta hold. Retain allowed rare-size and scaled NB faces. [bnbinom truncated proof](#bnbinom-all-free-truncated)

### bnbinom beta modified

Apply the hurdle factor to the held [conditional fit](#bnbinom-beta-truncated). [bnbinom modified proof](#bnbinom-all-free-modified)

### bnbinom beta inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bnbinom inflated proof](#bnbinom-all-free-inflated)

### bnbinom beta+size truncated

Vary alpha alone. Its zero endpoint loses positive mass, and its infinite endpoint conditions to one. [bnbinom truncated proof](#bnbinom-all-free-truncated)

### bnbinom beta+size modified

Apply the hurdle factor to the held [conditional fit](#bnbinom-betasize-truncated). [bnbinom modified proof](#bnbinom-all-free-modified)

### bnbinom beta+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bnbinom inflated proof](#bnbinom-all-free-inflated)

### bnbinom size truncated

Apply the two-coordinate score under the size hold. Retain allowed rare-beta and scaled NB faces. [bnbinom truncated proof](#bnbinom-all-free-truncated)

### bnbinom size modified

Apply the hurdle factor to the held [conditional fit](#bnbinom-size-truncated). [bnbinom modified proof](#bnbinom-all-free-modified)

### bnbinom size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [bnbinom inflated proof](#bnbinom-all-free-inflated)

### cmb nu truncated

Compare the unique fixed-design probability solutions or endpoint decisions across supported sizes. [cmb truncated proof](#cmb-all-free-truncated)

### cmb nu modified

Apply the hurdle factor to the held [conditional fit](#cmb-nu-truncated). [cmb modified proof](#cmb-all-free-modified)

### cmb nu inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmb inflated proof](#cmb-all-free-inflated)

### cmb nu+prob truncated

Compare one evaluation per supported size, subject to compatibility with the held endpoint probability. [cmb truncated proof](#cmb-all-free-truncated)

### cmb nu+prob modified

Apply the hurdle factor to the held [conditional fit](#cmb-nuprob-truncated). [cmb modified proof](#cmb-all-free-modified)

### cmb nu+prob inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmb inflated proof](#cmb-all-free-inflated)

### cmb nu+prob+size truncated

Evaluate the supplied design and canonical coefficients. [cmb truncated proof](#cmb-all-free-truncated)

### cmb nu+prob+size modified

Apply the hurdle factor to the held [conditional fit](#cmb-nuprobsize-truncated). [cmb modified proof](#cmb-all-free-modified)

### cmb nu+prob+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmb inflated proof](#cmb-all-free-inflated)

### cmb nu+size truncated

Use the scalar probability chart or its endpoint decisions at the given size. [cmb truncated proof](#cmb-all-free-truncated)

### cmb nu+size modified

Apply the hurdle factor to the held [conditional fit](#cmb-nusize-truncated). [cmb modified proof](#cmb-all-free-modified)

### cmb nu+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmb inflated proof](#cmb-all-free-inflated)

### cmb prob truncated

Compute each fixed-odds dispersion optimum or extreme-statistic limit, then compare design heights. [cmb truncated proof](#cmb-all-free-truncated)

### cmb prob modified

Apply the hurdle factor to the held [conditional fit](#cmb-prob-truncated). [cmb modified proof](#cmb-all-free-modified)

### cmb prob inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmb inflated proof](#cmb-all-free-inflated)

### cmb prob+size truncated

Use the scalar dispersion moment or the extreme-statistic limit at the given odds and size. [cmb truncated proof](#cmb-all-free-truncated)

### cmb prob+size modified

Apply the hurdle factor to the held [conditional fit](#cmb-probsize-truncated). [cmb modified proof](#cmb-all-free-modified)

### cmb prob+size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmb inflated proof](#cmb-all-free-inflated)

### cmb size truncated

Use the positive-support hull, including the size-two rank drop and the size-one flat case. [cmb truncated proof](#cmb-all-free-truncated)

### cmb size modified

Apply the hurdle factor to the held [conditional fit](#cmb-size-truncated). [cmb modified proof](#cmb-all-free-modified)

### cmb size inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmb inflated proof](#cmb-all-free-inflated)

### cmp lambda truncated

Use the scalar dispersion moment with the geometric KKT condition when the held rate is below one. [cmp truncated proof](#cmp-all-free-truncated)

### cmp lambda modified

Apply the hurdle factor to the held [conditional fit](#cmp-lambda-truncated). [cmp modified proof](#cmp-all-free-modified)

### cmp lambda inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmp inflated proof](#cmp-all-free-inflated)

### cmp lambda+nu truncated

Evaluate the supplied law if the positive event is available. [cmp truncated proof](#cmp-all-free-truncated)

### cmp lambda+nu modified

Apply the hurdle factor to the held [conditional fit](#cmp-lambdanu-truncated). [cmp modified proof](#cmp-all-free-modified)

### cmp lambda+nu inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmp inflated proof](#cmp-all-free-inflated)

### cmp nu truncated

Use the scalar conditional mean with the rate domain determined by the held dispersion. [cmp truncated proof](#cmp-all-free-truncated)

### cmp nu modified

Apply the hurdle factor to the held [conditional fit](#cmp-nu-truncated). [cmp modified proof](#cmp-all-free-modified)

### cmp nu inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [cmp inflated proof](#cmp-all-free-inflated)

### dgamma rate truncated

Compare the right shape score at zero shape and use its strictly decreasing score. [dgamma truncated proof](#dgamma-all-free-truncated)

### dgamma rate modified

Apply the hurdle factor to the held [conditional fit](#dgamma-rate-truncated). [dgamma modified proof](#dgamma-all-free-modified)

### dgamma rate inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [dgamma inflated proof](#dgamma-all-free-inflated)

### dgamma rate+shape truncated

Evaluate the supplied gamma cell integrals. [dgamma truncated proof](#dgamma-all-free-truncated)

### dgamma rate+shape modified

Apply the hurdle factor to the held [conditional fit](#dgamma-rateshape-truncated). [dgamma modified proof](#dgamma-all-free-modified)

### dgamma rate+shape inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [dgamma inflated proof](#dgamma-all-free-inflated)

### dgamma shape truncated

Check the all-ones escape first. For other data bracket the rate, with strict concavity at shape at least one. [dgamma truncated proof](#dgamma-all-free-truncated)

### dgamma shape modified

Apply the hurdle factor to the held [conditional fit](#dgamma-shape-truncated). [dgamma modified proof](#dgamma-all-free-modified)

### dgamma shape inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [dgamma inflated proof](#dgamma-all-free-inflated)

### dweibull shape1 truncated

Hold t fixed and use strict conditional concavity in shape2. All ones selects the upper-shape limit. [dweibull truncated proof](#dweibull-all-free-truncated)

### dweibull shape1 modified

Apply the hurdle factor to the held [conditional fit](#dweibull-shape1-truncated). [dweibull modified proof](#dweibull-all-free-modified)

### dweibull shape1 inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [dweibull inflated proof](#dweibull-all-free-inflated)

### dweibull shape1+shape2 truncated

Evaluate the supplied conditional survival differences. [dweibull truncated proof](#dweibull-all-free-truncated)

### dweibull shape1+shape2 modified

Apply the hurdle factor to the held [conditional fit](#dweibull-shape1shape2-truncated). [dweibull modified proof](#dweibull-all-free-modified)

### dweibull shape1+shape2 inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [dweibull inflated proof](#dweibull-all-free-inflated)

### dweibull shape2 truncated

Hold beta fixed and use the strict rate score. All ones selects the upper-rate limit. [dweibull truncated proof](#dweibull-all-free-truncated)

### dweibull shape2 modified

Apply the hurdle factor to the held [conditional fit](#dweibull-shape2-truncated). [dweibull modified proof](#dweibull-all-free-modified)

### dweibull shape2 inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [dweibull inflated proof](#dweibull-all-free-inflated)

### pig mean truncated

Compare the two held-mean boundary values with the scalar Bessel score. All ones has the exact two-endpoint test. [pig truncated proof](#pig-all-free-truncated)

### pig mean modified

Apply the hurdle factor to the held [conditional fit](#pig-mean-truncated). [pig modified proof](#pig-all-free-modified)

### pig mean inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [pig inflated proof](#pig-all-free-inflated)

### pig mean+shape truncated

Evaluate the supplied Bessel law. Shape zero has no positive component. [pig truncated proof](#pig-all-free-truncated)

### pig mean+shape modified

Apply the hurdle factor to the held [conditional fit](#pig-meanshape-truncated). [pig modified proof](#pig-all-free-modified)

### pig mean+shape inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [pig inflated proof](#pig-all-free-inflated)

### pig shape truncated

At positive held shape, all ones selects the lower-mean limit. A strict inward comparison removes the upper-mean Lévy limit. [pig truncated proof](#pig-all-free-truncated)

### pig shape modified

Apply the hurdle factor to the held [conditional fit](#pig-shape-truncated). [pig modified proof](#pig-all-free-modified)

### pig shape inflated

Preserve the same parent holds in both branches and in the parent boundary fit. Evaluate directly when all are held. [pig inflated proof](#pig-all-free-inflated)
