# Attainment and maximizing sets across fourteen families

TASK-408 · Base revision `fc24c05b`

The results use the joint likelihood on the sample, with the dossier's endpoint conventions. An excluded limit can match a finite winner without changing the maximizing set. A finite candidate list gives an exact set by height comparisons, but in smooth families it may leave the number of equal winners unknown without an exact value comparison. The integer results below use the unbounded mathematical model. Where a count cap is imposed, intersect the feasible grids with that cap **before** maximizing. Intersect the displayed flat or saturated sets directly.

The [atlas](../existence-atlas/overview.md), its [Lean inventory](../existence-atlas/lean-map.csv), the [likelihood closure lemma](../ml-behaviour.tex#lem:closure), and the [tie](2026-11-25-ties-math-report.md) and [certificate](2026-11-25-uniq-cover-math-report.md) audits set the scope. Below, likelihoods are products unless written as log likelihoods. Samples are nonempty. Integer samples have nonnegative counts, and continuous samples have the support and holds specified for their family. Supremum comparisons on an empty feasible positive-likelihood domain are treated as incompatibility.

### bbinom held alpha

**Proved.** Put $a=\alpha>0$, $b=\beta$, $M=\max x_i$, and let $h$ be the sample size. At each size $N\ge M$ the fixed-size result below identifies the maximizing coefficient or endpoint. For all zeros the maximizing set is $N=0$ with all positive $b$. For positive constants $x_i=M$, $N=M$, $b\downarrow0$ reaches the upper atom of likelihood one. No finite feasible parameter gives likelihood one. The other competitive boundary, for nonconstant data, is the negative binomial of size $a$ and mean $\bar x$.

For exhaustiveness, bounded integer sizes have constant subsequences. At each such size, $b\downarrow0$ and $b\to\infty$ give the upper and lower atoms, respectively. For $N\to\infty$, take a subsequence with $b/N$ convergent in $[0,\infty]$. The small-beta bound $B(N,c)$ of the [held-alpha dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md#held-alpha-negative-binomial-face-and-uniform-size-bound) goes to minus infinity after $N\to\infty$ and then $c\downarrow0$ when a positive count occurs. At $b/N\to\infty$, $\Pr(X\ge1)\le Na/(a+b)\to0$. When $b/N\to c\in(0,\infty)$, the product ratio for the positive-count masses and the gamma ratio for the zero mass give the fixed-$a$ negative binomial of mean $a/c$. This proves the sample envelope along all competitive escapes, including the constants for which an atom or the attained size zero supplies height one. For nonconstant data the fixed-size boundaries have zero likelihood and the optimized negative-binomial face $F>0$ is reachable.

For all-zero data the bounded-size lower atom also has height one. At growing sizes, height one is reached exactly when $b/N\to\infty$. At finite positive $b/N$ the limiting NB zero mass is strictly below one, and at $b/N\to0$ the same dossier zero-mass bound gives zero regardless of the sample.

Let $L_N$ be the likelihood at the unique fixed-size positive root. On nonconstant data, attainment is equivalent to **some** $L_N\ge F$. If a strict witness exists, the dossier's uniform tail bound gives a finite cutoff. Compare all sizes up to that cutoff and retain every co-winner. If every $L_N\le F$, precisely the ones with $L_N=F$ attain, with their unique fixed-size coefficients. If every $L_N<F$, the only maximizing escape is the fitted negative binomial. This handles the equality case as a characterization, with an unbounded size quantifier in the absence of a strict witness. A mixed binary sample has a unique winner at size one with success probability equal to its positive count share, because higher sizes necessarily spend some mass outside the two observed categories and cannot attain the saturated Bernoulli height.

A cheap strict witness condition is $V<\bar x+\bar x^2/a$, with $V$ the empirical variance with divisor $h$. To see this, put $b=cN$ with $c=a/\bar x$. Divide each mass by its limiting negative-binomial mass. The gamma ratio expansion $\Gamma(z+a)/\Gamma(z)=z^a[1+a(a-1)/(2z)+O(z^{-2})]$ gives the zero-mass correction $a(a-1)/(2Nc(1+c))$ on the log scale. The finite product for the ratio of the $x$ mass to the zero mass contributes $[-x(x-1)+x(x+1)/(1+c)]/(2N)$. After averaging, the total log gain is
\[
\frac{c(\bar x+\bar x^2/a-V)}{2N(1+c)}+O(N^{-2}).
\]
For the opposite strict sign, the optimized size profile is eventually strictly below the face as well. The expansion is uniform for $b/N$ on compact subsets of $(0,\infty)$. Its leading negative-binomial log likelihood has a unique maximizer at $c$ and its next coefficient is continuous, so any $L_N\ge F$ at arbitrarily large $N$ would have $b/N\to c$ and a strictly negative $1/N$ correction. The zero coefficient and exact finite/face equality remain open for a terminating unconditional check.

### bbinom held alpha+size

**Proved.** When $N\ge1$, an attained fit is unique. For $N=0$ with compatible data, every positive free shape maximizes. For $N>0$ and data that are not constant at an endpoint, write $k_i=N-x_i$ and $B(t)=1-e^{-Nt}$. The beta score divided by $h$ is
\[
\int_0^\infty \frac{e^{-bt}}{1-e^{-t}}
 \left[\overline{1-e^{-k_i t}}-e^{-at}B(t)\right]dt .
\]
For $0<k<N$, $(1-e^{-kt})/(1-e^{-Nt})$ is increasing in $t$ since its log derivative is $k/(e^{kt}-1)-N/(e^{Nt}-1)>0$. For $k=0,N$ the quotient is constant. Thus $e^{at}\overline{1-e^{-k_i t}}/B(t)$ is strictly increasing, starts at $\bar k/N<1$, and tends to infinity. The kernel changes sign once from negative to positive. At a zero of the score, replacing $b$ by a larger value tilts the integrand by an exponential strictly decreasing in $t$. Relative to the factor at the sign change, this tilt makes the integral strictly negative. A smaller value makes it strictly positive. The endpoint cases have no finite fit by the [fixed-size existence proof](2026-11-25-bbinom-math-report.md#held-shapes). In the remaining case existence supplies one score root, which the tilt proves unique.

### bbinom held beta

**Proved.** For $b>0$, all zeros attain exactly at $N=0$ with arbitrary positive $\alpha$. Positive constants do not attain and escape to the upper atom at $N=M$ as $\alpha\to\infty$. Every nonconstant sample attains, and the entire set consists of the unique fixed-size roots at the sizes attaining the best height in the following finite list.

At bounded sizes, the two free-shape endpoints are the two atoms. At growing sizes, the [uniform bound in the dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md) gives zero likelihood whenever any count is positive. For all zeros at growing sizes more generally, $P_0=\prod_{j=0}^{N-1}(1+a/(b+j))^{-1}$. Its limiting supremum one requires $a\log N\to0$, which also suffices. Indeed $a\log N\to0$ implies $a\to0$ and the sum of the logarithms is asymptotic to $a\log N$ (for bounded positive $b$ fixed). Conversely the product tending to one first forces $a\to0$ by its first factor. This is an escape tied with attainment at size zero. For $N\ge1$ bounded the lower atom has the same value on all-zero data.

Write $u=\#\{i:x_i>0\}$ and $H_N=\sum_{j=0}^N(b+j)^{-1}$. The ordinary alpha score at $N+1$ is bounded below by $u/a-hH_N$ because of the $j=0$ terms on the positive counts. Thus its root is at least $u/(hH_N)$. The successive-size mass ratio is
\[
\frac{N+1}{N+1-x}\frac{N-x+b}{N+a+b}.
\]
For $b\le1$ it is strictly less than one, so the maximizing size on nonconstant data is $M$. For $b>1$, choose the first $N_0\ge M$ such that
\[
 u(N_0+1-M)>hM(b-1)H_{N_0}.
\]
The ratio $(N+1-M)/H_N$ strictly increases: $H_N>(N+1-M)/(b+N+1)$. It tends to infinity. Beyond this cutoff the mass ratios at the $N+1$ optimizer are strictly below one on every observation. The profiles are strictly decreasing from $N_0$ onward, so compare precisely the fixed-size optima on $[M,N_0]$. Scores and heights are rational functions in the free coefficient, so for algebraic $b$ the comparisons, including equality, can use real algebraic root isolation.

**Exact nonconstant tie.** Use $(7,8,13)$. At $b=1$ size 13 strictly wins by the preceding ratio. As $b\to\infty$ at fixed size, the profiles converge to the binomial profiles: reparametrize by the beta mixing mean $p=a/(a+b)$; the mixing variance is at most $1/[4(b+1)]$. The binomial mass at fixed size is Lipschitz as a function of its probability on $[0,1]$, giving uniform convergence, including at both ends. The optimized binomial likelihood at size 38 is strictly greater than at size 13 by direct rational comparison at $p=28/(3N)$. Take $B$ to be the first integer $b>1$ for which the size-38 profile beats size 13. A common cutoff on $[1,B]$ follows from the strict inequality above with $(b-1)H_N\le(B-1)\sum_{j=0}^N(1+j)^{-1}$ and can be taken at least 38. Each fixed-size optimum depends continuously on $b$ (the kernel tilt makes the score derivative strictly negative at its root). Take the first $b_*$ in $[1,B]$ at which the best other size equals the size-13 height. This specifies an exact held value, strictly larger than 1, and at least two finite global maximizers. The profile comparisons at a proposed adjacent or nonadjacent tie use the same algebraic heights. This construction does not assert that the only tied sizes are adjacent.

### bbinom held beta+size

**Proved.** Apply the fixed-size alpha-held proof to $N-x_i$ with $a$ and $b$ exchanged. At positive size the singleton conclusion holds whenever a maximum exists. At size zero the entire free-shape ray maximizes, and a constant at a positive-size endpoint has only its excluded atom. The reflection holds within a size, since the reflected sample itself changes when $N$ changes.

### bnbinom held alpha

**Refuted.** Hold $\alpha=2$ and use the exact 500 integer observations under the `data` key of [golden.json](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json). The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#scalar-certificates) proves compactness of positive superlevels for a sample containing a positive count, and the mass is symmetric in size $r$ and beta $b$. Every diagonal point $(r,b)=(s,s)$ is strictly worse than $(r,b)=(1,5)$. This gives at least two labeled global maximizers by the exact swap; it does not count the number of winning orbits.

The [earlier diagonal cover](2026-11-06-bnbinom-cert-data/nonuniqueness.py) used high-precision numerical comparisons. The [new rational checker](2026-11-27-uniq-atlas-math-data/exact_checks.py) checks strict exclusion with outward arithmetic. At the witness $p_x=60/[(x+5)(x+6)(x+7)]$. On $[l,h]$ the diagonal mass satisfies $p_x(s,s)\le p_0(l,l)R_x(h)$, where
\[
p_0(s,s)=\Gamma(s+2)^2/\Gamma(2s+2),\qquad
R_x(s)=(s)_x^2/[(2+2s)_x x!].
\]
The zero mass decreases by monotonicity of the digamma, and each ratio factor increases by differentiation. Below $10^{-6}$ use $p_x(s,s)\le (s)_x/x!$ for positive counts and one for zeros. Above 256, the diagonal count is stochastically increasing: in the beta-NB mixture, increasing either size at fixed mixing probability or the second beta shape at fixed first shape increases counts. Thus every observation mass is bounded by $\Pr_{256,2,256}(X\le \max x_i)$. Both exterior comparisons and every rational subdivision comparison use the same strict witness bound. The checker uses the log atanh series on $[1,2]$ with a geometric remainder, and recurrence to arguments at least 16 followed by the enveloping Stirling bounds through $1/(12z)$, with the lower correction $-1/(360z^3)$. This supplies an integer-arithmetic verification of the strict inequalities, independent of a floating-point tie margin.

### bnbinom held beta

**Proved.** Fix $b>0$ and regard the original count law, by the $r,b$ swap, as $\mathrm{NB}(b,1-q)$ mixed over $q\sim\mathrm{Beta}(r,\alpha)$. This follows directly from the beta integral, and the mixing variable lies strictly inside $(0,1)$. Every compactifying sequence outside the finite parameter domain has, after subselection, one of the following limits for the sample masses.

- If $r\to0$, each positive mass tends to zero since $p_x\le (r)_x/x!$ for $x>0$. If $\alpha$ is bounded below, $q$ tends to zero and the limit is the zero atom. If $\alpha\to0$ too, take $r/(r+\alpha)\to q_*$; the mixture concentrates on the two endpoints with probabilities $(1-q_*,q_*)$. In fact $E[q(1-q)]\to0$ and $E q\to q_*$. This permits defective finite-count limits, whose only finite mass is $1-q_*$ at zero.
- If $\alpha\to0$ and $r$ stays bounded below, $q\to1$ in probability by its mean, and all finite masses vanish. If $r+\alpha\to\infty$, the variance is at most $1/[4(r+\alpha+1)]$, so $q$ concentrates at the limiting ratio $q_*$. This gives the zero atom at $q_*=0$, total escape from finite counts at $q_*=1$, and fixed-size negative binomial in between.

The negative-binomial mass with size $b$ is continuous in $q$ on the closed interval with these endpoint interpretations. Thus this is an exhaustive calculation of the joint sample envelope. All-zero data have supremum one by the zero atom and give no finite maximum. With a positive count the largest face is the fixed-size NB at $q_0=\bar x/(b+\bar x)$, obtained by differentiating $h b\log(1-q)+\sum x_i\log q$. It is reachable by diverging $r+\alpha$ in that ratio. Denote its likelihood by $F$.

For a positive count, solve the unique conditional $r$ score root at every $\alpha$, using the [scalar crossing proof](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#scalar-certificates). At that root, the $r$ derivative of the score is strictly negative: in the score integral, subtract the kernel's sign-change point from the multiplier $-t$, using the zero score. The resulting integral has a strictly negative sign on both sides. The profile is therefore analytic. It attains iff at least one of its finite stationary points has height at least $F$. A strict improvement confines the maximizing superlevel to a compact subset of the quadrant; in the absence of a strict improvement a finite tie itself attains. If every stationary point loses, the fitted NB is the only maximizing escape. The whole finite maximizer set is the conditional root above each profile coordinate with top height meeting $F$.

Here are sufficient conditions in both directions. At mixture precision $T=r+\alpha$ with mean $q_0$, the first log-likelihood correction per datum averaged over the sample is
\[
 \frac{q_0(1-q_0)}{2(T+1)}
 \overline{\frac{g_{x_i}''(q_0)}{g_{x_i}(q_0)}},\qquad
 g_x(q)=\frac{(b)_x}{x!}(1-q)^bq^x.
\]
Taylor expansion about $q_0$ gives remainder $o(T^{-1})$: $E(q-q_0)^4=O(T^{-2})$ by the beta moment formulas, so the contribution away from any fixed neighborhood is $O(T^{-2})$, and the third absolute centered moment is $O(T^{-3/2})$. At the fitted mean the averaged ratio equals $(V-\bar x-\bar x^2/b)/q_0^2$, giving a strict finite witness for overdispersion relative to that NB. The same expansion is uniform as the beta mean varies in a compact neighborhood of $q_0$. For underdispersion, a sequence of escaping points with height at least $F$ would necessarily have $T\to\infty$ and beta means tending to $q_0$ by the face classification. Its negative correction and the leading NB height at most $F$ contradict that height bound.

**Proved finite cardinality away from the face seam.** If there is a strict finite witness, keep the profile at and above its height. For underdispersion, keep it at and above $F$. In either case the preceding arguments confine the relevant finite parameter points to a compact subset of the quadrant. The analytic profile is nonconstant: as $\alpha\downarrow0$ every sequence of positive-count likelihoods tends to zero by the closure calculation, even after profiling. Its stationary roots in the compact projected interval form a finite set. Comparing their heights gives all finite co-winners in these cases. This leaves universal singleton status and the equidispersed case with no strict witness open.

For a sample-checkable escape condition, if $\sum_i g_{x_i}(q)/g_{x_i}(q_0)\le h$ on $(0,1)$, the inequality is strict except at isolated points since the left side is analytic and tends to zero at 1. Integrate against the positive beta density and apply $\log z\le z-1$ to each mass ratio to get a strict global loss at every finite point. For positive constants the condition holds directly from the unique mode of $g_x$ in $q$. The predicate can be checked by a polynomial-root comparison in $q$ followed by evaluation of $(1-q)^b$ at the finite candidates. In fact the sum has the form $(1-q)^b P(q)$ with $P$ polynomial, so its critical points satisfy $(1-q)P'-bP=0$.

### bnbinom held size

**Proved.** Exchanging the NB size and the second beta shape is an exact involution of the probability mass for every count. It exchanges this mask with held beta at the same positive value, and sends the free size above to free beta. The compactification, optimized fixed-size NB face, stationary-value criterion with equality, finite cardinality on the stated branches, and sufficient conditions therefore apply verbatim with the held size in place of $b$.

### nhyper held all free

**Proved.** Write $t_i$ for total draws. All zeros attain exactly at $r=0$ with any feasible population and composition. Mixed zeros and positives are infeasible. At constant $c>0$, the complete maximizing set is $(N,m,r)=(N,N,c)$ for integers $N\ge c$. For $r>0$, the support runs from $r$ to $N-m+r$, and all its masses are positive. Consequently a mass of one at $c$ requires $N=m$ and $r=c$, and these values suffice.

For positive nonconstant data, $r$ ranges over $1,\ldots,\min t_i$. At each such $r$ the [fixed-stopping-count analysis](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape) computes a finite test against its optimized face. Specifically, its residue-class polynomial comparisons show every sufficiently large population profile strictly below or strictly above its face, with a computable cutoff, and its uniform tail bound cuts off all larger populations against a strict finite witness. If it ties the face at an attained point and none beats it, all maximizers occur in the finite prefix. The report's rule excludes an identically zero face polynomial using the multiplicity of the falling-factorial roots. Its assumptions $r>0,S=\sum_i(t_i-r)>0$ hold for every $r$ in this mask on nonconstant data. Evaluate each of the finite number of slice outcomes with the mass factors $\prod_i\binom{t_i-1}{r-1}$ restored. Take the largest height among all slice suprema and retain all finite co-winners from the slices at that height. If the highest value has no finite co-winner, it is reached on exactly the winning shifted-NB slices.

For exhaustiveness, along an escaping subsequence $r$ is eventually constant and $N\to\infty$. If $m/N\to p$, the falling factorial mass gives the shifted NB for $0<p<1$, the deterministic draw count $r$ for $p=1$, and zero at all finite counts for $p=0$. Each interior proportion is reachable by feasible integers. On positive nonconstant data both endpoints have zero sample likelihood, so they cannot win. For positive constants the winning escapes are $p\to1$ with $r=c$; an interior NB has mass strictly below one. For zeros feasibility forces $r=0$, and every feasible sequence has the constant sample mass one. This covers equality between any finite and face slices, including when their stopping counts differ.

### nhyper held N

**Proved.** At given $N$ compare at most two feasible compositions per strictly feasible positive $r\le\min t_i$. Feasible compositions form the integer interval $r\le m\le N-\max_i(t_i-r)$. The fixed-$r$ adjacent rule below gives the entire maximizing set on each line. Compare those line heights as rationals and retain every co-winner; for all zeros the full set is $r=0$, $0\le m\le N$. Integer product equality between different $r$ values can be decided after reinstating their binomial factors.

### nhyper held N+m

**Proved.** For positive data, feasible $r$ run from $\max(1,\max t_i-(N-m))$ to $\min(m,\min t_i)$. Across feasible adjacent pairs the likelihood ratio is
\[
\prod_i \frac{(t_i-r)(m-r)}{r(N-m-t_i+r+1)}.
\]
This follows from the two binomial coefficients $\binom{t_i-1}{r-1}\binom{N-t_i}{m-r}$ in the mass. Every factor strictly decreases with $r$ on feasible adjacent pairs, since each positive numerator factor decreases and each denominator factor increases. The first ratio at most one gives the smaller maximizing $r$; include its successor exactly on product equality. If every ratio exceeds one use the last feasible $r$. All zeros force $r=0$ and give a singleton with these holds.

### nhyper held N+r

**Proved.** For $r>0$, over the composition interval stated above the ratio for incrementing $m$ at fixed $N$ is
\[
 \prod_i \frac{m+1}{m+1-r}\,\frac{N-m-(t_i-r)}{N-m}.
\]
Each second factor is weakly decreasing and the first is strictly decreasing. This gives the first-crossing set, including the terminal case, and at most two adjacent maximizing compositions. Integer product equality at the crossing determines the tie. For $r=0$ the only feasible data are all zeros, and all compositions maximize. The same composition crossing supplies the slice step in the [prior tail proof](2026-11-25-nhyper-math-report.md#an-exact-finite-test-including-equality-and-escape).

### nhyper held m

**Proved.** For positive data take $1\le r\le\min(m,\min t_i)$ and apply the [held-white single crossing](2026-11-25-nhyper-math-report.md#white-count-held) to each line $N\ge m+\max t_i-r$. The [Lean proof](2026-11-25-lean-nhyper-report.md#held-white-count) verifies the integer ratio and the strict ordering at the crossing. Each line gives one winning population or two adjacent co-winners when the ratio equals one. Restore the mass constants before comparing across $r$ and retain all ties. All zeros give $r=0$ and every $N\ge m$. At positive $r$ the line bound is finite, with the first nonpositive log ratio by $N=\max(m+\max t_i-r,\lceil m\max t_i/r\rceil-1)$. For example, at $(m,r)=(5,2)$, $t=(2,5)$ gives line maximizers $N=9,10$ from factors $4/5$ and $5/4$.

### hyper held all free

**Proved on constants.** A hypergeometric support has extreme points $\max(0,m+k-N)$ and $\min(m,k)$. For constant $c>0$, equality of these two endpoints at $c$ holds iff $m=N,k=c$ or $k=N,m=c$, with $N\ge c$. For $c=0$ the entire saturated set has $m=0$ or $k=0$. This includes all degenerate bounds. For nonconstant samples the dossier's strict witness above every binomial face and the Poisson face gives a finite search box containing all finite winners. In that scope the exact set is computed by rational comparisons on that box, using the [proved adjacent composition test](2026-11-26-lean-hyper-report.md#adjacent-ratios-and-equality) at each fixed $(N,k)$. The open branch is the nonconstant sample when the maximum only reaches a face height, or no strict witness or strict exclusion is known. The two-held or one-held tail results cannot be applied uniformly as both $m$ and $k$ grow.

### hyper held k

**Proved.** The following exact comparison gives the full maximizing set, including equality with the binomial face, for fixed $k$ on the unbounded domain. For $k=0$ all feasible zero-sample parameters maximize. With $k>0$, $x_i\in[0,k]$ is necessary and sufficient for feasibility. If $\bar x/k=0$ or 1 the sample is constant at the corresponding endpoint. In these cases the maximizing sets have $m=0$ and $m=N$, respectively, for every $N\ge k$.

For $p=\bar x/k\in(0,1)$, the optimized face likelihood $F$ is the product of the binomial masses at $p$. For $N\to\infty$ the falling-factorial formula has the binomial limit at any convergent $m/N$. The [dossier bound](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md#r-notes) is uniform in $m$, since numerator falling factorials are bounded by powers and the denominator correction tends to one. Any strict finite witness therefore gives a finite upper cutoff for all maximizers.

To settle equality and decide whether a witness exists, write $p=a/d$ in lowest terms, $S=\sum_i x_i$, and $T=hk-S$. For large $N$ and real $q=m/N$ near $p$, the slice log likelihood is a data-only constant plus
\[
f(q)+G(q)/N+O(N^{-2}),\quad f(q)=S\log q+T\log(1-q),
\quad
G(q)=\frac12\sum_i\left[k(k-1)-\frac{x_i(x_i-1)}q-\frac{(k-x_i)(k-x_i-1)}{1-q}\right].
\]
The expansion follows term by term from the log of the falling-factorial mass. With $m=pN+z$ for fixed $z$, expanding through order $N^{-2}$ gives the same additional $J(p)/N^2$ in both adjacent heights; the coefficient of their difference is $G'(p)+f''(p)(z+1/2)$. Put $I=-f''(p)>0$. In residue $e$ of $N$ modulo $d$, put $s_e=\{pe\}$ and $j_e=\lfloor G'(p)/I+s_e-1/2\rfloor$. The adjacent ratios at offsets $j_e-1$ and $j_e+1$ from $\lfloor pN\rfloor$ eventually have strict signs $+,-$ relative to one. Each log falling-factorial summand is concave in real $m$ on the positive support interval. The likelihood ratio hence decreases strictly (at least one log factor is strictly concave), so only $j_e,j_e+1$ can maximize a tail slice.

Clear the positive denominators of the two bracketing ratio comparisons and of the differences of the two candidate likelihoods from $F$ after substituting $dm=aN+d(j-s_e)$. These are polynomials in $N$ over the rationals, since all counts, offsets and binomial probabilities are rational. Take an integer cutoff beyond every real root of their nonzero members and large enough for all four offsets to be feasible. The bracketing polynomials have the strict signs just proved. Each face comparison is then specified by its leading sign or identically zero. The exact full-set algorithm is as follows.

1. Inspect the finite prefix and the signs of the two tail comparisons in each residue. If any prefix winner or tail sign gives strict improvement over $F$, take such a witness and use the uniform binomial bound to finish the finite search.
2. Otherwise retain precisely prefix candidates at height $F$ and the offset candidates on any identically zero face polynomials in their corresponding tail residues. If this set is empty the face wins strictly. Otherwise it is the entire finite maximizer set, including any infinite rays on the integer lattice.

An identically zero comparison cannot in general be discarded. At $k=1$ with both outcomes present, the objective depends only on $m/N$, and all exact representations of $p$ attain the binomial height. At a positive constant $c<k$, there is a saturated fit at $(m,N)=(c,k)$, and the deterministic-support condition above excludes every other pair.

### hyper held m

**Proved.** The mass is symmetric in $m,k$ as a function of the observation, since $\binom{m}{x}\binom{N-m}{k-x}/\binom Nk=\binom{k}{x}\binom{N-k}{m-x}/\binom Nm$. Thus the entire held-$k$ finite-prefix and residue procedure, including the endpoint flat sets and lattice rays, transfers after exchanging the design counts.

### triang held a

**Proved full set.** Every positive maximizing mode is an observed knot. At fixed endpoints, between knots the mode-dependent terms in the log likelihood are $-h_L\log(c-a)-h_R\log(b-c)$ plus constants, strictly convex wherever the likelihood is positive. Outside the observation range moving toward the range improves strictly. If an observation equals $a$, positivity forces $c=a$. This is the same argument as the [Lean fixed-endpoint proof](2026-11-25-lean-triang-report.md#2-attainment-the-maximizer-set-and-the-rank-criterion), applied in each fixed-endpoint slice. At each feasible knot, the held-$a+c$ argument below gives its unique upper endpoint on the attained branch, so keep all knots with the greatest likelihood.

**Refuted singleton.** Hold $a=0$ and use $x=(1,t_*)$. Let $B$ be the smallest root in $(2,20)$ of
\[
 (3B-2)^4=B^2(2B-1)^3,\qquad t_*=\frac{B(2B-1)}{3B-2}.
\]
The polynomial has opposite signs at 2 and 20, and $1<t_*<B$. At $c=1$ the likelihood is $4(b-t_*)/[b^2(b-1)]$ for $b>t_*$. Its derivative sign is that of $-2b^2+(3t_*+1)b-2t_*$, positive at $b=t_*$ and negative eventually, with exactly one root above $t_*$. That root is $B$. At $c=t_*$ the likelihood is $4/(b^2t_*)$, uniquely maximized at $b=t_*$. Substitution of $t_*$ reduces equality of the two heights exactly to the displayed polynomial. There are exactly two maximizers because there are only two observation knots. This is an algebraic real tie with an integer held endpoint.

### triang held a+c

**Proved.** With $u=c-a$ fixed and $v=b-c$, put $d_i=c-x_i$ for observations below $c$ and $e_i=x_i-c$ for observations above $c$. The positive likelihood, with constant factors suppressed, is
\[
 (u+v)^{-h}\prod_{x_i<c}(1-d_i/u)\prod_{x_i>c}(1-e_i/v).
\]
For $u=0$, the empty left product is one. If a right distance is positive then $\log(1-e_i e^{-w})$ is strictly concave in $w=\log v$ on its positive domain. The derivative is $e_i e^{-w}/(1-e_i e^{-w})$ and strictly decreases. The width term $-h\log(u+e^w)$ is concave. Thus the optimum, whose existence follows from the [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md), is unique. If no right distance is positive and $u>0$, the likelihood strictly decreases in $v$ and the unique optimum is $v=0$. If $u=0$ as well, every compatible observation is at $c=a$ and width collapse is unbounded.

### triang held b

**Proved.** Reflect $(x,a,b,c)$ to $(-x,-b,-a,-c)$. The held-$a$ knot rule gives the entire set. In particular $b=0$ and $x=(-t_*,-1)$ has exactly the two reflected maximizers.

### triang held b+c

**Proved.** Reflection transfers the held-$a+c$ singleton proof on the attained branch. It exchanges the right and left distances and preserves the endpoint-mode convention.

### triang held c

**Proved.** If there are positive distances on both sides, use $w=\log u$ and $z=\log v$. In the positive-support domain each side's factor provides strictly negative curvature in its own log-width coordinate. The additional term $-h\log(e^w+e^z)$ is concave, so the joint log likelihood is strictly concave. If exactly one side has positive distances, the other width can only reduce the likelihood, so set it to zero and apply the one-dimensional strict concavity in the occupied log width. If neither side has positive distances, width collapse is unbounded. Thus every compatible attained fit is unique.

### gengamma held all free

**Refuted by dyadic data.** Use $(1/4,(1/2)^{[10000]},2^{[10000]},4)$, where brackets denote multiplicities. In log-base-two units the data are symmetric about zero with variance $s^2=10004/10001$ and maximum 2. The [signed profile reduction](2026-11-10-gg-theory-report.md#free-fit-signed-profile-and-certificate) gives on each side the optimized power or Pareto face value $-1-\log 2$, below the fitted normal value $-\frac12\log(2\pi e s^2)$ because $2>s\sqrt{2\pi/e}$. Take a positive Stacy log-power $p=\log3$ in these units and gamma shape $k=1$. After optimizing log location its value is $\log\log3-1-\log A$, where
\[
 A=\frac{10000(5/3)+41/9}{10001}.
\]
This exceeds the fitted normal if $(\log3)\sqrt{2\pi/e}>A$, a strict inequality verified by the [rational checker](2026-11-27-uniq-atlas-math-data/exact_checks.py). In particular $\log3>1.0986$, $\sqrt{2\pi/e}>1.520$, and $A<1.6670$. Both side profiles are continuous up to the normal seam and converge to their respective excluded face heights. Thus a global maximum exists at nonzero signed shape, and reflection about log location zero supplies a distinct global maximizer. The Jacobian for log coordinates is data-only. The proof leaves the number of reflected orbits to the finite candidate comparison below.

**Proved full set by a finite comparison.** On nonconstant data use the whole compact stationary list on each signed side from the [transformed-gamma root theorem](2026-11-25-trgamma-math-report.md#signed-exteriors-and-finite-root-count), together with the attained lognormal fit. The previous theorem's near-normal signs include the exactly balanced case, and exclude roots accumulating at zero on either side. Take the maximum of these finite candidates and the two face values, and retain every finite candidate at that level. The [balanced two-point Lean result](2026-11-26-gengamma-twopoint-report.md#proof-and-implementation) is consistent with this construction: that special sample loses strictly to the two faces. Constants instead allow unbounded scale collapse.

### gengamma held mu

**Proved full set by a finite comparison.** For nonzero residuals $z_i=\log x_i-\mu$ (meaning the vector is not identically zero), set $s^2=\overline{z_i^2}$. Work in units $z_i/s$ and consider each orientation separately. In the positive orientation put $J(t)=\overline{e^{t z_i/s}-1-t z_i/s}$. The [held-mode reduction](2026-11-25-gbshape-math-report.md#generalized-gamma) profiles gamma shape at the unique $k>0$ with $f(k)=\log k-\psi(k)=J(t)$ and gives $L(t)=\log t+B(k)-kJ(t)$, $B(k)=k\log k-k-\log\Gamma(k)$. The map $Q=1/\sqrt k,\ \sigma=Qs/t$ is bijective before profiling. The normal candidate at the seam is $(Q,\sigma)=(0,s)$. The opposite orientation uses the negated residuals and negative $Q$.

The score sign is that of $J-g(tJ')$ where $g(w)=f(1/w)$ increases and $tJ'>0$. With $m_j=\overline{(z_i/s)^j}$, the near-zero expansion using $g(w)=w/2+w^2/12+O(w^4)$ gives
\[
 J-g(tJ')=-m_3t^3/12-(m_4+2)t^4/24+O(t^5).
\]
Thus the score has a strict sign in a collar of zero. At infinity, if the maximum $M$ of the oriented residuals in these units is positive, $J\to\infty$, $J'/J\to M$ and $kJ\to1$ by the digamma recurrence at zero. Thus $tL'=1-ktJ'<0$ eventually and the profile tends to minus infinity by the prior held-mode bound. If $M\le0$, put $d=-\overline{z_i/s}>0$. Then $J\ge td-1$ and $tJ'\le td$. Digamma recurrence gives $f(1/(td))<td-\log(td)+\gamma_E$, so $k<1/(td)$ for $td>e^{1+\gamma_E}$. Consequently $L'>0$ on that tail. Its limit is the endpoint trace $-1-\log d$ because $J/(td)\to1,\ kJ\to1$ and $B(k)=\log k+o(1)$. This includes residuals exactly zero at the endpoint. Therefore every retained stationary list is compactly contained in $(0,\infty)$ and finite by real analyticity and the strict tail sign. Compare the normal and both finite lists against the available faces and retain all finite co-winners. All residuals zero give unbounded scale collapse.

**Proved singleton on a symmetric residual sample.** For exact sign symmetry and at least one nonzero residual, $\overline{e^{t z_i}-1-tz_i}\ge t^2\overline{z_i^2}/2$. In physical log units a fixed nonzero $Q$ therefore has mean log likelihood strictly below the normal expression at the same $\sigma$. The gap is at least $R(Q^{-2})$, the positive Stirling remainder. The unique best scale at the seam is the RMS residual. For arbitrary residuals the comparison rule gives the set but does not decide whether two distinct finite critical heights can tie.

### gengamma held sigma

**Refuted by dyadic data and hold.** Use $(1/4,4)$ and $\sigma=1$. At every $Q$ the location is unique by the [held-scale profile](2026-11-10-gg-theory-report.md#sigma-held-mu-and-q-free). Put $d=\log4$. After profiling location, the change from the $Q=0$ value is
\[
 -R(Q^{-2})-\frac{\log\cosh(dQ)}{Q^2}+\frac{d^2}{2}
 =\frac{d^4-1}{12}Q^2+O(Q^4).
\]
Stirling's remainder is $R(Q^{-2})=Q^2/12+O(Q^6)$, and the fourth Taylor coefficient of $\log\cosh$ is $-1/12$. Since $d>1$, nonzero small shapes improve. The uniform mode-density bound excludes both tails, so there is a global maximizing nonzero shape. Exact symmetry makes the profile even and supplies its distinct reflected maximizer at the same held scale. For constants, strict positivity of $R$ gives the normal as the unique fit. General cardinality requires comparing the other finite candidates and the seam, including possible higher-order seam contacts.

### bhatt held mu

**Refuted by an existing exact real sample.** The [normal/positive-width crossing](2026-11-25-bhatt-math-report.md#all-free-normal-and-positive-width-global-maxima-proved) explicitly includes $\mu=0$ held. Use one observation at each of $\pm1024$ and $2^{20}-1$ at each of $\pm t_{\rm N}$, where $t_{\rm N}$ is the first zero of the maximum positive-width gain outside the strictly losing normal collar as defined in that proof. The strict uniform-face loss, the common normal collar and the change of sign between $t=0$ and $t=10$ prove equality with a distinct finite positive-width maximum. Symmetry identifies location zero at each scale-width pair by strict location concavity, so the same crossing remains global under this hold.

The [finite-width theorem](2026-11-25-bhatt-math-report.md#finite-stationary-width-criterion-proved) applies to this hold on all data with at least one nonzero residual. At each $k=a/\sigma$ there is exactly one nuisance fit, including the normal at $k=0$. The strict tail above $k=2n$ lies below the uniform height at held centre. The analytic continuation through zero and nonconstancy give a finite stationary list on this compact interval. The full maximizing set consists of the normal and stationary candidates at the largest finite height, provided it reaches or exceeds the uniform height. All-zero residuals instead collapse.

### expweibull held scale

**Proved full set by a finite comparison.** In the notation of the [held-scale proof](2026-11-25-expweibull-math-report.md#what-this-changes-for-the-fitter), write $d_i=\log(x_i/a)$ for held scale $a$, $u_i=e^{bd_i}$, $Q(u)=1-e^{-u}$ and $H(b)=\overline{-\log Q(u_i)}$. At fixed $b$ the unique exponent solves $c=1/H(b)$ by strict concavity. The gain profile in log-observation units, up to common constants, is $\log b-1-\log H+\overline{\log[u_i/(e^{u_i}-1)]}$. On the attained branch (nonconstant with some $x_i>a$), the explicit end bounds in the prior report give a compact shape superlevel for any finite witness. The profile is analytic and nonconstant because its height tends to minus infinity at both ends of that branch. Its derivative has finitely many zeros on a closed neighborhood of the compact superlevel, by the identity theorem. Take precisely all the stationary shapes at the best height and reconstruct the exponent above each one. This proof needs no grid or effective o-minimal root bound. The sign or height inequality that would reduce the closed candidate list to one on every attained sample is open; the earlier all-free crossing uses a movable scale and supplies no held-scale counterexample.

### invburr held shape2

**Refuted over the reals, with a proved singleton branch.** Hold power $\gamma=\mathrm{shape2}>0$ and write $y_i=x_i^{-\gamma}$. Taking the reciprocal observation and then its $\gamma$ power adds a data-only log Jacobian, and the Lomax shape is $\mathrm{shape1}$ and rate is $\mathrm{scale}^{\gamma}$. This is a bijection, so it preserves every maximizer and every multiplicity. The [Lomax two-support proof and exact crossing](2026-11-25-lomax-uniq-math-report.md#exact-tie-construction) give a singleton whenever attained on at most two positive support points, and exactly two maxima for $\gamma=1$ and the exact sample $(1/a_*,1^{[10]},(1/100)^{[10]})$ with $a_*$ from that proof. The two maxima strictly beat the inverse-Weibull face. They can be recovered by transforming the winning Lomax rates. The construction places no arithmetic restriction on $a_*$.

### invtrgamma held all free

**Proved full set.** Reciprocation changes the likelihood by a data-only term and maps public scale to its reciprocal. The [positive signed profile](2026-11-25-trgamma-math-report.md#signed-exteriors-and-finite-root-count) has a finite stationary candidate list on every nonconstant sample, with strict score signs in both omitted collars. It specifies the entire maximizing set as the retained roots tying the largest finite height whenever that height reaches or exceeds the larger face. The normal end corresponds to lognormal and the power end to Pareto. An exact tie with Pareto already occurs in the prior report, but the face itself adds no finite maximizer.

**Refuted by a finite/finite crossing.** Use the prior [finite/power crossing path](2026-11-25-trgamma-math-report.md#exact-finitepower-equality-over-the-reals). Let $Y_h$ be its twelve base-two log levels, so for $h\in[0,1]$ they are $(-(2+h),2,(-2,2)^{[5]})$, and for $h\in[1,3]$ they are $(-3,2,(-r,r)^{[5]})$ with $r=3-h$. In these fixed log units let $L_h(p)$ be the transformed-gamma mean log likelihood profiled at power $p>0$, with normal value $N_h=L_h(0)$ and power face $P_h$. In the prior proof there is a rational $h_0$ before face equality with $L_{h_0}(p_0)>P_{h_0}$ for some rational $p_0>0$, and $P_h>N_h$ for all $h$ in $[0,h_0]$. Existence of rational choices follows from strictness and continuity. At $h=0$ every finite value is strictly below $P_0$ by the balanced two-point theorem. Specify $h_0,p_0$ by the first such pair, ordering reduced positive rational pairs by the sum of numerators and denominators, then lexicographically by their numerator and denominator tuples.

Replicate the twelve levels $m$ times and append **one** observation at log level 100. Use a tilde for the new profile and faces, and put $\epsilon=1/(12m+1)$. The inverse transformed-gamma sample is obtained by raising 2 to the negatives of the resulting log levels. We show that there is an exact choice of $m$ and $h$ with two distinct global maxima.

For the original data choose the least positive integer $A>p_0$ satisfying the prior strictly increasing tail bound $A(\max Y_h-\overline{Y_h})>12e^{\gamma_E}$ on $[0,h_0]$. In particular $L_h(p)<P_h$ for $p\ge A$. By continuity on the compact path,
\[
 \inf_h \min\{P_h-L_h(A),\ P_h-N_h\}>0 .
\]
The new power face $\widetilde P_h=-1-\log(100-\mathrm{mean}(\widetilde Y_h))$ is uniformly strictly below $P_h$ for all sufficiently large $m$, since the old maximum is 2 and its mean is bounded. The new normal face converges uniformly to $N_h$. At fixed positive power the new profile converges uniformly in $h$ on compact power intervals: this follows from the formula $\log p+B(k)-kK_h(p)$ with $K_h(p)=\log \overline{e^{p(Y_h-\overline{Y_h})}}>0$ and strictly monotone inner equation $f(k)=K_h(p)$. For a sufficiently large common finite $D>A$, the old value $L_h(D)$ is uniformly close to $P_h$ from below. This follows from monotone convergence on the common tail, continuity of the limit and compactness (or by a finite subcover). Hence a single $D$ and all sufficiently large $m$ give
\[
 \widetilde L_h(D)>\max\{\widetilde L_h(A),\widetilde N_h,\widetilde P_h\}
\]
throughout the path. Choose $D$ large enough also that $L_0(D)>\max_{0\le p\le A}L_0(p)$, possible by strict balanced-sample loss on that compact interval. Then $\widetilde L_0(D)>\max_{[0,A]}\widetilde L_0$ for sufficiently large $m$. Uniform convergence on $[0,A]$, including zero, follows from the moment expansion $K_h(p)=s_h^2p^2/2+O(p^3)$ with a common remainder, positive variance floor and bounded support.

At the other endpoint we need an upper bound on the whole large-power branch. The new centered cumulant satisfies $\widetilde K_h(p)\ge(1-\epsilon)K_h(p)$ by the weighted arithmetic/geometric mean inequality on the two component exponential moments after centering each component. The derivative in $K$ of $\sup_k(B(k)-kK)$ is $-k(K)$, and $k(K)<1/K$ by $\psi(k+1)>\log k$ and the digamma recurrence. Therefore
\[
 \widetilde L_h(p)\le L_h(p)-\log(1-\epsilon)\le P_h-\log(1-\epsilon)
 \quad(p\ge A).
\]
The fixed witness at $(h_0,p_0)$ then beats the entire large-power branch for sufficiently large $m$. Choose the first integer pair $(D,m)$, ordered by $D+m$ then by $D$, with $D>A$, $m\ge1$, satisfying the three strict tests just established (the all-$h$ displayed comparison, the small-branch loss at $h=0$, and the upper-bound test at $h_0$). The existence argument above supplies such a pair. These definitions involve only a fixed compact path, continuous profiles and strict inequalities.

The two heights $\max_{[0,A]}\widetilde L_h$ and $\sup_{p\ge A}\widetilde L_h$ are continuous in $h$. For the latter, use the new uniform strictly increasing tail below $\widetilde P_h$, with the same prior bound using sample size $12m+1$ and a positive lower bound for the new maximum minus its mean. Beyond a common cutoff this whole tail loses to the witness at $D$, so the supremum is a compact maximum. The sign of the height difference reverses on the path. Define $h_*$ as its first zero. Both branch heights strictly beat the separator at $A$ and the two excluded faces by the witness inequality. Thus a maximizing power in each branch lies in $(0,A)$ and $(A,\infty)$, respectively. The exact sample specified by $(h_*,m)$ gives two distinct finite global maximizers after reciprocation. The definition leaves the number of winners within each branch and binary64 representability open.

### pareto2 held min

**Refuted over the reals, with a proved singleton branch.** For positive excesses $y_i=x_i-\min$ the Lomax rate is $1/\mathrm{scale}$, with the same shape. Apply the [Lomax proof](2026-11-25-lomax-uniq-math-report.md#exact-tie-construction) at $\min=0$ to $(a_*,1^{[10]},100^{[10]})$ for exactly two global maxima. With at most two distinct positive excesses the same theorem proves uniqueness conditional on attainment. If some excess is zero there is no finite joint optimum by the [endpoint rule](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md#r-notes): holding any positive shape small enough and sending scale to zero gives an unbounded likelihood. Below-threshold observations are infeasible.

### pareto4 held min+shape2

**Refuted over the reals, with a proved singleton branch.** With a held threshold $\mu$ and power $\gamma=\mathrm{shape2}$, positive excesses map by $y_i=(x_i-\mu)^\gamma$ with a data-only Jacobian. The Lomax shape is $\mathrm{shape1}$ and rate is $\mathrm{scale}^{-\gamma}$. Use the [same exact crossing](2026-11-25-lomax-uniq-math-report.md#exact-tie-construction) at $\mu=0,\gamma=1$ for exactly two global maxima, and its two-support theorem at any positive held power for uniqueness conditional on attainment. At threshold contacts $\gamma<1$ gives a singular density, $\gamma>1$ forces zero at that contact, and $\gamma=1$ reduces to the Pareto II endpoint rule. These contacts have no finite positive-likelihood MLE on the fitting domain. The claim concerns exact powers and subtraction; a transformed rounded sample can have different roots and comparisons.

### pgw held scale

**Proved full set by a finite comparison.** On nonconstant positive data with $a>\min x_i$, the [prior compactness and critical-list result](2026-11-25-pgw-math-report.md#gompertz-decisions-and-the-finite-critical-list) excludes small and large shape $b$ from any positive witness superlevel. For fixed $b$, in its notation $L_i=\log(1+e^{b(\log x_i-\log a)})>0$, and the mean log likelihood in $c$, omitting constants, is $\log c+\overline{cL_i-e^{cL_i}}$. Its second derivative is strictly negative, and its derivative changes sign from positive to negative. The conditional optimum is unique and analytic. The profile is nonconstant by its vanishing likelihood at $b\downarrow0$. Its stationary roots in the compact shape region are finite and precisely the ones at the highest height give the maximizing set. For other samples there is no finite maximum by the held-scale face and collapse analysis. A universal proof of one winning height, or a finite/finite counterexample on this mask, remains open. The fixed-exponent and free-scale examples use different feasible profiles.

### sn held omega

**Refuted by dyadic data and hold.** Hold $\omega=250$ and take $(-437,0^{[5]},437)$. Divide observations and scale by 437. This recovers the [prior rational witness](2026-11-25-sn-math-report.md#exact-finite-global-tie) with inverse scale $437/250$ and sample $(-1,0^{[5]},1)$. The rational inequalities there prove that this witness beats even the unconstrained normal optimum and both unconstrained half-normal face values. Holding its own scale therefore preserves all three strict comparisons. At each finite shape the held-scale location optimum is unique by strict concavity. The [strict-tail result](2026-11-25-sn-math-report.md#strict-tail-exclusion-and-exact-sample-criterion) applies with fixed inverse scale and gives a compact shape interval containing all finite winners. Thus a global winner has $\alpha\ne0$, and reflection about zero gives a distinct winner with identical held scale. The same theorem provides the complete set for any nonconstant sample by retaining exactly the finite stationary roots at the best height when it reaches the larger face. The construction does not count additional reflected orbits.

## Summary

“Proved” includes exact maximizing-set descriptions by critical-height comparisons. “Refuted” records an exact counterexample to universal singleton status.

| Family | Held | Question | Result |
| --- | --- | --- | --- |
| bbinom | alpha | criterion | proved |
| bbinom | alpha | failure | proved |
| bbinom | alpha | uniqueness | partial |
| bbinom | alpha+size | uniqueness | proved |
| bbinom | beta | criterion | proved |
| bbinom | beta | failure | proved |
| bbinom | beta | uniqueness | refuted |
| bbinom | beta+size | uniqueness | proved |
| bnbinom | alpha | uniqueness | refuted |
| bnbinom | beta | criterion | proved |
| bnbinom | beta | failure | proved |
| bnbinom | beta | uniqueness | partial |
| bnbinom | size | criterion | proved |
| bnbinom | size | failure | proved |
| bnbinom | size | uniqueness | partial |
| nhyper | all free | failure | proved |
| nhyper | all free | uniqueness | proved |
| nhyper | N | uniqueness | proved |
| nhyper | N+m | uniqueness | proved |
| nhyper | N+r | uniqueness | proved |
| nhyper | m | uniqueness | proved |
| hyper | all free | uniqueness | partial |
| hyper | k | uniqueness | proved |
| hyper | m | uniqueness | proved |
| triang | a | uniqueness | refuted |
| triang | a+c | uniqueness | proved |
| triang | b | uniqueness | refuted |
| triang | b+c | uniqueness | proved |
| triang | c | uniqueness | proved |
| gengamma | all free | uniqueness | refuted |
| gengamma | mu | uniqueness | proved |
| gengamma | sigma | uniqueness | refuted |
| bhatt | mu | uniqueness | refuted |
| expweibull | scale | uniqueness | proved |
| invburr | shape2 | uniqueness | refuted |
| invtrgamma | all free | uniqueness | refuted |
| pareto2 | min | uniqueness | refuted |
| pareto4 | min+shape2 | uniqueness | refuted |
| pgw | scale | uniqueness | proved |
| sn | omega | uniqueness | refuted |

## What this changes for the fitter

- For beta-binomial with one shape and size held, use a sign bracket for the unique root on the attained positive-size branch. A size-zero fit has a free-shape ray. With only beta held, a strict integer tail cutoff bounds the comparison and one root per size suffices. With only alpha held, a strict finite witness bounds the comparison against fixed-shape NB. Maintain the upper and lower bounds of every size examined until a verified ordering or equality is available.
- For beta-negative-binomial with held beta or size, the positive-count comparison uses a fixed-size NB delegate. The tangent inequality gives a sufficient exact stop, and overdispersion relative to the fitted face gives a strict finite witness. The alpha-held swap example requires preserving distinct labels. Certify all canonical winning orbits before claiming that the payload is complete.
- For the integer urn models, compare exact adjacent products at one-dimensional crossings. Retain adjacent equals and compare across slices after putting back the data-dependent factors. For fixed-draw or fixed-composition hypergeometric equality, an identically equal residue polynomial can describe infinitely many fitted designs. Saturated and flat branches can use symbolic sets or the exact count-cap restriction.
- For triangular fits, use strict log-width concavity on a fixed feasible mode. Enumerate or rigorously prune observation knots when the mode is free. Keep every equal winning knot, and include a zero side width where the theorem sets it to zero.
- For generalized gamma, transformed gamma, Bhattacharjee, held-scale exponentiated Weibull and PGW, and skew-normal, keep the complete competitive root list until the height comparisons close. Retain the normal when it belongs to the model and use the face exclusion collars where proved. In generalized gamma and skew-normal, check exact reflection on the original observations before generating a second label. To certify that the reflected orbit is the whole set, exclude every other orbit. The held-mode generalized-gamma symmetry rule gives a direct singleton branch with the RMS log residual.
- For the Lomax transfers, preserve the exact transformed likelihood ordering and verify coefficient accuracy in the public parameters. A two-support predicate requires exact equality in the original coordinates. A certificate for a rounded transformation must account for its error, including any displacement of a previously retained root.

A strict comparison with a reachable face can refute a candidate and justify a named nonexistence outcome. Exact finite/face equality retains the finite fit. If only the global certificate fails to close, preserve a successful accuracy-checked search result with certification withdrawn and cardinality unknown. [Wrapper failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Dossier follow-ups

- Beta-binomial held shapes at positive fixed size have strictly single-crossing scores. With beta held and size free, replace any blanket singleton implication by the finite integer comparison and note the algebraic cross-size example. A face-height equality on the alpha-held route can permit attainment even when a strict witness is unavailable.
- Negative hypergeometric all-free design has a terminating comparison for positive nonconstant samples by the finite union of the proved fixed-stopping-count procedures. The saturated positive constant set is precisely `(N,N,c)` with `N >= c`. Hypergeometric fixed-size design holds have an exact residue procedure even at binomial equality.
- Generalized gamma has exact finite global ties in the all-free and held-scale masks. Inverse transformed gamma has separated finite global winners on an exact real sample. Distinguish the bounded finite candidate comparisons from a claim of universal singleton cardinality.
- Pareto IV currently fits only strictly positive excesses in this mask. The Lomax transfer applies on that domain. At an endpoint contact the underlying pointwise density would be singular below power one, zero above power one, and would inherit the Pareto II rule at power one. Report those conventions separately from the strict-support fitting route.

## Open

- **Beta-binomial, held alpha.** The first-order tail coefficient vanishes at NB equidispersion. A uniform subsequent coefficient or an exact profile-tail comparison is needed to bound the equality search there. A nonconstant exact tie between competing sizes on this mask, or a theorem excluding such a tie, remains open.
- **Beta-negative-binomial, held beta or size.** Compactness closes above a strict witness, but the scalar root reduction gives neither a negative profile curvature inequality nor a construction of two equal finite global heights. When the tangent criterion fails, the sign of the inward direction at the NB face alone cannot exclude a distant finite winner. A compact collar bound at the dispersion seam and a root-height ordering or exact tie are missing. On the alpha-held route the exact exclusion proof leaves the number of canonical winning orbits open.
- **Hypergeometric, all free.** The bounded search with a strict witness gives a complete set in that scope. Without such a witness an eventual comparison must be uniform as the smaller design count also grows. The fixed-design residue argument is for fixed exponent and denominator and does not provide that bound.
- **Continuous profiles.** For exponentiated Weibull and PGW at held scale, there is a compact finite list, but no general comparison proving one critical height wins or an exact sample refuting it. For generalized gamma with held location, sign symmetry settles the singleton case and the general root list is finite. The existence of a tie for asymmetric log residuals remains open. Held-scale generalized gamma needs a general count at higher-order zero-shape seams. The exact crossing proofs for Bhattacharjee and inverse transformed gamma do not count every root tied at the crossing. Skew-normal and generalized gamma reflection show at least two winners and leave additional orbits open.
- **Exact arithmetic for continuous ties.** Root and height comparisons in the finite sets are mathematical characterizations. In general a collection of increasingly narrow likelihood intervals cannot establish exact equality. The Lomax-derived, triangular, Bhattacharjee and transformed-gamma crossings are specified over the reals. Except for the explicitly dyadic examples, no binary64 representability claim is made.

## Reproduction

Run from the worktree root with the one-slot lock. The integer arithmetic checks verify the discrete comparisons, the generalized-gamma witness inequalities, and the beta-negative-binomial diagonal cover (373 rational intervals plus both tails). The atlas validator checks regime and question coverage, proof rows and report links. The exact examples above use analytical comparison, rational verification or the cited proof as indicated. A grid search is unnecessary for the claims recorded here.

```sh
d=dev/research/evaluations/2026-11-27-uniq-atlas-math-data
TMPDIR="$PWD/build/tmp" /tmp/umvl-cpu/run nice -n 10 python3 "$d/exact_checks.py"
TMPDIR="$PWD/build/tmp" /tmp/umvl-cpu/run nice -n 10 python3 "$d/validate.py"
```

Both final Python checks ran on the workstation. [Proof rows](2026-11-27-uniq-atlas-math-data/proofs.csv)
