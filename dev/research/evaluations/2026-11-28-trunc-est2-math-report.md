# Maximum likelihood with estimated truncation bounds for 22 further families

TASK-468, source revision `bb912f76`. The input is the reviewed known-window atlas at that revision. The 104 held masks give 312 estimated-window regimes.

## Corrections and scope

**Proved.** The generic transfer in the [exponential-family report](2026-11-27-trunc-expfam-math-report.md#estimated-bounds) requires one parent law per coefficient vector across all compared windows. For sample `(2,3)` at held NB size zero and specification `c(NA, Inf)`, the ordinary tight window has zero probability, whereas the separate positive-truncation extension at `[1,Inf]` fits a logarithmic law. The [first estimated-bound review](2026-11-27-mod-rev5-report.md) also distinguishes the signed finite zeta chart from the parent domain and the positive-density hypothesis from mere inclusion of a cut in the declared support. The earlier audit gives exact route comparisons for those models. No family in the present scope has a window-specific extension of that kind.

Two reviewed corrections matter directly for these regimes. At held discrete-Weibull probability $1/2$, sample `(1)` with lower-estimated cut and fixed upper cut $\infty$ conditions on $[1,\infty)$. Its conditional likelihood is $1-2^{1-2^\beta}$, strictly below one for every finite shape $\beta>0$, tending to one at the excluded lower atom. With a fixed lower cut zero and known infinite upper cut the same sample instead selects the excluded Bernoulli law, with supremum $1/2$. A positive observation alone does not imply attainment in the held-probability parent. For CMB, sample `(1,2)` at size two on $[1,\infty)$ has the full two-cell law on the fiber $\eta=\nu\log 2$, even as dispersion diverges. The finite-carrier argument below completes that escape classification. [Discrete review](2026-11-27-mod-rev8-report.md#proof-errors-and-scope-corrections)

**A null effective window can have positive nominal width.** For sample `(1)`, half-normal location and scale both held at one, and specification `c(0,NA)`, the tight nominal window is $[0,1]$ and its intersection with the support is $\{1\}$. The tight likelihood is undefined. The compatible windows $[0,1+\epsilon]$ have conditional density at one at least $1/\epsilon$, since the half-normal kernel is maximal at its endpoint. The joint likelihood diverges and the retained law tends to $\delta_1$. The same counterexample works for half-Cauchy with the same holds. This is an extra branch of the estimated-bound reduction even after the fixed-window compatibility corrections.

The continuous half-laws have an included support endpoint at their location. In particular sample `(1,1)` on $[0,1]$ at half-Cauchy scale one diverges as location increases to one from below. The density at one is at least $1/(2\epsilon)$ when the location is $1-\epsilon$ and $0<\epsilon\le1$. Setting location to one makes the retained event null. This is the upper-cut correction from the [location-scale review](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy).

## Joint profiling lemma

**Proved.** Let $m=\min_i x_i$, $M=\max_i x_i$, and $n>0$. Denote known cuts by $a,b$. The tight nominal windows are $[m,b]$, $[a,M]$, and $[m,M]$ for lower, upper, and both estimated, respectively. Each feasible nominal window $A$ contains its tight window $A_*$; intersection with the support at any fixed coefficient vector preserves inclusion. For positive parent likelihood and positive tight event mass,
$$
L(\theta,A)
= L(\theta,A_*)\left(\frac{P_\theta(A_*)}{P_\theta(A)}\right)^n
\le L(\theta,A_*).
$$
The numerator is the product of parent point probabilities or densities, including repeated observations at a closed cut. The proof needs neither exponential-family form nor a parameter-independent support. The exact tight window is itself admissible on these slices, so taking suprema gives equality in both directions. A looser window attains the joint optimum exactly when its coefficients maximize the tight-window likelihood and the removed part has zero mass. An escaping tight coefficient sequence also witnesses the joint supremum, and a finite compatible pair on a looser window cannot rescue a missing tight optimum.

This also controls joint limits when the supremum is finite and positive. Along any maximizing sequence, the tight-window objective tends to that same supremum, so the ratio of masses in the formula tends to one. The total variation distance between the two conditional probability laws equals one minus this ratio, by restricting the looser law to the tight event. Thus loosening creates no new finite-height limiting laws. Bound sequences themselves may escape on plateaus. In a divergent case it suffices to exhibit a compatible path with diverging likelihood; its limiting atom describes the concentrating path.

**Counts.** A compatible sample has positive mass at each occupied count. In particular the tight event has positive probability even at a singleton. A singleton has likelihood one for every compatible coefficient vector; a wider nominal window ties only when it also retains just that atom under those coefficients. For a fitted law positive at the adjacent discarded counts, real lower cuts tie in $(m-1,m]$ and upper cuts in $[M,M+1)$. For gaps or support endpoints extend each interval as far as zero removed mass permits. A lower cut can be anywhere below $m$ if $m$ is the smallest supported count. In particular this applies to lgser and yulesimon at one, and zeta at one inside its specified chart. An attained fit always has more than one nominal real cut tuple when at least one cut is free. The canonical cuts used by the interface retain one representative. Included parent atoms and moving integer supports use the same equality test. For example changing size can remove the upper neighbor for bbinom or CMB, and changing an integer uniform endpoint can hide an entire looser interval.

**Continuous laws.** On positive-width feasible windows the density is positive in the parent support interior and at included finite endpoints in these families. A free lower cut can tie below $m$ only when $m$ is the parent support's lower end; likewise for the upper cut at $M$. Half-normal and half-Cauchy can therefore have a lower-cut plateau when their location is $m$. Uniform can have either plateau, depending on which parent endpoints maximize. The numerator remains fixed even when a cut equals a moving support endpoint. A putative coefficient vector giving only a null intersection is infeasible. For nonconstant continuous data here, any coefficient vector compatible on a loose window is also compatible on the tight window, since the support is an interval containing the two distinct extrema.

**Zero effective width on a continuous constant sample.** A tight intersection consisting only of $c=m=M$ has zero probability even when the nominal cuts are distinct. If a parent with a null tight event has an admissible positive-mass loose window, keep that parent fixed and move the estimated cuts to their tight values. The effective widths $d$ tend to zero through positive values, and continuity from inside the support gives $L\sim d^{-n}$. The conditional laws tend weakly to $\delta_c$. A compatible expansion must have carrier on a direction permitted by the cuts. For example, with both half-normal coefficients held and location $c$, lower-estimated with fixed upper cut $c$ is incompatible, whereas upper-estimated with fixed lower cut $c$ admits shrinkage from the right. For uniform with both parent ends held at $u<v$, the upper-estimated design with lower cut $c=v$ is incompatible, and the mirrored lower-estimated design with upper cut $c=u$ is also incompatible. These examples use a positive endpoint density and illustrate the separate positive-mass requirement. A single such parent slice proves joint divergence, even if other coefficients give positive tight mass. Discard null tight slices without a compatible loose window. If every slice is then infeasible there is no positive conditional fit. For positive effective tight width, constants follow the coefficient tests below. In particular some of those constants attain at held coefficients or an ordinary finite fit.

## Fixed-window comparisons after profiling

**Proved.** Use the reviewed comparison at the displayed window in each regime, with the same holds and support restrictions. For counts round the known lower cut up and the known upper cut down, then intersect with the support at each coefficient or design. A one-sided estimated window can still be bounded, and a parent support can bound a nominal ray. A both-estimated count window has $M-m+1$ nominal cells and all compatible interval supports contain them. For a nonconstant continuous sample, the both-estimated interval is bounded and both endpoints occur in the sample. If only one cut is free the other may also equal an observed extreme. The comparisons use the resulting support and counts, including all endpoint multiplicities.

Let $H$ be the largest applicable excluded likelihood height, or the supremum of such heights when an escape parameter remains. On admissible finite pieces use stationary roots and feasible endpoints, with kink and support branches included, or the exact moment, odds or integer enumeration rules in the linked theorem. A finite witness at least as high as $H$ suffices for attainment. At equality the witness already wins or a strictly better level exists. A level strictly above $H$ confines a maximizing subsequence to an admissible compact level after removing any identified flat directions or replacing their limits by an equivalent finite chart. The known-window arguments provide the required escape bounds, root checks or finite comparisons. Transfer preserves finite-to-face equality. Implicit stationary comparisons do not give an effective root isolation or a general cardinality theorem.

On two retained count cells with mixed data, compare the best accessible odds on either side of the empirical odds $n_{l+1}/n_l$ by $R^{n_{l+1}}/(1+R)^n$. This product increases up to the empirical odds and decreases from there. Inclusion of an odds value requires an admissible finite preimage. At both estimated cuts, range one gives this rule at exactly $m,m+1$; range zero gives the singleton test. For example each of the two beta-binomial pairs $(\alpha,\beta)=(1,1),(3,2)$ at size two gives unit odds on sample `(1,2)`. For beta-negative-binomial on histogram $(10,6,4)$ at counts $1,2,3$, the two labeled triples $(\mathrm{size},\alpha,\beta)=(1,1,2),(2,1,1)$ induce odds $3/5,2/3$, which attain the empirical law. These witnesses work for each estimated pattern by selecting the other fixed cut at the corresponding endpoint. The same substitution does not apply to a witness whose known window is wider than the sample on the side being estimated.

### CMB finite-carrier limits

**Proved.** Fix a permitted size and absorb all held natural coordinates into the positive carrier $h_i$. The remaining weight is $h_i e^{v\cdot T_i}$ on the effective cells, with $v$ unrestricted in its free natural coordinates on the interior-probability chart. Included endpoint probabilities are evaluated separately as compatible true parent atoms. For any convergent sequence of normalized weight vectors let $F$ be the nonempty set of cells with positive limit. Against a reference $i_0\in F$, log ratios show that $v$'s projection on the span of the differences $T_i-T_{i_0}$, $i\in F$, converges to a finite projection. For all cells outside $F$ the log ratios tend to minus infinity. Subtracting the bounded projection leaves vectors orthogonal to the differences within $F$, and at a sufficiently late index one such vector has strictly negative inner product with every difference outside $F$. It exposes $F$. On $F$ the limit is the positive-carrier finite tilt from the convergent projection.

Conversely each proper exposed set and every such tilt on it are reached along the exposing ray. A limit with all cells surviving comes from an escaping sequence exactly when there is an unidentified direction, since all identified log ratios then have finite limits. This includes the rank-zero carrier and the two-cell rank-one fiber when both canonical coefficients are free. With no free canonical coordinate there is no escaping sequence at fixed size. The bounded size range reduces all possible escapes to these slices, plus included endpoint probability laws where compatible. The empirical-statistic hull and strict covariance in identified coordinates decide each slice. Compare the slice heights including all attaining designs and all finite preimages. This completes the failure list left one-sided in the reviewed fixed-window rows.

### Uniform endpoint formulas

**Proved.** Write parent endpoints $l,u$ and nominal cuts $A,B$. Let $L_0=m$ if either the parent lower end or nominal lower cut is free, and $L_0=\max(l,A)$ otherwise. Let $U_0=M$ if either upper end is free, and $U_0=\min(u,B)$ otherwise. Require that all held lower ends are at most $m$ and all held upper ends are at least $M$, with valid holds and an admissible positive-mass window. Every compatible interval has effective lower end at most $L_0$ and upper end at least $U_0$.

For the continuous uniform with $L_0<U_0$, setting each free end to its corresponding extreme realizes both targets simultaneously, and gives maximum $(U_0-L_0)^{-n}$. All solutions satisfy $\max(l,A)=L_0, \min(u,B)=U_0$, including valid clipping plateaus. If $L_0=U_0$, use any compatible positive-width pair and move the free endpoints continuously toward their target values. At interpolation fractions strictly below one the parent interval and the retained width stay positive. The likelihood tends to infinity and the retained laws converge to the common atom. With infinite fixed ends, retain those ends while interpolating the free finite coordinates. If no positive-width pair exists, the design is incompatible. For the discrete uniform, round fixed nominal cuts inward in these formulas and maximize $(U_0-L_0+1)^{-n}$, including equality and attained singleton parent supports. The same effective-end equations with rounded cuts give all joint preimages.

### Lower-tight half-normal

**Proved.** Whenever the lower nominal cut is $m$, write $y=x-m\ge0$ and $D=b-m>0$ (possibly infinite). All compatible locations satisfy $\mu\le m$. The conditional law is proportional to $e^{\lambda y-\tau y^2}$ on $[0,D]$, with $\lambda=(\mu-m)/\sigma^2\le0$, $\tau=1/(2\sigma^2)>0$. Thus this is a single identified convex chart, including $\lambda=0$, even when the original fixed-window proof uses two branches.

With both coefficients free and nonconstant data, fit the $\tau=0$ edge by matching $\bar y$ on the ray or on a bounded interval when $\bar y<D/2$; otherwise take $\lambda=0$ on the bounded interval. Denote this edge law by $Q$. It is a nonincreasing exponential or uniform. Inward gain at the edge is $E_QY^2-\overline{y^2}$. Attainment holds iff this is strictly positive, and the coefficient fit is unique. The strictly concave likelihood and coercivity for nonconstant samples on the closed wedge follow from the quadratic argument in the [fixed-window proof](2026-11-27-trunc-ls-math-report.md#hnorm-all-free-truncateknown). At a nonpositive inward score the edge solves both tangent inequalities, including at the corner. If the score is positive, a small positive $\tau$ improves on the edge at fixed $\lambda$; coercivity and integrability then give the finite winner. At held scale and nonconstant data, the linear-tilt derivative $\bar y-EY$ decreases strictly in $\lambda$. If nonnegative at zero, take $\lambda=0$. Otherwise use the unique negative root since the mean tends to zero at negative infinity. For a constant at the lower cut with positive width, sending $\lambda\to-\infty$ at fixed scale makes the likelihood diverge at the lower atom. Positive-width cases with held location still use the squared-residual mean comparison. For example, sample `(0,1)` on the lower-estimated right ray has edge rate two, and its edge and empirical second moments both equal $1/2$. The half-normal fit has no MLE and the excluded translated exponential gives the supremum.

### Continuous samples with just the two endpoints occupied

**Proved.** Take a bounded window $[m,M]$ and let the counts at $m,M$ be $k,j>0$. This includes any of the three designs when the fixed cuts, if present, coincide with the respective extrema. At fixed Laplace rate $r>0$, the log likelihood is strictly convex in the location between the endpoints by the split-integral proof, so the winning locations are the outside plateau on the side with the larger count, or both plateaus if $k=j$. With free rate the winning exponential slope matches the sample mean in the open interval. It is nonzero exactly when $k\ne j$; a zero slope is excluded and gives the uniform. With held scale the corresponding entire plateau or plateaus attain, including their endpoints.

For logistic at held inverse scale $r>0$, set $U=e^{r(M-m)}$, $c=e^{r(\mu-m)}$. The location-dependent part of the log likelihood is $(j-k)\log[(1+c)/(U+c)]$. Its derivative in $c$ has the sign of $j-k$. For unbalanced counts only the corresponding fixed-rate exponential face wins. When both coefficients are free, optimizing the signed exponential slope gives the same height as the joint supremum, with no finite fit. For balanced counts at held scale every location ties. With free scale the endpoint product for one copy of each endpoint is $[r/(2\sinh(r(M-m)/2))]^2$, strictly below the uniform product by $\sinh t>t$, approaching it as $r\downarrow0$. The same strict comparison applies to repeated balanced endpoints.

For balanced endpoints and Student with free degrees of freedom, put $K(x)=((x-\mu)^2+s^2)^{-1}$, $p=(\nu+1)/2>1$, $s=\mathrm{sd}\sqrt{\nu-2}$. The angular identity for Cauchy gives $\int K/(M-m)>\sqrt{K(m)K(M)}$. Jensen for $z^p$ gives the strict version for $K^p$ and hence a strict likelihood bound by the uniform, for every finite admissible $\nu$. If either location or sd is free, the uniform height is reachable on the bounded window by moving that coefficient to infinity with a fixed finite $\nu$. This settles nonattainment for these endpoint-balanced samples in three free-df masks. The argument does not require a joint exhaustion of the free-df parameter space.

## Regime statements

The linked fixed-window results and the profiling proof establish the claims below. In every regime apply compatibility, singleton or zero-effective-width tests first. A limit label describes a conditional law on the displayed window. For continuous collapse it describes the weak limit together with the corresponding divergent or finite sample-density height. The bound multiplicity rule applies separately from parent coefficient cardinality.

<!-- Regimes generated below -->
### bbinom all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Failure (exact).** Compare fixed-size zero-shape and binomial faces and compatible growing-size NB/power, logarithmic, Poisson or atom limits; include drift at height zero. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), size two, with (alpha,beta)=(1,1) or (3,2); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Failure (exact).** Compare fixed-size zero-shape and binomial faces and compatible growing-size NB/power, logarithmic, Poisson or atom limits; include drift at height zero. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), size two, with (alpha,beta)=(1,1) or (3,2); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Failure (exact).** Compare fixed-size zero-shape and binomial faces and compatible growing-size NB/power, logarithmic, Poisson or atom limits; include drift at height zero. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), size two, with (alpha,beta)=(1,1) or (3,2); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom alpha truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alpha-truncateknown)

**Failure (exact).** At fixed size the free-beta endpoints give the rare-beta recurrence or upper atom and the lower atom. At growing size the limits are the fixed-alpha NB, its finite power endpoint, the lower atom, or zero sample height on a ray. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alpha-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom alpha truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alpha-truncateknown)

**Failure (exact).** At fixed size the free-beta endpoints give the rare-beta recurrence or upper atom and the lower atom. At growing size the limits are the fixed-alpha NB, its finite power endpoint, the lower atom, or zero sample height on a ray. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alpha-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom alpha truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alpha-truncateknown)

**Failure (exact).** At fixed size the free-beta endpoints give the rare-beta recurrence or upper atom and the lower atom. At growing size the limits are the fixed-alpha NB, its finite power endpoint, the lower atom, or zero sample height on a ray. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alpha-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom alpha+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

**Failure (exact).** At fixed size the free shape tending to zero gives the corresponding rare-shape recurrence or true-endpoint atom. Its infinite end gives the opposite retained-endpoint atom. A singleton stays saturated. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

### bbinom alpha+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

**Failure (exact).** At fixed size the free shape tending to zero gives the corresponding rare-shape recurrence or true-endpoint atom. Its infinite end gives the opposite retained-endpoint atom. A singleton stays saturated. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

### bbinom alpha+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

**Failure (exact).** At fixed size the free shape tending to zero gives the corresponding rare-shape recurrence or true-endpoint atom. Its infinite end gives the opposite retained-endpoint atom. A singleton stays saturated. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-alphasize-truncateknown)

### bbinom beta truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-beta-truncateknown)

**Failure (exact).** At fixed size the free-alpha endpoints give the rare-alpha recurrence or lower atom and the upper atom. Growing size gives power or harmonic weights or an atom on finite windows, and zero height on rays containing positive observations. Defective zero-cell limits can tie the attained zero singleton. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-beta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom beta truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-beta-truncateknown)

**Failure (exact).** At fixed size the free-alpha endpoints give the rare-alpha recurrence or lower atom and the upper atom. Growing size gives power or harmonic weights or an atom on finite windows, and zero height on rays containing positive observations. Defective zero-cell limits can tie the attained zero singleton. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-beta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom beta truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise some compatible fixed-size critical value or singleton is at least the best excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-beta-truncateknown)

**Failure (exact).** At fixed size the free-alpha endpoints give the rare-alpha recurrence or lower atom and the upper atom. Growing size gives power or harmonic weights or an atom on finite windows, and zero height on rays containing positive observations. Defective zero-cell limits can tie the attained zero singleton. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-beta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bbinom beta+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

**Failure (exact).** At fixed size the free shape tending to zero gives the corresponding rare-shape recurrence or true-endpoint atom. Its infinite end gives the opposite retained-endpoint atom. A singleton stays saturated. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

### bbinom beta+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

**Failure (exact).** At fixed size the free shape tending to zero gives the corresponding rare-shape recurrence or true-endpoint atom. Its infinite end gives the opposite retained-endpoint atom. A singleton stays saturated. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

### bbinom beta+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

**Failure (exact).** At fixed size the free shape tending to zero gives the corresponding rare-shape recurrence or true-endpoint atom. Its infinite end gives the opposite retained-endpoint atom. A singleton stays saturated. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-betasize-truncateknown)

### bbinom size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Failure (exact).** Allowed zero-shape recurrences, endpoint mixtures, binomial or endpoint atoms at fixed size. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), size two, with (alpha,beta)=(1,1) or (3,2); holds: size. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

### bbinom size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Failure (exact).** Allowed zero-shape recurrences, endpoint mixtures, binomial or endpoint atoms at fixed size. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), size two, with (alpha,beta)=(1,1) or (3,2); holds: size. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

### bbinom size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton or a finite critical component meets the maximum of the allowed fixed-size face heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Failure (exact).** Allowed zero-shape recurrences, endpoint mixtures, binomial or endpoint atoms at fixed size. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Uniqueness (one-sided).** At compatible singleton designs every admissible shape fits. Otherwise the maximizing set, when nonempty, lies in finitely many semialgebraic critical components; at algebraic holds their heights can be compared exactly. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), size two, with (alpha,beta)=(1,1) or (3,2); holds: size. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bbinom-size-truncateknown)

### bnbinom all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Failure (exact).** Normalize reachable rare-shape, NB/power, logarithmic, Poisson and lower-atom ratio limits; on rays discard drifting paths. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Label tie (one-sided).** Not unique in general: [1,3], histogram (10,6,4), with (size,alpha,beta)=(1,1,2) or (2,1,1); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Failure (exact).** Normalize reachable rare-shape, NB/power, logarithmic, Poisson and lower-atom ratio limits; on rays discard drifting paths. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Label tie (one-sided).** Not unique in general: [1,3], histogram (10,6,4), with (size,alpha,beta)=(1,1,2) or (2,1,1); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Failure (exact).** Normalize reachable rare-shape, NB/power, logarithmic, Poisson and lower-atom ratio limits; on rays discard drifting paths. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Label tie (one-sided).** Not unique in general: [1,3], histogram (10,6,4), with (size,alpha,beta)=(1,1,2) or (2,1,1); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Failure (exact).** The bounded rare-numerator recurrences and the growing-numerator power or harmonic weights on finite windows are exhaustive, with atoms and ray drift at their degenerate ends. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Label tie (one-sided).** Not unique in general: [1,3], histogram (10,6,4), with (size,alpha,beta)=(1,1,2) or (2,1,1); holds: alpha. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Failure (exact).** The bounded rare-numerator recurrences and the growing-numerator power or harmonic weights on finite windows are exhaustive, with atoms and ray drift at their degenerate ends. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Label tie (one-sided).** Not unique in general: [1,3], histogram (10,6,4), with (size,alpha,beta)=(1,1,2) or (2,1,1); holds: alpha. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Failure (exact).** The bounded rare-numerator recurrences and the growing-numerator power or harmonic weights on finite windows are exhaustive, with atoms and ray drift at their degenerate ends. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Label tie (one-sided).** Not unique in general: [1,3], histogram (10,6,4), with (size,alpha,beta)=(1,1,2) or (2,1,1); holds: alpha. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alpha-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha+beta truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Failure (exact).** A free size approaching zero gives the rare-size recurrence or lower atom. Size infinity gives finite-window power weights or ray drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha+beta truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Failure (exact).** A free size approaching zero gives the rare-size recurrence or lower atom. Size infinity gives finite-window power weights or ray drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha+beta truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Failure (exact).** A free size approaching zero gives the rare-size recurrence or lower atom. Size infinity gives finite-window power weights or ray drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabeta-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha+beta+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

### bnbinom alpha+beta+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

### bnbinom alpha+beta+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphabetasize-truncateknown)

### bnbinom alpha+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Failure (exact).** A free beta approaching zero gives the rare-beta recurrence or lower atom. Beta infinity gives finite-window power weights or ray drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Failure (exact).** A free beta approaching zero gives the rare-beta recurrence or lower atom. Beta infinity gives finite-window power weights or ray drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom alpha+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Failure (exact).** A free beta approaching zero gives the rare-beta recurrence or lower atom. Beta infinity gives finite-window power weights or ray drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-alphasize-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom beta truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Failure (exact).** Bounded parameters give the rare-size and zero-alpha recurrences. Diverging parameters give fixed-beta NB or power weights, lower atoms, or drift on rays, with possible defective zero-cell mass. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom beta truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Failure (exact).** Bounded parameters give the rare-size and zero-alpha recurrences. Diverging parameters give fixed-beta NB or power weights, lower atoms, or drift on rays, with possible defective zero-cell mass. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom beta truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Failure (exact).** Bounded parameters give the rare-size and zero-alpha recurrences. Diverging parameters give fixed-beta NB or power weights, lower atoms, or drift on rays, with possible defective zero-cell mass. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-beta-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom beta+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Failure (exact).** Alpha approaching zero gives the finite zero-alpha recurrence or ray drift. Alpha infinity gives the lower atom. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom beta+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Failure (exact).** Alpha approaching zero gives the finite zero-alpha recurrence or ray drift. Alpha infinity gives the lower atom. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom beta+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Failure (exact).** Alpha approaching zero gives the finite zero-alpha recurrence or ray drift. Alpha infinity gives the lower atom. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-betasize-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Failure (exact).** Bounded parameters give the rare-beta and zero-alpha recurrences. Diverging parameters give fixed-size NB or power weights, lower atoms, or drift on rays, with possible defective zero-cell mass. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Failure (exact).** Bounded parameters give the rare-beta and zero-alpha recurrences. Diverging parameters give fixed-size NB or power weights, lower atoms, or drift on rays, with possible defective zero-cell mass. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### bnbinom size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible finite critical value meets the hold-specific escape envelope, or a singleton saturates. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Failure (exact).** Bounded parameters give the rare-beta and zero-alpha recurrences. Diverging parameters give fixed-size NB or power weights, lower atoms, or drift on rays, with possible defective zero-cell mass. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite windows with algebraic holds admit a semialgebraic attainment and maximizing-set decision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#bnbinom-size-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cmb all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown)

### cmb all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown)

### cmb all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-all-free-truncateknown)

### cmb nu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nu-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nu-truncateknown)

### cmb nu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nu-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nu-truncateknown)

### cmb nu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nu-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nu-truncateknown)

### cmb nu+prob truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

**Failure (exact).** A finite size enumeration always attains on compatible data, with no parameter escape. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

### cmb nu+prob truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

**Failure (exact).** A finite size enumeration always attains on compatible data, with no parameter escape. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

### cmb nu+prob truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

**Failure (exact).** A finite size enumeration always attains on compatible data, with no parameter escape. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprob-truncateknown)

### cmb nu+prob+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

### cmb nu+prob+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

### cmb nu+prob+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nuprobsize-truncateknown)

### cmb nu+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nusize-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nusize-truncateknown)

### cmb nu+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nusize-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nusize-truncateknown)

### cmb nu+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nusize-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-nusize-truncateknown)

### cmb prob truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-prob-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-prob-truncateknown)

### cmb prob truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-prob-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-prob-truncateknown)

### cmb prob truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-prob-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-prob-truncateknown)

### cmb prob+size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-probsize-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-probsize-truncateknown)

### cmb prob+size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-probsize-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-probsize-truncateknown)

### cmb prob+size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-probsize-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-probsize-truncateknown)

### cmb size truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-size-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-size-truncateknown)

### cmb size truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-size-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-size-truncateknown)

### cmb size truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the greatest compatible height among permitted sizes is attained on at least one winning slice. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-size-truncateknown)

**Failure (exact).** At each permitted size use the finite tilt on an exposed set of the free statistics. A full-carrier law can persist at infinity exactly along an unidentified direction. Check included endpoint atoms separately. A compatible retained singleton attains.

**Uniqueness (exact).** Combine all tied sizes, endpoint fibers and short-window fibers with each identified moment root. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmb-size-truncateknown)

### cmp all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise on three or more cells apply the constant/adjacent dispatch, then the mean-matched geometric inward test and rate inclusion rule; mixed pairs attain. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

**Failure (exact).** Point and adjacent laws and the excluded finite geometric chart with rate at least one. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

**Uniqueness (exact).** On nonsingleton windows the identified non-atomic root is unique on three or more cells. A mixed pair has the full empirical-odds affine fiber intersected with the parent domain, with nu=0 allowed only at lambda<1. A compatible included zero atom or a singleton gives the corresponding flat set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

### cmp all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise on three or more cells apply the constant/adjacent dispatch, then the mean-matched geometric inward test and rate inclusion rule; mixed pairs attain. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

**Failure (exact).** Point and adjacent laws and the excluded finite geometric chart with rate at least one. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

**Uniqueness (exact).** On nonsingleton windows the identified non-atomic root is unique on three or more cells. A mixed pair has the full empirical-odds affine fiber intersected with the parent domain, with nu=0 allowed only at lambda<1. A compatible included zero atom or a singleton gives the corresponding flat set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

### cmp all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise on three or more cells apply the constant/adjacent dispatch, then the mean-matched geometric inward test and rate inclusion rule; mixed pairs attain. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

**Failure (exact).** Point and adjacent laws and the excluded finite geometric chart with rate at least one. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

**Uniqueness (exact).** On nonsingleton windows the identified non-atomic root is unique on three or more cells. A mixed pair has the full empirical-odds affine fiber intersected with the parent domain, with nu=0 allowed only at lambda<1. A compatible included zero atom or a singleton gives the corresponding flat set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-all-free-truncateknown)

### cmp lambda truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the empirical factorial statistic against its attainable interval and inclusion bits, with separate flat or atomic branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

**Failure (exact).** Minimal factorial-statistic set, or the excluded zero-dispersion tilt at held rate at least one on a finite window. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

**Uniqueness (exact).** Each identified moment root is unique; the stated flat, atom and two-cell fibers give every tied coefficient. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

### cmp lambda truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the empirical factorial statistic against its attainable interval and inclusion bits, with separate flat or atomic branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

**Failure (exact).** Minimal factorial-statistic set, or the excluded zero-dispersion tilt at held rate at least one on a finite window. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

**Uniqueness (exact).** Each identified moment root is unique; the stated flat, atom and two-cell fibers give every tied coefficient. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

### cmp lambda truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the empirical factorial statistic against its attainable interval and inclusion bits, with separate flat or atomic branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

**Failure (exact).** Minimal factorial-statistic set, or the excluded zero-dispersion tilt at held rate at least one on a finite window. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

**Uniqueness (exact).** Each identified moment root is unique; the stated flat, atom and two-cell fibers give every tied coefficient. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambda-truncateknown)

### cmp lambda+nu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

### cmp lambda+nu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

### cmp lambda+nu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-lambdanu-truncateknown)

### cmp nu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the conditional mean interval and included zero atom. At zero dispersion on a finite window the upper mean is excluded. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

**Failure (exact).** Endpoint atom or uniform at the excluded upper geometric rate on a finite window. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

**Uniqueness (exact).** Each identified moment root is unique; the stated flat, atom and two-cell fibers give every tied coefficient. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

### cmp nu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the conditional mean interval and included zero atom. At zero dispersion on a finite window the upper mean is excluded. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

**Failure (exact).** Endpoint atom or uniform at the excluded upper geometric rate on a finite window. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

**Uniqueness (exact).** Each identified moment root is unique; the stated flat, atom and two-cell fibers give every tied coefficient. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

### cmp nu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the conditional mean interval and included zero atom. At zero dispersion on a finite window the upper mean is excluded. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

**Failure (exact).** Endpoint atom or uniform at the excluded upper geometric rate on a finite window. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

**Uniqueness (exact).** Each identified moment root is unique; the stated flat, atom and two-cell fibers give every tied coefficient. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#cmp-nu-truncateknown)

### dlaplace all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Use the center and distance comparisons on integer cells from $m$ to $\lfloor b\rfloor$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the largest compatible center-specific height occurs at a finite rate in at least one center slice. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

**Failure (exact).** On an exterior-center plateau an escaping center can preserve the same clipped-center law. A free rate can additionally reach a nearest-cell atom or the finite uniform; drifting on an unbounded side gives zero height. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

### dlaplace all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Use the center and distance comparisons on integer cells from $\lceil a\rceil$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the largest compatible center-specific height occurs at a finite rate in at least one center slice. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

**Failure (exact).** On an exterior-center plateau an escaping center can preserve the same clipped-center law. A free rate can additionally reach a nearest-cell atom or the finite uniform; drifting on an unbounded side gives zero height. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

### dlaplace all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Use the center and distance comparisons on integer cells from $m$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise the largest compatible center-specific height occurs at a finite rate in at least one center slice. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

**Failure (exact).** On an exterior-center plateau an escaping center can preserve the same clipped-center law. A free rate can additionally reach a nearest-cell atom or the finite uniform; drifting on an unbounded side gives zero height. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-all-free-truncateknown)

### dlaplace location truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Use the center and distance comparisons on integer cells from $m$ to $\lfloor b\rfloor$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise an identified rate root exists exactly when the mean distance is strictly between its endpoint expectations. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

**Failure (exact).** A free rate can escape to the nearest atom or finite uniform; held rate always attains on compatible data. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

### dlaplace location truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Use the center and distance comparisons on integer cells from $\lceil a\rceil$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise an identified rate root exists exactly when the mean distance is strictly between its endpoint expectations. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

**Failure (exact).** A free rate can escape to the nearest atom or finite uniform; held rate always attains on compatible data. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

### dlaplace location truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Use the center and distance comparisons on integer cells from $m$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise an identified rate root exists exactly when the mean distance is strictly between its endpoint expectations. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

**Failure (exact).** A free rate can escape to the nearest atom or finite uniform; held rate always attains on compatible data. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-location-truncateknown)

### dlaplace location+scale truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Use the center and distance comparisons on integer cells from $m$ to $\lfloor b\rfloor$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

### dlaplace location+scale truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Use the center and distance comparisons on integer cells from $\lceil a\rceil$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

### dlaplace location+scale truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Use the center and distance comparisons on integer cells from $m$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-locationscale-truncateknown)

### dlaplace scale truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Use the center and distance comparisons on integer cells from $m$ to $\lfloor b\rfloor$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by the center reduction and direct comparison. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

**Failure (exact).** On an exterior-center plateau an escaping center can preserve the same clipped-center law. With rate held every compatible sample attains. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

### dlaplace scale truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Use the center and distance comparisons on integer cells from $\lceil a\rceil$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by the center reduction and direct comparison. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

**Failure (exact).** On an exterior-center plateau an escaping center can preserve the same clipped-center law. With rate held every compatible sample attains. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

### dlaplace scale truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Use the center and distance comparisons on integer cells from $m$ to $M$.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by the center reduction and direct comparison. Singletons are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

**Failure (exact).** On an exterior-center plateau an escaping center can preserve the same clipped-center law. With rate held every compatible sample attains. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

**Uniqueness (exact).** The winning integer centers and their plateau preimages, with the unique identified rate per center, give the full set. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dlaplace-scale-truncateknown)

### dnorm all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The integrated normal carrier runs from $m$ to $\lfloor b\rfloor+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise short-window and collapse dispatch, then compare finite stationary values with the best geometric tilt for range at least two. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Failure (exact).** Point and adjacent limits, geometric tilts including uniform, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [0,1], sample (0,1), mean one and sd=1 or sd=2; holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The integrated normal carrier runs from $\lceil a\rceil$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise short-window and collapse dispatch, then compare finite stationary values with the best geometric tilt for range at least two. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Failure (exact).** Point and adjacent limits, geometric tilts including uniform, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [0,1], sample (0,1), mean one and sd=1 or sd=2; holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The integrated normal carrier runs from $m$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise short-window and collapse dispatch, then compare finite stationary values with the best geometric tilt for range at least two. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Failure (exact).** Point and adjacent limits, geometric tilts including uniform, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [0,1], sample (0,1), mean one and sd=1 or sd=2; holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm mean truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The integrated normal carrier runs from $m$ to $\lfloor b\rfloor+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise compare finite scalar candidates with the uniform and projected sharp-face heights, using the exact two-cell rule on a pair. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Failure (exact).** Uniform, point, equal-half adjacent sharp law, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell cardinality (exact).** At the shared edge all scales tie; otherwise the nearer cell's probability increases strictly with precision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm mean truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The integrated normal carrier runs from $\lceil a\rceil$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise compare finite scalar candidates with the uniform and projected sharp-face heights, using the exact two-cell rule on a pair. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Failure (exact).** Uniform, point, equal-half adjacent sharp law, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell cardinality (exact).** At the shared edge all scales tie; otherwise the nearer cell's probability increases strictly with precision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm mean truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The integrated normal carrier runs from $m$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise compare finite scalar candidates with the uniform and projected sharp-face heights, using the exact two-cell rule on a pair. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Failure (exact).** Uniform, point, equal-half adjacent sharp law, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell cardinality (exact).** At the shared edge all scales tie; otherwise the nearer cell's probability increases strictly with precision. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-mean-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm mean+sd truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The integrated normal carrier runs from $m$ to $\lfloor b\rfloor+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

### dnorm mean+sd truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The integrated normal carrier runs from $\lceil a\rceil$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

### dnorm mean+sd truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The integrated normal carrier runs from $m$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-meansd-truncateknown)

### dnorm sd truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The integrated normal carrier runs from $m$ to $\lfloor b\rfloor+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise at least two cells give attainment exactly unless all observations equal a finite window end; a singleton always attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Failure (exact).** Favored finite endpoint atom or drift toward an infinite end. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell cardinality (exact).** Every compatible mixed pair has a unique location at the held positive scale. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm sd truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The integrated normal carrier runs from $\lceil a\rceil$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise at least two cells give attainment exactly unless all observations equal a finite window end; a singleton always attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Failure (exact).** Favored finite endpoint atom or drift toward an infinite end. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell cardinality (exact).** Every compatible mixed pair has a unique location at the held positive scale. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dnorm sd truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The integrated normal carrier runs from $m$ to $M+1$ when those cuts are finite.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise at least two cells give attainment exactly unless all observations equal a finite window end; a singleton always attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Failure (exact).** Favored finite endpoint atom or drift toward an infinite end. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell cardinality (exact).** Every compatible mixed pair has a unique location at the held positive scale. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dnorm-sd-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dunif all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif max truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $\min(u,b)$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif max truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif max truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif max+min truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $\min(u,b)$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif max+min truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $\max(l,a)$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif max+min truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif min truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif min truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $\max(l,a)$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dunif min truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** Every compatible sample attains at the optimal effective ends, including coincident ends.

**Failure (exact).** Incompatibility gives no fit. Coincident optimal ends give an attained atom.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### dweibull all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the saturated constant/adjacent dispatch and the odds rule on two cells; compare finite critical values against the escape envelope for wider samples. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Failure (exact).** Power and log increments, discretised Pareto, singleton and adjacent laws, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), with finite shape1 roots at shape2=2 and at shape2=3; holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dweibull all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the saturated constant/adjacent dispatch and the odds rule on two cells; compare finite critical values against the escape envelope for wider samples. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Failure (exact).** Power and log increments, discretised Pareto, singleton and adjacent laws, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), with finite shape1 roots at shape2=2 and at shape2=3; holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dweibull all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the saturated constant/adjacent dispatch and the odds rule on two cells; compare finite critical values against the escape envelope for wider samples. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Failure (exact).** Power and log increments, discretised Pareto, singleton and adjacent laws, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), with finite shape1 roots at shape2=2 and at shape2=3; holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dweibull shape1 truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a singleton is flat. Otherwise compare all finite scalar candidates with the two held-probability endpoint heights including equality. On the full parent support, maximum at least two gives one finite fit, maximum one selects the excluded Bernoulli law, and all-zero data are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Failure (exact).** Small-shape log increments, zero mass on fixed positives on rays, lower atom or zero-one law at large shape. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Two-cell odds are monotone on [0,1]; at positive lower cut their finite critical values and endpoint limits give the comparison. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dweibull shape1 truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a singleton is flat. Otherwise compare all finite scalar candidates with the two held-probability endpoint heights including equality. On the full parent support, maximum at least two gives one finite fit, maximum one selects the excluded Bernoulli law, and all-zero data are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Failure (exact).** Small-shape log increments, zero mass on fixed positives on rays, lower atom or zero-one law at large shape. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Two-cell odds are monotone on [0,1]; at positive lower cut their finite critical values and endpoint limits give the comparison. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dweibull shape1 truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a singleton is flat. Otherwise compare all finite scalar candidates with the two held-probability endpoint heights including equality. On the full parent support, maximum at least two gives one finite fit, maximum one selects the excluded Bernoulli law, and all-zero data are flat. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Failure (exact).** Small-shape log increments, zero mass on fixed positives on rays, lower atom or zero-one law at large shape. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Two-cell odds are monotone on [0,1]; at positive lower cut their finite critical values and endpoint limits give the comparison. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### dweibull shape1+shape2 truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

### dweibull shape1+shape2 truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

### dweibull shape1+shape2 truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape1shape2-truncateknown)

### dweibull shape2 truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton attains on the entire free-probability slice. Otherwise attainment requires a count above the lower end and a strictly positive lower-rate score, with equality selecting the excluded increment law. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

**Failure (exact).** Lower atom or finite-window increment law; the broad ray has zero height. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

**Uniqueness (exact).** The finite rate root on a nonsingleton window is unique by strictly negative curvature. On a compatible singleton every free probability maximizes. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

### dweibull shape2 truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton attains on the entire free-probability slice. Otherwise attainment requires a count above the lower end and a strictly positive lower-rate score, with equality selecting the excluded increment law. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

**Failure (exact).** Lower atom or finite-window increment law; the broad ray has zero height. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

**Uniqueness (exact).** The finite rate root on a nonsingleton window is unique by strictly negative curvature. On a compatible singleton every free probability maximizes. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

### dweibull shape2 truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton attains on the entire free-probability slice. Otherwise attainment requires a count above the lower end and a strictly positive lower-rate score, with equality selecting the excluded increment law. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

**Failure (exact).** Lower atom or finite-window increment law; the broad ray has zero height. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

**Uniqueness (exact).** The finite rate root on a nonsingleton window is unique by strictly negative curvature. On a compatible singleton every free probability maximizes. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#dweibull-shape2-truncateknown)

### hyper all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper N truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

### hyper N truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

### hyper N truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-n-truncateknown)

### hyper N+k truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

### hyper N+k truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

### hyper N+k truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nk-truncateknown)

### hyper N+m truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

### hyper N+m truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

### hyper N+m truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-nm-truncateknown)

### hyper k truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper k truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper k truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-k-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper k+m truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Population only (exact).** With both counts held, only the eventual lower endpoint is an escaping atom. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper k+m truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Population only (exact).** With both counts held, only the eventual lower endpoint is an escaping atom. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper k+m truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Population only (exact).** With both counts held, only the eventual lower endpoint is an escaping atom. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-km-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper m truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper m truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hyper m truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Failure (exact).** The population-tail laws are conditioned binomial, Poisson or endpoint atoms as permitted by the holds, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#hyper-m-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### lgser all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton attains for every admissible theta. Otherwise attainment holds iff the empirical mean is strictly above the lower end and strictly below the harmonic mean, with the upper threshold infinite on rays. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

**Failure (exact).** Lower atom, finite harmonic law, or drift on a ray. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

**Uniqueness (exact).** On nonsingletons an attained moment root is unique. On a compatible singleton every theta in (0,1) fits. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

### lgser all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(1,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton attains for every admissible theta. Otherwise attainment holds iff the empirical mean is strictly above the lower end and strictly below the harmonic mean, with the upper threshold infinite on rays. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

**Failure (exact).** Lower atom, finite harmonic law, or drift on a ray. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

**Uniqueness (exact).** On nonsingletons an attained moment root is unique. On a compatible singleton every theta in (0,1) fits. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

### lgser all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a compatible singleton attains for every admissible theta. Otherwise attainment holds iff the empirical mean is strictly above the lower end and strictly below the harmonic mean, with the upper threshold infinite on rays. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

**Failure (exact).** Lower atom, finite harmonic law, or drift on a ray. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

**Uniqueness (exact).** On nonsingletons an attained moment root is unique. On a compatible singleton every theta in (0,1) fits. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-all-free-truncateknown)

### lgser theta truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

### lgser theta truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(1,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

### lgser theta truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#lgser-theta-truncateknown)

### nhyper all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper N truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

### nhyper N truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

### nhyper N truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-n-truncateknown)

### nhyper N+m truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

### nhyper N+m truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

### nhyper N+m truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nm-truncateknown)

### nhyper N+r truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

### nhyper N+r truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

### nhyper N+r truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise every compatible sample attains by bounded rational enumeration. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

**Failure (exact).** No compatible candidate if the integer enumeration yields no positive-weight sample. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

**Uniqueness (exact).** Compare all compatible exact rational heights and retain every tied integer design. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-nr-truncateknown)

### nhyper m truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Ray attainment (exact).** Every compatible sample on a right ray attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper m truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper m truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-m-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper m+r truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Ray attainment (exact).** Every compatible sample on a right ray attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper m+r truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper m+r truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-mr-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper r truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper r truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### nhyper r truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite compatible design reaches at least the hold-specific population-tail envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Failure (exact).** The population tail normalizes shifted-NB or finite polynomial weights, gives the lower atom, or loses fixed sample masses. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Apply the adjacent-ratio law, including the rational odds image or the monotone integer-neighbor rule, and check compatible singleton branches. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#nhyper-r-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Failure (exact).** Reachable restricted Poisson, Sibuya or tilted Sibuya, Lévy and endpoint atoms, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), with z=4 or z=5 and v=2/(1+1/z); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Failure (exact).** Reachable restricted Poisson, Sibuya or tilted Sibuya, Lévy and endpoint atoms, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), with z=4 or z=5 and v=2/(1+1/z); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Failure (exact).** Reachable restricted Poisson, Sibuya or tilted Sibuya, Lévy and endpoint atoms, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Two cell tie (one-sided).** Not unique in general: [1,2], sample (1,2), with z=4 or z=5 and v=2/(1+1/z); holds: all parent coefficients free. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig mean truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Failure (exact).** Held mean leaves the Sibuya and Poisson faces, with the included zero atom when compatible. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig mean truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Failure (exact).** Held mean leaves the Sibuya and Poisson faces, with the included zero atom when compatible. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig mean truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Failure (exact).** Held mean leaves the Sibuya and Poisson faces, with the included zero atom when compatible. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-mean-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig mean+shape truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

### pig mean+shape truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

### pig mean+shape truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-meanshape-truncateknown)

### pig shape truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Failure (exact).** At held positive shape the lower-mean limit is the lower atom and the upper-mean limit is Poisson–Lévy, which is strictly improvable on rays. Shape zero is a flat compatible atom. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Positive shape ray (exact).** On a ray at held positive shape, attainment holds exactly when some count exceeds the lower cell. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig shape truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(0,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Failure (exact).** At held positive shape the lower-mean limit is the lower atom and the upper-mean limit is Poisson–Lévy, which is strictly improvable on rays. Shape zero is a flat compatible atom. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### pig shape truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 0$. At $m=0$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included atom meets the hold-specific escape envelope. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Failure (exact).** At held positive shape the lower-mean limit is the lower atom and the upper-mean limit is Poisson–Lévy, which is strictly improvable on rays. Shape zero is a flat compatible atom. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Finite rational (exact).** Finite-window attainment and cardinality admit a rational critical-set decision at algebraic holds. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#pig-shape-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Failure (exact).** Geometric tilts where summable, endpoint atoms including rare-axis conditional atoms, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Failure (exact).** Geometric tilts where summable, endpoint atoms including rare-axis conditional atoms, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Failure (exact).** Geometric tilts where summable, endpoint atoms including rare-axis conditional atoms, or drift. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam mu1 truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Failure (exact).** At a held positive rate the free rate can reach a favored endpoint atom or drift at infinity. At zero it reaches the allowed Poisson axis or the nearest-to-zero atom conditional on a disjoint axis. At held rate zero use the remaining one-sided Poisson chart. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam mu1 truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Failure (exact).** At a held positive rate the free rate can reach a favored endpoint atom or drift at infinity. At zero it reaches the allowed Poisson axis or the nearest-to-zero atom conditional on a disjoint axis. At held rate zero use the remaining one-sided Poisson chart. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam mu1 truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Failure (exact).** At a held positive rate the free rate can reach a favored endpoint atom or drift at infinity. At zero it reaches the allowed Poisson axis or the nearest-to-zero atom conditional on a disjoint axis. At held rate zero use the remaining one-sided Poisson chart. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam mu1+mu2 truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

### skellam mu1+mu2 truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

### skellam mu1+mu2 truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu1mu2-truncateknown)

### skellam mu2 truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Failure (exact).** At a held positive rate the free rate can reach a favored endpoint atom or drift at infinity. At zero it reaches the allowed Poisson axis or the nearest-to-zero atom conditional on a disjoint axis. At held rate zero use the remaining one-sided Poisson chart. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam mu2 truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Failure (exact).** At a held positive rate the free rate can reach a favored endpoint atom or drift at infinity. At zero it reaches the allowed Poisson axis or the nearest-to-zero atom conditional on a disjoint axis. At held rate zero use the remaining one-sided Poisson chart. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### skellam mu2 truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. Intersect the displayed lattice window with each candidate design's support. An included endpoint atom or shorter carrier can change the inclusion test.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise a finite critical value or included axis candidate meets the hold-specific excluded height. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Failure (exact).** At a held positive rate the free rate can reach a favored endpoint atom or drift at infinity. At zero it reaches the allowed Poisson axis or the nearest-to-zero atom conditional on a disjoint axis. At held rate zero use the remaining one-sided Poisson chart. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Bound cardinality (exact).** If a finite fit attains, the estimated nominal real cuts are nonunique by inward rounding and any additional support plateau.

**Two cell comparison (exact).** Mixed pairs are saturated on a free odds fiber, or follow the monotone held-rate ratio with its inclusion bits. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#skellam-mu2-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### yulesimon all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the ray score or on finite windows compare rational critical values to the two endpoint heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

**Failure (exact).** Lower atom or finite harmonic law; at zero shape on rays all fixed masses vanish. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

**Uniqueness (one-sided).** Every attained ray fit is unique. On finite nonsingleton windows the rational likelihood is nonconstant and only finitely many stationary roots can maximize. Compatible singletons are flat. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

### yulesimon all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(1,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the ray score or on finite windows compare rational critical values to the two endpoint heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

**Failure (exact).** Lower atom or finite harmonic law; at zero shape on rays all fixed masses vanish. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

**Uniqueness (one-sided).** Every attained ray fit is unique. On finite nonsingleton windows the rational likelihood is nonconstant and only finitely many stationary roots can maximize. Compatible singletons are flat. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

### yulesimon all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise use the ray score or on finite windows compare rational critical values to the two endpoint heights. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

**Failure (exact).** Lower atom or finite harmonic law; at zero shape on rays all fixed masses vanish. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

**Uniqueness (one-sided).** Every attained ray fit is unique. On finite nonsingleton windows the rational likelihood is nonconstant and only finitely many stationary roots can maximize. Compatible singletons are flat. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-all-free-truncateknown)

### yulesimon rho truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

### yulesimon rho truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The ordinary effective lower cell is $\max(1,\lceil a\rceil)$, and the upper cell is $M$. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

### yulesimon rho truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The ordinary effective lower cell is $m\ge 1$. At $m=1$ all lower padding is flat. Check included atoms separately.

**Criterion (iff).** On the displayed window, a compatible retained singleton attains. Otherwise attains at the held law exactly when it is compatible. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

**Failure (exact).** No compatible likelihood when retained mass is zero or an observed cell is unsupported. A compatible retained singleton attains. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

**Uniqueness (exact).** The compatible held coefficient tuple is a singleton. Nominal real estimated cuts are nonunique at every attained fit. [Fixed-window evidence](2026-11-27-trunc-discrete-math-report.md#yulesimon-rho-truncateknown)

### laplace all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant, and on bounded windows a finite candidate reaches the uniform threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Failure (exact).** Constant-data collapse or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, unequal counts attain at the mean-matched exponential on the majority-side plateau; equal counts escape to uniform.

**Nonunique (one-sided).** Not unique in general: on [a, Inf), sample (a, a+1) with neither coefficient held has every location at or below a maximizing at scale 1/2. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### laplace all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant, and on bounded windows a finite candidate reaches the uniform threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Failure (exact).** Constant-data collapse or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, unequal counts attain at the mean-matched exponential on the majority-side plateau; equal counts escape to uniform.

**Nonunique (one-sided).** Not unique in general: on [a, Inf), sample (a, a+1) with neither coefficient held has every location at or below a maximizing at scale 1/2. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### laplace all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant, and on bounded windows a finite candidate reaches the uniform threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Failure (exact).** Constant-data collapse or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, unequal counts attain at the mean-matched exponential on the majority-side plateau; equal counts escape to uniform.

**Nonunique (one-sided).** Not unique in general: on [a, Inf), sample (a, a+1) with neither coefficient held has every location at or below a maximizing at scale 1/2. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-all-free-truncateknown)

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### laplace mu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, mean absolute residual lies strictly between the minimum and zero-rate model mean. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

**Failure (exact).** Collapse at the projection at the lower equality, otherwise uniform at or above the finite upper threshold. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

**Uniqueness (exact).** Unique on attainment. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

### laplace mu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, mean absolute residual lies strictly between the minimum and zero-rate model mean. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

**Failure (exact).** Collapse at the projection at the lower equality, otherwise uniform at or above the finite upper threshold. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

**Uniqueness (exact).** Unique on attainment. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

### laplace mu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, mean absolute residual lies strictly between the minimum and zero-rate model mean. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

**Failure (exact).** Collapse at the projection at the lower equality, otherwise uniform at or above the finite upper threshold. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

**Uniqueness (exact).** Unique on attainment. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-mu-truncateknown)

### laplace mu+sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

### laplace mu+sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

### laplace mu+sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-musigma-truncateknown)

### laplace sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, all compatible samples. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

**Failure (exact).** No failure. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

**Uniqueness (exact).** On bounded windows and rays all winners are the best cuts, sample locations and integrable exterior plateaus. On the whole line all sample medians maximize. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, the majority-side exterior plateau attains, and both plateaus attain at equal counts.

### laplace sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, all compatible samples. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

**Failure (exact).** No failure. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

**Uniqueness (exact).** On bounded windows and rays all winners are the best cuts, sample locations and integrable exterior plateaus. On the whole line all sample medians maximize. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, the majority-side exterior plateau attains, and both plateaus attain at equal counts.

### laplace sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, all compatible samples. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

**Failure (exact).** No failure. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown)

**Uniqueness (exact).** On bounded windows and rays all winners are the best cuts, sample locations and integrable exterior plateaus. On the whole line all sample medians maximize. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#laplace-sigma-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, the majority-side exterior plateau attains, and both plateaus attain at equal counts.

### logis all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant with a finite score candidate at or above the optimized exponential face on a bounded window or ray. On the whole line all nonconstant samples attain. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Constant-data collapse, or optimized signed exponential on the window, including uniform at slope zero. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, the optimized signed exponential wins without attainment, including uniform at balance.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### logis all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant with a finite score candidate at or above the optimized exponential face on a bounded window or ray. On the whole line all nonconstant samples attain. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Constant-data collapse, or optimized signed exponential on the window, including uniform at slope zero. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, the optimized signed exponential wins without attainment, including uniform at balance.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### logis all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant with a finite score candidate at or above the optimized exponential face on a bounded window or ray. On the whole line all nonconstant samples attain. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Constant-data collapse, or optimized signed exponential on the window, including uniform at slope zero. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, the optimized signed exponential wins without attainment, including uniform at balance.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### logis location truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at the projection. On rays the rest attain; on bounded windows a rate root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-location-truncateknown)

**Failure (exact).** Collapse at the projection or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-location-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### logis location truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at the projection. On rays the rest attain; on bounded windows a rate root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-location-truncateknown)

**Failure (exact).** Collapse at the projection or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-location-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### logis location truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at the projection. On rays the rest attain; on bounded windows a rate root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-location-truncateknown)

**Failure (exact).** Collapse at the projection or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-location-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### logis location+scale truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

### logis location+scale truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

### logis location+scale truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-locationscale-truncateknown)

### logis scale truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, a positive rational-score root reaches the maximum of zero and the applicable fixed-rate integrable exponential face values, or the score is identically zero. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Exponential escape of an integrable slope of the held-rate magnitude. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

**Uniqueness (one-sided).** Compare the polynomial roots; every location maximizes if the score vanishes identically. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, every location attains at equal counts; otherwise the fixed-rate signed exponential wins without attainment.

**Nonunique (one-sided).** Not unique in general: on a bounded window [a,b], sample (a,b) at any held scale greater than zero has every location maximizing. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

### logis scale truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, a positive rational-score root reaches the maximum of zero and the applicable fixed-rate integrable exponential face values, or the score is identically zero. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Exponential escape of an integrable slope of the held-rate magnitude. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

**Uniqueness (one-sided).** Compare the polynomial roots; every location maximizes if the score vanishes identically. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, every location attains at equal counts; otherwise the fixed-rate signed exponential wins without attainment.

**Nonunique (one-sided).** Not unique in general: on a bounded window [a,b], sample (a,b) at any held scale greater than zero has every location maximizing. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

### logis scale truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, a positive rational-score root reaches the maximum of zero and the applicable fixed-rate integrable exponential face values, or the score is identically zero. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Exponential escape of an integrable slope of the held-rate magnitude. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

**Uniqueness (one-sided).** Compare the polynomial roots; every location maximizes if the score vanishes identically. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

**Endpoint histogram (one-sided).** On a bounded window with only the two endpoints occupied, every location attains at equal counts; otherwise the fixed-rate signed exponential wins without attainment.

**Nonunique (one-sided).** Not unique in general: on a bounded window [a,b], sample (a,b) at any held scale greater than zero has every location maximizing. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#logis-scale-truncateknown)

### cauchy all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie and a finite candidate reaches the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-all-free-truncateknown)

**Failure (exact).** Collapse at a feasible strict majority; critical collapse, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie and a finite candidate reaches the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-all-free-truncateknown)

**Failure (exact).** Collapse at a feasible strict majority; critical collapse, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie and a finite candidate reaches the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-all-free-truncateknown)

**Failure (exact).** Collapse at a feasible strict majority; critical collapse, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy location truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-location-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-location-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy location truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-location-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-location-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy location truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-location-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-location-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy location+scale truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

### cauchy location+scale truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

### cauchy location+scale truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-locationscale-truncateknown)

### cauchy scale truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample attains on unbounded windows. On bounded windows a location root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-scale-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-scale-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy scale truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample attains on unbounded windows. On bounded windows a location root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-scale-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-scale-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### cauchy scale truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample attains on unbounded windows. On bounded windows a location root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-scale-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#cauchy-scale-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### gumbel all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant with a finite score candidate reaching the optimized exponential face on a bounded window or ray. On the whole line all nonconstant samples attain. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Constant-data collapse or optimized exponential escape, including uniform at slope zero. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### gumbel all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant with a finite score candidate reaching the optimized exponential face on a bounded window or ray. On the whole line all nonconstant samples attain. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Constant-data collapse or optimized exponential escape, including uniform at slope zero. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### gumbel all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant with a finite score candidate reaching the optimized exponential face on a bounded window or ray. On the whole line all nonconstant samples attain. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Constant-data collapse or optimized exponential escape, including uniform at slope zero. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-all-free-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### gumbel mu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at the projection; on bounded windows a finite rate root must also reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-mu-truncateknown)

**Failure (exact).** Collapse at the projection or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-mu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### gumbel mu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at the projection; on bounded windows a finite rate root must also reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-mu-truncateknown)

**Failure (exact).** Collapse at the projection or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-mu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### gumbel mu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at the projection; on bounded windows a finite rate root must also reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-mu-truncateknown)

**Failure (exact).** Collapse at the projection or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-mu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### gumbel mu+sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

### gumbel mu+sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

### gumbel mu+sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-musigma-truncateknown)

### gumbel sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, in the exponential transform the sample mean is strictly between its lower cut and its zero-rate mean. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

**Failure (exact).** Upper-x-end collapse or the decreasing exponential with slope -1/sigma. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

**Uniqueness (exact).** Unique by the scalar exponential score. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

### gumbel sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, in the exponential transform the sample mean is strictly between its lower cut and its zero-rate mean. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

**Failure (exact).** Upper-x-end collapse or the decreasing exponential with slope -1/sigma. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

**Uniqueness (exact).** Unique by the scalar exponential score. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

### gumbel sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, in the exponential transform the sample mean is strictly between its lower cut and its zero-rate mean. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

**Failure (exact).** Upper-x-end collapse or the decreasing exponential with slope -1/sigma. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

**Uniqueness (exact).** Unique by the scalar exponential score. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#gumbel-sigma-truncateknown)

### hnorm all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, require nonconstant data and a strictly positive inward second-moment score at the optimized nonpositive exponential edge.

**Failure (exact).** Constant-data collapse, or the optimized decreasing exponential or uniform when its inward second-moment score is nonpositive. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma.

**Uniqueness (exact).** The identified concave wedge gives one coefficient vector whenever it attains. The lower cut has a plateau exactly when the fitted location equals the minimum. A free cut ties exactly on the zero-removed-mass plateau.

### hnorm all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, nonconstant with a finite branch winner reaching the other branch supremum. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-all-free-truncateknown)

**Failure (exact).** Atomic collapse, or an excluded uniform or negative-slope exponential from the branch comparison. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-all-free-truncateknown)

**Uniqueness (one-sided).** At most two finite maximizing parameter vectors, one per branch. Compare the attained branch winners, counting coincidences once. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-all-free-truncateknown)

**Null effective window (exact).** A constant at the estimated upper cut has divergent likelihood whenever the fixed lower cut permits the data. Take the parent location at the constant and let the estimated upper cut approach it from above.

### hnorm all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, require nonconstant data and a strictly positive inward second-moment score at the optimized nonpositive exponential edge.

**Failure (exact).** Constant-data collapse, or the optimized decreasing exponential or uniform when its inward second-moment score is nonpositive. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma.

**Uniqueness (exact).** The identified concave wedge gives one coefficient vector whenever it attains. The lower cut has a plateau exactly when the fitted location equals the minimum. A free cut ties exactly on the zero-removed-mass plateau.

### hnorm mu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, the empirical squared residual lies strictly between the minimum and zero-tilt mean on the effective window. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

**Failure (exact).** Collapse at the lower effective cut or bounded effective uniform. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

**Uniqueness (exact).** Unique by strict monotonicity of the squared-residual mean. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

### hnorm mu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, the empirical squared residual lies strictly between the minimum and zero-tilt mean on the effective window. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

**Failure (exact).** Collapse at the lower effective cut or bounded effective uniform. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

**Uniqueness (exact).** Unique by strict monotonicity of the squared-residual mean. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

**Null effective window (exact).** A constant at the held location with fixed lower cut at or below that location has a null tight event. Increasing the estimated upper cut above the location and then shrinking it back gives divergent likelihood and the weak point mass.

### hnorm mu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, the empirical squared residual lies strictly between the minimum and zero-tilt mean on the effective window. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

**Failure (exact).** Collapse at the lower effective cut or bounded effective uniform. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

**Uniqueness (exact).** Unique by strict monotonicity of the squared-residual mean. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-mu-truncateknown)

### hnorm mu+sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

### hnorm mu+sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

**Null effective window (exact).** A constant at the held location with fixed lower cut at or below that location has a null tight event. Increasing the estimated upper cut above the location and then shrinking it back gives divergent likelihood and the weak point mass.

### hnorm mu+sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-musigma-truncateknown)

### hnorm sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, exactly the compatible nonconstant samples attain.

**Failure (exact).** A positive-width constant at the lower cut diverges along the remote-location tilt. Coincident cuts follow the compatible shrinking-window rule. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma.

**Uniqueness (exact).** One location from the constrained monotone mean score. The lower cut has a plateau exactly when the fitted location equals the minimum. A free cut ties exactly on the zero-removed-mass plateau.

### hnorm sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, all samples except constants at a finite window cut. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-sigma-truncateknown)

**Failure (exact).** Collapse at the lower cut by remote location or at the upper cut by shrinking support. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-sigma-truncateknown)

**Uniqueness (one-sided).** At most two finite maximizing parameter vectors, one per branch. Compare the attained branch winners, counting coincidences once. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hnorm-sigma-truncateknown)

**Null effective window (exact).** A constant at the estimated upper cut has divergent likelihood whenever the fixed lower cut permits the data. Take the parent location at the constant and let the estimated upper cut approach it from above.

### hnorm sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, exactly the compatible nonconstant samples attain.

**Failure (exact).** A positive-width constant at the lower cut diverges along the remote-location tilt. Coincident cuts follow the compatible shrinking-window rule. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma.

**Uniqueness (exact).** One location from the constrained monotone mean score. The lower cut has a plateau exactly when the fitted location equals the minimum. A free cut ties exactly on the zero-removed-mass plateau.

### hcauchy all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at a finite upper cut and a feasible sample center tied in strict majority. Otherwise a finite candidate must reach the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy)

**Failure (exact).** Divergent collapse at a feasible strict majority or shrinking support at a finite upper-cut constant. Finite escape can be critical collapse, effective uniform or an exterior inverse-square face. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at a finite upper cut and a feasible sample center tied in strict majority. Otherwise a finite candidate must reach the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy)

**Failure (exact).** Divergent collapse at a feasible strict majority or shrinking support at a finite upper-cut constant. Finite escape can be critical collapse, effective uniform or an exterior inverse-square face. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Null effective window (exact).** A constant at the estimated upper cut has divergent likelihood whenever the fixed lower cut permits the data. Take the parent location at the constant and let the estimated upper cut approach it from above.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at a finite upper cut and a feasible sample center tied in strict majority. Otherwise a finite candidate must reach the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy)

**Failure (exact).** Divergent collapse at a feasible strict majority or shrinking support at a finite upper-cut constant. Finite escape can be critical collapse, effective uniform or an exterior inverse-square face. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-all-free-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#upper-cut-half-cauchy)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy mu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-mu-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-mu-truncateknown)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy mu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-mu-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-mu-truncateknown)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Null effective window (exact).** A constant at the held location with fixed lower cut at or below that location has a null tight event. Increasing the estimated upper cut above the location and then shrinking it back gives divergent likelihood and the weak point mass.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy mu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-mu-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior inverse-square escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-mu-truncateknown)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy mu+sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

### hcauchy mu+sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

**Null effective window (exact).** A constant at the held location with fixed lower cut at or below that location has a null tight event. Increasing the estimated upper cut above the location and then shrinking it back gives divergent likelihood and the weak point mass.

### hcauchy mu+sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-musigma-truncateknown)

### hcauchy sigma truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. For compatible location $\mu\le m$, the effective interval is $[m,b]$.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at a finite upper cut. Otherwise a finite candidate reaches the remote-location threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

**Failure (exact).** Shrinking-support collapse at the upper cut or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy sigma truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. For compatible location $\mu\le m$, use effective interval $[\max(a,\mu),M]$.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at a finite upper cut. Otherwise a finite candidate reaches the remote-location threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

**Failure (exact).** Shrinking-support collapse at the upper cut or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Left ray unique (one-sided).** On a left ray with m<b the unique location is m. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

**Null effective window (exact).** A constant at the estimated upper cut has divergent likelihood whenever the fixed lower cut permits the data. Take the parent location at the constant and let the estimated upper cut approach it from above.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### hcauchy sigma truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. For compatible location $\mu\le m$, the effective interval is $[m,M]$.

**Criterion (iff).** On a compatible positive-width tight window, exclude constants at a finite upper cut. Otherwise a finite candidate reaches the remote-location threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

**Failure (exact).** Shrinking-support collapse at the upper cut or bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#hcauchy-sigma-truncateknown)

**Bound cardinality (exact).** For any winning coefficients a free lower cut can be loosened exactly when the fitted location equals the sample minimum. An estimated upper cut is unique at those coefficients.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at a sample level with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-all-free-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at a sample level with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-all-free-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at a sample level with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-all-free-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std mean truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at the held center with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-mean-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std mean truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at the held center with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-mean-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std mean truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at the held center with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-mean-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std mean+nu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannu-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior power of exponent nu+1 escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std mean+nu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannu-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior power of exponent nu+1 escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std mean+nu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie at the held center and a scale root reaches the applicable boundary threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannu-truncateknown)

**Failure (exact).** Divergent or critical collapse at the held center, effective uniform, or exterior power of exponent nu+1 escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std mean+nu+sd truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

### std mean+nu+sd truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

### std mean+nu+sd truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample with positive retained mass. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

**Failure (exact).** No failure on compatible input. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

**Uniqueness (exact).** The feasible parameter set is a singleton. A free cut ties exactly on the zero-removed-mass plateau. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meannusd-truncateknown)

### std mean+sd truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at the held center with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meansd-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std mean+sd truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at the held center with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meansd-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std mean+sd truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at the held center with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-meansd-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std nu truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie and a finite candidate reaches the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown)

**Failure (exact).** Collapse at k > nu(n-k) at a feasible center; critical collapse, effective uniform, or exterior power of exponent nu+1 escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std nu truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie and a finite candidate reaches the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown)

**Failure (exact).** Collapse at k > nu(n-k) at a feasible center; critical collapse, effective uniform, or exterior power of exponent nu+1 escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std nu truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, no divergent tie and a finite candidate reaches the applicable power-family threshold. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown)

**Failure (exact).** Collapse at k > nu(n-k) at a feasible center; critical collapse, effective uniform, or exterior power of exponent nu+1 escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nu-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std nu+sd truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample attains on unbounded windows. On bounded windows a location root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std nu+sd truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample attains on unbounded windows. On bounded windows a location root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std nu+sd truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Criterion (iff).** On a compatible positive-width tight window, every compatible sample attains on unbounded windows. On bounded windows a location root must reach uniform. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown) · [Fixed-window evidence](2026-11-27-mod-rev4-report.md#whole-line-windows)

**Failure (exact).** Bounded uniform escape. Also apply zero-effective-width divergence or incompatibility as specified in the profiling lemma. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-nusd-truncateknown)

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Cardinality.** The coefficient comparison keeps ties. General winning-coefficient cardinality remains open outside the stated cases. Use the profiling lemma for bound ties.

### std sd truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The minimum is at the lower cut. If $b=\infty$, use the right-ray faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at a sample level with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-sd-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std sd truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The maximum is at the upper cut. If $a=-\infty$, use the left-ray faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at a sample level with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-sd-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### std sd truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. On positive width both cuts have observed densities. Use the bounded-window faces.

**Balanced endpoints (one-sided).** On a bounded window with equal positive counts at the two endpoints and no other observations, the excluded uniform strictly beats every finite Student law.

**Bound cardinality (exact).** Whenever a fit attains, each estimated cut is unique at the associated parent coefficients by strictly positive density.

**Divergent tie (one-sided).** A tie of multiplicity k at a sample level with k > 2(n-k) gives divergence. [Fixed-window evidence](2026-11-27-trunc-ls-math-report.md#std-sd-truncateknown)

**Open.** A joint compactification as df approaches two or infinity with varying location or effective scale, followed by finite-to-face comparisons and cardinality.

### unif all free truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif all free truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif all free truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif max truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $\min(u,b)$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif max truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif max truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif max+min truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $\min(u,b)$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif max+min truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $\max(l,a)$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif max+min truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif min truncate:lower-estimated

**Proved window reduction.** Use $[m,b]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif min truncate:upper-estimated

**Proved window reduction.** Use $[a,M]$. The target effective ends are $\max(l,a)$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

### unif min truncate:both-estimated

**Proved window reduction.** Use $[m,M]$. The target effective ends are $m$ and $M$ by the uniform endpoint formulas.

**Criterion (iff).** On compatible data, the two optimal effective ends must have strictly positive separation.

**Failure (exact).** If the target effective width is zero and a compatible positive-width pair exists, the likelihood diverges with a weak point-mass limit. Otherwise a missing compatible pair is infeasible.

**Uniqueness (exact).** The entire joint maximizing set consists of the admissible preimages of the two optimal effective ends under clipping (including inward rounding for counts).

## Summary

Counts below are for estimated-bound regimes. The failure and uniqueness columns give complete and one-sided main claims. Scoped case rows are additional.

| Family | Held masks | Regimes | Attainment iff | Failure complete / one-sided | Cardinality complete / one-sided |
| --- | ---: | ---: | ---: | ---: | ---: |
| bbinom | 6 | 18 | 18 | 18 / 0 | 0 / 9 |
| bnbinom | 8 | 24 | 24 | 24 / 0 | 3 / 0 |
| cmb | 8 | 24 | 24 | 24 / 0 | 24 / 0 |
| cmp | 4 | 12 | 12 | 12 / 0 | 12 / 0 |
| dlaplace | 4 | 12 | 12 | 12 / 0 | 12 / 0 |
| dnorm | 4 | 12 | 12 | 12 / 0 | 3 / 0 |
| dunif | 4 | 12 | 12 | 12 / 0 | 12 / 0 |
| dweibull | 4 | 12 | 12 | 12 / 0 | 6 / 0 |
| hyper | 7 | 21 | 21 | 21 / 0 | 9 / 0 |
| lgser | 2 | 6 | 6 | 6 / 0 | 6 / 0 |
| nhyper | 7 | 21 | 21 | 21 / 0 | 9 / 0 |
| pig | 4 | 12 | 12 | 12 / 0 | 3 / 0 |
| skellam | 4 | 12 | 12 | 12 / 0 | 3 / 0 |
| yulesimon | 2 | 6 | 6 | 6 / 0 | 3 / 3 |
| laplace | 4 | 12 | 12 | 12 / 0 | 9 / 0 |
| logis | 4 | 12 | 12 | 12 / 0 | 3 / 3 |
| cauchy | 4 | 12 | 12 | 12 / 0 | 3 / 0 |
| gumbel | 4 | 12 | 12 | 12 / 0 | 6 / 0 |
| hnorm | 4 | 12 | 12 | 12 / 0 | 10 / 2 |
| hcauchy | 4 | 12 | 12 | 12 / 0 | 3 / 0 |
| std | 8 | 24 | 12 | 12 / 0 | 3 / 0 |
| unif | 4 | 12 | 12 | 12 / 0 | 12 / 0 |

<!-- End generated regimes -->

## Implementation audit

**Source inspection.** There are zero implemented estimated-window routes for the requested families and masks. The [design](../../design/modifiers.md) lists general truncation only for pois, geom, norm, binom, held-size nbinom and finite signed zeta. The R dispatcher checks the family metadata before fitting. The common dispatcher rejects the unsupported families, with lgser reaching a separate refusal for truncation; its zero-modified route is separate. Consequently there are no fit-status disagreements on implemented routes in scope. For an exact coverage example, `mllgser(c(1,2), truncate=c(NA,NA))` is reserved; the theorem gives excluded harmonic law $(2/3,1/3)$, since $3/2>4/3$. For an attained coverage example, `mlunif(c(0,1), truncate=c(NA,NA))` is reserved; mathematically every parent interval containing $[0,1]$ fits at the canonical cuts with likelihood one. These are implementation gaps under the documented scope. [R metadata gate](../../../bindings/r/R/modifiers.R#L57) · [Common dispatcher](../../../bindings/common/modifiers.hpp#L92)

For the implemented families outside this task, the earlier audit covers exact witnesses for the positive-window aliases, signed zeta restrictions, binomial short-window ties, the fixed-positive-size NB weighted boundary, and the normal finite comparison margin. The `[1,Inf]` zero-truncation alias dispatch is for pois, geom, binom and nbinom. Its looser-window extensions do not change any of the present 312 mathematical regimes. [Earlier route audit and exact inputs](2026-11-27-trunc-est-math-report.md#implementation-audit) · [Independent verification](2026-11-27-mod-rev5-report.md)

## What this changes for the fitter

Resolve each free cut to the corresponding sample extreme and preserve the distinction between a nominal cut and the parent support endpoint. Use retained mass and pointwise compatibility before calling a fixed-window optimizer. Dispatch count singletons and continuous zero-effective-width cuts before moment or score checks. Integer bounds, parameterized supports and included atoms require an explicit inclusion test when comparing a finite fit with an excluded likelihood limit.

A fitter can return canonical cuts even when other cuts tie. Coefficient cardinality and nominal-bound cardinality describe different parts of the optimum. An implementation of CMB can use identified coordinates within each integer slice and recover the full finite fibers afterward. Half-normal can use one concave chart whenever the lower cut resolves to $m$. An unresolved global comparison leaves a successful, checked search uncertified. A proved winning excluded face or a failed search triggers the corresponding stop.

## Open

- **Integer designs and beta mixtures.** For beta-binomial, hypergeometric and negative hypergeometric, the comparison with the large-design envelope needs a signed tail bound or a finite equality decision beyond the short-window and bounded-design cases. Beta-negative-binomial rays still need competitive root isolation, including ties beyond the known label swap.
- **Other count profiles.** Larger proper windows leave competitive root isolation and exact finite-to-face equality open for grouped normal, free-shape discrete Weibull, PIG and Skellam. For Yule–Simon on finite windows the polynomial gives a finite decision, while a direct sample inequality and a general singleton theorem remain open. The CMB per-slice proof gives a bounded exact comparison; efficient certification across its size range remains a computational question.
- **Location-scale families.** The strict convexity or strict concavity arguments identify the specified branches. On more general windows, Laplace free-rate comparisons, logistic and Gumbel inverse-rate roots, power-family critical and exterior faces, and upper-only half-normal branch ties still need the root isolation or equality arguments identified in the [location-scale review](2026-11-27-mod-rev4-report.md#open-by-family). Upper-only with the fixed lower cut at the sample minimum also admits the single half-normal chart.
- **Student with free degrees of freedom.** Outside the proved strict divergent ties, compatible coincident cuts and the balanced-endpoint cases, a joint bound is still needed as df approaches two or infinity while the location or effective scale moves. In particular the strict condition $k>2(n-k)$ leaves equality open, and a fixed sd and fixed mean require comparing the Gaussian and near-two limits to all intermediate degrees of freedom. These masks lack a general positive-width attainment, failure or cardinality theorem.

## Verification

The workstation single-slot Python check passed exact rational and symbolic witnesses and a numerical replay at 70 and 100 decimal digits. The logistic grid used inverse rates $1/8,1,2,8$ and transformed locations $1/8,1,8$ on $[0,1]$. The half-normal upper-cut shrink used widths $10^{-1},10^{-2},10^{-3}$ with location moving up to the cut and with the cut moving down to held location. In both paths the checked density times width approached one. Numerical results illustrate the paths; the equalities, inequalities and limits above use the analytical arguments. The report and proof-row replay and the repository atlas validator passed on the workstation in single-slot Python runs. They check masks, route scope, keys, directions and anchors. [Witness script](2026-11-28-trunc-est2-math-data/check.py) · [Numerical table](2026-11-28-trunc-est2-math-data/checks.csv) · [Proof rows](2026-11-28-trunc-est2-math-data/proofs.csv)
