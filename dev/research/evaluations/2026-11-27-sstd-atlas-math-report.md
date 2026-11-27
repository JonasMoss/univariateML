# Fernández–Steel skew Student likelihood boundaries and finite ties

TASK-401 · Base `fc24c05b`

**Proved.** Every hold mask has a joint sample-likelihood envelope and an exact attainment criterion in terms of its boundary values and finite stationary points. At fixed degrees of freedom the finite stationary comparison reduces to finitely many semialgebraic components. At free degrees of freedom the criterion is a root-existence test with an exact inequality, whose general numerical decision remains open.

**Refuted.** Universal all-free uniqueness fails on an exact six-level dyadic sample. The same sample refutes uniqueness for every mask with free skew. The proof establishes finite attainment and excludes every reflection-fixed fit. Reflection then gives at least two distinct finite global maximizers.

## Prior results and scope

The [family dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) supplies the moment equations. [TASK-157](2026-11-08-sstd-moment-report.md) gives the corresponding stable gamma-ratio evaluation. [TASK-116.11](2026-11-06-sstd-cert-report.md) gives the symmetric scale and two-location proofs and identifies asymmetric scale multimodality. [TASK-116.48](2026-11-08-sstd-free-report.md) gives a fixed-moment skew certificate. The [earlier closure proof](2026-11-07-skew-theory-data/proofs.md#5-a-complete-list-of-sstd-escape-coordinates-in-the-strict-tie-regime) and [TASK-252](2026-11-25-tcert-math-report.md#fernandezsteel-sstd) supply the strict-mass bounds and the fixed-skew scale score. [TASK-264](2026-11-25-sstdsged-math-report.md#criterion-and-sharp-value) gives the critical collapse value. [TASK-347](2026-11-25-sstd2-math-report.md#moving-exact-half-contacts) settles the moving half contacts and corrects the earlier ordinary-width normal-end mean derivative. The results below use the corrected derivative. The [tie classification](2026-11-25-ties-math-report.md#classification-of-the-constructions) distinguishes exact reflection from a real crossing or a numerical near-tie. In particular, the real critical finite/collapse tie in TASK-264 uses a non-dyadic sample.

The [Lean map](../existence-atlas/lean-map.csv) and [held-sweep supplement](../existence-atlas/lean-map.d/lean-heldsweep.csv) give the sstd fully held singleton and the symmetric Student scale results. The [TDist proofs](../../lean/UnivariateML/Family/TDist.lean#L459) transfer to exactly held unit skew. At asymmetric skew, holding the public mean couples the join to the scale. The [atlas](../existence-atlas/overview.md) previously recorded strict all-free closure and a partial held-scale criterion for mean and both shapes held. The [likelihood closure lemma](../ml-behaviour.tex#L80) is used with the constrained parameter set and the joint product at the observations. The domain is the dossier's finite public coefficients, with valid holds and a nonempty finite real sample. The product likelihood is used throughout, with zero representing log likelihood $-\infty$.

## Joint envelope

**Proved.** Write $q=\nu$, $w=1/(1+\xi^2)$, $z=1-2w$, and use total ordinary width $T>0$. For join $c$, the two widths are $wT,(1-w)T$. Let $g_q$ be the ordinary unit-scale t density, $K_q=2g_q(0)$, $m_q=E|T_q|$, and $v_q=q/(q-2)$. Use $w_e=w$ on the left and $1-w$ on the right. Put
$$
 F_q(r)=(1+r^2/q)^{-(q+1)/2},\qquad
 f(x)=K_q T^{-1}F_q((x-c)/(Tw_e)).
$$
At $c=x$ the kernel factor is one. The peak coefficient satisfies $K_q\le\sqrt{2/\pi}<1$ by Jensen’s inequality applied to the square root of a gamma variable of mean $q/2$, divided by that mean inside the square root. At infinity take $F_\infty(r)=e^{-r^2/2}$, $m_\infty=\sqrt{2/\pi}$, $v_\infty=1$, $K_\infty=\sqrt{2/\pi}$. The moment equations are
$$
 \mu=c+m_qzT,\qquad
 \sigma^2=T^2 V(q,z),\qquad
 V(q,z)=\frac{v_q}{4}(1+3z^2)-m_q^2 z^2 .
$$
Since $m_q^2<v_q$, $V\ge v_q(1-z^2)/4+(v_q-m_q^2)z^2>0$. The uniform limiting positive variance factor at infinity also follows from this inequality. With sd held, $T=\sigma/\sqrt V$ tends uniformly to zero at two and stays bounded away from zero on every closed shape interval separated from two, uniformly in skew. With mean held, $c=\mu-m_qzT$, and $m_q$ is bounded on the closed shape interval. A held skew fixes one interior $w$.

Compactify in $(c,T,q,w)\in[-\infty,\infty]\times[0,\infty]\times[2,\infty]\times[0,1]$, taking the closure of the feasible set. The likelihood is $C^1$ in the free public coefficients in the finite domain because both first derivatives at the join agree. The envelope at a closure point is the infimum over its neighborhoods of the suprema of the feasible likelihood therein. This definition gives upper semicontinuity since any strict upper bound in one neighborhood also holds on a smaller neighborhood of any sufficiently close point. The finite likelihood agrees there by continuity. Subsequence compactness then gives an envelope maximizer, and the envelope and finite-domain suprema agree.

### Finite positive width

**Proved.** At $0<T<\infty$ with finite join and $0<w<1$, the envelope is the ordinary two-piece likelihood at the limiting coordinates. It is an excluded two-piece $t_2$ face **D** when shape and sd are free and $q=2$, or an excluded two-piece normal face **N** when shape is free and $q=\infty$. The moment equations constrain each face with the corresponding limiting constants. Positive width at two requires sd free, since $V\to\infty$. Fixing the free join and width, or using the moment equations for their held counterparts, constructs the required paths. If skew is held, **N** and **D** use exactly that skew. At unit skew they are ordinary normal and $t_2$.

With skew free, **H** denotes both half faces at the relevant shapes, together with their **D** or **N** intersections when available. Discuss the right half $w=0$, and reflect for $w=1$. Let $L=\min_i x_i$. At limiting $c_0>L$ the product tends to zero. At $c_0<L$ it is the ordinary half-kernel product with width $T_0$. At $c_0=L$, all $j=\#\{x_i=L\}$ contacts have the same thin-side limiting penalty. If
$$
 R=\lim (c-L)_+/(wT)\in[0,\infty],
$$
their combined factor relative to the closed broad-side likelihood is $F_{q_0}(R)^j$. Along supported approaches the broad-side ratio tends to zero. Along unsupported approaches the displayed ratio gives the exact factor, including zero when it diverges. For a general sequence pass to subsequences on which $R$ converges in the extended interval. This is a joint penalty for all tied contacts. The following smallest achievable ratios give the sharp envelope.

- **Mean free.** Take $c=L$ for a contact. This is feasible even when sd fixes $T$. For fixed half shape and width the best supported join is $L$ by monotonicity of every broad kernel. The reflected left half uses the sample maximum.
- **Mean held, sd free.** On the right half $c_0=\mu-m_{q_0}T_0$, so positive support requires $T_0\ge(\mu-L)/m_{q_0}$. At contact, $T=(\mu-L)/(m_qz)$ keeps $c=L$ along the path. It stays positive since $\mu-L=m_{q_0}T_0>0$. These two cases give $R_{\min}=0$, including at two and infinity.
- **Both moments held, shape held.** With $T_0=\sigma/\sqrt{v_q-m_q^2}$, $c_0=\mu-m_qT_0$, differentiate the moment equations to obtain $c_w(q,0)=m_q T_0 v_q/[2(v_q-m_q^2)]>0$. At contact the forced ratio is $R_{\min}=m_qv_q/[2(v_q-m_q^2)]$. Strict support keeps the ordinary value and strict lack of support gives zero.
- **Both moments held, free shape with $2<q_0<\infty$.** The prior [moving-contact result](2026-11-25-sstd2-math-report.md#moving-exact-half-contacts) gives $c_q(q_0,0)<0$. For Student, $m_q^2/v_q=(q-2)[\Gamma((q-1)/2)/(\sqrt\pi\Gamma(q/2))]^2$ strictly increases, since the digamma difference in its log derivative is bounded by $1/(q-3/2)<1/(q-2)$ from $1+e^{-u/2}\ge2e^{-u/4}$ in the integral for that difference. Then $c(q(w),w)=L$ has an admissible branch $q(w)=q_0-c_w(q_0,0)w/c_q(q_0,0)+O(w^2)$. Thus $R_{\min}=0$. With $q-q_0=\eta w+o(w)$, the actual contact ratio tends to $(c_w+c_q\eta)_+/T_0$. The contact constraint on the half shape itself is the monotone test $c(q,0)\le L$.
- **Both moments held, free shape at infinity.** Put $h=1/q$, $m=m_\infty$, $T_0=\sigma/\sqrt{1-m^2}$, $J=T_0 m/[2(1-m^2)]$. The [corrected normal-end derivatives](2026-11-25-sstd2-math-report.md#1-uniform-comparisons-at-the-student-normal-end) give $c_w=J$ and $c_h=J/2>0$. These follow from $m'(0)=3m/4$ and $v'(0)=2$ in the moment equations. At contact, any finite ratio $h/w\to\lambda$ gives $R\to J(1+\lambda/2)/T_0$. If $h/w\to\infty$, the ratio diverges. Consequently $R_{\min}=J/T_0$, reached along $h=o(w)$. The Gaussian contact penalty is $e^{-j(J/T_0)^2/2}$. The closed broad value generally overstates this envelope.

These are all possibilities at finite positive width. In particular a half-$t_2$ intersection has infinite variance and needs sd free. At regular and half faces the densities converge almost everywhere to the indicated probability density. Pointwise convergence except at the join and unit integrals imply total variation convergence by integrating the minimum of the limiting and approximating densities and applying dominated convergence.

### Vanishing width and other ends

**Proved.** Let $k_y=\#\{x_i=y\}$ for a possible concentration point $y$. If mean is held only $y=\mu$ is possible. If it is free, only sample atoms can contribute a positive collapse value. Write $k=\max_y k_y$ with those constraints, allowing $k=0$ when the held mean is absent from the sample. A collapse of interest is possible if sd or shape is free. Define $q_*=\nu$ if held, and $q_*=2$ otherwise, and use the comparison
$$
 E=q_*(n-k)-k .
$$
When shape and sd are held, positive total width is bounded away from zero and the exponent is irrelevant.

For bounded shape use denominator width $S=\sqrt q\,T$ and $h_q=m_q/\sqrt q$, $\widetilde K_q=\sqrt q K_q$. As $c\to y$ and $S\to0$, the exact product is
$$
 S^{q(n-k_y)-k_y}\widetilde K_q^n
 \prod_{x_i\ne y}\frac{w_{e_i}^{q+1}}
 {[(x_i-c)^2+S^2 w_{e_i}^2]^{(q+1)/2}}
 \prod_{x_i=y}\left[1+\left(\frac{y-c}{Sw_{e_i}}\right)^2\right]^{-(q+1)/2}.
$$
The off-atom factors after extracting the weights converge uniformly as $w$ varies, since their distances stay separated from zero. With mean held the last factors are evaluated at $(y-c)/S=h_qz$, on the broad side at the skew extremes. With mean free they are at most one and $c=y$ achieves one. If $k_y=0$, all factors are off-atom. This factorization also gives the subsequential value along rate-dependent paths, including a moving join or a skew approaching an endpoint. With held sd at two, $S\asymp\sqrt{q-2}$ uniformly in $w$, so $(q-2)\log S\to0$.

- $E<0$ gives unbounded likelihood. Fix any admissible finite skew, take the exact join or enforce the held mean, and let $S\downarrow0$ at held $q$ or at some sufficiently small $q>2$ when sd is free. If sd is held, take $q\downarrow2$ using the variance equation. More precisely, a bounded-shape zero-width point with local exponent $q_0(n-k_y)-k_y<0$ has infinite envelope. Reach an extreme skew slowly enough that its powers are subdominant. With sd free and $q_0>2$ free, zero local exponent also permits infinite envelope by taking $q\uparrow q_0$ from below and making $S$ shrink fast enough. The fixed-mean or exact-join path again provides the tie factors. A strictly positive local exponent gives zero envelope.
- $E>0$ makes every bounded-shape collapse vanish, by the factorization and $w_e\le1$. It also vanishes at $E=0$ except at the critical atom and $q=q_*$. At $E=0$, the critical atom is unique since $q_*\ge2$ and $k_y>n/2$. Its sharp envelope at each limiting $w$, denoted **C** before maximizing over $w$, is
$$
 C(w)=\widetilde K_{q_*}^n\!
 \prod_{x_i\ne y}|x_i-y|^{-(q_*+1)}
 w^{(q_*+1)n_-}(1-w)^{(q_*+1)n_+}\,J(w)^k .
$$
Here $n_-,n_+$ count off-atom observations by side and an exponent zero contributes one. $J=1$ for mean free. For mean held, $J=[1+(h_{q_*}z/w_{\operatorname{sign}z})^2]^{-(q_*+1)/2}$, with $J=1$ at $z=0$. Use the admitted $w$ or maximize over the closed interval. At $q=2$, $\widetilde K_2=h_2=1$. Extra powers $S^{(q-2)(n-k)}\le1$ near the lower shape end can only decrease the value. The bound is attained in the limit by requiring $(q-2)|\log S|\to0$. Choose such a path directly when sd is free and use the variance relation otherwise. At a skew endpoint one may let $w$ approach slowly so that $(q-2)|\log w_e|\to0$. If an off-atom count is on the disappearing side, the envelope there is zero. With free mean and skew the optimizing weight is $n_-/(n-k)$. With held mean, the log derivative of the expression is rational on the two sign branches. This recovers the earlier critical formula and proves its pointwise envelope interpretation. [Critical-mass proof](2026-11-25-sstdsged-math-report.md#criterion-and-sharp-value)

At $q\to\infty,T\to0$, any separated observation forces zero product. Indeed, for fixed $u$, $q\log(1+u^2/q)$ increases in $q$. For all $q\ge p>n$, its kernel is bounded by $(1+u^2/p)^{-p/2}$, where $u\ge |x-c|/T$; the product has the peak bound $T^{-n}$ times that one penalty. For a constant sample with allowed concentration point, the ordinary peak on an exact-join path, or a bounded broad-side shift at held mean, gives infinity if this corner is reachable. That sample already has $E<0$ whenever collapse is possible. At $T\to\infty$ the peak bound $K_q/T\le1/T$ gives zero, uniformly in all other coordinates. At $|c|\to\infty$ and $T$ bounded away from both extremes, there is a separated residual tending to infinity, giving zero by the same monotone kernel bound with $p=2$. At $|c|\to\infty, T\to0$ every residual is eventually separated by at least one, and that bound applies to every observation to give zero even with small total width. This exhausts the compactified coordinates, including constant samples and joins escaping together with a width or shape. Positive-width paths give the probability limits described above. Zero width at finite join gives a weak point mass since the ordinary kernels are uniformly tight by the $p=2$ bound.

## Attainment and candidate sets

**Proved.** If the envelope contains infinity, the supremum is infinite and unattained. Otherwise define $B\ge0$ to be the maximum boundary product (use zero for an empty or entirely zero boundary) over the admitted **N**, **D**, **H**, **C** and the zero ends. Let $\mathcal S$ be the zero set of the score in the free public coefficients and let $P(\theta)$ denote the product. Then
$$
 \text{attainment}\quad\Longleftrightarrow\quad
 \exists\theta\in\mathcal S\text{ with }P(\theta)\ge B .
$$
A finite witness strictly above $B$ gives an interior global maximum by compactness. Conversely an attained maximum is stationary and at least $B$. A stationary point at level $B$ either attains the supremum or a higher finite maximum exists. If there is no stationary point at or above $B$, the supremum is attained only in the closure. If finite and boundary supremum values are equal, a stationary point at $B$ is necessary and sufficient for a finite maximizer. A real example where such a stationary point exists is the six zeros, $-1,d_*$ construction in [TASK-264](2026-11-25-sstdsged-math-report.md#which-model-wins). The unit-skew case at the same critical multiplicity has no such point. The earlier rational-input exclusion for the specified asymmetric critical mask remains valid. Failure modes are exactly the excluded points at the largest envelope value when there is no finite winner. When a finite maximizer and a face share the maximum value, attainment takes precedence. The complete finite maximizing set consists of all stationary points at the greatest stationary value, provided that value is attained and at least $B$.

The comparison yields simple sufficient checks. Negative reachable exponent rules out existence. No positive boundary product gives attainment for any finite starting fit. A strict finite witness above a face upper bound excludes the face. Beating all faces gives attainment. For **H** with both moments held, support feasibility on the right requires $\min x_i\ge\mu-m_q\sigma/\sqrt{v_q-m_q^2}$. The left condition follows by reflection. The ratio $m_q/\sqrt{v_q-m_q^2}$ is bounded above by its normal limit, so sufficiently extreme observations in both directions exclude both half faces across all shapes. The constrained normal and lower faces remain separate tests.

### Fixed-shape component comparison

**Proved**, using the [prior fixed-shape reduction](2026-11-07-skew-theory-data/proofs.md#several-of-msxi-free-nu-held). At fixed $q$, choose $c,T,w$ as coordinates, eliminating $c$ by the mean equation when held, inserting $T=\sigma/\sqrt V$ when sd is held, and inserting the specified $w$ when skew is held. Keep $T>0$ and $0<w<1$; the positive square root can be adjoined by $T^2 V=\sigma^2$. Split at the finitely many equations $c=x_i$. Derivatives of the log likelihood are rational in the remaining coordinates and the adjoined width after substituting the differentiated moment equations. For example, the logarithm contains $-n\log T$ and a constant multiple of a sum of logs of positive rational functions of the residuals and side widths. Differentiating $T^2V=\sigma^2$ uses $T_w=-TV_w/(2V)$, so no division by zero occurs in the charts. Both branches give the same first derivative at a join. Clearing positive denominators therefore makes the *entire* stationary set semialgebraic in these coordinates, over the field of the real data and hold constants.

A semialgebraic decomposition gives finitely many connected components with piecewise smooth paths joining points in each. The score is zero along those paths, so each component has constant likelihood. Compare one representative from each with the boundary and retain the full components of equal highest admissible value. This gives the full maximizing set, including a possible positive-dimensional set. At rational $q$, if desired, clear rational powers of the likelihood divided by $K_q^n$ to make stationary-value comparisons polynomial as well, with the moments treated as supplied real constants. For general held real $q$, the component proof is exact with real-value comparisons. An effective equality test for arbitrary inputs and real powers remains open. In the mean/shape/skew held case the scalar polynomial in the [earlier derivation](2026-11-25-tcert-math-report.md#fernandezsteel-sstd) has degree at most twice the distinct sample size on each join interval. Its nonzero constant coefficient gives a finite root set even at critical collapse.

## Reflected finite maxima

### Six-level sample and exact separation

**Proved with a rational box cover and outward interval comparisons.** Let $M=R=2^9$. Use the multiset
$$
 X=\big(-R,\,(-7/4)^{[M]},\,(-1)^{[10M]},\,
                   1^{[10M]},\,(7/4)^{[M]},\,R\big),
 \qquad n=22M+2,
$$
where a bracketed exponent is a multiplicity. All inputs are exact dyadics. The same construction uses held values $\mu=0,\sigma=9/8,\nu=31$ whenever those coefficients are held. Let $a/b=4$ for the ordinary left and right widths, so $z=-3/5$, and choose $T=(9/8)/\sqrt{V(31,z)}$ and $c=m_{31}(a-b)$. This finite witness matches all three holds. Its geometric mean likelihood exceeds $233/1000$. The reflection map is $(\mu,\sigma,\nu,\xi)\mapsto(-\mu,\sigma,\nu,1/\xi)$ and fixes precisely the centered unit-skew subfamily. The following simultaneous bounds exclude that subfamily and all excluded faces.

1. **Centered and symmetric.** At $x\ne0$, $T'\mapsto g_q(x/T')/T'$ is maximized at $T'=|x|$ as follows by differentiating, and its height is $g_q(1)/|x|$. Moreover $g_q(1)$ increases to $\phi(1)$. Differentiating its log gives half the difference of $\psi((q+1)/2)-\psi(q/2)$ and $\log(1+1/q)$. By the digamma difference integral and the integral of $1/s$ on $[q,q+1]$, the former is $2\int_0^\infty e^{-qu}/(1+e^{-u})\,du$, the latter $\int_0^\infty e^{-qu}(1-e^{-u})/u\,du$. The integrand comparison follows from $2u>1-e^{-2u}$ for $u>0$. Thus the product is bounded by $\phi(1)^{22M}(7/4)^{-2M}$ after dropping the remote factors, each less than one.
2. **Normal shapes.** At least one remote observation is at distance $\ge R$ from the join, and every side width is at most $T$. Thus $P\le T^{-n}\exp[-R^2/(2T^2)]$. Maximizing in $T$ bounds $P^{1/n}\le\sqrt{n/e}/R<\sqrt n/R$. This includes a penalized half-normal contact.
3. **Every half shape.** A right half with positive product has $c\le -R$. All points apart from the leftmost observation have distance at least $D=R-2$ from the join. For the normalized decreasing broad density $f(d)$ on $d\ge0$, $d f(d)\le1$ by integration. Bound $n-2$ factors this way. The leftmost and one other observation together contribute at most $T^{-2}[1+D^2/(2T^2)]^{-1}\le2/D^2$, by the uniform kernel inequality at $p=2$. Hence $P\le2/D^n$. The reflected face has the same bound, and contact penalties can only decrease it.
4. **Two-piece $t_2$.** Use denominator width $S=\sqrt2 T$, where the peak is $1/S$. For $S\le1/32$, reflect to $c\ge0$, leaving at least $11M+1$ residuals of magnitude $\ge1$. Therefore $P\le S^{3(11M+1)-n}=S^{n/2}$. For $S\ge8$ the peak suffices. In between, the exact rational cover below proves that the 22-point bulk has geometric mean likelihood $<23/100$. The two extra factors for the full sample are at most $32^2$ together. This is sufficient since $32^2(23/100)^{22M}<(233/1000)^n$.
5. **Collapse.** The largest atom count is at most $10M$, so $2(n-k)>k$ for both free and held mean. Even at degrees of freedom two, every collapse has value zero.

For the bulk cover, move any join outside $[-7/4,7/4]$ to its nearest endpoint, which increases every affected kernel at fixed widths. Reflect to $0\le c\le7/4$. On a box $[c_l,c_u]\times[S_l,S_u]\times[w_l,w_u]$, bound a left-separated residual by $(c_l-x)/(S_u w_u)$ and a right-separated residual by $(x-c_u)/(S_u(1-w_l))$. For a residual that can meet the join use zero as the bound. For the squared product the peak bound is $S_l^{-44}$ and each distinct observation's penalty bound is $(1+r_{\mathrm{lb}}^2)^{-3\,\mathrm{count}}$. Thus every test against $(23/100)^{44}$ is exact rational arithmetic. Bisect at the midpoint of the longest side relative to the original coordinate widths. Every leaf passed. The entire cover used 35,819 boxes and maximum depth 24.

The remaining comparisons use 80-decimal-digit interval arithmetic. For the finite witness, at $q=2j+1$ the normalizer is $K_q=2\cdot4^j/[\binom{2j}{j}\pi\sqrt q]$ by the half-integer gamma recurrence, and $m_q=K_q q/(q-1)$ by integrating the first moment. The checker substitutes those expressions in the moments and kernels and compares the lower witness interval endpoint to $\log(233/1000)$ and each upper bound to the same threshold. The comparisons use the endpoints, while the [recorded output](2026-11-27-sstd-atlas-math-data/check.txt) displays rounded midpoints. The witness supplies a strict lower bound on the maximizing likelihood. Any hold mask with free skew contains the witness and its reflection, and its feasible boundary has envelope bounded by the larger all-free envelope at excluded coordinates. The hold equations are continuous on the all-free finite domain, so each constrained boundary point also lies on the all-free boundary. Thus the strict comparisons give attained off-symmetry maxima for each of the eight masks. Reflection produces a distinct partner in each mask. The argument certifies at least one orbit of size two. It is a structural tie in the sense of TASK-324.

### Fixed skew and held sd

**Proved previously**, by transferring the [two-point Student result](2026-11-08-std-free-data/derivation.md#nonuniqueness-with-held-standard-deviation-and-free-shape) at unit skew. For $x=(-100,100)$, sd one and skew one, any centered fit has $f(100)\le 3/(2\cdot100^3)$, since $1=2\int_0^\infty u^2 f(u)\,du$ and $f$ is decreasing. The mean-100, shape-three witness has product $4/[\pi^2(1+4\cdot100^2)^2]$, strictly above $9/(4\cdot100^6)$ by the integer comparison $100^6>9(1+4\cdot100^2)^2$ and $\pi<4$. The best normal face is centered by minimizing the sum of squared residuals at sd one, and satisfies the same centered bound. Collapse has exponent $2-1>0$. Thus every global maximizer is finite and off-center and has a distinct reflected partner.

## Regimes

**N** denotes the two-piece normal, **D** the positive-width two-piece $t_2$, **H** the two orientations of the half faces, and **C** collapse. The definitions include the allowed intersections and the exact contact penalties above. Zero ends cannot win because a finite feasible parameter has positive likelihood. A collapse check is always relative to the relevant held or free mean.

### sstd held all free

**Criterion and failure proved.** Compare **N**, **D**, **H** (including half-$t_2$ and half-normal) and **C** by the score criterion. In **H** the optimal join can be at the supporting sample extreme with full height. Collapse at any sample atom is unbounded when $2(n-k)<k$ for the largest atom count, zero when $2(n-k)>k$, and has the weighted critical value when equal. This extends the previous strict-tie closure to its equality and supercritical branches. **Uniqueness refuted** by the six-level sample.

### sstd held mean

**Criterion and failure proved.** Compare **N**, **D**, **H** and concentration **C** at the specified mean. In **N** and **D** the join is $c=\mu-m_q zT$. On the right half $c=\mu-m_q T$ and support requires $T\ge(\mu-L)/m_q$. Its contact is reachable at full height by varying sd, including the lower and normal shapes. **Uniqueness refuted** by the six-level sample with mean zero.

### sstd held mean+nu

**Criterion and failure proved.** Compare the two fixed-shape **H** profiles and **C** at the held mean. The support inequality of the mean-only mask still applies, and the fixed-shape exponent uses the supplied $\nu$. The finite candidate components are given by the fixed-shape comparison. **Uniqueness refuted** by the six-level sample with mean zero and shape 31.

### sstd held mean+nu+sd

**Criterion and failure proved.** The only possible nonzero excluded ends are the fixed-shape **H** pair with $T=\sigma/\sqrt V$. At contact the forced thin-side penalty applies. The skew score, split at observation joins, gives the finite candidate components. **Uniqueness refuted** by the six-level sample at holds $0,31,9/8$.

### sstd held mean+nu+xi

**Criterion and failure proved.** Only **C** at the held mean can have positive boundary likelihood. Positive exponent forces attainment and negative exponent forces divergence. At equality compare the scale-score roots against **C**, keeping equality winners. In the symmetric case the monotone scale score proves that a unique fit exists exactly at positive exponent, with unattained collapse at equality. **Maximizing set proved** for arbitrary supplied skew by comparing the finitely many polynomial scale-score roots as described above. An additional strict-mass singleton test holds if all nonzero $d_i=x_i-\mu$ are on the long side of the held mean. Reflect so $z>0$ and $d_i>0$ on this subset. In log inverse width $t=-\log T$ the score is $n-(q+1)\sum u(u+\beta)/(A+(u+\beta)^2)$, with $u=d_i/T$, $\beta=m_qz$, $A=q(1-w)^2$. Each fraction has derivative in $u$ with positive numerator $(2u+\beta)A+\beta(u+\beta)^2$. Thus the score decreases strictly from $n$ to $-E$, proving a unique root for $E>0$ and unattained collapse for $E=0$. Universal singleton status at asymmetric skew and mixed residuals remains open.

### sstd held mean+sd

**Criterion and failure proved.** Compare **N**, **H** and **C**. Both moments are preserved by substitution. The shape-two boundary has zero width. The right half join decreases with shape, so its support is a shape interval. At an admissible finite-shape contact the full broad height is reachable by increasing shape. At a normal contact on that side only the penalized value is reachable, and strict unsupported nearby finite half shapes give zero. Collapse can occur only as $\nu\downarrow2$ at the held mean. **Uniqueness refuted** by the six-level sample at mean zero and sd $9/8$.

### sstd held mean+sd+xi

**Criterion and failure proved.** The sole finite variable is shape. Compare the hold-compatible **N** endpoint and the **C** lower endpoint at the held mean, using the shape score. A positive exponent removes the lower endpoint from positive likelihood consideration, and a negative exponent gives unboundedness. At equality the small-width constant uses the tied broad-side correction when skew is asymmetric. The earlier $(0,0,1)$, hold $(\mu,\sigma,\xi)=(0,1/100,1/10)$ example strictly improves both endpoints. **Uniqueness open.** The projected score has both moving-join contributions and differentiated gamma ratios at asymmetric skew. The ordering of the finite shape roots remains open.

### sstd held mean+xi

**Criterion and failure proved.** Compare **N**, **D** with exactly the held skew and **C** at the held mean. The conditions on width in **N** and **D** are $T>0$ and $c=\mu-m_q zT$, so these are generally moving-location scale profiles. **Uniqueness open.** At unit skew the conditional scale root is unique for each shape at strict exponent by the monotone score. For asymmetric skew even a fixed-shape scale profile may have multiple local maxima. Comparing the maxima of the profiled shape curves remains open.

### sstd held nu

**Criterion and failure proved.** Compare the two fixed-shape **H** profiles with free width and the supporting extreme join, and **C** at reachable atoms. At equality with a critical extreme atom, the [previous symmetric upper comparison](2026-11-25-sstdsged-math-report.md#which-model-wins) proves unattained collapse. For other samples use the fixed-shape component criterion. **Uniqueness refuted** by the six-level sample at shape 31.

### sstd held nu+sd

**Criterion and failure proved.** The two **H** ends preserve held sd via their finite shape-dependent widths, with join at the supporting extreme. Collapse is excluded. Use the stationary component comparison with both holds. **Uniqueness refuted** by the six-level sample at shape 31 and sd $9/8$.

### sstd held nu+xi

**Criterion and failure proved.** Only **C** at sample atoms can compete with finite fits. Positive exponent guarantees attainment, negative exponent gives divergence, and at equality use the fixed-shape component values. At unit skew the [symmetric geodesic proof](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one) gives unattained collapse at equality and a unique fit at strict positive exponent. **Maximizing set proved** for general skew by the fixed-shape component comparison. At each fixed join and strict exponent the total-width score decreases strictly from $q(n-k_c)-k_c$ to $-n$ as $\log T$ increases. This follows by differentiating each nonzero residual's term $(q+1) r_i^2/(q+r_i^2)$. Ranking the join branches requires the joint component comparison.

### sstd held sd

**Criterion and failure proved.** Compare **N**, **H** at the supporting extreme with held variance widths, their normal corners, and **C** at $\nu\downarrow2$ with $T\asymp\sqrt{\nu-2}$. No positive-width **D** is feasible. **Uniqueness refuted** by the six-level sample at sd $9/8$.

### sstd held sd+xi

**Criterion and failure proved.** The two-piece **N** preserves both supplied values and optimizes the join. The only other positive boundary is **C** at $\nu\downarrow2$, where any atom can be the concentration point. **Uniqueness refuted** by the two-point sample $(-100,100)$ at sd and skew one.

### sstd held xi

**Criterion and failure proved.** Compare **N** and **D** at the supplied skew with both moments free and **C** at sample atoms. With $\xi=1$ this is exactly the standard Student problem including its faces. **Uniqueness open.** The fixed-shape scale root is unique at each join in the strict branch and the symmetric fixed-shape strict problem has a unique location-scale fit. The optimized shape score has no ordering proved here, and the fixed-shape geodesic curvature argument fails at asymmetric skew.

### sstd held nu+sd+xi

This already resolved mask was outside the open-question table. The location optimum always exists because the other coordinates are fixed and the likelihood tends to zero at either infinite join. The rational location score yields a finite set of candidates, since its denominator is positive and the score has leading tail term $-n(q+1)/c$. The [known symmetric two-point example](2026-11-06-sstd-cert-report.md#mathematics-and-numerical-contract) has exactly two means $\pm\sqrt3$ at $x=(-2,2)$, sd one, shape three and skew one.

### sstd held mean+nu+sd+xi

The feasible set is the singleton proved in the Lean held sweep. Evaluating that point gives the attained maximum.

## Summary

| Family | Held | Question | Result |
|---|---|---|---|
| sstd | all free | criterion | proved |
| sstd | all free | failure | proved |
| sstd | all free | uniqueness | refuted |
| sstd | mean | criterion | proved |
| sstd | mean | failure | proved |
| sstd | mean | uniqueness | refuted |
| sstd | mean+nu | criterion | proved |
| sstd | mean+nu | failure | proved |
| sstd | mean+nu | uniqueness | refuted |
| sstd | mean+nu+sd | criterion | proved |
| sstd | mean+nu+sd | failure | proved |
| sstd | mean+nu+sd | uniqueness | refuted |
| sstd | mean+nu+xi | criterion | proved |
| sstd | mean+nu+xi | failure | proved |
| sstd | mean+nu+xi | uniqueness | proved maximizing-set comparison |
| sstd | mean+sd | criterion | proved |
| sstd | mean+sd | failure | proved |
| sstd | mean+sd | uniqueness | refuted |
| sstd | mean+sd+xi | criterion | proved |
| sstd | mean+sd+xi | failure | proved |
| sstd | mean+sd+xi | uniqueness | open |
| sstd | mean+xi | criterion | proved |
| sstd | mean+xi | failure | proved |
| sstd | mean+xi | uniqueness | open |
| sstd | nu | criterion | proved |
| sstd | nu | failure | proved |
| sstd | nu | uniqueness | refuted |
| sstd | nu+sd | criterion | proved |
| sstd | nu+sd | failure | proved |
| sstd | nu+sd | uniqueness | refuted |
| sstd | nu+xi | criterion | proved |
| sstd | nu+xi | failure | proved |
| sstd | nu+xi | uniqueness | proved maximizing-set comparison |
| sstd | sd | criterion | proved |
| sstd | sd | failure | proved |
| sstd | sd | uniqueness | refuted |
| sstd | sd+xi | criterion | proved |
| sstd | sd+xi | failure | proved |
| sstd | sd+xi | uniqueness | refuted |
| sstd | xi | criterion | proved |
| sstd | xi | failure | proved |
| sstd | xi | uniqueness | open |
| sstd | nu+sd+xi | criterion | proved previously |
| sstd | nu+sd+xi | failure | proved previously |
| sstd | nu+sd+xi | uniqueness | refuted previously |
| sstd | mean+nu+sd+xi | criterion | proved in Lean |
| sstd | mean+nu+sd+xi | failure | proved in Lean |
| sstd | mean+nu+sd+xi | uniqueness | proved in Lean |

## Reproduction

The final runs used the workstation, one CPU slot at a time. The exact separation checker used rational box bounds and 80-decimal-digit interval arithmetic for the witness and its face comparisons. The second checker replayed five contact paths and eight collapse paths at 80 decimal digits and steps $10^{-8},10^{-16},10^{-24}$. The largest finest-step absolute log-product discrepancies were $7.15\times10^{-21}$ for contact and $1.28\times10^{-6}$ for collapse. Those path replays are numerical corroboration. The same run checked all 39 proof rows against the atlas masks and report anchors.

```sh
d=dev/research/evaluations/2026-11-27-sstd-atlas-math-data
export PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run python3 "$d/check.py"
/tmp/umvl-cpu/run python3 "$d/check_envelope.py"
```

The exploratory NumPy grid used 181 joins, 151 skew fractions and 191 widths in binary64 to choose the bulk support. The proof uses the separate rational cover. [Reflection checker](2026-11-27-sstd-atlas-math-data/check.py) · [Contact, collapse and row checker](2026-11-27-sstd-atlas-math-data/check_envelope.py) · [Replay results](2026-11-27-sstd-atlas-math-data/envelope-check.txt) · [Exploratory grid](2026-11-27-sstd-atlas-math-data/explore.py) · [Importable proof rows](2026-11-27-sstd-atlas-math-data/proofs.csv)

## What this changes for the fitter

- Screen collapse with the largest reachable multiplicity and the held shape or lower shape two. Negative exponents justify `Unbounded`. At equality compute the sharp joint collapse witness, then compare with every eligible face and finite maximizing component. Use certified `ScaleBoundary` only for a winning, unattained collapse. A finite winner tying the collapse remains an attained fit.
- Enforce the moment equations on each shape and skew face. A proved winning half face carries the orientation, the limiting width and join, and the actual reachable contact value. At fixed shape with both moments held, the contact penalty is forced. With shape free finite contacts allow full height, and normal exact contacts with both moments held retain a penalty. At positive width and shape two, sd must be free.
- Use stationary enclosures to certify global value and coefficients. Exact reflection can establish distinct finite global maximizers after finite attainment and exclusion of the reflection-fixed set. An exact count of all orbits requires covering the remaining stationary regions. Keep cardinality unknown when only the global value is certified. Fraction or algebraic candidate comparisons can be used for rational held shape when the remaining constants have an exact comparison representation.
- A failed global comparison or exhausted certificate budget on ordinary data preserves a successful accuracy-checked search as uncertified. A proved inferior finite search result or a failed search follows the existing R6 failure rules. The nonseparated signs and value comparisons below still need practical covers.

## Dossier follow-ups

- Change the all-free uniqueness statement to “For the symmetric six-level sample in TASK-401, at least two distinct finite global maximizers are exchanged by reflection.”
- Replace the strict-only exhaustive closure claim with the mask-specific envelope and the reachable-atom test. The critical asymmetric scale-only case has an exact polynomial stationary comparison with collapse. The current implementation still stops numerically on asymmetric critical inputs. For the real example tying collapse, attainment must take precedence over a limit-only stop.
- Distinguish a named half-model evaluated at a closed endpoint from the constrained approach likelihood. In particular, the forced half-normal contact value with both moments held includes the penalty, and finite-shape moving contacts can reach full height. The dossier's existing distributional limits remain valid.

## Open

- For **mean+sd+xi**, bound or order all finite shape roots of the constrained scalar score, including asymmetric join crossings and leading coefficient cancellations at the normal end. A demonstration of two local roots would still need an equality and global separation proof to refute uniqueness.
- For **mean+xi**, compare all branches of the optimized free-shape scale profile, including a potentially changing maximizing scale at fixed asymmetric skew. For **xi**, the analogous missing comparison is across the optimized join/scale profiles. Exact unit-skew transport does not prove a singleton across different shapes.
- For free shape generally, the closed-end likelihood and exact stationary hit criterion leave practical high-order collars, degenerate stationary sets, and exact equality predicates for accidental finite/face and finite/finite ties open. The critical and contact formulas supply the values required in those comparisons. The reflection example proves the existence of an orbit of size two. A uniqueness or full set certificate for other samples needs global root exclusion. For general irrational held shape the finite list of component values still needs exact comparisons when their enclosures overlap.
