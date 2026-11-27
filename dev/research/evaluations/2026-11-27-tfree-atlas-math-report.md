# Student t attainment and global ties under holds

TASK-404. The results concern the fitting domains for finite nonempty real samples and valid held coefficients. The eleven requested atlas regimes have exact attainment and failure classifications. Global uniqueness fails in ten of them. Holding only `std` df gives a unique fit whenever it exists. The additional `lst` mask holding both location and scale has a closed-form sample criterion and a unique fit when attained.

## Prior evidence and scope

The [fixed-shape derivation](2026-11-07-std-held-data/derivation.md) proves the critical atom classification for df at least one and the exact symmetric two-point location example. The [free-shape held study](2026-11-08-std-free-report.md#proved-nonuniqueness) proves reflected global ties for `std` held sd and `lst` held subunit df. The [joint certificate report](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons) proves uniform scale and shape bounds and critical collars. Its earlier [two shape maxima](2026-11-25-tcert-math-report.md#an-exact-sample-with-two-interior-shape-maxima) are local. The comparisons in this report give exact global ties on different samples. The [strict-mass comparison](2026-11-04-t-cert-data/derivation.md#attainment-criterion-in-the-strict-mass-regime) already includes feasible equality with a face and the attained Cauchy member.

The atlas and Lean map entries for these masks were checked against [TDist.lean](../../lean/UnivariateML/Family/TDist.lean). The held-location, held-df monotone scale theorem and the joint negative-exponent path are used below, as are the normal limits. The joint negative-exponent theorem gives no converse. The all-free `lst` failure claim about fixed-shape collapse has narrower scope than the full envelope, despite full coverage of its individual Lean path entry. The Lean map fragments add no broader theorem for these masks. The [first location-scale audit](2026-10-29-p0-locscale.md#lst-and-std-arguments) distinguishes the model domain from the fitting floor. The two new shape ties are real crossings in the [tie classification](2026-11-25-ties-math-report.md#classification-of-the-constructions). The fixed-shape reflection ties and the two held-scale reflection examples use exact dyadic inputs.

## Joint likelihood envelope

Write $m$ for location, $s$ for ordinary scale, $v$ for df, $a=s\sqrt v$, $B(v)=\Gamma((v+1)/2)/(\sqrt\pi\,\Gamma(v/2))$ and $A(v)=\log B(v)-\frac12\log v$. Let $\ell$ denote the total log likelihood. In `std`, $v>2$ and $s=d\sqrt{(v-2)/v}$ with public sd $d$. Holding $d$ forces $a=d\sqrt{v-2}$. In `lst`, estimated df ranges over $[1,\infty)$ if location is free and $(0,\infty)$ if location is held. A held `lst` df may be any positive value.

**Proved.** Compactify $(m,s,v)$ by extending each coordinate and taking the closure of the actual feasible surface. The [closure lemma](../ml-behaviour.tex#lem:closure) applies because the joint likelihood is continuous relative to the feasible domain, including at the feasible positive-scale Cauchy end. The following gives the upper semicontinuous envelope at every excluded point of that closure.

- **Infinite location or ordinary scale.** The log likelihood tends to $-\infty$. Each nonzero residual density is at most $1/(2|x_i-m|)$ by symmetry and monotonicity, and each density is at most $\phi(0)/s$ by the Gaussian-mixture peak bound. For optimization a free location can also be clipped to the sample range. Nonconstant data give a strictly inward score at its two endpoints.
- **Positive finite ordinary scale and positive finite df.** Ordinary likelihood is continuous, including at $v=2$ for `std`. That excluded point is a location-scale $t_2$ with the indicated ordinary scale and any held location. It is reachable with free sd by taking $d=s\sqrt{v/(v-2)}$. With sd held, this whole face is absent. The `lst` endpoint $v=1$ at positive scale is feasible when present.
- **Normal limit.** At $v\to\infty$ with finite location and positive finite scale, the extension is the actual-hold Gaussian likelihood, by the normalizer and kernel limits. At $s\to0,\ v\to\infty$, if any limiting residual is nonzero choose $N>n$ and a residual magnitude bounded below by $\eta>0$. The penalty is at least $\frac N2\log(1+\eta^2/(Ns^2))$ because $u\log(1+c/u)$ increases in $u$. This dominates $-n\log s$. The envelope is $-\infty$ there. If every residual vanishes at the limiting location and scale can approach zero, keeping that location gives $+\infty$.
- **Positive finite df and zero ordinary scale.** Write $b=\lim m$, $u=m-b$, $k=\#\{i:x_i=b\}$, $p=n-k$ and $E_v=vp-k$. The exact joint expression is
  $$
  \ell=n\log B(v)+E_v\log a-\frac{v+1}{2}
  \left[k\log(1+u^2/a^2)+\sum_{x_i\ne b}\log((x_i-b-u)^2+a^2)\right].
  $$
  The off-atom part has a finite limit. At limiting shape $q>0$, $E_q>0$ gives $-\infty$ and $E_q<0$ gives $+\infty$ as an envelope, reached along $m=b$. For $E_q=0$ the sharp finite value is
  $$
  C_b(q)=n\log B(q)-(q+1)\sum_{x_i\ne b}\log|x_i-b|
  $$
  when $v=q$ is held or when $q$ is the lower end of the shape domain. The atom term is nonpositive and $(v-q)p\log a\le0$. The constant-location path attains the envelope in the limit by taking $(v-q)|\log a|\to0$. For fixed sd this follows from $a=d\sqrt{v-2}$ and requires $q=2$. If shape and scale are free and $q$ can be approached from below, the critical envelope is instead $+\infty$. For example, take $v=q-\epsilon$, $a=\exp(-1/\epsilon^2)$ and $m=b$. These rules include $k=0$ and $k=n$.
- **Zero df.** This end occurs only for `lst` with $m$ held. If all residuals are nonzero, the bound $f_v(r;s)\le v/(2|r|)$ holds uniformly in $s$, as proved in the [joint certificate report](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons). At positive ordinary scale the peak $B(v)/(s\sqrt v)\sim\sqrt v/(2s)$ also forces $-\infty$, even with ties. At simultaneous zero scale with an exact residual zero the envelope is $+\infty$. For example, take $a=\exp(-1/v^2)$ and use the displayed atom expression together with $B(v)\sim v/2$. At fixed scale this corner is absent.

These are joint limsup values. For instance at critical mass a displacement $u/a\to z$ subtracts $\frac{q+1}{2}k\log(1+z^2)$, and a finite limiting product $(v-q)|\log a|$ subtracts $p$ times that product. At positive df collapse converges weakly to the point mass at $b$. At zero df and $a\to0$, the tail outside any fixed radius $r>0$ is asymptotic to $(2B(v)/v)(r/a)^{-v}$, by comparison with the integral of $y^{-v-1}$ on $[r/a,\infty)$. If $v\log(1/a)\to\tau<\infty$, this leaves mass $e^{-\tau}$ outside every finite radius and gives no proper weak limit. If $\tau=\infty$, the path converges to the point mass. The path proving the infinite envelope above uses this latter rate. At positive-scale normal and $t_2$ points, density convergence gives convergence to the indicated probability law.

## Exact comparison and equality

**Proved.** Let $N$ be the maximum Gaussian log likelihood respecting the location and physical-scale holds, with the free Gaussian scale the RMS residual about the fitted or held location. Below, $C$ is the maximum over eligible critical atoms of $C_b(q)$, taking $-\infty$ for an empty set. Let $T_2$ be the maximum positive-ordinary-scale $t_2$ log likelihood respecting the location hold in the strict-mass branches. Its unique nuisance fit follows from the held-location scale score or the [fixed-shape geodesic theorem](2026-11-04-t-cert-data/derivation.md#fixed-shape-is-a-geodesically-convex-problem). At critical mass the $t_2$ supremum is $C$ by the [critical-atom theorem](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one) or the held-location score.

For finite boundary maximum $F$, form a set $S$ of feasible candidates from the following equations, using the actual coordinates of the mask.

- At an interior point, set every free-coordinate score to zero. For free ordinary scale this reduces to the unique positive scale root at any eligible location with positive exponent. For fixed kernel scale and free location use all real location roots of the [polynomial score numerator](2026-11-25-tcert-math-report.md#1-stationary-points). To take the shape score with sd held, differentiate with $a=d\sqrt{v-2}$.
- At the feasible `lst` restriction $v=1$, include the conditional nuisance maxima, even when a shape score fails to vanish. For fixed scale use all maximizing location roots. With both nuisances free, use the fixed-df classification in the `lst` df section.

With $F=-\infty$ the same convention applies provided $\ell$ is finite at each feasible point. When $F<+\infty$, an MLE exists **iff** there is a candidate in $S$ with $\ell\ge F$. In fact a finite feasible witness of value strictly above $F$ suffices even if it is not stationary. The compact envelope has a maximum. If its value exceeds $F$, it lies in the feasible set, hence among the candidates. If no point exceeds $F$ and a feasible candidate equals $F$, it is a genuine maximizer and the boundary also maximizes the envelope. If all candidates are below finite $F$, the supremum is $F$ and is unattained, even if a sequence of their values approaches it. To recover the full finite maximizing set in an attained case, keep all candidates of largest value. When $F=+\infty$, every finite point has finite density and no MLE exists. This criterion is a comparison of feasible candidate **values**, with a weak inequality. Comparing only the supremum of the candidate or feasible values to a face by weak inequality would lose the equality distinction.

A convenient strict witness at the Gaussian fit is a positive derivative inward from inverse df zero. In total log likelihood it is $\sum(z_i^4-2z_i^2-1)/4$ with ordinary scale held, $\sum(z_i^4-6z_i^2+3)/4$ with sd held, and $\sum(z_i^4-3)/4$ with scale fitted. Here $z_i$ uses the Gaussian fit respecting the holds. The [actual-hold expansion](2026-11-08-std-free-data/signed-derivation.md#first-order-coefficients) proves these formulas. A positive coefficient suffices when $N$ is the highest excluded face. A negative coefficient alone is a local result. An explicit finite witness beating both $N$ and the lower face is sufficient in the two-face masks.

### std held all free

**Criterion and failure, proved.** Let $k$ be the largest multiplicity. For $3k>2n$ the likelihood is unbounded, by taking $v>2$ sufficiently close to two and collapsing at that atom. For $3k<2n$ the excluded maximum is $F=\max(N,T_2)$. For $3k=2n$ it is $F=\max(N,C)$. In the critical case the positive-scale $t_2$ face has no maximum at $C$, by the critical geodesic theorem. The exact comparison above applies. The only finite winning excluded ends are the normal, $t_2$ with positive ordinary scale, and critical collapse with df tending to two. In the strict-mass case this recovers the prior criterion.

**Uniqueness refuted.** The [joint real crossing](#joint-real-crossing) specifies a symmetric sample with two finite global df maximizers greater than two and a unique nuisance fit at each. Transport to public sd is bijective for each such df. Reflection forces the same location at both fits. Equality is between different shapes.

### std held mean

**Criterion and failure, proved.** Use the preceding formulas with $k$ counting observations at the supplied mean only. With $3k>2n$ the likelihood diverges. With $3k<2n$ compare to $\max(N,T_2)$, computing both with that mean held. With $3k=2n$ compare to $\max(N,C)$. At critical mass the held-location score at df two is strictly negative at positive scale, so the lower supremum comes from collapse and no stand-alone positive-scale $t_2$ optimum occurs. No other escape has a competitive value.

**Uniqueness refuted.** The [joint real crossing](#joint-real-crossing) uses mean zero. Its fixed-shape conditional maximizers already have mean zero, so the two global maxima remain feasible and global.

### std held mean+sd

**Criterion and failure, proved.** Let $k$ count observations at the supplied mean, and $d$ be the supplied sd. At $3k>2n$ the likelihood diverges as $a=d\sqrt{v-2}\downarrow0$. At $3k<2n$ the lower envelope is $-\infty$ and $F=N$. At $3k=2n$ compare to $F=\max(N,C)$, with $C=-n\log2-3\sum_{x_i\ne m}\log|x_i-m|$. The constant is independent of the held sd at criticality. The scalar lower-end bound in the [prior derivation](2026-11-08-std-free-data/derivation.md#lower-tail-held-mean-and-standard-deviation) gives strict loss in a punctured critical collar. Global equality is handled by the candidate comparison.

**Uniqueness refuted.** The [scalar real crossing](#scalar-real-crossing) specifies exact data with mean zero and sd one held and two separated finite maximizing df values.

### std held nu

**Criterion, failure and uniqueness, proved.** There is a unique MLE iff $k_{\max}<v(n-k_{\max})$. A negative exponent at a largest atom gives an infinite supremum. At equality the finite supremum is $C$ and is unattained. With fixed $v>2$ the scale conversion is a positive bijection and removes both shape ends. The envelope proof reduces all nontrivial escapes to the critical or negative atom. With all exponents positive it gives attainment. Strict geodesic convexity at fixed $v>1$ gives uniqueness in the attained case. At equality the [critical ray proof](2026-11-07-std-held-data/derivation.md#joint-critical-atom-at-held-shape-at-least-one) shows strict loss at every finite point: the negative log likelihood along a ray to that atom is strictly convex with a finite lower limit, so the limit strictly improves any finite point.

### std held nu+sd

**Criterion and failure, proved.** Every nonempty finite sample has an MLE. The log likelihood tends to $-\infty$ as location tends to either infinity. Full support leaves no contact boundary. All location roots can be compared by the product of their squared distances augmented by $a^2=(v-2)d^2$.

**Uniqueness refuted and full location set characterized.** For $x=(-2,2)$ with $v=3$ and $d=1$, the maximizers are exactly $m=\pm\sqrt3$. For any balanced two-point sample at $\pm b$, the product to minimize is $(m^2+a^2-b^2)^2+4b^2a^2$. The full set is $\{0\}$ if $b^2\le a^2$ and $\{\pm\sqrt{b^2-a^2}\}$ otherwise. This is the [held-shape result](2026-11-07-std-held-data/derivation.md#free-location-fixed-scale-and-shape).

### std held sd

**Criterion and failure, proved.** The formulas for held mean+sd apply with $k=k_{\max}$ and with the normal location optimized. At equality take $C$ at the critical atom. The envelope proof allows moving locations and the [critical held-sd bound](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons) excludes a punctured lower collar below $C$. The possible finite winning excluded ends are the actual-hold normal and critical collapse. A supercritical atom forces divergence. No positive-scale $t_2$ path obeys the sd hold.

**Uniqueness refuted.** The [prior proof](2026-11-08-std-free-data/derivation.md#nonuniqueness-with-held-standard-deviation-and-free-shape) uses $(-100,100)$ with sd one. Every centered candidate, including the best Gaussian, has product likelihood at most $9/(4\cdot100^6)$, by integrating its unit variance up to radius $100$. The shape-three candidate at location $100$ beats this bound because $100^6>9(1+4\cdot100^2)^2$ and $\pi<4$. Strict mass removes the lower end. A finite winner therefore exists off center and reflection yields a distinct winner.

### lst held all free

**Criterion and failure, proved.** Strict majority $2k_{\max}>n$ gives an unbounded likelihood, including at df one. Otherwise $F=\max(N,C)$ with $C$ present at half mass only. There is no zero-df path. The Cauchy is a feasible endpoint at positive scale and must be included when it has a conditional optimum. On balanced two-point data the Cauchy has a ridge but the full free-shape fit is unattained at normal. Indeed, for $v>1$ symmetry and conditional uniqueness force midpoint location and half-gap scale. The profile strictly increases with $v$, by the [balanced Cauchy comparison](2026-11-25-tcert-math-report.md#2-compactness-and-face-comparisons). At half mass with three or more support points the Cauchy value is only an unattained conditional supremum. The full criterion still compares candidates with both $N$ and $C$.

**Uniqueness refuted.** The [joint real crossing](#joint-real-crossing) establishes two global maxima at different shapes strictly above one. The [finite/normal tie](#a-realized-finiteface-equality) separately shows that the excluded normal model can tie a feasible global maximizer.

### lst held df

**Criterion and failure, proved.** With $E=v(n-k_{\max})-k_{\max}$, $E>0$ gives attainment and $E<0$ gives divergence. The envelope proves that at $E=0$ only critical atoms can supply an excluded maximum, with finite $F=C$. At $v>1$ this is the unattained supremum, by the same critical ray proof as for `std`. At $v=1$ it is unattained unless there are exactly two equally occupied atoms $b<c$. Then the full maximizing set is $b<m<c$, $s^2=(m-b)(c-m)$. The score equations or the identity $(D_1D_2)/s^2\ge(c-b)^2$ with equality on that arc establish the curve. At $0<v<1$ and $E=0$, compare the joint score candidates with $C$ using the exact criterion. Both attainment and nonattainment occur in this branch.

For nonattainment take $(-1,0,1)$ and $v=1/2$. The largest collapse face is at zero. With $z=a^2>0$ and $T=m^2+z$, the likelihood raised to the power $4/3$ is proportional to $z/[T((T-1)^2+4z)]$. The denominator exceeds $z$: for $T\ge1/4$ this follows from $T(T-1)^2+(4T-1)z>0$, and for $T<1/4$ the difference is at least $T^2(T+2)>0$ using $z\le T$. The centered collapse reaches the ratio one in the limit.

For attainment take $(0,0,1,9/8,5/4)$ and $v=2/3$. Zero is the only critical atom, with $k=2$. Put $R_j=\sum_{x_i\ne0} x_i^{-j}$ and take $m=a^2 R_1/k$. Expanding the atom formula gives $\ell-C_0=(v+1)a^2(R_1^2/k-R_2)/2+O(a^4)$. The coefficient is strictly positive by the rational comparison in [algebra.py](2026-11-27-tfree-atlas-math-data/algebra.py). Thus a finite point beats the highest collapse face. This is a fixed-df counterexample to a universal equality stop.

In the critical branch the comparison can also be posed entirely algebraically for real algebraic input. Write $v=h/j$ with positive integers $h,j$, as required by criticality. After removing $B(v)^n$ and raising the likelihood to $2j$, compare $z^{nh}/\prod_i D_i^{h+j}$ with $\max_b \prod_{x_i\ne b}|x_i-b|^{-2(h+j)}$. The denominators are positive for $z>0$ and the two scores have polynomial numerators. Thus weak equality is a polynomial feasibility question over the real algebraic inputs. The exact candidate rule also applies to arbitrary real samples.

**Uniqueness refuted.** For $v\ge1$ and strict atom mass the fixed-shape theorem gives a unique attained fit. The balanced Cauchy critical curve already refutes universal uniqueness. Subunit shape refutes it even at strict mass: on $(-100,-99,0,99,100)$ at $v=1/2$ the [prior rational comparison](2026-11-08-std-free-data/derivation.md#exact-rational-version-of-the-nonuniqueness-comparison) shows that $(m,s)=(99.5,0.5)$ beats the whole centered profile. Attainment follows from strict mass and reflection gives two global maximizers.

### lst held df+sigma

**Criterion and failure, proved.** All samples attain and the only escape is infinite location, where likelihood vanishes. With both held values fixed, minimizing $\prod_i[(x_i-m)^2+vs^2]$ over the location score roots gives the full set. This comparison is algebraic for real algebraic input and kernel scale.

**Uniqueness refuted.** For $(-5,5)$ with df one and ordinary scale three, the balanced two-point identity gives precisely $m=\pm4$.

### lst held mu

**Criterion and failure, proved.** With any exact residual zero, a sufficiently small positive fixed df has negative collapse exponent, so the likelihood is unbounded. This branch can also diverge at the simultaneous zero-df and zero-scale corner. With no residual zeros the scale and zero-df ends have envelope $-\infty$. The only competitive excluded end is normal with location held and fitted RMS scale. Consequently $F=N$, and attainment is equivalent to a finite candidate reaching or exceeding $N$. As a checkable case of nonattainment, if all residual magnitudes equal a positive $r$, the scale optimum is $s=r$ at every df. Its shape profile increases strictly to the normal value since its derivative per observation is $A'(v)+(1/v-\log(1+1/v))/2>0$.

**Uniqueness refuted.** The [joint real crossing](#joint-real-crossing) proves the claim under the entire held-location shape range $(0,\infty)$. Its low-df exclusion is essential for this transfer. The [finite/normal construction](#a-realized-finiteface-equality) also applies to this range.

### lst held sigma

**Criterion and failure, proved.** With ordinary scale held and location free, $v\ge1$. The only competitive excluded end is $F=N$ with that scale. The actual Cauchy point or points participate in the candidate comparison. If every observation equals $b$, location $b$ is conditionally optimal and the peak increases strictly to the Gaussian peak, giving simple nonattainment.

**Uniqueness refuted with a full set.** Use $(-100,100)$ and $s=1$. Let $Q=100^2$. For $1\le v\le Q$ the two-point identity gives geometric-mean log likelihood $A(v)-(v+1)\log(4Q/v)/4$, with its maximizers at $m=\pm\sqrt{Q-v}$. Four times its derivative is bounded above by $1+1/v+1/v^2-\log(4Q/v)$ using $A'(v)\le1/(4v^2)$. This is negative on $[1,4]$ by $\log Q>3$, and on $[4,Q]$ by $\log4>21/16$. For $v\ge Q$ the location optimum is centered and its density at residual $100$ is at most $\phi(0)\exp(-Q/4)$ by $\log(1+Q/v)\ge Q/(2v)$. This is strictly below the attained geometric-mean Cauchy density $1/(200\pi)$. Thus the full maximizing set is exactly $(m,v)=(\pm\sqrt{9999},1)$.

### lst held mu+sigma

**Criterion, failure and uniqueness, proved.** This mask is absent from the current atlas but the fitter supports it and it belongs to the free-df audit. Write $q_i=((x_i-m)/s)^2$. A finite MLE exists iff $\sum_i(q_i^2-2q_i-1)>0$, and then it is unique. Otherwise the profile increases strictly to the normal with both coefficients held. Df zero has vanishing likelihood by the peak bound, and no other end is feasible.

For the proof define $b_v(q)=\log(1+q/v)-(v+1)q/[v(v+q)]$. Twice the per-observation shape score is $2A'(v)-\overline{b_v(q_i)}$. Since $b_v(0)=0$ and $\partial_q b_v(q)=(q-1)/(v+q)^2$,
$$
(v+1)^2 b_v(q)=\int_0^q (z-1)\left(\frac{v+1}{v+z}\right)^2 dz .
$$
The integrand increases with $v$, with derivative $2(v+1)(z-1)^2/(v+z)^3$. Meanwhile $(v+1)^2 A'(v)$ strictly decreases: with $\lambda=v+1$ the digamma integral gives
$$
(v+1)^2 A'(v)=\tfrac12\int_0^\infty e^{-y}y\,
   \frac{e^{y/\lambda}\tanh(y/(2\lambda))}{y/\lambda}\,dy .
$$
The function $e^t\tanh(t/2)/t=((e^t-1)/t)/(1+e^{-t})$ increases strictly for $t>0$. The scaled score is therefore strictly decreasing, and positive near zero because $A'(v)$ diverges and each scaled $b_v$ is bounded above on $(0,1]$. Its limit is $(1+2\overline q-\overline{q^2})/2$ by the integral and the displayed expression for $b_v$. This yields a unique finite root exactly at the claimed strict inequality. At equality the score is positive for every finite df.

## Exact crossing proofs

For the two constructions, the relative per-observation log likelihood adds the constant $\tfrac12\log(2\pi)$ to the usual per-observation value. Each row of radii represents both signs, each with the indicated multiplicity. For path parameter $t$, use the exact real affine sample $r(t)=r_0+t(r_1-r_0)$. The common fixed per-sign counts are $(300000,4000000,4000000,4800000,3100000,1)$. Weighted averages below use these counts. No rounded root specifies either sample.

### Joint real crossing

**Proved by interval comparisons and continuity.** Use the following exact decimals.

| $r_0$ | $r_1$ |
| ---: | ---: |
| .047824 | .036672 |
| .828300 | .772301 |
| .047379 | .047294 |
| .162880 | .144816 |
| .337914 | .322601 |
| 326.079611 | 335.645682 |

Use $0\le t\le1$. For `lst` with free location, the strict atom bound holds for every $v\ge1$ because no radius receives half the sample. Conditional geodesic uniqueness forces $m=0$. For the held-location fits, use $m=0$ and the unique positive scale root at every $v>0$. Let $P_t(v)$ be this centered scale profile. The score fixes $z=a^2$ by $(v+1)\overline{q/(q+z)}=1$, with $q=r(t)^2$. Its derivative is $2P'_t(v)=D(v)-\overline{\log(1+q/z)}$ with $D(v)=\psi((v+1)/2)-\psi(v/2)$. The same root and profile apply to all-free `std` on $v>2$.

The [interval checker](2026-11-27-tfree-atlas-math-data/crossing.py) proves the following lower bounds on the gaps. Each path bound applies to the entire interval in $t$.

| Comparison | Certified lower bound |
| --- | ---: |
| $P_t(158)-P_t(30)$ | .0000131 |
| $P_t(5.5)-P_t(2)$ | .00610 |
| $P_t(5.5)-N_t$ | .01452 |
| $P'_t(v)$ on $[.05,2]$ | .000419 |
| $P_0(5.5)-\sup_{[30,4000000]}P_0$ | .00000298 |
| $P_1(158)-\sup_{[2,30]}P_1$ | .00000129 |

For $v\le.05$, the uniform $v/(2|r|)$ bound gives a relative log likelihood below $-1.02$ (use $\tfrac12\log(2\pi)<1$), and a fixed scale $.36$ at df $5.5$ gives a lower bound $.305$. For $v\ge4{,}000{,}000$, the scale score at $s=.3$ is positive since $\overline q/(.09+\max q/4{,}000{,}000)>1$. At any larger scale, $\log(1+u)\ge u-u^2/2$ and $A\le A(\infty)$ give the upper bound $N_t+\overline{q^2}(1+1/v)/(4v(.3)^4)$. The extra term is below $.006047$. The witness at df $5.5$ therefore beats both tails and the interval $[.05,2]$ uniformly.

Define $U(t)=\max_{2\le v\le30}P_t(v)$ and $V(t)=\max_{30\le v\le4{,}000{,}000}P_t(v)$. Both are continuous by compactness and the scale score's strictly decreasing root. The endpoint bounds give $U(0)>V(0)$ and $U(1)<V(1)$. Let $t_*$ be the least root of $U(t)=V(t)$ on $[0,1]$. This definition, together with the exact decimals and counts, specifies the exact sample. At $t_*$ the common maximum exceeds $P_{t_*}(30)$ by the witness at $158$. The lower and upper end exclusions put the two maximizers strictly inside the two regions, and no other shape can improve on their value. This proves the global tie for both families, with free or zero-held location. It leaves the number of additional tied finite shapes unspecified. A [75-digit local replay](2026-11-27-tfree-atlas-math-data/continuation.txt) finds a crossing near $t=.729148156$, with shapes near $5.631656$ and $149.525918$. Those approximations do not identify the least root by themselves.

### Scalar real crossing

**Proved by interval comparisons and continuity.** Hold `std` mean zero and sd one. Use the same counts and these exact decimal radii.

| $r_0$ | $r_1$ |
| ---: | ---: |
| .08387377 | .10722859 |
| 1.62238777 | 1.76965608 |
| .08624025 | .09903796 |
| .37818028 | .38411865 |
| .73883338 | .71640914 |
| 734.657008 | 700.304874 |

Use $0\le t\le1/8$ and set $w=v-2$. The relative per-observation log likelihood is
$$
P_t(v)=A(v)-A(\infty)-\tfrac12\log(w/v)-\tfrac12(v+1)\overline{\log(1+q/w)}.
$$
The [scalar checker](2026-11-27-tfree-atlas-math-data/scalar_crossing.py) proves these comparisons.

| Comparison | Certified lower bound |
| --- | ---: |
| $P_t(135)-P_t(30)$ on the path | .0000193 |
| $P_t(8.25)-N_t$ on the path | .01621 |
| $P_0(8.25)-\sup_{[30,10000000]}P_0$ | .00000120 |
| $P_{1/8}(161)-\sup_{[2.00001,30]}P_{1/8}$ | .00000262 |

For $0<w\le.00001$ let $q_{\min}$ bound all squared radii below on the path. It exceeds $.00001$. The bound $\log w+\frac12\log3-\frac32\log q_{\min}<-3.52$ follows by retaining the leading log kernel penalty. The df $8.25$ witness is above $-.408$. For $v\ge10^7$, the effective ordinary scale squared exceeds $1-2/10^7$. The normal-relative error is bounded by the same kernel inequality as in the joint crossing plus $-\frac12\log(1-2/10^7)$ for the Gaussian peak adjustment, giving an upper bound $.000450$. Both tails are strictly worse. Define $t_*$ as the least equality of the maxima on $[2.00001,30]$ and $[30,10000000]$ on the path. Endpoint signs give existence, and the separator and tail comparisons force two distinct attained global maxima. The [75-digit local replay](2026-11-27-tfree-atlas-math-data/continuation.txt) finds a crossing near $t=.050692404$, with shapes near $8.621623$ and $142.584881$.

### How the crossing comparisons are certified

The two covers use interval arithmetic with 60-decimal-digit precision. The basic operations widen outwards, including the correctly rounded decimal logarithm. The dyadic subdivision endpoints and terminating decimal constants are exact at this precision at the stated subdivision depths. The float scale solve only nominates two decimal endpoints. Interval score signs establish the bracket, and $a^2$ increases with df and with each $q_i$, giving bounds on full cells. A cell value is bounded using its midpoint value plus the interval displacement times an interval derivative, or an endpoint value when the derivative has fixed sign. Path derivatives for the finite witnesses are $-(v+1)\overline{r r'/(a^2+q)}$. The Gaussian path derivatives are $-\overline{r r'}/\overline q$ with fitted scale and $-\overline{r r'}$ with sd one held. The derivative $D$ is decreasing by its integral $D(v)=2\int_0^\infty e^{-vt}/(1+e^{-t})\,dt$. No cell is accepted on a grid sign alone. [Python decimal rounding contract](https://docs.python.org/3/library/decimal.html#decimal.Decimal.ln)

For the normalizer enclosures, the digamma integral gives $A'(v)=\frac12\int_0^\infty e^{-vt}\tanh(t/2)\,dt$. The bounds $u-u^3/3\le\tanh u\le u$ follow by integrating $1-\tanh^2 u$. Thus $A(v)-A(\infty)$ lies between $-1/(4v)$ and $-1/(4v)+1/(24v^3)$, and $D(v)$ lies between $1/v+1/(2v^2)-1/(4v^4)$ and $1/v+1/(2v^2)$. Start at $v+2j\ge80$ and use $\Gamma(z+1)=z\Gamma(z)$ to descend by two. The normalizer subtracts $\frac12\log(1+1/[v(v+2)])$ per step and the digamma difference adds $2/[v(v+1)]$. This supplies the rigorous special-function enclosures used in the covers. [Joint interval results](2026-11-27-tfree-atlas-math-data/crossing.json) · [Scalar interval results](2026-11-27-tfree-atlas-math-data/scalar_crossing.json)

### A realized finite/face equality

**Proved.** In `lst` consider three copies of each of $\pm e$ and two of each of $\pm1$, with $2^{-20}\le e\le1$. Use either all-free or zero-held location. For $e<1$ and df at least one, the all-free conditional fit is centered by strict atom mass and reflection. At $e=1$ the same holds for df above one. The centered point at df one attains the balanced two-point Cauchy ridge value. For either route, the Gaussian standardized fourth moment is bounded above by $1/\overline{r^2}\le5/2$ because all $r^2\le1$ and $\overline{r^2}\ge2/5$. The first inward normal coefficient is strictly negative uniformly. The [uniform tail remainder](2026-11-08-std-free-data/signed-derivation.md#reoptimization-of-the-free-nuisance) then gives a common upper cutoff in df beyond which every finite profile is strictly below its normal value. The held-location scale root remains uniformly away from zero near normal since $|r|\ge2^{-20}$, and the derivative and curvature constants in that proof are bounded on the compact sample path. The zero-df bound also gives a common positive lower cutoff when location is held.

At $e=1$ the entire finite profile is strictly below normal by the equal-magnitude score formula. At $e=2^{-20}$ the feasible Cauchy witness at scale $e$ has product likelihood $[2^6\pi^{10}(1+e^2)^4 e^2]^{-1}>1$ using $\pi<4$, whereas the best Gaussian product is less than one because its variance is at least $2/5$. Thus the maximum profile-minus-normal value over the common compact df interval changes sign. The least crossing from positive to zero as $e$ increases defines an exact real sample in each route. At that sample, the finite maximum equals the normal face. In the all-free case the definition includes the admissible Cauchy endpoint. This construction distinguishes an attained boundary tie from an escape with the same supremum value.

## Summary

The held-mask names below follow the atlas. The dash in the last row records an absent atlas mask.

| Family | Held | Criterion | Failure | Uniqueness |
| --- | --- | --- | --- | --- |
| std | all free | proved | proved | refuted |
| std | mean | proved | proved | refuted |
| std | mean+sd | proved | proved | refuted |
| std | nu | proved | proved | proved |
| std | nu+sd | proved | proved | refuted |
| std | sd | proved | proved | refuted |
| lst | all free | proved | proved | refuted |
| lst | df | proved | proved | refuted |
| lst | df+sigma | proved | proved | refuted |
| lst | mu | proved | proved | refuted |
| lst | sigma | proved | proved | refuted |
| lst | mu+sigma (—) | proved | proved | proved |

## What this changes for the fitter

- Count exact ties at attainable locations, or at the held location only. An exponent of the wrong sign proves unboundedness. At criticality a finite collapse envelope is a comparison value. Fixed df at least one has the separate critical classification, including the Cauchy ridge. Fixed subunit critical mass needs joint candidate comparison and can have an attained fit.
- Preserve the actual holds in every face comparison. The `std` lower limit with free sd is an ordinary-scale `lst` fit at df two when its nuisance maximum exists. With sd held the lower limit is collapse. A normal diagnostic requires the Gaussian fit with the actual scale and location holds. A feasible `lst` df-one winner is a restricted fit.
- Include the excluded face values in a global cover. A finite witness can exclude a face. An equal feasible optimum is attained, so reserve an escape status for cases where no feasible candidate reaches the winning face. If a comparison or cover remains unresolved, retain a successful search marked uncertified. An exhausted or score-failed search supplies no estimate.
- The `lst` shape-only route can use the moment sign and monotone scaled score as its mathematical certificate, then enclose the positive root and meet the ordinary accuracy contract. Floating-point cancellation in the moment sign requires a margin or a higher-precision comparison.
- The real crossings rule out a universal singleton assertion even when every fixed-df nuisance optimum is unique. Report a singleton only after global exclusions and root inclusion, and preserve the full maximizing set when established. Rounded versions of the crossing samples are suitable for near-tie tests, with no assertion of exact equality.

## Dossier follow-ups

- Qualify the `std` fixed-df tie-limit wording. The unattained equality theorem requires that no atom with negative exponent be attainable under the same hold. If any such atom exists, the likelihood is unbounded. At free df and held sd, the df-two end has the three mass branches proved above.
- Expand the `lst` all-free majority statement to the exact finite comparison with normal and critical Cauchy collapse. Make clear that the positive-scale Cauchy member remains feasible. The statement about strict positive exponents at fixed subunit shape guarantees attainment, with global uniqueness refuted even under strict mass.
- Record the missing `lst` `mu+sigma` atlas mask and its moment criterion when the coordinator updates the atlas generator or dossier sources. The staged CSV contains only masks present in the current atlas.

## Open

All requested regime-level propositions have proofs or exact refutations. Computing all maximizers on arbitrary data still requires root isolation through degeneracy, branch changes and certified value comparisons. In particular, the crossing arguments do not count every global maximizing branch or prove that either crossing lies on binary64 inputs. General exact finite/finite and finite/face equality predicates at estimated transcendental shapes remain open. For fixed subunit critical df the algebraic predicate is exact on algebraic inputs, and its practical implementation or a simpler sample-only test remains open.

## Reproduction

The final runs used the workstation through one `/tmp/umvl-cpu/run` slot. The [data directory](2026-11-27-tfree-atlas-math-data/) contains the two interval covers, the 75-digit replay, the exact rational identities and comparisons, and the staged atlas rows. No library, dossier, test, or generated binding changed. No build was run.
