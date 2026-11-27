# Tukey lambda likelihood under five hold masks

TASK-406. The five masks have exhaustive escape classifications and exact attainment criteria. At held negative shape with free location, critical multiplicity leads to a compact comparison in which a finite tie with the collapse face counts as attainment. Every attained held-location, held-shape scale fit is unique. Each of the other masks admits nonunique fits.

## Scope and prior proofs

Take a finite nonempty sample, valid holds, and the R7a endpoint convention. Attainment means a positive finite maximum. An identically zero likelihood gives no positive-likelihood fit. Write \(P\) for the product likelihood. The envelope in the [closure lemma](../ml-behaviour.tex#lem:closure) is the logarithm of the joint product envelope. Estimated shape is in \([-1,1]\) with location free and in \((-\infty,1]\) with location held. Held shape can be any finite real number.

The [atlas proof inventory](../existence-atlas/proofs.csv) has open main entries for all five masks. The Lean maps list the fully held singleton only. The November 9 status matrix named in the brief was absent from this checkout. The dossier, fitter and available reports give the domains and existing behavior. The main prior proofs are the [fixed-shape reductions, envelopes and asymptotics](2026-11-07-tlambda-theory-data/proofs.md), the [fitted-branch uniform collar and held-scale examples](2026-11-25-tl-math-report.md), and the [closed-support log-concavity window](2026-11-25-transfer-math-report.md#tukey-lambda-beyond-the-fitted-branch). The [U-branch ties](2026-11-25-tlambda2-math-report.md) and [root-isolation results](2026-11-25-tlroot-math-report.md) concern free U shapes or all-free half ties. Fixed-shape reductions apply here, while free U shapes are outside the fitting domains. The [prior tie classification](2026-11-25-ties-math-report.md#classification-of-the-constructions) distinguishes structural ties from real crossings. Both occur below.

Write \(x_-,x_+\) for the extrema, \(W=x_+-x_-\), \(R(\mu)=\max_i|x_i-\mu|\), and \(k_t\) for the multiplicity at \(t\). The standardized quantile \(Q_\lambda\) has derivative \(D_\lambda(u)=u^{\lambda-1}+(1-u)^{\lambda-1}\), and \(g_\lambda(Q_\lambda(u))=1/D_\lambda(u)\). Compactify scale by \([0,\infty]\) and unbounded location and shape by extended real endpoints.

**Finite-boundary behavior, proved.** At positive finite scale the inverse is continuous inside support, including across \(\lambda=0\). At positive held shape, endpoint densities are zero below 1, \(1/(2\sigma)\) at 1, and \(1/\sigma\) above 1. Thus the joint contact envelope equals the closed-support product. A strictly exterior observation gives zero. At the free shape ceiling, support convergence forces all observations into the limiting closed interval for a positive limsup. Each factor has limsup at most \(1/(2\sigma)\) since \(D_\lambda\ge2\) for \(\lambda\le1\). The eligible uniform member itself reaches the joint upper bound, including when several observations contact. The lower bound \(-1\), wherever imposed, is also an attained finite member. The likelihood is consequently upper semicontinuous on each fitting domain.

### tlambda held lambda

**Failure, proved.** For \(\lambda=-a<0\), put \(c=1/a,\ r=1+c,\ M=2^{-a-2},\ C=a^{-r}\). For the smaller inverse probability \(p\) at \(z=|x-\mu|/\sigma\), \(p^{-a}=az+(1-p)^{-a}\ge az+1\). Since \(D_{-a}(p)>p^{-a-1}\),
\[
 f(x)\le\min\{M/\sigma,\ C\sigma^c/(|x-\mu|+\sigma/a)^r\},
\]
strictly in the second term. At collapse \((\mu,\sigma)\to(t,0)\) where \(k=k_t>0\), let \(e_t=c(n-k)-k\). The envelope is infinity if \(e_t<0\), zero if \(e_t>0\), and when \(e_t=0\) it is
\[
 B_t=M^k C^{n-k}\prod_{x_i\ne t}|x_i-t|^{-r}.
\]
The density bounds give the joint upper bound in the nonnegative exponent cases. On the path \(\mu=t\), central densities are exactly \(M/\sigma\), and \(p\sim(az)^{-1/a}\) for the noncentral observations gives the matching tail asymptotic \(g_{-a}(z)\sim C z^{-r}\). This path also gives divergence for a negative exponent. A collapse location outside the sample gives zero. At infinite scale the modal bound gives zero uniformly in location. Optimizing the envelope \(\min(M/\sigma,C\sigma^c/|x-\mu|^r)\) over scale gives \(f(x)\le M^{c/r}C^{1/r}/|x-\mu|\) away from the center, so infinite location gives zero even when scale varies.

At \(\lambda=0\), the logistic bound \(g_0(z)\le e^{-|z|}\) excludes collapse for positive range because \(\inf_\mu\sum_i|x_i-\mu|>0\). With constant data, collapse at the common value has infinite envelope by centering there. Elsewhere the logistic bound gives zero. For \(\lambda>0\), support requires \(\sigma\ge\lambda R(\mu)\) at positive likelihood, so zero scale gives zero except collapse at the common value of a constant sample, where the envelope is infinite. Infinite scale gives zero by the finite modal bound for every held shape. Infinite location at bounded scale gives zero by support for \(\lambda>0\), and \(\sup_{\sigma>0}\sigma^{-1}e^{-|d|/\sigma}=1/(e|d|)\) handles logistic infinite-location corners. This exhausts the boundary together with finite contacts.

**Criterion, proved.** For nonnegative held shape, attainment holds iff \(W>0\). For negative held shape, any negative exponent gives an infinite supremum and all positive exponents give attainment. In each attaining case, a sufficiently large finite scale supplies positive likelihood and the zero boundary envelope gives attainment by compactness. When all exponents are nonnegative and at least one vanishes, set \(B=\max_{e_t=0}B_t\).

For a critical \(t\), define \(\alpha=g_{-a}(1/a)/M<1\) and \(d_t=\min_{x_i\ne t}|x_i-t|\). Strict radial decrease follows from strict decrease of \(D_{-a}(p)\) up to \(1/2\). Take \(0<\eta_t\le d_t(1-\alpha^{k/[r(n-k)]})/2\). If \(|\mu-t|\le\eta_t\) and \(|\mu-t|\le\sigma/a\), every noncentral denominator \(|x_i-\mu|+\sigma/a\) is at least \(|x_i-t|\). Multiplication of the strict tail bounds gives \(P<B_t\). If instead \(|\mu-t|>\sigma/a\), the central factors lose at least a factor \(\alpha^k\) from their modal bound, giving
\[
 P/B_t\le\alpha^k(1-\eta_t/d_t)^{-r(n-k)}<1.
\]
Scale powers cancel in both cases. Each critical location therefore has a strict exclusion neighborhood valid at every positive scale.

Choose a common radius \(\eta\) no larger than the critical radii and smaller than a third of the distinct-value spacing. In each noncritical observed-value neighborhood, use \(P\le M^{k_t}C^{n-k_t}\eta^{-r(n-k_t)}\sigma^{e_t}\). On the complement use the same formula with \(k=0\). All these powers are positive, so choose \(\tau>0\) with each bound at most \(B/2\) at \(\tau\) and below. Take \(S=2M B^{-1/n}\), \(H=2M^{c/r}C^{1/r}B^{-1/n}\), and shrink \(\tau\) to at most \(S/2\). Every parameter outside \(K=[x_--H,x_++H]\times[\tau,S]\) has \(P<B\) by the scale, location or collar bounds. The continuous likelihood has a maximum \(V\) on \(K\).

**Attainment holds iff \(V\ge B\)** in this critical case. If \(V=B\), each point of \(K\) at level \(B\) is an MLE and a collapse sequence approaches that same value. If \(V<B\), the supremum \(B\) is unattained. A finite witness at or above \(B\) suffices for attainment. A strict upper bound below \(B\) on \(K\) suffices for nonattainment. The held \(-1\) pairing proof gives this nonattainment directly throughout its critical case. [Prior pairing inequality](2026-11-07-tlambda-theory-data/proofs.md#6-collapse-faces-including-equality)

**Uniqueness, refuted in general.** For nonconstant data, the fit is unique at \(0\le\lambda<1\) and \(2<\lambda\le8/3\). The prior strict log-concavity on positive support gives a strictly concave log likelihood in \((A,B_0)=(1/\sigma,-\mu/\sigma)\). The term \(n\log A\) gives strictness if \(A\) varies, and a standardized observation gives strictness at fixed \(A\) if \(B_0\) varies. Shapes 1 and 2 give the unique tight uniform. For \(1<\lambda<2\) the prior anchored-support proof gives the unique tight support by strictly improving each contact curve as its width decreases to \(W\). [Anchored-support proof](2026-11-25-tl-math-report.md#free-scale)

For an exact counterexample take \(x=(-1,1)\) and \(\lambda=5\). For every \(\lambda>2\), expansion gives \((u^\lambda-v^\lambda)(u^{\lambda-2}-v^{\lambda-2})<D_\lambda(u)^2\) for \(u+v=1\) in the open support because the difference consists of positive cross terms. Thus \(Q_\lambda D_\lambda'/D_\lambda^2<(\lambda-1)/\lambda<1\) and the log-scale score of each density is negative. Continuity at the endpoint makes the smallest feasible scale optimal at fixed location. At \(\mu=0\), the best scale is 5 with \(P=1/25\). At \((\mu,\sigma)=(1,10)\), the mode and contact give \(P=8/100>1/25\). Existence and reflection give at least two global maximizers with opposite nonzero locations.

**Algebraic comparison, proved.** At nonzero rational held shape \(m/d\) and algebraic data and holds, the polynomial lift \(s=u^{1/d}, t=(1-u)^{1/d}\), \(s^d+t^d=1\), makes the likelihood graph semialgebraic. Split off contacts and exterior points and clear only positive denominators on the interior. The critical negative-shape \(B\) is algebraic. Quantifier elimination can decide whether a finite point has \(P\ge B\), including equality. It can also describe the entire maximizing set via the feasible points for which no feasible point has higher likelihood, including in the fixed-scale mask. This supplies an exact route for binary64 inputs. Degree growth is left untreated. For general irrational held shape the compact criterion still holds and an exact comparison procedure remains open. [Prior polynomial lift](2026-11-07-tlambda-theory-data/proofs.md#9-exact-rational-held-shape-route-and-its-limitation)

### tlambda held lambda+mu

**Failure and criterion, proved.** Write \(k=k_\mu\). For \(\lambda=-a<0\), the only nonzero escape is zero scale with \(e=c(n-k)-k\le0\), with envelope infinity for \(e<0\) and \(B_\mu\) for \(e=0\). The tail bound is strict at fixed location for the noncentral observations, giving \(P<B_\mu\) at every positive scale when \(e=0\). Thus a finite MLE exists iff \(e>0\). At \(\lambda\ge0\), a finite MLE exists iff \(R(\mu)>0\), by the nonnegative-shape bounds above restricted to the held location. If the radius is zero the center density diverges at zero scale. Infinite scale always gives zero and the finite support threshold has the closed-support contact value.

**Uniqueness, proved in every attaining case.** For \(0\le\lambda<1\) apply strict concavity in inverse scale. At 1 and 2 the unique fit is the tight uniform with \(\sigma=\lambda R(\mu)\). For \(1<\lambda<2\), radial density increases with distance, so shrinking scale improves both the radial and normalization factors. For \(\lambda>2\) use the strict scale decrease proved above. All shapes above 1 therefore give the unique fit \(\sigma=\lambda R(\mu)\).

At \(\lambda=-a<0\), set \(t=p/(1-p)\) at a nonzero residual and \(z=|x-\mu|/\sigma\). The log-scale score contribution is \(-1+E_a(z)\), where
\[
 E_a(z)=\frac{Q_\lambda D_\lambda'}{D_\lambda^2}
 =\frac{a+1}{a}\frac{(1-t^a)(1-t^{a+2})}{(1+t^{a+1})^2}.
\]
Factor powers of \(p\) out of \(z,D_\lambda,D_\lambda'\) on the upper quantile half, with \(p\) the smaller probability. Both numerator factors strictly decrease with \(t\) and the denominator increases. Thus \(E_a\) strictly increases with \(z\), from 0 to \((a+1)/a\), since \(t\) decreases with \(z\). At a zero residual the score contribution is \(-1\). The total score strictly decreases with log scale when any residual is nonzero and has limits \(e\) at zero and \(-n\) at infinity. In the attaining case it crosses zero exactly once. This proves the negative-shape log-scale concavity proposed in the [speed study](2026-11-08-tlambda-speed-report.md#structural-checks).

### tlambda held lambda+sigma

**Failure and criterion, proved.** At \(\lambda\le0\), every finite location has positive likelihood and infinite location gives zero by the fixed-shape bounds. For \(\lambda>0\) let \(w=\sigma/\lambda\). The feasible closed interval is \(I=[x_+-w,x_-+w]\). At \(0<\lambda<1\), only its interior gives positive likelihood, so attainment holds iff \(W<2w\). At \(\lambda\ge1\) the closed contacts are positive, so attainment holds iff \(W\le2w\), including a singleton at equality. Infinite location is exterior at all positive shapes. There is no scale escape. A positive finite witness and the zero infinite-location envelope give attainment when feasible.

**Uniqueness, refuted in general.** At \(0\le\lambda<1\) and \(2<\lambda\le8/3\), strict log-concavity gives a singleton when feasible. At 1 and 2, the full set is \(I\). At \(1<\lambda<2\) strict convexity of the log likelihood follows from \(D_\lambda''<0\) and log-density curvature \((2(D_\lambda')^2-D_\lambda D_\lambda'')/D_\lambda^4\). Compare the two endpoints and keep both at equality, counting a singleton interval once. In particular \(\sigma=1,\lambda=3/2,x=(-1/4,1/4)\) has precisely two maximizers \(\mu=\pm5/12\) by symmetry. At \(\lambda<0\), radial decrease confines all maxima to \([x_-,x_+]\). The earlier curvature example \(x=(-80/9,80/9),\lambda=-1,\sigma=1\) has a strict local minimum at the center and hence at least two reflected global maxima. The polynomial lift gives the full set for algebraic inputs and nonzero rational shape outside the analytic cases. [Earlier fixed-scale examples](2026-11-07-tlambda-theory-data/proofs.md#10-exact-uniqueness-counterexamples)

### tlambda held mu

**Failure, proved.** Let \(k=k_\mu\). The modal bound \(P\le(2\sigma)^{-n}\) gives zero at infinite scale even at infinite negative shape. At finite positive scale, \(\lambda=-a\to-\infty\) gives zero by \(g_{-a}\le2^{-a-2}\). If \(k=0\), the bound \(|x_i-\mu|f(x_i)<p_i/a\le1/(2a)\), obtained from \(z/D_{-a}(p)<p/a\), gives zero at \(\lambda=-\infty\) even during joint collapse. If \(k>0\), take \(a_j\to\infty\) with each \(a_j>(n-k)/k\) and then \(0<\sigma_j<1/j\) with \(P>j\), using the fixed-shape asymptotic. This gives infinite envelope at the joint corner.

At zero scale and finite negative limiting shape \(-a_0\), for \(0<k<n\) the envelope is infinite if \(e(a_0)<0\) by a fixed-shape path. It is zero if \(e(a_0)>0\) by uniform fixed-shape bounds in a neighborhood with the exponent bounded below by a positive constant. At \(e(a_0)=0\), choose \(a_j\downarrow a_0\) from above and a sequence of scales as in the joint corner. The joint envelope is infinite at this critical point. On the held-shape slice the envelope there is \(B_\mu\). With \(k=0\), the positive exponent gives zero at all finite negative limiting shapes. With \(k=n\), the modal path gives infinity at every finite shape as scale vanishes.

When any residual is nonzero, zero scale at a finite strictly positive limiting shape gives zero by support. At limiting shape zero, take the prior uniform Hölder bound on \([-A,1]\), \(f\le\min((2\sigma)^{-1},C_A\sigma^{1/A}/|x-\mu|^{1+1/A})\), with \(C_A=\max(1,A^{-1-1/A})\). Taking \(A>0\) small enough that \((n-k)/A-k>0\) gives zero uniformly in this neighborhood. Finite-scale contacts and the shape ceiling were covered above.

**Criterion, proved.** Attainment holds iff \(k=0\). Then a logistic witness is positive and the joint escape list confines its superlevel set to a compact region. If \(k>0\), an eligible fixed sufficiently negative shape gives an infinite supremum. With ties absent, each individual shape has a unique maximizing scale by the preceding mask.

**Uniqueness, refuted by an exact real crossing.** Hold \(\mu=0\) and take \(x=(t,1)\) with \(t\in[t_0,1]\), \(t_0=2^{-20}\). The uniform optimum has \(\sigma=1\) and \(U=1/4\) throughout. The prior uniform collar gives \(P<U\) for every \(\lambda=1-h\) with \(0<h<e^{-8}\). Indeed a candidate at least \(U\) needs \(1-h\le\sigma\le 2^{-h}\) by support and the modal bound. The contact-side quantile deficit is at most \(2h\) and \(D\ge2\) gives \(p\le h\). Its derivative factor is at least \(1+p^{-h}\), giving \(\log(P/U)\le4h+(h/2)\log h<0\) by the arithmetic-geometric mean inequality. [Prior uniform collar](2026-11-25-tl-math-report.md#a-uniform-neighborhood-and-a-positive-shape-profile-result)

Let \(V(t)\) be the maximum over \(\lambda\le1-\frac12e^{-8}\) with scale free. One compact rectangle suffices throughout the sample path. The logistic witness at scale 1 has a positive minimum over \(t\in[t_0,1]\). Since \(t\ge t_0>0\), the joint negative-shape bound supplies a uniform lower shape cutoff at this witness level. On the remaining shape range the Hölder and modal bounds give uniform lower and upper scale cutoffs. The objective is jointly continuous there, including sub-uniform support contacts, so \(V\) is continuous.

At \(t=1\), for any \(\lambda<1\) and nonzero \(z\) inside support,
\[
 z g_\lambda(z)=\int_p^{1/2}D_\lambda(v)\,dv/D_\lambda(p)
 \le 1/2-p<1/2
\]
by monotonicity on the smaller tail. Thus \(V(1)<U\), by attainment on the rectangle. At \(t=t_0\) use \(\lambda=-2,\sigma=t_0\). Since \(p^{-2}\le2z+4\) and \(D_{-2}(p)\le2p^{-3}\), \(g_{-2}(z)\ge1/[2(2z+4)^{3/2}]\). This gives \(P\ge t_0^{-1/2}/(4\cdot6^3)>U\). Define the exact observation \(t_*\) as the largest \(t\in[t_0,1]\) with \(V(t)\ge U\). Continuity gives \(t_0<t_*<1\) and \(V(t_*)=U\). The uniform fit and every winning point on the rectangle are global maximizers by the collar. At least two shapes maximize on the exact sample \((t_*,1)\). Its arithmetic type is undetermined.

The same radial inequality gives a unique uniform fit whenever all distances to the held location are equal and positive. It bounds each density strictly below \(1/(2R(\mu))\) at every nonuniform shape. For general data, uniqueness of scale at each shape reduces cardinality to exact shape-profile comparisons.

### tlambda held sigma

**Failure and criterion, proved.** Shape ranges over the compact interval \([-1,1]\). The prior Hölder bound \(f(x_i)\le\sigma/(x_i-\mu)^2\) off center applies throughout, so infinite location gives zero. There is no scale escape. Contacts and shape faces have their upper semicontinuous likelihoods, including uniform contacts. A logistic witness is positive for every sample, so attainment always holds. [Uniform envelope proof](2026-11-07-tlambda-theory-data/proofs.md#3-uniform-envelopes)

**Uniqueness, refuted in general.** If \(W\le2\sigma\), the full set is \(\lambda=1,\ \mu\in[x_+-\sigma,x_-+\sigma]\). The strict modal bound at every lower shape is below the uniform density. There is a singleton at equality, and \(x=(-1/4,1/4),\sigma=1\) gives a plateau. For an exact isolated tie when uniform is infeasible, use the prior proof at \(\sigma=1,\ x=(0,10^{20})\). The far-residual shape score and competing shape bounds force \(\lambda=-1\). Likelihood bounds exclude all locations outside two reflected intervals whose strict log curvature gives one maximum each. The same proof yields exactly one maximum for \(x=(0,0,10^{20})\). [Complete held-scale bounds](2026-11-25-tl-math-report.md#exact-held-scale-uniqueness-and-nonuniqueness-with-free-shape)

## Summary

| Family | Held | Question | Result |
|---|---|---|---|
| tlambda | lambda | Criterion | Proved |
| tlambda | lambda | Failure | Proved |
| tlambda | lambda | Uniqueness | Refuted |
| tlambda | lambda+mu | Criterion | Proved |
| tlambda | lambda+mu | Failure | Proved |
| tlambda | lambda+mu | Uniqueness | Proved |
| tlambda | lambda+sigma | Criterion | Proved |
| tlambda | lambda+sigma | Failure | Proved |
| tlambda | lambda+sigma | Uniqueness | Refuted |
| tlambda | mu | Criterion | Proved |
| tlambda | mu | Failure | Proved |
| tlambda | mu | Uniqueness | Refuted |
| tlambda | sigma | Criterion | Proved |
| tlambda | sigma | Failure | Proved |
| tlambda | sigma | Uniqueness | Refuted |

Refutation means general uniqueness fails. The regime sections give the proved singleton and maximizing-set scopes.

## What this changes for the fitter

- At held negative shape and held location, preserve the exponent-based unbounded and collapse-limit routes. In the attaining branch bracket the monotone total log-scale score and contract a scale enclosure. This also supplies a global inner solve for held location with shape free. Shape coverage remains an outer certificate.
- At critical held negative shape and free location, use the strict collars and compact rectangle. A strict upper bound below the face proves the collapse-limit outcome. A proved finite tie gives attainment. With unresolved comparison, a successful search retains the uncertified result subject to the score and accuracy gates and the rule for a separating refutation. A narrow likelihood gap alone leaves both equality and cardinality undecided.
- At held scale and free shape, test uniform feasibility directly and return the exact location interval or singleton when feasible. At held positive shape use the exact support threshold, including the strict width test below 1 and the positive singleton contact at and above 1.
- At held location with free shape, test for an observation equal to that location to detect infinite collapse. With no ties, derive a lower shape cutoff from the incumbent and joint bound and compare the eligible uniform endpoint explicitly.

## Dossier follow-ups

- Document the new scale-score result when replacing scalar boxes. “At held location and negative shape, a positive collapse exponent gives a unique finite scale, the root of a strictly decreasing log-scale score. Zero exponent gives an unattained collapse supremum and negative exponent an infinite supremum.”
- Extend the joint held-shape statement. “At held negative shape with free location, a critical collapse comparison takes place on a compact rectangle that excludes all smaller scales at the face level. A finite tie with the largest face value counts as attainment. Joint location and scale can be nonunique even at fixed shape, as at shape 5 on \((-1,1)\).”
- Clarify the scope of the collapse exponent. “The held-location collapse exponent describes a fixed-shape slice. With shape free, a joint approach to the critical negative shape from below has infinite envelope at zero scale. An observation at held location suffices for an infinite supremum.”

## Open

- At arbitrary irrational held negative shape, the compact criterion leaves the exact sign of \(V-B\) as a comparison. The missing predicate is exact equality of the largest rectangle likelihood with the collapse value, or an analytic inequality separating the values for this class. For algebraic data and rational shape the polynomial lift decides the comparison, with a practical high-degree solver still missing.
- Outside the proved singleton and explicit maximizing-set scopes, full cardinality requires certified exhaustion of the negative-shape joint critical sets, the contact strata above \(8/3\), or the free-shape profile branches, as applicable. Root isolation leaves exact comparison of winning heights, including those of any positive-dimensional critical set. The held-location crossing leaves existence of such a tie on dyadic data open. The missing dyadic predicate is exact equality of a winning sub-uniform height with the uniform height, or equality of two winning sub-uniform heights.
- The all-free half-tie and free U-branch junction predicates have different fitting domains and remain as described in the earlier reports.

## Reproduction

The workstation Python check completed through the one-slot wrapper. At 80 decimal digits it checked the elasticity identity and strict ordering at five shapes and 49 probabilities per shape, the collar at six critical counts, five location offsets and five scales per count, and the separated-sample witness for the held-location crossing. These are numerical checks of the full-range proofs above. [Script](2026-11-27-tlambda-atlas-math-data/check.py) · [Output](2026-11-27-tlambda-atlas-math-data/check.txt) · [Atlas rows](2026-11-27-tlambda-atlas-math-data/proofs.csv)

The atlas checker also passed on the workstation through the one-slot wrapper, covering all 15 rows, their canonical masks, and their evidence anchors. [Checker](2026-11-27-tlambda-atlas-math-data/check_atlas.py)
