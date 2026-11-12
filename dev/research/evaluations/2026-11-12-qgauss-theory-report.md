# q-Gaussian: parameterization, existence and fitting contract

TASK-201, 2026-11-12. Baseline: af824a98d78f2f2325c7a675c20c84c45e6bb826, branch codex/qgauss-theory. This report settles the model contract for TASK-202 and TASK-203. It changes no fitter, density, binding, production test, golden, dossier or roadmap. Statements labelled **proved** are exact-arithmetic arguments; the independent high-precision checks are numerical corroboration. A proposed certificate is not an implemented certificate.

## Decision and use case

Use coefficients $(\mu,\sigma,\eta)$, with signed inverse degrees of freedom $\eta$, and id **qgauss**. Accept the corresponding quantile name **qqgauss**. No collision was found in the existing C++, bindings or site sources. Keep the package's ML-first public interface.

The use case is a single symmetric error model whose fitted tail weight can be lighter or heavier than Gaussian: bounded measurement residuals, Gaussian errors and robust Student errors share the same location and ordinary scale coordinates. Student t alone omits the bounded, platykurtic alternatives. The continuation also makes normality an interior hypothesis, allowing its likelihood to be assessed without a one-sided normal escape. This is the reason to add the family beyond distribution-parity coverage. It is ordinary iid maximum likelihood, not maximization of a q-logarithmic criterion.

Recommend the all-free fitting restriction $-1\le\eta\le1$. The lower bound prevents endpoint spikes; the upper prevents unrestricted Student scale collapse. **Exempt the upper bound when either mu or sigma is held**, with the exceptional held-mu ties diagnosed below. There is no universal hold exemption from the lower bound. Do not restrict at $-1/3$ for inference regularity, or at a moment-existence threshold.

## Density, coordinates and proper domain

For finite real mu and eta and positive finite sigma, put $z=(x-\mu)/\sigma$ and

$$
f(x)=\frac{C_\eta}{\sigma}(1+\eta z^2)^{-(1+\eta)/(2\eta)}.
$$

At eta zero use the normal density. For positive eta,

$$
\nu=1/\eta,\qquad C_\eta=\frac{\sqrt{\eta}}{B(1/2,1/(2\eta))};
$$

this is exactly lst with ordinary t scale sigma. For negative eta,

$$
m=-\frac{1+\eta}{2\eta},\quad h=\frac{\sigma}{\sqrt{-\eta}},\quad
C_\eta=\frac{\sqrt{-\eta}}{B(1/2,m+1)},\quad
f(x)=\frac{[1-((x-\mu)/h)^2]^m}{hB(1/2,m+1)}.
$$

The support is the closed interval $[\mu-h,\mu+h]$. Its endpoint density is zero for $m>0$, $1/(2h)$ for $m=0$, and infinity for $m<0$, following R7a. Outside support the density is zero. The transformed variable $(X-\mu+h)/(2h)$ is Beta$(m+1,m+1)$.

**Proved:** every finite negative eta is proper, since $m>-1/2$. Thus the reference domain is eta in the entire real line, sigma positive and mu real. The bounded branch covers beta shapes greater than $1/2$, not the whole symmetric Pearson II family. Zero scale and infinite coefficients are excluded. At eta $=-1$ the law is uniform on $[\mu-\sigma,\mu+\sigma]$. Sigma is an ordinary scale, equal to standard deviation only at eta zero; when the variance exists it is $\sigma^2/(1-2\eta)$. Infinite variance does not imply irregular ML.

The conventional Tsallis coordinates are

$$
q=\frac{1+3\eta}{1+\eta},\qquad \eta=\frac{q-1}{3-q},\qquad
\beta=\frac{1+\eta}{2\sigma^2}.
$$

They are useful for translation, but q sends uniformity to minus infinity and has a pole at eta $=-1$. The proper eta $<-1$ continuation has q $>3$ and negative beta, outside the usual positive-beta q-Gaussian convention. Document this naming qualification. Signed df has two infinite normal ends; the bounded exponent m also puts normality at infinity. Eta has a finite, nonsingular normal coordinate and preserves lst's scale and holds.

The [literature report](2026-11-10-qgauss-lit-report.md) records the statistical antecedent and software conventions. [Papastathopoulos and Tawn's institutional record](https://www.research.ed.ac.uk/en/publications/a-generalised-students-t-distribution/) verifies the negative-df antecedent; no global optimization theorem is attributed to its inaccessible full text here.

## Why the restrictions are needed, and their exemptions

### Endpoint spikes

**Proved:** hold eta $<-1$, so $-1/2<m<0$. Write endpoints as a and b. Set $a=\min x-\epsilon$ and keep $b>\max x$ fixed. For minimum multiplicity k, the likelihood is a positive bounded factor times $\epsilon^{mk}$ and diverges. This path uses strictly interior observations. The unrestricted all-free family therefore has no finite global likelihood maximum.

This persists with mu held and sigma free: let $R_\mu=\max_i|x_i-\mu|>0$ and decrease h to $R_\mu$. The extreme residuals spike. If $R_\mu=0$, scale collapse is already unbounded. With sigma held and mu free, a fixed spiking shape with width exceeding the sample range allows the same endpoint path.

Even holding both mu and sigma does not give a universal lower-bound exemption. If $0<R_\mu<\sigma$, varying eta towards $-\sigma^2/R_\mu^2<-1$ from inside support makes the extreme observations spike. If $R_\mu=0$, eta tending to minus infinity makes the central density diverge. For $R_\mu\ge\sigma$, support feasibility itself excludes the spiking branch, but that is a data-dependent fact, not a structural exemption. Likewise, with sigma held and mu free, samples of range below $2\sigma$ give an open set of spike examples.

Hence eta at least minus one is the sharp closed all-free endpoint restriction. It retains the proper uniform member and the estimable nonregular bounded shapes above it.

### Student scale collapse

**Proved:** at fixed positive df nu, place mu at an observation of multiplicity k and decrease sigma to zero. The likelihood satisfies

$$
L\sim A_\nu\,\sigma^{\nu(n-k)-k},\qquad
A_\nu=C_\nu^n\nu^{(\nu+1)(n-k)/2}
 \prod_{x_i\ne\mu}|x_i-\mu|^{-(\nu+1)},
$$

where $C_\nu=\Gamma((\nu+1)/2)/[\sqrt{\nu\pi}\Gamma(\nu/2)]$.
A negative exponent proves unboundedness. Free positive df can always be chosen small enough. Df at least one, equivalently eta at most one, is the weakest universal fixed cutoff for the policy's class of tie masses below one half: any smaller df permits a rational tie fraction between $\nu/(1+\nu)$ and $1/2$. It does not cure majority ties. Held positive eta outside the fitting bound must instead receive its actual collapse test.

Holding sigma removes this path completely. More strongly, **a finite maximum exists over eta in $[-1,\infty)$ and free mu for every finite sample with sigma held**. To control its unbounded shape tail, beta integration gives

$$
C_\nu\le\sqrt{\nu}/2,\qquad
\ell\le n\{\tfrac12\log\nu-\log(2\sigma)\}\longrightarrow-\infty
\quad(\nu\downarrow0),
$$

uniformly in mu. On bounded shape intervals, mu escaping to infinity has vanishing likelihood. Normality supplies a positive finite candidate.

Holding mu also removes the upper restriction for almost every sample, as in lst, but only when scale is free and no observation equals that mu. For a nonzero residual r, maximizing its density over sigma gives sigma $=|r|$. For $0<\nu\le1$,

$$
\sup_{\sigma>0}f(\mu+r;\mu,\sigma,\nu)
=\frac{C_\nu}{|r|}(1+1/\nu)^{-(\nu+1)/2}
\le\frac{\nu}{2|r|}.
$$

Thus, when every residual is nonzero,

$$
\ell\le n\log(\nu/2)-\sum_i\log|x_i-\mu|\longrightarrow-\infty.
$$

Finite-shape scale tails also vanish, giving an attained finite maximum over eta in $[-1,\infty)$. This includes constant data away from held mu. If k observations equal held mu and sigma is free, select $\nu<k/(n-k)$ to prove unboundedness; k $=n$ is immediate. **Do not apply this tie stop when sigma is also held.**

These proofs justify two separate schema records, without changing the reference domain:

    fit_domain:
      - lower: -1
        restriction:
          mechanism: endpoint
          exempt_when_held: []
          reason: Shapes below uniform permit endpoint-spiking likelihoods.
      - upper: 1
        restriction:
          mechanism: collapse
          exempt_when_held: [[mu], [sigma]]
          reason: Free location and scale permit small-df collapse; held mu still requires exact-match diagnostics.

Estimated coefficients on applicable bounds receive the R5 restricted flag and warning. Held coefficients are not flagged. Eta one is not a restriction face when its upper bound is exempt. Sample-dependent failures belong in limits and diagnostics; the structural target is holds: [].

## Fixed-shape nuisance likelihood

For eta in $(-1,0)$ set $u=1/h>0$, $v=-\mu/h$, $t_i=ux_i+v$. Then

$$
\ell=n\log u-n\log B(1/2,m+1)+m\sum_i\log(1-t_i^2),
\qquad |t_i|<1.
$$

**Proved:** the feasible set is convex and

$$
D^2\ell[(du,dv)]^2
=-n(du/u)^2
-2m\sum_i\frac{1+t_i^2}{(1-t_i^2)^2}(x_i\,du+dv)^2<0
$$

for every nonzero direction and nonempty sample. This is strict joint concavity, including constant samples; constants can nevertheless be unbounded on the open parameter domain. For nonconstant data, h is at least half the range, large h gives vanishing density, and either endpoint touching an observation gives zero likelihood. Thus there is a unique finite fixed-shape nuisance maximum. Holding mu restricts to the line $v=-\mu u$; holding sigma fixes u at each fixed eta. These retain the relevant conditional concavity. Reconstruction is $\mu=-v/u$, $\sigma=1/[u\sqrt{2m+1}]$.

At eta zero, inverse-scale normal coordinates give the usual strict concavity and normal MLEs. At eta minus one, solve the closed uniform problem directly.

For eta positive, Euclidean log-concavity fails beyond $|z|=1/\sqrt{\eta}$:

$$
(\log g_\eta)''(z)
=-\frac{(1+\eta)(1-\eta z^2)}{(1+\eta z^2)^2}.
$$

For nu at least one, use lst's upper-half-plane geometry with $a=\sigma\sqrt{\nu}$. The negative likelihood, up to its shape normalizer, is

$$
\frac{\nu+1}{2}\sum_i\log\frac{(x_i-\mu)^2+a^2}{a}
+\frac{n(\nu-1)}2(-\log a).
$$

The terms are Busemann functions with positive-semidefinite covariant Hessians. This proves geodesic convexity, not Euclidean convexity. Under $k_{\max}<\nu(n-k_{\max})$ it is coercive and has a unique maximum; see the [existing derivation](2026-11-04-t-cert-data/derivation.md). The balanced two-point Cauchy case has a maximum curve. Below df one, strict-mass existence still follows from the tail/collapse argument, but this geodesic uniqueness proof is unavailable.

For fixed mu and any fixed eta $>-1$, the log-scale score is

$$
S=-n+(1+\eta)\sum_i\frac{z_i^2}{1+\eta z_i^2}.
$$

It decreases strictly with positive sigma whenever a residual is nonzero. On the bounded branch it starts at positive infinity at the support boundary; at normality it gives the residual RMS. On the Student branch its lower-end limit is $\nu(n-k)-k$. A positive limit gives exactly one finite scale root; equality gives an unattained finite scale-zero supremum, and a negative limit gives unbounded likelihood.

With sigma and positive eta held, the mu score need not be monotone. For J distinct observations its stationary equation

$$
\sum_i\frac{x_i-\mu}{\nu\sigma^2+(x_i-\mu)^2}=0
$$

has a numerator polynomial of degree at most $2J-1$. Exhaustive real-root isolation and comparison is a global route. One location root from EM or Newton is not that comparison.

## Attainment, uniformity and collapse faces

### All-free attainment

Let $k_{\max}$ be the largest sample multiplicity.

**Proved:** if $k_{\max}<n/2$, the restricted all-free model has an attained finite maximum. This is stronger than the corresponding Student-only normal-escape result.

Here is a uniform compactness proof. For every $0<p<1$, the standardized absolute p-moment is bounded uniformly over eta in $[-1,1]$, by the beta moment formulas and their normal limits. The densities are symmetric and nonincreasing away from zero. Consequently

$$
g_\eta(z)\le K,\qquad g_\eta(z)\le K_p|z|^{-1-p}
$$

uniformly over shape. Choose $p>k_{\max}/(n-k_{\max})$. Near any prospective collapse atom the likelihood is at most a constant times $\sigma^{p(n-k)-k}$, which tends to zero. Away from atoms it also vanishes. Large scale is excluded by the mode bound; escaping location with bounded scale is excluded by the tail bound. Positive likelihood superlevel sets therefore have bounded mu and scale bounded away from zero and infinity.

The likelihood is continuous through normality and upper semicontinuous at uniformity. At a uniform endpoint the factors $(1-t^2)^m\le1$ imply the required limsup bound. It is not jointly continuous there: endpoint gaps $\exp(-c/m)$ leave attenuation $\exp(-c)$. Upper semicontinuity on the remaining compact set proves attainment. No uniqueness of the outer shape maximum follows.

If $k_{\max}>n/2$, held Cauchy shape already proves unboundedness. Constants are included.

If $k_{\max}=n/2$, only eta approaching one can retain a nonzero scale-collapse likelihood. Its envelope at an atom c of half the mass is

$$
L_{\rm col}(c)=\pi^{-n}\prod_{x_i\ne c}|x_i-c|^{-2}.
$$

For joint eta/scale paths, the nonnegative collapse exponent and the central displacement factor, at most one, cannot exceed this envelope. Shapes bounded away from one have a uniformly higher tail moment and vanish at collapse.

Let B be the largest such envelope. **A finite maximum exists iff some finite feasible point has likelihood at least B.** If a point exceeds B, compactness above that level gives a maximum; equality is already an attained witness. If no finite point reaches B, the supremum is the unattained collapse face. A strictly winning face requires a global comparison. A numerical tie is not proof of nonexistence.

For fixed positive nu, the same statement uses the displayed $A_\nu$ at all zero-exponent atoms, provided there is no negative exponent. For nu at least one, the fixed-shape geometry makes equality unattained except for the balanced two-point Cauchy curve. With free location and df below one, compare finite points and equality envelopes; do not copy the held-location equality stop.

### Exact uniform solutions and a useful mode bound

**Proved:** $C_\eta\le1/2$ for eta at least minus one, with equality only at uniformity. On the bounded branch write $A(m)=-\log B(1/2,m+1)$. Then

$$
A'(m)=\int_0^\infty\frac{e^{-(m+1)t}}{1+e^{-t/2}}\,dt
<\frac1{2m+1},
$$

so $\log C=A(m)-\tfrac12\log(2m+1)$ decreases from $\log(1/2)$. On the Student branch Cauchy–Schwarz gives $\Gamma(a+1/2)<\sqrt a\,\Gamma(a)$, hence $C_\nu<1/\sqrt{2\pi}<1/2$. Also $g_\eta(z)\le C_\eta$.

It follows that **with sigma held, if the sample range R is at most $2\sigma$, the global free-shape solution is uniform**, and all locations in $[\max x-\sigma,\min x+\sigma]$ are maximizers. Eta is uniquely minus one; location is nonunique unless this interval is a singleton. If mu and sigma are held and $R_\mu\le\sigma$, the unique free-shape winner is likewise uniform.

For uniform shape held:
- Both mu and sigma free, R positive: $\hat\mu=(\min x+\max x)/2$, $\hat\sigma=R/2$.
- Mu held, sigma free: $\hat\sigma=R_\mu$ if positive; otherwise unbounded.
- Sigma held, mu free: the interval above is the full maximum set if feasible.
- Infeasible fixed support requires an explicit support diagnostic.

For equal-weight observations at two distinct values separated by R, **the all-free q-Gaussian maximum is uniquely the uniform interval**, including its shape. At every nonuniform bounded shape and every nu greater than one, symmetry and the proved conditional uniqueness put mu at the midpoint and sigma at $R/2$. At those coefficients its density is strictly below $1/R$ by the mode bound. Cauchy's maximum curve is also strictly worse. Thus this case is not merely a bounded-collapse ambiguity.

For constant data with sigma free and mu free, the likelihood is unbounded. With mu held at the common value it is also unbounded. With mu held elsewhere, free shape selects uniform with sigma equal to the absolute residual: any symmetric nonincreasing density satisfies $f(\mu+r)\le1/(2|r|)$. With sigma held, the uniform maximum-set result above applies.

### Uniform profile cusp and an explicit certificate bound

Let $P(m)$ be the all-free bounded-branch profile, R positive, endpoint multiplicities $k_-,k_+$ and $K=k_-+k_+$. Endpoint likelihood equations give

$$
a=\min x-\delta_-,\quad b=\max x+\delta_+,\qquad
\delta_\pm/m\longrightarrow k_\pm R/n.
$$

Indeed the endpoint scores equate $m(k_\pm/\delta_\pm+O(1))$ to $n/R+o(1)$. Substitution proves

$$
P(m)=P(0)+K m\log m+O(m).
$$

Uniformity is always a strict local maximum of this all-free shape profile. It need not be the global maximum.

There is an explicit uniform upper bound, not just this expansion. Put
$E=k_-\log(k_-/K)+k_+\log(k_+/K)$ and $d=2-2\log2$. For any endpoints, their excess width D gives endpoint kernel factors at most $4\delta_\pm/(R+D)$, while all other kernel factors are at most one. Weighted maximization over the two gaps and then $t=D/(R+D)$ gives

$$
P(m)-P(0)\le U(m)
=n\log\frac2{B(1/2,m+1)}
+n\log\frac n{n+mK}
+mK\log\frac{4mK}{n+mK}+mE.
$$

Concavity of A implies the simpler bound

$$
U(m)\le m\{nd+K\log(4mK/n)+E\}.
$$

Thus the whole open uniform neighborhood is strictly excluded by taking, for example,

$$
0<m\le \frac{n}{8K}\exp[-(nd+E)/K],
$$

which gives an upper bound at most $-mK\log2$. This avoids indefinite subdivision next to an attained uniform winner.

With mu held, use $R_\mu>0$, K equal to the number of residuals with absolute value $R_\mu$, and the conditional uniform likelihood $P_\mu(0)=-n\log(2R_\mu)$. The same argument using $t=1-R_\mu^2/h^2$ gives

$$
P_\mu(m)-P_\mu(0)\le
n\log\frac2{B(1/2,m+1)}+\frac n2\log\frac n{n+2mK}
+mK\log\frac{2mK}{n+2mK}
\le m\{nd+K\log(2mK/n)\}.
$$

These are exact-arithmetic signed bounds. With sigma held and uniform feasible, the global mode bound already solves the problem.

## Every hold subset

The held-set column fixes exactly those coefficients. Free eta always has the lower bound; its upper bound is exempt in every row containing mu or sigma. All statements assume valid holds and a nonempty finite sample.

| Held | Result and required handling |
|---|---|
| none | Eta in $[-1,1]$. Strict half-mass condition gives finite attainment; majority ties give unboundedness; equality needs the collapse-envelope comparison. Constants are unbounded. |
| mu | Eta in $[-1,\infty)$. With sigma free, any exact match to held mu makes the likelihood unbounded. With no matches a finite maximum exists; use the unique conditional scale root and scalar shape profile. Constant offset data select uniform. |
| sigma | Eta in $[-1,\infty)$. A finite maximum always exists. If R is at most $2\sigma$, uniform and the stated location interval solve it globally. Otherwise compare the scalar shape profile, including every conditional Student location mode. |
| eta | Zero: normal MLE, or unbounded constants. Between minus one and zero: unique nuisance fit for nonconstant data, unbounded constants. Minus one: exact uniform interval. Positive: actual-nu collapse/strict-mass/equality rules above, including held eta greater than one. Below minus one: no finite nuisance maximum because of endpoint spikes. |
| mu, sigma | Finite free-shape maximum exists over $[-1,\infty)$ for every sample. If $R_\mu\le\sigma$, uniform wins exactly; otherwise solve and cover the scalar profile. No uniqueness theorem is asserted in the latter case. |
| mu, eta | Scale-only monotone score. Eta $>-1$: bounded/normal root exists unless all residuals vanish; positive eta additionally needs $\nu(n-k)>k$. Student equality is an unattained scale-zero supremum; negative exponent is unbounded. Uniform gives sigma $=R_\mu>0$. Spiking eta gives no finite maximum. |
| sigma, eta | Mu-only. Normal: sample mean. Bounded nonuniform: unique maximum if R $<2h$; if R $\ge2h$ there is no positive likelihood. Uniform: feasible location interval, possibly nonunique. Positive eta: finite maximum, isolate and compare all location-score roots. Spiking eta: endpoint singularity if R $\le2h$, incompatible support if R $>2h$. |
| mu, sigma, eta | Direct evaluation, including explicit zero or nonfinite endpoint likelihoods. No optimizer and no claim of an estimated MLE. |

For spiking eta with sigma held and R $=2h$, closed support forces both extreme observations onto infinite-density endpoints. This is an attained infinite endpoint likelihood, not a divergence through feasible interior locations; report the singularity accurately. For R $<2h$, an interior-to-endpoint divergent path exists.

For held Cauchy shape and equal-weight two-point data, the maximum curve is
$(\mu-\mathrm{mid})^2+\sigma^2=(R/2)^2$, sigma positive. Finite-sample nonuniqueness is not nonexistence or population nonidentifiability. Report the set/nonuniqueness, and use a documented equivariant representative where the result API returns one point. Uniform plateaus similarly admit their midpoint representative.

Normal, positive-eta lst, uniform and Cauchy are **attained members**, not escape stops. An internal delegate must preserve holds and the qgauss model: held mu or sigma imposes linked constraints on uniform endpoints, so an unconstrained mlunif call is generally wrong. An all-free qgauss fit at eta zero still has three free coefficients. A normal or uniform internal delegate must not silently reduce the model's parameter count.

## Outer profile and certification

The outer problem is one-dimensional shape, with the conditional nuisance problem solved according to its structure. General profile unimodality is false; a grid or monotone EM improvement is only a nomination mechanism.

On the bounded branch with mu and sigma free, or mu held and scale free,

$$
P(m)-nA(m)=\sup_{u,v}\{n\log u+m\sum_i\log(1-(ux_i+v)^2)\}
$$

is convex. The nuisance domain is independent of m, including the held-mu line. Since
$0<-A''(m)=\psi_1(m+1)-\psi_1(m+3/2)\le\pi^2/6$, an interval of width Delta has

$$
P(m)\le \max(P(m_0),P(m_1))+n\pi^2\Delta^2/48.
$$

Certified endpoint upper values can replace exact endpoint optima. Pair this finite-interval cover with the signed uniform bounds and a normal-neighborhood bound.

On the Student branch, in the a coordinate above,
$P(\nu)-nc(\nu)$ is convex, where
$c(\nu)=\log\Gamma((\nu+1)/2)-\log\Gamma(\nu/2)-\tfrac12\log\pi$.
The [Student study](2026-11-04-t-cert-report.md) proves $0<-c''(\nu)<1/\nu^2$, giving interval upper bound
$\max(P(\nu_0),P(\nu_1))+n(\nu_1-\nu_0)^2/(8\nu_0^2)$.
This also applies with mu held. Nuisance upper enclosures, not merely converged objective values, are required.

**Holding sigma changes these profile constraints.** Then $a=\sigma\sqrt\nu$ and $u=1/[\sigma\sqrt{2m+1}]$ depend on shape, so the two affine-profile arguments cannot be reused unchanged. Use actual-hold derivative bounds or joint shape/location boxes, with exhaustive conditional roots and mu contained in the sample range for nonuniform fits. The analytic small-nu bounds above exclude the infinite shape tail. Negative-eta support feasibility and its zero-density edge must be handled explicitly; an infeasible bounded branch does not make the full free-shape fit fail.

### The two-sided normal seam

For all-free nonconstant data, let $\bar x$ and s be the normal ML fits, $z_i=(x_i-\bar x)/s$, and $M_j=\overline{z^j}$. The refitted profile has

$$
P(\eta)=P(0)+A_N\eta+B_N\eta^2+O(|\eta|^3),
$$

$$
A_N=\frac n4(M_4-3),\qquad
B_N=n\left\{\frac{M_4}{4}-\frac{M_6}{6}
+\frac{M_3^2}{2}+\frac{(1-M_4)^2}{4}\right\}.
$$

**Proved:** expand the normalized density, then optimize the quadratic nuisance expansion using the normal nuisance Hessian. The last two terms are the nuisance-refitting correction. This is a two-sided expansion: nonzero $A_N$ excludes exact normality as an interior maximum; $A_N=0$ still needs curvature and global comparison. For held fits use their conditional normal optimum and the corresponding nuisance block, not these all-free moment formulas.

The existing exact-rational counterexample
$[-4,-3,-2,-1,1,2,3,4,-101,-99,99,101]/100$
has $A_N=-1170351/50160128<0$, yet the feasible eta $=1/3$, mu zero, sigma $=1/3$ point beats normal by about 0.491870907499. Its [rational proof](2026-11-04-t-cert-data/exact.py) and the new 80-digit re-evaluation rule out a kurtosis-sign global shortcut.

For an operational normal-neighborhood bound, enclose the nuisance optimum and bound finite Taylor remainders uniformly. For $t=z^2$ and $|\eta|t\le\rho<1$, the kernel remainder after eta squared is bounded by

$$
|\eta|^3\{t^4/8+t^3/6\}/(1-\rho).
$$

Gamma-ratio/Stirling remainder bounds supply the corresponding normalizer bound; score and nuisance-root bounds must cover the whole neighborhood. The existing positive-side normal certificate is a component, not a black-box two-sided qgauss certificate. Normal is included, so its old NormalBoundary outcome cannot be propagated as a qgauss escape. Flat seam cases require higher-order or root inclusion work.

A complete production certificate must compare all retained finite roots, the included restriction members, and any collapse envelope, preserving attainment witnesses. It must also enclose public coefficients, not just likelihood values. Use the [accuracy contract](../../design/accuracy-contract.md): relative sigma accuracy, a fitted-scale floor for mu, and floor one for eta. Propagate inverse-coordinate root brackets or establish positive-information Newton accuracy with quadratic convergence. A small likelihood gap or score norm alone does not justify snapping shape to zero or selecting a boundary.

The intended certificate standard is exact arithmetic conditional on computed comparisons, with stated practical floating-point and accumulation margins. Outward-rounded interval arithmetic is not a prerequisite. A successful locally accurate search whose global certificate cannot settle a sign, comparison or budget returns certified false. A proven better candidate requires correction; proven unboundedness or a strictly winning unattained face requires its reasoned stop. Search/accuracy failure is distinct from certificate exhaustion. Solver and certificate caps are respectively $1$ and $128$ times $\min(\mathrm{iterlim},100000)$; no fitted shape cap or fixed-cap estimate is acceptable.

The admissible likelihood escapes are now accounted for by compactness, collapse envelopes and the held-shape tail bounds. Outside the recommended fitting domain, eta tending to minus infinity with h fixed and sigma $=\sqrt{-\eta}h$ gives the arcsine law, while fixed sigma gives a point mass at mu. Neither path is admissible for free eta under the lower bound. The arcsine path is not a reason for an extra default escape detector.

## Inference regimes

**Proved local classification:** for finite eta $>-1/3$, sigma positive, the underlying model is DQM with finite positive-definite Fisher information, including eta zero and every finite positive eta. Positive-eta scores have integrable logarithmic/polynomial-tail envelopes even when the observations have no mean or variance.

On the bounded branch, near an endpoint at distance d the density is proportional to $d^m$. A moving-endpoint score behaves like $1/d$, so its squared integral is proportional to

$$
\int_0^\epsilon d^{m-2}\,{\rm d}d.
$$

It is finite exactly when $m>1$, equivalently eta $>-1/3$. In this regime the moving support strip has probability $O(|\delta|^{m+1})=o(\delta^2)$ and the interior square-root derivative is integrable, establishing DQM. Odd location score and the distinct rational and logarithmic even scale/shape terms establish full information rank; each regular coordinate-held submodel inherits a positive principal information matrix.

At normality, at unit scale, the per-observation scores in coordinates mu, log sigma, eta are

$$
Z,\quad Z^2-1,\quad (Z^4-2Z^2-1)/4,
$$

and

$$
I=\begin{pmatrix}1&0&0\\0&2&2\\0&2&7/2\end{pmatrix},
\qquad \det I=3,\qquad I_{\eta,\mathrm{eff}}=3/2.
$$

The efficient eta score is $(Z^4-6Z^2+3)/4$; efficient q-information is $3/8$. DQM is not merely formal differentiation: split into a growing central region and tails, controlling the latter by uniformly bounded high even moments
$E_\eta Z^{2k}=(2k-1)!!/\prod_{j=1}^k(1-2j\eta)$
in a sufficiently small two-sided neighborhood. The central square-root expansion and tail bounds give the L2 derivative.

| Regime | Consequence |
|---|---|
| Eta $>-1/3$, away from active fitting bounds | Regular local likelihood theory and ordinary normality testing at eta zero are eligible, subject to consistency, isolated accurate fits and validated derivatives. Normality is an interior hypothesis. |
| Eta $=-1/3$, m $=1$ | Logarithmically divergent endpoint information; not ordinary root-n joint Wald inference. |
| $-1<$ eta $<-1/3$, $0<m<1$ | Non-DQM moving endpoints. Endpoint order statistics have gap scale $n^{-1/(m+1)}$; this alone does not establish a joint MLE limit law. |
| Eta $=-1$ | Uniform support jumps; pointwise zero location score misses boundary mass. With shape held, exact order-statistic/spacing procedures are available and endpoints have n-rate behavior. Free-shape boundary inference needs its own result. |
| Eta $<-1$ held | Moving-endpoint irregularity and the finite-likelihood failures above; not a regular estimated-endpoint fit. |
| Eta $=1$ with the upper fitting bound active | Underlying Cauchy model is regular, but constrained shape inference is a tangent-cone problem; a one-sided likelihood test can have the usual chi-bar-square calibration under its additional conditions. With the bound exempt, eta one is an ordinary interior point. |

Every nonempty coordinate-free submodel in the bounded $m\le1$ regimes moves at least one endpoint. Holding a coordinate does not automatically restore DQM. A linked submodel holding both endpoints fixed while changing the beta exponent is different and can have regular shape inference; it is not one of these coordinate hold subsets.

[Smith's primary abstract](https://academic.oup.com/biomet/article-abstract/72/1/67/242523) supports the alpha greater than two, critical alpha two and nonnormal alpha between one and two distinction, with alpha $=m+1$. Its full joint two-endpoint theorem conditions were not inspected. The detailed joint rates and laws with both endpoints and shape estimated remain open; do not transplant a single-endpoint result without checking them.

These results do not validate production scores, covariance code, a bootstrap, robust sandwich inference or coverage. Follow the [inference plan](../../roadmap/2026-10-30-inference-and-dqm.md). In particular nonunique finite-sample fits, active restrictions, collapse outcomes and uncertified numerical fits need their own inference diagnostics.

## Reproducible evidence and verification

The independent [checker](2026-11-12-qgauss-theory-data/check.py) and [results](2026-11-12-qgauss-theory-data/check.json) use 80-digit mpmath. They check the normal derivative from both sides, three collapse exponents, an eta minus two endpoint-spike path, the bounded nuisance Hessian, normal information, uniform-corner attenuation, the held-mu large-eta bound, the existing seam counterexample, profile-normalizer curvature, exact two-point profiles and the signed uniform bounds. These are formula checks, not a global numerical search or fitted estimates.

For example, at m $=0.001$ the exact two-point profile deficit is about minus 0.01320451319 and its signed upper bound is minus 0.01320351419. The cusp ratio approaches two. The held-location bound is exact when every residual has the same nonzero magnitude.

Reproduce from this worktree:

    export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
    mkdir -p "$TMPDIR"
    /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-12-qgauss-theory-data/check.py

All started test commands finished with exit zero before this report was written. [Validation metadata](2026-11-12-qgauss-theory-data/validation.json) records the results and evidence hashes.

| Check | Final result | Machine |
|---|---|---|
| Independent high-precision checker | All assertions passed | Workstation, one-slot run |
| just cpp-test | 108/108 | Workstation GCC 15.2.0, run4 |
| just test | FAIL 0, WARN 0, SKIP 155, PASS 29000 | Workstation, run4 |
| just cpp-test-no-long-double | 135/135 | Workstation GCC 15.2.0, run4 |

Each requested simbox suite attempt returned 75 as unreachable, and used its authorized local fallback. No GCC 13 pass is claimed. The test runner unnecessarily repeated the workstation no-long-double run after the fallback; that repeat also finished 135/135. Scratch logs are under build/scratch. No command remains running.

All nine local Markdown destinations were checked and exist. The artifact-pane show command was attempted; the viewer returned HTTP 403 because this worktree is outside its configured readable roots. No viewer configuration or files outside this worktree were changed.

No production fitting, d/p/q/r or test infrastructure changed, so the conditional full R tier was not triggered. No dossier, signature, shared core, binding or matched message changed, so site and JS checks were not triggered. The repository suites check the existing implementation; the mathematical arguments and independent checker supply this report's evidence. No package installation, sub-agent, commit, board edit, push or publication was performed.

## Open items with precise scope

1. **General global uniqueness:** conditional concavity/geodesic convexity does not establish uniqueness in shape. Uniform is always a local profile maximum, and the exact normal-seam counterexample disproves unimodality shortcuts. A uniqueness theorem beyond the exact cases above is not available; a fitter must compare every surviving maximum and report ties.
2. **Production certificate and coefficient enclosures:** finite-interval and signed uniform/tail bounds are supplied, but their floating-point implementation, actual-hold normal remainders, degenerate root handling and end-to-end coefficient inclusion are TASK-203 work. Especially with sigma held, the affine-profile chord bounds cannot be imported unchanged. An inconclusive global certificate is not authorization for a new refusal.
3. **Joint nonregular inference:** the endpoint threshold, failure of DQM and spacing scales are established. Joint MLE laws/calibrations with unknown shape at eta at or below minus one third, including uniformity, still need a theorem checking both endpoints and nuisance estimation, followed by procedure-specific validation.
4. **Second compiler:** simbox was unavailable. GCC 13 validation remains unavailable operationally; the required fallback and workstation audit passed.

## Recommendations for TASK-202 and TASK-203

**TASK-202:** implement qgauss with mu, sigma and eta, over the full finite proper reference domain. Use normal at zero, lst with df $=1/\eta$ for positive eta, symmetric beta with shape $1/2-1/(2\eta)$ and half-width h for negative eta, and exact closed-support uniform at minus one. Handle endpoint density zero/finite/infinite explicitly. Supply tail-accurate beta/t probability and quantile branches, stable near-center reconstruction, and a two-sided normal seam with stable normalizer and derivatives. Preserve the distinction between scale and standard deviation. Accept the name qqgauss and document the conventional-q naming caveat.

**TASK-203:** estimate all three coefficients by default. Apply the two separate restriction records and their proved upper-bound exemptions. Diagnose actual collapse multiplicities and spiking held shapes before optimization. Use the bounded inverse-affine concave solve, the appropriate Student nuisance solver, and an adaptively covered scalar shape profile. Install the exact uniform shortcuts, uniform-neighborhood bounds, two-sided normal comparison, proper actual-hold tails and collapse envelopes. Normal, lst, uniform and Cauchy remain attained qgauss members; preserve free-parameter counts and holds through delegates. Retain nonuniqueness information and attainment witnesses. Meet tol in public coefficients and retain successful accurate candidates as uncertified when only global certification fails. Keep structural holds empty, attach data-dependent reasons to their diagnostics, and make no blanket numerical-inference claim before derivative and procedure validation.
