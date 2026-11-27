# Generalized beta and GB2 hold masks and uniqueness

TASK-405 · Base revision fc24c05ba5cc8eac994292af2e71f9cde1f3592c

The genbeta profile with endpoint and first beta shape held is strictly concave for every positive held shape. Two free-endpoint masks also have unique finite fits when the upper beta shape exceeds one and the sample is nonconstant. For GB2, holding either beta shape leaves two competitive faces. Exact finite ties and finite/face ties transfer from Burr and beta prime into the corresponding GB2 hold masks.

## Notation and envelope

For genbeta, \((p,q,a,b)\) are (shape1, shape2, shape3, scale). For GB2 they are (shape3, shape1, shape2, scale). All data are positive and finite, with \(n\ge1\). Objectives are mean log likelihood on log observations. The original total is \(n\) times this value minus \(\sum_i\log x_i\). Bars denote sample means including multiplicities.

For genbeta set \(M=\max x_i,\ d_i=\log(M/x_i),\ D=\bar d,\ s=\log(b/M),\ k=\#\{d_i=0\}\), and \(\lambda=pa\). Then
\[
 F=\log a-\log B(p,q)-\lambda(s+D)+(q-1)\overline{\log(1-e^{-a(s+d_i)})}.
 \tag{1}
\]
A free endpoint uses R7a at \(s=0\): density is infinite at contacts with \(q<1\), zero with \(q>1\), and the last term in (1) is zero at \(q=1\). We call infinite likelihood unbounded, even when it occurs at finite coefficients, and use “finite fit” to require a real maximum likelihood value. \(b<M\) puts observations outside support. The implemented held-endpoint domain requires all \(x_i<b\). If one extends it to R7a, contact with free \(q\) or held \(q<1\) yields the singularity, contact with held \(q>1\) yields zero joint likelihood, and at \(q=1\) the exponential has rate \(1/D\) if \(D>0\) or an unbounded likelihood if \(D=0\) and the rate is free. [R7a](../../roadmap/2026-10-29-parameter-rules.md)

**Proved comparison rule.** Suppose \(V\) is the highest approachable escaping value, bounded along every escaping subsequence and realized by some path. A finite point strictly above \(V\) forces attainment by compactness of its superlevel set. More generally a finite fit exists iff some finite stationary candidate (or allowable attained support-boundary candidate) has value at least \(V\). If its value equals \(V\), either a higher finite point forces attainment above \(V\), or this candidate itself attains. Every finite candidate strictly below \(V\) means nonattainment. On the unbounded free-\(q\) branch, R7a specifies pointwise contacts and joint upper semicontinuity need not hold at \(q=1,s=0\); the direct divergence proof handles that branch. A supremum of stationary values equal to \(V\) does not suffice without an actual witness. The attaining set consists of the candidates with greatest value. This uses the upper semicontinuous joint limsup envelope in the compactified coordinates and continuity at finite positive likelihood; the face formulas below retain simultaneous contact penalties. [Closure lemma](../ml-behaviour.tex#lem:closure) · [Tie classification](2026-11-25-ties-math-report.md)

## Genbeta free-endpoint proofs

### Singular paths and collapse

**Proved.** If \(q\) is free or held below one, fix \(p,a,0<q<1\) and let \(s\downarrow0\). The product grows as a positive constant times \(s^{-k(1-q)}\). By decreasing density on log distances for \(q<1\), each sampled density is at most \(1/(s+d_i)\). Thus a divergent sequence restricted to \(q<1\) must have \(s\to0\). For nonconstant data the log-concavity integral bound rules out divergent likelihood on \(q\ge1\). For all such subunit paths the prior equation (2) is the necessary and sufficient joint divergence test, with the present holds substituted. Its approximation error tends uniformly to zero since \(0\le R(ad)-R(a(d+s))\le\log(1+s/d)\) for \(d>0\), where \(R(z)=-\log(1-e^{-z})\). The corresponding weak laws are exactly the applicable rows of the prior weak-limit table with those same holds, transported by \(X=M e^{-Y}\). Every feasible weak direction in that table with \(q<1\) can be combined with a sufficiently fast endpoint approach, by choosing \(s\) separately for each fixed triple. This includes mixed laws when the exact holds permit them.

On constants with \(q\ge1\) every divergence forces weak collapse at the observation by the prior log-concavity proof. Equation (3) of that report, with holds substituted, gives the joint divergence test. If \(p,a\) are fixed, such divergence is impossible: the peak for \(q>1\) occurs at \(e^{-as}=p/(p+q-1)\); it is uniformly bounded for \(q\ge1\), with limiting height \(a p^p e^{-p}/\Gamma(p)\) as \(q\to\infty\) by the gamma ratio. With only \(p\) fixed, fix \(q>1\), take \(s=c/a\), and send \(a\to\infty\). With only \(a\) fixed, fix \(q>1\), use \(s=c/(pa)\), and send \(p\to\infty\). For any fixed \(c>0\) these give infinite heights by (1) and the gamma ratio. At \(q=1\) the constant-data divergence criterion is exactly \(\log\lambda-\lambda s\to\infty\), including contact. Exact subunit contacts already give infinite likelihood. [Joint rate tests](2026-11-25-genbeta-limits-math-report.md#rate-conditions-and-laws) · [Weak-limit table and log concavity](2026-11-25-genbeta-limits-math-report.md#4-weak-limit-appendix-and-domination-of-mixtures)

### Held upper shape greater than one

**Proved.** Fix \(q>1\) and take nonconstant data. \(1/B(p,q)\) is bounded above by \(C p\) for \(p\le1\) and \(C p^q\) for \(p\ge1\), by gamma ratios and continuity. Since \(1-e^{-t}\le\min(1,t)\), each density in (1) is bounded by
\[
 C\lambda[1+(\lambda(s+d_i))^{q-1}]e^{-\lambda(s+d_i)}.
\]
The peak is bounded by \(C'\lambda\). This excludes \(\lambda\to0\), and a positive \(d_i\) makes its exponential decay overwhelm all the polynomial peak factors when \(\lambda\to\infty\). With \(\lambda\) compact and positive, \(s\to\infty\) gives zero likelihood. With \(a\) also compact and positive, \(p\) is compact and \(s\to0\) gives zero likelihood at the \(k\) contacts.

At \(a\to0\) one has \(p\to\infty\). By \(1/B(p,q)\sim p^q/\Gamma(q)\) and \((1-e^{-ay})/(ay)\to1\) uniformly for bounded \(y\ge0\), the limit is the gamma likelihood at \(s+d_i\) with shape \(q\) and rate \(\lambda\), including zero density at contacts with \(s\to0\). In \((\lambda,c=\lambda s)\) the log objective is
\[
 G=\log\lambda-c-\lambda D+(q-1)\overline{\log(c+\lambda d_i)}-\log\Gamma(q).
\]
It is strictly concave and its finite superlevels are compact by the same density bounds, so there is a unique best value \(G_*\) and fitted pair. Every finite face value is realized with \(p=\lambda/a\) and \(s\) fixed.

At \(a\to\infty\), \(p\to0\) and \(1/B(p,q)\sim p\). With \(s\to s_0>0\) the limit is \(\log\lambda-\lambda(D+s_0)\). With \(s\to0\) and \(as\to z\) after subselection, it is
\[
 \log\lambda-\lambda D+(k/n)(q-1)\log(1-e^{-z}).
\]
At \(z=0\) this is \(-\infty\), and at \(z=\infty\) the penalty vanishes. The best power face is \(W=-1-\log D\), realized at \(\lambda=1/D,\ s=(\log a)/a\); equality requires \(as\to\infty\). The beta-gamma representation yields the respective shifted gamma and exponential weak laws. The exponential weak law still holds when the contact penalty is nonzero. These bounds exhaust the finite-level escapes. If either \(p\) or \(a\) is held, compactness of \(\lambda\) excludes both power limits and gives a finite maximizer.

**Proved signed large-power bound.** For held \(q>1\), take a finite level below \(W\). On that superlevel the preceding bounds give a finite upper bound on \(\lambda\). There is \(C>0\) with \(-\log B(p,q)-\log p\le Cp\) for sufficiently small \(p\), by differentiability at zero of \(pB(p,q)\). For large \(a\), the inequality applies throughout the superlevel. Dropping the nonpositive terms for \(d_i>0\), then optimizing over \(\lambda\), gives for \(z=as\)
\[
 F-W\le -\log(1+(z-C)/(aD))+(k/n)(q-1)\log(1-e^{-z}).
\]
Take \(aD>C\). For \(z\ge C\) the first term is nonpositive and the second strictly negative at every finite \(z\). For \(0<z<C\) the first is bounded by \(-\log(1-C/(aD))\), which tends to zero, and the second is bounded above by the strictly negative value at \(C\). Thus all sufficiently large powers lie strictly below \(W\). This is a signed tail exclusion; which of \(W,G_*\) and the finite stationary values wins still needs a global comparison.

**Proved finite-attainment sufficient test.** Let \((\nu,s_*)\) be the shifted-gamma fit, and suppose \(G_*\ge W\) and \(\overline{(s_*+d_i)^2}>q(q+1)/\nu^2\). On the feasible path \(\lambda=\nu-(q-1)a/2,\ s=s_*\) the objective is strictly above \(G_*\) for sufficiently small positive \(a\). To see this, put \(Q(t)=\log[\sinh(t/2)/(t/2)]\). The density in log distances, after inserting \(\lambda\), is proportional to the gamma kernel of rate \(\nu\) times \(\exp((q-1)Q(ay))\). Taylor expansion gives \(Q(t)=t^2/24+O(t^4)\). The normalization integral has quadratic coefficient \((q-1)\mathbb E[Y^2]/24\) for \(Y\sim\operatorname{Gamma}(q,\nu)\): \(0\le Q(t)\le \min(t^2/24,t/2)\), the quadratic bound from the product for \(\sinh(t/2)/(t/2)\) and \(\log(1+v)\le v\). Consequently the exponential difference quotient is bounded by a constant times \(y^2 e^{(q-1)ay/2}\), integrable under the gamma law uniformly for small \(a\). The finite objective's quadratic correction is \((q-1)(\overline{(s_*+d_i)^2}-q(q+1)/\nu^2)/24>0\). The comparison rule proves finite attainment.

## Genbeta regimes

### genbeta held scale

**Proved criterion and failure, inherited.** On nonconstant interior data the competitors are the optimized gamma on \(\log(b/X)\) and power with endpoint \(M\), with the stationary-value comparison. The full power trace requires \(q e^{-a\min\log(b/x_i)}\to0\). Constants are unbounded by concentration. [Nonconstant proof](2026-11-25-genbeta-limits-math-report.md#1-held-endpoint-nonconstant-data) · [Constant proof](2026-11-25-genbeta-limits-math-report.md#2-held-endpoint-constant-data)

**Uniqueness, partial.** The prior cover gives a singleton on the golden conditional on its guarded comparisons. At any nonconstant input the beta-shape Hessian at fixed power is negative definite, with analytic scores, so implicit differentiation gives an analytic power profile on \((0,\infty)\). The signed large-power bound is strictly below the limiting power value for sufficiently large finite power; hence the profile is nonconstant. A strict witness above both faces puts all maximizing powers in a compact interval, where the zeros of the analytic profile score are isolated and finite in number. The exact eight-point sample in the prior report proves two strict local maxima. Comparing those maxima to each other and to the faces is a separate step. General singleton or an exact global tie remains open. [Local multiplicity and golden](2026-11-25-gbshape-math-report.md#genbeta) · [Signed bound](2026-11-09-genbeta-allfree-data/proof.md)

### genbeta held scale+shape1

**Proved criterion and failure.** Nonconstant interior data have a finite global maximum by the uniform \(a\to0,\infty\) tails at held \(p\) and the uniform \(q\) tails on compact power intervals. Constants diverge with \(a\to\infty,\ q=p e^{a\log(b/x)}\). Every maximizing sequence on constants collapses at \(x\), with the joint rate test of the earlier constant-data proof. [Uniform tails](2026-11-07-genbeta-cert-data/bounds.md) · [Constant classification](2026-11-25-genbeta-limits-math-report.md#2-held-endpoint-constant-data)

**Proved singleton for every \(p>0\).** With \(z_i=a\log(b/x_i)\), put \(R=-\overline{\log(1-e^{-z_i})}\). The unique conditional optimum solves \(A(q)=R\) where \(A(q)=\psi(q+p)-\psi(q)\) decreases from infinity to zero. Put \(I=-A'(q)>0\), \(K=\overline{z_i/(e^{z_i}-1)}\) and \(J=\overline{z_i^2e^{z_i}/(e^{z_i}-1)^2}\). For the conditional profile \(P(a)\), the Schur complement gives
\[
 a^2P''=-1-(q-1)J+K^2/I.
\]
Cauchy–Schwarz gives \(K^2\le J\,\overline{e^{-z_i}}\), which by Jensen is at most \(J(1-e^{-R})\). Also \(J<1\) by \(2\sinh(z/2)>z\).

For all \(p,q>0\), \(qI>1-e^{-A}\). At fixed \(q\), the derivative of the difference in \(p\), with \(t=q+p\), is \(q[-\psi_2(t)]-e^{-A}\psi_1(t)\). Trigamma's integral gives \(\psi_1(t)>1/t\) and
\[
 t\psi_1(t)=\int_0^\infty e^{-v}\frac{v/t}{1-e^{-v/t}}\,dv.
\]
Here \(u/(1-e^{-u})\) increases strictly, so differentiation gives \(-\psi_2(t)>\psi_1(t)/t\). Integration of \(\psi_1(u)>1/u\) from \(q\) to \(t\) gives \(e^{-A}<q/t\). The difference thus increases from zero at \(p=0\). Therefore \(a^2P''<-1+J<0\), proving uniqueness together with the tail bounds. This extends the Kumaraswamy \(p=1\) result.

### genbeta held scale+shape2

**Proved criterion and failure, inherited.** For nonconstant interior data and \(q\ne1\), compare the stationary candidates to the fitted gamma with held shape \(q\); for \(q<1\) also include the power with the held endpoint \(b\). For \(q>1\) a finite point strictly beats that power limit, excluding it as a maximizer. On constants, \(q<1\) selects the unattained power and \(q>1\) the unattained gamma. [Nonconstant paths](2026-11-25-genbeta-limits-math-report.md#1-held-endpoint-nonconstant-data) · [Strict constant-data comparisons](2026-11-25-genbeta-limits-math-report.md#2-held-endpoint-constant-data)

**Proved refutation.** At \(q=1\), the full attaining set on all interior data is \(pa=1/\overline{\log(b/x_i)}\) by (1). For the exact binary64 sample \(b=1,\ x=(1/2,1/4)\), this gives \(pa=2/(3\log2)\) and infinitely many distinct pairs. The ridge reaches both power ends at the attained law. Cardinality for \(q\ne1\) remains open. [Earlier ridge proof](2026-11-07-genbeta-cert-report.md#uniqueness-and-additional-proved-bounds)

### genbeta held shape1

**Proved.** Every sample permits the singular endpoint spike because \(q\) is free. Constants also admit \(q\ge1\) collapse with \(a\to\infty\). The singular-branch rate tests with fixed \(p\) exhaust maximizing sequences. There is no finite fit, and all \(b=M,\ q<1,\ a>0\) give infinite extended likelihood.

### genbeta held shape1+shape2

**Proved.** With \(q<1\), the endpoint spike is unbounded, with precisely the singular-branch paths subject to fixed \(p,q\); every \(b=M,\ a>0\) gives infinite likelihood. At \(q=1,\ D>0\), the unique fit is \(b=M,\ a=1/(pD)\), since (1) is strictly decreasing in \(s\) and strictly concave in \(\lambda\). For \(q\ge1,\ D=0\) there is unbounded collapse with the earlier constant-data rate tests. For \(q>1,\ D>0\), the fixed-\(p\) exterior proof gives attainment. In \((a,c=as)\) the objective is strictly concave: \(\log a\) is strict in \(a\), and \(\log(1-e^{-(c+ad_i)})\) is strict in \(c+ad_i\). Every nonzero direction therefore has negative second variation. The unique fit is interior.

### genbeta held shape1+shape2+shape3

**Proved.** At \(q<1\) only \(b=M\) yields infinite extended likelihood, with every strict-interior maximizing path giving the fixed-shape divergence as \(b\downarrow M\). At \(q=1\), \(b=M\) is the unique finite fit even on constants. At \(q>1\), for any sample the score
\[
 a\left[-p+(q-1)\overline{1/(e^{a(s+d_i)}-1)}\right]
\]
decreases strictly from \(+\infty\) to \(-ap\). Its unique root gives the unique finite fit, with zero likelihood at contact or infinite endpoint.

### genbeta held shape1+shape3

**Proved.** With \(q\) free, every sample permits the singular spike. The uniform \(q\ge1\) peak bound with \(p,a\) fixed excludes divergence there even on constants. Every maximizing path inside support approaches \(b=M\) on the subunit branch and obeys its joint rate test. At \(q\to0\) the weak law under these exact holds is the endpoint atom, by the earlier weak-limit table. The finite maximizing set is empty, and all \(b=M,\ q<1\) give infinite extended likelihood.

### genbeta held shape2

**Proved criterion and failure.** At \(q<1\) every sample has the unbounded spike, with the subunit path test at this fixed \(q\). At \(q=1,\ D>0\), precisely \(b=M,\ pa=1/D\) maximize; the ridge can escape in either direction in \(p,a\) at the attained law. Constants are unbounded by collapse for \(q\ge1\), with the preceding rate tests. At \(q>1,\ D>0\), the exhaustive candidates are \(G_*,W\) of the free-endpoint proof and finite stationary values. For each fixed \(p\), the nuisance optimum \((a,c=as)\) has negative definite Hessian and is unique by the shape1+shape2 result, giving an analytic scalar profile in \(p\). A stationary point at or above \(\max(G_*,W)\) is necessary and sufficient for finite attainment. A maximizing gamma or power escape must use the fitted face parameters and, for power, \(as\to\infty\). Both faces can contribute if tied at the supremum.

**Proved refutation.** With held \(q=1\) and exact sample \((1/2,1)\), the entire ridge \(b=1,\ pa=2/\log 2\) maximizes. For \(q>1\) strict nuisance concavity does not order the outer stationary values in \(p\). General singleton and a direct sample test for finite/face equality remain open.

### genbeta held shape2+shape3

**Proved.** At \(q<1\) every sample has the unbounded spike with the joint rate test at fixed \(q,a\); all \(b=M,\ p>0\) give infinite extended likelihood. At \(q=1,\ D>0\), the unique fit is \(b=M,\ p=1/(aD)\). For \(q\ge1,\ D=0\), collapse is unbounded with the previous tests. At \(q>1,\ D>0\), compactness follows from the held-\(a\) exterior argument.

**Proof of uniqueness in the last case.** Conditionally on \(p\), \(c=as\) solves \((q-1)\bar h=p\) with \(h_i=1/(e^{c+ad_i}-1)\). The profiled second derivative in \(p\) is
\[
 -[\psi_1(p)-\psi_1(p+q)]+\frac{1}{(q-1)\overline{h_i(1+h_i)}}.
\]
Jensen bounds the positive term by \((q-1)/(p(p+q-1))\). Its integral is \(\int_0^\infty e^{-pt}(1-e^{-(q-1)t})\,dt\). The trigamma difference has integrand \(e^{-pt}t(1-e^{-qt})/(1-e^{-t})\), strictly larger since \(t/(1-e^{-t})>1\). Thus the profile is strictly concave.

### genbeta held shape3

**Proved.** Fixed \(a\) leaves the subunit spike possible on all samples, with no finite fit. Nonconstant maximizing strict-interior sequences obey the singular-branch test at fixed \(a\). Constants can also collapse at \(q\ge1\) by increasing \(p\), with the constant-data rate test at fixed \(a\). Every \(b=M,\ p>0,\ q<1\) gives infinite extended likelihood.

## GB2 exteriors with one beta shape held

Write \(y_i=\log x_i,\ v=\log b,\ z_i=a(y_i-v),\ S(z)=\log(1+e^z)\). Then
\[
 L=\log a-\log B(p,q)-p\overline{S(-z_i)}-q\overline{S(z_i)}.
\]
The log density on log observations is strictly concave with mode \(m=v+\log(p/q)/a\). Reflection of \(y_i\) interchanges \(p,q\) and negates \(v\), with a parameter-independent change of Jacobian on the original data scale. It suffices to hold \(p\).

**Proved compactness bounds.** If \(H\) is the peak and \(t_i=\log H-\log g(y_i)\), the log-linear integral between \(m,y_i\) gives \(1\ge H|m-y_i|/(1+t_i)\); use \((1-e^{-t})/t\ge1/(1+t)\). Hence \(L\le\log H+1-H\overline{|y_i-m|}\). The positive minimum mean absolute distance on nonconstant samples bounds \(H\) on each finite superlevel, and \(H\ge e^L\) bounds \(m\) there. The log-density slopes lie between \(-aq\) and \(ap\), so integrating exponential lower bounds on either side of \(m\) gives \(H\le\min(ap,aq)\). Thus \(a\to0\) is excluded at held \(p\). At bounded positive \(a\), \(q\to0\) is excluded, and bounded \(q\) keeps \(v\) compact by the mode identity. [Prior level bounds](2026-11-25-gbshape-math-report.md#uniform-exterior-bounds)

**Proved gamma face.** If \(a\to a_0>0,\ m\to m_0,\ q\to\infty\), use \(q e^{z_i}=p e^{a(y_i-m)}\) and the beta gamma ratio. The limiting log density is
\[
 \log a_0+p\log p-\log\Gamma(p)+pa_0(y_i-m_0)-p e^{a_0(y_i-m_0)}.
\]
It is transformed gamma on log observations with held shape \(p\). Pointwise convergence of normalized densities proves total variation convergence. Optimizing \(m\) gives \(e^{am}=\overline{e^{ay_i}}\), with profile
\[
 \log a+p\log p-\log\Gamma(p)-p-p\log\overline{e^{a(y_i-\bar y)}}.
\]
The log-sum-exp term is convex and \(\log a\) is strictly concave. This profile tends to \(-\infty\) as \(a\to0,\infty\) on nonconstant data and has a unique best value \(G_p^{(2)}\). Its maximizing \(a,m\) realize that value along \(q\to\infty\).

**Proved Pareto face.** If \(a\to\infty\), bounded \(H\) forces \(q\to0\). Indeed the strictly positive peak factor \(H/a=(p/(p+q))^p(q/(p+q))^q/B(p,q)\) is bounded below on \(q\ge\delta>0\) since it has positive limit at infinity. At \(q\to0\) it is asymptotic to \(q\); therefore \(\beta=aq\) is bounded above and away from zero. The mode and \(v\) differ by \(O(\log a/a)\). After passing to limits \(\beta_0,v_0\),
\[
 L=\log\beta+o(1)-\beta(\bar y-v)-(p+q)\overline{S(-z_i)} .
\]
If \(v_0>\min y_i\) the likelihood tends to zero. If \(v_0<\min y_i\) the softplus penalty vanishes. At \(v_0=\min y_i\), there is a penalty \(-p(k_-/n)S(-z_0)\) with \(k_-\) ties at the minimum and \(z_0=\lim a(\min y_i-v)\). The best value is \(E_-=-1-\log(\bar y-\min y_i)\). It is achieved as a trace with \(\beta=1/(\bar y-\min y_i)\), \(v=\min y_i-(\log a)/a\), and requires \(z_0=+\infty\). The limit log law is a right exponential with cutoff \(v_0\) by pointwise convergence away from the cutoff and normalization, giving Pareto on \(X\). These are the only two competitive exteriors. The prior signed free-scale tail bound shows that sufficiently large finite powers lie strictly below the one-sided face, even with a moving cutoff. [Signed large-power bound](2026-11-08-gb2-power-data/open-analysis.md) When \(q\) is held, reflect to obtain inverse transformed gamma and power.

For nonconstant data and fixed \(a,q\) at held \(p\), the unique location root solves \(\overline{(1+e^{-z_i})^{-1}}=p/(p+q)\) by strict monotonicity and the endpoint signs. The exact criterion compares the stationary points of the smooth profile over \((a,q)\) to \(\max(G_p^{(2)},E_-)\), with equality included. On constants set the mode at the observation and send \(a\to\infty\) with both shapes fixed. The peak diverges, and any sequence of diverging sampled densities forces collapse at the common observation by the log-concavity argument for genbeta. The exact rate condition on a constant is \(\log H-\log(H/g(y))\to\infty\) with \(H,g\) as above.

### gb2 held all free

**Uniqueness, partial.** The prior cover excludes all exteriors for the 500-observation golden and confines all maximizing roots to one strictly concave validated slab, giving a singleton conditional on computed comparisons. Exact log-affine equivariance transports this result. The prior exact five-point trough proves the free-power profile need not be unimodal. Held-mask counterexamples below leave a coefficient fixed; their global comparison does not apply to the all-free family. General uniqueness remains open. [Golden proof](2026-11-25-gb2-golden-math-report.md#finite-comparison) · [Trough proof](2026-11-25-gbshape-math-report.md#exact-non-unimodality-and-a-fixed-power-face-test)

### gb2 held shape1

**Proved criterion and failure.** Here \(q\) is held. The reflected exterior proof gives precisely inverse transformed gamma at held shape \(q\) and the power value \(-1-\log(\max y_i-\bar y)\) on nonconstant data, with the reflected contact penalty. Apply the stationary comparison including equality. Constants have only maximizing collapse.

**Proved refutation.** At \(q=1\) reciprocate the exact Burr sample in the next section. Reflection gives separated global maximizers above both faces. Reciprocation also transports its exact finite/Weibull tie to finite/inverse-Weibull equality. General cardinality remains open.

### gb2 held shape3

**Proved criterion and failure.** Here \(p\) is held. Compare stationary values to \(G_p^{(2)},E_-\) as proved above on nonconstant data. Constants have only maximizing collapse.

**Proved refutation and exact attained face equality.** At \(p=1\), \(B(1,q)=1/q\), so the entire held-shape space is Burr XII, with free \(q,a,b\). The exact sample uses the exponentials of \(N_0=2^{90}\) copies of each log in
\[
 Y(u_*)=(0,\ .23826u_*,\ 7u_*/15,\ 8u_*/15,\ 8u_*/15,\ 1,\ 1)
\]
and the exponential of one additional log observation \(-1\). Here \(u_*\) is the least \(u\in[0,1]\) at which the Burr profile maxima over \(q\in[1/(7N_0),1/8]\) and \(q\in[1/8,2^{24}]\) coincide. The parent proof supplies strict opposite comparisons at the ends, continuity of the compact profiles, strict exclusion of their separator, and exhaustion of the outside regions. Both finite branches are global and above both faces. That proof also gives a separate exact seven-observation finite/Weibull tie with value strictly above Pareto, by a least zero construction. These are real crossings. General cardinality beyond these examples remains open. [Two finite maxima](2026-11-25-burr2-math-report.md#two-finite-global-maximizers-above-both-faces) · [Finite/Weibull tie](2026-11-25-burr2-math-report.md#a-finite-maximum-equal-to-a-winning-weibull-face)

### gb2 held shape2

**Proved transfer, additional to the requested table.** With fixed \(a\), \(X^a\) is scaled beta prime with parameters \(p,q,b^a\). The scale map is bijective and the Jacobian \(a x^{a-1}\) independent of fitted coefficients. The completed beta-prime ratio profile has unique nuisance fits at each interior ratio and extends to the two gamma edges. Those edges are exhaustive on nonconstant data, giving transformed gamma and inverse transformed gamma on \(X\). Apply its stationary-value iff including equality. Constants have unbounded collapse. There is also an explicit nonattainment test: with centered logs of the transformed data \(X^a\) denoted by \(w_i\) and log span \(D_a\), if \(0<D_a\le .01\) and \(|\overline{w_i^3}|>69D_a^4\), the parent proof excludes all finite stationary points. Positive skew selects inverse transformed gamma and negative skew transformed gamma. The exact sample \(x_i=\exp(T z_i/a)\) with \(T=512\log2\), \(z=(-1,(-1/2)^{[10]},(1/2)^{[10]},1)\) has at least two finite global maximizers. At \(a=1\) every observation is exactly binary64. A finite witness beats both faces and the reflection-fixed slice \(p=q,b=1\), so a finite winner and its reflection are distinct. The parent's real crossing also gives exactly two finite maxima tied with both gamma faces. [Beta-prime profile and proof](2026-11-25-betapr-math-report.md) · [Binary64 structural tie](2026-11-25-betapr-math-report.md#binary64-finite-tie-and-binary64-face-tie) · [Four-way tie](2026-11-25-betapr-math-report.md#exactly-two-finite-maxima-tied-with-both-faces)

## Results by regime

“Refuted” rejects universal uniqueness; it does not count all maxima on arbitrary samples. The supplementary GB2 held-power result has no matching row in the current atlas.

| Family | Held | Criterion | Failure | Uniqueness |
|---|---|---|---|---|
| genbeta | scale | Proved comparison | Proved | Partial, golden and finite cardinality above faces |
| genbeta | scale+shape1 | Proved | Proved | Proved |
| genbeta | scale+shape2 | Proved comparison | Proved | Refuted by ridge |
| genbeta | shape1 | Proved | Proved | Proved, empty finite set |
| genbeta | shape1+shape2 | Proved | Proved | Proved |
| genbeta | shape1+shape2+shape3 | Proved | Proved | Proved |
| genbeta | shape1+shape3 | Proved | Proved | Proved, empty finite set |
| genbeta | shape2 | Proved comparison and closed cases | Proved | Refuted by ridge |
| genbeta | shape2+shape3 | Proved | Proved | Proved |
| genbeta | shape3 | Proved | Proved | Proved, empty finite set |
| gb2 | all free | Outside this card | Outside this card | Partial, golden |
| gb2 | shape1 | Proved comparison | Proved | Refuted |
| gb2 | shape3 | Proved comparison | Proved | Refuted |
| gb2 | shape2 | Proved comparison | Proved | Refuted |

## Python replay

**Numerical.** The workstation run at 80 decimal digits checked the curvature inequalities on 64 pairs \(p,q\in\{.01,.1,.5,1,2,10,100,10000\}\), restricting the fixed-power comparison to \(q>1\). The smallest sampled margin \(qI-(1-e^{-A})\) was \(5.00032833288\cdot10^{-11}\). It checked finite contact penalties and full exponential traces in both families at powers \(10^3,10^6,10^9\) on log distances \((0,0,1/4,2)\), and the GB2 gamma trace at the same three values of the escaping shape. The command exited zero on the workstation. [Script](2026-11-27-genbeta-atlas-math-data/check.py) · [Output](2026-11-27-genbeta-atlas-math-data/check-results.json)

~~~sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
study=dev/research/evaluations/2026-11-27-genbeta-atlas-math-data
/tmp/umvl-cpu/run nice -n 10 python3 "$study/check.py"
~~~

## What this changes for the fitter

- With genbeta \(p,b\) held and nonconstant interior data, bracket the unique power root. The conditional \(q\) solve is monotone. Propagate its uncertainty into the outer score signs and enclose both coefficients to the requested tolerance before giving a certified singleton.
- A future free-endpoint genbeta route can dispatch a free or subunit \(q\) to unbounded likelihood and solve \(q=1\) at \(b=M\). Return the identified rate and its full ridge when both factors are free. With \(q>1\) and one of \(p,a\) fixed, use the unique conditional/profile root on nonconstant data. With both held, the scale root is unique even on constants. With both free, test the shifted gamma and power faces as well as finite profile roots.
- GB2 held-beta-shape covers need only the corresponding transformed gamma and one-sided model. A held-power cover needs both transformed gamma faces. A strict finite witness above the exhaustively bounded faces proves attainment. A face-only result requires strict exclusion of finite equality. A proved finite/face tie retains the attained fit or maximizing set along with the tied-face evidence.
- Optional comparison or budget exhaustion retains a successful search explicitly uncertified. A proved contradiction or a failed search takes the applicable diagnostic route. No production changes were made.

## Dossier follow-ups

- The genbeta free-endpoint hold entries should distinguish current implementation support from mathematical existence. A closed endpoint gives the exact \(q=1\) fits, and holds that leave only scale free at \(q>1\) give a unique scale even for constants. The held-\(p,b\) entry can replace its general cardinality uncertainty by strict profile concavity.
- The GB2 one-sided large-power summaries should explicitly say which beta shape varies with \(a\) to keep the limiting slope positive and finite. At a fitted endpoint, tied extreme observations can retain a contact penalty even when the law converges to the named face.
- TASK-305 closed the earlier report's 395 unresolved GB2 golden cells. The separated finite maxima in the earlier genbeta power study are local maxima; their proof does not compare global heights.

## Open

- **Genbeta with endpoint alone held.** A strict gap above both faces makes the maximizing powers a finite set. A generic singleton proof still needs to order heights of the isolated maxima, or bound the profile so that only one can reach the global value. A refutation needs a global crossing with both modes above the two faces, with the rest of the profile bounded uniformly during the deformation.
- **Genbeta with \(q\ne1\) held.** At held endpoint the conditional \(p\) curvature does not sign the power Schur complement. With free endpoint and \(q>1\), the strict \((a,as)\) Hessian gives a unique nuisance fit but leaves the outer \(p\) profile unclassified. A sample-only equality or boundary detector still needs a signed refitted comparison at a potentially winning gamma seam, including cancellation of the quadratic term, and an exact value comparison of remaining stationary candidates.
- **GB2 all free.** The golden singleton and trough do not order finite branch heights on arbitrary data. To extend a held-shape tie one must bound the exteriors and remaining finite cells after releasing the held shape or power. Reflection would produce a second winner upon proving a strict gap over the reflection-fixed slice and all exteriors on the same symmetric sample.
- **GB2 single beta shape held away from one.** The exhaustive two-face comparison and the unit-shape refutations settle the criterion, failure and universal uniqueness questions. Full cardinality on other samples still requires uniform stationary exclusion and an exact comparison of finite roots in the two-dimensional profile.
