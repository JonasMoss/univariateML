# Generalized beta likelihood limits

TASK-277 · baseline bb391a57789c0f23ea7501668d07ce4deac09955

**Proved.** With the upper endpoint held and nonconstant interior data, every maximizing escape of the three-shape fit reaches either the fitted gamma law on log distances or the fitted power law with endpoint at the sample maximum. The power escape must also have a vanishing density penalty at the tied maxima. With \(q\) held, the candidate power law retains the held endpoint. With \(q=1\), escapes represent an attained identification ridge. No mixed weak limit can maximize any supported held-endpoint fit.

With the endpoint free, the supremum is infinite on every positive sample. Mixed weak limits can occur along sequences reaching that supremum. In particular, for the exact sample \((1/2,1)\), a sequence converges weakly to half uniform on \((0,1)\) plus half an atom at 1 while its likelihood tends to infinity. The atomic weight does not define a density-based likelihood for the limiting measure. The divergence belongs to the moving-endpoint sequence.

## Setup and prior results

Let \(p,q,a\) be the two beta shapes and the power, and let \(b\) be the upper endpoint. For \(X=bU^{1/a}\) and \(U\sim\operatorname{Beta}(p,q)\), set \(Y=\log(b/X)\). A data set has \(n\ge1\) finite positive observations. For the supported holds the data must satisfy \(x_i<b\). Write
\[
y_i=\log(b/x_i),\quad m=\min_i y_i,\quad \mu=\overline y,\quad
d=\mu-m,\quad R(t)=-\log(1-e^{-t}),\quad r_a=\overline{R(ay_i)}.
\]
Write \(M=\max_i x_i\) and \(k=\#\{i:x_i=M\}\). With \(N(p,q)=-\log B(p,q)\), the log likelihood on \(X\) is \(nF-\sum_i\log x_i\), where
\[
 F=\log a+N(p,q)-pa\mu-(q-1)r_a. \tag{1}
\]
For fixed \(b\), define the optimized shifted-exponential value
\[
 W(c)=-\log(\mu-c)-1,\qquad 0\le c<\mu.
\]
The corresponding rate is \(1/(\mu-c)\). Its law on \(X\) is power with endpoint \(be^{-c}\). Define the gamma value at shape \(q\) after fitting its rate by
\[
 G_q=q\log(q/\mu)-q-\log\Gamma(q)+(q-1)\overline{\log y}.
\]
For nonconstant data, \(G=\max_{q>0}G_q\) is attained at the unique \(q_*\) solving \(\log q_*-\psi(q_*)=\log\mu-\overline{\log y}>0\). Its rate is \(q_*/\mu\). All reported face values are on the scale of \(F\) unless the \(X\)-likelihood is explicitly given.

The October reports establish the unrestricted endpoint singularity and the fixed-power beta transfer. The November certificates supply fixed-power shape rectangles, uniform tails for held \(p\), signed large-power comparisons and uniform all-free gamma bounds. This report uses those results and adds restrictions on the coefficients of **arbitrary** maximizing sequences, including sequences that never maximize a conditional profile. The concentration and endpoint analyses below also cover constant samples. The mathematical scope is the unrestricted model and the fitter's eight held-endpoint masks. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) · [October base analysis](2026-10-29-genbeta-report.md) · [P0](2026-10-29-p0-genbeta-report.md) · [P2](2026-10-29-p2-genbeta-report.md) · [Held-shape and power certificates](2026-11-07-genbeta-cert-report.md) · [Held-\(q\) certificate](2026-11-08-genbeta-rest-report.md) · [Three-shape certificate](2026-11-09-genbeta-allfree-report.md) · [Beta transfer](2026-11-25-transfer-math-report.md)

The October base report already gave the fixed-endpoint mixed atom–power parameterization at positive finite \(p/q\), in addition to the half-uniform example checked in TASK-117. Its abbreviated description of the pure-power path should require control of \(q\) near zero and, when the endpoint meets an observation, a separate density condition. The results below supply both. [TASK-117](2026-10-31-open-existence-report.md#4-generalized-beta-closure-versus-maximizing-escapes)

## 1. Held endpoint, nonconstant data

**Proved classification.** Take subsequences so that coefficients converge in \([0,\infty]\) and, where needed, the indicated composite coordinates converge. A sequence that leaves every compact subset and tends to the supremum has only the following escaping subsequences. Each row is sufficient when its face value is the global supremum of \(F\).

| Free shapes | Directions | Weak limit in \(Y\) | Limiting \(F\) |
|---|---|---|---|
| \(p,q,a\) | \(a\to0,\ p\to\infty,\ q\to q_*,\ pa\to q_*/\mu\) | Gamma of shape \(q_*\), rate \(q_*/\mu\) | \(G\) |
| \(p,q,a\) | \(a\to\infty,\ p\to0,\ pa\to1/d,\ q\to\infty,\ \log q/a\to m,\ q e^{-am}\to0\) | \(m+\operatorname{Exp}(1/d)\) | \(W(m)\) |
| \(p,a\), \(q\ne1\) held | \(a\to0,\ p\to\infty,\ pa\to q/\mu\) | Gamma of held shape \(q\), rate \(q/\mu\) | \(G_q\) |
| \(p,a\), \(0<q<1\) held | \(a\to\infty,\ p\to0,\ pa\to1/\mu\) | \(\operatorname{Exp}(1/\mu)\) | \(W(0)\) |
| \(p,a\), \(q=1\) held | \(a\to0,\ p\to\infty\) or \(a\to\infty,\ p\to0\), with \(pa\to1/\mu\) | \(\operatorname{Exp}(1/\mu)\) | \(W(0)\), attained |

There are no escaping maximizers for the other six masks. These are no free shapes, only \(p\), only \(q\), only \(a\), \(p,q\) at fixed \(a\), and \(q,a\) at fixed \(p\). The same statement includes maximizers that alternate between available faces. The individual subsequences carry the stated limits. For a sequence that also visits compact sets, any interior cluster point is a finite maximizer, by continuity.

The criterion “face value equals the global supremum” can be expressed using stationary points. Form the one-dimensional profile by maximizing the free beta shapes at each positive power. Compare the listed face values with all values at finite stationary points. The supremum is the greatest of the face values and the supremum over those stationary values, taking an empty stationary set to have supremum \(-\infty\). A strict improvement over all boundary values forces attainment in the interior, so in that case a stationary point attains the comparison. Equality with a face permits both finite maximizers and escaping sequences. This characterization does not assume a finite number of stationary points. The \(q=1\) row follows from the exact exponential likelihood \(\log(pa)-pa\mu\).

### Why this is exhaustive

A concavity observation will be used twice. Suppose concave objectives on a fixed convex open domain converge uniformly on each interior compact set to an objective with a unique interior maximizer \(z_*\). In the applications the limit is strictly concave. Take any sufficiently small closed ball about \(z_*\). The limiting values on its sphere are strictly below the center, uniformly. For large indices the same holds for the approximating objectives with a fixed gap. A point outside the ball and the center span a segment meeting the sphere. Concavity bounds that point's objective above by the center value minus at least the sphere gap. Therefore the global profile converges to the limiting maximum. Any points achieving that limiting maximum in the sequence must converge to \(z_*\). The argument also works for a one-dimensional concave profile formed by taking a supremum over the other coordinate of a jointly concave objective. Indeed two approximate conditional maximizers and their convex interpolation prove the profile concavity.

**Power bounded above and away from zero.** Along \(a\to a_0>0\), the beta log likelihood is strictly concave in its free shapes and converges locally uniformly to the fixed-\(a_0\) objective. The nonconstant transformed sample has a unique interior beta optimum, also in each nonempty one-shape slice. Concavity on spheres therefore excludes shape escapes for maximizing sequences. With both shapes held there is no shape coordinate to escape in this case. The fixed-power beta claim and the uniform conditional rectangles are established in the earlier work. [Fixed-power transfer](2026-11-25-transfer-math-report.md#1-transfers) · [Conditional rectangles](2026-11-09-genbeta-allfree-data/proof.md)

**Power tends to zero with \(p\) free.** Use \(\lambda=pa\). At every \(a>0\), \(F\) is concave in \((\lambda,q)\) or in \(\lambda\) for a held \(q\). On positive compact sets of \((\lambda,q)\),
\[
 F\longrightarrow q\log\lambda-\log\Gamma(q)-\lambda\mu
                  +(q-1)\overline{\log y}.
\]
This uses \(\log\Gamma(p+q)-\log\Gamma(p)=q\log p+o(1)\) uniformly on compact \(q\) ranges, and \(1-e^{-ay_i}\sim ay_i\). The limiting objective is strictly concave in \((\lambda,q)\) by the positive definite covariance of the gamma sufficient statistics \((Y,\log Y)\). Its unique maximizing parameters are those above. The sphere argument gives compactness of all near-optimal shape sequences, including paths with a proposed \(q\to0\) or \(q\to\infty\). The same local likelihood formula holds for constant data at a held \(q\), because the rate maximization still has a unique optimum. The beta–gamma representation, proved in §4, gives the weak gamma limit.

**Power tends to infinity with \(q\) held and \(p\) free.** For compact positive \(\lambda=pa\),
\[
 F=\log\lambda-\lambda\mu+o(1).
\]
Here \(N(p,q)=\log p+o(1)\), and \(r_a\to0\). The same concavity argument in \(\lambda\) yields the fitted rate \(1/\mu\). For \(q>1\) the prior held-\(q\) proof gives a finite improvement of order \(1/a\) at \(\lambda=1/\mu\), with positive leading coefficient \((\psi(q)-\psi(1))/\mu\). Thus this power limit cannot maximize for \(q>1\), even for constants. For \(0<q<1\) the prior signed bound shows that the sufficiently large finite-power profile lies below \(W(0)\). A global comparison still requires the gamma face and the remaining finite region. [Held-\(q\) tails](2026-11-08-genbeta-rest-data/held-q-proof.md#uniform-tails) · [Signed \(q<1\) tail](2026-11-07-genbeta-cert-data/remaining-profile-bounds.md#-a-signed-fixed-endpoint-power-neighborhood-when-q1-is-held)

**Power tends to infinity with both beta shapes free.** Let \(P_a(\lambda)=\sup_{q>0}F(a,\lambda/a,q)\). At fixed \(\lambda>0\) the uniform \(q\)-elimination bound in the previous all-free proof, in unnormalized \(Y\) units, is
\[
 F\le \log a+r_a+p\log p-p-\log\Gamma(p)-p(a\mu+\log r_a-r_a),
 \qquad r_a<1.
\]
It follows by integrating \(\psi(t)\le\log t\) over \([q,q+p]\) and maximizing the resulting \(p\log(q+p)-qr_a\). Since \(r_a\sim(k/n)e^{-am}\), the right side converges locally uniformly in \(\lambda\) to \(\log\lambda-\lambda d\). For a lower witness take \(q=e^{am}/a\). Uniformly on compact positive \(\lambda\) ranges, \(\log\Gamma(q+p)-\log\Gamma(q)=p\log q+o(1)\) and \(q r_a\to0\). The witness achieves the same limit. The concavity observation for \(P_a\) forces \(\lambda\to1/d\) for every maximizing escape and bounds the best large-power value by \(W(m)\). [Previous uniform bound](2026-11-09-genbeta-allfree-data/proof.md#eliminating-an-enormous-q-analytically)

It remains to recover the direction of \(q\) from equality. If \(q\) stays bounded while \(p\to0\), then \(\limsup(N(p,q)-\log p)\le0\). For \(q\) bounded below this difference tends to zero. If \(q\to0\) it equals \(\log(q/(p+q))+o(1)\). Thus bounded \(q\) can reach at most \(W(0)<W(m)\), since \(m>0\). For \(q\to\infty\), write \(D=q r_a\). Uniformly for bounded positive \(\lambda\),
\[
 F=\log\lambda-\lambda d+\frac{\lambda}{a}\log D-D+o(1).
\]
For \(D\ge1\), \(\lambda\log D/a\) is at most \((\lambda/a)D\). For \(0<D<1\) both corrections are nonpositive. Equality in the limit therefore requires \(D\to0\) and \(\log D/a\to0\). These are equivalent to the two conditions on \(q\) in the table. For example, if \(q r_a\to D_0>0\), the weak law is still \(m+\operatorname{Exp}(\lambda)\), but \(F\) tends to \(\log\lambda-\lambda d-D_0\). The mass at any tied sample maximum is zero in the weak law, and its density value there is resolved by the approach to the boundary.

**Held \(p\) or held \(a\).** At held \(p\) and free \(q,a\), the prior uniform tail bounds give \(F\to-\infty\) as \(a\to0\) or \(a\to\infty\), for every \(q\) path on nonconstant data. Together with the bounded-power argument this excludes all escapes. At fixed \(p,q\), \(F\) tends to \(-\infty\) at the two power ends directly by (1), with the small-\(a\) leading term \(q\log a\) and the large-\(a\) leading term \(-pa\mu\). The six masks follow. This argument plus continuity of the conditional maxima on positive powers also proves the stationary comparison above. [Held-\(p\) uniform bounds](2026-11-07-genbeta-cert-data/bounds.md)

## 2. Held endpoint, constant data

**Proved.** Write \(y=\log(b/x)>0\) for the common log distance.

| Free shapes | Maximizing escapes |
|---|---|
| \(p,q\), \(q,a\), or \(p,q,a\) | Unbounded likelihood. Every maximizing sequence has \(q\to\infty\) and \(X\Rightarrow\delta_x\). The rate conditions below characterize the allowed joint directions, subject to the holds. |
| \(p,a\), held \(0<q<1\) | Only \(a\to\infty,\ pa\to1/y\). Fixed-endpoint power, \(F=-1-\log y\). No finite maximizer. |
| \(p,a\), held \(q>1\) | Only \(a\to0,\ pa\to q/y\). Gamma of held shape and rate \(q/y\), \(F=G_q\). No finite maximizer. |
| \(p,a\), held \(q=1\) | The attained ridge and its two escapes from §1 with \(\mu=y\). |
| No free shapes, or only \(p\), \(q\) or \(a\) | No maximizing escape. The one-free cases have an interior maximum. |

For the last row this follows from the one-shape beta scores and the fixed-shape power tails. At \(q<1\), the binomial series for \((1-e^{-ay})^{q-1}\) has strictly positive coefficients at every integer order. The \(Y\) density is hence an exponential mixture with rates \(a(p+j)\) for \(j=0,1,\ldots\). The mixture weights are obtained by integrating its nonnegative terms and sum to one. Every exponential density at \(y\) is at most \(1/(ey)\), with equality only for rate \(1/y\). At finite \(p,a\) the rates differ, so the bound is strict. The fixed-endpoint power path reaches it. At \(q>1\), \(\log[(1-e^{-t})/t]\) is strictly convex in \(t>0\) because its second derivative is \(1/t^2-1/(4\sinh^2(t/2))>0\). In the density of \(T=aY\), keep \(t^{q-1}\) and replace this log factor by its tangent at \(t_0=ay\). The tangent gives a gamma kernel with positive rate \(p+(q-1)(1/t_0-1/(e^{t_0}-1))\), agreeing with the original kernel at \(t_0\) and integrating to strictly less than it. After normalization, the gamma density is higher at \(t_0\). Maximizing its value over rates gives \(G_q\) on \(Y\). The small-power gamma path reaches it. The two compactness arguments from §1 apply at held \(q\), and strictness at finite parameters excludes the other directions.

For the unbounded masks, the known witnesses take \(p=s e^{-ay},\ q=s(1-e^{-ay})\) at fixed \(a\), or \(a\to\infty,\ q=p e^{ay}\) at fixed \(p\). In the first case the density grows as a positive constant times \(\sqrt{s}\). In the second it grows as \(a p^p e^{-p}/\Gamma(p)\). To see necessity of \(q\to\infty\), when \(q\le1\) the density on \(Y\) decreases, so its value at \(y\) is bounded by \(1/y\). For \(1\le q\le Q<\infty\), uniformly \(1/B(p,q)\) is bounded by \(C_Q p\) for \(p\le1\) and \(C_Q p^q\) for \(p\ge1\), from the gamma ratio and continuity at \(p=0\). If \(a\ge1\), use \((1-e^{-ay})^{q-1}\le1\) and boundedness of \((pa)^q e^{-pay}\) and \((pa)e^{-pay}\). If \(a\le1\), use \(1-e^{-ay}\le ay\) and the same boundedness. This gives a uniform upper bound at \(y\) whenever \(q\) is bounded. When \(q\ge1\), the log density on log observations is concave. A diverging density at the common observation forces weak concentration there, by the log-concavity argument in §3.

Here are **necessary and sufficient rate conditions** for the unbounded constant masks. They also apply to constant-data concentration in §3, using its varying \(y\). Set \(u=e^{-ay}\). Along \(q\to\infty\), split into \(p\to\infty\) and bounded \(p\). In each case the expression below must tend to \(+\infty\). It equals the per-observation log density on \(Y\) up to additive \(o(1)\).

- When \(p,q\to\infty\), use
  \[
  \log\frac{a}{1-u}+\frac12\log\frac{pq}{2\pi(p+q)}
       -(p+q)K\!\left(\frac{p}{p+q},u\right),
  \quad K(v,u)=v\log(v/u)+(1-v)\log((1-v)/(1-u)).
  \]
- When \(q\to\infty\) and \(p\) stays bounded, including \(p\to0\), use
  \[
  \log a+p\log(qu)-\log\Gamma(p)+(q-1)\log(1-u).
  \]

The first formula follows by Stirling expansion of the three gamma terms and the second by \(\log\Gamma(q+p)-\log\Gamma(q)=p\log q+o(1)\), uniformly for bounded \(p\). These criteria retain the density penalties, including near the edge of a shrinking concentration region. They permit rates of \(p,q,a\) to vary independently subject to the displayed inequalities. A mere weak convergence to \(\delta_x\) does not guarantee divergence of the sampled density.

## 3. Endpoint free

**Proved.** The original four-parameter problem has infinite supremum. The October fixed-\(p,q,a\) witness takes \(0<q<1\) and \(b\downarrow M\), giving likelihood of order \((b-M)^{-k(1-q)}\). The classification below uses \(b>M\), so it does not depend on the density convention at \(x=b\). If the specified endpoint density \(+\infty\) at \(q<1\) is included in the optimization, then \(b=M,\ q<1\) already gives infinite likelihood. Such points may even have finite coefficients. Escapes within that infinite-likelihood set have \(b=M\) and the same allowed weak limits in §4. In this mathematical classification an infinite endpoint density is treated as an unbounded-likelihood outcome.

For nonconstant data, every maximizing subsequence in the strict interior eventually has \(q<1\) and \(b\to M\). To prove this, work on \(\log X\) with its density \(g\), which is log-concave for \(q\ge1\), including its support interval. Let \(A\) be the largest sampled density and \(B\) the density at any distinct sample point from the one giving \(A\). The two log observations are separated by at least a fixed \(\eta>0\). Log-concavity bounds the integral on their interval below by \(\eta(A-B)/\log(A/B)\), with its continuous interpretation for \(A=B\). If \(A\to\infty\), integration forces \(B\le A/2\) eventually, then \(B\le A e^{-\eta A/2}\). Hence the product of sampled densities is at most \(A^n e^{-\eta A/2}\) in that regime, and is bounded also when \(A\) is bounded. A sample point of density zero gives product zero. This bound is independent of \(b,p,q,a\). For \(q<1\), the density of \(Y\) decreases, so every sampled density is at most \(1/y_i\). If \(b\) is separated above \(M\), these bounds prevent divergence of the product. Thus the only possibility is the approaching endpoint.

For a constant sample, every strict-interior maximizing subsequence has one of two forms. Its \(q<1\) subsequences must have \(b\to x\) by the same decreasing-density bound. Its \(q\ge1\) subsequences may have any allowed endpoint direction, but \(X\Rightarrow\delta_x\). Indeed if \(g(\log x)=A\to\infty\), on each side at distance at most \(\epsilon\) and at least \(\epsilon/2\) there is a point with \(g\le2/\epsilon\), by integration. If the support ends earlier the corresponding outer tail is zero. Concavity of the log density bounds the density beyond that point by an exponential with initial height at most \(2/\epsilon\) and decay rate at least \(\log(A\epsilon/2)/\epsilon\). Both outer tail probabilities tend to zero. This argument applies also to the held-endpoint constant samples.

### Rate conditions and laws

For \(q<1\) and \(b\to M\), put \(m=\log(b/M)\), \(d_i=\log(M/x_i)\), and \(\bar d=\overline{d_i}\). The necessary and sufficient condition on the joint direction is that
\[
 \log a+N_0(p,q)-pa(\bar d+m)
 +(1-q)\left(\frac{k}{n}R(am)+\frac1n\sum_{d_i>0}R(ad_i)\right)\ \longrightarrow\ +\infty. \tag{2}
\]
Here \(N_0\) may be taken from the expansions below. The expression equals \(F+o(1)\), since \(0\le R(ad_i)-R(a(d_i+m))\le\log(1+m/d_i)\) by \(-R'(t)=1/(e^t-1)\le1/t\). This retains possible simultaneous divergence of the negative \(pa\) term and the positive endpoint term. The weak limit is obtained from §4 by \(X=M e^{-Y}\) in the limit. Conversely, every weak law in §4 realizable with \(q<1\) can occur on such a maximizing sequence: for each set of shapes choose \(m>0\) sufficiently small that the sampled log likelihood is at least the sequence index and \(m\le1/\text{index}\). This is possible by the fixed-shape endpoint singularity at that step. The selected \(m\) does not affect the weak limit of \(X/M\).

For \(q\ge1\) on a constant sample, take \(y=\log(b/x)\). With strict inequality \(b>x\), the necessary and sufficient condition is divergence to \(+\infty\) of
\[
 \log a+N_0(p,q)-pa y+(q-1)\log(1-e^{-ay}). \tag{3}
\]
When both shapes diverge, the relative-entropy expression in §2 supplies the equivalent separated penalty. When a shape is bounded, use the following explicit expansion for \(N_0\) in (2) or (3). It is valid with additive \(o(1)\) along each indicated subsequence.

| Shapes on the subsequence | \(N_0(p,q)\) |
|---|---|
| Both converge to positive finite constants \(p_0,q_0\) | \(-\log B(p_0,q_0)\) |
| Both tend to zero | \(\log(pq/(p+q))\) |
| One tends to zero and the other to a positive finite constant | Log of the vanishing shape |
| One diverges, say \(q\), and the other stays bounded | \(p\log q-\log\Gamma(p)\), with roles swapped when \(p\) diverges |
| Both diverge | \(S(p+q)-S(p)-S(q)\), where \(S(t)=(t-1/2)\log t-t+\tfrac12\log(2\pi)\) |

These follow from \(t\Gamma(t)\to1\), the uniform gamma ratio for bounded increments, continuity and Stirling's formula. Thus (2) and (3) also decide directions with \(a\) or \(b\) on extremely different scales from the beta shapes. Their limiting log likelihood is \(+\infty\) when the rate condition holds. At the literal endpoint \(b=x\) with \(q=1\), the constant-sample likelihood tends to infinity precisely when \(pa\to\infty\), and again the weak limit is \(\delta_x\). At \(b=x,\ q>1\) the specified density is zero.

### Exact mixed example

Take \(x=(1/2,1)\), \(a=t,\ p=q=1/t,\ b=e^{1/t^3}\). The shapes satisfy the \(1/2\) mixture in §4 and \(b\to1\). At \(x_1=1/2\) the density tends to \(1/2\), since \(a/B(p,q)\to1/2\), \(\exp(-pa y_1)\to1/2\) and the Jacobian is 2. At \(x_2=1\), \(a y_2=t^{-2}\) and \((1-e^{-t^{-2}})^{1/t-1}\sim t^2\), giving density asymptotic to \(t^2/2\). Hence the likelihood is asymptotic to \(t^2/4\). The mixed law is an **upper-endpoint atom plus a power law**, here half an atom at 1 and half a uniform law. There is no strictly better finite likelihood than this escape, and no distinguished winning mixed model, because the same infinite supremum is also achieved by nonmixed weak paths.

## 4. Weak-limit appendix and domination of mixtures

**Proved.** Use laws on the compactified half-line \([0,\infty]\) for \(Y\). For fixed positive \(b\), or \(b\to M>0\), this corresponds to ordinary weak convergence on the closed bounded \(X\) support under \(X=b e^{-Y}\). Let \(T=aY\), and let \(G_s\) denote a rate-one gamma variable of shape \(s\). Two independent gammas give \(U=G_p/(G_p+G_q)\) by changing variables to the sum and the fraction in their joint density. Then
\[
 T=\log(1+G_q/G_p).
\]
For \(s\to\infty\), \(G_s/s\to1\) in probability by its variance. For \(s\to s_0>0\) the gamma density converges to that of \(G_{s_0}\), with convergence in distribution by normalization and pointwise convergence. For \(s\to0\), \(-s\log G_s\Rightarrow\operatorname{Exp}(1)\). At a positive exponential-tail threshold \(z\), integrate the gamma density to \(e^{-z/s}\), bounding \(e^{-t}\) by its endpoints, and use \(s\Gamma(s)\to1\). The probability \(G_s\ge1\) tends to zero by its mean. Together these facts give the table. Limits of the composite coordinates are taken after subselection. A scaled positive variable divided by a quantity tending to zero or infinity gives the appropriate atom at infinity or zero.

| Shape directions | Limit of \(Y=T/a\) |
|---|---|
| \(p,q\) positive finite limits | \(T\) has the corresponding log-beta law in the limit. Divide by the limiting power, including atom limits for \(a\to0,\infty\). |
| \(q\to q_0>0\) finite, \(p\to\infty\) | \(pT\Rightarrow G_{q_0}\). Divide by \(pa\). |
| \(q\to q_0>0\) finite, \(p\to0\) | \(pT\Rightarrow\operatorname{Exp}(1)\). Divide by \(pa\). |
| \(q,p\to\infty\) | \(T/\log(1+q/p)\to1\) in probability. The point mass is at the limit of \(\log(1+q/p)/a\). |
| \(q\to\infty,\ p\to p_0>0\) finite | \(T/\log q\to1\) in probability. The point mass is at the limit of \(\log q/a\). |
| \(q\to\infty,\ p\to0\) | A shift by the limit of \(\log q/a\), plus an \(\operatorname{Exp}(1)\) divided by the limit of \(pa\). A shift or scale that sends the positive variable to infinity gives \(\delta_\infty\). |
| \(q\to0,\ p\to p_0>0\) or \(p\to\infty\) | Only atoms at 0 and \(\infty\). Mass at \(\infty\) is \(1-e^{-L_+}\), where \(L_+=\max(0,\lim[-q\log(pa)])\). |
| \(p,q\to0,\ a\to\infty\) | At positive finite \(y\), survival tends to \(w e^{-\lambda y}\), where \(w=\lim q/(p+q)\) and \(\lambda=\lim pa\). For \(0<\lambda<\infty\) this is mass \(1-w\) at 0 plus weight \(w\) on \(\operatorname{Exp}(\lambda)\). At \(\lambda=0\) the weight \(w\) goes to \(\infty\), and at \(\lambda=\infty\) all mass goes to 0. |
| \(p,q\to0,\ a\to a_0>0\) finite | Atoms at 0 and \(\infty\) with mass \(w=\lim q/(p+q)\) at \(\infty\). |
| \(p,q\to0,\ a\to0\) | Atoms at 0 and \(\infty\), with mass \(1-\lim [(p/(p+q))a^q]\) at \(\infty\). |

For the first six rows use the representation for \(T\). When both gammas concentrate relatively, multiplying the ratio \(q/p\) by \(1+o_P(1)\) changes \(\log(1+q/p)\) by a relative \(o_P(1)\), by concavity of \(\log(1+t)\) and its value zero at zero. When \(p\to0,\ q\to\infty\), \(G_q/G_p\to\infty\) in probability, and \(p(T-\log q)\Rightarrow\operatorname{Exp}(1)\). This yields the stated shift and scale with nonnegative limiting terms. When \(q\to0\) and \(p\) is bounded below, \(G_q/G_p\to0\) in probability, so \(pT/G_q=(p/G_p)(1+o_P(1))\). The log of this factor is tight, hence \(-q\log(pT)\Rightarrow\operatorname{Exp}(1)\). For each fixed \(y>0\), compare with the threshold \(-q\log(pay)\) to obtain the row for that case.

When both shapes vanish, use the beta CDF \(H(u)\). Uniformly for \(0<u\le1/2\), \(H(u)=\frac{q}{p+q}u^p+o(1)\). Indeed \((1-t)^{q-1}-1=O(t)\) for \(t\le1/2\) and small \(q\), so the unnormalized integral differs from \(u^p/p\) by a bounded term. Multiply by \(1/B(p,q)\sim pq/(p+q)\). For \(u\ge1/2\), symmetry gives \(1-H(u)=\frac{p}{p+q}(1-u)^q+o(1)\). Apply the two formulas at \(u=e^{-ay}\) to obtain all three rows, using \((1-e^{-ay})^q/a^q\to1\) when \(a\to0,\ q\to0\). Constant survivor values for all positive \(y\) identify the endpoint-atom weights. These arguments cover all shape limits and the complementary power directions.

**Mixed limits with \(b\) held.** The only mixture with a continuous component is \(w\operatorname{Exp}(\lambda)+(1-w)\delta_0\), with \(0<w<1\) and \(0<\lambda<\infty\). Its limiting sampled density on \(Y\) is \(w\lambda e^{-\lambda y}\) by the beta normalizer asymptotic, since \(a\to\infty\) and \(p,q\to0\). Thus its \(F\) is the attained \(q=1,\ pa=\lambda\) value plus \(\log w<0\). Whenever this path is possible with supported holds, those shapes can all vary, so the comparator is feasible. With two endpoint atoms, the sample lies strictly inside the interval between the atoms. Such mixtures require \(q\to0\). The decreasing \(Y\) density at each \(y_i>0\) is bounded by \(2\Pr(y_i/2\le Y\le y_i)/y_i\), which tends to zero. This gives log likelihood tending to \(-\infty\), even on constants. A nonzero endpoint atom also precludes Hellinger convergence from these absolutely continuous laws. The sample singularity in §3 explains why this domination argument requires a held endpoint above the data.

## Numerical replay

**Numerical.** The workstation replay passed at 150 decimal digits through the one-slot lock. It evaluates beta log density by log gamma differences and survivor probabilities independently by the incomplete beta integral. The plateau and penalty tests use \(y=(1/8,1/4,1/2)\) and powers \(100,400,1200\). The limiting penalty \(3/4\) test has final absolute per-observation error \(0.00693\), and the vanishing penalty test has error \(0.03287\), consistent with the slow \((\log a)/a\) term. The gamma check uses \(q=1/4,1,3\) and \(a=10^{-3},10^{-6},10^{-9}\). The mixed power tests use \(p/q=1/4,1,3\), the same large-power grid and survivor thresholds \(1/8,1/2,5/4\). The largest final mixed error is \(4.56\times10^{-6}\). Two-atom paths use \(t=10,40,100\), \(a=e^{-t}\), and thresholds \(1/4,1,2\). The free-endpoint example uses \(t=10,100,100000\), with final error \(0.000231\) in the log likelihood after removing \(2\log t\). Constant cases include the two collapsing paths on \(t=10,40,100\) and 48 finite-point comparisons with the held-\(q\) envelopes. [Replay script](2026-11-25-genbeta-limits-math-data/check-boundaries.py)

```sh
TMPDIR="$PWD/build/tmp" /tmp/umvl-cpu/run nice -n 10 python3 \
  dev/research/evaluations/2026-11-25-genbeta-limits-math-data/check-boundaries.py
```

## Dossier appendix

Proposed replacement scope for the `limits` list, with firmness **proved** for each row.

| Model or outcome | Condition and detection requirement |
|---|---|
| Unbounded, upper endpoint free | Every positive sample when \(q\) is free or held below one. The strict-interior endpoint witness works at fixed positive \(p,a\). |
| Unbounded, upper endpoint held | All observations equal strictly below \(b\), \(q\) free, and at least one of \(p,a\) free. |
| Log-gamma on \(b/X\) | With \(p,a\) free, \(a\downarrow0\) and \(pa\to q/\mu\), fitting \(q\) or retaining its hold as applicable. On nonconstant data compare the applicable gamma value with the power face and finite profile. On constant data with held \(q>1\) it is the strict boundary supremum. |
| Power with endpoint \(M\) | On nonconstant interior data with \(p,q,a\) free, \(a\to\infty,\ pa\to1/d,\ \log q/a\to m,\ q e^{-am}\to0\). Its score comparison uses closed-endpoint densities at \(M\). |
| Power with endpoint \(b\) | With held \(0<q<1\) and free \(p,a\), \(a\to\infty,\ pa\to1/\mu\). On constants it is the strict boundary supremum. On nonconstant data compare it with held-shape gamma and finite profile. |

Use the exact product ridge in the identification discussion for \(q=1\). Its power law is already attained. If the dossier mentions weak closure, record the endpoint-atom mixtures descriptively, with strict loss on interior held-endpoint samples and the unrestricted singularity as their only maximizing context here. The nonconstant near-constant numerical comparison can remain a diagnostic account. Its particular sample threshold is not the theorem that detects a global face winner.

## What this changes for the fitter

- For nonconstant data and held endpoint, complete face comparison requires only \(G\) and \(W(m)\) with all three shapes free. Use \(G_q\) and \(W(0)\) with held \(0<q<1\), and only \(G_q\) with held \(q>1\). Preserve the held gamma shape throughout fitting and transport. These values plus complete finite-region coverage are sufficient. Mixed laws require no candidate or status.
- For moving power, use the endpoint \(M\), rate \(1/\overline{\log(M/x_i)}\) on the log-distance scale and exact face value \(nW(m)-\sum\log x_i\). A fitted tail or a weak-path validation must account for \(q e^{-am}\). Tracking \(\log q/a\) alone misses a penalty proportional to the multiplicity at \(M\).
- On constant interior data, `Unbounded` is justified exactly for the supported free-concentration masks above. With held \(q\ne1\) and both \(p,a\) free, the strict envelope gives a direct `PowerBoundary` at \(q<1\), or `UnitGammaBoundary` at \(q>1\). With \(q=1\), return the canonical attained product fit and its ridge. The masks with zero or one free shape have finite fits.
- For general inputs, a certified boundary return needs a signed neighborhood bound at any potentially winning face, a comparison against the other face where applicable, and coverage of the remaining finite domain. Finite winners may use unsigned tail exclusion by a strictly better witness. A proven equality with a finite maximizer should retain the attained fit. An unfinished optional certificate keeps the successful existing search result explicitly uncertified. A proved contradiction or failed search uses the relevant stop. The unrestricted free-endpoint route retains the unbounded outcome.

## Open

- A practical signed, uniform held-\(q\) gamma neighborhood for nonconstant samples is still needed to certify a gamma winner over finite powers when there is no strict finite improvement. The unsigned tail converges to \(G_q\) but cannot certify its sign. For all-free gamma, the existing quadratic bound covers a strict negative coefficient, while a cancellation seam outside the excluded strip around \(q=1\) needs higher-order bounds or an independently better witness. The present classification leaves no additional candidate on those seams. [Gamma expansion and remaining cases](2026-11-09-genbeta-allfree-report.md#remaining-certification-limits)
- An explicit sample-only decision rule beyond the constant and ridge cases would require a bound deciding the finite profile against the relevant faces, including exact ties. The stationary-value characterization gives the exact comparison to perform. Uniqueness of a general finite maximizer and reliable enumeration of all equal finite maxima remain separate questions.
