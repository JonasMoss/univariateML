# Beta-binomial with size and both shapes free: closure and finite attainment

TASK-113, 2026-11-03. Source revision: f14d43321b89e5a685eec2889da9fa42679de958, branch codex/bbinom-closure. Research only: the additions are this report and its [small reproducer/data directory](2026-11-03-bbinom-closure-data/README.md). No library, binding, dossier, test, golden, or roadmap changes were made.

## Results

The positive-likelihood escape conjecture in the [numerical map](2026-11-02-bbinom-map-report.md) can be proved. If size tends to infinity and any fixed positive count retains positive probability, a subsequence converges to a **proper negative binomial or Poisson distribution**. There is no defective limit retaining mass at a positive count. Defective limits do exist, but are exactly subprobabilities concentrated at zero.

Consequently, the full two-shape profile at size \(N\), not just a profile with one shape held, converges to the optimized NB/Poisson face. This is a qualitative uniform upper-tail result; it does not supply an implementable numerical cutoff.

For a nonconstant sample with an observation strictly between zero and its maximum, a finite positive-shape MLE exists **if and only if some finite beta-binomial has likelihood at least as large as the best boundary law**. The active boundary is the best free-size binomial for underdispersion, Poisson for equidispersion, and the best NB for overdispersion. Strict improvement is sufficient and avoids equality questions.

At equidispersion there is a useful sufficient condition:

$$
\frac1h\sum_i x_i(x_i-1)(x_i-2)<\bar x^3
\quad\Longrightarrow\quad
\text{a finite positive-shape MLE exists},
$$

provided the sample is not supported only on its two endpoints. It follows from the second-order expansion at Poisson proved below. The threshold is sharp for the sign of the second-order cancelling direction, not a necessary condition for global attainment.

A stronger local/global counterexample than a negative NB slope is available: for \((2,2,3,3,4,7,7)\), Poisson is a **strict local maximum in the joint near-Poisson model**, but the explicit law \(BB(7,2,1)\) beats it. Thus a finite MLE exists despite local stability of the boundary. Its likelihood comparison has an exact rational certificate.

## 1. Model and boundary conventions

Write \(BB(N,a,b)\), where \(N\) is a nonnegative integer and \(a,b>0\). The observations are nonnegative integers, \(h\ge1\), with \(M=\max x_i\ge1\). Set

$$
\mu=\bar x,\qquad
v=h^{-1}\sum_i(x_i-\mu)^2,\qquad
F_j=h^{-1}\sum_i(x_i)_{\underline j}.
$$

Likelihoods are products of probabilities, without an additional multinomial sample-order factor; \(\ell\) denotes their logarithm. A finite MLE here means finite integer \(N\) and finite positive \(a,b\). A binomial or endpoint law at finite \(N\) is a finite-size maximum in a closure, but need not be an MLE in this parameter space.

Two identities drive the argument:

$$
P_N(0)=\prod_{j=0}^{N-1}\left(1+\frac a{b+j}\right)^{-1},
\qquad
\frac{P_N(x+1)}{P_N(x)}
=\frac{(N-x)(a+x)}{(x+1)(b+N-x-1)}.
\tag{1}
$$

In particular,

$$
\frac{P_N(x)}{P_N(0)}
=\frac{N_{\underline x}(a)_x}{x!(b+N-x)_x}.
\tag{2}
$$

The [size investigation](2026-11-01-bbinom-size-report.md), [held-beta report](2026-11-02-bbinom-beta-report.md), and [held-alpha report](2026-11-02-bbinom-alpha-report.md) give earlier tail bounds and implementation results. Their held-parameter conclusions remain distinct from the unrestricted problem considered here.

## 2. Exhaustive closure

### 2.1 Bounded size

A bounded integer sequence of sizes has a constant-size subsequence. At fixed \(N\), put \(p=a/(a+b)\) and \(s=a+b\). The compactified possibilities are:

| Parameter limit | Law |
|---|---|
| \(0<p<1,\ 0<s<\infty\) | Ordinary beta-binomial |
| \(s\to\infty,\ p\to p_0\) | \(Bin(N,p_0)\) |
| \(s\to0,\ p\to p_0\) | \((1-p_0)\delta_0+p_0\delta_N\) |
| \(p\to0\) or \(p\to1\), at any concentration scale | \(\delta_0\) or \(\delta_N\) |

These exhaust all joint shape rates. To see this, use the beta mixing variable \(U\). Its mean is \(p\), its variance is \(p(1-p)/(s+1)\), and
\(E[U(1-U)]=p(1-p)s/(s+1)\). Large concentration concentrates \(U\) at \(p\); small concentration concentrates it on \(\{0,1\}\); a mean tending to an endpoint concentrates it there. The finite binomial kernels are continuous in \(U\).

The parametrization by \(p\in[0,1]\) and compactified \(s\in[0,\infty]\) therefore gives a compact fixed-size closure. Its likelihood maximum is attained, even when only by a face.

### 2.2 Unbounded size: positive mass at a positive count forces a proper limit

**Theorem.** Suppose \(N_k\to\infty\) and \(P_{N_k,a_k,b_k}(m)\ge\epsilon>0\) for some fixed integer \(m\ge1\). There is a subsequence of one of the following two types:

1. \(a_k\to A\in(0,\infty)\), \(b_k/N_k\to D\in(0,\infty)\), and
   \[
   P_k(x)\longrightarrow
   \frac{(A)_x}{x!}
   \left(\frac D{1+D}\right)^A
   \left(\frac1{1+D}\right)^x.
   \tag{3}
   \]
   This is NB with shape \(A\) and mean \(A/D\).
2. \(a_k\to\infty\), \(a_k/(1+b_k/N_k)\to\lambda\in(0,\infty)\), and
   \[
   P_k(x)\longrightarrow e^{-\lambda}\lambda^x/x!.
   \tag{4}
   \]

Both are convergence in total variation as well as weak convergence.

**Proof.** Extract subsequences of the nonnegative scales whenever needed.

First exclude \(a\to0\), uniformly in \(b\). For \(N\ge2m\), (2) gives

$$
P_N(m)\le
\left(\frac N{N-m}\right)^m\frac{(a)_m}{m!}
\le 2^m a,\qquad 0<a\le1.
\tag{5}
$$

The last inequality uses \((a)_m\le a\,m!\). Thus the positive-count assumption bounds \(a\) away from zero. It does not require an observed zero.

Now suppose \(a\to A\in(0,\infty)\). If \(b/N\to\infty\), Markov's inequality and
\(E X=Na/(a+b)\) force \(P_N(m)\to0\). If \(b/N\to0\), use

$$
P_N(0)
\le\exp\left[-a\sum_{j=0}^{N-1}\frac1{a+b+j}\right]
\le\left(\frac{a+b}{a+b+N}\right)^a.
\tag{6}
$$

Here \(\log(1+t)\ge t/(1+t)\), followed by a decreasing-sum integral bound, proves both inequalities. Equation (2) bounds \(P_N(m)/P_N(0)\) uniformly when \(a\) stays in a compact positive interval and \(N\ge2m\). Hence \(P_N(m)\to0\) in this regime too.

It remains that \(b/N\to D\in(0,\infty)\). Expanding the logarithm of the product in (1), with each summand \(O(N^{-1})\), gives

$$
\log P_N(0)\longrightarrow
-A\int_0^1\frac{dt}{D+t}
=A\log\frac D{1+D}.
$$

The finite probability ratios converge to those in (3). Their normalization is that of a proper NB law.

Finally let \(a\to\infty\) and define
\(\lambda_N=Na/(b+N)=a/(1+b/N)\). For each fixed \(j\),

$$
\frac{P_N(j+1)}{P_N(j)}
=\frac{\lambda_N}{j+1}
\left(1-\frac jN\right)\left(1+\frac ja\right)
\frac{b+N}{b+N-j-1}
\sim\frac{\lambda_N}{j+1}.
\tag{7}
$$

If \(\lambda_N\to0\), multiplying the first \(m\) ratios forces \(P_N(m)\to0\). If \(\lambda_N\to\infty\), \(P_N(m)\le1/[P_N(m+1)/P_N(m)]\to0\). Extracting a further subsequence therefore leaves \(\lambda_N\to\lambda\in(0,\infty)\).

Then \(b/N\to\infty\), \(a/b\sim\lambda/N\), and \(j/b\to0\) uniformly over \(0\le j<N\). The product for \(P_N(0)\) yields
\(\sum_{j<N}\log(1+a/(b+j))\to\lambda\). Equation (7) now gives (4). Pointwise convergence of probabilities to a law of total mass one implies total variation convergence on this countable space. This proves the theorem.

### 2.3 Proper and defective closures

It follows that the proper weak closure of the whole family consists exactly of:

- finite-size beta-binomials;
- finite-size binomials, including their point masses;
- endpoint mixtures on \(\{0,N\}\);
- arbitrary proper NB laws and Poisson laws.

The same proper closure is obtained in total variation or Hellinger distance. Every listed law is approachable by the paths above; the subsequence argument excludes any other proper limit.

In the topology of pointwise convergence allowing subprobabilities, the **only additional limits** are

$$
q\delta_0,\qquad 0\le q<1.
\tag{8}
$$

Indeed, any limit with positive mass at a positive integer must be proper by the theorem. Conversely, for \(b=1\) and \(a=t/\log N\), \(P_N(0)\to e^{-t}\) while every fixed positive probability tends to zero. The product in (1) proves this because its log is \(-aH_N+O(a^2)\). Fixed \(a>0,b=1\) gives the zero subprobability.

This explains precisely why defective escape does not matter when \(M\ge1\): it gives zero sample likelihood. It can retain positive likelihood for an all-zero sample, for which \(N=0\) already attains likelihood one.

No moment-tightness assumption was used. Such an assumption would be unsafe: \(a=N^{-1/2},b=1\) gives weak convergence to \(\delta_0\) but \(E X\sim\sqrt N\).

## 3. What closure proves about attainment

Let

$$
Q_N=\sup_{a,b>0}\ell_{BB}(N,a,b),\qquad
F=\sup_{a>0,\ m>0}\ell_{NB}(a,m),
\tag{9}
$$

where \(F\) includes the Poisson limit. The NB mean profiles at \(m=\mu\). The standard NB result is that its finite-shape maximum is unique when \(v>\mu\), whereas its supremum is Poisson when \(v\le\mu\); see [Brøns and Jensen (1988), especially Section 3](https://www.math.ku.dk/bibliotek/arkivet/preprints-fra-ims/1988/preprint_1988_-_no._8_br_ns__hans_and_jensen__s_ren_tolver_-_maximum_likelihood_estimation_in_the---.pdf).

### 3.1 Uniform upper-tail convergence of the full profile

**Corollary.**

$$
\boxed{\lim_{N\to\infty}Q_N=F.}
\tag{10}
$$

For the lower bound, approach each fixed NB law along \(b=Na/\mu\); approach Poisson along \(a=N,\ b=N^2/\mu-N\). Taking suprema gives \(\liminf Q_N\ge F\).

For the upper bound, if \(Q_N\ge F+\eta\) infinitely often, choose finite shapes with likelihood above \(F+\eta/2\). Each observed probability is then bounded below by the positive joint likelihood, since all other factors are at most one. Apply the theorem at \(m=M\). Its proper subsequential NB/Poisson limit has log likelihood at most \(F\), a contradiction.

The uniform statement is: for every \(\eta>0\), all shapes at all sufficiently large sizes have \(\ell\le F+\eta\). It does not say that every shape path approaches the optimized face, or give a computable size as a function of \(\eta\).

### 3.2 Finite size and finite shapes are different questions

If fixed-size shape boundaries are admitted, a global maximum at a finite size exists **if and only if**

$$
\exists N\ge M:\quad Q_N\ge F.
\tag{11}
$$

Each \(Q_N\) is attained in its compact fixed-size closure. If one exceeds \(F\), (10) excludes the integer tail above some finite cutoff, so finitely many compact profiles suffice. If one equals \(F\), either it is already a maximum or another profile exceeds \(F\), reducing to the preceding case. If every \(Q_N<F\), only the infinite-size face gives the supremum.

For attainment with positive finite shapes, define \(B\) as the supremum over all finite-size binomial laws, including their Poisson limit, and \(E_{\mathrm{end}}\) as the best endpoint/point likelihood. Let \(E=\max(F,B,E_{\mathrm{end}})\). Then the exact characterization is

$$
\boxed{\text{A finite positive-shape MLE exists}
\ \Longleftrightarrow\
\exists (N,a,b)\text{ finite with }\ell_{BB}(N,a,b)\ge E.}
\tag{12}
$$

Necessity holds because all faces are approachable by actual beta-binomials. For sufficiency, if the global supremum is \(E\), the displayed candidate attains it. If it exceeds \(E\), a maximizing sequence can approach neither an infinite-size limit nor any fixed-size shape boundary. A subsequence stays at a finite size with shapes in a compact positive interval and attains the supremum.

Equality in (11) or (12) is an exact mathematical comparison. A small numerical difference is not an equality certificate.

For nonconstant samples having some \(0<x_i<M\), \(E_{\mathrm{end}}=-\infty\). The boundary threshold simplifies:

| Sample regime | Boundary threshold \(E\) in (12) |
|---|---|
| \(v<\mu\) | Best free-size binomial, at a finite integer size |
| \(v=\mu\) | \(Pois(\mu)\) |
| \(v>\mu\) | Best finite-shape \(NB(\widehat a,\mu)\) |

The binomial ingredient is the positive-mean existence result in [DeRiggi (1983)](https://www.tandfonline.com/doi/abs/10.1080/01621459.1983.10477949): its finite-size maximum exists for \(v<\mu\), while the other cases have Poisson supremum. The [binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md) records the variance convention and integer-search interpretation. These two classical face results, combined with the closure theorem, prove the table; the table alone does not decide whether the BB interior beats the threshold.

Exceptional samples are explicit:

- All zeros attain likelihood one at \(N=0\).
- Constant positive data have only the point-mass supremum; finite positive shapes cannot attain it.
- Nonconstant data on \(\{0,1\}\) attain their empirical Bernoulli law at \(N=1\), with concentration unidentified.
- Nonconstant data on \(\{0,M\}\), \(M\ge2\), have the empirical endpoint-mixture supremum at size \(M\). It is not attained by positive shapes, since a finite beta-binomial at \(N\ge M\) assigns positive mass to unobserved counts.

### 3.3 First-order sufficient conditions at the active face

The [map report](2026-11-02-bbinom-map-report.md) derived these two slopes. The closure theorem upgrades strict improvement above the active face to an existence result for nonendpoint samples.

For \(v<\mu\), let \(N_B\) maximize the binomial face and \(p=\mu/N_B\). The derivative toward positive intraclass correlation \(\rho=1/(a+b+1)\) is

$$
T_{\mathrm{Bin}}
=\frac{h[v-\mu(1-\mu/N_B)]}{2p(1-p)}.
\tag{13}
$$

If \(T_{\mathrm{Bin}}>0\), a finite positive-shape MLE exists.

For \(v>\mu\), the derivative from the optimized NB face in \(1/N\) is

$$
T_{\mathrm{NB}}
=\frac{h[\mu^2-\widehat a(v-\mu)]}{2(\widehat a+\mu)}.
\tag{14}
$$

If \(T_{\mathrm{NB}}>0\), a finite positive-shape MLE exists. Equivalently, the NB ML shape is smaller than its moment shape \(\mu^2/(v-\mu)\). A negative slope is only a local obstruction; Section 5 gives an exact counterexample to its global sufficiency.

## 4. The joint Poisson expansion and the NB seam

### 4.1 A second-order expansion valid at all relative rates

Hold the mean at \(\mu>0\), set

$$
u=1/a,\qquad k=1/N,\qquad b=a(N/\mu-1).
$$

For fixed observations and \(u,k\to0\) through positive admissible values,

$$
\frac{\ell_{BB}-\ell_{\mathrm{Pois}}}h
=\frac{v-\mu}{2}(u-k)
+B(u^2+k^2)+\frac{\mu v}{2}uk
+O((u+k)^3),
\qquad
B=\frac{\mu^3-F_3}{6}-\frac{F_2}{4}.
\tag{15}
$$

The remainder is uniform in the ratio \(u/k\); its constant depends on the fixed sample and mean. This is more than expanding at fixed \(a\) and then letting \(a\) increase.

Here is a direct derivation. In these coordinates, (1) becomes

$$
\frac{P(x+1)}{P(x)}
=\frac{\mu}{x+1}
\frac{(1-xk)(1+xu)}
 {1+\mu(u-k)-\mu(x+1)uk}.
\tag{16}
$$

The log of the factor after \(\mu/(x+1)\) has linear term \((x-\mu)(u-k)\) and quadratic term

$$
-\frac{x^2}{2}(u^2+k^2)
+\frac{\mu^2}{2}(u-k)^2+\mu(x+1)uk.
$$

The zero probability has the exact representation

$$
\log P(0)=
\sum_{j=0}^{N-1}
\log\left(1-\frac{\mu k}{1+\mu u k j}\right).
$$

Expanding uniformly over \(0\le kj\le1\) and summing powers of \(j\) gives

$$
\log P(0)=-\mu+\frac{\mu^2}{2}(u-k)
-\frac{\mu^3}{3}(u^2+k^2)
+\frac{\mu^3-\mu^2}{2}uk
+O((u+k)^3).
\tag{17}
$$

For sufficiently small \(\mu(u+k)\), the Taylor remainders are bounded uniformly term by term; summing \(N\) terms cancels their leading factor \(k\). Adding the finitely many ratio terms and averaging proves (15). The reproducer checks the polynomial algebra independently with SymPy.

The per-observation first-order coefficient is

$$
\frac{x(x-1)-2\mu x+\mu^2}{2}(u-k).
$$

Thus the two shape/size directions have opposite, collinear first-order scores at Poisson. This is a tangent degeneracy, not a likelihood classification. Local reoptimization of the mean leaves (15) unchanged through second order: at the sample mean, the derivative in the mean of the summed first-order coefficient vanishes.

### 4.2 Equidispersion: a sharp local threshold, and a global sufficient condition

If \(v=\mu\), then \(F_2=\mu^2\), and the quadratic term becomes

$$
B(u-k)^2+Guk,\qquad
G=\frac{\mu^3-F_3}{3}.
\tag{18}
$$

Always \(B<0\) in this case. Indeed, Cauchy–Schwarz gives
\(E_nX^3\ge(E_nX^2)^2/E_nX\), whence
\(F_3\ge\mu^3-\mu^2\) and \(B\le-\mu^2/12<0\).

Consequently:

- \(F_3<\mu^3\): along \(u=k\), the likelihood increases above Poisson. For a nonendpoint sample, (12) proves a finite MLE exists.
- \(F_3>\mu^3\): the quadratic form is strictly negative throughout the positive quadrant away from the origin. Poisson is a strict local maximum near this corner, including after local mean optimization. It can still lose globally.
- \(F_3=\mu^3\): the quadratic term vanishes on \(u=k\). Higher-order terms or remote comparisons are necessary.

In particular, separate analysis of the NB and binomial axes misses the mixed term. Both axes lose likelihood at equidispersion, even when the cancelling direction gains.

On the exact diagonal \(a=N\),

$$
\frac{P(x+1)}{P(x)}
=\frac{\mu}{x+1}\frac{1-x^2/N^2}{1-\mu(x+1)/N^2},
\qquad
P(0)=
\frac{\Gamma(N^2/\mu)^2}
{\Gamma(N^2/\mu-N)\Gamma(N^2/\mu+N)}.
\tag{19}
$$

Thus the expansion is even in \(1/N\), and at equidispersion
\(N^2(\ell_{BB}-\ell_{\mathrm{Pois}})\to hG\). The coefficient need not be nonzero. For \((1,1,4)\), \(G=0\); expanding (19) one order further gives

$$
\frac{\ell_{BB}(N,N,N(N/2-1))-\ell_{\mathrm{Pois}}(2)}3
=-\frac{37}{15N^4}+O(N^{-6}).
\tag{20}
$$

The script checks this coefficient as well as the second-order coefficients.

There is also an exact equidispersed finite-model curve. At fixed mean,

$$
\operatorname{Var}(X)-\mu
=\frac{\mu^2(N-a-\mu-1)}{aN+\mu}.
\tag{21}
$$

Taking \(a=N-\mu-1>0\) makes the model variance exactly \(\mu\). Its leading joint-Poisson coefficient is still \(G/N^2\). The finite model therefore need not differ from Poisson in its first two moments to improve the likelihood.

### 4.3 Why NB is a seam, but BNB is not an extra BB limit

The exact ratio to zero can also be written

$$
\frac{P(x)}{P(0)}
=\frac{(-N)_x(a)_x}{(1-b-N)_x\,x!}.
$$

Substitution \(N=-r,\ b=1-\alpha-a\) gives the proper BNB ratio

$$
\frac{(r)_x(a)_x}{(r+\alpha+a)_x\,x!},
$$

when \(r,\alpha,a>0\). In the mean-fixed coordinates above, negative \(k\) corresponds to

$$
r=-1/k,\qquad \alpha=1+ar/\mu.
\tag{22}
$$

This BNB has mean exactly \(\mu\) and approaches \(NB(a,\mu)\) as \(r\to\infty\). The BB and BNB expansions meet there from opposite signs of \(k\). At the NB fitted mean, the first derivative is (14); the derivative on the BNB side reverses sign when expressed in \(1/r\). The reproducer checks both sides.

The earlier map used \(b=Na/\mu\), whose finite-size mean differs slightly from \(\mu\). Replacing it by the mean-fixed path subtracts \(a\) from \(b\); its first-order contribution to the summed likelihood is a multiple of the NB mean score, which is zero at \(\mu=\bar x\). Thus the fitted-face derivative is the same in the two coordinates.

Similarly, the ratio
\(\mu(1-\kappa x)/[(x+1)(1-\kappa\mu)]\) describes binomial for positive reciprocal integer \(\kappa\), Poisson at zero, and NB of shape \(-1/\kappa\) for negative \(\kappa\).

These statements concern analytic extensions of probability ratios and their asymptotic germs. Positive BB sizes are still integers. Substituting a negative size is not an admissible BB parameter path. A proper finite-parameter BNB has a power tail and is neither NB nor Poisson; the closure theorem excludes it as a BB limit. The seam explains local geometry, not an additional escape family.

## 5. Explicit witnesses and failed conjectures

No numerical optimizer supplies an estimate in this study. Except for one explicitly labelled NB root refinement, all displayed parameters are fixed feasible witnesses. The exact certificates use rational arithmetic, not floating-point sign decisions.

### Dispersion does not decide attainment

For \((0,1,2)\), \(BB(2,1,1)\) is uniform on the three observed values and attains the saturated bound \(L=1/27\). It is a proved global MLE, despite \(v=2/3<\mu=1\).

By contrast, for \((1,2)\), the **global supremum is the binomial face**
\(Bin(2,3/4)\), with product \(27/128\), and no finite positive shapes attain it. Here is a proof covering every size, even arbitrary binomial mixtures:

At \(N=2\), let \(q=E U^2\). Then \(P_2=q\) and
\(P_1=2(E U-q)\le2(\sqrt q-q)\). Maximizing their product gives \(27/128\), with equality only when \(U=3/4\) almost surely. A finite beta mixing law is nondegenerate, so equality is impossible.

For \(N\ge3\), the largest binomial probabilities at counts one and two are each at most \(4/9\). For fixed positive \(x\), the optimized point probability decreases with \(N>x\): its consecutive ratio is

$$
\frac{(1+1/(N-x))^{N-x}}{(1+1/N)^N}<1,
$$

because \(t\log(1+1/t)\) increases. Mixtures obey the same pointwise bounds. Their likelihood product is at most \(16/81<27/128\). This excludes every larger size. Both examples are underdispersed; their attainment differs.

### Both separate axes lose, but a joint direction gains

For

$$
x=(0,0,0,1,1,1,2,3),
\qquad \mu=v=1,\qquad F_3=3/4,
$$

equation (18) gives \(G=1/12>0\), while \(B=-5/24<0\). Hence a finite MLE exists. Explicit witnesses beating Poisson are \(BB(10,10,90)\) and \(BB(10,8,72)\). The latter has **exactly the sample mean and variance**, both one. The script proves both likelihood inequalities by exact rational comparisons with a Taylor lower bound for \(e^8\).

### Even a strict local Poisson maximum can lose globally

For

$$
x=(2,2,3,3,4,7,7),\qquad
\mu=v=4,\quad F_3=456/7,
$$

we have \(B=-88/21\) and \(G=-8/21\). Poisson is therefore a strict local maximum in the joint near-Poisson model.

Nevertheless \(BB(7,2,1)\) has \(P(x)=(x+1)/36\), and

$$
\ell_{BB}(7,2,1)-\ell_{\mathrm{Pois}}(4)
=0.03544946687484035326\ldots>0.
$$

The inequality is exact-certifiable: multiply by the factorial denominators and compare the resulting rational threshold with
\(\sum_{j=0}^{80}28^j/j!<e^{28}\). The rational comparison is strict. All binomial and NB faces are bounded by Poisson for this equidispersed sample, and no endpoint face has positive likelihood. A finite positive-shape MLE follows from (12). The witness parameters are not asserted to be that MLE.

For the critical case \((1,1,4)\), \(\mu=v=2\) and \(F_3=8\), so \(G=0\); (20) loses along the diagonal. Still, the uniform \(BB(4,1,1)\) beats Poisson by \(0.19085700968597263934\ldots\). A rational Taylor comparison with \(e^6\) proves this too. The global decision cannot be inferred from the cancelling diagonal alone.

### A negative NB seam slope does not establish nonexistence

For \((1,1,5)\),
\(\mu=7/3\), \(v=32/9\). The NB shape is numerically

$$
\widehat a=4.51202785824429370716\ldots,\qquad
F=-5.70476783345171442402\ldots,
$$

and \(T_{\mathrm{NB}}=-0.01539501040659003489\ldots<0\). Yet the uniform \(BB(5,1,1)\) has
\(\ell=-3\log6=-5.37527840768416500244\ldots>F\). Thus a finite MLE exists.

This counterexample, already identified numerically in the map, now has a separate exact certificate. Rational atanh-series bounds for logarithms prove opposite NB score signs at \(a=4.51\) and \(4.52\). The known uniqueness theorem brackets the maximizing shape. Bounding the score magnitude over that interval bounds the face likelihood above; the uniform BB likelihood exceeds that upper bound by more than \(0.3044\). The whole bracket lies above the moment shape \(49/11\), proving the seam slope is negative. The tighter 80-digit root bracket is reported separately as numerical evidence.

These counterexamples disprove classifiers based on dispersion sign, first-order face slopes, or even strict local stability at Poisson. They do not prove that no other simple global characterization can exist.

## 6. Reproduction, validation, and remaining open questions

### Reproducer and evidence

The [script](2026-11-03-bbinom-closure-data/check.py) writes [78 check rows](2026-11-03-bbinom-closure-data/checks.csv) and a [summary with exact-certificate outcomes](2026-11-03-bbinom-closure-data/summary.json). It uses mpmath at 80 decimal digits, increasing to 180 for the defective path at \(N=10^{60}\), and SymPy for algebra. All assertions passed.

Workloads include NB and Poisson paths over fixed counts zero through six, sizes through \(10^6\); defective paths through \(10^{60}\); joint expansions for four samples at \(N=100,1000,10000\) and \(u/k=1/4,1,4\); diagonal second- and fourth-order limits; and two-sided NB seam slopes. The asserted coefficient tolerances are \(0.003\) for the joint expansion, \(10^{-6}\) for the diagonal limits, and \(0.005\) for the seam. They check convergence at those points, not error bounds for all parameters.

The strict-local/remote example was discovered deterministically among sorted multisets with entries in zero through seven, increasing sample size, then lexicographically. Five equidispersed, strictly local-Poisson candidates were examined before finding the displayed length-seven witness, using 36 fixed shape pairs at \(N=M\). The enumeration limit is only a discovery budget; no capped optimum is reported. The final witness is independently certified with exact rational arithmetic.

The finite witnesses use degree-80 Taylor lower bounds for exponentials. The exact NB certificate uses 40 terms of
\(\log q=2\operatorname{atanh}((q-1)/(q+1))\), after rational range reduction, with a geometric bound on the remaining positive terms. These certificate assertions are exact integer/rational computations. They are stronger than the separate mpmath checks. Neither substitutes for the closure proof or a global numerical fitter.

All computation ran on the **local workstation** through the supplied slot locks, with nice priority and one BLAS/OpenMP thread per worker. Environment: Linux 7.0.0-38-generic x86-64, GCC 15.2.0, R 4.6.1, BH 1.90.0-1 / Boost 1.90, Python 3.14.5, mpmath 1.3.0, SymPy 1.14.0. No simbox, dependency installation, sub-agent, board edit, commit, or publishing was used.

Commands, from the worktree root:

~~~sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-bbinom-closure-data/check.py
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test
/tmp/umvl-cpu/run4 just test-full
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-bbinom-closure-data/render.py
~~~

The reproducer was rerun after its final changes. Final suite results, all on the source revision named above:

| Run | Result |
|---|---|
| C++ dev | 106/106 passed |
| C++ full | 127/127 passed |
| R dev | 20,507 passes; 0 failures, 0 warnings, 153 skips |
| R full | 43,324 passes; 0 failures, 0 warnings, 0 skips |

The full R tier executed the coverage skipped in dev. These are regression checks of the unchanged implementation, not tests of a newly enabled free-size fitter. Logs are scratch files under the worktree's build directory; the lasting mathematical checks and their results are retained beside this report.

The report/data local links are checked separately by the [preview renderer](2026-11-03-bbinom-closure-data/render.py): all 14 passed. The [HTML preview](2026-11-03-bbinom-closure-data/preview.html) contains 280 MathML elements, no scripts, and no conversion warnings. The repository-wide link checker found four pre-existing issues outside this work: the absent desktop.png and mobile-evidence.png historical images linked from the 2026-10-31 inference report, and two /inference/ website routes interpreted as filesystem paths in the C++ inference note. Those tracked sources are unchanged from the tested revision; this research task does not repair their historical assets or route checking.

Artifact-pane delivery is blocked by the viewer configuration: the prescribed show command returned HTTP 403, stating that this worktree is outside every readable root. The finished preview remains in the allowed report/data directory. Displaying it in that pane requires the viewer to register this worktree; no external configuration or other worktree was changed to bypass that boundary.

### Open: computable unrestricted tail cutoff

Equation (10) proves a uniform upper-tail limit, but its compactness argument does not calculate the first excluded size. An implementation still needs a computable upper bound valid over all \(a,b\), tied to a likelihood gap and propagated fixed-size allowances. Equation (5) now excludes small \(a\) uniformly; (6) controls small \(b/N\) on compact shape intervals; (15) controls the joint large-shape corner locally. Combining these into an economical global numerical certificate, with explicit constants and practical floating-point margins, remains open.

The held-alpha correction proportional to \(a^2/N\) cannot simply be optimized over unrestricted \(a\); the [held-alpha report](2026-11-02-bbinom-alpha-report.md) already explains that obstruction. It is now a quantitative-bound problem, rather than an unidentified-limit-family problem.

### Open: deciding genuine infinite-size winners and equality

No global certificate is supplied for a nonendpoint sample whose only maximizing size is infinity. The map's NB candidates remain candidates until every finite profile is excluded. The necessary-and-sufficient comparison (12) and the sufficient conditions (13), (14), and (18) do not turn a negative local slope into that exclusion. Exact ties with a boundary threshold also require a separate argument.

### Open: uniqueness and production inference

Neither the closure theorem nor these witnesses prove uniqueness of unrestricted finite MLEs. No estimator asymptotics, calibration, or covariance procedure is established here. Enabling the unsupported fitter would additionally require certified shape/size comparisons and explicit failure outcomes; research-only authorization does not enable it. No change to the settled d/p/q/r or truth machinery is indicated by this study.
