# Showcase proof audit — TASK-165

The six requested results withstand the checks below, with explicit hypotheses and certificate qualifications. The PIG showcase statement needs the positive-mean hypothesis. The beta-prime continuity argument needs a common compact competitive domain; that argument is supplied here, with independent endpoint covers. The accessible Aragón–Eberly–Eberly preprint's Case 3 cancellation is invalid. The complete NB convexity proof establishes the theorem independently of that inference.

All required final library suites and both independent 120-digit checks finished green. Work is confined to this report and its [evidence directory](2026-11-09-proof-audit-data/check.py). Production code, dossiers, fixtures, defaults, bindings and generated density sources were not edited. No commits, merges, rebases, board edits, dependency installations or sub-agents were used.

Audited revision: e638713c52ee730ef0ba2ce483fd79d1053c3900, branch codex/proof-audit. The [provenance manifest](2026-11-09-proof-audit-data/provenance.json) identifies the exact source files, scripts, environment and archived logs. Later coverage notes in the current beta-prime and beta-binomial dossiers supersede the older reports' former coverage limitations; those historical limitations are not asserted as current findings.

## Reading the verdicts

“Verified” means the displayed mathematical inference has been re-derived. “Verified with a correction” identifies a hypothesis, wording change or additional justification needed for the stated inference. “Gap” identifies an inference that does not follow, together with its missing requirement or replacement.

Universal inequalities below are exact-arithmetic statements. The concrete beta-prime endpoint covers are certificates in exact arithmetic conditional on the computed comparisons, with a stated practical floating-point allowance. Finite numerical tests supplement these arguments. They do not establish universal inequalities by sampling.

| Result | Verdict |
|---|---|
| PIG existence and uniqueness | Verified for positive-mean integer samples; correct the unrestricted showcase shorthand |
| Fréchet, shape held | Verified, including the finite unattained supremum at equality |
| Ordinary NB | Verified through strict convexity of the negative profile in the transformed coordinate |
| AEE preprint Case 3 | Gap confirmed; replace its use by the complete NB proof |
| SGED constant-sample two-maxima example | Verified for the exactly defined skew, with exactly two global scale maxima |
| Beta-prime held-shape tie | Verified conditional on the retained endpoint comparisons; uniform compactness supplied |
| Beta-prime joint exterior and lognormal corner | Verified, including the signed joint gamma-tail sharpening |
| Beta-binomial signed face bounds | Verified; global face selection additionally requires the outside cover and tail exclusion |

The source proof sections are [PIG](2026-11-06-pig-cert-report.md), its [expanded concavity proof](2026-11-06-pig-cert-data/concavity-proof.md), [Fréchet](2026-11-07-evt-scalar-report.md), [NB](2026-11-07-nb-proof-report.md), [SGED](2026-11-07-sged-subunit-report.md), [conditional beta-prime](2026-11-07-betapr-prof-report.md), [joint beta-prime](2026-11-08-betapr-joint-report.md) and [beta-binomial](2026-11-07-bbinom-face-report.md). Each derivation below follows the mathematical steps of those sections and their linked dossiers.

## Common special-function inequalities

**Verified.** Differentiating the gamma product gives

$$
\psi_1(z)=\sum_{j=0}^{\infty}(z+j)^{-2},\qquad z>0.
$$

The first summand proves $\psi_1(z)>z^{-2}$. Integral comparison for this decreasing function gives $\psi_1(z)<z^{-1}+z^{-2}$. Strict convexity and the trapezoidal comparison give the sharper lower bound

$$
\psi_1(z)>\frac1z+\frac1{2z^2}.
$$

Integrating $\psi_1(t)-1/t$ towards infinity, using $\psi(t)-\log t\to0$, gives $\psi(z)<\log z-1/(2z)$. In particular $\psi(z)<\log z$ and $\psi_1(z)>1/z$.

For the Stirling remainder, Binet's formula uses

$$
q(t)=\frac12\coth(t/2)-\frac1t
 =2t\sum_{j=1}^{\infty}\frac1{t^2+4\pi^2j^2},
\qquad 0<q(t)<t/12.
$$

Consequently $w(z)=\int_0^\infty e^{-zt}q(t)\,dt/t$ satisfies $0<w(z)<1/(12z)$. These bounds supply the gamma increments, curvature and mode-height comparisons used below. The checker tests the kernel inequalities and the relevant digamma/trigamma bounds separately, including small and large arguments.

## 1. PIG: existence, uniqueness and the zero-law exception

Sources: the [PIG dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md), expanded concavity proof and [original fixed-z reduction](2026-10-29-pig/analysis.md).

**Precise theorem.** Let $x_1,\ldots,x_n$ be a finite nonempty sample of nonnegative integers, with $m=\bar x>0$ and $v=n^{-1}\sum_i(x_i-m)^2$. Over finite $\mu,\lambda>0$, the joint PIG likelihood has a finite maximizer exactly when $v>m$. That maximizer is unique and has $\hat\mu=m$. If $v\le m$, its supremum is the unattained Poisson likelihood with mean $m$. With $\mu>0$ held and at least one positive count, the corresponding condition is

$$
v+(m-\mu)^2>m.
$$

At equality the held-mean supremum is Poisson with that held mean.

**P1 — Verified: finite-polynomial identities.** For a count $k\ge2$, put $c=k(k-1)$ and

$$
Q_{k-1}(y)=\sum_{j=0}^{k-1}a_jy^j,\qquad
a_j=\frac{(k-1+j)!}{j!(k-1-j)!\,2^j}.
$$

The coefficient identity $2(j+1)a_{j+1}=\{c-j(j+1)\}a_j$ proves
$y^2Q''+2(y+1)Q'-cQ=0$, including the top-degree coefficient. For the probability weights proportional to $a_jz^{-j}$, write $E=E(J)$ and $V=\operatorname{Var}(J)$. Differentiation and the ODE give

$$
\frac{dE}{d\log z}=-V,\qquad V=c-(2z+1)E-E^2.
$$

**P2 — Verified: the strict variance barrier.** Let
$B(E)=E(k^2-E^2)/(k^2+E^2)$ and $f(z)=V-B(E)$. At any hypothetical zero of $f$, substitute $V=B(E)$ and
$2z=\{c-E-E^2-B(E)\}/E$ into its derivative. Simplification gives

$$
zf'(z)=
\frac{2Ek(E+k)^2(E^3+E^2k-Ek^2+k^3)}
     {(E^2+k^2)^3}>0.
$$

Here $0<E<k-1<k$, and the last cubic is
$E^3+E^2k+k^2(k-E)>0$. The finite polynomial expansion at infinity gives

$$
E=\frac c{2z}-\frac c{2z^2}+O(z^{-3}),\quad
V=\frac c{2z}-\frac c{z^2}+O(z^{-3}),\quad
f(z)=-\frac c{2z^2}+O(z^{-3})<0
$$

eventually. Every zero would cross upwards with positive derivative. Any nonnegative value would therefore force a later downward crossing before this negative tail, which is impossible. Thus $V<B(E)$ everywhere. No assumption about the sign near zero is required.

**P3 — Verified: strict profile concavity.** At held $\mu$, use

$$
w=\frac{\sqrt{1+2\mu^2/\lambda}-1}
        {\sqrt{1+2\mu^2/\lambda}+1},\qquad
z=\frac{\mu(1-w^2)}{2w},\qquad 0<w<1.
$$

For $k>0$,

$$
\log p_k=-\mu(1-w)
 +k\log\frac{\mu(1-w)}{1+w}
 +\log Q_{k-1}(1/z)-\log(k!).
$$

Differentiation yields

$$
(1-w^2)^2(\log p_k)''
 =-4kw+2E(3+w^2)+(V-E)\frac{(1+w^2)^2}{w^2}.
$$

Since the coefficient of $V$ is positive, substituting the strict barrier gives a strict upper bound. With $r=E/k$, that bound divided by $k$ factors as

$$
\frac{2(r-w)^2(rw^2-r-2w)}{w^2(1+r^2)}\le0.
$$

The last factor is $-r(1-w^2)-2w<0$. Strictness survives $r=w$, because the preceding substitution was strict. For $k=1$ the scaled curvature is $-4w<0$; for $k=0$, $\log p_0=-\mu(1-w)$ is linear. Hence any sample with a positive count has a strictly concave likelihood in $w$.

**P4 — Verified: boundary signs and the criterion.** At $w=0$ the score is

$$
n\mu-2\sum_i x_i+\frac{\sum_i x_i(x_i-1)}{\mu}
=\frac n\mu\{v+(m-\mu)^2-m\}.
$$

Each positive-count log mass behaves as $\log(1-w)+O(1)$ at $w=1$; its score tends to minus infinity. Strict concavity therefore gives one interior maximum exactly when the displayed boundary score is positive. A nonpositive score gives strict decrease for every positive $w$, including the equality case. At $w=0$ the mass is Poisson.

**P5 — Verified: global joint reduction.** At fixed $z$, set $t=\lambda/\mu$ and $\lambda=(z^2-t^2)/2$, with $0<t<z$. The entire $t$-dependent log likelihood is $nt+S\log\lambda$, where $S=\sum x_i>0$. Its derivative is $n-St/\lambda$, and its second derivative is
$-S(\lambda+t^2)/\lambda^2<0$. The unique maximum has $\lambda=mt$, hence $\mu=m$. Every finite joint point is dominated by this fixed-z reduction; this covers joint escape sequences as well as stationary points. Its inward derivative at $t=0$ is $n>0$, excluding a winning infinite-mean Lévy face.

**P6 — Verified with a correction to the showcase shorthand.** The theorem needs $m>0$. In the package's declared extended domain, all-zero data attain likelihood one at $\lambda=0$; a free $\mu$ is unidentified. Thus an unrestricted claim that “otherwise the Poisson supremum is unattained” would be false. The dossier already separates this case. No change to the PIG proof or fitter is needed.

## 2. Fréchet with shape held

Source: the [Fréchet dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) and scalar-certificate report.

**Precise theorem.** Hold $\lambda>0$, and allow $\mu<\min x_i$ and $\sigma>0$ to vary for any finite nonempty real sample. Let $k$ be the original minimum's multiplicity. A finite joint MLE exists exactly when
$A=\lambda(n-k)-k>0$, and is then unique. At $A=0$ the supremum is finite and unattained. At $A<0$ it is infinite. Constant samples have $k=n$, and belong to the last case.

**F1 — Verified: profiling scale.** Put $\epsilon=\min x_i-\mu$, $g_i=x_i-\min x_i$, and $S_p=\sum_i(g_i+\epsilon)^{-p}$. The log likelihood is

$$
\ell=n\log\lambda+n\lambda\log\sigma
 -(\lambda+1)\sum_i\log(g_i+\epsilon)-\sigma^\lambda S_\lambda.
$$

Its unique conditional scale maximum has $\hat\sigma^\lambda=n/S_\lambda$. The profile is

$$
H(\epsilon)=n\log\lambda+n\log(n/S_\lambda)
 -(\lambda+1)\sum_i\log(g_i+\epsilon)-n,
$$

and

$$
H'(\epsilon)=S_1\{n\lambda R-(\lambda+1)\},\qquad
R=\frac{S_{\lambda+1}}{S_\lambda S_1}.
$$

**F2 — Verified: strict single crossing.** Write $q_i=(g_i+\epsilon)^{-1}$ and $E_p=S_{p+1}/S_p$. Under weights proportional to $q_i^p$,

$$
\frac{\partial E_p}{\partial p}
=\sum_{i<j}w_iw_j(q_i-q_j)(\log q_i-\log q_j)>0
$$

for nonconstant data. Consequently

$$
\frac{d\log R}{d\epsilon}
=\lambda(E_\lambda-E_{\lambda+1})+(E_1-E_{\lambda+1})<0.
$$

Both differences are strict because $\lambda+1$ exceeds both indices. Also $R(0+)=1/k$ and $R(\infty)=1/n$. The profile-score bracket therefore starts with sign $A$ and ends at $-1$. There is exactly one crossing when $A>0$, with the correct maximum orientation.

**F3 — Verified: both tails and equality.** Expansion at zero gives
$H(\epsilon)=A\log\epsilon+C+o(1)$; at infinity it gives
$H(\epsilon)=-n\log\epsilon+O(1)$. Thus $A>0$ has a global finite maximum and $A<0$ has an unbounded collapse. When $A=0$, strict decrease of $R$ makes $H'<0$ at every finite positive gap, while

$$
\sup H=n\log(n\lambda)-n-n\log k
 -(\lambda+1)\sum_{g_i>0}\log g_i.
$$

This proves finite nonattainment, rather than a finite boundary tie. For $\lambda=1$, $x=(0,0,1,2)$, the value is $2\log2-4$.

**F4 — Verified: constants and the adjacent held-scale argument.** For constants, $R=1/n$ and $H'=-n/\epsilon$, giving unbounded collapse. If scale is also held, the score instead uses $S_{\lambda+1}/S_1$. Its logarithmic derivative is
$-(\lambda+1)E_{\lambda+1}+E_1<0$, including constants; its limits are infinity and zero. This verifies the source's separate unique-location result without assuming joint concavity.

## 3. NB: convexity repair and the preprint gap

Source: the [NB dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md) and NB-proof report.

**Precise theorem.** For a finite nonempty nonnegative-integer sample with $m>0$, ordinary NB has a unique finite joint MLE exactly when $v>m$. Otherwise its profile increases strictly to the unattained Poisson supremum, including $v=m$. The package's all-zero boundary maxima are treated separately.

**N1 — Verified, with the sign convention made explicit.** At fixed $r>0$, the unique probability maximum is $p(r)=r/(r+m)$. Let $f=-\ell_{\rm prof}$ be the total negative profile log likelihood. Then

$$
f'=n\log(1+m/r)-\sum_i\sum_{j=0}^{x_i-1}(r+j)^{-1},
\quad
f''=-\frac{nm}{r(r+m)}
 +\sum_i\sum_{j=0}^{x_i-1}(r+j)^{-2}.
$$

The convex function below is $f$. The log likelihood has the opposite curvature.

**N2 — Verified: the new coordinate.** For

$$
b=\frac r m\log(1+m/r),\qquad
b'=\frac{\log(1+m/r)-m/(r+m)}m>0,\qquad
b''=-\frac m{r(r+m)^2}<0,
$$

the range is $(0,1)$. Positivity follows from $\log(1+t)>t/(1+t)$.

**N3 — Verified: every finite-sum convexity term.** Define $T_0=0$ and
$T_q=r(r+m)^2\sum_{j=0}^{q-1}(r+j)^{-2}$ for integer $q\ge1$. Put $d=\min(q-1,\lfloor m\rfloor)$. The telescoping identity

$$
r\sum_{j=0}^{d}(r+j)^{-2}
=\frac{d+1}{r+d}
 +\sum_{j=1}^{d}\frac{j}{(r+j)^2(r+j-1)}
$$

follows by subtracting the expressions for consecutive $d$. Multiplication by $(r+m)^2$ decomposes $T_q$ into

$$
\frac{(d+1)(r+m)^2}{r+d}
 +\sum_{j=1}^{d}\frac{j(r+m)^2}{(r+j)^2(r+j-1)}
 +\sum_{j=d+1}^{q-1}\frac{r(r+m)^2}{(r+j)^2}.
$$

The first factor is convex because
$(r+m)^2/(r+d)=r+2m-d+(m-d)^2/(r+d)$.
Each middle factor has positive log curvature

$$
-\frac2{(r+m)^2}+\frac2{(r+j)^2}+\frac1{(r+j-1)^2}>0,
$$

since $j\le m$. Each final factor has second derivative

$$
\frac{2(m-j)(m-3j)r+4mj(j-m)}{(r+j)^4}>0,
$$

since $j>m$. Strictness holds in every case: if $d=0$, the first term has positive $m^2/r$ curvature; if $d\ge1$, a middle term is strict. This covers $m<1$, integer $m$, $q=1$ and empty sums.

**N4 — Verified: strict concavity and its finite limit.** Consequently

$$
h(r)=\frac{f''}{b''}=n(r+m)-\frac1m\sum_iT_{x_i}(r)
$$

is strictly concave. Expanding
$T_q=qr+2mq-q(q-1)+O(r^{-1})$ gives
$h(\infty)=n(v-m)/m$. A strictly concave differentiable function with a finite limit at infinity must have $h'>0$: a nonpositive derivative would become strictly negative later and force a limit of minus infinity.

**N5 — Verified: weighted-tail averaging supplies the missing monotonicity.** Since $f'(\infty)=b'(\infty)=0$,

$$
G(r)=\frac{f'}{b'}
=\frac{\int_r^\infty h(s)(-b''(s))\,ds}
       {\int_r^\infty(-b''(s))\,ds}.
$$

The integrals converge because $b''=O(r^{-3})$ and $h$ is bounded on each tail. Thus $h(r)<G(r)<h(\infty)$ and
$G'=(b''/b')(h-G)>0$. This proves strict convexity of $f$ in $b$, without assuming that the raw size score is monotone.

**N6 — Verified: endpoints and globality.** If $a$ is the number of positive observations, $f'\sim-a/r$ at zero, while $b'\sim\log(m/r)/m$; hence $G(0+)=-\infty$. The tail-average squeeze gives $G(\infty)=n(v-m)/m$. There is exactly one zero for $v>m$, with $f'$ changing from negative to positive. For $v\le m$, $G<0$ at every finite $r$, including equality. Conditional probability profiling dominates every other probability, so this is a joint global result. Along the profile the NB masses converge to Poisson masses. All-zero samples instead attain their nonunique zero-law maxima on the declared $r=0$ or $p=1$ boundaries.

### AEE Case 3

**A1 — Verified: the accessible source and the valid integration step.** The [Stanford preprint](https://stacks.stanford.edu/file/druid:gy645ty0351/gy645ty0351_OLK-NSF-282.pdf), printed pp. 6–7, was checked directly from its page images. Its equation (4), with $M=3$ and $a=F_2$ held, is

$$
u(z,m,a)=\frac{(2z+1)\log(1+mz)-mz}{2z^2}
 -\frac a{2(1+z)}.
$$

Indeed $u_{mz}=-m(2-m)/\{2(1+mz)^2\}<0$ for $0<m<2$. Integrating at fixed $a$ gives, for $z_2>z_1$,

$$
u(z_2,m,a)-u(z_1,m,a)
<u(z_2,0,a)-u(z_1,0,a).
$$

**A2 — Gap: the right side cannot be cancelled.** At fixed $a$,
$u(z,0,a)=-a/\{2(1+z)\}$. For $a=1/4$, $z_1=1$, $z_2=2$, the boundary values are $-1/16$ and $-1/24$, and their difference is $1/48>0$. Feasibility at mean zero does force $a=0$, but that changes the held coordinate used in the integral. The valid inequality has a positive boundary term and does not establish the claimed graph ordering.

A direct repair of Case 3 would need a bound showing that its negative mixed-derivative integral dominates this positive boundary difference throughout each relevant feasible slice, or a valid feasible-path integration including the varying-frequency derivatives. This is a defect in that inference, rather than a feasible-sample counterexample to the NB theorem. N1–N6 give a complete replacement proof, following the expanded Brøns–Jensen argument identified in the source report.

**Open — historical attribution only.** The exact identity with Wang's objection requires Wang's printed pp. 113–114 and comparison with the published AEE pp. 375–379. The source report records unsuccessful publisher/PDF/API/repository attempts; this audit inspected the accessible preprint and its actual cancellation. No assertion is made that the unread published version contains the identical step, or that this is exactly Wang's objection. The theorem has no dependency on resolving that attribution.

## 4. SGED: exactly two global scale maxima

Source: the [SGED dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) and subunit-scale report.

**Precise theorem.** For any positive sample size, take every observation equal to $-1$, hold public mean zero and $\nu=1/2$, and define

$$
\log R=2(1-1/R),\quad R>2,\qquad
K=\frac{2\sqrt{R-1}}R,\qquad
\xi=\sqrt{1+K^2/6}.
$$

With this exact skew and scale alone free, there are exactly two global scale maxima.

**S1 — Verified: the defining root.** For $H(R)=\log R-2+2/R$, $H'=(R-2)/R^2$. It decreases from $H(1)=0$ to a negative minimum at two, then increases to infinity. There is exactly one further root. It lies in $(4,6)$; $0<K<1$ follows from $(R-2)^2>0$.

**S2 — Verified: public moment coordinates.** At $\nu=1/2$, the standardized GED constants are

$$
\lambda_0=\frac1{4\sqrt{120}},\quad
m_1=\frac6{\sqrt{120}},\quad
d=m_1(\xi-\xi^{-1}),\quad
s_u^2=(1-m_1^2)(\xi^2+\xi^{-2})+2m_1^2-1>0.
$$

Here $d>0$ and $s_u^2\ge1$. The post-cusp penalty coefficient is
$\sqrt{d/\lambda_0}\sqrt{\xi}/2=\sqrt{6(\xi^2-1)}=K$.
With $r=s_u/(d\sigma)$, the cost relative to the cusp is, per observation,

$$
F(r)-F(1)=
\begin{cases}
-\log r+(K/\xi)\sqrt{1-r},&0<r<1,\\
-\log r+K\sqrt{r-1},&r\ge1.
\end{cases}
$$

The log likelihood is a constant minus this cost.

**S3 — Verified: every stationary point and its orientation.** On $(0,1)$ the derivative is strictly negative and the cost decreases to zero. On $(1,\infty)$ put $y=\sqrt{r-1}$. Stationarity is
$Ky^2-2y+K=0$. The two stationary points are
$r_{\rm max}=1+1/(R-1)\in(1,2)$ and $r_{\rm min}=R>2$. At a stationary point,
$F''=(r-2)/\{2r^2(r-1)\}$ per observation. Thus the first is a cost maximum and the second a cost minimum. The defining equation gives $F(R)-F(1)=0$.

The cost rises from the cusp to the first stationary point, falls to zero at $R$, and then rises to infinity. Its left tail also diverges. Its only global minima are therefore $r=1,R$. The two public scales are

$$
\sigma_1=s_u/d\approx17.8700813571242,\qquad
\sigma_2=s_u/(dR)\approx3.63098376732302.
$$

The checker also compares their public-coordinate density values directly.

**S4 — Verified: the source's global-search inequalities.** For the general held asymmetric subunit-scale problem, $u=\max|x_i-\mu|>0$ and

$$
F(r)=-n\log r+P(r),\qquad
P=\sum_i k\,\xi^{-\nu\operatorname{sign}(d(1+b_ir))}
             |1+b_ir|^\nu.
$$

Since $P\ge0$, an incumbent $B$ excludes $\log r<-B/n-1$. For $r\ge2$, a term with $|b_i|=1$ gives
$F(r)\ge Cr^\nu-n\log r$ with
$C=k e^{-|\nu\log\xi|}/2^\nu>0$. This envelope eventually increases and diverges. On each sign cell $P$ is concave, so its chord lies below it. Minimizing that affine chord minus $n\log r$ uses the clamped point $n/\text{slope}$ when the slope is positive, and the right endpoint otherwise. Also
$P'(h)-n/l\le F'(r)\le P'(l)-n/h$ on $[l,h]$. These bounds retain every minimum. Both sides of a negative-data cusp are checked numerically.

**S5 — Verified with a precision qualification.** The theorem uses the exact definition of $\xi$, approximately $1.05258493227323583829$. Substituting a rounded decimal does not assert an exact mathematical tie. A numerical tie stop with competing witnesses has the appropriate weaker meaning. Constants equal to the held mean have $u=0$ and the separate unbounded scale-collapse case.

## 5. Beta-prime: held-shape tie and joint exteriors

Source: the [beta-prime dossier](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md) and the two beta-prime reports.

### Conditional ingredients

**B1 — Verified: unique scale and profile bounds.** Hold $p=a>0$, let $q>0$ vary, write $\tau=\log s$, and use arithmetic-mean-one data when deriving tail bounds. The per-observation log likelihood is

$$
f=(a-1)\overline{\log x}-a\tau
 -(a+q)\overline{\log(1+xe^{-\tau})}-\log B(a,q).
$$

For $u_i=x_i/(s+x_i)$ and $Q=\overline{u_i(1-u_i)}$,
$f_\tau=-a+(a+q)\bar u$ and $f_{\tau\tau}=-(a+q)Q<0$.
The root satisfies $qx_{\min}/a\le s(q)\le qx_{\max}/a$.
Implicit differentiation gives $\tau_q=a/\{(a+q)^2Q\}>0$; with both shapes varying, $\tau_p=-q/\{(p+q)^2Q\}<0$.

The profile derivative is
$D(q)=\psi(a+q)-\psi(q)-\overline{\log(1+x/s(q))}$.
The digamma increment decreases, while $s(q)$ increases. On $[L,U]$,

$$
\psi(a+U)-\psi(U)-\overline{\log(1+x/s(L))}
\le D(q)\le
\psi(a+L)-\psi(L)-\overline{\log(1+x/s(U))}.
$$

Its curvature is
$D'=\psi_1(a+q)-\psi_1(q)+a^2/\{(a+q)^3Q\}$.
The checker independently differentiates the original likelihood and its full scale-profile Hessian to verify these Schur-complement identities.

**B2 — Verified with a wording correction.** The dossier's Q enclosure needs the products at the two endpoint u-values. The minimum is the smaller endpoint product $u(1-u)$; the maximum is the larger product or $1/4$ when the interval straddles $1/2$. Raw endpoint u-values do not generally enclose those products. The [implementation](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/certificate.hpp) already computes the correct products. This is a prose clarification.

**B3 — Verified: lower-shape exterior.** From $\psi_1(t)>t^{-2}$, the scale bracket, and increase of $q\log(1+c/q)$,

$$
qD(q)\ge\frac a{a+L}
 -L\,\overline{\log(1+ax/(Lx_{\min}))},\qquad 0<q\le L.
$$

A positive right side excludes that whole lower exterior.

**B4 — Verified: unsigned and signed gamma remainders.** Put $M=\max x_i$, $V=\overline{x^2}$, $r=q/s(q)$ and
$G(a)=a\log a-\log\Gamma(a)-a+(a-1)\overline{\log x}$.
For $q>2aM$, Jensen's inequality and the scale equation give

$$
a\le r\le2a,\qquad r-a\le2a^2M/q.
$$

For example, the lower estimate of the scale equation gives
$r\le a/\{1-a(M-1)/q\}$, which implies both upper bounds.
Using $\log\Gamma(q+a)-\log\Gamma(q)\le a\log(q+a)$ and
$\log(1+z)\ge z-z^2/2$, and dropping the additional negative
$-a\overline{\log(1+rx/q)}$ term, gives

$$
f(q)\le G(a)+a^2(1+2V)/q.
$$

For the signed refinement, integrating $\psi(t)<\log t-1/(2t)$ gives

$$
\log\Gamma(q+a)-\log\Gamma(q)-a\log q
\le\frac{a(a-1)}{2q}+\frac{a^2}{4q^2}.
$$

The remaining first-order coefficient is
$H(r)=a(a-1)/2-ar+r^2V/2$. At $r=a$ it is
$A=a\{a(V-1)-1\}/2$, and $|H'|\le a(1+2V)$ on $[a,2a]$.
The rate displacement bounds this coefficient by
$A+2a^3M(1+2V)/q$. The residual term is at most $2a^3V/q^2$.
Therefore

$$
f(q)\le G(a)+A/q+C(a)/q^2,\qquad
C(a)=a^2/4+2a^3M(1+2V)+2a^3V.
$$

If $A<0$, a cutoff above $2aM$ and $2C(a)/(-A)$ puts the tail strictly below its gamma face. Interior competitors must still be compared. The boundary checker includes $A=0$ and both adjacent signs.

### A common compact domain and an exact intervening tie

**Precise theorem.** Hold $a=0.22690393135372058$. Take the nine observations defined by the exact decimal tokens in the [retained source data](2026-11-07-betapr-prof-data/counterexample-refined.json), replacing only its last observation by $x_9e^t$. There exists $t_*\in(5.6463,5.6466)$ for which the held-shape problem has at least two separated finite global maxima. This is conditional on the computed endpoint and gap comparisons retained below.

**B5 — Verified independently: endpoint ordering.** At each endpoint, the audit covers the entire losing half of $q\in[10^{-6},10^{20}]$ using B1's integrated derivative bounds, comparing its upper bounds with a valid finite witness on the other side. The witness need not itself be assumed globally optimal.

| t | Losing side of q=0.12 | Processed cells | Profile points | Minimum per-observation exclusion gap |
|---|---|---:|---:|---:|
| 5.6463 | Left | 43 | 23 | $>4.63\,10^{-7}$ |
| 5.6466 | Right | 107 | 55 | $>5.15\,10^{-7}$ |

The two approximate stationary q-values are respectively
$(0.0835277964475,0.168833666103)$ and
$(0.0835274316567,0.168831553237)$. All cover comparisons use profile upper bounds with propagated scale-root error. Their accepted separation exceeds $10^{-12}$ per observation. The [120-digit results](2026-11-09-proof-audit-data/results.json) retain values and gaps.

**B6 — Verified with explicit uniform compactness.** Changing one log observation has total likelihood derivative
$a-1-(a+q)x_9/(s+x_9)$, of absolute value at most $1+q$ since $a<1$. Taking a supremum over scale preserves this Lipschitz bound. The optimized fixed-shape gamma face has absolute derivative at most $1+na$.

At the retained center $t=5.646438395028742$, a finite witness with $q=0.08352762816269817$ and these Lipschitz bounds prove, throughout the whole t interval, total gaps

$$
\text{witness}-\text{barrier}>0.17569,\qquad
\text{witness}-\text{gamma face}>17.33799.
$$

The computed conservative values are 0.175691794873145 and 17.3379988534127. For the small-q bound, $x/x_{\min}$ is uniformly bounded by its upper-t endpoint values, so B3 excludes $q<10^{-6}$ throughout the path. For the large-q bound, normalized $V\le n$ uniformly; B4's unsigned remainder at $10^{20}$ is far smaller than the uniform gamma gap. Thus every competitive q lies in the same fixed compact interval. The scale brackets in B1 supply a common positive finite scale interval as well.

**B7 — Verified: continuity supplies global maxima on both sides.** The scale root and profile are continuous. The maxima over the two fixed closed q intervals separated at 0.12 are continuous functions of t, by uniform continuity on their common compact domain. B5 makes their difference strictly negative at the first endpoint and strictly positive at the second. The intermediate value theorem gives equality at an interior t. B6 excludes the separator itself from global maximization, and excludes both exteriors. Hence the equal side maxima occur at distinct finite parameter vectors and are global.

This proves at least two maxima. Neither a two-root picture alone nor agreement of rounded likelihoods proves exact equality. The rounded numerical crossing is not specified as the exact value of $t_*$.

### Joint exterior

**Precise result.** For any nonconstant finite positive sample, the following scale-uniform bounds exclude small harmonic shape and simultaneous large shapes. With harmonic shape retained, uniform gamma/inverse-gamma bounds control either remaining large individual shape. A finite witness strictly above both certified face upper bounds therefore supplies a compact competitive parameter set and proves finite attainment. These bounds alone do not assert arbitrary-sample joint uniqueness or select a winning face.

**J1 — Verified: small harmonic shape.** For $Y=\log X$, set $h=pq/(p+q)$, $y_i=\log x_i$ and mode $m_*=\log b+\log(p/q)$. Its log-density derivative is between $-q$ and $p$. Integrating the resulting lower exponential envelopes on either side of the mode gives $g(m_*)\le h$. Hence

$$
\ell_X\le n\log h-\sum_i y_i
$$

uniformly in scale. This tends to minus infinity as $h\downarrow0$.

**J2 — Verified: sharp height and curvature loss.** Stirling's remainder gives

$$
\log g(m_*)=\tfrac12\log(h/(2\pi))+w(p+q)-w(p)-w(q)
\le\tfrac12\log(h/(2\pi))+\frac1{48h}.
$$

Here $p+q\ge4h$. At displacement v from the mode, the negative log curvature divided by h is
$e^v(p+q)^2/(q+pe^v)^2\ge e^{-|v|}$; bounding the denominator separately for positive and negative v proves this inequality. Integrating twice gives loss at least $h\rho(d)$, where

$$
\rho(d)=d-1+e^{-d}\ge d-\log(1+d)\ge\frac{d^2}{2(1+d)}.
$$

The first inequality integrates $1-e^{-d}\ge d/(1+d)$; the second follows because the derivative of their difference is $d^2/\{2(1+d)^2\}\ge0$.

**J3 — Verified: simultaneous large shapes.** Convexity and evenness of $\rho(|v|)$ imply combined loss at least $2h\rho(D/2)\ge hD^2/(4+2D)$ for two log observations separated by D, whatever the mode. Pairing sorted extremes, let $J$ be the sum of these rational pair contributions. Then

$$
\ell_X\le U(h)=\frac n2\log\frac h{2\pi}+\frac n{48h}
 -hJ-\sum_i y_i.
$$

For nonconstant data $J>0$,
$U'<0$ on $h\ge\max(1,n/(2J))$, and $U\to-\infty$. This excludes the whole two-large-shape corner uniformly over scale and shape ratio. Constants have $J=0$ and their separate unbounded concentration likelihood.

**J4 — Verified: individual tails and finite attainment.** If $h<H$ and $q\ge2H$, then $p<2H$; the small-h cutoff bounds p away from zero. B4 gives the total remainder
$np^2(1+2V)/q\le4nH^2(1+2V)/q$ uniformly in the retained p range and all scales. The gamma profile has
$G''=n(1/p-\psi_1(p))<0$, so supporting tangents give valid face upper bounds on that compact shape interval. A strict witness-to-face gap supplies a finite q cutoff. Reciprocation gives the other tail with likelihood Jacobian $-2\sum\log x_i$. Both shapes then lie in a compact rectangle bounded away from zero, and their maximizing scales lie in $[qx_{\min}/p,qx_{\max}/p]$. Continuity proves attainment.

**J5 — Verified: the signed joint sharpening.** Use per-observation normalized gamma profile G, an enclosed optimum $a_*$, retained upper shape $H_p$, and $u=\max(H_p,a_{*,\rm upper})$. With $\kappa=1/(2u^2)$, the trigamma bound gives
$G(p)\le G(a_*)-\kappa(p-a_*)^2/2$.
Also $|A'|\le B=1/2+u(V-1)$, so
$A(p)\le A_0+B|p-a_*|$ for an upper bound $A_0$ over the optimum enclosure. Since C increases,

$$
F(p,q)\le G(a_*)+\frac{A_0}{q}
 +\frac{C(H_p)+B^2u^2}{q^2}.
$$

The extra term follows by completing the square:
$-\kappa d^2/2+Bd/q\le B^2u^2/q^2$.
If $A_0<0$, a strict cutoff above both $2H_pM$ and
$\{C(H_p)+B^2u^2\}/(-A_0)$ excludes the entire tail below the gamma supremum, even where A changes sign. Reciprocal transport gives the inverse-gamma result. The alternative local-negative-A neighborhood plus outside gamma-tangent-gap argument in the dossier follows from the same signed and unsigned bounds.

**J6 — Verified: no nondegenerate lognormal corner.** The representation
$X=bG_p/G_q$ for independent unit-scale gamma variables gives, by differentiating their Mellin transforms,

$$
\operatorname{Var}(\log X)=\psi_1(p)+\psi_1(q).
$$

This tends to zero if both shapes diverge; changing b only translates log X. Chebyshev concentration implies any tight proper log-coordinate limit is a point mass. It cannot be a nondegenerate fixed-variance normal law. A centered and rescaled limit uses an additional transformation outside this three-parameter family. J3 is the stronger likelihood exclusion for nonconstant data, uniform even along coupled scale escapes.

## 6. Beta-binomial: signed uniform face certificate

Sources: the [beta-binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md) and face report.

**Precise theorem.** Let known $N\ge2$, with h observations in $\{0,\ldots,N\}$ and at least one interior count. Write $S=\sum x_i$, $Q=\sum x_i^2$, $p_0=S/(hN)\in(0,1)$, $p=\alpha/(\alpha+\beta)$ and $s=1/(\alpha+\beta)$. Let $L(s)=\max_{0<p<1}\ell(p,s)$ and $F=L(0)$, dropping the data-only binomial coefficients. Then the uniform bound and conditional curvature bounds below hold. Together with a strict outside cover and tail exclusion, they certify $L(s)<F$ at every finite positive s and the unattained binomial supremum.

**C1 — Verified: decomposition and the uniform first-order strip.** With
$A_j=\#\{x_i>j\}$ and $C_j=\#\{x_i<N-j\}$,

$$
\ell=V-D,\quad
V=\sum_{j=0}^{N-1}\{A_j\log(p+js)+C_j\log(1-p+js)\},\quad
D=h\sum_{j=0}^{N-1}\log(1+js).
$$

V is jointly concave and $V_p(p_0,0)=0$. Its tangent therefore bounds V uniformly over every mean. Since $(-D)''\le B=h\sum j^2$, Taylor's upper bound gives

$$
L(s)-F\le g_0s+\tfrac12Bs^2,\quad
B=\frac{h(N-1)N(2N-1)}6,\quad
g_0=\frac{NhQ-(N-1)S^2-NhS}{2hNp_0(1-p_0)}.
$$

The moment expression follows from
$\sum jA_j=(Q-S)/2$ and
$\sum jC_j=\{hN(N-1)-(2N-1)S+Q\}/2$.
If $g_0<0$, $0<s\le-g_0/B$ is strictly below the face. A negative boundary score supplies this local strip; it does not decide the entire global comparison.

**C2 — Verified: the rectangle must enclose every profiled mean.** On $p\in[l,u]$, $0\le s\le t$, lower and upper bounds on the mean score at the two edges are

$$
\sum_j\left\{\frac{A_j}{l+jt}-\frac{C_j}{1-l}\right\},
\qquad
\sum_j\left\{\frac{A_j}{u}-\frac{C_j}{1-u+jt}\right\}.
$$

If these are positive and negative respectively, strict concavity in p places its unique root inside the rectangle for every s. The interior-count assumption makes the likelihood tend to minus infinity at either mean endpoint.

**C3 — Verified: all curvature denominators and signs.** Set

$$
a_j^-=\frac{A_j}{(u+jt)^2},\quad a_j^+=\frac{A_j}{l^2},\quad
c_j^-=\frac{C_j}{(1-l+jt)^2},\quad c_j^+=\frac{C_j}{(1-u)^2}.
$$

Then $M=\sum(a_j^-+c_j^-)>0$ bounds $-\ell_{pp}$ below,
$J=\sum j^2(h-a_j^--c_j^-)$ bounds $\ell_{ss}$ above, and
$K_-=\sum j(c_j^--a_j^+)$, $K_+=\sum j(c_j^+-a_j^-)$ enclose $\ell_{ps}$. For J, the positive denominator term $h/(1+js)^2$ is bounded above by h. Implicit differentiation of the mean root gives

$$
L''=\ell_{ss}+\frac{\ell_{ps}^2}{-\ell_{pp}}
\le C=J+\frac{\max(|K_-|,|K_+|)^2}{M}.
$$

If $C<0$ and $g_0\le0$, integration gives
$L(s)-F\le g_0s+Cs^2/2<0$ throughout the positive strip. This includes exact zero score. The size-two table $(1,2,1)$ verifies such a rectangle and negative C independently.

**C4 — Verified: residual-safe cover and the tail.** Concavity of V and a chord above convex $-D$ give, on $s\in[a,b]$ and at midpoint anchor $(p_m,m)$,

$$
\ell(p,s)\le\ell(p_m,m)+|\ell_p(p_m,m)|
 +\max_{v\in\{a,b\}}\{V_s(p_m,m)(v-m)-D(v)+D(m)\}.
$$

The residual term uses $|p-p_m|\le1$, so the inequality remains valid for an approximate mean solve. It must be strictly below F for every retained interval outside the signed strip.

Let $I=\#\{0<x_i<N\}>0$. Expanding $js/(p+js)$ and its complementary terms gives constant sum
$\sum_{j=1}^{N-1}(A_j+C_j-h)=-I$. Dropping the remaining negative terms and using $1/(1+js)\le1/(js)$ gives

$$
s\ell_s(p,s)\le-I+hH_{N-1}/s.
$$

Thus beyond $hH_{N-1}/I$ every mean has negative dispersion score. A strict cover through a tail anchor, followed by this decreasing tail, excludes all remaining finite s. The limit at $(p_0,0)$ is F. These three pieces, rather than a score sign alone, prove the global nonattainment assertion.

**C5 — Verified: adversarial tables and the finite-gain safeguard.** For $N=3$, frequencies $(1000,3001,2999,1000)$ have exact face numerator $-2$; reflection preserves it. For $N=2$, the saturated empirical solution when $n_1>0$ and $\Delta=4n_0n_2-n_1^2>0$ is

$$
\alpha=\frac{(n_1+2n_2)n_1}{\Delta},\qquad
\beta=\frac{(2n_0+n_1)n_1}{\Delta}.
$$

It reproduces the empirical probabilities and attains the multinomial likelihood upper bound. The independent checks reproduce both source examples:

| N | Frequencies | Symmetric shape | Total finite gain over binomial |
|---:|---|---:|---:|
| 2 | (25001, 50000, 25001) | 25000 | 0.0000199996000093331 |
| 3 | (12501, 37499, 37499, 12501) | 18749.5 | 0.000106664770458860 |

All empirical probabilities in both tables are matched directly, so each is a global finite witness by saturation. Their tiny positive gains demonstrate why a positive likelihood preference for the binomial cannot replace the signed certificate. This proof does not supply a general symbolic uniqueness theorem. Sizes zero and one, and endpoint-only samples, require their separately stated handling.

## Independent numerical implementation

The independent [main checker](2026-11-09-proof-audit-data/check.py) and [boundary checker](2026-11-09-proof-audit-data/boundaries.py) use Python 3.14.5, mpmath 1.3.0, 120 decimal digits and seed 165. Decimal tokens in the beta-prime counterexample are read as exact decimal strings. Exact rational classifications are used for root-existence boundaries and integer face numerators.

The main run passed 56,036 recorded mathematical comparisons and 150 cover-cell evaluations, giving 56,186 recorded checks across 117 counters. The boundary run passed 1,070 recorded comparisons and retained 160 signed root brackets: 132 PIG held-mean brackets and 28 NB brackets. Raw [main results](2026-11-09-proof-audit-data/results.json), [boundary results](2026-11-09-proof-audit-data/boundaries.json) and [foreground computation records](2026-11-09-proof-audit-data/computations.tsv) are retained.

| Family | Random and adversarial coverage |
|---|---|
| PIG | Bessel degrees from counts 2, 3, 4, 10, 50, 100; z from $10^{-20}$ to $10^{20}$; factorization at $r=w$; zero and one counts; zero laws; equality and perturbed held means |
| Fréchet | 76 samples including constants and tied minima; gaps spanning $10^{-20}$ to $10^{20}$; shapes $10^{-8}$ through $10^8$; exact collapse ratios and adjacent relative perturbations; gaps $\epsilon$ from $10^{-25}$ to $10^{25}$ |
| NB | Seeded count samples; means below one; integer split boundaries; $r=10^{-12}$ through $10^{12}$; pairs $m\pm k$ with $m=k^2-1,k^2,k^2+1$; zero laws; exact AEE boundary difference and mixed-derivative step |
| SGED | Exact constructed tie; public scale reconstruction; near-cusp and far-tail points; 200 random subunit shape/weight/skew cases; chords and derivative bounds on both sign cells |
| Beta-prime | 160 random joint points, 80 conditional/tail/Hessian cases; both endpoint losing-side covers; shapes $10^{-30},10^{30}$ and extreme ratios; log-scale shifts $\pm10000$; constant and nearly constant data; zero and adjacent signed tail coefficients |
| Beta-binomial | 105 fixtures at sizes 2, 3, 5, 10, 20; means near $10^{-15}$ and $1-10^{-15}$; dispersion down to $10^{-20}$; rectangle/Schur bounds; zero score, numerator $-2$, reflections and both tiny positive gains |

Weak inequalities and identities have a practical scaled allowance $10^{-70}$. Strict inequalities require a positive computed slack. The beta-prime cover accepts only gaps exceeding $10^{-12}$ per observation, well separated from that arithmetic allowance.

Every inner beta-prime scale solve has an explicit score-derivative lower bound over its initial bracket. Its residual therefore bounds log-scale error by $\delta<10^{-75}$, rather than treating a small score as sufficient. The profile value error is at most $(a+q)\delta^2/8$, and derivative error at most $\delta$; both enter the endpoint covers. PIG/NB bisections require opposite endpoint signs and width below $10^{-60}\max(1,|\text{left}|)$.

The research solvers use the adjustable UMVL_AUDIT_ITERLIM, default 512, with solver cap $\min(\mathrm{iterlim},100000)$ and cover cap $128\min(\mathrm{iterlim},100000)$. Exhaustion fails the check. Numerical root searches used to propose witnesses do not establish their globality; the monotonicity proofs or closed covers do.

## Final validation and source preservation

All commands below were foreground commands and were waited for. Local computation used the supplied CPU-slot wrappers; the final short numerical runs used one slot and nice 10. R suites and the remote C++ double-mode run used simbox-run. BLAS/OpenMP and the suite driver were pinned to one worker. Temporary files stayed under the worktree's build directories. No simbox exit-75 fallback was needed.

| Final run | Machine | Result |
|---|---|---|
| C++ dev | Workstation, GCC 15.2 | 106/106 shards |
| C++ full | Workstation, GCC 15.2 | 129/129 shards |
| C++ no-long-double | Workstation, GCC 15.2 | 129/129 shards |
| C++ no-long-double | simbox, GCC 13.3 | 129/129 shards |
| R dev | Workstation, R 4.6.1 | FAIL 0, WARN 0, SKIP 154, PASS 22,218 |
| R dev | simbox, R 4.6.1 | FAIL 0, WARN 0, SKIP 154, PASS 22,218 |
| R full | simbox, R 4.6.1 | FAIL 0, WARN 0, SKIP 0, PASS 45,053 |
| Main mathematical checker | Workstation | PASS, 120 digits |
| Boundary/root checker | Workstation | PASS, 120 digits |

The [suite ledger](2026-11-09-proof-audit-data/suites.tsv) records start/finish times and exit statuses, including queue time. The earlier workstation R full attempt was interrupted by the usage limit and is superseded by the completed simbox run. Its retained log is not counted as a successful run.

The [provenance manifest](2026-11-09-proof-audit-data/provenance.json) hashes the proof sources, scripts, final numerical results and decompressed logs. Representative retained final logs are [workstation C++ double mode](2026-11-09-proof-audit-data/workstation-cpp-double.log.gz), [simbox C++ double mode](2026-11-09-proof-audit-data/simbox-cpp-double.log.gz), [simbox R dev](2026-11-09-proof-audit-data/simbox-r-dev.log.gz) and [simbox R full](2026-11-09-proof-audit-data/simbox-r-full.log.gz). Compiler warnings from unchanged sources remain in the build logs; the R warning counts above are test counts.

The final [consistency checker](2026-11-09-proof-audit-data/validate.py) verifies script/result identities, source hashes, archived logs, completed green suite summaries, report links and an empty tracked production diff. Its retained output is validation.json in the evidence directory.

To reproduce individual runs from this worktree, use one foreground command at a time:

~~~sh
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-09-proof-audit-data/local-check.sh check
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-09-proof-audit-data/local-check.sh boundaries
bash dev/research/evaluations/2026-11-09-proof-audit-data/remote-suite.sh r-dev
bash dev/research/evaluations/2026-11-09-proof-audit-data/remote-suite.sh r-full
bash dev/research/evaluations/2026-11-09-proof-audit-data/remote-suite.sh cpp-double
~~~

The remote launcher performs the prescribed local fallback only on exit 75. The retained suite driver also supports local cpp-dev, cpp-full, cpp-double, r-dev and r-full modes through a local slot wrapper.

## Findings to carry into publication

Add the positive-mean hypothesis to the PIG showcase shorthand, and distinguish the all-zero attained zero law. Describe NB convexity with its negative-log-likelihood sign. Clarify that the beta-prime Q enclosure uses endpoint products. Keep the SGED skew and the beta-prime intervening sample defined exactly, rather than converting rounded near-ties into exact examples.

Use the verified complete NB proof for the existence/uniqueness theorem. The AEE preprint's specific graph-ordering cancellation remains a gap with the missing comparison identified above. The historical Wang/published-version comparison remains the precisely bounded attribution question; it does not leave any of the six requested mathematical results dependent on that gap.
