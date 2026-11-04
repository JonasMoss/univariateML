# Derivation: seams, integer sizes and the beta-binomial corner

All samples are iid from the indicated limit law, with its nondegenerate
parameters fixed in the interior. Log likelihoods are total sums. Information
below is **per observation**, in the displayed coordinates. Model Fisher
information is used; these are not misspecification-robust results.

## 1. Common seam experiment

Let eta be a regular face nuisance and t>=0 a transverse coordinate. Suppose
the model has a one-sided DQM expansion with scores `(s_eta,s_t)` and positive
definite information, locally uniform over eta in a compact interior set.
For eta=eta0+a/sqrt(n), t=b/sqrt(n), finite-dimensional likelihood ratios
converge to a Gaussian shift. Removing nuisance by orthogonal projection gives

$$s_e=s_t-E(s_t s_\eta^T)I_{\eta\eta}^{-1}s_\eta,\quad I_e=E s_e^2>0,$$
$$\ell_{\mathrm{prof}}(b)-\ell_{\mathrm{prof}}(0)
 =b\Delta_e-\tfrac12b^2I_e+o_P(1),\quad\Delta_e\Rightarrow N(0,I_e).$$

For localized maximization (first compact sets of b, then increasing compact
sets), twice the supremum difference is
$\max(0,Z)^2$, hence $\tfrac12\chi_0^2+\tfrac12\chi_1^2$. Under local
alternative b, replace Z by $N(b\sqrt{I_e},1)$. This is the elementary
Gaussian projection behind Self–Liang Case 5. Full finite-sample ML needs
localization and accurate supremum computation; DQM alone proves neither.

### Lomax to exponential; Burr to Weibull

Use the GPD extension
$p_{\xi,\sigma}(x)=\sigma^{-1}(1+\xi x/\sigma)^{-1/\xi-1}$,
with threshold zero fixed and xi>=0 on the Lomax side. Under xi=0,
`z=x/sigma` is unit exponential. The scores for xi and log sigma are

$$s_\xi=z^2/2-z,\quad s_{\log\sigma}=z-1,\qquad
 I=\begin{pmatrix}2&1\\1&1\end{pmatrix}.$$

Thus I_e=1 with scale fitted, versus I_xi=2 with scale known. With scale
fitted, $s_e=z^2/2-2z+1$. Lomax's original tail shape is `1/xi`, so its
divergence at the limit is of order sqrt(n), not a finite root-n coordinate.

DQM follows by Taylor expansion on z<=L log(1/xi), then a tail split. For
small xi the Lomax tail at this cutoff is bounded by an exponential with a
slightly smaller rate; its mass can be made o(xi^2). Polynomial score tails
under the exponential are equally negligible. This proves the square-root
remainder is o(xi) in L2; differentiation of the pointwise density alone
would not suffice. The argument is locally uniform in positive compact scales.
The negative-xi GPD side gives a proper extension; its moving upper support
has exponentially negligible mass at zero shape. The extension is useful,
but a negative side is unnecessary for the one-sided experiment.

For Burr XII take z=(x/lambda)^c and xi=1/k_B, where k_B is the Burr tail
shape and lambda=s*k_B^(-1/c) in the original Burr scale s,
keeping c,lambda as Weibull face coordinates. The transformed z has
the same GPD density. At xi=0 the nuisance scores are

$$s_{\log\lambda}=c(z-1),\qquad
 s_c=\{1+(1-z)\log z\}/c.$$

The shape score remains z^2/2-z. Their three functions are linearly independent
in L2(Exp(1)), so joint information is positive definite; projecting both
nuisances gives positive I_e. Knowing c gives I_e=1; knowing both nuisance
parameters gives I_e=2. Estimating c and lambda requires the full projection,
not the Lomax information. In fact, removing scale leaves covariance -1
between the transverse score and the residual c score, whose information is
pi^2/(6c^2) with the corresponding 1/c covariance factor. Therefore
I_e=1-6/pi^2=0.3920728981 when both Weibull nuisances are fitted.
Integrable logarithmic factors at zero and
exponential tails justify joint DQM. Original-data Jacobians and face scores
are included in these formulas. `w=1/c` held in the Pareto analysis is a
different regime from this fixed-positive-c Weibull analysis.

### NB/binomial to Poisson

Fix mean mu>0. For NB shape a, u=1/a>=0. For binomial size N integer,
kappa=1/N>=0 and p=mu/N. At Poisson define

$$C_2=x(x-1)-2\mu x+\mu^2,\quad s_u=C_2/2,\quad s_\kappa=-C_2/2.$$

Both scores are orthogonal to the Poisson mean score `(x-mu)/mu`, and
$E C_2^2=2\mu^2$, giving I_e=mu^2/2 whether the mean is known or fitted.
Each one-sided family has the half-chi-square experiment. The union of the
two sides has a full Gaussian line and chi-square_1 against Poisson; treating
that union as a single one-sided test would be incorrect.

Finite-count Taylor expansions with uniformly controlled exponential tails
give the square-root expansion. For binomial, mass beyond N under the
Poisson is smaller than every power of 1/N. The integer parameter does not
have a Euclidean DQM chart, but the **sequence of actual integer models**
has the same local experiment: around N~sqrt(n), neighboring kappas are
spaced O(1/n), which is o(1/sqrt(n)). This is an asymptotic set-convergence
argument, not an assertion that noninteger binomial sizes define probability
laws. Known fixed N with p->0 instead gives a degenerate mean-zero law and
is outside this positive-mean experiment.

Consequences for size inference with unknown p and bounded positive mu:

- N~sqrt(n) is the Poisson detection scale; N much larger than sqrt(n) is
  indistinguishable from Poisson at this resolution.
- Away from the seam, the approximate standard error of N is
  $N^2/\sqrt{nI_e}$, so relative error is of order N/sqrt(n).
- Neighboring integer sizes have Gaussian separation of order
  sqrt(n)/N^2. A continuous approximation to **individual-size** inference
  needs N much larger than n^(1/4). At N~n^(1/4) a lattice Gaussian
  experiment remains; at smaller diverging N, adjacent sizes become
  distinguishable. A fixed-N limit has its own discrete-design analysis.

Thus relative consistency can coexist with poor recovery of the exact integer.
An interval in kappa containing zero maps to a size set unbounded above,
including the infinite-size limit if admitted. A finite upper Wald endpoint
for N discards that scientifically relevant uncertainty. These rate results
derive the mechanism suggested by Olkin–Petkau–Zidek's verified abstract;
they are not quoted as a theorem of that paper.

## 2. The two BB seams

Write BB(N,a,b), with mean mu=Na/(a+b). At fixed finite N>=2, put
rho=1/(a+b+1). At rho=0 the face is Bin(N,p), p=mu/N. Taylor expansion
of the beta mixture about its mean gives

$$s_\rho=\frac{(x-Np)^2-(1-2p)(x-Np)-Np(1-p)}{2p(1-p)}.$$

It is orthogonal to the binomial probability score; its information is
N(N-1)/2 for 0<p<1. This follows either by elementary binomial factorial
moments or the degree-two orthogonal polynomial identity. Finite support
makes one-sided DQM immediate from the pmf expansion. Therefore the
BB/binomial test has the same half-chi-square local limit. N=1 is an
exception: the whole BB family is Bernoulli and concentration is unidentified,
with exactly zero score/information. N=0 is a point mass. Mean endpoints
p=0,1 are excluded from the regular statement.

For the other seam fix mu>0,a>0 and let N->infinity with
b=a(N/mu-1), kappa=1/N. The face is NB(a,mu). The exact BB ratio gives

$$s_\kappa(x)=\frac{-a x(x-1)+2\mu(a+1)x-(a+1)\mu^2}{2(a+\mu)}.$$

The nuisance scores are

$$s_\mu=\frac{a(x-\mu)}{\mu(a+\mu)},\qquad
 s_a=\psi(a+x)-\psi(a)+\log\frac a{a+\mu}+\frac{\mu-x}{a+\mu}.$$

All are square integrable under NB; project whichever of mu,a are fitted.
The three functions are linearly independent: a quadratic cannot be an
affine combination of the mean score and the shape score (whose asymptotic
form contains log x and a linear term). The nuisance pair is also independent.
Thus the full information and every efficient transverse information are
strictly positive for fixed positive a,mu.

Here is the missing **one-sided DQM proof**, beyond a fixed-count expansion.
The BB factorial moments satisfy

$$E(X)_{\underline j}=\mu^j\prod_{i=0}^{j-1}
 \frac{(1-i/N)(1+i/a)}{1+\mu i/(aN)}
 \le\mu^j\prod_{i=0}^{j-1}(1+i/a).$$

Consequently $E(1+z)^X\le(1-\mu z/a)^{-a}$ for a fixed 0<z<a/mu,
uniformly in N; the NB has the same bound. On x<=L log N, expand the exact
finite ratio and zero probability in 1/N, with polynomial-in-x remainder
O(N^-2). For the zero probability the factor denominators stay bounded
away from zero and a sum-to-integral expansion with its first endpoint
correction gives the score constant above and O(N^-2) error. The product
representation, or Euler–Maclaurin with a bounded second derivative, gives
that bound uniformly for a,mu in compact positive sets. Summing the ratio
increments adds only a fixed polynomial in x. Taking square roots gives
$\sqrt p_N=\sqrt p_{NB}(1+\kappa s_\kappa/2)+o_{L^2}(\kappa)$:
choose L so both exponential tails and their score-weighted tails are
o(N^-2). This also covers the NB tail beyond the BB support.

Joint variation in regular a,mu adds their usual derivatives, with the same
compact uniform control. Integer mesh size is negligible at N~sqrt(n), as
above. Hence the local BB/NB test is half-chi-square, with the nuisance
projection appropriate to the chosen hypothesis. This does not establish a
two-sided BB/BNB chart, and its bounds are not uniform as a tends to infinity
toward the Poisson corner.

### Localization of these seam likelihood suprema

The Gaussian limits above also hold for mathematical unrestricted closure
likelihood suprema in these stated regimes. For Lomax/Burr at the
exponential/Weibull law, use the mode-height/envelope compactness and Wald
argument from the [Burr localization proof](burr-localization.md), Section 1.
Identifiability forces the Weibull face. In its xi=1/k_B chart the total
log likelihood has locally uniformly integrable derivatives through third
order under the true Weibull: derivatives are bounded by powers of
z and integrable logarithmic factors at zero. Uniform Hessian convergence
and positive information give root-n localization and the cone projection.
The Lomax case is the c=1 restriction.

For NB/binomial-to-Poisson and BB-to-fixed-NB use the empirical-KL finite
count truncation argument from [BB localization](bb-localization.md), Section 1.
The true NB also has an exponential moment, so K_n=C log n with sufficiently
large fixed C gives the same preliminary Hellinger bound. Closure and
identifiability force the indicated face coordinates; nondegenerate
information turns the preliminary Hellinger rate into a parameter rate
O_P(sqrt(log n/n)). A third-order expansion on the logarithmic observed
support has summed remainder O_P((log n)^(3/2)/sqrt(n)) times fixed sample
moment bounds, hence o_P(1). Quadratic coercivity improves the rate to
root-n. The missing far tail above an integer support is handled by
truncated normalization, not infinite KL in the wrong direction.

For fixed-binomial BB, finite support gives the usual finite-cell argument.
If size is also allowed to vary over integers, a true Bin(N0,p0), N0>=2,
0<p0<1, is locally isolated from other sizes: the BB closure theorem
excludes unbounded sizes (their nondegenerate limits are NB or Poisson),
and each other bounded size's compact closure lacks this binomial law.
Thus competitive fits have N=N0 with probability tending to one, before
the regular concentration-direction argument applies. This excludes the
N0=1 Bernoulli nonidentifiability exception already stated.

## 3. The BB Poisson corner: the experiment rather than coordinate rank

Take u=1/a,k=1/N, b=a(N/mu-1), with actual integer N. Let

$$C_3=(x)_{\underline3}-3\mu(x)_{\underline2}+3\mu^2x-\mu^3,
\quad E C_j C_l=1_{j=l}j!\mu^j.$$

The expansion established in the boundary note is

$$\sqrt p-\sqrt g=\left\{\frac{u-k}{4}C_2+
 \frac{k^2}{2}\left[-\frac{C_3}{3}-\frac{\mu+1}{2}C_2\right]\right\}\sqrt g
 +o_{L^2}(k^2),\qquad u-k=O(k^2).$$

Its uniform tail proof uses the preceding factorial-moment bound with a=1/u,
which gives arbitrary fixed exponential moments bounded near u=k=0.
Outside cancellation the linear `(u-k)C2/4` term dominates. Define

$$d=u-k-(\mu+1)k^2,\quad
 A=\sqrt n\,\mu d/\sqrt2,\quad
 B=\sqrt n\sqrt{2\mu^3/3}\,k^2\ge0.$$

For bounded A,B the per-observation root expansion is

$$\sqrt{p_n/g}=1+\frac{A e_2+B e_3}{2\sqrt n}
 +o_{L^2(g)}(n^{-1/2}),\quad
 e_2=C_2/\sqrt{2\mu^2},\quad e_3=-C_3/\sqrt{6\mu^3}.$$

The notation for root ratios uses the null support; BB's lost far-tail mass
is superpolynomially small at these rates. The expansion plus normalization
and the iid triangular-array likelihood lemma gives

$$\log\frac{dP_{A,B,n}^{\otimes n}}{dP_{0,0}^{\otimes n}}
 =A Z_{2n}+B Z_{3n}-\tfrac12(A^2+B^2)+o_P(1),\quad
 (Z_{2n},Z_{3n})\Rightarrow N_2(0,I_2).$$

One way to check the lemma is to truncate the root-ratio remainder and the
scores, expand log(1+root increment), and use normalization to identify the
mean quadratic term. A deterministic expansion of n log pmfs through degree
two in u,k is *not* sufficient: n k^3 need not vanish. The L2 expansion at
the effective experiment speed supplies the necessary centered remainder.
Compact uniform polynomial/tail bounds also give equicontinuity and compact
local likelihood-process convergence. At B=0 one uses the noncancelling
expansion and sequences k=o(n^-1/4); its attainable limiting A is arbitrary.

Every `(A,B)` with B>0 is attained to o(1) by choosing N nearest the prescribed
multiple of n^(1/4), then choosing continuous u to set d. Neighboring k^2
values have spacing O(n^-3/4), o(n^-1/2). B=0 and arbitrary signs of A
are approached with k of order n^-1/2 or smaller and positive u. Thus the
local set is **R x [0,infinity)**. It is not a quadrant with two independent
one-sided first-order scores. The Poisson mean score is orthogonal to both
e2,e3; fitting the mean adds an independent Gaussian nuisance that cancels.

The localized Poisson-vs-BB LR therefore has limit

$$T=Z_2^2+\max(0,Z_3)^2
 \sim\tfrac12\chi_1^2+\tfrac12\chi_2^2.$$

There is no atom at zero. Restricting to NB alone or binomial alone gives
half-chi-square; restricting to their union gives chi-square_1. On a fixed
cancelling curve with d=0 there is only B>=0 and half-chi-square. These are
different tests, even though all approach the same Poisson law.

The inferred rates are root-n for mean and d, but n^-1/4 for u,k along
cancellation. If a bounded-coordinate local maximum has Bhat>0,
$k\approx n^{-1/4}\sqrt{\widehat B/\sqrt{2\mu^3/3}}$;
if the limiting Bhat=0 its next rate/representation is not determined by this experiment.
The limiting projection is on that edge with probability 1/2. This is not
a claim about the probability of exact finite-sample face selection; weak
convergence alone does not establish that probability. There is no
single nonsingular root-n covariance for `(a,N)` at the corner.

**Global supremum.** The separate [localization proof](bb-localization.md)
upgrades this corner result to the unrestricted mathematical BB-vs-Poisson
likelihood supremum. It first bounds every competitive law by empirical KL,
then obtains an initial rate and a uniform fourth-order expansion. The
earlier fixed-sample closure theorem alone was insufficient for that step.
This proves a supremum-value limit, not correctness of the production fitter.

## 4. Confidence sets and uniformity

In the whitened seam experiment observe Y~N(h,1), h>=0. The LR for a candidate
h is

$$W_h=(Y-h)^2-\{Y-\max(Y,0)\}^2.$$

At h=0 it is half-chi-square; at h->infinity it becomes chi-square_1. Using
the 95% boundary critical value 2.705543 for every candidate yields coverage
tending to 90% in the interior. Using 3.841459 is conservative uniformly in
this Gaussian experiment, because W_h<=Z^2; boundary coverage is 97.5%.
Candidate-dependent Gaussian test inversion achieves the appropriate local
coverage. Transfer uniformly over bounded local h and compact face nuisances
requires the uniform LAN/control already stated, not merely pointwise Wald
normality. It does not cover nuisance approaching a new singularity.

At the BB corner, use the bivariate Gaussian cone experiment for joint
effective-coordinate regions. Candidate-specific LR distributions change
with the true B. A chi-square_2 cutoff gives a conservative uniform region
within that experiment since the LR is bounded by the squared norm of its
two-dimensional error. Inverting and projecting through the actual BB model
preserves integer N and can give unbounded sets for sizes/shapes. A chi-square_1
cutoff or separate Wald intervals ignores the cubic direction. Approaching
Poisson along NB, binomial, or a cancelling BB sequence changes the
experiment, so seam approximations are not uniformly valid over all such
sequences.

For example, an n-out-of-n parametric bootstrap fitted at the constrained
Gaussian mean uses hhat=max(Y,0), a nondegenerate random local parameter;
its conditional law generally fails to converge to the fixed h=0 law.
Bootstrap validity cannot be inferred from availability of a sampler.
Null-model simulation for a stated test and candidate-wise calibrated
inversion are different procedures; neither is automatically robust to
misspecification. The Burr PPP experiment needs its own inversion and
localization, as detailed in the separate derivation.
