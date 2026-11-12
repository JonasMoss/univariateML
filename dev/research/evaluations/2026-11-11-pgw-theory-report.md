# Power generalized Weibull: parametrization and likelihood

TASK-181, 2026-11-11. This decision is for uncensored, finite, strictly positive observations, $n\ge1$. It does not implement a fitter. The baseline is `bcea25d29800d4ddf058182c1749584b704cba3b`. Exact-arithmetic derivations and numerical illustrations are distinguished below. Empty samples, nonpositive or nonfinite data, and invalid holds require their usual separate input diagnostics.

## Decision

Recommend **ordinary PGW**, id `pgw`, with positive finite `scale` $a$, `shape` $b$, and `exponent` $c$, in that order. Its simple definition matches the proposal, its shape $c$ has a strictly concave conditional likelihood, and all finite coefficients are interior. Do not add a proportional-hazards coefficient. Adapted PGW with AFT scale only is a genuinely different three-parameter family; it gives an explicit log-logistic member and a more natural distribution-selection scale, but changes the likelihood and has an included shape boundary. It is a reasonable alternative for a separate future contract, not a synonym or a second name for this model. Its free PH version is four-parameter and is not recommended here. The decision does not claim ordinary PGW dominates adapted PGW in fit or ease of global certification.

## Exact definitions and comparison

For $x>0$, ordinary PGW is

$$H_P(x)=\{1+(x/a)^b\}^c-1,\quad S_P(x)=e^{-H_P(x)},\quad
f_P(x)={bc\over x}t(1+t)^{c-1}e^{1-(1+t)^c},\quad t=(x/a)^b.$$

All $a,b,c\in(0,\infty)$ give a proper distribution on $(0,\infty)$: $H_P(0+)=0$ and $H_P(\infty)=\infty$. The hazard is asymptotic to $(bc/a^b)x^{b-1}$ at zero and $(bc/a^{bc})x^{bc-1}$ at infinity. $c=1$ gives Weibull with shape $b$, scale $a$; $b=c=1$ is exponential with rate $1/a$. With $b=1$ the cumulative hazard is $(1+x/a)^c-1$, sometimes called a generalized exponential; naming is not unique. Finite PGW is neither Burr nor log-logistic: its survival has stretched-exponential, not power-law, tail.

[Burke, Jones and Noufaily](https://arxiv.org/abs/1901.03212) define the adapted baseline, here with AFT scale $A>0$ and power $b>0$, by

$$H_A(x)={1+\kappa\over\kappa}\left[\left\{1+{(x/A)^b\over1+\kappa}\right\}^{\kappa}-1\right],\qquad \kappa>-1,\ \kappa\ne0.$$

The continuous extension at $\kappa=0$ is $H_A=\log(1+(x/A)^b)$. It is proper exactly for $\kappa\ge0$ in this domain. For $-1<\kappa<0$, $H_A(\infty)=-(1+\kappa)/\kappa$ and the mass at infinity is $\exp((1+\kappa)/\kappa)>0$; exclude that cure regime from a proper-distribution contract. At $\kappa=1$ it is Weibull $(b,A)$; at $\kappa=0$ it is log-logistic $(b,A)$; and as $\kappa\to\infty$ it has $H=e^{(x/A)^b}-1$, a power-transformed Gompertz. When $b=1$ the latter is Gompertz with package parameters $a_{G}=b_{G}=1/A$.

The paper also considers $\lambda H_A$ with PH coefficient $\lambda>0$. At $\kappa=0$ that is Burr XII with `shape1 = lambda`, `shape2 = b`, `scale = A`; $\lambda=1$ is log-logistic. At $\kappa\to\infty$, $b=1$, it is the full Gompertz family with $a_G=\lambda/A$, $b_G=1/A$. These are claims about the PH-extended construction, **not** about the three-parameter AFT-only adapted model. The fully simultaneous PH/AFT formulation has four coefficients and a redundancy at Weibull.

For positive $c=\kappa$, ordinary PGW can be written $H_P=\lambda H_A$ by setting $A=a/(1+c)^{1/b}$ and $\lambda=c/(1+c)$. This is a shape-dependent PH normalization, not AFT-only APGW. Indeed, matching both early and late hazard exponents forces the same $b,c$; matching early coefficients gives $A=a c^{-1/b}$. Equality of late coefficients would then require $c^{1-c}=(1+c)^{1-c}$, which for $c>0$ holds only at $c=1$. Thus the two positive-shape three-parameter families meet at Weibull but are not reparametrizations of one another. The adapted $\kappa=0$ log-logistic member is also absent from finite ordinary PGW. The definitions and the PH/AFT distinction are from the cited primary source; these coefficient comparisons are derived here.

## Likelihood and a certificate route

Write $y_i=\log x_i$, $\mu=\log a$, $z_i=b(y_i-\mu)$, $t_i=e^{z_i}$, $L_i=\log(1+t_i)$, $q_i=t_i/(1+t_i)$, $W_i=e^{cL_i}$. The exact log likelihood is

$$\ell=n\log b+n\log c-\sum_i y_i+\sum_i z_i+(c-1)\sum_iL_i-\sum_i(W_i-1).$$

For fixed $a,b$,

$$\ell_c={n\over c}+\sum_iL_i(1-W_i),\qquad
\ell_{cc}=-{n\over c^2}-\sum_iL_i^2W_i<0.$$

The score tends to $+\infty$ at $c\downarrow0$ and to $-\infty$ at $c\to\infty$: its unique zero gives the exact inner solve. Profiling it leaves a smooth outer two-dimensional search in $(\mu,\log b)$. For stable implementation, calculate $L$, $W-1$ and the score in the log domain with `log1p`/`expm1` equivalents; underflow or a capped iterate is not a proof of a limit.

There is also a useful alternative **one-dimensional outer profile in $c$**. Define

$$Q_c(t)=1-q+cq(1-W),\qquad
{dQ_c\over d\log t}=-q(1-q)\{1+c(W-1)\}-c^2Wq^2<0.$$

The scores for $\mu,b,\log c$ are $-b\sum Q_c(t_i)$, $n/b+\sum(y_i-\mu)Q_c(t_i)$, and $n+c\sum L_i(1-W_i)$. For fixed $c$, let $v=-b\mu$. The log density of $Y=\log X$ is $\log b+\log g_c(by+v)$, where $d\log g_c(z)/dz=Q_c(e^z)$. Strict decrease of $Q_c$ proves strict log-concavity of $g_c$. The joint likelihood is strictly concave in $(b,v)$ for $b>0$: $n\log b$ is strictly concave and the log-density terms are concave. For nonconstant data it has a unique finite maximum. To see existence, $\log g_c(z)\le C-\delta|z|$ for some $C,\delta>0$, since the left log-tail is linear and the right tail decreases faster than exponentially in $z$. Then $\ell\le n\log b+C'-\delta\sum|by_i+v|$; the absolute-value sum is at least $b(y_{\max}-y_{\min})$. This excludes $b\to\infty$; boundedness of $g_c$ excludes $b\downarrow0$; and the same bound excludes $|v|\to\infty$ with $b$ bounded. Thus all fixed-$c$ nonconstant fits, including two-point data, exist uniquely. This does **not** prove that the resulting profile is unimodal in $c$.

A certificate can isolate all stationary points using the scalar $c$ profile, certified conditional $(b,v)$ inclusion, and upper bounds on the profile and its derivative; alternatively cover the two-dimensional $c$-profile. It must give uniform bounds on *all joint* boundary paths, compare all surviving interior roots and boundary envelopes, handle equal maxima and endpoint conventions, and enclose coefficients to `tol`, with work charged according to `dev/design/work-budgets.md`. Exact arithmetic conditional on comparisons with a stated floating-point margin is the project standard. This is a route, not an implemented or complete certificate. A successful numerical search whose certificate cannot close a sign, face comparison or budget returns its accurately solved result `certified = FALSE`; a proven better face, distinct tied maxima, proved nonexistence, or failed search follows the applicable diagnostic. A local score alone is not a global certificate, and no capped or unchecked estimate is a successful search.

## Proved boundary paths

These are limits of distributions and pointwise densities, not claims that a sample likelihood selects the face. Convergence of densities almost everywhere to the displayed proper densities implies total-variation convergence by Scheffé's lemma.

**Power-transformed Gompertz.** For fixed $b,s>0$, let $c\to\infty$, $a=s c^{1/b}$. Then $ct\to u=(x/s)^b$ and

$$H\to e^u-1,\qquad f(x)\to {b\over x}u e^u\exp\{1-e^u\}.$$

For $b=1$ this is the constrained `gompertz` member with rate and shape both $1/s$. An unrestricted `mlgompertz` maximization is **not** this face: it adds a degree of freedom. For other $b$ it is not a package Gompertz model on the original scale. This face remains available if $b$ is held but not if $a$ or $c$ is held.

**Lower-threshold conditional Weibull.** For $s,k>0$, let $a=s$, $b\to\infty$, $c=k/b$. For $x<s$, $H\to0$; for $x>s$,

$$H\to(x/s)^k-1,\qquad f(x)\to {k\over x}(x/s)^k\exp\{1-(x/s)^k\}.$$

This is a Weibull with shape $k$ and scale $s$ conditioned on exceeding its own scale $s$, not an unrestricted truncated Weibull. There is no matching package fitter identified. With $a=s$ exactly the limiting density at $x=s$ is $k/(2s)$; with $a_b=s\exp(-1/\sqrt b)$ it tends to the right-hand density $k/s$, and both paths approach the same law. Likelihood comparisons at a sample minimum must therefore use the *joint endpoint envelope*, not the arbitrary pointwise value of a weak limit. Holding $a$ fixes $s$; holding either $b$ or $c$ excludes this particular path.

Simple one-coordinate limits with the other two fixed are: $a\downarrow0$ or $c\to\infty$ gives a point mass at zero; $a\to\infty$ or $c\downarrow0$ gives escape to infinity; $b\to\infty$ gives a point mass at $a$; $b\downarrow0$ gives mass $1-e^{-(2^c-1)}$ at zero and the rest at infinity on the extended line. Joint paths may differ. These are not proper positive-density limit models and do not by themselves establish likelihood nonexistence.

**Open:** the two displayed proper faces are not proved exhaustive over all mixed-rate paths. A face law alone cannot establish a strictly winning likelihood, attainment, or a tie. Derive uniform remainder/envelope bounds, including concentration, non-tight and mixed-rate sequences, and compare them with every interior candidate. In particular no blanket stop or named-model instruction follows merely from a large coefficient.

## Constant data and all hold subsets

The held-set column lists the fixed coefficients. All statements assume valid finite positive holds. Unspecified global existence, uniqueness and face comparison are open, not structural exceptions to R2.

| Held | Result or reduction |
|---|---|
| $a,b,c$ | Evaluate the fully specified model; no optimization. |
| $a,b$ | Unique finite $c$ by strict concavity and the endpoint scores above, even for constant data. |
| $b,c$ | Unique finite $a$: $\sum Q_c(t_i)$ decreases strictly from $n$ to $-\infty$ as $a$ decreases, so its zero is bracketable. |
| $a,c$ | $\ell_{bb}=-n/b^2+\sum(y_i-\mu)^2\,dQ_c/d\log t_i<0$. The score starts at $+\infty$; it becomes negative unless all observations equal $a$. Hence one finite root except that all $x_i=a$ give $\ell=n\log b+O(1)\to\infty$. |
| $c$ | For nonconstant data, unique finite $(a,b)$ by strict concavity and coercivity above. For constant $x_0$, $a=x_0$, $b\to\infty$ makes $f(x_0)$ proportional to $b$ and the likelihood unbounded. The special hold $c=1$ is the ordinary Weibull fit. |
| $b$ | Profile $c$ and search over $a$, or use the unique conditional scale. The power-Gompertz face is available. Constant and nonconstant data: finite existence, global uniqueness and the boundary comparison remain open. A fixed-$b$ constant sample must not be declared unbounded merely by importing the exponentiated-Weibull result. |
| $a$ | Profile $c$ and search over $b$; the threshold face has fixed endpoint $a$. For constant $x_0=a$, fixed $c$ and $b\to\infty$ give an unbounded likelihood. For $x_0<a$, take $b\to\infty$ and $c=(a/x_0)^b$; then $f(x_0)/b\to e^{1-e}/x_0$, so the likelihood is unbounded. For $x_0>a$, as well as general nonconstant data, finite existence, face ordering and uniqueness are open. |
| none | Profile $c$ in $(a,b)$, or use the proved fixed-$c$ unique fit and profile $c$. Constant samples are unbounded by $a=x_0$, fixed $c$, $b\to\infty$. Nonconstant global existence, uniqueness and exhaustive face comparison are open. |

For $x_L<x_H$ with multiplicities $m$ and $h=n-m$, fixed $c$ already gives a unique finite fit. On the threshold face with free endpoint $s\le x_L$, the log-density envelope is

$$\ell_T(s,k)=n\log k-\sum_i\log x_i+\sum_i\{k\log(x_i/s)+1-(x_i/s)^k\}.$$

For fixed $k$, it increases with $s\le x_L$, since its derivative in $\log s$ is $k\sum_i[(x_i/s)^k-1]\ge0$. The maximal endpoint envelope is attained as a limit with $s\uparrow x_L$ and the endpoint-adjusted common path above. With $d=\log(x_H/x_L)$, its unique $k>0$ solves

$$ {n\over k}=h d\{e^{kd}-1\},$$

because the score is strictly decreasing from $+\infty$ to $-\infty$. This proves the threshold face's own finite optimum, not its ordering against the interior or power-Gompertz face, or exclusion of other boundary paths. With $a$ held, use the fixed $s=a$ envelope only if $a\le x_L$; the $s\uparrow x_L$ optimization is unavailable unless the held value equals $x_L$. All-free and other two-point global existence, uniqueness and possible unbounded mixed paths remain open.

## Interior inference

At every finite $a,b,c>0$, including $c=1$, the support is fixed and the density is positive and smooth in log-coordinates. In the $t$ variable the density is $c(1+t)^{c-1}\exp\{1-(1+t)^c\}$, bounded near zero and super-polynomially suppressed at infinity. On a sufficiently small compact parameter neighbourhood, the scores and their local derivatives have an integrable envelope: only logarithmic growth appears at zero, while exponential tail suppression dominates powers and logarithms at infinity. This gives DQM and finite information.

The information is nonsingular. As $t\downarrow0$, the log-$b$ score diverges like $\log t$, while the log-$a$ and log-$c$ scores tend to finite nonzero constants; its coefficient in any almost-sure zero linear combination must vanish. As $t\to\infty$, the latter two scores have different leading orders $bc t^c$ and $-c t^c\log t$, respectively, so their coefficients also vanish. Thus the full information, and every principal free-subset information matrix, is positive definite. The Weibull submodel $c=1$ is an ordinary interior one-dimensional restriction, not a singular seam. These results do not prove existence or consistency of a selected global MLE, root-$n$ convergence, likelihood-ratio calibration or bootstrap validity. Face/point-mass regimes need separate analysis. No PGW derivative, covariance or inference implementation has been validated; `inference_numeric` should initially be unavailable until that work is done.

## Dossier candidates

The following is proposed metadata, not an implemented detector. `holds: []` expresses the R2 target; sample-dependent failures belong in `limits`, not structural holds. Status identifiers are provisional; limit entries should be installed only along with their stated detector and endpoint handling. `firmness: proved` grades the limit path, not the unimplemented global comparison.

```yaml
limits:
  - name: unbounded likelihood
    kind: nonexistence
    firmness: proved
    when: 'Scale equals the common observation, exponent is fixed and shape tends to infinity.'
    detect: 'All observations are equal and scale and shape are free.'
    status: Unbounded
    message: unbounded
  - name: unbounded likelihood
    kind: nonexistence
    firmness: proved
    fixed: [scale]
    when: 'For a common observation below held scale, shape tends to infinity and exponent equals (scale/x0)^shape; equality with held scale is also unbounded.'
    detect: 'All observations equal x0 at or below held scale, and shape and exponent are free.'
    status: Unbounded
    message: unbounded
  - name: power-transformed Gompertz boundary
    firmness: proved
    when: 'Exponent tends to infinity and scale / exponent^(1/shape) tends to a positive constant with shape fixed.'
    detect: 'A completed global certificate proves the unattained face strictly wins after all interior and boundary comparisons.'
    status: PowerGompertzBoundary
  - name: lower-threshold conditional Weibull boundary
    firmness: proved
    when: 'Shape tends to infinity, shape times exponent tends to a positive constant, and scale tends to a positive threshold.'
    detect: 'A completed global certificate proves the unattained endpoint-envelope face strictly wins after all interior and boundary comparisons.'
    status: ThresholdBoundary
holds: []
```

The first unbounded entry requires only free scale and shape; exponent may be held or free. The second is a sufficient condition, not a complete held-scale classification. Neither boundary has an exact unrestricted package delegate: the $b=1$ power-Gompertz face is a constrained Gompertz submodel. Do not point to `mlgompertz` as though its unrestricted maximum were the face maximum. No modification to settled `dpqr.hpp`, `special/`, truth data, code generation, or generated d/p/q/r was made. A subsequent d/p/q/r card must separately implement and validate stable `expm1(c*log1p(t))`, inverse operations and extreme tails.

## Numerical evidence and reproducibility

[check.py](2026-11-11-pgw-theory-data/check.py) and [check.json](2026-11-11-pgw-theory-data/check.json) use 100-digit mpmath arithmetic for the conditional curvature, pointwise power-Gompertz and threshold limits, the held-scale concentration path, and the two-point threshold score. The power-Gompertz log-density error falls from about $0.253$ at $c=10$ to $0.000315$ at $c=10000$; the threshold error at $b=100$ is about $7.5\times10^{-16}$. These check formulas, not global existence. [profiles.py](2026-11-11-pgw-theory-data/profiles.py) and [profiles.json](2026-11-11-pgw-theory-data/profiles.json) give finite-box feasible log-likelihood lower bounds for ordinary and adapted models on spread, skew and two-point samples. Several searches touch the box edge. They return no fitted estimates and do not certify any global ordering or boundary winner.

Reproduce each script from the repository root with, for example, `TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-11-pgw-theory-data/check.py`; substitute `profiles.py` for the second. Both ran on the workstation under the slot lock. The local searches use SciPy and are illustrations, not production algorithms.

## Verification and outstanding issues

- Simbox GCC 13: `just cpp-test` passed 108/108.
- Workstation GCC 15: `/tmp/umvl-cpu/run4 just cpp-test-no-long-double` passed 135/135.
- Simbox GCC 13: `just cpp-test-no-long-double` failed 1 of 135 shards, `distribution_bnbinom`. The existing test at `bnbinom/test.cpp:373` requires exact equality of swapped-held log likelihoods; three assertions differed below printed precision. A targeted repeat reproduced the same failure. The corresponding GCC 15 shard passed. No fitter or test was changed; this unrelated cross-compiler discrepancy prevents claiming all requested suites green.
- Simbox: `just test` passed `[ FAIL 0 | WARN 0 | SKIP 154 | PASS 23231 ]`.
- The repository link checker reports 47 existing broken destinations in other files and none in this report.
- No fitting, d/p/q/r, binding, test infrastructure, or dossier changed; the task's conditional full R, JS and site checks were not triggered. The suites validate the existing repository, not the new mathematical claims.

For the later fitter card, close or explicitly bound the mixed-rate boundary classification and the unproved held-subset/global comparisons; implement and test the conditional solves and root/face cover, coefficient inclusion, budget accounting, endpoint envelope and uncertified fallback; and validate derivatives and inference separately. These are open mathematical and implementation questions, not evidence that a finite MLE exists or fails for every unresolved sample.
