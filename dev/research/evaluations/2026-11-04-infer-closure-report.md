# Local experiments at closure points — TASK-102.12

2026-11-04. Source: `a3fbb01383ab1ec6065c43f7fade40b1bd2751a4`, branch
`codex/infer-closure`. Research only. All pre-existing tracked files are
unchanged. The additions are this report and its [small evidence directory](2026-11-04-infer-closure-data/README.md).
All research computations and final regression suites finished locally before this
report was written. No commit, board edit, sub-agent, dependency installation,
simbox run or publication was used.

## Findings

The ordinary seams have Gaussian half-line experiments, with a
$\tfrac12\chi_0^2+\tfrac12\chi_1^2$ limit for testing the limit family
against its one-sided enlargement. The beta-binomial Poisson corner has a
**Gaussian half-plane**, giving $\tfrac12\chi_1^2+\tfrac12\chi_2^2$.
Burr's Pareto face has a **Poisson point-process endpoint experiment** and an
independent Gaussian log-tail-shape factor. Its endpoint and width rates are
$n^{-1}$; its likelihood-ratio limit is a point-process supremum, with no
Gaussian chi-bar-square reduction established here.

These statements concern correctly specified iid sampling, positive
nondegenerate limit parameters, and the specified free subsets. They are
derived below and in the linked proofs, including localization of the
mathematical likelihood suprema. They do not certify the production fitters
or provide a new inference API. All likelihoods are total sums; Fisher
information quoted below is per observation. A likelihood ratio against a
limit law means the limit submodel is admitted in the closure.

| Limit and hypothesis | Local transverse coordinates | Experiment after face nuisance is removed | Null LR limit |
|---|---|---|---|
| Exponential versus Lomax; scale fitted | $\xi=1/a=h/\sqrt n\ge0$ | Gaussian ray; efficient information 1 | $\frac12\chi_0^2+\frac12\chi_1^2$ |
| Weibull versus Burr; Weibull shape and scale fitted | $\xi=1/k_B=h/\sqrt n\ge0$ | Gaussian ray; efficient information $1-6/\pi^2$ | Same mixture |
| Poisson versus NB, or versus unknown-size binomial, separately; mean known or fitted | $1/a\ge0$, or $\kappa=1/N\ge0$, of order $n^{-1/2}$ | Gaussian ray; information $\mu^2/2$, opposite signed scores | Same mixture |
| Binomial versus BB; finite size $N\ge2$, probability fitted | $\rho=1/(a+b+1)=h/\sqrt n\ge0$ | Gaussian ray; information $N(N-1)/2$ | Same mixture |
| NB versus BB; positive finite NB shape and mean fitted | $\kappa=1/N=h/\sqrt n\ge0$ | Gaussian ray with projected information strictly positive | Same mixture |
| Poisson versus BB with size and both shapes free; mean fitted | Effective $d=O(n^{-1/2})$, $k^2=O(n^{-1/2})$ | Gaussian $\mathbb R\times[0,\infty)$ | $\frac12\chi_1^2+\frac12\chi_2^2$ |
| Pareto versus Burr; endpoint and log-tail shape fitted | Endpoint shift and width $O(n^{-1})$ | Logistic-intensity PPP | $T_\Pi$, defined below |
| Pareto versus Burr with endpoint held; shape known or fitted | Width $O(n^{-1})$ | Restricted PPP; alternative has singular support mass | Exactly zero under the null; support violations reject |

The rates in this table name particular functionals of a law. Hellinger
distance itself is of order $n^{-1/2}$ at distinguishability in every row.
Holding a transformed face coordinate need not mean holding the original
parameter: the Weibull face scale is $\lambda=s k_B^{-1/c}$. A held positive
binomial success probability with $N\to\infty$ is not the positive-fixed-mean
Poisson regime considered here. BB at $N=1$ is Bernoulli with concentration
unidentified; $N=0$ is degenerate. Mean/probability endpoints require separate
experiments.

## Gaussian seams and the integer-size interpretation

The [Gaussian derivation](2026-11-04-infer-closure-data/gaussian-experiments.md)
gives the scores, DQM tail arguments, nuisance projections and localization.
For Lomax at exponential, with $z=x/\sigma$,

$$s_\xi=z^2/2-z,\qquad s_{\log\sigma}=z-1,\qquad
I=\begin{pmatrix}2&1\\1&1\end{pmatrix}.$$

Thus known scale gives information 2; fitting scale gives 1. Burr's Weibull
seam has the same transverse score, but estimating the additional Weibull
shape reduces efficient information to $1-6/\pi^2=0.3920728981$.
Ignoring that nuisance would give the wrong scale for inference.

At Poisson, write
$C_2=x(x-1)-2\mu x+\mu^2$. NB and binomial have scores $C_2/2$ and
$-C_2/2$, respectively. Both are orthogonal to the Poisson mean score.
Testing against their **union** gives a Gaussian line and $\chi_1^2$,
rather than the one-sided mixture in either separate test.

For the BB-to-NB seam, put $b=a(N/\mu-1)$, $\kappa=1/N$. The new
one-sided DQM argument uses the score

$$s_\kappa(x)=\frac{-a x(x-1)+2\mu(a+1)x-(a+1)\mu^2}{2(a+\mu)}.$$

Its information remains positive after projecting both fitted NB nuisance
scores. The proof supplies a uniform exponential-moment bound and a
logarithmic-count Taylor split, addressing the gap recorded in
[the boundary literature report](2026-11-03-boundary-lit-report.md).
It proves one-sided differentiability along actual integer sizes, not a
proper open chart of noninteger binomials or a two-sided BB/BNB extension.

At $N\asymp\sqrt n$, adjacent $1/N$ values differ by $O(n^{-1})$, so the
integer mesh disappears at the root-n dispersion scale. The approximate size
standard error is $N^2/\sqrt{nI_e}$. Consequently:

- $N\gg\sqrt n$ is indistinguishable from the Poisson limit at this resolution.
- Relative size error is of order $N/\sqrt n$.
- Adjacent sizes have separation of order $\sqrt n/N^2$: individual-size
  inference retains a lattice at $N\asymp n^{1/4}$, although the Poisson
  detection scale is $N\asymp\sqrt n$.

This derives a mechanism for the instability described in the verified
abstract of [Olkin, Petkau and Zidek (1981), JASA 76, 637–642](https://doi.org/10.1080/01621459.1981.10477697).
Their full text was inaccessible; the rate calculation is not attributed to
an unread theorem. An interval for $1/N$ containing zero must map to an
unbounded size confidence set. Truncating that set at a search cap would
erase the principal uncertainty.

## The BB Poisson corner

Set $u=1/a$, $k=1/N$, $b=a(N/\mu-1)$ and

$$d=u-k-(\mu+1)k^2,\quad
A=\sqrt n\,\mu d/\sqrt2,\quad
B=\sqrt n\sqrt{2\mu^3/3}\,k^2\ge0.$$

Let $C_3=(x)_{\underline3}-3\mu(x)_{\underline2}+3\mu^2x-\mu^3$.
The effective orthonormal scores are $e_2=C_2/\sqrt{2\mu^2}$ and
$e_3=-C_3/\sqrt{6\mu^3}$. For bounded effective coordinates,

$$\ell_n(A,B)-\ell_n(0,0)
 =A Z_{2n}+B Z_{3n}-\tfrac12(A^2+B^2)+o_P(1),\qquad
(Z_{2n},Z_{3n})\Rightarrow N_2(0,I_2).$$

The mean score is orthogonal to both, so fitting the mean does not remove
the cubic direction. Every bounded $(A,B)$ with $B\ge0$ is approached by
actual integer sizes and positive shapes: around $N\asymp n^{1/4}$ the
mesh of $k^2$ is $O(n^{-3/4})$, negligible at its $n^{-1/2}$ scale.
Thus the LR is

$$T\Rightarrow Z_2^2+\max(0,Z_3)^2
 \sim\tfrac12\chi_1^2+\tfrac12\chi_2^2.$$

The two collinear coordinate scores therefore do **not** imply a
one-dimensional experiment, nor a quadrant of independent boundary scores.
The cancelling paths have $u,k\asymp n^{-1/4}$, while $d$ and mean remain
root-n coordinates. On the edge of the limiting projection the next rate
and the representation by finite or infinite shapes are not determined.
Its limiting edge mass is 1/2; that does not by itself prove an exact
finite-sample face-selection probability.

The [global localization proof](2026-11-04-infer-closure-data/bb-localization.md)
first bounds every likelihood-competitive law by empirical KL, obtaining
$H^2=O_P(\log n/n)$. The existing
[BB closure theorem](2026-11-03-bbinom-closure-report.md) then forces the
corner. Hellinger geometry gives an initial parameter rate, permitting a
uniform fourth-order log-pmf expansion; quadratic coercivity removes the
logarithmic slack. This proves the limit for the unrestricted mathematical
supremum, not just an arbitrarily restricted neighborhood. The
[exact symbolic check](2026-11-04-infer-closure-data/symbolic.json) verifies
normalization through fourth order and the drift
$-\mu^2d^2/4-\mu^3k^4/3$. Tail and localization arguments, rather than
symbolic algebra alone, justify taking a likelihood supremum.

## Burr's Pareto face: an endpoint experiment

On log data $Y=\log X$, write $\alpha=k_Bc$, $b=\log s$,
$w=k_B/\alpha=1/c$. The Pareto face is the shifted exponential
$g_{\alpha,b}(y)=\alpha e^{-\alpha(y-b)}1_{y\ge b}$.
The [endpoint derivation](2026-11-04-infer-closure-data/pareto-experiment.md)
proves, for $b=b_0+aw$, $\alpha=\alpha_0$,

$$H^2(p,g)=k_BJ(a)+o(k_B),\quad
J(a)=a-4\log2+4\operatorname{atanh}\sqrt{(1+e^a)^{-1}},$$
$$\min_aJ(a)=\log(27/16)>0,\qquad a_{\min}=\log3.$$

Here $H^2=\int(\sqrt p-\sqrt g)^2$, without a factor 1/2. Matching the
Pareto endpoint and shape cannot remove the order-$k_B$ transverse squared
distance. Hence $k_B\asymp1/n$ is the width scale, with $c\asymp n$.
There is no nonzero transverse strong L2 derivative at this speed: root mass
concentrates at the endpoint. Slowing a parametrization can give a zero
derivative, which would not create a regular transverse extension.

Set

$$\alpha_n=\alpha_0(1+h/\sqrt n),\quad
b_n=b_0+t/(n\alpha_0),\quad w_n=v/(n\alpha_0),\quad v\ge0.$$

The rescaled endpoint observations tend to a PPP on the whole real line
with intensity $\lambda_{t,v}(z)=(1+e^{-(z-t)/v})^{-1}$; at $v=0$ it is
$1_{z\ge t}$. Independently, the log-tail shape gives a unit Gaussian shift
in h. Under the null let $\Pi$ have unit intensity on $[0,\infty)$ and first
point $Z_{(1)}$. On null-supported configurations the log likelihood is

$$\Lambda(t,v)=t-\sum_{z\in\Pi}\log(1+e^{(t-z)/v}),$$

with $\Lambda(t,0)=t$ for $t\le Z_{(1)}$ and $-\infty$ otherwise. This
is the absolutely continuous part of the alternative: its likelihood has
expectation $\exp\{-v\log(1+e^{-t/v})\}<1$. The missing mass is the
alternative probability of observing a negative point. Mutual contiguity
and Gaussian LAN therefore fail.

With endpoint and shape fitted under both hypotheses,

$$T_\Pi=2\left\{\sup_{t\in\mathbb R,v\ge0}\Lambda(t,v)-Z_{(1)}\right\}$$

is the LR limit. It is finite almost surely. The
[global Burr proof](2026-11-04-infer-closure-data/burr-localization.md)
uses the existing density envelope for consistency, then exponential
order-statistic spacings to exclude widths larger than $1/n$ and distant
endpoints. This supplies the localization missing from a fixed-sample slope
argument. Ordinary shape profiling cancels the independent Gaussian factor.

For $0<v\le1$ the first point alone prevents improvement over the fitted
sharp endpoint. Nevertheless $P(T_\Pi>0)\ge(5/6)e^{-8}>0$: a sufficiently
large gap after the first point allows a fixed feasible width to improve.
Thus the fixed-sample result $H(k_B)=P+nk_B\log k_B+O(k_B)<P$ near zero
does not imply that the probability of a Pareto optimum tends to one.
Its remainder is not uniform in sample size at $k_B=v/n$.

If the endpoint is held instead, the conclusion changes sharply. On its
support, every Burr density is pointwise below the Pareto density at the
same alpha. The closure LR is exactly zero for every null sample, including
with shape fitted. A support violation makes the null likelihood zero;
the corresponding support test has limiting power $1-2^{-v}$ at $t=0$.
This illustrates why a boundary law alone does not specify an inference
problem.

## Confidence sets, uniformity and classification

In a whitened Gaussian seam observe $Y\sim N(h,1)$, $h\ge0$. The LR for a
candidate h is
$W_h=(Y-h)^2-\{Y-\max(Y,0)\}^2$. Its 95% critical value changes from
2.705543 at h=0 toward 3.841459 in the interior. Applying the boundary
cutoff to every candidate gives coverage tending to **90%** in the
interior. A chi-square_1 cutoff is conservative throughout this Gaussian
experiment, with 97.5% boundary coverage; candidate-specific inversion
is another option. These conclusions transfer on bounded local sets and
compact regular nuisance sets under the uniform likelihood bounds proved
above. Pointwise interior Wald normality does not supply that uniformity.

At the BB corner, invert the **bivariate** cone experiment and project through
the actual integer model. Joint chi-square_2 cutoffs are conservative in
that Gaussian experiment; corner-specific critical values change when B
moves into its interior. Separate Wald intervals discard the cubic direction
and the dependence between u and k. Seam approximations with fixed NB shape
are not uniform as that shape tends to infinity into the corner.

At Burr's Pareto face, endpoint and width confidence sets require inversion
of the candidate-indexed PPP experiment. They have n^-1 scales and generally
asymmetric boundary behavior. Shape has its separate root-n Gaussian
experiment; ordinary shape inference can survive even though joint Wald
inference fails. The Pareto shape pivot is exact on the face and has the
same first-order shape coverage on bounded n*k_B local sets; this does not
claim uniform validity over wider interior approaches. Bootstrap sampling
capability does not prove calibration: even the constrained Gaussian mean
has a random local fitted parameter in an ordinary n-out-of-n bootstrap.

Local experiments are parametrization-free when indexed by laws and
specified relative to a limit submodel and hypothesis. They provide useful
invariant descriptions here: Gaussian ray, Gaussian half-plane, and
Gaussian-times-PPP. They do **not** produce an exhaustive geometric
seam/corner/cusp taxonomy, or determine exact face-selection probabilities.
The [classification argument](2026-11-04-infer-closure-data/classification.md)
proves a stronger obstruction: two three-atom models have equivalent first
local experiments, yet their exact null face-selection probabilities tend
to 1 and 1/2. The difference is a positive likelihood gain of order 1/n,
below first-order detectability. A hierarchy of uniform likelihood
expansions is needed for that question. BB's half-plane experiment also
coincides with a smooth Gaussian half-plane model despite different local
topology. No new universal definition of “cusp” is justified.

## Numerical evidence

[Results and versions](2026-11-04-infer-closure-data/results.json) are generated
by [the independent reproducer](2026-11-04-infer-closure-data/run.sh).
Seed: 1021220261104; the Gaussian confidence illustration uses seed+1.
No production fitter or optimizer is used. The numerical results corroborate
the derivations; they do not replace the proofs.

| Check | Workload and result |
|---|---|
| Poisson orthogonality and BB fourth order | Exact SymPy normalization through degree four; $EC_2^2=2\mu^2$, $EC_3^2=6\mu^3$, $EC_2C_3=0$ |
| BB/NB transverse derivative | At a=2, mu=3, weighted RMS errors for N=1,000/10,000/100,000: 0.0247 / 0.002443 / 0.000244; fitted-nuisance information 1.48298 |
| Burr/Weibull nuisance information | Independent quadrature: 0.392072898148, agreeing with $1-6/\pi^2$ |
| BB corner likelihood versus LAN | 10,000 null samples per n, mu=3; n=1,000 / 16,000 / 256,000 / 4,096,000; RMS log-likelihood errors 0.609 / 0.251 / 0.121 / 0.0598 |
| Corner score-surrogate calibration | Mixture 95% cutoff 5.13838 gives rejection 4.40 / 4.65 / 4.99 / 4.76%; wrong chi-square_1 cutoff gives 9.42 / 9.68 / 9.74 / 9.38% |
| Lomax efficient-score approximation | 6,000 exponential samples each at n=500 and 8,000; rejection 5.82% and 5.58%; finite-sample error remains visible (MC SE about 0.28 percentage points) |
| Pareto Hellinger speed | At k_B=0.01 / 0.001 / 0.0001, H^2/k_B=0.751196 / 0.752734 / 0.752888; limit at aligned endpoints 0.752906; independently checked minimum log(27/16) |
| PPP likelihood and singular mass | 20,000 processes: mean likelihood 0.38450 versus derived 0.38746 at t=1,v=2; finite-n log-likelihood corrections decrease from 0.00994 to 0.0000387 |
| PPP finite-grid reconnaissance | Specified 21-by-5 grid improves over the sharp endpoint on 2.36% of draws; truncated-tail expected uniform error bound about 9.4e-10; this is not a global LR quantile or a proof of its atom size |
| Feasible finite Burr/Pareto comparison | Retained 1,000-observation Pareto log sample; t=4,v=2 gives gain 0.1102564431 above the fitted Pareto law, independently replayed at 55 digits |

The corner calibration is for the explicit score statistic supplied by the
limit experiment, not a replay of an optimized finite-sample LR. Histogram
simulation represents iid Poisson samples; the tail above count 60 is below
1e-50 per observation. One of 10,000 smallest-n likelihood comparisons falls
outside the chosen BB candidate's support and is explicitly counted and
excluded from its finite-error RMS; none is hidden or repaired. Later cells
have no such failures. The finite-n Burr process checks retain 80 order
statistics with deterministic omitted-sum bounds, at worst 1.64e-6 in the
recorded workloads. The PPP grid has a probabilistic tail bound, not a
pathwise enclosure of its global supremum. The full-data feasible witness
uses all observations; 55-digit agreement is not an interval-arithmetic
certificate or an optimizer estimate.

## Literature and proposed replacement text

The [literature record](2026-11-04-infer-closure-data/literature.md) separates
identity, located reading and access limits. The inspected
[Self–Liang (1987), pp. 605–608, Cases 5–6](https://www.stat.cmu.edu/~brian/763-2013/week06/papers/self-liang-1987.pdf)
supports Gaussian cone projection when its regularity assumptions hold.
[Drton (2009), Theorem 2.6 and Remark 2.8](https://arxiv.org/pdf/math/0703360)
requires a regular ambient experiment, set regularity and consistency;
it cannot simply be invoked on BB's singular coordinate scores. The proofs
here supply the relevant experiments and localization directly. Verified
primary metadata/abstracts also cover Smith (1985), Andrews (2000),
Dacunha-Castelle–Gassiat (1997), and the two distinct Shao (2004) papers.
No priority or exhaustive coverage claim is made for the BB or Burr results.

Suggested text for `boundary-points.tex`, which is unchanged:

> Closure inference should be stated for a model–face pair and a hypothesis.
> At the positive-mean NB/binomial–Poisson and the Lomax/Burr–exponential/
> Weibull seams, the transverse local experiment is a Gaussian ray after
> nuisance projection. The full beta-binomial Poisson corner instead has a
> Gaussian half-plane experiment: a second-order cubic count direction
> survives cancellation, with n^-1/4 shape/size coordinates and a
> half-chi-square_1 plus half-chi-square_2 likelihood-ratio limit. Burr's
> Pareto face has an endpoint Poisson-process experiment at width n^-1,
> independent of a regular Gaussian tail-shape experiment. Its fitted-endpoint
> LR is a point-process supremum, while a held endpoint gives a support test.

> These experiments are invariant under relabeling the laws, but do not
> define an exhaustive geometric trichotomy or imply exact face-selection
> probabilities. Such probabilities can differ between models with the same
> first local experiment because smaller likelihood improvements disappear
> from its limit. Fixed-sample inward slopes, likelihood localization and
> uniform confidence calibration must therefore remain distinct statements.

## Open work, precisely scoped

1. **Certified numerical calibration of T_Pi.** Coercivity and the process
   formula are proved, and feasible-point/grid checks were run. A critical
   value needs a search with controlled compact-domain error, residual PPP
   tail error, and Monte Carlo quantile uncertainty. The grid does not supply
   those; no capped maximizer or estimated global critical value is returned.
2. **Finite-sample confidence procedures.** Candidate-wise inversion is
   identified mathematically, with Gaussian conservativeness and bounded-local
   uniformity established. Validated finite-sample implementations, coverage
   studies for PPP endpoint/width sets, and adaptation over unbounded
   interior-to-face or seam-to-corner transition ranges remain to be supplied.
   The observed finite-sample Lomax score error argues against advertising
   blanket calibration. No misspecification-robust validity is claimed.
3. **Exact face-selection laws and classification beyond first order.** The
   toy counterexample proves first local experiments insufficient; the exact
   Burr atom probability and BB finite-MLE/face-selection probabilities need
   separate finer analysis. Neither limiting projection mass nor a finite
   grid settles those questions. An exhaustive higher-order invariant
   taxonomy and several cited inaccessible full texts remain open.

These gaps do not change the derived local experiments, rates or LR
supremum limits. They delimit which confidence procedures and exact
selection claims can currently be justified. No settled d/p/q/r, special
function, truth fixture, code generator, default fit or golden needs a change
for this research result. The canonical
[inference plan](../../roadmap/2026-10-30-inference-and-dqm.md) and
[seams discussion](../../roadmap/2026-11-02-ml-difficulties-paper.md)
remain the coordination references.

## Final validation

All runs were local, foreground commands through the shared slot locks;
the runners applied nice priority and one-thread BLAS/OpenMP limits. No
process was detached or left running. Environment: Ubuntu 26.04.1,
Linux 7.0.0-38-generic, GCC 15.2.0, R 4.6.1, BH 1.90.0-1,
Python 3.14.5, NumPy 2.4.6, SciPy 1.18.0, SymPy 1.14.0, mpmath 1.3.0.
CMake used the documented BH fallback because system Boost was absent;
it also emitted the existing FindBoost policy warning. Test outcomes below
are distinct from those configuration messages.

After creating `build/tmp` and setting TMPDIR, JUST_TEMPDIR and
XDG_RUNTIME_DIR to that absolute worktree path:

| Command | Result |
|---|---|
| `/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-04-infer-closure-data/run.sh` | Exact algebra and all numerical assertions passed on the final scripts |
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 154, PASS 20612 |
| `/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test` | 127/127 full shards passed |
| `/tmp/umvl-cpu/run4 just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43427 |

All 154 dev skips explicitly required the full tier; there were no permitted
failures. Counts describe this source revision, rather than the older baseline
in the brief. Regression suites validate the unchanged implementation on
their workloads, not the mathematical theorems or a new inference procedure.
Captured logs are under `build/scratch/infer-closure-*`; lasting derivations,
scripts and small results are linked above. No goldens or truth fixtures were
regenerated. The final source-diff, whitespace and local-link checks are
recorded in the [validation record](2026-11-04-infer-closure-data/validation.json).

The requested artifact-pane display was attempted. The viewer returned HTTP
403 because this worktree is outside its configured readable roots. The
report remains at the requested path; no viewer configuration or destination
outside this worktree was changed.
