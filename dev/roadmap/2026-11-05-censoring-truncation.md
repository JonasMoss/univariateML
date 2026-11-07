# Censoring and truncation (2026-11-05)

Status: idea, not scheduled. Natural next step once the current
migration (TASK-101) and certification work are done. Jonas's interest
is mainly the curiosities: how limit models and existence change, not
the plumbing.

## Likelihood pieces

Observation types: exact $x$ (log f), right-censored $u$ (log S),
left-censored (log F), interval $(a, b]$ (log(S(a) - S(b))), and
truncation to a fixed set $A$ (subtract $n \log P(A)$). All pieces
already exist in the tail-accurate `dpqr.hpp` functions; a generic
fitter over them is plumbing.

## Burr XII as the worked example

With $z = c \log(x/s)$ and $\mathrm{sp} = $ softplus,
$\log S = -k\,\mathrm{sp}(z)$ is linear in $k$, which is what lets
`mle.hpp` profile $k$ exactly.

- Right censoring: profile survives, $\hat k = d / \sum_{\text{all}} \mathrm{sp}(z)$
  ($d$ = number of exact observations).
- Left truncation at fixed $t$: profile survives,
  $\hat k = n / \sum_i [\mathrm{sp}(z_i) - \mathrm{sp}(z_t)]$; compute the
  differences as $\log((1+y_i)/(1+y_t))$.
- Left/interval censoring, right truncation: $\log F = \mathrm{log1mexp}(k\,\mathrm{sp}(z))$
  is not linear in $k$; the profile and the 2-D certificate structure are lost.

## Closure under an observation map

Let $T$ map the law of $X$ to the law of what is observed. Censoring is
a Markov kernel, so $H(TP, TQ) \le H(P, Q)$ and
$T(\overline{M}) \subseteq \overline{T(M)}$. The extra boundary

$$\partial_T M = \overline{T(M)} \setminus T(\overline{M})$$

comes only from sequences in $M$ with no Hellinger-convergent
subsequence (mass escaping to $0$, $\infty$ or onto a point) whose
images converge. Truncation $T_A P = P(\cdot \cap A)/P(A)$ is not a
kernel but is continuous where $P(A) > 0$; the same argument applies.

Findings to check properly (from chat on 2026-11-05, not proved):

1. Burr, fixed left truncation: the limit set looks unchanged. The
   $s \to 0$ route gives Pareto with minimum $t$, which is
   $T_t(\text{Pareto}(\alpha, m))$ for $m \le t$, already in
   $T(\overline{M})$, and it is not the face optimum ($m = x_{(1)}$ beats it).
   What changes is the routes: truncation breaks scale invariance, so
   the Pareto face is reachable at finite $c$. Detection and certificate
   seams change even though the closure does not.
2. Burr, fixed right censoring at $u$: adds the all-censored law (atom
   at $u$, mass 1), reached as $s \to \infty$. Hellinger sees atoms, so
   this point is new; it is the $d = 0$ case.
3. Burr, interval censoring on fixed bins: the image lives in a simplex.
   With $c \to \infty$ and $c \log(b/s) \to \rho$, Burr puts mass
   $1 - (1+e^\rho)^{-k}$ just below a bin edge $b$ and the rest just
   above. So every law on two adjacent bins is in $\partial_T M$. Data in
   at most two adjacent bins drive the MLE to this face, with a
   *bounded* likelihood (the empirical multinomial), where uncensored
   all-equal data gave an unbounded one.
4. Burr, right censoring, all exact observations equal to $x_0$ and
   $m \ge 1$ censored at times $u_j > x_0$: the likelihood is bounded
   (it is unbounded without censoring). Since $h(x) = (k c/x)\,\sigma(z)$
   and softplus is convex, with $A = k c\,\sigma(z_0)$,
   $\log f(x_0) \le \log(A/x_0)$ and $\log S(u_j) \le -A \log(u_j/x_0)$.
   The bound is attained only on the Pareto face with minimum $x_0$,
   $\alpha^* = d / \sum_j \log(u_j/x_0)$. Censoring beyond the cluster turns a
   point-mass escape into a Pareto escape.
   Caveat: in uncensored Burr any distinct exact observation already
   bounds the likelihood (the unbounded case is all-equal only), so the
   censored point acts like one. The asymmetry is the new part: a
   censoring time below $x_0$ does not help, an exact point below does.
   Closure view: censored factors are probabilities of closed sets, so
   they are weakly upper semicontinuous functions of the law; at
   $\delta_{x_0}$ they vanish, exact factors diverge, and the race needs
   rates as in the collapse exponent of `dev/research/ml-behaviour.tex`.
   For data censored into fixed bins the likelihood is a continuous
   function of the observed law on a compact simplex, so the envelope
   $L_x^*$ of the likelihood-closure lemma is the likelihood of the
   limit law and the supremum is always attained in $\overline{T(M)}$.
5. Weak closure: the censoring map $x \mapsto (\min(x,u), 1\{x \le u\})$
   is discontinuous at $u$, so concentration at $u$ gives the weak-only
   limits $(1-p)\,\delta_{(u,\text{exact})} + p\,\delta_{(u,\text{cens})}$,
   $p \in [0,1]$. In contrast, the all-censored law (finding 2) and the
   two-bin edge (finding 3) are Hellinger limits in the observed space.

General question: characterise $\partial_T M$ for each family and each
censoring/truncation scheme, and which faces of $\partial_T M$ attract
the MLE on which samples.

## Grouped data (income brackets), added 2026-11-07

Fixed bins $b_0 < \dots < b_J$ with counts $n_j$: the likelihood is
multinomial, $\sum_j n_j \log p_j(\theta)$ with
$p_j = F(b_j) - F(b_{j-1})$, which equals $-n\,\mathrm{KL}(\hat p \,\|\, p(\theta))$
up to a constant. The MLE is a KL projection of $\hat p$ onto
$\overline{T(M)}$ in the simplex.

- Always attained in the closure (finding 3); the log likelihood is at
  most $0$, so unbounded likelihoods disappear. Escapes go to faces of
  the simplex (mass in one bin, or split across adjacent bins).
- Identifiability needs $J - 1 \ge p$. With $J - 1 = p$ the fit is exact
  when $\hat p$ lies in the image of the bin map and on its boundary
  otherwise: existence becomes a question about the shape of that image.
- Threshold coefficients (Pareto minimum, uniform endpoints) enter
  through $F(b_j)$ smoothly, except where the threshold crosses a bin edge.
  Binning should make these fits regular ($\sqrt n$) when the true
  threshold lies inside a bin. Unchecked.
- An open top bracket $[b_{J-1}, \infty)$ carries the tail through a single
  probability. Fixed bins also break scale invariance, as truncation does.
- Computation: cost $O(J)$ per evaluation, independent of $n$; $\log p_j$
  from the tail-accurate log CDF or log survival, whichever is smaller,
  with `log1mexp`; multinomial bootstrap; $G^2$ goodness of fit on
  $J - 1 - p$ degrees of freedom. Burr's exact $k$-profile is lost.

Bracket means (tax tables: count and total per bracket).

- The Burr partial moment is $s k\,B_t(1 + 1/c,\ k - 1/c)$ with
  $t = y/(1+y)$, $y = (b/s)^c$ (unregularized incomplete beta, from
  $Y/(1+Y) \sim \mathrm{Beta}(1, k)$). The top-bracket mean is finite only if
  $kc > 1$, so reporting it imposes a tail restriction. For a Pareto top
  bracket the mean $m$ alone gives $\alpha = m/(m - b_{J-1})$ (classical Pareto
  interpolation).
- The mean is only weakly lower semicontinuous ($\mathbb E_Q X \le \liminf \mathbb E_{P_n} X$),
  so in (share, mean) coordinates the closure can contain points whose
  top mean exceeds that of the limit law: vanishing mass at infinity
  carrying mean.
- Likelihood options: sample bracket means with a normal approximation
  give a composite likelihood; within-bin concentration at $\bar x_j$ sends
  its variance to $0$, so unbounded likelihood returns, at least for
  single-bin data. Population bracket means are constraints, not data;
  then it is GMM or minimum distance, outside ML. Hajargasht, Griffiths,
  Brice, Rao and Chotikapanich (2012, Journal of Business & Economic
  Statistics) use GMM on population
  shares and class means with unknown group bounds; then the data are
  Lorenz-curve points and scale drops out. (Venue checked 2026-11-07;
  volume and pages not checked.)

The classical income families are already here: burr (Singh-Maddala),
invburr (Dagum), llogis (Fisk), gb2, lnorm, gamma, pareto.

## Families that become redundant

Truncated and zero-truncated families would become a base family plus
truncation: `tnorm`, `tpois`, `tbinom`, `ztpois`, `ztbinom`, `ztgeom`,
`ztnbinom`, and arguably `hnorm` (normal truncated at its mean). Keep
them until the general mechanism has its own goldens and limits.

## Suggested order

1. Right censoring and fixed left truncation (survival-analysis case).
2. Generic local fits with the R6 score gate for every family.
3. Certify only families whose structure survives (Burr, Weibull, Lomax,
   Pareto: the $k$-profile carries over).
4. Interval censoring and right truncation later; most of the new
   boundary curiosities live here.

## Trigger to revisit

TASK-101 is done and the certification tasks (TASK-116, TASK-140) are
settled, or a user needs censored/truncated fits.

## Verification

- Findings 1-3 above: numerical oracle sweeps (as in
  `dev/research/ml-behaviour-sweep.R`) on censored/binned Burr samples,
  then written proofs in `dev/research/`.
- Censored fits reduce to the current goldens when nothing is censored
  or truncated.
- Truncated base family reproduces `tnorm`/`ztpois` goldens before
  those families are retired.
