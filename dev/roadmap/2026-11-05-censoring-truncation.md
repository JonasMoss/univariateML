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

General question: characterise $\partial_T M$ for each family and each
censoring/truncation scheme, and which faces of $\partial_T M$ attract
the MLE on which samples.

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
