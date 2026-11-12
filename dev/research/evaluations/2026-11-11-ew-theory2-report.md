# Exponentiated Weibull: second likelihood analysis

TASK-190, 2026-11-11. This extends the [first analysis](2026-11-10-ew-theory-report.md); it does not implement a fitter. Results labelled proved below are exact-arithmetic arguments, not claims that a floating-point certificate is implemented. The numerical searches are exploratory. Observations are finite and positive, $y_i=\log x_i$, $\mu=\log a$, $t_i=\exp\{b(y_i-\mu)\}$, $q(t)=1-e^{-t}$, $h(t)=\log q(t)$, and $n\ge1$. Write $R(t)=t/(e^t-1)$ and $g(y)=e^y f(e^y)$.

## Findings

* For nonconstant data, the likelihood is globally bounded over **all** simultaneous parameter paths. Any path retaining positive likelihood has a subsequence converging either to a finite member, to inverse Weibull, or to the power face, with the endpoint envelope specified below. This excludes other likelihood-retaining mixed-rate corners, including collapsing and defective limits. It does not claim every weak limit on the extended line is one of these three.
* With $c$ held, every nonconstant sample has a unique finite maximum. With $b$ held, every nonconstant sample has a finite maximum; uniqueness remains open. With $a$ held and both shapes free, every nonconstant sample with an observation above $a$ has a finite maximum; uniqueness remains open. If every observation is at or below $a$ and at least one is below it, the supremum is the explicitly evaluated power-face envelope and no finite maximum exists. These statements include two-point samples.
* Ties at a held endpoint change the likelihood envelope: each observation equal to $a$ contributes $1/(e-1)$ relative to the closed-endpoint power density. When $a$ is free, a common path approaching from above removes this factor, including for multiple endpoint ties. A weak limit alone does not supply a valid likelihood delegate.
* For the free fit, an exact but not yet computationally decidable attainment criterion follows from the two face suprema. Neither uniqueness nor a finite, budgeted global cover with numerical error margins and coefficient enclosures is proved. Thus this is not a ready-to-implement global certificate.

## A joint bound and boundary exhaustion

The log-density of $Y=\log X$ is

$$g(y)=bc\,R(t)\,q(t)^c,\qquad t=e^{b(y-\mu)}.$$

Its log derivative is $b Q_c(t)$, where $Q_c(t)=1-t+(c-1)R(t)$. The first report proves $Q_c'(t)<0$. Thus $g$ is strictly log-concave and has a finite mode $m$ and height $M$.

For any log-concave density with mode height $M$, the following **uniform** bound holds:

$$g(y)\le M\exp\{1-M|y-m|\}. \tag{1}$$

To see this, put $r=\log(M/g(y))$ and $d=|y-m|$. Log concavity bounds the density below by $M e^{-ru/d}$ along the segment from $m$ to $y$. Its integral is at least $Md(1-e^{-r})/r\ge Md/(1+r)$, so $r\ge Md-1$. The limiting case $r=0$ follows by continuity.

Let $D=\max y_i-\min y_i>0$. At least one observation is at distance $D/2$ or more from $m$. Consequently, uniformly in **all three parameters**,

$$\ell\le -\sum_i y_i+n\log M+1-MD/2. \tag{2}$$

The right side tends to $-\infty$ at both $M\downarrow0$ and $M\uparrow\infty$. On any likelihood superlevel set, $M$ is therefore bounded above and away from zero. Equation (1), applied to a fixed observation and the bound on the other densities, also bounds $m$. Together these facts give uniform exponential tails and a uniform density bound: the log-data distributions are tight and no limiting atom is possible. In particular, nonconstant samples cannot have an unbounded likelihood, even with endpoint ties and arbitrarily coupled parameter rates.

To identify the remaining limits, take $U$ uniform on $(0,1)$ and write

$$Y=\mu+\frac{1}{b}\log T_c,\qquad T_c=-\log(1-U^{1/c}).$$

Along any parameter sequence select a subsequence with $c\to c_0\in[0,\infty]$. If $c_0\in(0,\infty)$, convergence of two distinct quantiles and the nondegenerate tight limit force $b$ and $\mu$ to finite interior limits. If $c\downarrow0$, then $c\log T_c\to\log U$ in distribution. Quantile normalization forces $bc\to k\in(0,\infty)$ and $\mu\to s\in\mathbb R$; the limiting $Y$ is $s+\log U/k$, giving power endpoint $e^s$. If $c\to\infty$, with $T=\log c$, then $T(\log T_c-\log T)$ converges to standard maximum Gumbel. Quantile normalization forces $bT\to k\in(0,\infty)$ and $\mu+(\log T)/b\to s\in\mathbb R$; the limit is $s+G/k$, hence inverse Weibull with scale $e^s$ and shape $k$. The normalization assertions follow by subtracting two fixed interior quantiles of the converging nondegenerate normalized variables and then using one quantile for the location. These arguments also cover oscillating paths by subsequences. The asymptotic density statements below follow by inserting the same normalizations in $g$; they are stronger than weak convergence at the sample points.

## Joint face likelihoods and attainment

Let $y_+=\max y_i$, $D_+=\sum_i(y_+-y_i)>0$. The full free-scale power envelope is

$$P=n\log(n/D_+)-\sum_i y_i-n. \tag{3}$$

More generally, for log endpoint $s\ge y_+$ and power shape $k>0$, its log likelihood is $n\log k-\sum_i y_i-k\sum_i(s-y_i)$. It is maximized at $s=y_+$ and $k=n/D_+$.

The joint endpoint assertion is essential. With $c=k/b$, $b\to\infty$, $\mu\to s$ and $b(\mu-s)\to\infty$, every observation at or below $s$ simultaneously reaches its inside power density, including all copies of $s$. Conversely $R(t)\le1$ and $q(t)^c\le1$ give $g(y)\le bc$, so no endpoint factor can exceed $k$ in the limit. Observations strictly below $s$ converge to their power densities; a point above $s$ has density tending to zero. Thus (3) is a joint envelope, not a product of independently unattainable pointwise bounds.

For inverse Weibull, profiling its log-scale gives

$$I=\sup_{k>0}\left[n\log k-\sum_i y_i-k\sum_i y_i+n\log n-n\log\sum_i e^{-ky_i}-n\right]. \tag{4}$$

The bracket is strictly concave in $k$: its second derivative is $-n/k^2-n\operatorname{Var}_{w(k)}(y)$, where $w_i\propto e^{-ky_i}$. It tends to $-\infty$ at either end for nonconstant data, so this face has a unique finite optimum.

Let $B=\max(P,I)$. A finite free MLE exists **if and only if** at least one finite parameter vector has $\ell\ge B$. If a finite vector strictly beats $B$, boundary exhaustion and compactness of superlevel sets away from the faces imply a finite global maximizer. If a vector equals $B$, either it is globally maximal or some vector beats $B$ and the preceding argument applies. If no finite vector reaches $B$, the attained face sequences show the supremum is $B$ and it is not attained. This is an exact criterion, but it is not a practical decision procedure: detecting equality and excluding all other finite vectors are still open. A strictly winning unattained face justifies the relevant boundary diagnosis only after its global comparison is proved; a finite tie must not be reported as nonexistence.

## Held subsets

### Held exponent $c$

Set $v=by+\alpha$ with $\alpha=-b\mu$, and let $\psi_c(v)=\log[c e^v e^{-e^v}q(e^v)^{c-1}]$. Its second derivative is $e^v Q'_c(e^v)<0$. The log likelihood, apart from $-\sum y_i$, is $n\log b+\sum_i\psi_c(by_i+\alpha)$, strictly concave in $(b,\alpha)$ for $b>0$. For fixed $c$, $M=bM_c$ with $0<M_c<\infty$; (2) bounds $b$ away from zero and infinity on a superlevel set, and (1) bounds $\alpha$. Thus for nonconstant data the maximum exists and is unique. Constant data are unbounded as $a=x_0$, $b\to\infty$.

### Held shape $b$

For $c\downarrow0$, $M\le bc\to0$, by $R\le1$ and $q^c\le1$. For $c\to\infty$, evaluating $g$ at $t=\log c$ gives $M\ge b\log c/e\,(1+o(1))\to\infty$. Equation (2) therefore confines $c$ to a compact subset of $(0,\infty)$ on every relevant superlevel set. The mode bound then confines $\mu$, so every nonconstant sample has a finite maximum. An all-root bound or uniqueness proof is open. Constant samples are unbounded with $a,c$ free, via the $c\to\infty$ concentration path in the first report.

### Held scale $a$

Let $s=\log a$, $D_s=\sum_i(s-y_i)$, and let $m$ be the multiplicity of observations at $a$. If all $y_i\le s$ and $D_s>0$, then for each $y_i<s$,

$$g(y_i)=bcR(t_i)q(t_i)^c<bc\exp\{bc(y_i-s)\},$$

because $R(t)<1$ and $q(t)<t$. At $y_i=s$, $g(y_i)=bc R(1)q(1)^c<bcR(1)$, with $R(1)=1/(e-1)$. Thus every finite parameter vector is strictly below the common envelope

$$P_s=n\log(n/D_s)-\sum_i y_i-n-m\log(e-1). \tag{5}$$

The path $b\to\infty$, $c=(n/D_s)/b$ attains (5) as a supremum. This covers constant $x_0<a$, all nonconstant samples entirely below $a$, and endpoint ties. When $m>0$, ordinary `mlpower` with endpoint held at $a$ has the same limiting distribution and shape but a log likelihood larger by $m\log(e-1)$: do not silently substitute its likelihood. If $m=0$, the held-endpoint power likelihood equals (5).

If the sample is nonconstant and some observation exceeds $a$, the power face cannot retain positive likelihood because its endpoint is fixed at $a$; the inverse-Weibull normalization forces $\mu\to-\infty$, impossible with held $a$. Boundary exhaustion therefore gives a finite maximum. Its uniqueness and an all-root bound remain open. For constant data at $a$, the likelihood is unbounded with $b$ free even if $c$ is held. For constant data above $a$, it is unbounded with $b,c$ free, by taking $t_0=(x_0/a)^b\to\infty$ and $\log c=t_0$. With $c$ held instead, this last conclusion does not apply; the single-free result in the first report applies.

For two values $x_L<x_H$, the preceding results apply without exceptions: fixed $c$ gives a unique maximum, fixed $b$ gives an attained maximum, and fixed $a$ is decided by whether $x_H>a$. In the free fit the power face uses $D_+=n_L\log(x_H/x_L)$; whether it, inverse Weibull, or a finite fit wins remains sample-dependent and unproved by these formulas alone.

## Dossier-ready limit candidates

These are proposed records, with status identifiers to be defined by TASK-180. The conditions explicitly identify which coefficients are free; `fixed` alone does not enforce that. They should be checked against the eventual wrapper and message semantics before insertion. `holds: []` remains appropriate: these are data-dependent outcomes, not structural hold exceptions.

```yaml
limits:
  - name: unbounded likelihood
    kind: nonexistence
    firmness: proved
    when: '$a=x_0$ and $b$ tends to infinity'
    detect: 'All observations equal $x_0$, and scale and shape are free'
    status: Unbounded
  - name: unbounded likelihood
    kind: nonexistence
    firmness: proved
    fixed: [shape]
    when: '$c$ tends to infinity on the concentration path at $x_0$'
    detect: 'All observations equal $x_0$, and scale and exponent are free'
    status: Unbounded
  - name: unbounded likelihood
    kind: nonexistence
    firmness: proved
    fixed: [scale]
    when: '$b$ tends to infinity at the common observation'
    detect: 'All observations equal the held scale, and shape is free'
    status: Unbounded
  - name: unbounded likelihood
    kind: nonexistence
    firmness: proved
    fixed: [scale]
    when: '$b$ and $c$ tend to infinity on the concentration path at $x_0$'
    detect: 'All observations equal $x_0$ above the held scale, and shape and exponent are free'
    status: Unbounded
  - model: power
    firmness: proved
    fixed: [scale]
    when: '$b$ tends to infinity and $bc$ tends to a positive limit'
    detect: 'All observations lie strictly below the held scale, and shape and exponent are free'
    status: PowerBoundary
  - name: power-law limit with held endpoint
    firmness: proved
    fixed: [scale]
    when: '$b$ tends to infinity and $bc$ tends to a positive limit'
    detect: 'All observations are at or below the held scale, at least one is below it and at least one equals it, and shape and exponent are free'
    status: PowerBoundaryTied
  - model: power
    firmness: proved
    when: '$b$ tends to infinity and $bc$ tends to a positive limit'
    detect: 'All coefficients are free and a completed global comparison proves the power face strictly wins'
    status: PowerBoundary
  - model: invweibull
    firmness: proved
    when: '$c$ tends to infinity and $b\log c$ tends to a positive limit'
    detect: 'All coefficients are free and a completed global comparison proves the inverse Weibull face strictly wins'
    status: InvweibullBoundary
```

A proved strict win suffices for a boundary stop; when a face ties an interior fit, attainment must be checked. The held-endpoint tied record intentionally has no `model: power` automatic likelihood delegate. Additional entries for simultaneous held coefficients should not assert unboundedness without the stated free coordinates.

## Numerical evidence and implementation boundary

[check.py](2026-11-11-ew-theory2-data/check.py) and [check.json](2026-11-11-ew-theory2-data/check.json) record float64 multistart profile searches and the two face values for eight deterministic samples: unequal two-point multiplicities, near ties, endpoint ties, a nearly constant sample, and log ranges up to 30. The search truncates $\log t$ to $[-700,7]$ and bounds the inverse-face scalar search; successful termination at such limits is **not** an interior root certificate. For log data $(0,10^{-6},2\cdot10^{-6},3\cdot10^{-6},5\cdot10^{-6})$, one finite candidate beats the best computed face by approximately $0.00317$ log-likelihood units. This is a numerical witness, not a proof of a global maximum. Several other searches stop at the imposed boundaries below a face, which cannot decide the winner. The script uses stable `expm1`/`log1p` branches and log-sum-exp; no result is a high-precision oracle for the interior.

[endpoints.py](2026-11-11-ew-theory2-data/endpoints.py) and [endpoints.json](2026-11-11-ew-theory2-data/endpoints.json) evaluate the two common paths at 100 decimal digits for log data $(0,0.3,1,1,1)$. At $b=1280$, the held-scale and free-scale log likelihoods are respectively about $0.00316$ and $0.41104$ below their distinct envelopes. This checks the formulas and endpoint multiplicity numerically; the inequalities and exhaustion above are analytic arguments. Reproduce each script using `TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run python3 <script>`; redirect output to its JSON file.

**Open for TASK-180:** construct explicit finite bounds, with uniform remainders in both face neighborhoods, for a global cover of the profiled $(\mu,\log b)$ plane; isolate and compare every interior root including tied and nearly singular cases; and enclose all free coefficients to relative `tol`. Equation (2) supplies an explicit height bound but is not by itself a finite parameter-space cover or a convergence-rate bound. Certificate evaluations must use the `128 * min(iterlim,100000)` ledger and nested solves the solver ledger. Establish a practical floating-point margin for the actual evaluated bounds. No claim is made that a default budget closes every ordinary sample or that equality comparisons are decidable. If the global certificate is inconclusive, follow the requested uncertified-search fallback and score gate; do not turn uncertainty or certificate budget exhaustion into a new refusal. A proven better face or failed search still receives its appropriate outcome. Stable extreme evaluation and the accuracy of uncertified coefficients require separate implementation validation; no change to settled `dpqr.hpp` or `special/` is proposed here.

## Verification

* Workstation, GCC 15, `/tmp/umvl-cpu/run4 just cpp-test-no-long-double`: **135/135 passed**, including `bnbinom` and `expweibull`.
* Simbox, GCC 13, `simbox-run just cpp-test-no-long-double`: **134/135 passed**; `distribution_bnbinom` failed three exact `a.logLik == b.logLik` assertions at `bnbinom/test.cpp:373`. Its isolated no-long-double rerun reproduced all three failures. `distribution_expweibull` passed. This analysis changes neither the failing code nor its tests. The provided simbox wrapper set six CMake/CTest workers for this first run; that exceeded the four-core global instruction. The later simbox development run explicitly overrode both worker counts to four.
* Simbox, C++ development suite: **108/108 passed**. R development suite: **FAIL 0, WARN 0, SKIP 154, PASS 23231**. Both ran via `simbox-run`, with four-worker CMake/CTest limits and BLAS/OpenMP pinned to one thread.
* Workstation, single slot: both evidence scripts completed. No fitter, density, binding, test infrastructure, dossier or golden was changed, so the conditional full R, JS and site checks were not triggered. The simbox no-long-double failure is a real outstanding verification limitation, not a green result. None of these repository suites proves the mathematical claims in this report.
