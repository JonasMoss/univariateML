# Exponentiated Weibull: likelihood, holds and boundary analysis

TASK-178, 2026-11-10. This is an analysis for the subsequent implementation, not an implemented fitter or a proof of global certification. The observations are finite and strictly positive, $n\ge1$; write $y_i=\log x_i$, $\mu=\log a$, $t_i=\exp(b(y_i-\mu))$, $q_i=1-e^{-t_i}$ and $h(t)=\log(1-e^{-t})$. Parameters $a,b,c$ are strictly positive and finite. Invalid and empty inputs need their usual distinct diagnostics.

## Results and evidence

* The free likelihood reduces exactly to two coordinates; a globally exhaustive certificate and a complete existence/uniqueness criterion remain **open**. A finite stationary point is not by itself a certificate. The demonstrated boundary laws are inverse Weibull and a finite-endpoint power law, plus point-mass/defective limits. It is **not proved** that these exhaust every likelihood-retaining sequence.
* The proposed original-scale Gumbel limit is impossible: every weak limit of laws supported on $[0,\infty)$ is supported there, whereas a nondegenerate Gumbel has positive mass below zero. A Gumbel appears after centering and scaling, and $\log X$ has a Gumbel limit in a different joint regime, giving inverse Weibull on the original scale. The required direction is $b\to0$, not $b\to\infty$.
* Constant data are unbounded with all parameters free, with $c$ held and $a,b$ free, **and with $b$ held and $a,c$ free**. The last result is not visible from the Weibull subfamily alone. Other constant and two-point cases are discussed below; do not apply an all-free degeneracy rule indiscriminately.
* Mathematical derivations below are exact-arithmetic statements. [High-precision checks](2026-11-10-ew-theory-data/check.py) and [their output](2026-11-10-ew-theory-data/check.json) illustrate three asymptotic calculations; they neither establish global likelihood ordering nor prove exhaustion of boundary paths. Run with `TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-ew-theory-data/check.py`. The script uses mpmath at 100 digits; the recorded errors are absolute differences in log CDF and log density.

## Likelihood and reductions

Ignoring no data-dependent terms, the log likelihood is

$$\ell=n(\log b+\log c)-\sum_i y_i+\sum_i(\log t_i-t_i)+(c-1)\sum_i h(t_i).$$

For fixed $a,b$, it is strictly concave in $c$, with unique maximizer $\hat c=-n/H$, $H=\sum_i h(t_i)<0$. Substitution gives

$$\ell_p(\mu,b)=n\log b-\sum_i y_i+\sum_i(\log t_i-t_i-h(t_i))+n\log(-n/H)-n.$$

This is a smooth two-dimensional profile on $\mu\in\mathbb R,b>0$. Put

$$Q_c(t)=1-t+(c-1)\frac{t}{e^t-1}.$$

The scores for $\mu$, $b$, and $\log c$ are respectively

$$-b\sum_i Q_c(t_i),\quad {n\over b}+\sum_i(y_i-\mu)Q_c(t_i),\quad n+cH.$$

Thus profile scores are the first two with $c=\hat c$. For $t>0$, $0> d[t/(e^t-1)]/dt>-1$; hence $Q'_c(t)<0$ for all $c>0$, with $Q_c(0+)=c$ and $Q_c(\infty)=-\infty$. This gives a unique scale conditional on $b,c$, and a strictly concave conditional log likelihood in $b$ for fixed $a,c$: its second derivative is $-n/b^2+\sum_i(y_i-\mu)^2t_i Q'_c(t_i)<0$.

A plausible certificate route is a bounded cover of $(\mu,\log b)$ using analytic upper bounds for $\ell_p$, derivative/Hessian enclosures, and root isolation; compare all surviving interior boxes against **joint** likelihood-envelope bounds at every boundary. An isolated stationary point needs coefficient enclosures to the requested relative tolerance, not only a small score. A complete implementation needs explicit compactification, uniform remainder bounds, tie handling, and proof that the cover excludes all other paths. Comparisons may use the package standard: exact arithmetic conditional on computed comparisons with a stated practical floating-point margin, rather than mandatory outward rounding. Bound evaluations and solver steps must respect `dev/design/work-budgets.md`. These are requirements for a route, not a completed certificate.

When a future certificate cannot decide a face comparison, sign or budget on ordinary data, preserve the existing successful search result marked uncertified; do not turn uncertainty into a new stop. A proved better unattained boundary can stop with its reason; a failed search follows the diagnostic score-gate rule. There is no existing expweibull search in this analysis.

## Escape paths

### Large exponent and inverse Weibull

Let $T=\log c\to\infty$, $bT\to k>0$, and $aT^{1/b}\to s>0$, with sufficient accuracy in the latter scaling to ensure $T[(x/a)^b/T-1]\to k\log(x/s)$. For example take $b=k/T$ and $a=sT^{-1/b}$ exactly. Then

$$t(x)=T(x/s)^{k/T}=T+k\log(x/s)+o(1),\qquad F(x)\to\exp[-(s/x)^k].$$

Densities also converge at each $x>0$ to $(k/s)(x/s)^{-k-1}e^{-(s/x)^k}$. By Scheffé's lemma this is total-variation, hence Hellinger, convergence. This is the package `invweibull` model; $\log X$ is maximum Gumbel with location $\log s$ and scale $1/k$. On this example path $a\to0$. The explicit rate condition matters: mere $aT^{1/b}\to s$ need not force the displayed centered $t-T$ limit when multiplied by a divergent $bT$ or without controlling the residual.

For fixed $b>0$, choose $a=sT^{-1/b}$. Then $X\to s$ weakly and, with $Z=bT(X/s-1)$, $Z$ tends to standard maximum Gumbel. This is a **standardized** limit, not `mlgumbel` for $X$. For constant $x_i=s$, $f(s)\sim bT/(es)$, so the likelihood diverges with fixed $b$. For two separated observations the same concentration path does not yield two fitted spikes: off $s$ the density tends to zero.

### Small exponent and power face

For fixed $s,k>0$, take $a=s$, $b\to\infty$, $c=k/b$. For $0<x<s$, $t\to0$ and $c\log(1-e^{-t})\to k\log(x/s)$, while for $x>s$, $F\to1$. Thus the law tends in total variation to the power distribution on $(0,s)$ with CDF $(x/s)^k$ and density $k x^{k-1}/s^k$. The endpoint requires special care: at $x=s$ this particular path gives density tending to $(k/s)/(e-1)$, **not** the inside-limit density $k/s$. By allowing $a\to s$ from above with $b\log(a/s)\to\infty$ and $\log(a/s)\to0$, the endpoint density tends to $k/s$ while the same law is approached. Other approaches give other endpoint values. Therefore the sample likelihood envelope, not weak convergence alone, determines comparison when a datum equals $s$; one may not multiply pointwise limsups without proving one common path. The displayed common path realizes the inside-limit density simultaneously at all observations $\le s$.

The package `power` dossier uses endpoint `alpha = s`, shape `beta = k`, and a closed upper endpoint with the inside-limit density. The eventual delegate must preserve this mapping and endpoint convention. A face law does not itself prove an escape wins. If the power envelope wins and no interior tie attains the supremum, the planned diagnostic is `use mlpower` after the comparison is certified.

### Other boundary directions and limitations

Parameter directions alone do not specify a unique limit. With other parameters held finite, $a\to0$ sends $X$ to zero, $a\to\infty$ sends it to infinity, $c\to0$ sends it to zero, and $c\to\infty$ sends it to infinity. These statements are weak limits on the extended positive line, not ordinary finite-density model escapes. With $a,c$ fixed, $b\to\infty$ gives a point mass at $a$; $b\to0$ gives a defective extended-line limit with mass $(1-e^{-1})^c$ at zero and the remainder at infinity. Joint paths can instead yield either nondegenerate face above, or other degenerate/defective laws. For example $c\to0$ with fixed $b$ and $a\to\infty$ can place nonzero mass at each of zero and infinity by keeping $bc\log a$ finite; it does not give an ordinary continuous positive limit.

**Open boundary classification:** prove or refute exhaustiveness of the two continuous faces for every joint path, including $a\to0/\infty$, $b\to0/\infty$, $c\to0/\infty$ at mixed rates; establish uniform joint likelihood bounds, in particular at the power endpoint and for non-tight or collapsing laws. No blanket stop is justified by a single coordinate reaching a numerical bound. For fixed $b$ and nonconstant samples the two obvious concentration/diffusion ends have vanishing likelihood, but a full coercivity proof over all coupled $a,c$ paths remains to be written.

## All held subsets

The set named in the first column is held; the other coefficients are estimated. Statements of uniqueness concern the finite domain. Constant means all observations equal to $x_0$.

| Held | Reduction and status |
|---|---|
| $a,b,c$ | Evaluate the density. No optimization or data-dependent existence issue. |
| $a,b$ | Unique $\hat c=-n/H$ for any positive sample, including constant samples. Both $c$ boundaries have log likelihood $-\infty$. |
| $b,c$ | The equation $\sum Q_c(t_i)=0$ has exactly one $a$: the sum strictly decreases as $a$ decreases, from $nc$ to $-\infty$. It exists even for constant data. |
| $a,c$ | The $b$ score is strictly decreasing. It starts at $+\infty$. If some $x_i>a$, it tends to $-\infty$; otherwise, if any $x_i<a$, it tends to $c\sum_{x_i<a}\log(x_i/a)<0$. Thus a unique finite root exists unless every $x_i=a$, when $\ell=n\log b+O(1)$ is unbounded. |
| $c$ | For each $b$, solve the unique scale equation. Constant data are unbounded by taking $a=x_0$, $b\to\infty$. For nonconstant data, the fixed-$c$ model is a location-scale family for $\log X$; existence is expected from the tails but a complete uniform coercivity proof and global uniqueness or an implementable all-root bound are **open**. The Weibull subcase $c=1$ has the proved unique nonconstant fit in the Weibull dossier. |
| $b$ | Profile $c$ as above and search in $a$. Constant data are unbounded even for a held $b$ by the $c\to\infty$ concentration path above. For nonconstant data, prove compact bounds and isolate/compare all roots; existence and uniqueness are **open** here. An inverse-Weibull face requires $b\to0$ and is unavailable under this hold. |
| $a$ | Profile $c$ and search over $b$. If constant data equal $a$, $b\to\infty$ at fixed $c$ makes the likelihood unbounded. If constant $x_0>a$, set $t_0=(x_0/a)^b$, $\log c=t_0$; then $f(x_0)\sim bt_0/(ex_0)$, also unbounded. For constant $x_0<a$, the power face at fixed endpoint $a$ has finite maximized density $1/[e x_0\log(a/x_0)]$; whether a finite fit exceeds it, and global root/face comparisons for general samples, remain **open**. The inverse-Weibull path above requires $a\to0$ and is unavailable with $a$ held. |
| none | Profile $c$; compare all interior maxima and all rigorously bounded faces. Constant samples are unbounded. General nonconstant existence, uniqueness and complete face ordering are **open**. |

A held value of $c=1$ is the Weibull submodel, $b=1$ is generalized exponential, and $b=2$ gives Burr X; those identities do not give the optimizer for a larger free subset. The single-free monotonicity arguments above provide direct brackets and a certified solve, subject to representability and the stated practical comparison margin.

### Two distinct values

For $x_L<x_H$ with multiplicities $m,n-m$, the likelihood is finite at every finite parameter. The power boundary with endpoint $s=x_H$ has optimum $\hat k=n/[m\log(x_H/x_L)]$ and finite joint log likelihood

$$n\log\hat k+(\hat k-1)(m\log x_L+(n-m)\log x_H)-n\hat k\log x_H.$$

The endpoint-adjusted common path above realizes this value. A finite inverse-Weibull face fit also exists for nonconstant positive observations, by its Gumbel transformation. Neither fact establishes which face or an interior fit wins, whether ties occur, or whether another path can make the likelihood unbounded. Those questions remain **open**, including for each two-point held subset. In particular, do not classify all two-point data as constant or automatically send them to a limit fitter.

## Interior regularity and inference

At every finite $a,b,c>0$, including $c=1$, support is fixed and the density is positive and smooth in all three log coordinates. For $t\downarrow0$ the density in $t$ is asymptotic to $c t^{c-1}$ and scores and their local derivatives have at most logarithmic growth; as $t\to\infty$ the exponential tail dominates polynomial and logarithmic score growth. Uniform local integrable envelopes yield DQM and finite Fisher information. Pointwise log likelihood is $C^\infty$ for positive observations and the ordinary observed Hessian is a valid candidate subject to numerical derivative validation.

Information is positive definite: in coordinates $(\log a,\log b,\log c)$, the scores as $t\to\infty$ have distinct leading orders proportional to $t$, $-t\log t$, and $1$. No nontrivial linear combination can vanish almost surely. Hence every principal held-subset information matrix is positive definite. The $c=1$ Weibull member is an interior smooth submodel, not a singular-information seam. The conditional test $c=1$ with $(a,b)$ fitted is a regular one-dimensional nested hypothesis at the model level; treating their values as known gives a different information calculation.

DQM and rank do not by themselves establish existence or consistency of a chosen global MLE, its root-$n$ asymptotic normality, likelihood-ratio calibration, or a valid bootstrap. Those require the usual model-specific consistency, domination and optimization hypotheses. Model Fisher, observed Hessian, per-observation score outer products and misspecification sensitivity are distinct quantities. The planned family has no implemented or validated inference path here. At inverse-Weibull and power closure faces, ordinary interior Wald/LR and bootstrap calibration do not follow from the interior result; handle each target and nuisance structure separately.

## Dossier candidates

The following uses provisional parameter names `scale`, `shape`, `exponent` for $a,b,c$. Replace these with the final declared names; statuses are proposed identifiers, not implemented enum values. Detection deliberately requires a proved comparison and attainment analysis. Do not add a `limits` entry that asserts a winning face solely from convergence of distributions. The two constant-data entries are minimal rather than an exhaustive enumeration of held-scale degeneracy.

```yaml
limits:
  - name: unbounded likelihood
    firmness: proved
    when: '$b$ tends to infinity with $a$ at the common observation and $c$ positive and fixed.'
    detect: 'All observations are equal, and scale and shape are free.'
    status: Unbounded
    message: unbounded
  - name: unbounded likelihood
    firmness: proved
    fixed: [shape]
    when: '$c$ tends to infinity while $a=(x_0/(\log c)^{1/b})$ for the common observation $x_0$.'
    detect: 'All observations are equal, shape is held, and scale and exponent are free.'
    status: Unbounded
    message: unbounded
  - model: invweibull
    firmness: proved
    when: '$b\log c$ tends to a positive constant and the centered transformed scale follows the inverse-Weibull path in the analysis.'
    detect: 'A completed global certificate proves that the unattained inverse-Weibull face strictly beats every finite candidate and other boundary envelopes.'
    status: InvweibullBoundary
  - model: power
    firmness: proved
    when: '$b$ tends to infinity, $bc$ tends to a positive constant and scale tends to a finite positive endpoint.'
    detect: 'A completed global certificate proves that the unattained power-face joint likelihood envelope strictly beats every finite candidate and other boundary envelopes.'
    status: PowerBoundary
holds: []
```

`holds: []` states the intended R2-compliant design, not that a fitter exists. The `limits` snippets are contingent on implementing the stated checks and on verifying the eventual delegate/endpoint conventions; `fixed` describes applicability and must not be used as a replacement for checking which other coefficients are free. More entries are needed for the proved held-scale unbounded cases before the dossier can claim complete diagnostics. The unproved comparisons are not made proved by the `firmness` of the distributional limit.

## Remaining work for the fitter card

1. Close the uniform boundary and joint-envelope analysis, including mixed rates, the fixed-$b$ and fixed-$c$ coercivity proofs, and the held-scale constant-below-scale and two-point cases. Construct actual global cover bounds and show they meet the work budget and coefficient accuracy contract.
2. Independently compare finite solutions and face likelihoods over representative and adversarial samples, including near ties, tiny shapes, repeated extrema, huge dynamic ranges and the endpoint convention. The current small high-precision calculation is not such a search.
3. Validate `mlpower` parameter mapping and boundary semantics, inverse-Weibull transformations, stable $h(t)$ and its derivatives without changing settled `dpqr.hpp` or `special/` in this analysis. Numerical underflow in $H$ must not be mistaken for $c=\infty$.
4. Implement all holds, score gates, proved nonexistence diagnostics and an uncertified fallback consistent with the user's rule. Establish inference derivatives and calibration separately. No production fitter, d/p/q/r, binding, golden, or dossier was changed by this job.

## Verification record

* Workstation, `/tmp/umvl-cpu/run`: `check.py` completed at 100-digit precision; the saved JSON records the pointwise comparisons. This is numerical evidence only.
* Workstation, `/tmp/umvl-cpu/run4 just cpp-test`: 106/106 passed.
* Workstation, `/tmp/umvl-cpu/run4 just cpp-test-no-long-double`: 130/130 passed, including full-labelled shards.
* Workstation, `/tmp/umvl-cpu/run4 just test`: `[ FAIL 0 | WARN 0 | SKIP 154 | PASS 23031 ]`.
* Simbox: `simbox-run` returned 75 (unreachable) for the C++ dev, no-long-double, and R dev attempts. The local fallback results are above; the requested second-machine no-long-double comparison could not be obtained.
* No fitting, density, binding, or test-infrastructure changes were made, so the conditional `just test-full`, JS and site checks were not triggered. No tests establish the open mathematical statements above.
