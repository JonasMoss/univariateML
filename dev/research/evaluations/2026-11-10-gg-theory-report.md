# Prentice generalized gamma: free fit and held subsets

TASK-175, analysis for the later fitter card. No fitter, density, dossier, or generated binding was changed. All claims marked **proved** below are mathematical statements in exact arithmetic; the proposed numerical certificates are conditional on computed comparisons and practical error margins, not outward-rounded interval proofs. **Open** means a proof or implementable bound is still missing, not that the API may refuse an otherwise valid search result. The code-specific facts refer to this worktree's `trgamma/mle.hpp` and `invtrgamma/mle.hpp`; see also [the parent certificate report](2026-11-07-trgamma-seam-report.md).

## Coordinates and likelihood

Let $y_i=\log x_i$, $\bar y=n^{-1}\sum y_i$, and $s^2=n^{-1}\sum(y_i-\bar y)^2$. The log likelihood for $y$, omitting the data-only Jacobian $-\sum y_i$, is, for $Q\ne0$,

$$L/n=\log|Q|-\log\sigma +a\log a-\log\Gamma(a)+a\overline w-a\overline{e^w},\quad a=Q^{-2},\quad w_i=Q(y_i-\mu)/\sigma.$$

At zero it is the normal log likelihood in $y$. The mode in $y$ is $\mu$. The positive and negative signs of $Q$ are respectively the entire transformed-gamma and inverse-transformed-gamma interiors. In parent coordinates $p=|Q|/\sigma$, $k=a$, and the parent log scale is $\mu+(2\sigma/Q)\log|Q|$ on the original side; use log data for the reciprocal side. This coordinate can diverge or lose precision near zero even when $(\mu,\sigma,Q)$ is perfectly representable. For positive $Q$, the parent scale is $\exp[\mu+(2\sigma/Q)\log|Q|]$; for negative $Q$ its reciprocal-side log scale is the negative of that expression.

## Free fit: signed profile and certificate

Assume $s>0$, put $u_i=(y_i-\bar y)/s$, $t=Qs/\sigma$, $K(t)=\log\overline{e^{tu}}$, and let $a(t)>0$ uniquely solve $\log a-\psi(a)=K(t)$ for $t\ne0$. The gamma inner fit gives, per observation relative to the fitted lognormal,

$$\Delta(t)=\log|t|+\tfrac12\log a-R(a)-aK(t)+\tfrac12,\quad R(a)=\log\Gamma(a)-(a-\tfrac12)\log a+a-\tfrac12\log(2\pi).$$

The corresponding Prentice coefficients are

$$Q=\operatorname{sign}(t)/\sqrt a,\qquad \sigma=s/(|t|\sqrt a),\qquad \mu=\bar y+sK(t)/t.$$

Define $\Delta(0)=0$, $(\mu,\sigma,Q)=(\bar y,s,0)$. **Proved:** $K(t)=t^2/2+\kappa_3t^3/6+O(t^4)$ and $\log a-\psi(a)=1/(2a)+O(a^{-2})$ imply $Q=t+O(t^2)$, $\sigma=s+O(t)$, $\mu=\bar y+O(t)$ and

$$\Delta(t)=-\kappa_3t/6+O(t^2),\qquad \Delta'(t)\to-\kappa_3/6\quad(t\to0^\pm).$$

Thus the signed profile is $C^1$ at zero, including the derivative limits, and the two parent profiles are restrictions of this same function, with the negative side obtained by reflecting the standardized logs. Nonzero sample log-skewness rules out $Q=0$ as a free maximum. Zero skewness alone does not prove it is a maximum or unique; curvature can vanish. The seam is an ordinary interior model, not a lognormal escape or an automatically selected estimate.

**Proved reduction:** the supremum is the maximum of the two one-sided suprema and the attained zero point. The existing side cover supplies the convex-conjugate secant bounds, analytic far-tail bounds, inner gamma solve, concave root-neighborhood and coefficient-inclusion machinery, and the reflected data give the second side. The power and Pareto tail limits are $\Delta_+=\frac12\log(2\pi)-\frac12-\log(\max u)$ and $\Delta_-=\frac12\log(2\pi)-\frac12-\log(-\min u)$, with respective endpoint $\max x$ and $\min x$. A strictly winning unattained face gives proposed statuses `PowerBoundary` / `ParetoBoundary` with advice `mlpower` / `mlpareto`; an attained finite winner, including $Q=0$, gives `Ok`. The supremum identity alone does not establish uniqueness, strict face victory, numerical comparability, or that a face is unattained when it ties an interior winner. Retain all tied witnesses and distinguish attainment from an unattained strict winner.

**Seam work needed.** The parent's `normal_seam_upper` applies on $[0,T]$; apply it separately to reflected data for $[-T,0]$, changing the cubic sign. It bounds the entire omitted seam after refitting both nuisance parameters, including zero-skew data. Its exact-arithmetic bound uses fourth-moment Taylor bounds and alternating Stirling bounds; the current implementation uses practical floating-point margins. The parent also caps omitted cells at a $10^{-8}$ per-observation tie threshold and its public side result may stop as `LognormalBoundary` or a comparison failure; it is therefore **not** a black-box Prentice MLE. In particular a nonzero winner whose improvement over zero is positive but below $10^{-8}$ must not be rounded to $Q=0$, and a side stop does not prove that zero or the opposite side wins. Refine both signed seam covers and root inclusion until the actual comparison and coefficient accuracy close; certify the seam itself when zero is the winner. The existing seam upper bound is a bound on likelihood, not a coefficient enclosure. The $10^{-8}$ policy and floating-point padding can leave a face/finite comparison unresolved even after the parent's scan completes. Carry candidate and upper/lower likelihood witnesses across both sides; do not reinterpret a tolerance tie as a proved escape.

**Fallback rule:** if this global comparison, a seam sign, or the certificate budget cannot close on ordinary data, return the existing successful search candidate as **uncertified**, preserving its locally justified coefficient accuracy and witnesses. Do not turn a certificate-only failure into an unresolved, budget or numerical stop. A stop is appropriate if a proof establishes that candidate is wrong, or the underlying search/its coefficient-accuracy check actually fails. This distinction resolves the apparent tension between a global certificate budget and the rule against returning a capped or unchecked numerical search. Solver and certificate budgets must follow the repository's `1 * min(iterlim,100000)` and `128 * min(iterlim,100000)` rules. It is **open** how to close every near-equality and flat seam with the existing practical margins; do not label all free results certified on the strength of the supremum identity.

Constant positive samples with all coefficients free have unbounded likelihood by $Q=0$, $\mu=y_1$, $\sigma\downarrow0$ (`Unbounded`, or the implementation's established unbounded-data status).

## Holds: all subsets

The status names below are proposals for the fitter card, not enums already implemented. `Ok` requires a represented, accurate finite maximizer. `Unbounded` denotes proved concentration; `IterationFailure` denotes failure of the underlying search or coefficient accuracy, not an inconclusive certificate. All parameters held is a direct density evaluation and needs no optimization. Every formula uses positive finite observations and valid finite held values.

### Q held, mu and sigma free

**Proved:** for nonzero held $Q$, fix $a=Q^{-2}$ and optimize signed $r=Q/\sigma$ with the sign fixed. Profiling location gives $\mu=\bar y+K_y(r)/r$, where $K_y(r)=\log\overline{e^{r(y-\bar y)}}$. Apart from constants, the profile is $\log|r|-aK_y(r)$; its second derivative is $-1/r^2-aK_y''(r)<0$. It tends to $-\infty$ at both ends for nonconstant data, so has a unique finite maximizer. This is the parent's fixed-gamma-shape reduction, **not** its fixed-power route. At held $Q=0$ use the unique normal fit $(\bar y,s)$ for nonconstant data. For constants any fixed $Q$ permits $\mu=y_1$, $\sigma\downarrow0$: `Unbounded`. No power, Pareto, or lognormal escape remains with $Q$ held.

### sigma held, mu and Q free

**Proved:** for every $Q\ne0$ the unique location fit is $\mu=\bar y+K_y(Q/\sigma)\sigma/Q$, with the normal mean at zero. Thus optimize a continuous signed scalar profile. For fixed $\sigma$ the maximum density over $y$ and $\mu$ is

$$[\sigma\sqrt{2\pi}]^{-1}e^{-R(Q^{-2})},$$

which tends to zero as $|Q|\to\infty$; hence the profiled likelihood tends to $-\infty$ uniformly in location. A finite global maximum exists even for constant data. For constants, $R(a)>0$ at finite $a$, so the unique maximum is $Q=0,\mu=y_1$. No concentration or power/Pareto escape is possible. **Open:** general uniqueness, and a worked practical global certificate for the signed compact profile. A route is an explicit outer bound from this mode inequality, validated branch-and-bound on the remaining compact interval, a zero-seam Taylor bound and root enclosures. These are a route, not an implemented certificate; inconclusive certification uses the fallback.

### mu held, sigma and Q free

Put $z_i=y_i-\mu$. For each nonzero $Q$, $a=Q^{-2}$ is fixed and $r=Q/\sigma$ has its sign. Apart from constants, the objective is

$$H(a,r)=\log|r|+a\log a-\log\Gamma(a)+ar\bar z-a\overline{e^{rz}}.$$

**Proved:** $H_{rr}=-1/r^2-a\overline{z^2e^{rz}}<0$. Unless all $z_i=0$, its derivative has one root on each sign of $r$; this gives a unique scale for each fixed nonzero Q. At Q=0, the scale is $\sqrt{\overline{z^2}}$ when positive. If every $z_i=0$, $\sigma\downarrow0$ at Q=0 proves `Unbounded`.

**Proved face paths:** let $Q\to+\infty$, $\sigma Q\to d>0$. The log-data density tends to $d^{-1}e^{z/d}$ for $z<0$, zero for $z>0$, and its pointwise trace at $z=0$ tends to $d^{-1}$. This is power with endpoint $e^\mu$. The reflected $Q\to-\infty$, $\sigma|Q|\to d$ gives Pareto with the same endpoint, supported on $z>0$, trace $1/d$. If all $z_i\le0$ and $\bar z<0$, the positive face has maximizing $d=-\bar z$; if all $z_i\ge0$ and $\bar z>0$, the negative face has $d=\bar z$. Endpoint traces matter with ties; a limit density's endpoint convention must not silently replace the trace along this path. Compare actual likelihoods and attainment before proposing `PowerBoundary` or `ParetoBoundary`. With data on the wrong side the respective fixed-$d$ path has log likelihood $-\infty$.

For a constant $y_1\ne\mu$, the likelihood is bounded: the log-density is unimodal with mode $\mu$, so $f_Y(y_1)\le1/|y_1-\mu|$. A finite fit or an endpoint face may win. **Open:** a complete classification of all joint corners, including possible mixed samples with ties at $\mu$, existence and uniqueness of the outer maximum, and an executable global certificate with explicit tail bounds. In particular the displayed face paths do not prove that they are the only relevant joint escapes or that either wins. The required work is uniform control as $a\downarrow0$ and $a\to\infty$ with the optimizing $r$ allowed to vary, including endpoint ties and the zero-shape seam, plus a finite-region cover. Do not declare a face/nonexistence from the path alone. Use a successful search uncertified if these comparisons do not close.

### mu and sigma held, Q free

**Proved:** the scalar likelihood is continuous through zero, and the same fixed-sigma mode bound sends it to $-\infty$ as $|Q|\to\infty$. Thus a finite maximum exists for every sample, including constants. If all observations equal $e^\mu$, the unique maximum is $Q=0$ by positivity of the Stirling remainder. **Open:** general uniqueness and a practical interval/root cover for all finite Q, with stable seam derivatives. Compact tail exclusion plus validated finite-interval bounds is a certificate route; failure to close the global cover triggers the uncertified fallback, not an escape.

### mu and Q held, sigma free

For $Q\ne0$, the preceding $H(a,r)$ is strictly concave in $r$ on its fixed-sign half-line. **Proved:** if at least one $z_i\ne0$, its derivative crosses zero exactly once, giving a unique finite scale. If every $z_i=0$, $|r|\to\infty$ gives `Unbounded`. For $Q=0$, $\hat\sigma=\sqrt{\overline{z^2}}$ under the same positive-sum condition; otherwise the normal likelihood is unbounded. No external face occurs with Q fixed.

### sigma and Q held, mu free

**Proved:** the unique maximizer exists for every sample, including constants. For nonzero Q it is $\bar y+(\sigma/Q)K_y(Q/\sigma)$; at zero it is $\bar y$. No concentration or external face is available.

## Coefficient accuracy near Q = 0

Use the contract $|\hat\theta-\theta_*|\le\mathrm{tol}\max(|\theta_*|,s_\theta)$ with proposed natural floors $s_Q=1$, $s_\mu=\sigma_*$ and relative accuracy for positive $\sigma$. For a signed-profile root, maintain a sign-confirmed root bracket with a proved unique local root, enclose the inner gamma solution, and propagate the whole bracket through the three reconstruction formulas. Accept only when each resulting coefficient enclosure meets its own floor-scaled tolerance, including roundoff and stable evaluation margins. Near zero evaluate the continuous expansions or equivalent stable functions: $Q=t+O(t^2)$, $\sigma=s+O(t)$, $\mu=\bar y+s t/2+O(t^2)$. For example, $|t|\le\mathrm{tol}$ alone does **not** justify snapping to Q=0 unless the enclosed Q, mu and sigma errors all satisfy their contracts and zero is actually an admissible reported maximizer. At Q=0 establish the signed root and winner, including degenerate curvature cases; a small objective gap or the parent's $10^{-8}$ face threshold cannot establish $10^{-10}$ coefficient accuracy.

Alternatively use the contract's positive-definite observed-information Newton-step criterion in the original smooth coordinates, with the last steps establishing quadratic convergence and all three floor scales checked. This is local, not a global winner certificate. Flat/multiple roots, loss of sign or cancellation require a better formulation or a real search/accuracy failure. The global certificate's budget exhaustion alone instead invokes the uncertified fallback. **Open:** an explicit uniform floating-point error bound for the small-Q density, score and reconstruction and a bracket implementation for all flat seam cases; TASK-176's stable density work is a dependency, not a change made here.

## Inference regimes

At $Q=0$, with $Z=(Y-\mu)/\sigma\sim N(0,1)$, differentiating the smooth log density gives the per-observation scores

$$S_\mu=Z/\sigma,\quad S_\sigma=(Z^2-1)/\sigma,\quad S_Q=-Z^3/6.$$

Hence

$$I(\mu,\sigma,0)=\begin{pmatrix}1/\sigma^2&0&-1/(2\sigma)\\0&2/\sigma^2&0\\-1/(2\sigma)&0&5/12\end{pmatrix},\qquad \det I=1/(3\sigma^4)>0.$$

The efficient shape information after free location and scale is $1/6$. **Proved:** Q=0 is not an information singularity; every nonempty held-coordinate principal submodel is full rank. The density has common support and is DQM there: split the square-root likelihood expansion into a bounded standardized-data region and tails; the log-gamma moment generating function provides uniform exponential moments in a fixed neighborhood of zero Q, controlling the score and remainder tails, while the local Taylor expansion provides the interior limit. For nonzero finite Q and positive sigma, gamma log/power moments are finite and the score functions are linearly independent, giving regular full-rank finite interior models and their coordinate submodels. The numerical integrations below support the zero-point matrix but do not prove DQM.

Under correct specification and the additional consistency, local likelihood and optimization conditions of the [inference plan](../../roadmap/2026-10-30-inference-and-dqm.md), regular interior asymptotic-normal and ordinary likelihood-ratio reasoning can apply; testing $Q=0$ is an interior hypothesis, not automatically a boundary mixture. This analysis does **not** prove global MLE existence or consistency for every sample, provide a production score/Hessian/OPG implementation, validate a bootstrap, or establish inference at an escaping power/Pareto fit. Escapes, unbounded samples, near-singular observed information, and uncertified or insufficiently accurate optimizers must be recorded separately; limit-model endpoints have their own moving-support inference regimes.

## Dossier-ready starting entries

These entries use proposed fitter status names, to be synchronized with the actual enum and wrapper. `numerical: true` means detection requires a comparison, not that the limiting law is conjectural. Held-mu faces are included only where the fitter can prove a strict winning, unattained limit; the joint-corner issue above remains open. Q=0 is a member and must not appear as a limit failure. `holds: []` declares no structural exception; all sample-dependent outcomes belong in `limits`, not `holds` (A7).

```yaml
limits:
  - model: power
    firmness: proved
    when: '$Q\to+\infty$ and $\sigma Q\to d>0$, with $\mu$ tending to a finite endpoint'
    detect: 'The power face is proved to be the strictly winning unattained likelihood limit'
    status: PowerBoundary
    message: power
    numerical: true
  - model: pareto
    firmness: proved
    when: '$Q\to-\infty$ and $\sigma|Q|\to d>0$, with $\mu$ tending to a finite endpoint'
    detect: 'The Pareto face is proved to be the strictly winning unattained likelihood limit'
    status: ParetoBoundary
    message: Pareto
    numerical: true
  - kind: nonexistence
    name: unbounded likelihood
    firmness: proved
    when: 'The free location and scale allow concentration at a common positive observation'
    detect: 'All observations are equal and both $\mu$ and $\sigma$ are free'
    status: Unbounded
    message: unbounded
  - kind: nonexistence
    name: unbounded likelihood at held location
    firmness: proved
    fixed: [mu]
    when: 'The scale is free and all observations equal $e^\mu$'
    detect: 'Every observation equals the held-location mode and $\sigma$ is free'
    status: Unbounded
    message: unbounded
holds: []
```

The power/Pareto entries include free and held mu only when sigma and Q are free. Add `fixed`-scoped entries if implementation or conformance tooling needs separate fixed-endpoint declarations. The exact `message` spelling and `Unbounded` enum must match the eventual wrapper. These entries are contingent on the unresolved global comparisons, not authorization to stop merely because a face is close.

## Reproducible numerical evidence and checks

Run `TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-10-gg-theory-data/check.py`. The 70-digit mpmath checks agree between the signed profile and direct Prentice likelihood at $t=\pm10^{-4},\pm10^{-6}$, approach the same slope $-0.06597839\ldots$, verify the zero-shape scores and information determinant $1/3$ at $\sigma=1$, and compare both fixed-mu endpoint paths at $|Q|=1000$ (log-density error about $1.32\cdot10^{-5}$). They also check positive fixed-sigma mode deficits at $Q=0.1,1,10$. These are spot checks, not global searches or proofs.

Final checks, all on the workstation via the slot locks: `just cpp-test` passed 106/106; `just cpp-test-no-long-double` passed 130/130; `just test` passed 23,031, skipped 154, with zero failures and warnings. Simbox attempts for each suite returned 75, unreachable; the required local fallback was used. No R full suite is required for this analysis-only change to an evidence script and report. No fitter, d/p/q/r, test infrastructure, dossier, public signature, shared core or binding was changed, so JS and site checks are not triggered. GCC 13 simbox validation remains unavailable, rather than a claimed pass.

The repository-wide Markdown link checker reported 47 broken destinations in pre-existing files; it reported none in this report. These unrelated link failures were not changed. The artifact-pane `show` command was attempted, but the viewer rejected this worktree as outside its configured readable roots.
