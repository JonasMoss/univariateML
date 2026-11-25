# Log-gamma existence, uniqueness, and limit faces

TASK-359 · 2026-11-25 · Worktree base `338f045b`

**Proved.** The fit with both coefficients free has a unique finite MLE exactly when the valid sample contains two distinct values. With either coefficient held, every valid sample has a unique finite MLE, including a single observation. With both coefficients held there is one feasible point. The only likelihood escape that can win is the all-free collapse on an exactly constant sample, where the likelihood is unbounded. There are no finite/finite or finite/face maximizing ties.

**Numerical.** Archived public fits pass the default coefficient tolerance on selected samples covering all four hold masks. A new source-formula replay on the exact sample `(1 + 2^-52, 4 repeated 10 times)` finds a 1.11% coefficient error from rounding the Jensen gap. Replacing 4 by 16 makes that formula's gap infinite. Both samples have unique, well-representable finite maximizers. These findings concern the computation of the likelihood statistics and leave the dossier's existence claims intact.

The [existence audit](2026-11-02-existence-6-report.md) already established the mask results by transformation, and the dossier now records the constant-data limit that audit requested. The [Lean gamma evaluation](2026-11-25-lean-gamma-report.md) verifies the underlying gamma statements. The [transformed-fitter audit](2026-11-10-audit-transform-report.md) established the importance of exact-input sufficient statistics, public-coordinate restoration, and separate checks of reported likelihood. This report uses those results to classify joint escapes and to exercise a different statistic failure. The [tie outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter) applies to exact maximizer sets, with root count, attainment, and accuracy checked separately. [Log-gamma dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/README.md)

## Exact likelihood and decisions by hold mask

**Proved.** Use a nonempty sample of finite $x_i>1$ and finite, strictly positive held coefficients. Put $y_i=\log x_i$, $m=\overline y$, $b=\overline{\log y}$, $s=\log m-b$, and $C=b+m$. Shape is $k$, rate is $\lambda$, and $\ell$ is the total log likelihood. Set $r=\lambda m/k$, $h(r)=r-1-\log r$, and $N(k)=k\log k-k-\log\Gamma(k)$. The gamma likelihood with the Jacobian of $y=\log x$ gives the identity
$$
\ell(k,\lambda)/n = N(k)-ks-C-k h(r).
$$
Because $h'(r)=1-1/r$, the penalty is nonnegative and vanishes exactly at $r=1$. Strict Jensen equality gives $s=0$ exactly when all $x_i$ coincide, since log is one-to-one. Test that equality on the original observations.

| Held coefficients | Exact condition and maximizer | Fitter outcome after the numerical checks |
|---|---|---|
| Neither | Nonconstant sample. Solve $D(k)=s$, where $D(k)=\log k-\psi(k)$, then set $\lambda=k/m$. One root and one maximizing rate. | Singleton estimate |
| Neither | All observations equal, including $n=1$. The profile increases without bound. | `Degenerate`, `nonexistence(Unbounded)` |
| Shape $k$ | Every valid sample. $\widehat\lambda=k/m$ is the unique rate with $h(r)=0$. | Singleton estimate |
| Rate $\lambda$ | Every valid sample. Solve $\psi(k)=\log\lambda+b$, with a unique positive root. | Singleton estimate |
| Both | Every valid sample. The held pair is the sole feasible point. | Fixed-pair likelihood evaluation |

For the all-free profile the derivative is $D(k)-s$. The established gamma monotonicity gives $D$ strictly decreasing from $+\infty$ to zero. For a held rate the log-likelihood derivative per observation is $\log\lambda+b-\psi(k)$, strictly decreasing from $+\infty$ to $-\infty$ since $\psi'(k)>0$, $\psi(k)\sim-1/k$ at zero and $\psi(k)\sim\log k$ at infinity. Each root therefore has a strict increase followed by a strict decrease, and the conditional rate optimizer is unique. For example, `(2, 2)` gives unboundedness only with both free, and `(2, 4)` gives a singleton under every mask. Ties within a nonconstant sample leave the verdict unchanged. Invalid or empty data, including `(1)`, are `InvalidInput` for the fitting API.

The $k=1$ submodel is Pareto on $x>1$ with the same rate as its tail exponent. It is an attained interior shape of log-gamma. With both free its optimized member is also the joint maximum exactly at $s=D(1)$, in which case the parameter pair is the same singleton. With held rate the corresponding condition is $\log\lambda+b=\psi(1)$. These are identities of the fitted member, with no separate limit comparison.

## Faces and equality along escapes

**Proved.** At the two shape ends, the gamma expansion at zero and Stirling's formula give
$$
N(k)=\log k+o(1) \quad(k\downarrow0),\qquad
N(k)=\tfrac12\log k-\tfrac12\log(2\pi)+O(k^{-1})
\quad(k\to\infty).
$$

- At $k\downarrow0$, the profile bounds every rate path from above by a quantity tending to $-\infty$.
- At $k\to\infty$ on a nonconstant sample, the term $-ks$ forces the profile to $-\infty$ uniformly in rate.
- On every compact positive interval of shapes, a rate tending to zero or infinity makes $h(r)\to\infty$ uniformly. The log likelihood tends to $-\infty$. For nonconstant data these three bounds also control paths that alternate between faces. Every finite upper-level set is contained in a compact subset of the positive quadrant.
- On constant data with held rate, $k\to\infty$ makes $r=\lambda m/k\to0$ and $k h(r)$ grows as $k\log k+O(k)$. Thus the held-rate upper face loses even at $s=0$. With held shape only the two rate ends are available and both lose. Both-held fits have no face.

The remaining joint escape has $s=0$ and $k\to\infty$. Write $A=k h(r)-\tfrac12\log k$. The log likelihood per observation equals $-A-C-\tfrac12\log(2\pi)+o(1)$. It tends to $+\infty$, a finite value, or $-\infty$ as $A$ tends to $-\infty$, a finite value, or $+\infty$, respectively. The same identity governs the subsequential limits of oscillating paths. Any such sequence with the log likelihood bounded below has $h(r)=O(\log k/k)$ and therefore $r\to1$. The path $r=1$ gives unboundedness for every constant sample, so a finite level obtained by detuning the rate has no bearing on the outcome. More explicitly, for $r-1$ of order $\sqrt{\log k/k}$ or smaller,
$$
A = \tfrac12\big[k(r-1)^2-\log k\big]+o(1).
$$
This follows from $h(1+d)=d^2/2+O(d^3)$, since $k|d|^3\to0$ in this range. The critical equality layer includes paths with $k(r-1)^2-\log k\to0$, whose limiting mean log likelihood is $-C-\tfrac12\log(2\pi)$. On the winning path $\lambda=k/m$, the sampling law tends weakly to the point mass at the common $x_0=e^m$. This law is excluded from the continuous family. Report the reason for nonexistence, with no dispatch to another density fitter.

**Proved weak limits.** For clarity about endpoints where the likelihood vanishes, compactify the sampling space to $[1,\infty]$. At $k\to\infty$, $Y/(k/\lambda)$ concentrates at 1 by Chebyshev since its variance is $1/k$. A limit of $k/\lambda$ in $[0,\infty]$ therefore gives a point mass at $\exp(\lim k/\lambda)$, with the endpoint conventions. When shape tends to a strictly positive finite value, scaling a unit-rate gamma variable gives endpoint masses at 1 for $\lambda\to\infty$ and $\infty$ for $\lambda\to0$. At $k\to0$ with rate bounded below the mean of $Y$ tends to zero. At $k,\lambda\to0$, if $k\log(1/\lambda)\to c\in[0,\infty]$, the Laplace transform $(1+t/\lambda)^{-k}$ tends to $p=e^{-c}$ for every $t>0$. In the compact coordinate $Z=e^{-Y}=1/X$, the limit has all positive integer moments $p$. In particular $E[Z-Z^2]=0$ forces endpoint support, with weight $p$ at $Z=1$. Hence the joint small-coefficient face can give $p\delta_1+(1-p)\delta_\infty$. Taking subsequences in the extended parameter coordinates gives these cases for any escaping sequence with a weak limit. The uniform profile bounds above exclude likelihood competition from all of them on nonconstant data.

## Public coefficients and source-formula checks

**Numerical.** The [checker](2026-11-25-lgamma-math-data/check.py) reads archive inputs and frozen public output from native and double-only snapshots at `a1166cbf`, and the historical native restoration row from TASK-101.7.5. It solves the exact binary64-input likelihood at 180 and 360 decimal digits, with reference agreement within `1e-90` after scaling by the larger of unit magnitude and the reference magnitude. It evaluates each archived log likelihood at the actual published pair, with the holds interpreted as exact binary64 values. These are checks of existing public output. The fresh examples below replay individual source formulas in Python and use independent high-precision roots. The C++ entry point was not executed on those new examples.

| Public rows | Largest relative free coefficient error | Largest scaled returned-point likelihood error |
|---|---:|---:|
| Native and double-only `004428, 004431, 004435` (both free) | $2.71\times10^{-16}$ | $1.85\times10^{-16}$ |
| `004444` (held shape), both modes | $7.12\times10^{-17}$ | $5.24\times10^{-18}$ |
| `004460` (held rate), both modes | $2.53\times10^{-13}$ | $6.58\times10^{-17}$ |
| `004476` (both held), both modes | No free coefficient | $1.07\times10^{-16}$ |
| Native restoration sample $2(1+j2^{-52})$, $j=0,1,2,4,7$ | $4.41\times10^{-16}$ | $2.79\times10^{-16}$ |

The held coordinates in those rows match exactly. The restoration sample is flagged uncertified and its log likelihood at the public coefficients is $0.00032436$ below the continuous maximum. Small coefficient errors and a noticeable likelihood loss can coexist. In exact arithmetic, all of the loss from the rate at a given shape is $nk h(r)$. Near $r=1$ this is $nk(r-1)^2/2$ to leading order. [Accuracy contract](../../design/accuracy-contract.md)

### Rare observation near the lower endpoint

**Proved identity and numerical source replay.** The centered computation uses $d_i=(y_i-y_0)/y_0$, $M=1+\bar d$, and $u_i=(d_i-\bar d)/M$. Then $s=\overline{h(1+u_i)}$. In the branch $|\bar d|<0.1$, the shared helper calls `phi(u, 1+u)`. The scalar $1+u_i$ has already lost relative accuracy when a negative outlier makes $u_i$ close to $-1$. At a floating-point $u_i=-1$ the computed penalty is infinite even though the true $y_i/m>0$. Centering is useful for small $u_i$ and does not protect this end of the formula. [Shared transformed gamma fitter](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/transformed.hpp)

For one observation $1+2^{-52}$ and ten observations equal to 4, the exact gap is $3.21107960647707854$ and the binary64 replay gives $3.25306352469398656$. Solving with these two gaps gives, respectively,
$$
(k,\lambda)\simeq(0.224970924694730048,\ 0.178510440570710308)
\quad\text{and}\quad
(0.222463542683780585,\ 0.176520877394664266).
$$
The resulting 1.1145% relative errors in both coefficients greatly exceed the default tolerance. Bracketing the shape score using the erroneous gap can therefore certify the wrong root. The rate round-trip check also cannot catch this error because even the displaced root has rate $k/m$. At the displaced root the correct log likelihood is lower than the true peak by about $0.00057667$. The restoration test uses the same erroneous gap in its reference value, so its reference is about $0.10274$ **below** the correctly evaluated likelihood at that point. Its one-sided loss check can pass. Even if the ordinary delegate finds the correct root, the existing agreement gate would reject a discrepancy this large and retain the raw result. [Public fitter](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/mle.hpp)

For the exact sample with ten 16s instead, the replay gives $d_i=u_i=-1$ for the outlier. A nonfinite gap routes to `TransformFailure` before an ordinary delegate is tried. The 360-digit grid uses this one outlier with anchors $4,16,2^{16},2^{128},2^{512},2^{1023}$ and anchor counts $10,15,31,127$. Four of the 24 replayed gaps give finite roots outside tolerance and 20 are nonfinite. For $|u_i|$ away from zero use $u_i-\log(y_i/y_0)+\log M$, forming the log ratio independently of the rounded sum $1+u_i$. The current code already has an independently formed `logdelta` in this region. Keep the cancellation-free local series at small $u_i$. This substitution gave a largest relative gap error of $1.99\times10^{-16}$ on this grid. Both exact 11-observation examples should give singleton estimates with the corrected statistics.

### Representation and evaluation

**Numerical with exact input and nearest-value criterion.** With held $k=1$ and sample `(2)`, the best positive binary64 rate is `0x1.71547652b82fep+0`, $1.41\times10^{-17}$ relatively from $1/\log 2$. The request `tol=1e-20` is impossible, but the current mean-error gate allows $128\epsilon$ above the requested relative tolerance. A closed form that rounds normally can pass that gate even when no valid representation meets the request. With held $k=2^{-1074}$ and sample `(4)`, the best rate is $2^{-1074}$, with exact relative error $\log4-1\simeq0.38629436$ since $1/\log4$ is between $1/2$ and 1. The default tolerance is impossible there too. Both cases call for a numerical tolerance diagnosis, with the attainable relative error in public rate units.

**Proved overflow mechanism with numerical reference.** For the fully held sample `(4)` at $(k,\lambda)=(2^{-1074},1)$, the exact finite log likelihood is about $-747.539294903599325$. In the source's likelihood expression the ratio $z=\lambda\log(4)/k$ is too large for binary64. The log-ratio fallback still attempts the unscaled `expm1(logz)`, with log ratio about $744.77$, and scaling an infinite penalty by $k$ cannot recover the finite product. In that range use the exact identity $k h(z)=\lambda y-k-k\log z$, or evaluate the original sufficient-statistic likelihood with range-aware products. Retain the centered formula near $z=1$, where the expanded expression cancels. The expected outcome is a successful evaluation of the supplied pair. This example is specifically about the C++ fitting path. The earlier audit separately records the protected density-kernel discrepancy and the R fully held shortcut.

### Final run

The foreground workstation command `/tmp/umvl-cpu/run python3 -u dev/research/evaluations/2026-11-25-lgamma-math-data/check.py` exited 0 with Python 3.14.5. It checked 13 archived public rows including the paired modes, the 24-sample source-formula grid, and the three representation or evaluation cases. No builds were run.

## What this changes for the fitter

1. Use raw equality for the all-free `Degenerate` stop. On valid data the other three masks always have a feasible singleton, and a nonconstant free fit needs no further face search, tie comparison, or sample-size restriction. Report a certified singleton once coefficient and likelihood checks pass. The all-free constant collapse stays `nonexistence(Unbounded)`.
2. Fix the centered gap's far-outlier evaluation using the independent log ratio. Keep a bracket with signed monotone comparisons after allowing for the error in the statistic and special functions. A finite but distorted gap can refute the old candidate, and a nonfinite computed gap for these samples establishes an arithmetic failure only. Include both exact 11-point samples as regressions.
3. Carry the root and mean bounds through to the independent public coordinates. If $k\in[K_-,K_+]$ and $m\in[M_-,M_+]$, the free rate lies in $[K_-/M_+, K_+/M_-]$. For held shape use its exact value for both shape bounds. For held rate copy the exact held value and bracket only shape. Compare each published candidate to the endpoints of its own interval within the requested relative tolerance, including candidates accepted by the ordinary delegate. Test the distance to the nearest feasible double when a closed-form rate has a representation limit. A global certification flag cannot waive coefficient accuracy. A successful accuracy-checked candidate may still be retained with the uncertified flag when only the likelihood certificate fails to transfer.
4. Evaluate the returned pair with a range-aware likelihood and retain the actual public-point value when the likelihood is steep. Use the extreme fully held example and both tolerance examples as numerical acceptance cases. The protected density kernel and generated R route would need their own checks under the likelihood contract.

## Open

- **Implementation evidence for the new examples.** Confirm the new cases through the public C++ fitter in native and double-only modes and through the binding paths once a fix is made. The Python replay establishes the vulnerable operations and their scale. It does not measure current compiled return values or prove a uniform floating-point margin for the proposed switch. A practical error allowance across the branch switch, the log-ratio evaluation, and the monotone root bracket remains to be established.
- **Density agreement at extreme public pairs.** The protected density seam from TASK-101.7.5 remains. A stable normalizer and centered raw residuals need a density-level check at concentrated returned coefficients, alongside range tests at extreme held coefficients and the R fully held shortcut. The exact likelihood formula gives a reference for those checks.
