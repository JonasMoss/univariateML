# Nonstandard beta: existence, uniqueness and endpoint faces

TASK-362 · evidence against `338f045b` · 2026-11-25

**Proved.** Across the sixteen hold masks, whenever the likelihood has a positive, finite maximum under the dossier's closed density convention, its maximizer is unique. The other cases are a support violation, an identically zero likelihood, or an infinite supremum. Some infinite suprema come from the prescribed pointwise endpoint density. No bounded positive-likelihood case requires comparison with a competing excluded face. The current fitter implements the four masks with both endpoints held and requires strict interior support even there.

**Accuracy.** For the exact sample $(0,1/8)$ with endpoints $(-2^{50},1)$ the current affine transform collapses the two observations. Source inspection then implies a false `Unbounded` with both shapes free. Holding `shape2 = 1` instead gives a finite delegated root about 6.25% below the actual root. A proposed mixed endpoint fit has a related restoration hazard: on $(1,1+2^{-52})$, refitting its free shape at the first outward-rounded endpoint gives about 52.86% relative error. The archived successful fits and the new numerical experiments are distinguished below.

The existence results for free endpoints build on the November threshold report. Its eleven partial-hold rows omit the all-free case, whose spike was already proved in the October report. The fixed-endpoint reduction and the older normal limit come from the P0 analysis. The additions here are the complete dispatch including fixed-endpoint contact, the bounds excluding competing faces on the bounded masks, and physical-coordinate accuracy checks. [Threshold proofs](2026-11-25-thresh-math-report.md#beta-with-both-endpoints-free) · [Original threshold analysis](2026-10-29-threshold-report.md#mathematics-proved) · [P0 beta analysis](2026-10-29-p0-genbeta.md#nsbeta)

## Likelihood and contact convention

Write the endpoints $L<U$, width $W=U-L$, and shapes $\alpha,\beta>0$ at the lower and upper ends. For $u=(x-L)/W\in(0,1)$ the density is
$$
f(x)=\frac{u^{\alpha-1}(1-u)^{\beta-1}}{W\,\mathrm B(\alpha,\beta)}.
$$
At either contact the dossier gives density $+\infty$, a positive finite value, or zero according as the local shape is below, equal to, or above one. The finite value is the opposite shape divided by $W$. A finite result here means a proper parameter vector and a positive finite likelihood.

**Proved dispatch under the specified convention.** Validate a nonempty finite sample, valid held coefficients, and the order of two held endpoints. An observation strictly outside any held bound is incompatible with the support. Test equality with each held endpoint on the original data.

1. A contact whose local shape is held above one imposes a zero factor for every compatible parameter vector. The positive-likelihood problem has no solution. With another factor infinite at a simultaneous contact, the product is undefined there. Follow the threshold report's convention of requiring positive support at the held endpoint, including when taking limits at a free endpoint with the held coordinate fixed. For $x=(0,1)$, $L=0,U=1,\alpha=2,\beta=1/2$, this is the zero-times-infinity case. With $\beta=1$ the product is simply zero.
2. Otherwise a contact with a free local shape or with a local shape held below one gives an infinite likelihood by choosing the free local shape below one and making every other contact neutral or singular. With a free opposite endpoint, place it strictly beyond the data. Report `Unbounded` with the held-contact reason. For $x=(0,1/2)$, $L=0,U=1,\alpha=1/2$ and $\beta$ free, every positive $\beta$ gives an infinite likelihood.
3. Any remaining held contact has its local shape fixed at one. Retain those observations. They supply the positive finite endpoint density and may affect the optimizer.

The zero case can have trivial extended-real ties. For $x=(0,1/2)$, $L=0,\alpha=2,\beta=1$ and $U$ free, both $U=1$ and $U=2$ are among the infinitely many feasible values with likelihood zero. An implementation should stop with the held zero-density reason. Neither this case nor the infinite example is a tie between finite successful fits. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md) · [Pointwise endpoint density](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/dpqr.hpp)

## Decisions by hold mask

**Proved.** Apply the contact checks first. A sample is constant if its original minimum and maximum agree, including for $n=1$. The entries below use the remaining positive-support cases after the checks. Each row giving a finite solution gives one maximizer.

| Held coefficients | Exact rule |
|---|---|
| none, $\alpha$ only, or $\beta$ only | `Unbounded`. At least one free endpoint has a free shape. |
| $\alpha,\beta$ | `Unbounded` if either is below one or the data are constant. Otherwise attain both free endpoints, with contact at an end exactly when its shape is one. |
| $L$ only or $L,\alpha$ | `Unbounded` from the free upper shape, except that an all-held-contact sample with neutral lower shape uses width collapse. |
| $U$ only or $U,\beta$ | Reflection of the preceding row. |
| $L,\beta$ | `Unbounded` if $\beta<1$ or the data are constant. Otherwise attain $U=\max x$ when $\beta=1$, and $U>\max x$ when $\beta>1$. |
| $U,\alpha$ | Reflection of the preceding row. |
| $L,\alpha,\beta$ | `Unbounded` if the largest distance from $L$ is zero or if $\beta<1$. Otherwise attain $U=\max x$ when $\beta=1$, and $U>\max x$ when $\beta>1$. Positive constant distances are allowed. |
| $U,\alpha,\beta$ | Reflection of the preceding row. |
| $L,U$ | Remaining data are strict interior. `Unbounded` exactly for constant data. Otherwise fit the two shapes. |
| $L,U,\alpha$ or $L,U,\beta$ | Fit the free shape unless every observation is at the neutral held contact. In that case the density is the free shape divided by $W$ at every observation and the supremum is infinite. |
| $L,U,\alpha,\beta$ | Evaluate the supplied vector, including neutral contacts. |

For an isolated free-end spike, set its local shape to any fixed value $s\in(0,1)$ if that shape is free. Approach its sample extreme from outside by a gap $\varepsilon$ while keeping the opposite end fixed, away from any non-neutral contacts. The $k\ge1$ observations at the spiking extreme contribute a factor proportional to $\varepsilon^{k(s-1)}$, and every other factor tends to a strictly positive finite value. For an exact example take $x=(1/2,1,1)$, $L=0$, $\alpha=1,\beta=1/2$, and $U=1+\varepsilon$. If the entire sample is at the held endpoint of shape one, use $W\downarrow0$ with the free-side shape fixed, giving a sample likelihood proportional to $W^{-n}$. With two free endpoints and held shapes both at least one, constant data also allow width collapse by keeping the common observation at beta coordinate $1/2$. [Prior spike and collapse proofs](2026-11-25-thresh-math-report.md#beta-with-both-endpoints-free)

### Both endpoints held

**Proved.** Let $u_i=(x_i-L)/W$. With one shape free, let $v_i=u_i$ for free $\alpha$ and $v_i=1-u_i$ for free $\beta$, and write $c$ for the other, held shape. After the contact checks $v_i\in(0,1]$. The mean score for the free shape $s$ is
$$
\psi(s+c)-\psi(s)-T,\qquad T=-\overline{\log v_i}.
$$
The increment is strictly decreasing from infinity to zero by the positive decreasing trigamma (or its positive integral representation). Thus $T>0$ gives exactly one root with opposite exterior signs. $T=0$ holds exactly when all $v_i=1$. For example, hold $L=0,U=1,\alpha=1$ and fit $\beta$. The sample $(0,1/2)$ gives $\beta=2/\log 2$, while $(0,0)$ gives an infinite supremum. Strict interior constant data such as $(1/2,1/2)$ give a root with one shape held, but concentration gives an infinite supremum when both are free. In the latter case shapes $tu,t(1-u)$ have density asymptotic to $\sqrt{t/(2\pi u(1-u))}/W$ at the common beta coordinate $u$. [Fixed-endpoint reduction and constant-data proof](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md#details)

For two free shapes and nonconstant strict interior data, the covariance of $(\log u,\log(1-u))$ under every positive beta density is positive definite. A linear combination constant on $(0,1)$ would have derivative identically zero, forcing both coefficients to vanish. This gives strict concavity. For an explicit compactness bound write $(\alpha,\beta)=t(p,1-p)$ with $p\in(0,1)$ and set $g_p(v)=p\log v+(1-p)\log(1-v)$. At $\bar u$, strict Jensen gives $g_p(\bar u)>\overline{g_p(u_i)}$ for every $p\in[0,1]$. By continuity and compactness in $p$, some fixed interval $I\subset(0,1)$ around $\bar u$ and some $\eta>0$ satisfy $g_p(v)\ge\overline{g_p(u_i)}+\eta$ for all $v\in I$ and all $p$. Integrate the beta normalizer over $I$ using $e^{t g_p(v)}\,dv/[v(1-v)]$. The mean log likelihood is then bounded above by a fixed constant minus $t\eta$. If the sum of the shapes stays bounded and one tends to zero, the beta integral diverges at the corresponding endpoint, uniformly with the other shape bounded above, while the sample log terms stay bounded. Thus every superlevel set through a finite value is confined to a compact subset of the positive quadrant. There is one maximum and no tied shape face. [Prior exponential-family reduction](2026-11-25-transfer-math-report.md)

### Both endpoints free, shapes held

**Proved in the threshold report.** For nonconstant data and $\alpha,\beta\ge1$, use $m=\min x$, $M=\max x$, \(A=(m-L)/W\) and \(B=1/W\). With $u_i=A+B(x_i-m)$, the objective apart from the fixed beta normalizer is
$$
n\log B+(\alpha-1)\sum_i\log u_i+(\beta-1)\sum_i\log(1-u_i).
$$
A neutral term is zero even at contact. The feasible polygon is $0<B\le1/(M-m)$, $0\le A\le1-B(M-m)$. If $C$ is the value of the displayed objective at any feasible finite witness, a maximizer must have $B\ge e^{C/n}$ because both log sums are nonpositive. This gives a compact cover of every competing point or face. Strictness in $B$ comes from $\log B$, and at fixed $B$ a positive log coefficient gives strictness in $A$. For $\alpha=\beta=1$ the largest feasible $B$ forces $A=0$. If only $\alpha=1$, the objective at fixed $B$ decreases in $A$ and sets $A=0$; if only $\beta=1$, it increases in $A$ and sets $A=1-B(M-m)$. A shape above one excludes contact by its zero density. The objective tends to minus infinity on a forbidden contact or on $B\to0$. For $x=(0,1)$ and $(\alpha,\beta)=(1,1)$, the unique support is $[0,1]$. With $(1,2)$ the support is $[0,2]$, by the width formula below, so only the lower endpoint touches. [Concavity proof](2026-11-25-thresh-math-report.md#beta-with-both-endpoints-free)

### One endpoint held

Reflect about the held endpoint as needed. Write $z_i\ge0$ for distances toward the data, $Z=\max_i z_i$, $W$ for the width, $a$ for the shape at the **free** endpoint and $b$ at the **held** endpoint. The finite-width body is $W>Z$. Contact $W=Z>0$ is admitted at $a=1$. A zero distance can survive the held-contact checks only at fixed $b=1$.

**Proved in the threshold report, with explicit bracket equality.** For held $a\ge1,b>0$ and $Z>0$, the mean score in $\log W$ is
$$
-b+(a-1)\overline{\frac{z_i}{W-z_i}}.
$$
For $a=1$ the objective strictly decreases and $W=Z$ is feasible. For $a>1$ the score strictly decreases from infinity to $-b$. Its root is at or below $Z[1+(a-1)/b]$, by bounding each fraction by $Z/(W-Z)$. Equality of the root with that bound holds exactly when all $z_i=Z$. For example with $L=0$, $x=(1,1)$, $\alpha=\beta=2$, the unique upper endpoint is $3/2$. With $L=0$, $x=(0,1)$, $\alpha=1,\beta=2$, it is $2$. If $Z=0$ and the held contact is neutral, $W\downarrow0$ is unbounded.

**Proved in the threshold report, with bounds on competing tails below.** When $b$ is free, the surviving data have all $z_i>0$. If their common distance is $z>0$, the path $W=z(1+\varepsilon)$, $b=1/\varepsilon$ has per-observation density asymptotic to $e^{-1}/[z\Gamma(a)\varepsilon]$ for any fixed $a>0$. This follows from $\mathrm B(b,a)\sim\Gamma(a)b^{-a}$ and $(1+\varepsilon)^{-1/\varepsilon}\to e^{-1}$. For example, hold $L=0,\beta=5/2$ and use $x=(1,1)$.

Otherwise $G=\exp(\overline{\log z_i})<Z$. Put $h=\log(W/G)$ and $h_0=\log(Z/G)>0$. The unique conditional optimizer solves
$$
\psi(b+a)-\psi(b)=h.
$$
It decreases in $h$. At $a=1$ the profile score is $-b(h)$ and the exact fit is $W=Z,\ b=1/h_0$. For example with $L=0$, free $\alpha$, fixed $\beta=1$ and $x=(1,2)$, return $U=2,\ \alpha=2/\log 2$. For $a>1$, define $R=(a-1)\overline{z_i/(W-z_i)}$. The profile score in $h$ is $R-b$. Its sign starts positive, and an explicit test for a negative sign is
$$
\frac{(a-1)h}{\exp(h-h_0)-1}<1.
$$
Indeed $R\le(a-1)/(\exp(h-h_0)-1)$ and $b(h)\ge1/h$ by monotonicity of $\psi(b+a)$ in $a$ and $\psi(b+1)-\psi(b)=1/b$. The test eventually passes. At every root,
$$
R'\le-b-\frac{b^2}{a-1},\qquad
-b'=\frac{1}{\psi_1(b)-\psi_1(b+a)}
    < b+\frac{b^2}{a-1}.
$$
For the second bound the positive integrand for the trigamma difference is $e^{-bv}v(1-e^{-av})/(1-e^{-v})$. For $v>0$ it strictly exceeds $e^{-bv}(1-e^{-(a-1)v})$. The latter integrates to $(a-1)/[b(b+a-1)]$. The first bound follows from $r_i'=-r_i-r_i^2$ for $r_i=z_i/(W-z_i)$ and the mean-square inequality at $R=b$. Every root crosses strictly downward, so the opposite exterior signs give exactly one root. The conditional maximizer is unique at each width. [Prior width and mixed-shape proofs](2026-11-25-thresh-math-report.md#beta-with-one-endpoint-held)

## Faces and limit-model decisions

**Proved.** Positive finite contact faces above are attained members. Uniform arises at both shapes one. When one endpoint shape is one, the model measured from the opposite endpoint is a power density, $b z^{b-1}/W^b$ in the one-held-end orientation with $a=1$. These identities give finite estimates with the specified hold mask. A contact at a shape above one gives zero density. The concave polygon bound and the held-end shape bounds above exclude all competing faces on their routes.

For the one-held-end route with both shapes fixed, $W\to\infty$ gives each density of order $W^{-b}$ at any surviving observation, including a zero distance at $b=1$. For the mixed route and nonconstant positive distances, the mean log likelihood is
$$
-\log\mathrm B(b,a)-bh-\overline{\log z_i}+(a-1)\overline{\log(1-z_i/W)}.
$$
At large $h$, $b(h)\sim 1/h$ by the digamma pole, and $\mathrm B(b,a)\sim1/b$ as $b\downarrow0$. Hence the profiled log likelihood is $-\log h-1-\overline{\log z_i}+o(1)$. This is an upper bound in the sense that no joint sequence with $W\to\infty$ can exceed the profile at its own width. For all $W\ge Z$, $h\ge h_0>0$ and the final log sum is nonpositive for $a\ge1$. If $b\downarrow0$, the normalizer term tends to minus infinity; if $b\to\infty$, it grows only as $a\log b+O(1)$ and $-bh\le-bh_0$ dominates. At $W\downarrow Z$, the maximizing shape tends to the finite $b(h_0)$. The contact has finite positive likelihood at $a=1$ and zero likelihood at $a>1$. These bounds exclude correlated width/shape escapes as possible ties with a finite fit.

With more parameters free, reachable proper limiting laws include the P0 normal path with endpoints $\mu\pm R$ and shapes $R^2/(2\sigma^2)$, and a gamma path or its reflection. For the normal path, the density at $\mu$ tends to $1/(\sqrt{2\pi}\sigma)$ by Stirling, and the density ratio at $\mu+x$ is $(1-x^2/R^2)^{R^2/(2\sigma^2)-1}\to e^{-x^2/(2\sigma^2)}$. For the gamma path hold the nearer endpoint and its shape $b$, set $a=W/\theta$, and send $W\to\infty$. At every fixed $z>0$ the normalizer is asymptotic to $\Gamma(b)a^{-b}$ and $(1-z/W)^{a-1}\to e^{-z/\theta}$. The limit density is $z^{b-1}e^{-z/\theta}/[\Gamma(b)\theta^b]$. The sample rules already provide an infinite supremum or a held-support failure on masks admitting these paths. In particular a finite normal or gamma likelihood cannot win against an endpoint spike. A point-mass weak limit at a collapse is not a density comparison against another continuous fit. There is no finite positive unattained winning face in these masks. [Prior normal path](2026-10-29-p0-genbeta.md#endpoint-calculations-and-m2)

## Public coefficient and restoration checks

**Numerical, historical public results.** The Python replay reads five actual archived `Ok` rows from the November beta audit and reconstructs an independent physical-coordinate score at 350 and 450 decimal digits. Input doubles are read as exact dyadics. All five fits held the interval $[0,1]$. Relative free-shape errors for cases 5348, 10791, 10792, 10799 and 11011 were respectively $5.6911$, $5.6730$, $5.6646$, $3.6118$ and $5.6814$, in units of $10^{-14}$. All meet the default $10^{-10}$ contract. These are historical binary outputs. The same audit documents two historical joint-shape results above tolerance (5329 and 10786). [Archived outputs](2026-11-09-audit-beta-data/after.csv) · [Exact binary64 inputs and earlier references](2026-11-09-audit-beta-data/reference.json) · [Earlier accuracy findings](2026-11-09-audit-beta-report.md#open)

**Proved transform defect, with a numerical source-level replay.** Let $D=2^{50}$ and use endpoints $(-D,1)$ and exact observations $(0,1/8)$. In nearest-even binary64, both numerator differences round to $D$ and $W=D+1$ is exact. Both transformed coordinates round to $1-1/D$. The actual coordinates differ and lie strictly inside $(0,1)$. The current delegate's equality check therefore returns `Degenerate`, advertised as `Unbounded`, on a sample with a unique finite two-shape fit. With $\beta=1$, the two exact complements are $1/(D+1)$ and $7/[8(D+1)]$. The physical root is the reciprocal of the mean of their negative `log1p` contributions. The delegate instead uses $-\log(1-1/D)$. The inequality $v\le-\log(1-v)\le v/(1-v)$ bounds the ratio of delegated to physical roots between $0.93749$ and $0.93751$. Numerically the roots are about $1.125899906842624\times10^{15}$ and $1.200959900632133\times10^{15}$, respectively. The script replays binary64 bisection with the exact recurrence $\psi(s+1)-\psi(s)=1/s$ as its score kernel and gives $1125899906842559.5$. This is a replay of the transformed target and stopping rule, without a fresh compiled output from the current header. [Affine transform and outcome mapping](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/mle.hpp) · [Delegate score and equality check](../../../univariateML-cpp/include/cppunivariate/distributions/beta/mle.hpp)

**Numerical, future mixed route.** On $x=(1,2,5,5)$, hold $L=0$ and $\beta=5/2$. The joint solution is $U\approx7.1646716564152353$ and $\alpha\approx1.9384122004010275$. Converting both to binary64 meets the coefficient contract, including under reflection. A conditional refit at the binary64 endpoint changes the shape by less than $10^{-15}$ relative to its joint target on this sample.

For the exact sample $x=(1,1+e)$, $e=2^{-52}$, hold $L=0,\beta=2$. Let $Z=1+e$, $g=(U-Z)/e$, $c=e\alpha$. The two score equations have
$$
c=\frac12\left(\frac1{1+g}+\frac{1+e}{g}\right),\qquad
\frac1c+\frac1{c+e}=
\frac{\log(1+e(1+g))+\log(1+eg/(1+e))}{2e}.
$$
The root gap is $g\approx0.20710678118654755$. Even coarse exact bounds suffice to show $1/5<g<1/4$: if the right side of the second equation is $H$, its solution obeys $2/H-e<c<2/H$. At $g=1/5$ and $1/4$, bound the logs using $v-v^2/2\le\log(1+v)\le v$ and $e<10^{-15}$. Then $|H-(g+1/2)|<2e$. At the two ends the first equation's right side is respectively above $2.91$ and below $2.41$, whereas the conditional $c$ is respectively below $2.86$ and above $2.66$. The width rounds to $Z$, where the density at the maximum is zero. Using the next larger double gives $g=1$. The high-precision refit there changes the free shape from about $1.273810334505155\times10^{16}$ to $6.004799503160662\times10^{15}$, a relative error of about $0.52859548$. A conditional refit therefore fails the original joint coefficient contract in this case.

**Proved width restoration examples.** With $x=(1,2)$, $L=-2^{54}$ and both shapes held at one, the fitted public upper endpoint is exactly $2$. Forming $W=2+2^{54}$ in binary64 and adding it back to $L$ produces $0$ in nearest-even arithmetic. Returning the original sample maximum preserves the exact fit and support.

For a steep but smooth optimum, hold $L=0$, $\alpha=b=2^{55}$, $\beta=2$, and use the one-observation sample $(1)$. The fixed-shape width score gives $U_*=1+1/b$. The closest positive-likelihood binary64 width is $1+8/b$, also the best over all feasible binary64 widths by strict decrease to the right of $U_*$. Its coefficient error is below $10^{-10}$, but its exact log-likelihood loss is
$$
(b+1)\log\!\left(\frac{1+8/b}{1+1/b}\right)-\log 8
\simeq 4.9205584583201634.
$$
Literal nearest rounding gives endpoint $1$ and zero density. An accurate public coefficient here needs support-aware rounding and a likelihood evaluated at the represented width under the steep-optimum rule. [Accuracy contract](../../design/accuracy-contract.md#log-likelihood)

## Reproduction

The final foreground run completed successfully on the workstation through the one-slot lock. It checked all sixteen masks against the dossier and the prior partial-mask table, compared the five archived outputs with high-precision score roots, replayed the exact binary64 transform examples, and solved the two mixed-width systems. The face checks sampled $h=10,100,1000$ at $a=5/2$ for the mixed tail and $\varepsilon=10^{-5},10^{-10},10^{-20}$ for the two collapse examples. The reported numerical results agreed to 50 printed digits across 350 and 450 decimal digits. The compactness and tail decisions rely on the analytic bounds above. [Reproducer](2026-11-25-nsbeta-math-data/check.py)

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
export PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run nice -n 10 python3 -u dev/research/evaluations/2026-11-25-nsbeta-math-data/check.py
```

## What this changes for the fitter

1. Dispatch by the held coefficients and original sample extrema before any affine transform. Keep the structural no-finite-fit masks separate from masks with sample-dependent existence. Emit `Unbounded` for proved spikes or collapse, unsupported sample diagnostics for a held zero-density observation, and invalid input for data strictly beyond a held bound. If fit validation adopts the closed density convention, process neutral shape-one contacts explicitly.
2. With both endpoints held, certify the shapes using score targets computed from original endpoint distances. Form the smaller complement directly, use `log1p` where the fitted coordinate approaches one, scale or retain subtraction residuals as needed, and preserve original dispersion for the two-shape solve. A collapse in transformed storage means transformation or conditioning trouble when the original data are distinct. Propagate target error into the coefficient brackets and honor the caller's tolerance.
3. For free endpoints and both shapes held, use the concave affine problem with its linear constraints or its scalar reductions. A feasible witness provides the positive lower bound on inverse width. Use the signed fixed-shape width score and its explicit right bound for the scalar solve. In the mixed route use the conditional shape bracket and the unique outer width bracket. A width bracket $[W_l,W_r]$ gives $[b(W_r),b(W_l)]$ for the joint shape, with adjustment for numerical errors. A right bound can be found with the explicit negative-score test above. Implement the bracket in terms of the distance from the sample extreme so that a small endpoint gap is retained. Compute $h$ as a mean of log ratios of the width to the observed distances, with `log1p` for small ratios minus one.
4. At shape one, publish the exact data extreme directly. Otherwise restore each endpoint into a support-valid public double and verify its coefficient inclusion against the joint root box. Retain the jointly fitted shape through restoration. A conditional refit at a rounded endpoint needs its own inclusion check against the original joint target. Recompute the density at the public coefficients, including on the steep route where the reported `logLik` follows those coefficients.
5. Each positive finite route earns `certified = TRUE, unique = TRUE` after global proof, physical coefficient accuracy and computed comparison margins are established. A successful accuracy-checked search with an unresolved certificate retains the existing uncertified fallback. A calculation that lands outside the support or misses the proven coefficient inclusion needs a checked replacement or an appropriate numerical failure. The math supplies no positive finite set of tied fits and no finite excluded winner for these masks. [Tie and fallback rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter) · [Wrapper handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- The report uses the dossier's closed pointwise density and the prior threshold convention at a simultaneous held zero and infinite factor. The current API imposes strict interior observations at held endpoints. Enabling closed fitting and choosing the wrapper's specific held-zero reason are implementation decisions. A policy based on joint limits that perturb held coefficients, or that admits endpoint point masses, would require a new likelihood definition.
- Production arithmetic still needs a physical-coordinate score and dispersion repair. The required evidence is a coefficient bracket with a justified error allowance for the original endpoint differences, including finite spans whose subtraction overflows binary64, and a stable likelihood check at restored public values. The new transform result is from source inspection and Python. The five recorded successes come from the archived binary. A compiled regression on the current revision and a targeted replay of the earlier joint-shape concentration cases belong to the implementation job.
- For the future free-endpoint routes the exact theory gives a unique root or concave optimum. Practical work budgets and floating-point margins for bracketing and restoration remain to be measured, especially at near-contact gaps and at user tolerances near the representability floor.
