# Discretized normal MLE existence, uniqueness, and faces

TASK-358 · Base 338f045b

**Proved.** The existing existence predicates cover all four hold masks. Every attained optimum is unique, and every winning excluded face beats every finite fit strictly. A score enclosure in the concave coordinates can certify the public coefficients. The current default Newton decrement can accept a scale more than 7.8% from the MLE even in exact arithmetic. There is also a binary64 path to an Ok result with a NaN location.

## Existence and limiting laws

Let $l=\min x_i$, $u=\max x_i$ and $n$ be the sample size. The cell probability is $p_k=\Phi((k+1-\mu)/\sigma)-\Phi((k-\mu)/\sigma)$. The sample is nonempty and integer-valued, and held coefficients satisfy $|\mu|<\infty$ and $0<\sigma<\infty$. The October analysis and dossier already establish the existence classification by compactness and grouped-normal concavity. The boundary argument below records the full list of escaping laws and the strict comparisons. [October analysis](2026-10-29-p0-discrete2.md#dnorm) · [Implementation report](2026-10-29-dnormgamma-report.md) · [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md) · [Burridge on grouped-data likelihood](https://doi.org/10.1111/j.2517-6161.1981.tb01146.x)

| Held | Exact sample condition | Outcome |
|---|---|---|
| Neither | $u=l$ | Certified PointMass limit at $l$, with log likelihood $0$. |
| Neither | $u=l+1$ | Certified TwoPointLimit. The high-cell probability is its empirical fraction. Use the shifted Bernoulli fit. |
| Neither | $u\ge l+2$ | One finite MLE. Certify the singleton after checking public coefficient accuracy. |
| Mean $m$ | $u\le m\le l+1$ | Certified ScaleLimit. For noninteger $m$, the limit is the point mass at $\lfloor m\rfloor$ with log likelihood $0$. For integer $m$, it gives half mass to each of $m-1,m$, with log likelihood $-n\log 2$. |
| Mean $m$ | $m<u$ or $m>l+1$ | One finite scale MLE. Certify the singleton after checking public coefficient accuracy. |
| Scale | Every valid sample | One finite location MLE. For constant data it is exactly $l+1/2$. |
| Both | Every valid sample | Evaluate at the held pair. The feasible set is a singleton. |

**Proved.** At infinite scale $p_k\le 1/(\sigma\sqrt{2\pi})$ uniformly. Escaping location with bounded scale also sends every fixed-cell probability to zero by the normal tail bound. These escapes send the log likelihood to $-\infty$. The remaining escape, up to subsequences, has $\sigma\downarrow 0$ and $\mu$ approaching a finite $m$. For $m$ in a cell interior the limit is its point mass. At integer $m$, extract a limit $t\in[-\infty,\infty]$ of $(m-\mu)/\sigma$. The masses on $m-1,m$ are $\Phi(t),1-\Phi(t)$ since the outer endpoints diverge. Every split is reachable with mean free. Holding the mean at $m$ forces $t=0$, and holding a positive scale rules out this escape. A sequence with bounded location and scale bounded away from zero and infinity has a finite parameter subsequence.

For constant data any finite fit has log likelihood below zero. For adjacent cells write $M=p_l+p_{l+1}<1$ and $q=p_l/M$. The log likelihood is $n\log M+n_l\log q+n_{l+1}\log(1-q)$. The last two terms are maximized at $q=n_l/n$ and the first is strictly negative. The face with $t=\Phi^{-1}(n_l/n)$ attains the saturated value in the closure. If held mean is in every observed cell closure, the standardized intervals expand with inverse scale, with each cell having at least one endpoint moving strictly outwards. Its mass increases strictly toward its face mass. If any observed closure excludes held mean, that cell's mass vanishes at zero scale. A range of at least two also precludes any proper zero-scale law assigning positive mass to all observations. Thus the finite branches have compact positive likelihood level sets and every competing escape loses. This excludes finite/face equality, and the log likelihood is bounded above by zero. [Tie and outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter)

For $(0)$ at held mean $1/2$ the face is a point mass. At either exact edge $m=0$ or $1$, it is half-and-half. For $(0,1)$ the held-mean face occurs only at $m=1$, where the free and held face laws agree. For seven zeros and three ones, the same hold instead gives $-10\log 2$, while the free face gives $7\log(7/10)+3\log(3/10)$. On $(0,1)$ either exact hold $1\pm2^{-52}$ gives a finite MLE. On accepted inputs both $u-l$ and $l+1$ are exactly representable integers, so these predicates use direct comparisons. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/mle.hpp)

## Strict information and coefficient inclusion

**Proved.** The grouped-normal concavity can be strengthened to positive definite observed information everywhere in the working coordinates $A=(\mu-c)/\sigma$, $B=1/\sigma$, for a fixed center $c$. In one cell let $a=(k-c)B-A$, $b=(k+1-c)B-A$, $p=\Phi(b)-\Phi(a)$, with endpoint velocities $v_a,v_b$ along a line in $(A,B)$. Put $s=(\phi(b)v_b-\phi(a)v_a)/p$. The second directional derivative of the negative log mass equals
\[
 s^2+[b\phi(b)v_b^2-a\phi(a)v_a^2]/p.
\]
Define $w$ by $(\phi w)'=s\phi$ and $w(a)=v_a$. Integration gives $w(b)=v_b$. Squaring $w'-zw=s$, integrating against $\phi$, and integrating the cross term by parts gives
\[
 s^2p=\int_a^b ((w')^2+w^2)\phi\,dz-[z\phi(z)w(z)^2]_a^b.
\]
The second derivative is therefore the positive integral divided by $p$ whenever the endpoint velocities are nonzero. The endpoint map has full rank since $b-a=B$. Summing over the sample preserves strict positive definiteness, including on the restrictions $A=(m-c)B$ and $B=1/\sigma_{\rm held}$. Hence every finite fit with a free coordinate has exactly one stationary point. Distinct finite maxima and flat maximizing sets are excluded. When location is free, reflection symmetry of the histogram fixes its unique location at the reflection center.

**Proved search bounds.** The location score of each cell has the sign of $k+1/2-\mu$ by comparison of $\phi(a)$ and $\phi(b)$. Thus at any $B$ the location root is bracketed by $[l+1/2,u+1/2]$. At this root the profiled negative log likelihood has second derivative $H_{BB}-H_{BA}^2/H_{AA}>0$, where $H$ is the information in working coordinates. The joint fit therefore also has a monotone inverse-scale profile score. A feasible log likelihood $L_0<0$ gives $B\ge \sqrt{2\pi}\exp(L_0/n)$ on its upper level set. For a joint finite fit, the gap $u-l-1>0$ between the extreme cell closures forces at least one cell to be at distance at least $d=(u-l-1)/2$ from any location. For a finite held-mean fit take $d=\max(u-m,m-l-1)>0$. The normal tail bound $p_k\le\exp(-B^2d^2/2)$ for that cell gives $B\le\sqrt{-2L_0}/d$ on the same level set.

**Proved certificate.** Let $g,H$ denote the negative log likelihood gradient and Hessian in the free working coordinates. Choose a box centered at $z$ with radii $r_i>0$ inside the working domain, and a nonsingular matrix $M$, for example the inverse numerical Hessian at $z$. Check
\[
 |(Mg(z))_i|+\sum_j\sup_{\rm box}|(I-MH)_{ij}|\,r_j<r_i
 \quad\text{for each }i.
\]
Integration of the derivative along segments from $z$ proves that $v\mapsto v-Mg(v)$ maps the box into itself and is a contraction in the maximum norm weighted by $r_i$. Its fixed point has score zero and is the unique global optimizer. For the held masks, two separated signs of the monotone scalar score also suffice. Convert the entire enclosure to public intervals $I_\mu,I_\sigma$ by $\sigma=1/B$ and $\mu=c+A/B$ or the relevant hold. Compare with the actual rounded coefficients. A sufficient location check is
\[
 \sup_{v\in I_\mu}|\widehat\mu-v|
 \le \mathrm{tol}\max(\inf_{v\in I_\mu}|v|,\inf I_\sigma).
\]
Use $\mathrm{tol}\inf I_\sigma$ for the scale check and omit held coordinates. This proves accuracy and a unique optimum conditional on the score and information bounds. [Accuracy contract](../../design/accuracy-contract.md)

## Accuracy failures and public coefficient checks

### Held mean just outside a constant cell

**Proved.** Let $\delta>0$ be the distance from the held mean to the nearer edge of the observed cell. Reflecting if necessary, its mass is $p(B)=\Phi((1+\delta)B)-\Phi(\delta B)$. Equating the two terms of its derivative gives the unique scale
\[
 \sigma_*^2=\frac{1+2\delta}{2\log(1+1/\delta)}.
\]
At $B_*=1/\sigma_*$ the negative log likelihood information per observation satisfies
\[
 B_*^2H(B_*)=
 \frac{B_*^3\,\delta(1+2\delta)\,\phi(\delta B_*)}{p(B_*)}
 \longrightarrow 0\quad(\delta\downarrow0).
\]
Indeed $\delta B_*\to0$, $B_*\to\infty$ with order $\sqrt{\log(1/\delta)}$, and $p(B_*)\to1/2$. The Newton decrement has no uniform positive curvature in relative scale units.

**Proved default counterexample in exact arithmetic.** Take $(0)$ with held mean $-\delta=-2^{-100}$. The exact version of the current Newton loop, including its last step, returns $\widehat B\le10.2$, whereas $B_*>11$. To bound the trajectory, put $U=(1+\delta)\phi((1+\delta)B)$ and $V=\delta\phi(\delta B)$. For $1.5\le B\le10.2$, $p>1/6$, $U<1/4$, $V/U<1/100$ and $(1+\delta)^2<1.01$. The first bound follows by integrating on $[\delta B,1]$ with $\phi(1)>1/\sqrt{24}$. The second uses $B\ge1.5$. For the third, $U>e^{-53}/3>3^{-54}$ and $V<2^{-101}$, using $e<3$ and $3^{54}<2^{87}$. For $f=-\log p$,
\[
 g=-(U-V)/p,\qquad H=(U-V)^2/p^2+B((1+\delta)^2U-\delta^2V)/p.
\]
Here $H$ decreases. In its first term $U-V$ decreases and $p$ increases. In its second term $BU$ decreases, $BV$ increases and the numerator stays positive. The lower bound $H\ge B(U-V)/p$ and the upper bound obtained by dropping $-B\delta^2V/p$, with $U/p<B$, yield $0.49/B<-g/H\le1/B$.

The start $1/\sqrt{0.1+(1/2+\delta)^2}$ is between 1.5 and 2. Full steps before the first crossing of 10 and from the crossing stay inside $[1.5,10.2]$ by the upper step bound. Decreasing $H$ gives $f(B+h)\le f(B)+gh/2$ for the full Newton step $h=-g/H$, so it passes the line search. Each step raises $B^2$ by at least $0.98$, so unless the decrement fires earlier, the crossing occurs within 101 steps and has $B\le10.1$. There the decrement is at most $U/(pB)<e^{-50}<10^{-20}$ using $e^5>100$. After the last step $B\le10.2$. The closed form gives $B_*>11$ using $\log 2>2/3$. Thus $\widehat\sigma/\sigma_*-1>11/10.2-1>0.078$. The earlier refinement study verified the extra Newton step on ordinary archive cases. [Prior refinement study](2026-11-15-refine-causes-report.md#dnorm-12-entries-class-a)

**Numerical.** The following default-control replays take the decrement exit. The binary64 replay uses libm normal tails. For the last two holds, binary64 $m-c$ rounds to $-1/2$, losing the near-edge distance in the working score.

| Held mean on $(0)$ | True scale | 100-digit replay relative error | Binary64 replay relative error |
|---|---:|---:|---:|
| $-2^{-52}$ | 0.117779742319532 | $1.20\times10^{-7}$ | $1.20\times10^{-7}$ |
| $-2^{-60}$ | 0.109646972936213 | $3.19\times10^{-4}$ | $4.04\times10^{-2}$ |
| $-2^{-100}$ | 0.084932180028802 | $2.39\times10^{-1}$ | $2.39\times10^{-1}$ |

For the last row the binary64 replay returns $0.105219080751344$, with total likelihood loss $2.02\times10^{-21}$. At iterlim 20 it returns $0.150819736993821$ through the raw score gate, with relative error $0.776$. All three cases call for an accurate finite singleton and admit the closed-form scale.

### Subnormal held scale

**Proved from the source and binary64 arithmetic.** On $(0)$ with held $\sigma=2^{-1074}$, the exact optimizer is $\mu=1/2$. The fitter starts at $c=\mu=1/2$. The reciprocal overflows, so $A=0\cdot\infty$ is NaN. The first cell evaluation initializes the score and information to zero and returns positive infinity before accumulating either because the endpoints fail the interval ordering test. The Newton step is NaN and breaks the loop. The zero-score gate then sets Ok, and public reconstruction produces NaN location. The correct outcome is the midpoint with the scale held exactly. [Fitter](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/mle.hpp) · [Interval evaluation](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/dpqr.hpp)

**Proved representation limit.** Hold the same scale on $(-1,-1,0)$, and set $q=\mu/\sigma$, $W=1/\sigma$. At $q=-3/4,-1/4$, both outer endpoints have absolute value greater than 10. With $p=\Phi(-q)$ the two cell masses differ from $p,1-p$ by at most $e^{-50}$, and their derivatives in $q$ differ from $-\phi(q),\phi(q)$ by the same bound. Multiplying the score by the product of the two masses gives $\phi(q)(3p-2)$ up to error $6e^{-50}$. At $q=-1/4$ one has $p<5/8$ since $\phi(0)<1/2$. At $q=-3/4$ one has $p>11/16$ since $\phi(3/4)>1/4$. The signs survive the error, so the unique root lies between $-3/4$ and $-1/4$. Every binary64 location is an integer multiple of $\sigma$ and misses by more than $\sigma/4$. The location floor in the contract is $\sigma$, so default tolerance calls for a diagnosed representation failure despite finite real attainment.

### Ordinary fits and location rounding

**Numerical.** Independent 100-digit scores were solved in public location and log scale after exact translation by the sample minimum. The 500-observation golden and archived cases 1098, 1099, 1100, 1116–1119 and 1124 were checked against stored coefficients, with both native and double-only archived outputs. The largest normalized free-coefficient error was $2.14\times10^{-16}$. The largest normalized log likelihood discrepancy at an archived public pair was $1.68\times10^{-16}$. Case 1099, $(-2,0)$, also has an exact reference by reflection at $-1/2$ followed by the single-cell scale calculation with $\delta=1/2$, giving $\sigma_*=1/\sqrt{\log 3}$. Source replays on $(0,0,2)$ passed at default tolerance for the joint fit, held mean 1 and held scale 1. [Stored snapshots](2026-11-15-fit-snapshot-data/)

**Numerical translated witness.** Let $K=2^{51}$ and fit $(K,K,K+2)$ with both coefficients free. The target is $(K+1.16812475140128998,\,0.894548858795114875)$ to the digits shown. The binary64 replay publishes $(K+1,\,0.8945488587951148)$ within the coefficient tolerance. The rounded pair costs $0.0480070798296$ in total log likelihood relative to the exact optimizer. Holding the rounded location and remaximizing gives scale $0.911233235722914835$, which misses the joint scale by $1.865\%$. The existing reconstruction keeps the joint scale and evaluates the log likelihood at the public pair, in agreement with the contract's treatment of a steep maximum after rounding.

## Reproduction

Both final scripts exited zero on the workstation through the one-slot lock. New stress cases use the Python source replay with libm tails or 100-digit normal interval probabilities. The archive comparisons use existing compiled outputs. The score-box replay uses 100-digit scalar probabilities and densities padded by $10^{-80}$ relative, combined with 110-digit interval arithmetic. Its grid uses gaps $2,3,8$, lower-cell counts $1,2,9$ and upper-cell count one, offsets $0,2^{51}$, and the joint and two one-held masks. All 54 boxes and three additional near-seam boxes passed inclusion and coefficient checks. Probes at scale $0.103$ on the three near-seam cases failed inclusion. The face replay uses 280 digits at scales $2^{-3},2^{-4},2^{-5}$ for the adjacent $7:3$ path, with positive losses down to $9.37\times10^{-217}$. It also checks the point-mass and held-seam paths at scales $2^{-2},2^{-3},2^{-4}$.

~~~sh
d=dev/research/evaluations/2026-11-25-dnorm-math-data
export PYTHONDONTWRITEBYTECODE=1
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/check.py"
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/cover.py"
~~~

[Coefficient and stopping-rule replay](2026-11-25-dnorm-math-data/check.py) · [Score-box and face replay](2026-11-25-dnorm-math-data/cover.py)

## What this changes for the fitter

- Keep the exact dispatch in the table and the structured PointMass, TwoPointLimit and ScaleLimit payloads. For held mean, make the closure test on the original values and preserve the half probability at an integer seam.
- For constant data with scale held, use the cell midpoint directly. For constant data with mean held off the closure, use the displayed scale formula with the original edge distance. Evaluate the logarithmic ratio without overflowing $1/\delta$ at small $\delta$ or cancelling at large $\delta$. For general held-mean fitting, form endpoint differences against the original mean before scaling.
- Require finite evaluations, steps and public coefficients. Replace decrement-only and raw-score-only success with a signed bracket, score-box inclusion or another justified public-unit accuracy check. An enclosed root allows Ok with certified and unique flags both true. If a certificate allowance expires and a separate accuracy check supports the successful candidate, retain it with the standard uncertified fallback. Use the appropriate failure for an actual search or evaluation failure, a refuted candidate without an accurate replacement, or a proved representation limit. Charge refinements to the existing work units. [Wrapper failure rule](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6) · [Work budgets](../../design/work-budgets.md)
- Compare the published coefficients with the original joint root. Evaluate the log likelihood at the rounded pair in the steep-maximum case and keep the joint scale. The held seam, subnormal cases and large translation exercise distinct failure and rounding paths.

## Open

- Existence, finite uniqueness and escaping-law comparisons are settled for the dossier's masks. Production coefficient certification still needs practical error bounds on score and information calculations in both backend modes, particularly for narrow intervals and nearly cancelling tail scores. The high-precision padding in the replay supplies no production bound.
- General held-scale subnormal and extreme-range fitting needs a chart that avoids an overflowing reciprocal and a public resolution check when a location enclosure falls between representable values. The exact constant shortcut and the adjacent-cell obstruction cover the two samples above. The implementation job still needs end-to-end C++ and binding checks on the new cases.
