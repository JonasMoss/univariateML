# Pareto III and IV likelihood at held and free thresholds

TASK-361 · 2026-11-25 · Base `338f045bfe4d9631410ccfcce8f16e35e8e68b62`

**Proved.** With the threshold held, every Pareto III fit that exists on positive excesses is unique. Pareto IV has unique finite fits with its first shape held and on the finite part of its held-scale slice. With its power held or with all three regular coefficients free, exact real samples have distinct finite global maximizers. The all-regular-free slice also has exact finite/Pareto and finite/Weibull ties. With a free threshold, the multiplicity of the sample minimum governs scale collapse when both shapes are fixed. With scale and power fixed and the first shape free, two local maxima are possible. On algebraic observations with algebraic scale and rational power, every stationary point of that free-threshold profile is simple.

**Original-input finding.** The Pareto IV excess-subtraction guard can pass even when rounding moves the unique held-scale power root by about 25%. In a second exact binary64 sample, the same subtraction changes the Pareto face's exponent by about 16.7%. The model transfer is exact on real differences. The implementation's guard against overflow, new ties and new endpoint equalities does not bound the effect of nonzero subtraction residuals elsewhere.

The earlier work supplies the conditional and boundary theorems used below. For current dispatch and the historical switch, see the [Pareto III dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto3/README.md), [Pareto IV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md), and [Pareto IV switch report](2026-11-04-pareto4-switch-report.md). The threshold and equality results build on the [P0 family analysis](2026-10-29-p0-genbeta.md), [free-threshold report](2026-11-25-thresh-math-report.md), and [binary64 ties report](2026-11-25-ties-math-report.md).

## Threshold held and excesses strictly positive

Write $a=\texttt{shape1}$, $c=\texttt{shape2}$, $s=\texttt{scale}$ and $\mu=\texttt{min}$. Pareto III fixes $a=1$ and calls $c$ `shape`. Take a finite, nonempty sample and valid holds. Put $y_i=x_i-\mu$, $z_i=\log y_i$, $b=-c\log s$ and $h(v)=\log(1+e^v)$. Bars are arithmetic means, including multiplicities. The mean log likelihood plus the data-only term $\bar z$ is
$$
 L(a,c,b)=\log(ac)+\overline{cz+b}-(a+1)\overline{h(cz+b)}.
$$
Subtracting the threshold has unit Jacobian. Passing from positive excesses to their logs gives the displayed data-only shift. Thus all strict comparisons and equalities carry through on the exact inputs. Nonfinite data, invalid holds, empty data and excesses below the threshold are invalid. The current fitters also require strict excess positivity. Endpoint observations under the closed density convention are addressed below.

### Pareto III

**Proved.** The log-logistic transfer in the dossier gives the following complete table. The sample conditions use exact excesses.

| Held in addition to $\mu$ | Condition and mathematical fitter outcome |
| --- | --- |
| Nothing | Nonconstant data give a unique finite estimate. Constant data are `Unbounded`. |
| $c$ | Unique scale estimate on every sample, including constants. On a constant sample $s=y_1$. |
| $s$ | Unique power estimate unless every $y_i=s$, when the likelihood is `Unbounded`. |
| $c,s$ | Evaluate the held coefficients. |

For fixed first shape (including $a=1$), the Hessian in $(c,b)$ is the sum of the negative curvature of $\log c$ and the negative logistic curvature. In direction $(u,v)$ the quadratic form is $-u^2/c^2-(a+1)\overline{h''(cz+b)(uz+v)^2}<0$ for every nonzero direction. Nonconstant data give coercivity in these coordinates. The term $v-(a+1)h(v)$ is at most $-\min(1,a)|v|$, so increasing $c$ forces a loss linear in $c$ by the positive log range. At $c\downarrow0$ the term $\log c$ forces decay uniformly in $b$, and at bounded positive $c$ the same tail bound controls $|b|$. The one-coordinate restrictions preserve strict concavity in $b$ at fixed $c$, or in $c$ along $b=-c\log s$ at fixed $s$. The latter loses coercivity at infinity exactly when all relative logs vanish. On the constant concentration path $s=y_1$, the density is $ac/[y_1 2^{a+1}]$ at each observation. There is no competing positive finite escape on the finite-fit cases. [Log-logistic transfer](2026-11-25-transfer-math-report.md#1-transfers)

### Pareto IV

**Proved.** The single-coordinate results follow from the score equations in $L$. The remaining rows use the Burr and Lomax profiles described below.

| Held in addition to $\mu$ | Condition and mathematical fitter outcome |
| --- | --- |
| Nothing | Constant data are `Unbounded`. On nonconstant data, compare finite stationary heights with the two optimized faces. Attainment includes equality with the higher face. Uniqueness can fail. |
| $a$ | Unique finite estimate on nonconstant data by the fixed-first-shape argument above. Constant data are `Unbounded`. |
| $c$ | Compare the powered-data Lomax profile with its exponential face. Finite attainment includes equality on real inputs. The named limit is shifted Weibull with $c$ held. Finite uniqueness can fail. |
| $s$ | Nonconstant data with some $y_i<s$ give a unique finite estimate. Constant data at or below $s$ are `Unbounded`. All $y_i\ge s$ with some $y_i>s$ give the held-endpoint Pareto limit. |
| $a,c$ | Unique scale estimate. In log scale the score is $c[(a+1)\overline{h'(cz+b)}-1]$, strictly decreasing with opposite limiting signs. On a constant sample $s=y_1a^{1/c}$. |
| $a,s$ | Unique power estimate unless all $y_i=s$, when the density grows linearly with $c$. The scalar score is $1/c+\overline{w[1-(a+1)h'(cw)]}$, where $w_i=\log(y_i/s)$. Its derivative is strictly negative. |
| $c,s$ | Unique first shape $a=1/\overline{h(cw)}$. |
| $a,c,s$ | Evaluate the held coefficients. |

For the held-scale two-shape row, profiling $a$ gives $G(c)=\sum_i h(cw_i)$ and $R(c)=\sum_i h(-cw_i)$. The total physical log likelihood is $n\log n-n-\sum_i\log y_i-n\log(G(c)/c)-R(c)$. It is strictly concave in $c$. For nonconstant data with a negative relative log, it decays at both ends. For a constant $y<s$, take $a=(s/y)^c$ and $c\to\infty$ to give $f(y)\sim c/(ey)$. At $y=s$, fix $a$ and increase $c$. These are different from the finite face for a constant $y>s$. For all $y_i\ge s$ with some strict inequality, set $S=\sum_i\log(y_i/s)$ and $Z=\#\{i:y_i=s\}$. Every finite point is strictly below the supremum
$$
 n\log(n/S)-n-\sum_i\log y_i-Z\log 2.
$$
The limiting Pareto exponent is $n/S$, reached along $c\to\infty$, $ac\to n/S$. Its endpoint is $\mu+s$. The path halves density at the tied endpoint; a fresh estimate of the Pareto endpoint would change the held-scale problem. For example with $(x,\mu,s)=((3,4),2,1)$ the endpoint is 3, the exponent is $2/\log 2$, and the penalty is $\log 2$. [Burr held-scale proof](2026-11-06-burr-scale-report.md) · [Constant paths](2026-11-02-burr-collapse-report.md)

### All regular coefficients free

**Proved by exact transfer.** On nonconstant data let $F(a)=\max_{c,b}L(a,c,b)$, using the unique conditional root. The two optimal faces, in the units of $L$, are
$$
 P=-1-\log\overline{(z-z_{\min})},\qquad
 W=\max_{c>0}\{\log c+c\bar z-\log\overline{e^{cz}}-1\}.
$$
The Weibull power solves $1/c+\bar z-\overline{ze^{cz}}/\overline{e^{cz}}=0$, a strictly decreasing score. The Weibull scale is $(\overline{y_i^c})^{1/c}$. Its path has $a\to\infty$ with $s a^{-1/c}$ tending to that scale. The Pareto endpoint is $\min x$ and the exponent is $1/\overline{(z-z_{\min})}$. Its best path sends $c\to\infty$ with $ac$ tending to the exponent and $s$ approaching $y_{\min}$ from below with $c\log(y_{\min}/s)\to\infty$. This gives the full endpoint density. A path that instead fixes $s=y_{\min}$ loses $\log 2$ per minimum even though the weak law is the same. The closure proof exhausts likelihood-retaining escapes for nonconstant data. [Closure and trace](2026-11-03-burr-closure-report.md#2-an-envelope-and-the-complete-weak-closure)

A finite MLE exists exactly when a finite point reaches or exceeds $B=\max(P,W)$. The equivalent per-input rule uses the largest stationary height of $F$, with an empty list treated as $-\infty$. Its stationary set is finite by the prior Pfaffian bound for the number of distinct log levels. If the largest stationary height is at least $B$, retain all finite roots at the largest height. If it is below $B$, return the winning face, keeping both face witnesses if their equality is proved. A practical certificate uses the small-$a$ strict Pareto bound, the unsigned or signed large-$a$ Weibull bound, and a compact root/value cover in between. A finite point strictly above both face uppers suffices for existence before the root count has closed. At least half of the observations tied at the minimum suffice for strict Pareto dominance, after the constant case is removed. For example $x=(3,4)$ and $\mu=2$ on this mask give `ParetoBoundary` with the exponent above and the full trace. [Attainment and minimum-tie theorems](2026-11-03-burr-closure-report.md) · [Stationary-point bound](2026-11-25-burr2-math-report.md#bound-on-the-number-of-stationary-points)

The following are exact real samples. In each row take $\mu=2$ and $x_i=2+e^{v_i}$. A parameter specified as a zero of a regional height difference uses the indicated global cover as part of its exact definition.

| Log excesses $v$ | Proved result |
| --- | --- |
| $(0,2,2,5,6,8)/8$ | Two separated strict local maxima. Both lose to Pareto. Return the Pareto limit. |
| $Y(q)=(0,0.23826q,7q/15,8q/15,8q/15,1,1)$ at $q=1$ | A finite witness strictly beats both faces. |
| $Y(q_*)$, where $q_*$ is the infimum of $q\in[0,1]$ with a finite fit strictly above $P(q)$ | A finite global maximum equals $P$ and exceeds $W$. Retain finite attainment. |
| $N=2^{90}$ copies of each entry of $Y(q_*)$ and one entry $-1$, now with $q_*$ the least equality of the regional profile maxima on $[1/(7N),1/8]$ and $[1/8,2^{24}]$ | Two separated finite global maximizers above both faces have been proved. Completeness within the two regions is a separate question. This histogram specifies a mathematical sample. |
| $V(t)=(0,1-10t/15,1-8t/15,1-7t/15,1-7t/15,1,1)$ at $t=9/10$ | Weibull strictly wins. |
| $V(t_*)$, with $t_*$ the least zero on $[9/10,1]$ of $\max_{1/6\le a\le 2^{16}}F(a)-W$ | A finite global maximum equals $W$ and exceeds $P$. Retain finite attainment. |

The signed global covers, tail exclusions and constructions are in the [local-maxima proof](2026-11-25-burr-math-report.md#all-free-burr-xii), [finite/Pareto construction](2026-11-03-burr-closure-report.md#a-finite-mle-can-equal-the-best-boundary-likelihood) and [finite pair and finite/Weibull constructions](2026-11-25-burr2-math-report.md). These real crossings do not establish exact ties on binary64 data. In particular with $\mu=2$, the samples as written have transcendental levels.

### Power held

**Proved.** Put $u_i=y_i^c$, $m_u=\bar u$, $r=s^{-c}$, $A=\overline{\log(1+ru)}$ and $Q=\overline{1/(1+ru)}$. The powered-data law is Lomax. The change of variable adds $n\log c+(c-1)\sum_i\log y_i$ to its total log likelihood. The shape at each rate is $a=1/A$ and the mean gap over the optimized exponential is
$$
 H_0(r)=\log(rm_u/A)-A.
$$
The exponential becomes shifted Weibull of power $c$ and scale $m_u^{1/c}$. A finite estimate exists iff there is a stationary positive $r$ with $H_0(r)\ge0$. This includes real input equality. The right limit of $H_0$ is $-\infty$, the left limit is zero, and the finite likelihood condition follows by the same compactness argument as the one-face Lomax profile. The exact range test $u_{\max}\le 2u_{\min}$ gives strict face dominance, including constants and equality in the range test. At most two distinct powered values give uniqueness whenever a finite fit exists. [Held-power boundary and equality](2026-11-25-burr-math-report.md#face-comparisons-and-equality) · [Two-support uniqueness](2026-11-25-lomax-uniq-math-report.md#score-and-a-cheap-uniqueness-condition)

For arbitrary fixed $c>0$, the exact shifted data $x_i=2+u_i^{1/c}$ at $\mu=2$ transfer the following examples.

- $u=(1,2)$ gives a strict Weibull limit at the range equality. $u=(1,100)$ gives a unique finite estimate above Weibull. The fixture $(1,3000,3000,22000)$ has two strict local maxima, both above Weibull.
- For $u=(1,e^{5.1},e^{v_*},e^8)$, $v_*$ is the unique zero on $(5.99,6.02)$ of the higher minus lower regional peak gap, in log-rate bands $[-1.39,-1.32]$ and $[-4.56,-4.50]$. The prior cover proves exactly two finite global maximizers, both above Weibull. Each rate yields $(a,s)=(1/A,r^{-1/c})$. This is a real crossing. [Held-power exact tie](2026-11-25-burr-math-report.md#held-power-has-an-exact-global-tie)
- With $u=(1,v_*)$, take $v_*$ as the unique transition in $(2,100)$ where a positive rate first reaches the exponential value. The prior compactness and strict monotonicity argument gives a finite/Weibull tie there, with a unique finite fit by the two-support theorem. The reported numerical transition is near $28.80920557873453$. [Exact two-point transition](2026-11-25-burr-math-report.md#face-comparisons-and-equality)

**Proved per-input decision on algebraic powered data.** The ties report proves that every positive stationary rate is transcendental on positive algebraic powered data, and that no such rate can have $H_0=0$. The argument sets $A=(1-Q)/Q$ at stationarity. Equality would make $\prod_i(1+ru_i)=(rm_u/A)^n$ a nonidentical rational equation for $r$, forcing it algebraic, whereas $e^{nA}=\prod_i(1+ru_i)$ contradicts Hermite–Lindemann at an algebraic stationary rate. For original finite binary64 inputs and holds, $u_i$ is algebraic since the exact positive difference is dyadic and $c$ is rational. [Prior exclusion proof](2026-11-25-ties-math-report.md#unconditional-exclusion-of-a-finite-lomaxexponential-tie) · [Lindemann's theorem](https://doi.org/10.1007/BF01446522)

The same arithmetic makes root enumeration terminate. Set $J=A-(1-Q)/Q$, so the log-rate derivative of $H_0$ is $QJ/A$. For the distinct powered values $u_j$ and their proportions $p_j$, let
$$
 D=\prod_j(1+ru_j),\quad N=\sum_j p_jD/(1+ru_j),\quad
 K=(D-N)N^2-rD(ND'-N'D).
$$
The Lomax score identity gives $rJ'=K/(DN^2)$. The polynomial is nonzero because $J\to-\infty$ as $r\to\infty$. Its positive roots are algebraic, and none is a zero of $J$ by the stationary-rate exclusion. In particular all positive stationary rates are simple. Isolate the positive roots of $K$ over the real algebraic data field. Enclose the sign of $J$ at each cut to sufficient precision. The first monotone interval starts at $J(0)=0$, and the last ends at $J\to-\infty$. Each intervening sign change identifies exactly one stationary rate. Enclose its $H_0$ until the sign is strict. If any is positive, a finite estimate exists. If none is positive, the Weibull face strictly wins. When several rates give positive gaps, their ordering still needs height comparison. This algorithm terminates for existence with unbounded exact-arithmetic resources. Its polynomial construction can be expensive at high algebraic degree or many distinct powers. It supplies neither a uniform gap nor a default-budget guarantee.

## Threshold free

**Proved.** Let $m=\min x$, $t=m-\mu\ge0$, $d_i=x_i-m$, and $k=\#\{d_i=0\}$. Finite likelihood fits never require $\mu>m$. The table summarizes the [free-threshold proof](2026-11-25-thresh-math-report.md). Its mathematical dispatch includes the endpoint $t=0$ by R7a. Pareto III currently requires a held threshold at the public R entry point and reports unsupported at its free-threshold core entry. Pareto IV already routes the structural spikes to `Unbounded`. Its core reports unsupported on the remaining bounded-slice masks at $c\ge1$.

| Family and held regular coefficients | Exact conditions and mathematical fitter outcome |
| --- | --- |
| III, $\varnothing$ or $s$ | Free $c$ permits $c<1$. `Unbounded` for every sample. |
| III, $c$ | $c<1$ gives `Unbounded`. For $c\ge1$ apply the fixed-shapes collapse test below with $a=1$. |
| III, $c,s$ | $c<1$ gives `Unbounded`. At $c=1$ the unique estimate is $\mu=m$. At $c>1$ there is a unique interior gap root. |
| IV, $\varnothing$, $a$, $s$ or $a,s$ | Free $c$ permits $c<1$. `Unbounded` for every sample. |
| IV, $c$ | `Unbounded` for all $c>0$. In addition to the subunit-power spike, the free $a,s$ permit joint collapse. |
| IV, $a,c$ | $c<1$ gives `Unbounded`. For $c\ge1$ apply the fixed-shapes collapse test. |
| IV, $c,s$ | $c<1$ or constant data give `Unbounded`. Otherwise $c=1$ gives the unique estimate at $\mu=m$ and $a=1/A(0)$ as defined below. For $c>1$ a finite interior fit exists. Compare its stationary heights. |
| IV, $a,c,s$ | $c<1$ gives `Unbounded`. At $c=1$ the unique estimate is $\mu=m$. At $c>1$ there is a unique interior gap root. |

For $c<1$ the minimum factors diverge as $t^{k(c-1)}$ with positive finite limits for the other factors. With $a,s$ free, the prior joint ray $a=1/T$, $t=s=e^{-T}$ gives likelihood proportional to $e^{kT}/T^n$. These routes give stops on every finite nonempty sample. Both are compatible with weaker proper limit laws that do not maximize likelihood. For instance when $a$ is fixed and $c,s$ can vary, $\mu=-T$, $s=T+h$, $c=T/d$, $d>0$, gives the generalized logistic on the line, logistic for Pareto III. The logit tends to $(x-h)/d$ and the prefactor to $a/d$. [Structural paths](2026-11-25-thresh-math-report.md#structural-spikes) · [Receding threshold](2026-11-25-burr-math-report.md#inverse-and-shifted-burr)

**Fixed-shapes collapse test.** For $c\ge1$ put $E=ac(n-k)-k$. If $E<0$, return `Unbounded`. If $E=0$, return `Unattained` with the finite likelihood supremum
$$
 (ac)^n, g(r_*)^k \prod_{d_i>0}d_i^{-1-ac},\qquad
 g(r)=\frac{r^{c-1}}{(1+r^c)^{a+1}},\qquad
 r_*^c=\frac{c-1}{ac+1}.
$$
The path has $s\downarrow0$, $t=r_* s$. For $c=1$ use $r_*=0$ and $g(0)=1$. Every finite point loses strictly because each positive-gap factor is strictly below its power-tail upper. The weak law collapses to a point mass at $m$ and no proper continuous limit fit has this sample likelihood. If $E>0$, the finite MLE is unique, with $\mu=m$ at $c=1$ and with $t>0$ otherwise. Use the prior monotone profiled gap score with the unique conditional scale score root. With both shapes and scale held, the prior gap bracket is $[\max(0,y_* -\max d_i),y_*]$ for $y_*=s r_*>0$. The example $(0,1)$ with $a=c=1$ and scale free has $E=0$ and supremum 1. At $a=1,c=2$ the sample $(0,0,1)$ has $E=0$. The samples $(0,0,1)$ and $(0,1,2)$ with $a=c=1$ give $E<0$ and $E>0$ respectively. These examples apply to both families. [Scale-collapse and profile proof](2026-11-25-thresh-math-report.md#fixed-shapes-and-free-scale)

### Threshold free, power and scale fixed, first shape free

**Proved.** For nonconstant data and $c>1$ use
$$
 A(t)=\overline{\log(1+((t+d_i)/s)^c)},\quad
 J(t)=A'(t),\quad C(t)=(c-1)\overline{1/(t+d_i)}-J(t).
$$
The optimal first shape is $1/A(t)$ and the mean gap-coordinate score is $C-J/A$. The prior signed tail bounds isolate the relevant search to a compact interval inside $t>0$. The right exterior starts at $s(c-1)^{1/c}$ and the explicit left bound uses $1/A(0)$ and the positive gaps. The exact sample with two observations at zero and twenty at $41/100$, $s=1$, $c=48$, has two separate finite local maxima: the score has signs $+,-,+,-$ at $1/2,14/25,9/10,99/100$. This defeats a one-root search. It does not establish equal global heights. [Threshold profile, signs and bounds](2026-11-25-thresh-math-report.md#fixed-scale-and-free-upper-tail-shape)

**Proved on algebraic data with algebraic scale and positive rational power.** On $t>0$, $J,C$ are algebraic functions over the real algebraic numbers and $J>0$. Stationarity needs $C>0$ and is equivalent there to $H=A-J/C=0$. The numerator of $H'$ is
$$
 K_t=C^2J-J'C+JC'.
$$
This is algebraic and nonzero as a function. Otherwise $A-J/C$ would be constant on the large-$t$ interval where $C\sim-1/t$. There $A\to\infty$ and $J/C\to -c$, a contradiction. A zero of a nonzero algebraic function is algebraic: its minimal polynomial after clearing denominators has a nonzero constant coefficient as a polynomial in $t$, which must vanish at the zero. If $H=H'=0$, $t$ is therefore algebraic. At that $t$, $J/C$ is positive algebraic, while $nA$ is the real logarithm of the algebraic product $\prod_i[1+((t+d_i)/s)^c]>1$. Hermite–Lindemann rules out $A=J/C$. The original score equals $(C/A) H$ near its roots, so they too are simple.

On the compact interval from the earlier proof, isolate the zeros of $C$ and $K_t$ by algebraic root isolation with branch checks. For example, for $c=p/q$ introduce positive auxiliaries $z_i^q=t+d_i$ and one positive $q$th root of $s$. This makes the expressions rational in algebraic quantities with branch conditions. On $C\le0$ the score is strictly negative. At each zero of $C$ use $AC-J=-J<0$. At a zero of $K_t$ with $C>0$, the preceding argument guarantees a strict score sign under refinement. On each remaining open cell with $C>0$, $H$ is monotone and opposite boundary signs identify exactly one root. Thus all roots can be counted even when their heights cannot be ordered. This resolves the degenerate-root question on binary64 inputs for this mask in exact arithmetic.

### Equality with a threshold held at a data point

**Proved for the R7a density closure.** These are outside the present strictly positive-excess fitter domain. They matter if endpoint fitting is enabled.

- If some $y_i=0$ and $c$ is free or held below 1, the likelihood is unbounded in the extended density convention. A fixed $c>1$ makes all feasible likelihoods zero and provides no positive fit. At $c=1$ the model is the zero-aware Lomax on $y$, with density $a/s$ at zero.
- At $c=1$ with $a$ held and $s$ free, use $E=a(n-k)-k$, where $k$ now counts the zero excesses. The fixed-shapes collapse result with $t=0$ gives unboundedness, the strict finite collapse supremum, or a unique scale root for $E<0$, $E=0$, or $E>0$ respectively. If $a,s$ are both free in Pareto IV, fix a sufficiently small positive $a$ to make $E<0$.
- At $c=1$ with $s$ held and $a$ free, $a=1/\overline{\log(1+y/s)}$ is the unique estimate if some $y_i>0$. All zero gives unboundedness as $a\to\infty$. Fully held coefficients give density evaluation. Example at $\mu=0$, $x=(0,1)$ and $c=1$ is attainable with $s=1$ held and $a$ free, but with $a=1$ held and $s$ free it gives the unattained scale-collapse value 1.

## Returned coefficients and transfer arithmetic

**Numerical.** The workstation replay solved the analytical scores at 100 decimal digits for 18 stored public `Ok` records (nine each from native and double-only snapshots). It decoded the returned hexadecimal coefficients and formed differences from the exact input doubles in high precision. With free scale, it solved for power and intercept jointly and mapped back to the continuous optimum before comparing to the returned scale. It checked negative definite local Hessians and score residuals below $10^{-75}$. The rows whose attainment and uniqueness follow from the conditional theorems identify their actual maximizer. Archive 005648 has an uncertified joint search and the comparison is to its strict local maximum. Archive 005644 was a limit in both snapshots and has no finite coefficient comparison.

| Family and archive cases | Largest relative free-coefficient error across both modes |
| --- | --- |
| Pareto III, 005584, 005585, 005590, 005591, 005593 | $1.34\times10^{-15}$ |
| Pareto IV, 005648 joint | $1.96\times10^{-17}$ |
| Pareto IV, 005665 and 005666 with first shape held | $3.10\times10^{-16}$ |
| Pareto IV, 005674 with power held | $4.58\times10^{-15}$ |

All met the default $10^{-10}$ coefficient tolerance. These are archived public results. The new calculations below check mathematical roots and the rounding in the delegation path. [Archived inputs](2026-11-07-fit-audit-data/cases.json.gz) · [Native snapshot](2026-11-15-fit-snapshot-data/native.jsonl.gz) · [Double-only snapshot](2026-11-15-fit-snapshot-data/double.jsonl.gz) · [Reproducer](2026-11-25-pareto34-math-data/check.py)

**Proved subtraction counterexample.** Hold $\mu=-U$ and $s=U$ for $U=2^{54}$, and take $x=(-5,10)$ with both shapes free in Pareto IV. Under round-to-nearest ties-to-even the two supplied excesses become $U-4,U+8$. Neither point collides with another point or with the held scale. Both the true and rounded problem have unique finite maxima. With $q=c/U$, use the held-scale profile on $w_i=U\log(1+x_i/U)$. Its derivative in $q$ is $1/q+\bar w-(1+1/A_q)\overline{w h'(qw)}$, where $A_q=\overline{h(qw)}$. The 85-decimal outward score bounds bracket the true root in $(0.22777021007973985,0.22777021031254208)$ and the rounded-input root in $(0.2847127626542374,0.2847127628870396)$, with endpoints printed as their round-trip dyadics. The disjoint bounds imply a relative discrepancy above 22%. Numerically it is approximately 25%. The error precedes optimization. A certificate for the rounded data cannot establish accuracy on the supplied input. In the source the certified successful path delegates after the guard and refines on the original inputs only on an uncertified success. [Pareto IV delegation](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/mle.hpp) · [Held-scale certificate](../../../univariateML-cpp/include/cppunivariate/distributions/burr/scale_certificate.hpp)

**Proved face coefficient discrepancy.** Keep the same holds and use $x=(5,9)$. The rounded offsets are $(4,8)$ and the exact offsets are $(5,9)$. Both datasets satisfy the same strict held-scale Pareto predicate. The exponent is $2/\sum_i\log(1+x_i/U)$ with the appropriate pair of offsets. The ratio of rounded-data to original-data exponents is greater than $1.16$, for example by applying $v-v^2/2\le\log(1+v)\le v$ to all four positive ratios. The 100-digit values are $2.57348550135457\times10^{15}$ and $3.00239975158033\times10^{15}$ respectively for the true and rounded data. Also, $(x,\mu,s)=((1),-U,U)$ is strictly above the held scale in exact arithmetic even though the rounded excess equals it. Its correct Pareto IV outcome on the two-shape mask is a limit, while for Pareto III with power free it is a unique finite root. The equality control $x=(0)$ gives unboundedness in each case.

**Numerical restoration check.** For Pareto III use $x=(1,5)$ and $\mu=-U$ with shape and scale free. For a two-point log-logistic sample the exact scale is the geometric mean of $U+1,U+5$ by symmetry and strict intercept concavity. The nearest binary64 scale is $U+4$. The joint power satisfies $v\tanh(v/2)=1$ with $v=c(\log y_2-\log y_1)/2$. The continuous joint value of $c/U$ is $0.771702319209104$. Reoptimizing power at the rounded scale instead gives $0.728975417467086$, a relative change of 5.54%. Evaluating at the rounded scale with the joint power loses about $0.08687$ mean log likelihood. The restored scale itself is within the coefficient tolerance. This illustrates the need to check joint coefficients and then evaluate the likelihood at the represented tuple, as specified for steep smooth fits. [Accuracy and likelihood contract](../../design/accuracy-contract.md) · [Rounding and coefficient script](2026-11-25-pareto34-math-data/check.py)

Both final scripts exited zero on the workstation through the single-slot lock. The polynomial script used exact rational real-root isolation via SymPy followed by 65-decimal interval signs and height bounds on the three integer samples in the held-power section. Their positive stationary root counts were 0, 2 and 3 respectively. The two-root sample has a minimum below Weibull and a maximum above it. The three-root sample has both maxima and its intervening minimum above Weibull. The archive script also checked the outward held-scale score signs and the rounded-scale refit example. The job used Python only for computation and made no fitter, dossier, test or golden changes.

```sh
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
mkdir -p "$TMPDIR"
d=dev/research/evaluations/2026-11-25-pareto34-math-data
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/held_power.py"
/tmp/umvl-cpu/run nice -n 10 python3 -u "$d/check.py"
```

[Polynomial and face sign checker](2026-11-25-pareto34-math-data/held_power.py) · [Accuracy and translation checker](2026-11-25-pareto34-math-data/check.py)

## What this changes for the fitter

1. **Use exact sample predicates.** Compare original observations for constancy and minimum multiplicity. Compare $x_i-\mu$ with $s$ using an exact difference or a compensated comparison. Pareto IV's held-scale branches can then distinguish the constant spike, the unique finite root and the strict held-endpoint Pareto face, including its tie penalty. When enabling free-threshold masks, use the exact sign of $ac(n-k)-k$ after handling $c<1$. Dispatch the equality to an unattained supremum, and preserve the attained endpoint at $c=1$ when $E>0$. The table gives the other structural `Unbounded` routes.
2. **Carry the subtraction error through the certificate.** Use centered logs such as $\log1p((x_i-x_0)/(x_0-\mu))$ and compute or enclose the exact difference residual. When $s$ is held, form $\log((x_i-\mu)/s)$ with a compensated numerator relative to $s$. Where applicable use $\log1p(e_i/\operatorname{fl}(x_i-\mu))$ for residual $e_i$. Propagate the resulting input boxes through root and coefficient intervals, the Jacobian, Pareto traces and candidate/face heights. On the held-power route use centered powered logs and enclose the effect of the power transform too. The two exact held-scale samples test finite roots and face coefficients separately.
3. **Certify roots and heights separately.** Use signed brackets and public coefficient images on the concave slices. With power held, a positive original-input Lomax gap proves existence. On binary64 inputs the arithmetic exclusion lets a proved weak upper bound of zero exclude a finite stationary candidate from tying Weibull. Root counting by the derivative cuts or an independently exhaustive numerical cover can certify uniqueness after height comparisons. On the free-threshold $c,s$ mask, use the two analytic exterior signs and cover the remaining compact interval. The algebraic result removes the degenerate-root obstacle there on binary64 inputs. For the three-regular-free route keep both face bounds and isolate finite roots or exclude their regions.
4. **Return the proved kind of outcome.** A strictly winning face gives the corresponding limit with the held endpoint or power and any path penalty. At proved finite/face equality return the finite estimate and keep the face as a diagnostic. Report a certified singleton only with root-count and coefficient evidence. At proved finite multiplicity return the complete typed set once completeness is proved. If only two members have been proved and more may survive, retain that fact in diagnostics. If a budget or overlapping comparison leaves globality unresolved, retain an accuracy-checked, unrefuted search with `certified = FALSE, unique = NA`. Stop if the certificate refutes the candidate and no checked replacement survives, or if the search itself fails. Reevaluate the likelihood after restoring the public coefficient tuple. A conditional refit at the rounded scale changes the coefficient target. [Tie outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter) · [Wrapper failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- With all three regular coefficients free, general binary64 finite/finite and finite/face equality still need an exact witness or a separation theorem. The exact real examples also leave completeness within the two winning regions of the finite-pair construction open. To certify a complete set, isolate and count all roots at the common height. Equality of the two faces alone gives no attainment decision.
- With power held, the algebraic exclusion settles equality to Weibull on binary64 inputs. Equality between two distinct stationary Lomax rates on those inputs remains open. It needs an exact comparison at two transcendental stationary rates or a dyadic equality example with a global cover.
- With threshold free and scale and power held above the subunit regime ($c>1$), two local maxima are proved and degenerate roots are excluded for algebraic observations and scale with rational power. A global equality witness or separation theorem is still needed to settle universal uniqueness. A production cover also needs measured work bounds for algebraic or interval isolation on concentrated data and large held powers.
- The two subtraction fixtures give a quantified original-input defect in the mathematical transfer used by the source. A production replay should verify the emitted statuses and defend practical margins for original-input root and face enclosures, including narrow score brackets and public coefficient restoration. The archived replay covers positive excesses at threshold zero. The shifted fixtures use independent likelihood calculations and a source trace.
