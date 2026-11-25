# Translated half-t with free location

TASK-342 · 2026-11-25 · Base `f25d97e2`

**Proved.** The restricted all-free problem can have exactly two finite global maximizers, one at the attained Cauchy floor and one at an interior shape. At exactly half minimum ties, a finite global maximizer can equal the excluded scale-zero value. Both are exact real crossings. The samples below also give dyadic inputs with two distinct local maxima, an interval of inputs with a unique interior global maximizer, and an interval with either a unique floor maximizer or a strictly winning collapse face. Equal positive residuals give a separate exact half-normal escape rule. On the general restricted domain the candidate set is finite, and score and curvature bounds can isolate all its simple interior roots.

The earlier [half-threshold analysis](2026-10-29-p0-halfthreshold.md) and [held-endpoint report](2026-10-29-p4-report.md) establish the endpoint and fixed-shape results. The [existing half-t certificate](2026-11-02-ht-cert-report.md) bounds the missed likelihood value by a positive gap. The [uniqueness audit](2026-11-25-uniq-cover-math-report.md#value-gap-covers) explains why such a bound cannot decide the maximizer set. The [Student shape cover](2026-11-25-tcert-math-report.md#half-t-ht) supplies normal-side bounds for the same fixed-residual likelihood. The [tie report](2026-11-25-ties-math-report.md) supplies the equality classification and outcome rule used here.

## Profile and the two faces

Write `d_i = x_i - min(x)`, let $k$ be the number of zero residuals and $p=n-k$, and set $y_i=d_i^2$. The unique best location is $min x$ at any finite positive shape and scale. This follows from strict decrease of the density with distance from its endpoint. Use $u=
usigma^2$ and
$$
 b(
u)=lograc{2Gamma((
u+1)/2)}{sqrtpi,Gamma(
u/2)},qquad
 ell(
u,u)=nb(
u)-rac n2log u-
       rac{
u+1}{2}sum_{y_i>0}log(1+y_i/u).
$$
For $p>0$ and $
u>k/p$ the scale root is the unique solution of
$$
 S(u)=sum_{y_i>0}rac{y_i}{u+y_i}=rac{n}{
u+1}.
$$
It increases strictly in $
u$, because $S$ decreases strictly in $u$, and is real analytic by the implicit function theorem. For a held shape, the prior scale criterion gives a certified singleton when $
u p>k$, `ScaleBoundary` when $
u p=k$, and `Unbounded` when $
u p<k$. When $p=0$ and scale is free the likelihood diverges as the scale goes to zero at every fixed shape. With both shape and scale held, the endpoint alone is the unique estimate.

Let $P(
u)$ be the profile at the scale root. Differentiation gives
$$
 egin{aligned}
 P'(
u)&=nb'(
u)-	frac12sum_{y_i>0}log(1+y_i/u),\
 P''(
u)&=nb''(
u)+
 rac{n^2}{2(
u+1)^3 u,T(u)},qquad
 T(u)=sum_{y_i>0}rac{y_i}{(u+y_i)^2}.
 end{aligned}
$$
The first formula uses the envelope identity and the second uses
$u'=n/[(
u+1)^2T(u)]$. The digamma difference gives
$b'(
u)=int_0^infty e^{-
u t}/(1+e^{-t}),dt$, by the substitution of twice $t$ in the usual digamma difference integral. Thus $b'>0$, $b''<0$, $b''$ increases, and $b'(1)=log 2$ by integration. These formulas give interval score and curvature bounds by using the two endpoint scale roots, monotonicity of the logarithms, and bounds on the positive summands of $T$. The scale tube needs just the two scalar roots at the shape interval endpoints.

### Scale zero

**Proved.** For free shape and scale on $
uge1$, $2k>n$ gives `Unbounded`. For $2k<n$ there is no scale-zero competitor. The prior uniform scale bound separates the optimizing scales from zero, including along the normal tail. For $2k=n$ the only possible scale-zero competitor is
$$
 C=nlog(2/pi)-2sum_{d_i>0}log d_i.
$$
This is the limit of the profile at $
u=1$ and the strict supremum at shape one over positive scales, by the prior collapse and uniform tail bounds. The optimized fixed-endpoint profile also bounds every path with moving location.

There is a signed collar that excludes every finite point in it from tying or beating $C$. The scale equation yields $sum_{y_i>0}u/(u+y_i)=p(
u-1)/(
u+1)$, so $ule y_{max}(
u-1)/2$, with extrema over the positive squared residuals. Consequently
$$
 P'(
u)le nlog 2-rac p2
       log!left(1+rac{2y_{min}}{y_{max}(
u-1)}ight)<0
 quad	ext{for }1<
ule1+rac{y_{min}}{16y_{max}}.
$$
The profile tends to $C$ at the lower end, hence is strictly below $C$ throughout the punctured collar. In fact $usim p(
u-1)/(2sum_{y_i>0}1/y_i)$ by the same scale equation, so $P(
u)-C=	frac p2(
u-1)log(
u-1)+O(
u-1)$. The collar is a strict comparison even when $C$ is the incumbent.

### Half-normal

**Proved.** Put $M_j=n^{-1}sum_i d_i^j$. For positive $M_2$, the free-scale normal face has $sigma=sqrt{M_2}$ and value $H=	frac n2[log(2/pi)-1-log M_2]$. With $sigma$ held, evaluate the half-normal at that scale. In the free-scale case, for $h=1/
u$ the first inward profile coefficient per observation is $(K-3)/4$, with $K=M_4/M_2^2$. The second is
$$
 rac{K}{4}-rac{M_6/M_2^3}{6}+rac{(K-1)^2}{4}.
$$
For the derivation, set $eta=logsigma$ and $q_i=y_i e^{-2eta}$. Expanding the data kernel $-(1+h)log(1+h q_i)/(2h)$ gives coefficients $q_i^2/4-q_i/2$ and $q_i^2/4-q_i^3/6$. The normalizer relative to its normal value contributes $-h/4+O(h^3)$ per observation by the integral below. At the normal scale $overline q=1$, the $eta$ derivative of the first coefficient is $1-K$, and the normal profile's conditional curvature in $eta$ is $-2$ per observation. Substitution of the first-order change in $eta$ contributes $(K-1)^2/4$ to the second coefficient. If scale is held, the first coefficient is $overline{(q^2-2q-1)}/4$ and the second is $overline{q^2/4-q^3/6}$.

A positive leading coefficient supplies a finite point strictly above $H$. With $2k<n$ this proves existence of a finite maximizer. With $2k=n$ it proves existence if also $Cle H$. For example, $(0,1,2,3,32)$ has $2k<n$ and $K>3$. A negative leading coefficient proves $P<H$ in a punctured normal collar only. For an explicit collar, enclose the profile derivative in $h$ using the integral-moment and conditional-scale bounds of the [Student cover](2026-11-25-tcert-math-report.md#normal-face). A strict sign at zero persists in sufficiently short interval boxes. When comparing a finite witness already above $H$, the dossier's uniform value bound is an alternative tail exclusion.

For half ties the face-versus-face comparison is particularly cheap. If $G$ is the geometric mean of the positive residuals, then
$$
 rac{C-H}{n}=	frac12lograc{2e M_2}{pi G^2}.
$$
This ranks the two faces without optimizing over shape. The maximum can still be finite when $C>H$, as the exact crossing examples demonstrate.

**Proved, equal positive levels.** If all $p$ positive residuals equal $d>0$ and $1le kle p$, the all-free restricted fit always escapes to the half-normal. Let $r=k/p$ and $w=k/n$. The scale root is $u=d^2(
u-r)/(1+r)$. Substituting in the profile score gives
$$
 2P'(
u)/n=int_0^infty e^{-
u t}
 left[rac{2}{1+e^{-t}}-rac{1}{1+r}int_{-1}^{r}e^{zt},dzight]dt.
$$
The logarithm of $(
u+1)/(
u-r)$ in the score has this integral representation by integrating $1/(
u-z)$ over $[-1,r]$. The bracket starts at zero with derivative $1-r/2>0$. Its first term is strictly concave for $t>0$ and bounded, and its second term is strictly convex and tends to infinity since $r>0$. Thus the bracket is positive and then negative, with exactly one positive crossing $t_0$. If the integral is nonnegative at some shape, multiplying the integrand by $e^{-(
u-
u_0)(t-t_0)}$ at any larger shape $
u>
u_0$ strictly improves its signed integral after factoring out the positive $e^{-(
u-
u_0)t_0}$. Hence an interior stationary point can only be a strict minimum. The lower-end value $P(1)$, interpreted as $C$ when $k=p$, satisfies
$$
 rac{P(1)-H}{n}
 =	frac12log(2/pi)+	frac12
  +(	frac12-w)lograc{1-2w}{1-w}-(1-w)log2
 le	frac12(1-logpi)<0.
$$
The product with the logarithm at $w=1/2$ is interpreted by continuity. With no interior maximum and a strictly worse lower end, every finite value is strictly below $H$. This covers, for example, $(0,1)$ and $(0,1,1)$. The fitter can detect this slice by two residual levels and the integer tie count.

## Exact finite and collapse ties

**Proved by analytic reduction and rational interval comparisons.** Define two ordinary samples by writing the observations with multiplicities as
$$
 X_0(D)=(0^{[100]},1^{[99]},D),qquad
 X_1(D)=(0^{[102400]},1^{[101377]},D^{[1024]}).
$$
Use weights $(100,99+epsilon,1)$, where $epsilon=0$ for $X_0$ and $1/1024$ for $X_1$. The weighted likelihood is the ordinary likelihood for $X_0$ and the ordinary likelihood divided by 1024 for $X_1$. In this section $n=200+epsilon$. Let $L_epsilon(D)$ be the lower-end value, $C$ for $X_0$ and the attained $P(1)$ for $X_1$. Let $	au=17/16$.

For each weight and every $D$ in $I=[35/4,37/4]$, the profile has exactly one stationary minimum in $(	au,3)$ and exactly one stationary maximum in $(3,64)$. Denote the latter shape by $v_epsilon(D)$. There is a unique $D_epsilon$ in the open interval $I$ solving
$$
 Pig(v_epsilon(D);Dig)=L_epsilon(D).
$$
This equation and the specified interval define the sample exactly. The stationary maximum is simple. For $D<D_epsilon$ within $I$ it is the unique global maximizer. At $D=D_0$, the interior maximum is the **unique finite global maximizer** and ties the excluded scale-zero face. For $D>D_0$ within $I$, collapse strictly wins. At $D=D_{1/1024}$, the Cauchy floor and the interior maximum are **exactly the two finite global maximizers**. For $D>D_{1/1024}$ within $I$, only the Cauchy floor wins. The two right-hand classifications also hold beyond $I$, by the monotone comparison proved below. On the dyadic sample $X_1(35/4)$ the two strict local maxima already disprove all-free unimodality, and the interior one is the unique global winner. On $X_1(37/4)$ the floor is the unique global winner. On $X_0(37/4)$ the finite supremum is unattained.

### Proof of the comparisons

For $1<
ule	au$ and $0leepsilonle1$, the scale equation gives $(99+epsilon)u/(1+u)le 100+epsilon-(200+epsilon)/(
u+1)$. The ratio of the right side to $99+epsilon$ increases in both $epsilon$ and $
u$ on this rectangle. At $(1,	au)$ in the $(epsilon,
u)$ coordinates the resulting bound gives $u<1/25$. Since $Dge1$, all positive log penalties exceed $log26>3$ and
$P'(
u)<201log2-150<0$. The same bound applies at $
u=1$ when $epsilon>0$. Thus all shapes between the lower end and $	au$ lose to the lower value.

The normal value is below the lower value for both samples throughout $I$ and for all $Dge9/2$. For $epsilon=0$, $C-H$ increases for $D>1$, since its derivative in $log D$ is $-2+200D^2/(99+D^2)>0$. At $D=9/2$, $C-H>1/100$. This follows by raising the comparison to an integer power using $e>271828/100000$, $pi<314160/100000$, and $e^{1/100}<100/99$. At $epsilon=1/1024$ the additional positive residual 1 has half-normal density at most $sqrt{2/pi}<1$ over all scales, so $H_epsilonle H_0$. Evaluating the shape-one likelihood at $u=10^{-5}$ gives $L_epsilon>C-8/1000$. Indeed its loss from $C$ is at most $100u+epsilon(7+u)$, by $log(1+u)le u$, $log(2/pi)>-1$ and $log(10^5)<12$.

For $
uge64$, the normal-chart profile derivative in $h=1/
u$ is strictly positive throughout $I$. To see this, write $v=sigma_*^2$, so $vle M_2$ by Jensen applied to the scale equation. With $q=y/v$ and $W=q/(1+hq)$, the first derivative per observation is $c'(h)-overline W/2+overline{q^2 J}/2$, where $c$ is the relative normalizer and $J=int_0^1 t(1+hqt)^{-2},dt$. Here $c'ge-1/4$, $overline W=1/(1+h)$, and $q^2Jge[y/(M_2+hy)]^2/2$. Even retaining only the outlier in the last term makes the sum positive if
$$
 (35/4)^4 > 3(200+epsilon)
   left[rac{99+epsilon+(37/4)^2}{200+epsilon}
           +rac{(37/4)^2}{64}ight]^2.
$$
The rational verifier checks this inequality. Thus $P'(
u)<0$ for every $
uge64$, including immediately to the left in $h$ of the finite tail endpoint.

The remaining comparisons are reproduced by [isolate.py](2026-11-25-halft-math-data/isolate.py). For each weight it checks 198 rational boxes, counting the separate slice at $
u=3$. On that slice the score is positive. On $[	au,3]$ each box excludes a zero by score or encloses only strictly positive curvature. On $[3,64]$ each box excludes a zero by score or encloses only strictly negative curvature. By the signs at the two outer endpoints each shape interval contains a zero. Since every zero in its interval crosses in the same direction, there can be only one there. The curvature check includes the full closed boxes and covers their joins.

For reproducibility, the equation for $u$ in each box is a quadratic with leading coefficient $t=n/(
u+1)$, linear coefficient $t(1+D^2)-(99+epsilon)-D^2$, and constant $(t-100-epsilon)D^2$. Its positive root increases with $
u$ and $D$ by the scale equation. The verifier encloses its square root by integer square root on a dyadic mesh. For $b'$ and $b''$ it shifts by 64 via $b'(
u)+b'(
u+1)=1/
u$ and the differentiated recurrence. At the shifted point $z$, integration of $x-x^3/3le	anh xle x$ bounds $b'$ between $1/(2z)+1/(4z^2)-1/(8z^4)$ and $1/(2z)+1/(4z^2)$, and bounds $b''$ between $-1/(2z^2)-1/(2z^3)$ and that value plus $1/(2z^5)$. The verifier uses their monotonicities on boxes. For logarithms it reduces arguments to $[1,2]$ by powers of two and uses the first 16 terms of $2operatorname{atanh}((r-1)/(r+1))$ with a geometric remainder bound. The likelihood heights use $-	frac14hle c(h)le-	frac14h+	frac1{24}h^3$, direct shape-one normalization, and rational bounds for $pi$ from Machin's formula and the alternating arctangent series. These comparisons use rational arithmetic throughout.

The certified brackets for $P(v_epsilon(D);D)-L_epsilon(D)$ at the two ends of $I$, in weighted total log-likelihood units, are

| Weight $epsilon$ | $35/4$ | $37/4$ |
|---|---:|---:|
| $0$ | $[489,623]/4096$ | $[-705,-559]/4096$ |
| $1/1024$ | $[513,647]/4096$ | $[-681,-534]/4096$ |

To prove uniqueness of the crossing, put $q_D=D^2/(u+D^2)$. The derivative of any profiled finite value in $log D$ is $-(
u+1)q_D$ by the envelope identity. For $D>1$,
$$
 (
u+1)q_D=rac{n}{1+(99+epsilon)(u+D^2)/[D^2(u+1)]}
$$
increases strictly with $u$, hence with $
u$. Its limit at the collapse end is 2 when $epsilon=0$. At the normal end its limit is the negative of the derivative of $H_epsilon$ in $log D$. Thus every shape in $[	au,infty]$ has a strictly decreasing value relative to $L_epsilon$ as $D$ increases. The maximum over that compactified interval is continuous, by the analytic normal-side extension of the scale in $h$ and continuity at finite shapes. It is strictly decreasing by evaluating a maximizer for the larger $D$ at the smaller $D$. On $I$ that maximum is the interior maximum. Its opposite endpoint signs give exactly one crossing and establish the right-hand classifications even beyond $I$. At equality the lower collar, uniqueness of the two stationary roots and exclusion of the normal face account for the entire maximizing set.

### Numerical replay and input classification

**Numerical.** [check.py](2026-11-25-halft-math-data/check.py) uses 70-digit mpmath, an independent closed-form scale root, and 241 log-spaced shape scores on $[17/16,10^4]$ with local root refinement. It also verifies the elementary left-witness inequalities using exact fractions. Results for the real crossings are

| Sample | $D_epsilon$ | Interior $
u$ | Interior $sigma$ |
|---|---:|---:|---:|
| $X_0$ | 9.0039954851828020366233477 | 6.43980586684034264142 | 0.66174453372992499444 |
| $X_1$ | 9.0143075568291592701966078 | 6.43555819887501860419 | 0.66170118954434996087 |

The exact binary64 neighbors below and above the computed crossings give the following computed values for the interior height minus the lower value, in the weighted units above.

| Sample | Lower neighbor | Gap | Upper neighbor | Gap |
|---|---|---:|---|---:|
| $X_0$ | `0x1.2020bb23c6231p+3` | $9.71\times10^{-16}$ | `0x1.2020bb23c6232p+3` | $-5.24\times10^{-17}$ |
| $X_1$ | `0x1.2075351f15573p+3` | $1.01\times10^{-15}$ | `0x1.2075351f15574p+3` | $-1.47\times10^{-17}$ |

These signs are high-precision comparisons. The exact equalities are real crossings in the terminology of the [tie report](2026-11-25-ties-math-report.md#classification-of-the-constructions). The input arithmetic of the roots of the crossing equations remains open. Overlapping likelihood enclosures establish neither equality nor a singleton.

## Finite root list for general inputs

**Proved.** When the all-free restricted likelihood is bounded, it has only finitely many interior stationary shapes. This includes degenerate stationary points. At the regular lower end $2k<n$, analyticity extends across $
u=1$, which rules out accumulation there for a nonconstant profile. At $2k=n$ the signed collapse collar rules out accumulation. Real analyticity rules out accumulation on any interior compact interval, once nonconstancy has been established.

At the normal end use the scale equation in $v=sigma^2$,
$$
 n=(1+h)sum_i rac{y_i}{v+h y_i}.
$$
The derivative of the right side in $v$ at $(0,M_2)$ is $-n/M_2$, so $v(h)$ extends real analytically across zero by the implicit function theorem. The data kernel has a removable singularity at $h=0$ and is analytic there after composition with $v(h)$. The log scale term is also analytic. With held positive scale the same assertions about the data and scale terms hold directly. The sole remaining term, per observation and relative to the normalizer at zero, is
$$
 c(h)=-rac12int_0^infty e^{-t}	anh(ht/2),rac{dt}{t}.
$$
This is the normalizer identity from the [Student cover](2026-11-25-tcert-math-report.md#3-box-likelihood-scores-and-curvature). It is smooth at zero, with all derivatives justified by dominated differentiation. For order $jge1$, the derivative of $	anh$ is a polynomial in $	anh$, bounded on the real line, and the remaining factor is proportional to $e^{-t}t^{j-1}$. If $a_j$ is the Taylor coefficient of order $j$ of $	anh z$ at zero, the coefficient of $h^j$ in $c$ is $-a_j(j-1)!/2^{j+1}$. The Taylor radius of $	anh$ is $pi/2$ because $cosh z$ first vanishes at $z=pm ipi/2$ and the numerator does not vanish there. Its nonzero coefficients therefore have $|a_j|^{1/j}$ bounded away from zero along a subsequence. The factorial factor gives the series for $c$ radius zero.

As the other terms are analytic near zero, the Taylor coefficients of $P(1/h)-H$ cannot all vanish. Its first nonzero coefficient has finite order and its sign gives an eventual strict sign for the inward score by Taylor's theorem applied to the derivative. This proves nonconstancy and rules out stationary roots accumulating at infinity. On dyadic data the coefficients after subtracting $H$ can be computed from rational moments by the scale equation and formal differentiation. Exact coefficients terminate the search for the first nonzero term, in principle. For a quantitative collar, the derivative-polynomial bound above gives an integrable Taylor remainder for the normalizer and the analytic data term can be enclosed on a scale tube. There is no demonstrated small uniform order or practical cost bound for this general procedure.

The maximum is therefore among a finite list of interior stationary points, the attained floor when eligible, and the excluded faces. Existence is equivalent to at least one attainable candidate reaching both eligible excluded faces and equalling the largest finite candidate value. A finite witness **above** both eligible faces establishes existence without knowing the winning coefficients. To certify a simple root, take a shape interval with opposite score signs and curvature of one strict sign, and refine the resulting root and its scale tube to coefficient tolerance. Reject other boxes by a strict score sign or a likelihood upper strictly below a certified witness. For a tied maximum retain all surviving roots until an equality proof and a full count are available. A value-gap cover alone cannot perform either step.

## Does shape one matter for existence?

**Proved.** With free location and scale, the unrestricted $
u>0$ likelihood is unbounded for every nonempty sample. When $p>0$, choose any fixed $0<
u<k/p$ and collapse at the sample minimum. The log-scale coefficient is $
u p-k<0$. When $p=0$ every shape collapses. Thus a lower bound is needed in this mask. The exact sample-dependent bound that removes divergence is $k/p$, where equality permits the finite unattained conditional value. For a distinct sample of fixed size $n>1$ it is $1/(n-1)$. Among all sub-half-tie samples of arbitrary size, the ratios $k/p$ approach one from below, so one is the weakest uniform lower bound covering that class. For majority ties shape one still allows divergence. The half-normal escape can prevent attainment even when the lower bound removes divergence.

With **scale held**, the small-shape bound in the dossier tends to $-infty$ in total log likelihood, including observations at the endpoint. Thus no lower shape bound is needed to prevent this escape. Existence can still fail at the normal end. For example, if all $q_i=(d_i/sigma)^2le1$, then $log(1+hq)ge hq/(1+hq)$ implies $(1+h)log(1+hq)/hge q$, and $c(h)<0$ for $h>0$. Every finite shape loses to the matched half-normal. Conversely with $(0,4)$ and $sigma=1$ the inward coefficient is positive, and the lower end tends to $-infty$, so a finite maximum exists on both the unrestricted and restricted domains. This separates the mathematical role of the floor from the current held-scale policy in R5.

## Reproduction

Both final runs finished green on the workstation through the single-slot wrapper. `isolate.py` verified the rational cover, tail sign and crossing brackets for both weights. `check.py` verified the elementary witness checks and reproduced both crossings and their binary64 neighbors. These are the math-only final checks.

```sh
export TMPDIR="$PWD/build/tmp"
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-25-halft-math-data/isolate.py
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-25-halft-math-data/check.py
```

## What this changes for the fitter

- Detect $k$ by exact equality to the sample minimum. With held shape and free scale, apply the sign of $
u p-k$ and the monotone scale solve. With free shape and scale, return proved `Unbounded` at $2k>n$. At $2k=n$, use $C$ and its strict punctured collar. At $2k<n$, include the attainable floor and give a winning floor estimate the restriction flag. Held scale skips collapse.
- On the two-level residual slice above, return a proved `NormalLimit` with the fitted half-normal scale and endpoint. Else use the face table, the inward moment sign and the normal score or value enclosure. Return `NormalLimit` or `ScaleBoundary` as a proved outcome only when that face reaches the largest eligible face value and every finite candidate loses strictly. When two excluded faces tie and both strictly beat the finite list, record both diagnostics and report nonattainment. An attained finite fit equalling an excluded face stays an estimate, with the face in diagnostics.
- Reuse the positive-gap search to propose witnesses. Add the monotone $u$ tube and score/curvature cover on the finite remainder. A single included winner at coefficient tolerance gives a certified singleton. Proved finite/finite equality with a complete list gives a certified nonsingleton and all coefficients, with the restriction flag on the floor member. Use separate evidence for certification and uniqueness. Pointwise score sign, root cardinality and coefficient accuracy are independent of the likelihood gap.
- Preserve a converged, locally checked search estimate with `certified=false` and uniqueness unknown if a sign, ordering or root inclusion remains unresolved, or the certificate budget is exhausted. Follow the failure rule when the search itself fails or the certificate proves its estimate wrong. Use the stated work allowances for evaluations and solves, and prefer log or normalized residuals for extreme inputs. Exact dyadic inequalities in the study use arbitrary-size integers. Production comparisons need explicit numerical allowances.

## Open

- General bounded-data certification still needs a practical treatment of multiple stationary roots, an upper bound or good stopping criterion for the order of normal contact, and measured interval budgets. Finiteness of the candidate set does not supply numerical separation or an oracle for zero.
- The two explicit crossing equations yield isolated exact real inputs. Their rationality and the existence or exclusion of exact binary64 finite/finite and finite/collapse ties remain open. A general exact comparison of distinct critical values, or of a critical value with the half-normal, is also open. The neighbor checks show why a fixed likelihood tolerance cannot replace these comparisons.
- A cheap general pointwise decision beyond the tie count, residual-level slices, face ranking and signed collars remains open. In particular, a local negative normal coefficient does not settle the global comparison, and the normal face can only be selected after the bounded part is covered.
