# Burr XII global ties and a stationary-point bound

TASK-306 · 2026-11-25 · Base revision e1ff04c5

**Proved.** All-free Burr XII can have two separated strict finite global
maximizers whose common likelihood exceeds both Pareto and Weibull. A
one-parameter deformation reverses the order of the two regional maximum
heights while both stay above the faces. A second construction gives seven
observations for which a finite global maximum equals the optimized Weibull
likelihood and strictly exceeds Pareto. For a nonconstant sample with $m$
distinct observations, the profile has at most
$6\,2^{m(m+1)/2}8^{m+1}$ distinct stationary points, including degenerate ones.

All values are per observation on the log-data scale. Write $y_i=\log x_i$,
$b=-c\log s$, $z_i=cy_i+b$, $h(z)=\log(1+e^z)$, and
$p(z)=1/(1+e^{-z})$. The mean log likelihood and profile are
$$
 L_y(k,c,b)=\log(kc)+\overline{z-(1+k)h(z)},\qquad
 F_y(k)=\max_{c>0,\ b\in\mathbb R} L_y(k,c,b).
$$
Bars include multiplicities. We use the earlier strict conditional concavity
and coercivity, Pareto and Weibull bounds, and the attainment criterion.
The negative definite nuisance Hessian gives a real-analytic conditional
branch by the implicit function theorem, so $F_y$ is real analytic on
$k>0$. It is nonconstant because it tends to the optimized Pareto value
$P_y$ as $k\downarrow0$ and is strictly below $P_y$ for all sufficiently
small positive $k$. Every interior local maximum of $F_y$ is therefore
strict and gives a strict local maximum in all three coordinates.
[Closure and Pareto bounds](2026-11-03-burr-closure-report.md) ·
[Prior Burr results](2026-11-25-burr-math-report.md)

## Two finite global maximizers above both faces

**Proved.** The earlier finite/Pareto transition supplies the seven bulk
log observations
$$
 Y(q)=(0,\ 0.23826q,\ 7q/15,\ 8q/15,\ 8q/15,\ 1,\ 1),
 \qquad 0\le q\le1 .
$$
Let $N=2^{90}$, $w=1/(7N+1)$, and let $F_{q,v}$ denote the profile
of the sample with $N$ copies of each entry of $Y(q)$ and one extra
log observation $v$. Put
$$
 a=\frac{1}{7N},\qquad d=\frac18,\qquad K=2^{24},\qquad
 \mathcal L(q,v)=\max_{a\le k\le d}F_{q,v}(k),\qquad
 \mathcal R(q,v)=\max_{d\le k\le K}F_{q,v}(k).
$$
The **exact sample** consists of the exponentials of these log observations
at $v=-1$ and
$$
 q_*=\min\{q\in[0,1]:\mathcal R(q,-1)=\mathcal L(q,-1)\}.
$$
The two intervals exhaust all possible global maximizers. The maxima
of each at $q_*,v=-1$ are attained in their interiors, with the lower
one in $a<k<2^{-20}$ and the upper one in $d<k<K$. The argument
adapts the regional-height construction for Lomax. Here the extra
observation lowers the Pareto face while a finite Burr point retains
nearly all of the old Pareto height. [Lomax construction](2026-11-25-lomax-uniq-math-report.md) ·
[Prior finite/Pareto transition](2026-11-03-burr-closure-report.md#a-finite-mle-can-equal-the-best-boundary-likelihood)

### A common witness and an exhaustive cover

Write $D=\overline{Y(q)}$, and use $P_b,W_b,F_b$ for the bulk faces
and profile at $q$. The earlier majorization proof shows that
$W_b-P_b$ is increasing in $q$. Its value at 1 gives
$P_b-W_b>2\times10^{-6}$ uniformly by interval comparison. For
$-1.1\le v\le-0.9$, the new Pareto mean excess is $(1-w)D-v$,
so its optimized likelihood is below $P_b-0.4$. At each Weibull
power $c$, adding the outlier changes the profiled value by at most
$wc(v-D)-\log(1-w)$. Indeed, the new exponential moment is at least
$1-w$ times the old one. Thus the new Weibull value is at most
$W_b-\log(1-w)<W_b+10^{-10}$.

A single feasible family in the lower band gives
$\mathcal L>P_b-10^{-8}$, uniformly in $q,v$ on this rectangle. To
see this, set
$$
 k_e=2^{-40},\qquad c_e=\frac{1}{k_eD},\qquad
 b_e=\log\frac{1-6k_e}{7k_e}.
$$
For the bulk likelihood substitute these parameters into the existing
Pareto lower bound. Treat the first zero as the singled-out minimum.
The resulting lower bound is $P_b+Q(k_e)-E_e$, where
$$
 \begin{aligned}
 Q(k)&=k\log\frac{k}{M(k)}+[M(k)-k]\log\frac{M(k)-k}{M(k)},
 & M(k)&=\frac{1+k}{7},\\
 E_e&=\frac{6k_e(1+k_e)}{1-6k_e}.
 \end{aligned}
$$
The remainder uses $h(-b_e-c_e y)\le e^{-b_e-c_e y}\le e^{-b_e}$
on each of the six other entries, so the same lower bound applies
at $q=0$ with its additional zeros. The outlier logit is negative
and contributes at least
$\log(k_ec_e)+b_e-1.1c_e-(1+k_e)$. Combining the seven bulk
weights and $w$ gives the asserted lower bound. The outward
difference from $P_b$ is greater than $-3.0\times10^{-11}$.

For an upper comparison at arbitrary $k$, let
$\lambda=c\min(1,k)$. The prior density envelope at the two bulk
endpoints 0 and 1 gives
$$
 L_{q,v}(k,c,b)\le \log\lambda+1-\lambda(1-w)/14.
$$
Each endpoint has weight at least $(1-w)/7$, and the sum of their
distances from the mode is at least one. The right side is decreasing
and below $-2$ from $\lambda=1000$ onward. Each individual log
density is also at most $\log\lambda$. Consequently every fit at
height at least $-2$ satisfies $\lambda<1000$, and
$$
 F_{q,v}(k)\le \max\{-2,\ F_b(k)+\varepsilon\},\qquad
 \varepsilon=w(\log1000+2)<10^{-10}.
$$
This follows by bounding the two contributions by
$(1-w)F_b(k)+w\log1000$ and using the cutoff $-2$.
The interval comparisons give $P_b>-1.89$ throughout.

The following comparisons cover the shape domain and establish
opposite endpoint orders at $v=-1$.

| Shape range or endpoint | Upper comparison or witness |
| --- | --- |
| $0<k\le a$ | The strict small-$k$ Pareto exclusion applies since the sole new minimum has weight $w$, and $a=w/(1-w)$. |
| $k\ge K$ | The unsigned bulk Weibull bound $F_b(k)\le W_b+7(k+1)/[2(k+1-7)^2]$, together with the mixture upper, puts the whole tail below $P_b-9\times10^{-7}$. |
| $2^{-20}\le k\le d$ | The bulk Pareto bound $F_b(k)\le P_b+Q(k)$ uses minimum mass at least $1/7$. Increasing that mass decreases the entropy upper. The function $Q$ is convex as the perspective of negative binary entropy on an affine line. Its endpoint values are both below $-10^{-6}$, and $Q(d)<-0.085$. The mixture upper therefore excludes the whole range, including the separator, from competing with the lower witness. |
| $q=0,\ k\ge d$ | The bulk profile is below $P_b-0.001$ by a dual/chord cover of $[1/8,64]$ and the unsigned Weibull upper for $k\ge64$. The mixture upper gives $\mathcal R(0,-1)<\mathcal L(0,-1)$. |
| $q=1,\ k\le d$ | The bulk profile is below $P_b$ by the Pareto bound. The full-sample feasible point $(k,c,b)=(1,4.932,-2.624)$ exceeds $P_b+0.003$, so $\mathcal R(1,-1)>\mathcal L(1,-1)$. |

The regional maxima are continuous in $(q,v)$ by joint continuity
on their fixed compact shape intervals. The strict endpoint orders
and the intermediate value theorem give the least equality $q_*$
in $(0,1)$. The common height is above both faces by the uniform
lower witness. Both regional optima are interior by the strict
domain exclusions. Together these two regions and their excluded
tails cover every finite shape. [Interval comparisons](2026-11-25-burr2-math-data/certify.py)

### Strict crossing with a single observation moving

**Proved.** Fix the bulk at $q_*$ and vary just $v$ on
$[-1-\delta,-1+\delta]$, where $\delta=2^{-120}$.
The right optimum at $v=-1$ has $c\le8000$ from the envelope and
$k\ge d$. Retaining its parameters over this interval, its height
changes by less than $10^{-9}$ since the absolute derivative in $v$
is bounded by $8000w(1+K)$. Thus both regional heights are above
$P_b-2\times10^{-8}$ throughout. The exclusions in the table keep
them interior and above both faces.

For each lower-band optimizing triple, $k<k_0=2^{-20}$ and
$c>c_{\min}=e^{-1.9}/k_0$ by the density-height bound and the
regional witness. Its $c$ is also at most $1000/a$.
At that triple the outlier logit must be negative. If it were
nonnegative, its logistic weight would be at least $1/2$ and
all the bulk weights would be at least $1-e^{-0.9c_{\min}}$.
The intercept score would then be at most
$$
 w-k(1-w)-(1+k)w/2+(1+k)e^{-0.9c_{\min}}<0 .
$$
Here $k>a=w/(1-w)$ and
$(1+k_0)e^{-0.9c_{\min}}<w/4$. This violates the conditional
intercept equation. When the same triple is evaluated anywhere else
in the tiny deformation interval, the logit increases by at most
$2\delta\,1000/a<\log 2$, so its logistic weight stays below $2/3$.
The height derivative of that fixed triple is therefore bounded by
$$
 \partial_v L_{q_*,v}
   =wc[1-(1+k)p(cv+b)]
   > wc_{\min}(1-2k_0)/3>8000w .
$$
For every right-band optimizer the same derivative, at its fixed
parameters, is at most $wc\le8000w$ throughout the interval.
For $v_1<v_2$, retain a lower-band optimizer from $v_1$ to
lower-bound $\mathcal L(q_*,v_2)-\mathcal L(q_*,v_1)$. Retain a
right-band optimizer from $v_2$ to upper-bound the corresponding
right increment. It follows that $\mathcal R-\mathcal L$ strictly
decreases in $v$. The upper-$k$ band wins at $v=-1-\delta$ and
the lower-$k$ band wins at $v=-1+\delta$, with equality at $-1$.
In particular, each endpoint sample also has two separated strict
local maxima above both faces.

## A finite maximum equal to a winning Weibull face

**Proved.** Take the exact log-data family
$$
 U(t)=(0,\ 1-10t/15,\ 1-8t/15,\ 1-7t/15,\ 1-7t/15,\ 1,\ 1),
 \qquad 9/10\le t\le1 .
$$
The **exact sample** consists of the seven exponentials of
$U(t_*)$, where
$$
 J(t)=\max_{1/6\le k\le 2^{16}} F_{U(t)}(k)-W_{U(t)},\qquad
 t_*=\min\{t\in[9/10,1]:J(t)=0\}.
$$
The following outward comparisons prove the existence of this least
zero and its global interpretation.

- On the full interval, $W-P>0.05$. One observation is the unique
  minimum, so every $0<k\le1/6$ is strictly below $P$.
- On the full interval, the signed seam theorem excludes all
  $k\ge2^{16}$ relative to the same exact $W$. The checker divides
  the data interval into 1,000 closed slabs of width $10^{-4}$.
  On each slab it brackets the Weibull power with opposite signs
  of $W'(c)$ and propagates to the exact Weibull nuisance solution.
  In mean units the slope satisfies
  $A=\overline{v_i^2}/2-1<-0.0069$, where
  $v_i=e^{c_W y_i}/\overline{e^{c_W y_i}}$.
  With nuisance radius $R=1/16$, $h=2^{-16}$, Hessian lower
  bound $\mu$, and score bound $D$ from the prior derivation,
  the checker verifies $hD<\mu R/2$ and
  $h(1/6+D^2/(2\mu))<-A/2$ on every slab.
  This gives strict exclusion all the way to the face.
  [Signed seam proof](2026-11-03-burr-xi-data/derivation.md) ·
  [Propagation of Weibull brackets](2026-11-04-burr-root-data/derivation.md)
- At $t=9/10$, the compact dual/chord cover of $[1/8,8192]$
  lies strictly below a feasible Weibull likelihood. The same
  signed seam test at this endpoint passes with cutoff $8192$.
  Thus $J(9/10)<0$.
- At $t=1$, the feasible triple $(1,5.16,-2.816)$ has a
  likelihood more than $0.009$ above an upper bound on Weibull.
  Hence $J(1)>0$.

Joint continuity of the finite profile and continuity of the
optimized Weibull value give continuity of $J$. At $t_*$ the
regional maximum is attained at finite positive $k$ with value
$W$. The two strict tail exclusions eliminate all shapes outside
the compact interval, and $W>P$ eliminates Pareto. The resulting
finite global maximum is strict locally. A sequence escaping to
Weibull attains the same limiting height. [Interval checker](2026-11-25-burr2-math-data/certify.py)

## Bound on the number of stationary points

**Proved.** Aggregate equal logs into $m$ distinct values with their
empirical weights. Write
$$
 A=\overline{h(cy+b)},\quad B=\overline{p(cy+b)},\quad
 C=\overline{yp(cy+b)},\quad D=\bar y.
$$
The nuisance scores are $1-(1+k)B$ and $1/c+D-(1+k)C$.
On their unique conditional branch the normalized profile score is
$G(k)=kF_y'(k)=1-kA$. The function $G$ is analytic and
nonconstant. A constant $G$ would make $F_y$ affine in $\log k$.
Its finite Pareto limit forces that affine function to be constant,
contrary to the strict Pareto exclusion near zero.

For $|e|<1$ solve $G(k)=e$. Eliminating $k=(1-e)/A$ from
the nuisance scores and clearing positive factors gives
$$
 A-(A+1-e)B=0,\qquad
 A+cAD-c(A+1-e)C=0 .
$$
The chain $(p_1,\ldots,p_m,A)$ is analytic on all of $\mathbb R^2$
in $(c,b)$. Each $p_j$ has partial derivatives given by $p_j(1-p_j)$
times the corresponding coefficient of $cy_j+b$.
The derivatives of $A$ are $C$ and $B$, linear in the $p_j$.
This is a Pfaffian chain of order $r=m+1$ and differential degree
2. The two equations have polynomial degrees at most 2 and 3 in
the variables and chain. Khovanskiĭ's regular-root bound on
$\mathbb R^2$ is
$$
 M=2^{r(r-1)/2}\cdot 2\cdot3\,
       [2+3-2+2\min(2,r)+1]^r
   =6\,2^{m(m+1)/2}8^{m+1}.
$$
The bound applies on the whole plane, hence also on its positive-$c$
part. [Khovanskiĭ theorem, proof and statement by Lotz and Natarajan](https://arxiv.org/html/2607.29267v1#S1)

At a level root with $G'(k)\ne0$, the three-variable Jacobian of
the two nuisance scores and $1-kA-e$ is nonsingular by the
Schur complement. The nuisance block is invertible, and the
remaining scalar is $G'(k)$ along its conditional solution.
The $k$-derivative of $1-kA-e$ with $c,b$ fixed is $-A\ne0$,
so elimination preserves regularity in the two remaining equations.
Distinct level roots have distinct $(c,b)$, since those coordinates
determine $k$ at each fixed $e$.

Finally, select any finite set of zeros of $G$ and disjoint small
intervals around them. Analyticity gives a first nonzero term
$a(k-k_*)^j$ at each zero and strict monotonicity on each side
sufficiently close to the root. For sufficiently small
$\epsilon>0$, an odd $j$ supplies one regular solution at each
level $\epsilon,-\epsilon$. An even $j$ supplies two regular
solutions at the level of the matching sign. Each selected zero
thus supplies two distinct solutions across the two systems,
and each system has at most $M$ regular solutions. Every finite
selection has size at most $M$, proving the asserted bound on
all distinct stationary points. The constant-data route has
unbounded likelihood by concentration.

## Reproducible checks

Both compact covers use outward 65-decimal interval arithmetic and
arbitrary feasible entropy duals at the knots. Each dual is forced
feasible by correcting one logistic weight exactly in the formula
$\bar p=1/(1+k)$. For a knot interval $[l,u]$ the existing chord
upper is $\max(U_l,U_u)+(u-l)^2/(8l^2)$, where the $U$'s are the
knot upper bounds. The bulk cover used 10 knots and 9 accepted
cells, with smallest outward separation greater than $0.2213$
from its threshold. The Weibull endpoint cover used 572 knots
and 571 accepted cells, with separation greater than $2.91\times10^{-8}$.
Numerical conditional solves only propose dual weights.
[Dual and chord derivation](2026-11-04-burr-root-data/derivation.md) ·
[Checker](2026-11-25-burr2-math-data/certify.py)

**Numerical.** Binary64 reconnaissance used seven two-point weight
patterns and two 21-point data deformations, with 113 geometrically
spaced shapes from $1/128$ to $512$. The upper-mass deformation
identified the interval used in the seven-observation proof.
The independent 80-decimal replay solves the stationarity and equality
equations near the candidate branches. Its results locate numerical
branch crossings. The least-zero definitions above specify the exact
samples and provide their global guarantees.

| Replay | Deformation value | Stationary $k$ | Stationary $c$ | Stationary $b$ |
| --- | ---: | ---: | ---: | ---: |
| Finite pair, lower shape | $q=0.99291020580472189458$ | $2.60632588579532\times10^{-15}$ | $7.14485093740583\times10^{14}$ | $31.6349247228624$ |
| Finite pair, upper shape | same $q$ | $0.808130480334586$ | $5.36594964212106$ | $-2.48940963648451$ |
| Finite/Weibull | $t=0.96024095654752758625$ | $1.13354929771781$ | $5.01074624983288$ | $-3.00882505662592$ |

For the numerical finite pair the gaps over Pareto and Weibull are
approximately $1.05158391981622$ and $0.00625916749846$.
At the numerical finite/Weibull crossing $W-P$ is about $0.102666405013$
and the signed seam slope is about $-0.014812625173$.
[High-precision replay](2026-11-25-burr2-math-data/replay.py) ·
[Grid](2026-11-25-burr2-math-data/explore.py)

All three final Python runs exited zero on the workstation, in
sequence through the one-slot runner. The scripts reuse prior
conditional proposals and likelihood routines from the Burr study.
The job used Python only. Reproduce from the worktree root.

~~~sh
set -e
export PYTHONDONTWRITEBYTECODE=1
d=dev/research/evaluations/2026-11-25-burr2-math-data
for script in certify.py replay.py explore.py; do
  /tmp/umvl-cpu/run python3 -u "$d/$script"
done
~~~

## What this changes for the fitter

- All-free Burr can require several finite competitors even after a
  witness has beaten both faces. To certify a singleton, isolate
  its coefficients to the requested tolerance and exclude both
  faces and the full complementary profile, including other root
  boxes. To certify nonuniqueness, prove equality at distinct finite
  roots and exclude all higher competitors. Return the full maximizing
  set with the nonunique flag where supported, or report the
  nonuniqueness diagnostic if the interface cannot represent the set.
- A proved finite/Weibull tie is a finite outcome. A signed negative
  seam bound excludes a tail relative to Weibull and supports a
  named Weibull stop only with strict exclusion of the remaining
  finite profile and Pareto. Overlapping height intervals leave the
  comparison unresolved. If a certificate fails to close and a
  successful search result survives refutation, keep it uncertified.
- The stationary bound proves finiteness even at multiple roots.
  Its size makes it unsuitable as a production work budget or an
  enumeration stopping rule. The profile and equality results
  transfer to inverse Burr and to shifted Burr with held threshold
  under the earlier exact transformations. [Transfer proof](2026-11-25-burr-math-report.md#inverse-and-shifted-burr)

## Open

- The finite-pair construction proves at least two global maximizers.
  Exact cardinality needs a root isolation or uniqueness argument
  inside each winning interval. The sample uses $7\cdot 2^{90}+1$
  observations counting multiplicities. A small unweighted example
  would need a new construction or a sharper quantitative analysis
  that tolerates a much heavier outlier.
- The two experimental branch crossings have high-precision
  residuals. Enclosing the least zeros in narrow data-parameter
  intervals would also require uniform exclusion of earlier
  competing branches. Exact ties for binary64 inputs remain open.
- A practical or sharp stationary bound needs additional information
  about the sign changes of the conditional profile score.
