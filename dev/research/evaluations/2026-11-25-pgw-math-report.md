# Power generalized Weibull likelihood existence and faces

TASK-352 · 2026-11-25 · Base `f22e71d9ca129f3568b9b9955e31ddc433603cb2`

**Proved.** On a nonconstant positive sample, every PGW escape with likelihood bounded away from zero goes to the power-transformed Gompertz or lower-threshold conditional Weibull face, up to subsequences. Holding scale at the sample minimum forces a factor of $2^{-m}$ in the threshold likelihood, where $m$ is the minimum's multiplicity. Holding scale at or below the minimum makes the applicable threshold envelope strictly better than every finite fit, except when all observations equal the held scale, in which case the likelihood is unbounded. Holding scale above the minimum on nonconstant data guarantees finite attainment.

**Proved.** All-free attainment depends on the sample. The exact sample $(1,2)$ selects the threshold face, while $(1,2,4,4,4,8)$ has a finite MLE by an interval comparison with both optimized faces. The exact per-input rule for the remaining comparisons uses a finite list of stationary profile values and the applicable face values. The proof gives an explicit cutoff for the threshold tail and an analytic derivative test for the Gompertz collar. Equality with an excluded face preserves any finite MLE. Detecting general equality between transcendental critical values remains open.

The [PGW dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pgw/README.md) is at stage `dpqr`. It has neither `mle.hpp` nor an implemented hold mask. The analysis covers all eight proposed masks under R2, on the finite strictly positive input domain of the [prior theory](2026-11-11-pgw-theory-report.md). The [d/p/q/r report](2026-11-12-pgw-dpqr-report.md) supplies the endpoint density convention. The [ties report](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter) supplies the comparison and outcome rules.

## Attainment by held coefficients

**Proved.** Write $a,b,c$ for scale, shape and exponent. All holds are finite and positive. In the table, $G$ and $T$ denote the optimized log likelihoods of the Gompertz and threshold envelopes. Compare them with stationary values using the finite criterion below. The boxes and comparisons use exact arithmetic.

| Held | Finite existence, uniqueness and outcome |
|---|---|
| $a,b,c$ | Evaluate the specified model. |
| $a,b$ | Unique finite exponent on every positive sample. The strictly decreasing exponent score has opposite signs at its ends. |
| $b,c$ | Unique finite scale on every positive sample. Use the strictly monotone log-scale score. |
| $a,c$ | Unique finite shape unless all observations equal $a$. In that equality case the likelihood is unbounded. |
| $c$ | Unique finite scale and shape on nonconstant data. Constant data give an unbounded likelihood. At $c=1$ this is exactly the Weibull fit on positive data. |
| $a$ | For $x_{min}<x_{max}$, $a>x_{min}$ guarantees finite attainment. For $a<x_{min}$ the threshold at $a$ strictly wins. At $a=x_{min}$ the threshold strictly wins with the factor $2^{-m}$. For constant $x_0$, the likelihood is unbounded if $x_0\le a$ and selects the threshold at $a$ if $x_0>a$. |
| $b$ | Constant data select the Gompertz face uniquely in its scale, with no finite MLE. On nonconstant data the Gompertz face strictly wins if $(x_{min}/x_{max})^b\ge 1/2$, including equality. Otherwise a finite MLE exists exactly when a retained stationary profile value is at least $G$. |
| None | Constant data give an unbounded likelihood. On nonconstant data a finite MLE exists exactly when a retained stationary profile value is at least $\max(G,T)$. |

The first four nontrivial rows and both constant concentration paths come from the [prior strict-concavity and constant-sample proofs](2026-11-11-pgw-theory-report.md#constant-data-and-all-hold-subsets). For example, the exact sample $(1,1)$ with $a=1,c=2$ fixed is unbounded as $b\to\infty$. With only $a=2$ held it is unbounded via $c=2^b$. The exact nonconstant sample $(1,2)$ with $a=2$ held has a finite MLE by the compactness proof below. With $a=1$ held it selects the penalized threshold. With $a=1/2$ held it selects the unpenalized threshold at that held scale.

The prior fixed-$c$ proof is a uniqueness result on the entire nonconstant positive slice, including exactly two distinct observations and all multiplicities. Its negative-definite Hessian in the affine log-data chart and coercivity give a unique root region before global comparison. With $c$ free, the compact criterion below identifies the entire maximizer set by finite comparisons. There is no general root-count-one claim on that slice.

## Bounds and exhaustive competitive escapes

**Proved.** In log-observation units set
$$
y_i=\log x_i,\quad \mu=\log a,\quad z_i=b(y_i-\mu),\quad
L(z)=\log(1+e^z),\quad q(z)=\frac{e^z}{1+e^z},
$$
$$
k=bc,\qquad u_i=cL(z_i),\qquad r_i=kq(z_i),\qquad
E(u)=\exp(u+1-e^u).
$$
The unlogged likelihood in these units is $P=\prod_i r_i E(u_i)$. The original log likelihood is $\log P-\sum_i y_i$. Thus all faces, values and bounds can be compared in log-observation units. Since $q\le L$, $r_i\le bu_i$ and $r_i\le k$. The elementary bound $E(u)\le e^{-u^2/2}$ for $u\ge 0$ gives
$$
uE(u)\le 1,\qquad u^2 E(u)\le 1,\qquad P\le \min(b^n,k^n).
$$
The last inequalities exclude $b\downarrow 0$ and $k\downarrow 0$ on any positive likelihood superlevel $P\ge W>0$.

Let $D=y_H-y_L>0$ be the log span, with subscripts $L,H$ denoting one observation at each extreme. If $z_L\le -bD/2$, then
$$
L(z_L)/L(z_H)\le 2e^{-bD/2}.
$$
For $z_H\le 0$ this follows from $e^z/2\le L(z)\le e^z$, since $z_H-z_L=bD$. For $z_H>0$, use $L(z_H)\ge \log 2>1/2$. Bounding the two extreme factors by $b^2 (u_L/u_H) u_H^2E(u_H)$ and each other factor by $b$ gives $P\le 2b^n e^{-bD/2}$. Choose $B_1$ so this bound is below $W$ for every $b>B_1$, using the decreasing branch beyond $2n/D$.

When $P\ge W$ and $b>B_1$, we therefore have $z_H>bD/2$ and $u_H\ge kD/2$. Consequently,
$$
P\le k^n e^{-k^2D^2/8}.
$$
Choose $K>W^{1/n}$ on the decreasing branch of this bound with $K^n e^{-K^2D^2/8}<W$. Then $k\in[W^{1/n},K]$. Also
$$
q(z_L)\ge \delta:=W/K^n,\qquad
z_L\ge \log\delta,\qquad
u_H\le \sqrt{-2\log\delta}.
$$
The first inequality uses $P\le k^n q(z_L)$, and the last uses $P\le k^n E(u_H)$. These estimates bound the supremum on every nonconstant positive sample and control the joint escape $b\to\infty$.

**Proved.** Along $b\to\infty$ with $P\ge W$, pass to a subsequence with $k\to k_*>0$. The bounds force $\mu$ into a bounded interval, and every limit $s_y$ of $\mu$ satisfies $s_y\le y_L$. For the lower bound on $\mu$, use $u_H\ge k(y_H-\mu)$. If $s_y<y_L$, then $r_i\to k_*$ and $u_i\to k_*(y_i-s_y)$ for every observation. If $s_y=y_L$, these limits hold above the minimum and $u_i\to 0$ at the minimum. At the minimum $r_i/k_*$ tends along a further subsequence to some $\rho\in(0,1]$. The limiting likelihood in $y$ is the threshold expression multiplied by $\rho^m$. Any $0<\rho<1$ is reachable with the endpoint free by holding $z_L=\log(\rho/(1-\rho))$; the envelope $\rho=1$ uses $z_L=\sqrt b$. When $a=x_{min}$ is held, $z_L=0$ throughout and only $\rho=1/2$ is possible. When $a>x_{min}$ is held, the bound $z_L\ge \log\delta$ precludes escape altogether. Holding $b$ or $c$ excludes this face by the bounds on $b$ and $k$.

**Proved.** For bounded $b$, put $h=1/c$ and let $t_i=e^{v+b(y_i-y_L)}$, where $v=\log c+b(y_L-\mu)$. Then
$$
u_i=\frac{\log(1+h t_i)}h,\qquad r_i=\frac{bt_i}{1+h t_i}.
$$
Suppose $P\ge W$ and $b\le B$. Here $h=b/k\le B/W^{1/n}$. Also $u_L\ge W/B^n$, using $P\le b^n u_L$, and $u_H\le U$ whenever $B^n u e^{-u^2/2}<W$ for all $u>U$. Since $u_i\le t_i\le u_i e^{hu_i}$, $v=\log t_L$ lies in a compact interval. If $h$ stays bounded away from zero, the public parameters stay in a compact subset of their domain. If $h\to 0$, the chart extends with $r_i=bt_i$ and $u_i=t_i$, giving the power-transformed Gompertz face. Holding $a$ excludes it because $v=\log c+b(y_L-\mu)$ would diverge as $h\downarrow 0$. Holding $c$ excludes it directly. With $a,c$ free the face is reachable at fixed $b,v$ by this chart. The argument also applies with $b$ held and $D=0$, where the bounds in the bounded-$b$ chart require no positive span.

This proves exhaustiveness for competitive sequences on fixed positive samples. The one-coordinate point-mass and non-tight probability limits in the [prior analysis](2026-11-11-pgw-theory-report.md#proved-boundary-paths) are compatible with these bounds.

## Threshold decisions and the tail cutoff

**Proved.** For $s_y\le y_L$, put $d_i=y_i-s_y$. The unpenalized threshold log likelihood in $y$ is
$$
T(s_y,k)=n\log k+\sum_i\{kd_i+1-e^{kd_i}\}.
$$
If any $d_i>0$, its score $n/k-\sum_i d_i(e^{kd_i}-1)$ decreases strictly from $+\infty$ to $-\infty$. Its unique root gives the face value. With free scale, the largest threshold value is at $s_y=y_L$ because $\partial_{s_y}T=k\sum_i(e^{kd_i}-1)>0$ for a nonconstant sample. Write $T=\max_k T(y_L,k)$ in that case.

With scale held at $\mu\le y_L$, any finite fit has $u_i> k d_i$, $r_i<k$, and $r_i=k/2$ exactly at ties with $\mu$. Since $E$ is strictly decreasing on the positive half-line, its likelihood is strictly less than the threshold likelihood at the same $k$, with the factor $2^{-m}$ when $\mu=y_L$. The limit $b\to\infty$, $c=k/b$ reaches that value. This gives a direct nonattainment decision, including at exact endpoint equality. The fixed-scale endpoint distinction matters for applying the threshold optimization in the [earlier two-point discussion](2026-11-11-pgw-theory-report.md#constant-data-and-all-hold-subsets).

**Proved.** With scale free, the entire large-$b$ tail lies strictly below the best threshold value. Choose any positive $W\le e^T$ and the preceding bounds. A point with $P<W$ already loses. At a retained point with $z_L\ge 0$, $u_i\ge k(y_i-y_L)$ and $r_i<k$, so it loses strictly. If $z_L<0$, at the minima $q(z_L)\le 1/2$. Also $u_i\ge \max(0,k(y_i-y_L)-\tau)$ with $\tau=K|\log\delta|/b$. Using $-(\log E)'=e^u-1$ on $[0,KD]$ gives
$$
\log P\le T-m\log 2+n\tau(e^{KD}-1).
$$
Choose $B>B_1$ so the last term is less than $m\log 2$ for $b\ge B$. These inequalities exclude every finite point beyond $B$, although the supremum of that tail equals $T$. For a scale held above the minimum, use any finite witness $W$, and choose $B>B_1$ so $b(\mu-y_L)>|\log\delta|$ for $b\ge B$. Then no such point is on the witness superlevel.

**Proved.** An all-free two-level sample with $m$ copies of the lower value and $j\le m$ copies of the upper value selects the threshold. For finite PGW, as a function of $y$, $r=u'$ is increasing and $\log r$ is concave. For the Gompertz face $\log r$ is affine with positive slope. Set $k'=(u_H-u_L)/D$. Integrating $r$ and applying Jensen to its logarithm gives $k'\ge \sqrt{r_Lr_H}$, and monotonicity gives $k'\ge r_L$. Thus $r_L^m r_H^j\le (k')^{m+j}$. Also $E(u_L)^m E(u_H)^j<E(k'D)^j$ since $u_L>0$. This strictly excludes every finite $k'$-matched PGW point and every Gompertz point. The threshold optimizer is reachable and unique in $(s_y,k)$. In particular $(1,2)$ has no finite all-free MLE even though it has a unique finite fit at every held positive $c$.

## Gompertz decisions and the finite critical list

**Proved.** Holding $b$, suppose $p_i=(x_i/x_H)^b\ge 1/2$ everywhere. Match a finite model's $U=u_H>0$ by the Gompertz model with $u_i^G=p_i U$. In the finite model
$$
u_i=\frac{\log(1+p_i(e^{hU}-1))}{h}\ge p_i U,\qquad
\frac{r_i}{b}=\frac{p_i(e^{hU}-1)}{h(1+p_i(e^{hU}-1))}.
$$
The first inequality is convexity of the exponential. The derivative of the last expression with respect to $U$ is $p_i e^{hU}/(1-p_i+p_i e^{hU})^2$. It is at most $p_i$, and is strictly smaller for $U>0$, since the derivative of $z/(1-p_i+p_i z)^2$ is nonpositive for $z\ge 1$ and $p_i\ge 1/2$. Hence $r_i/b<p_i U$. Every finite point loses strictly to its matched Gompertz point, including at the sample ratio equality. This includes constant data, for which the maximizing face scale is set by $U(e^U-1)=1$. For $b=1$, $(1,2)$ is an exact equality-case sample for this rule.

**Proved.** At $h=0$ the Gompertz log-density in $y$ is $\log b+w+e^w+1-e^{e^w}$, with $w=v+b(y-y_L)$. The derivative of the base term is $1-u(e^u-1)$, $u=e^w$, and decreases strictly from 1 to $-\infty$. The base term has a linear left log tail and faster than linear right decay. With $b$ held there is one finite optimizing $v$. With $b$ free and nonconstant data the concavity and coercivity argument from the prior fixed-$c$ result applies to this base. The Hessian is negative definite in $(b,v)$, because the $\log b$ term supplies strictly negative curvature in $b$ and at least one affine base term supplies strictly negative curvature in $v$.

Let $F(h)$ be the optimized log likelihood at fixed $h$ when $a$ is free, with $b$ held or optimized as applicable. At every finite positive $h$, uniqueness and nonsingularity of its nuisance root follow from the prior fixed-$c$ proof. The bounded chart proves convergence of those roots to the Gompertz optimizer as $h\downarrow 0$. Indeed, any fixed witness at the Gompertz maximum extends continuously in $h$ and gives a positive superlevel. For all-free data $b\to\infty$ would force bounded $k$, whereas $k=b/h\to\infty$. Thus all optimizers eventually lie in a compact $(b,v)$ chart, whose only possible accumulation point maximizes the Gompertz likelihood. The equivalent held-$b$ argument uses the bounded chart directly. The analytic implicit function theorem now extends $F$ analytically across $h=0$ locally, and $F(0)=G$.

In particular, at the fitted face $u_i$,
$$
F'(0)=\sum_i\left[-u_i+\frac{u_i^2}{2}(e^{u_i}-1)\right].
$$
This follows by holding the nuisance coordinates fixed at their stationary values and differentiating $\log r_i+\log E(u_i)$. Positive derivative proves that the face loses to finite points. Negative derivative strictly excludes a right collar of the face from the maximizer set. If the derivative vanishes, the first nonzero Taylor coefficient gives the same signed rule. Such a coefficient exists. For held $b$, $F(h)\to-\infty$ as $h\to\infty$ by $P\le (b/h)^n$. For all-free nonconstant data, $F(h)\to T$ as $h\to\infty$ and $F(h)<T$ for all sufficiently large $h$. The lower limit uses the threshold path with $b=kh$ at each $h$ and $z_L=\sqrt b$. The upper and strict comparison use the threshold tail bound, since $P\ge W$ forces $b\ge hW^{1/n}$. Thus $F$ cannot be constant on $(0,\infty)$, and analyticity forbids all its Taylor coefficients above order zero from vanishing.

**Proved example.** Hold $b=1$ and take $(1^{[31]},1024)$. At the Gompertz scale optimum, write $U=u_H$ and $A(u)=u(e^u-1)$. The score equation is $A(U)+31A(U/1024)=32$, which puts $U$ in $(5/2,3)$. For instance, $12<e^{5/2}<13, e^3>20$, and $e^s-1<2s$ for $0<s<3/1024$, which suffice for these signs. Then $F'(0)/U\ge A(U)/2-1-31/1024>0$. The finite MLE exists because the sole face loses. This gives both outcomes on the held-$b$ mask using exact samples.

**Proved per-input criterion.**

1. With $a>x_{min}$ held and nonconstant data, use a positive finite witness to restrict $b$ to a compact interval strictly inside $(0,\infty)$. Profile $c$ by its unique strictly concave root. The profile is analytic and nonconstant because it goes to $-\infty$ at $b\downarrow 0$. It has finitely many stationary points in the retained interval. The ones with largest value are the finite MLEs.
2. With scale free and exponent free, take a right collar of $h=0$ where the first nonzero coefficient's sign has been verified uniformly for $F'$. A point strictly inside this collar cannot be a global finite maximizer. For held $b$, the bound $P\le (b/h)^n$ excludes large $h$ below any positive face witness. With $b$ free the threshold cutoff and $b\ge hW^{1/n}$ exclude sufficiently large $h$. On the remaining compact interval of positive $h$, the nonconstant analytic profile has finitely many stationary points. Include a root on a closed interval boundary if such a boundary was used.
3. Compare the largest retained stationary value, or $-\infty$ for an empty list, with the largest available face value. A finite MLE exists exactly when the stationary value is at least the face value. Return all finite roots tied at that maximum if equality and completeness can be proved. If the stationary value is lower, the supremum is excluded and the winning face or faces determine the nonattainment diagnosis.

The criterion specifies the equality cases without inferring equality from numerical overlap. Strict signs can be certified using nuisance inclusion and derivative and value enclosures. The finiteness proof supplies neither a general symbolic critical-value equality predicate nor a uniform subdivision budget.

**Proved by interval comparison.** For the all-free sample $(1,2,4,4,4,8)$, take the feasible $(a,b,c)=(13/3,11/6,1)$. The [interval certificate](2026-11-25-pgw-math-data/witness.py) works in log observations normalized to unit span and compares both optimized faces with this point. If a Gompertz point has at least the witness likelihood $W$ in those units, its normalized shape $B_*$ must satisfy $P\le B_*^n e^{-B_*}$, by using $u_L/u_H=e^{-B_*}$ at the two extremes. This bounds $B_*\le 120$, and $120^n Ue^{-U^2/2}<W$ at $U=60$ bounds $u_H\le 60$. Hence $\log W-n\log 120\le v\le \log 60$. The analytic tangent plane at an exact decimal center bounds the concave Gompertz likelihood across this entire box in log units. The threshold tangent works on $k\le 120$, from $P\le k^n e^{-k^2/2}$ in those units. Candidate centers come from numerical solves, and the tangent bounds use their actual gradients, with no assumption that the centers are stationary. The 75-digit interval comparisons enclose the Gompertz upper bound minus witness in $(-0.12704,-0.12703)$ and the threshold upper bound minus witness in $(-0.02377,-0.02376)$. Thus no face can achieve the witness and a finite MLE exists.

## Public coefficients and reproducibility

**Numerical.** The [coefficient replay](2026-11-25-pgw-math-data/check.py) solves the unique held-exponent nuisance problem at 85 digits and rounds its public scale and shape once to binary64. These are reference outputs for a prospective fitter. The current library returns no PGW ML coefficients.

| Exact sample and held exponent | Relative error in rounded scale | Relative error in rounded shape | Loss at the rounded point | Relative shape error after refitting at rounded scale |
|---|---:|---:|---:|---:|
| $(1,2,4,8), c=1$ | $6.19\times10^{-17}$ | $3.25\times10^{-17}$ | $1.48\times10^{-32}$ | $2.18\times10^{-17}$ |
| $(1,2,3,8), c=2$ | $7.98\times10^{-17}$ | $6.95\times10^{-17}$ | $5.96\times10^{-32}$ | $2.98\times10^{-17}$ |
| $(1,1,1+2^{-52}), c=1$ | $9.26\times10^{-17}$ | $8.54\times10^{-17}$ | $0.889172296895$ | $0.150511767678$ |

The last rounded coefficients are `0x1.0000000000001p+0` and `0x1.0ee4fbb9adcbep+53`. Both meet the relative coefficient contract at default tolerance. The likelihood loss falls under the [steep-maximizer convention](../../design/accuracy-contract.md#log-likelihood), which uses the likelihood at the published coefficients in this case. The rounded scale equals the larger observation. The mechanism can also be read from the Weibull equations. Put $r=b\log(1+2^{-52})$. Before rounding, the joint optimum solves $1/r+1/3-e^r/(2+e^r)=0$, with $a^b=(2+e^r)/3$. Refitting only the shape at $a=1+2^{-52}$ solves $3/r-2+2e^{-r}=0$, a different equation at order one in the normalized coordinate. Round once after solving jointly. Near tied observations, compute log displacements through relative differences and use a centered and scaled log-data chart. Propagate the final enclosure through $a=\exp(y_L+(\log c-v)/b)$, and propagate $h,b$ to the public coefficients as well. A small scalar profile bracket alone gives no bound on the relative errors in those coefficients.

**Numerical.** The [limit replay](2026-11-25-pgw-math-data/limits.py) uses 75 and 105 digits. On $(1,1,2,4)$, at $b=32,128,512$, the losses against the held-endpoint threshold approach zero from above as $4.10\times10^{-4}, 2.55\times10^{-5}, 1.59\times10^{-6}$. With the endpoint free and $z_L=\sqrt b$, the corresponding losses against the full envelope are $0.7003,0.3188,0.1527$. The held-$b=1$ Gompertz derivatives agree at both precisions, approximately $-0.5747571$ for $(1,2)$ and $38.7923040$ for $(1^{[31]},1024)$. The exploratory 85-digit all-free grid also evaluates nine samples at $h=0$ and attempts the finite-profile points $h=0.1,0.5,1,2,5$, using unique nuisance roots where the numerical solve succeeds. The grid makes no root-count or global dominance claim.

All three Python checks exited zero locally on the workstation through the one-slot lock. No builds were run. From the worktree root:

```sh
d=dev/research/evaluations/2026-11-25-pgw-math-data
export TMPDIR=$PWD/build/tmp PYTHONDONTWRITEBYTECODE=1
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run python3 -u "$d/check.py"
/tmp/umvl-cpu/run python3 -u "$d/witness.py"
/tmp/umvl-cpu/run python3 -u "$d/limits.py"
```

## What this changes for the fitter

- Use exact sample comparisons for constant data, equality with a held scale, the side of the held threshold, and minimum multiplicities. Implement the direct unbounded and strictly winning threshold routes from the table. A scale held exactly at the minimum uses $T-m\log 2$ on nonconstant data. A scale held above it admits a compact finite search.
- Use the strict conditional roots for the one-free cases and the held-exponent slice. For held shape on narrow samples, use the matched Gompertz dominance predicate, including the equality case. For the all-free two-level mask, minimum multiplicity at least half the sample gives certified threshold nonattainment.
- For the other masks use the bounded profile regions, the optimized face bounds and the signed collar. Compare candidate and face enclosures with the [ties policy](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter). A finite/face equality preserves the finite estimate, with uniqueness decided among finite points. A complete, proved finite tie returns the complete typed set. Proven finite attainment with established coefficient accuracy and unresolved cardinality allows `certified = TRUE, unique = NA`. An unresolved comparison or exhausted certificate allowance preserves a successful accuracy-checked search as `certified = FALSE, unique = NA`. A separated witness that refutes the candidate, or a genuine search failure, requires replacement by a valid checked result or an appropriate stop.
- Use specific PGW face diagnoses on proved nonattainment. Neither threshold Weibull conditioned at its own scale nor the power-transformed Gompertz face has an exact unrestricted package delegate. At $b=1$, the Gompertz face constrains its two package coefficients to be equal. At $c=1$ held, the exact finite-model delegate is Weibull.
- Use finite, strictly positive fitting data as in the prior theory. The d/p/q/r endpoint at zero is defined for evaluation. **Proved under that endpoint density convention**, if zero occurs and $b$ is free, a finite $b<1$ already gives pointwise infinite likelihood. A held $b<1$ gives infinite likelihood everywhere and $b>1$ gives zero likelihood everywhere. With $b=1$, zero observations contribute $c/a$ each. If both $a,c$ are free, $a=e^{-1/c}$ as $c\downarrow 0$ makes the likelihood unbounded, since each positive observation contributes order $c$, while each zero contributes $ce^{1/c}$. With exactly one of $a,c$ free and a positive observation present, the remaining score is strictly monotone in $c$ or log scale and has a finite unique root. The zero terms add $m\log c-m\log a$, and a positive observation supplies the decaying right tail in each free coordinate. On all-zero data, either free coordinate makes $c/a$ unbounded. A fully held model can be evaluated directly. These endpoint effects motivate a positive fitting domain independently of the support convention for the evaluator.

## Open

- For held scale above the minimum, held shape outside the narrow predicate, and the all-free mask, settle general finite root cardinality or supply exact separated competing maxima. The finite critical list and the interval witness establish attainment where indicated. They leave root count and winner separation to a per-input cover.
- Resolve exact finite/finite and finite/face equality beyond the strict-comparison slices. There is neither an exact PGW tie construction here nor a rational-data exclusion. At a degenerate stationary root the missing input is a multiplicity-aware enclosure or exact predicate. At a vanishing first Gompertz derivative the Taylor proof ensures a first nonzero coefficient exists, but exact zero tests for earlier coefficients and practical uniform remainder bounds are still needed.
- The tail constants can be conservative when $D$ is small or multiplicities are extreme. Measure bounded-work cover costs and develop stable value and coefficient enclosures on original binary64 inputs. No production fit exists yet to replay. A future implementation should test the three rounded vectors above and the original-input likelihood of the steep case.
