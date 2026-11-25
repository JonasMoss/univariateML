# Shifted Gompertz existence, uniqueness and faces

TASK-351 · 2026-11-25 · Source base `f22e71d9`

**Proved.** Joint uniqueness fails. There is an exact real sample consisting of nine ones and one outlier for which the exponential member and one positive-shape point are the complete maximizer set. The two adjacent binary64 outliers bracketing that crossing have different, unique winners. The simpler integer sample $(1^{[9]},8)$ has a strict exponential local maximum and a strictly better positive-shape fit. All of these statements use the ordinary shifted Gompertz likelihood with both coefficients free.

## Existence for every hold mask

Write $S=\sum x_i$, $m=\min x_i$, $G=S-nm$, $z=\#\{x_i=0\}$,
$v_i=e^{-bx_i}$, $a_i=1-v_i$, and $V=\sum v_i$. Inputs here are nonempty finite nonnegative samples with finite holds in the declared domains. Other inputs fail validation. The log likelihood is

$$
\ell(b,\eta)=n\log b-bS-\eta V+\sum_i\log(1+\eta a_i).
$$

**Proved** by the existing held-score and envelope arguments, with the equality cases shown below. [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sgomp/README.md) · [Original analysis](2026-10-29-sgomp-report.md) · [Compact certificate](2026-11-07-scalar-trio-report.md#shifted-gompertz-scalar-concavity-and-compact-joint-bounds)

| Held | Exact condition | Mathematical outcome |
|---|---|---|
| Neither | $G>0$ | Finite attained estimate. Winner and multiplicity follow the comparison below. |
| Neither | $G=0$ | `nonexistence(unbounded)`, including a sample of size one and all zeros. |
| $b$ only | $n-2V\le0$ at the held rate | Unique estimate $\eta=0$. Equality selects the same endpoint. All zeros have $n-2V=-n$. |
| $b$ only | $n-2V>0$ | Unique strictly positive shape estimate. |
| $\eta$ only | $S>0$ | Unique rate estimate. At $\eta=0$ it is $b_0=n/S$, and at $\eta>0$ it is a strictly concave score root above $b_0$. Positive constant samples are included. |
| $\eta$ only | $S=0$ | `nonexistence(unbounded)` for every admissible held shape. |
| Both | Every valid sample | The single feasible pair, with likelihood evaluated there. |

The shape score is $s=\sum a_i/(1+\eta a_i)-V$. It decreases strictly if any $x_i>0$ and tends to $-V<0$. For all zeros it is the constant $-n$. With shape held, $\ell_{bb}<0$ by the dossier formula. The upper bound $n\log b-bS+n\log(1+\eta)$ excludes both rate ends when $S>0$. At all zeros the exact log likelihood is $n(\log b-\eta)$. At a positive constant $c$ and both parameters free, $\eta=e^{bc}$ gives the exact density $f(c)=b/e$.

For $G>0$ the dossier bounds give $\sup_\eta\ell\le n\log b+n$ for $b\max x_i\le1$, and $\sup_\eta\ell\le U(b)=n\log b+n\log n-bG$ at large $b$. On any compact positive rate interval, $V$ is bounded below by a positive number and $\sum\log(1+\eta a_i)\le n\log(1+\eta)$. Thus every upper likelihood level containing a feasible witness is compact relative to $b>0,\eta\ge0$. This also rules out a winning simultaneous rate and shape escape for nonconstant samples.

## Attained exponential member and excluded ends

**Proved.** If $2z\ge n$ and $S>0$, then $V>n/2$ at every finite rate. The all-free fit is uniquely $(b_0,0)$. In particular $(0,1)$ gives $(2,0)$ and realizes the zero-count equality $2z=n$. If $2z<n$, the equation $V(\beta)=n/2$ has exactly one solution since $V$ strictly decreases from $n$ to $z$. The conditional shape $\hat\eta(b)$ is zero on $b\le\beta$ and positive on $b>\beta$. Let $P(b)=\ell(b,\hat\eta(b))$. It is continuously differentiable across $\beta$, with $P'(b)=\ell_b(b,\hat\eta(b))$.

For a nonconstant sample, use this exact finite-candidate comparison.

1. Include $(b_0,0)$ precisely when $b_0\le\beta$. For $b_0>\beta$, the shape score at the only possible exponential winner is positive.
2. Choose $B>\max(\beta,n/G)$ with $U(B)<\ell(b_0,0)$. Include every root of $P'$ in $(\beta,B)$ and its unique conditional shape. The right profile is analytic on that interval and extends analytically through $\beta$ by the implicit function theorem since $-\ell_{\eta\eta}>0$ there. It cannot be constant on the positive-shape branch because the large-rate envelope tends to $-\infty$. Its derivative therefore has finitely many zeros on the compact interval $[\beta,B]$. The left piece is the exponential log likelihood. A seam maximizer requires $b_0=\beta$.
3. Compare the values at these candidates. Strict domination by the exponential gives the unique attained exponential fit. Equality at the largest value keeps every attaining point, including the exponential if present. This is a mathematical comparison rule. A numerical implementation must enclose or otherwise decide the roots and their heights.

A sufficient sample rule avoids the interior comparison if $b_0\le\beta$ and some $L$ satisfies $n/G\le L\le\beta$ and $U(L)<\ell(b_0,0)$. The envelope decreases after $L$. Those inequalities give a separate singleton proof on the positive sample $(1^{[9]},128)$ using $L=1/2$. The exact interval script checks the inequalities. Also, every positive-shape stationary rate is strictly above $b_0$ because $\ell_b(b,\eta)>n/b-S$ for $\eta>0$ and $S>0$.

**Proved local equality rule.** If $b_0<\beta$ the exponential candidate is a strict local maximum. At equality $b_0=\beta$ its left curvature is $-n/b_0^2$. The right curvature is
$$
K=-n/b_0^2+\frac{(2\sum x_i v_i)^2}{\sum a_i^2},
$$
evaluated at $b_0$. A negative $K$ gives a strict local maximum; a positive $K$ excludes it even locally. If $K=0$, the first nonzero higher derivative of the analytic right profile decides the one-sided comparison by its sign. Such an order exists by nonconstancy. The global comparison still applies when the local test passes.

Both strict signs at exact tangency occur over real data. For $(1^{[9]},M)$ let $M$ be the unique zero of $n-2V(b_0)$ in $(7.95,7.97)$. Here and on the tie path below, $V(b_0)$ has derivative $90(v-w)/(9+M)^2>0$ for $M>1$, with $v=e^{-b_0}$ and $w=e^{-b_0 M}$. The exact checker brackets the sign change and proves $K>0$ throughout the small box with $b=b_0$. For $(0,1,M)$ let $M$ be the root in $(1,2)$ of $1+e^{-3/(1+M)}+e^{-3M/(1+M)}=3/2$. The left side strictly increases there, with opposite endpoint signs checked by the same script. At equality $\sum a_i^2\ge9/8$ because the two positive-observation terms sum to $3/2$. With $t_i=b_0 x_i$, the bound $t_i e^{-t_i}\le1/e$ gives $b_0^2 K\le -3+128/(9e^2)<0$.

On rational data with a positive observation, $n-2V(b_0)$ is never exactly zero. The same holds for the held-rate sign when $b$ is rational. Indeed write $x_i=d k_i$ with rational $d>0$ and nonnegative integer $k_i$. The equality would make $t=e^{-bd}$ a root of the nonzero integer polynomial $n-2\sum t^{k_i}$ at a positive rational rate, contradicting Hermite–Lindemann. This excludes tangency on valid binary64 data for both comparisons. The sign can still require adaptive precision. [Transcendence tools](2026-11-25-ties-math-report.md#what-the-transcendence-results-can-decide)

**Proved excluded ends.** The sampling representation in the original report gives $X=\max(E/b,(\log\eta+Z)/b)$ with $E$ unit exponential, $Z$ standard Gumbel and $\log 0=-\infty$. If $b\to0$, mass escapes to infinity. If $b$ approaches a finite positive rate, $\eta\to\infty$ also sends mass to infinity, and $\eta\to0$ gives the attained exponential member. If $b\to\infty$ and $\log\eta/b\to c$ in the extended reals, the limit is the atom at $\max(0,c)$ for $c<\infty$, or mass escaping to infinity for $c=+\infty$. These cases classify subsequential distribution limits at excluded parameter ends by taking subsequences of $b$ and $\log\eta/b$. The atom is a singular collapse, and the Gumbel chart would require recentering or rescaling the observations. The level-set proof excludes all parameter escapes on the finite-fit masks. Collapse on the unbounded masks gives `nonexistence(unbounded)`. [Sampling representation](2026-10-29-sgomp-report.md#mathematics-proved)

## Exact separated maxima and the crossing

**Proved by directed rational enclosures.** For $(1^{[9]},8)$ the exponential candidate has inward score between $-0.01360$ and $-0.01359$, and the feasible pair $(1,1)$ beats it by more than $0.0969$ in total log likelihood. The compactness proof gives a separated global interior maximum. Thus neither the local score nor a nearby improvement check decides the exponential face.

For the exact tie, use $(1^{[9]},M)$ on $I=[8.60,8.62]$. These endpoints are exact decimals. On $I$, $b_0<.58<\beta<.63$, and $U(20)<\ell(b_0,0)$ with $U$ decreasing beyond 20. The profile above $.58$ has the following signs. The right curvature rows lie strictly above the seam.

| Rate interval | Certified inequality uniformly on $I$ | Terminal cells |
|---|---|---:|
| $[.58,.63]$ | $P'<0$ | 5 |
| $[.63,.76]$ | $P''>0$ | 11 |
| $[.76,.90]$ | $P'>0$ | 32 |
| $[.90,1.05]$ | $P''<0$ | 10 |
| $[1.05,20]$ | $P'<0$ | 87 |

The signs give exactly one valley root and one interior maximum root $b_+(M)$ in the two curvature rows. Define
$$
D(M)=P_M(b_+(M))-\ell_M(b_0,0).
$$
The exact sample is $(1^{[9]},M_*)$ where $M_*$ is the unique zero of $D$ on $I$. The positive-shape branch is differentiable there by strict curvatures and the implicit function theorem. The tests give $D(8.60)>0.00522$ and $D(8.62)<-0.00292$. The derivative proof below gives $D'<0$. The complete maximizer set at equality consists of $(b_0,0)$ and $(b_+,\hat\eta(b_+))$. Both are attained and separated by a valley. This is a real crossing in the terminology of the ties report. [Tie classification](2026-11-25-ties-math-report.md#classification-of-the-constructions)

**Proof of the one-crossing decision for every $M\ge8$.** For this path $b_0<\beta$ by the sign at 8 and the strict increase of $V(b_0)$ with $M$. For fixed $b\ge\beta(M)$, let $F_M(b)=P_M(b)-\ell_M(b_0,0)$. The derivative in $M$ at fixed $b$ is
$$
\partial_M F_M(b)=b_0-b+b\hat\eta\,e^{-bM}(1+(1+\hat\eta a_M)^{-1}),
\qquad a_M=1-e^{-bM}.
$$
If $b\ge.7$, use $\hat\eta V\le10$, $V\ge9e^{-b}$, and $e^{-b(M-1)}\le e^{-4.9}<1/100$. Then $\partial_M F\le10/17-(44/45)b<0$. Otherwise, $\beta>.5$ since $9e^{-.5}>5$. On $[\beta,.7]$ we have $49/100<v\le5/9$, $w<1/50$, and $Mw\le8e^{-4}<4/25$. Thus $s_0=10-2V\le1.18$, $D_0=\sum a_i^2\ge9(4/9)^2+(49/50)^2$ and $D_0-s_0>1.5$. Also $\partial_b s_0<11$, so $s_0\le11(b-\beta)$. Subtracting the positive-shape score from its value at zero gives $s_0=\hat\eta\sum a_i^2/(1+\hat\eta a_i)\ge\hat\eta D_0/(1+\hat\eta)$. Consequently $\hat\eta\le8(b-\beta)$ and the last term of $\partial_M F$ is at most $.224(b-\beta)$. This proves strict negativity since $b_0<\beta$. In particular $D'=\partial_M F_M(b_+(M))<0$ on $I$.

As $M$ increases, $\beta(M)$ decreases. A fixed rate that enters the positive-shape branch starts at a strictly negative $F$ because at entry $P$ is exponential and $b=\beta>b_0$. Each rate already on that branch strictly loses relative to the exponential as $M$ increases. The maximum of $F$ over $b\ge\beta(M)$ is attained by the compact rate bound. If it is nonpositive at any $M_1\ge8$, it is strictly negative at every $M_2>M_1$. It follows that for $8\le M<M_*$ every global winner has positive shape. For $M>M_*$ the exponential is the unique winner. On $I$ there is exactly one candidate of each type.

**Exact certificate construction.** With $A=1-e^{-b}$, $C=1-e^{-bM}$ and $V=9e^{-b}+e^{-bM}$, the positive conditional shape is the positive root of
$$
VAC\,\eta^2+[V(A+C)-10AC]\eta+V-9A-C=0.
$$
At a nonpositive score it is zero. The score is strictly increasing with $b$ and with $M$ at fixed shape. For $M$ the derivative of its outlier term is $b e^{-bM}(1+(1+\eta C)^{-2})$. Thus two opposite rectangle corners bound $\hat\eta$. The script encloses $P'=\ell_b$ and $P''=\ell_{bb}+\ell_{b\eta}^2/(-\ell_{\eta\eta})$ by substituting the shape and coordinate boxes into the derivative formulas. It bisects a rectangle until its sign is strict, keeping both children. The displayed cell counts account for the full rectangles. Signed rate bisection isolates $b_+$ for the four height comparisons.

The interval endpoints are integer multiples of $2^{-160}$. For arithmetic operations the lower endpoint uses integer floor, the upper uses integer ceiling, and square roots use integer square-root bounds. For $e^{-u}$ the program halves the argument to at most one, uses an even and subsequent odd partial sum of the alternating series as upper and lower bounds, and squares back. For $u\ge128$ the bound $[0,2^{-160}]$ suffices since $\log 2<4/5$. For $\log y$, scale by a power of two to $1\le y\le2$ and sum $2\sum_{j=0}^{47}w^{2j+1}/(2j+1)$ with $w=(y-1)/(y+1)$. The tail is between zero and $2w^{97}/(97(1-w^2))$ by the geometric series. All series operations round outward. The signs are asserted on integer endpoints before printing decimal approximations. [Certificate](2026-11-25-sgomp-math-data/certificate.py)

**Proved dyadic comparisons.** In both rows the other nine observations are exactly one. The gaps are total log likelihood differences, with the displayed decimals rounded. The full sign cover applies to both rows.

| Exact binary64 outlier $M$ | $D(M)$ approximately | Unique winner |
|---|---:|---|
| `0x1.139be516e77b3p+3` | $+1.90490\times10^{-16}$ | Positive shape |
| `0x1.139be516e77b4p+3` | $-5.31412\times10^{-16}$ | Exponential |

As these values are adjacent, comparing any binary64 $M\ge8$ on this exact nine-ones path to these two endpoints decides which type wins, and there is no binary64 crossing on the path. It does not count possible positive-shape winners outside $I$.

**Numerical coordinates.** At 85 decimal digits $M_*\simeq8.612780136809169676$, with positive-shape rate $0.9746048221125007613$, shape $1.412298799516437993$, and a valley at rate $0.6974603274196443821$. The mpmath replay solves two stationary scores and the height equality, then re-solves the branches at the two exact dyadic inputs. The equality and root count use the exact certificate above. [High-precision profile replay](2026-11-25-sgomp-math-data/profile.py)

## Rational samples and exponential ties

**Proved conditionally on Schanuel's conjecture.** On rational data, a positive-shape stationary point cannot tie the optimized exponential. Write $x_i=d k_i$ as above and $t=e^{-bd}$. At a positive-shape stationary point, $v_i=t^{k_i}$, $\eta$ is algebraic over $\mathbb Q(t)$ by the cleared shape score, and
$$
b=\frac{n}{S-\eta J},\qquad
J=\sum_i x_i v_i \bigl(1+(1+\eta a_i)^{-1}\bigr).
$$
If $t$ were algebraic, both $\eta$ and $b$ would be algebraic, contradicting Hermite–Lindemann for $t$. The shape polynomial is nonzero at fixed $t$ because its value at zero, before any scalar factor, is $n-2V>0$. In fact $b$ is also transcendental, unconditionally. The function $S-\eta J$ is algebraic in $t$ on the positive conditional branch and is less than $S$ there, approaching $S$ at the seam. A nonconstant algebraic function over $\mathbb Q$ cannot take an algebraic value at a transcendental argument on this analytic branch. To see this, subtract the putative value and take an irreducible polynomial over $\overline{\mathbb Q}(t)$, then clear denominators. The constant coefficient is a nonzero algebraic polynomial and would have to vanish at $t$. This argument also proves the nonvanishing criterion used next.

At equality to the face, $\exp(bS+\eta V-n)=(b/b_0)^n\prod_i(1+\eta a_i)$ is algebraic over $\mathbb Q(t)$. The exponents $-bd$ and $bS+\eta V-n$ are rationally independent. Otherwise $\eta V-n=qbd$ at the putative tie for some rational $q$, since $S/d$ is rational. By the nonvanishing criterion the analytic algebraic function $(\eta V-n)(S-\eta J)$ would then be identically $qnd$ on the conditional positive branch. Its seam limit is $-nS$. For $t\downarrow0$ and $z=0$, the shape tends to infinity by its score equation, so the scaled score gives $\eta V=\sum_i\eta a_i/(1+\eta a_i)\to n$, while $\eta J\le2(\max_i x_i)\eta V$ stays bounded. The product tends to zero. For $0<z<n/2$, the shape remains bounded by $n/z$ and the score equation gives $\eta\to(n-z)/z-1$, $\eta V\to n-2z$, and $J\to0$. The product tends to $-2zS$. In both cases the limit differs from $-nS$. When $2z\ge n$ there is no positive-shape conditional optimum.

The field of the two exponents and their exponentials has transcendence degree at most one because all four values are algebraic over $\mathbb Q(t)$. Schanuel requires at least two. The argument applies to the exponential comparison and relies on the conjecture for this last step. [Schanuel's conjecture and related comparisons](2026-11-25-ties-math-report.md)

## Public coefficient accuracy

**Numerical, archived public outputs.** The 85-digit checker compares independently refined roots to the two final binary64 coefficients emitted by the binding-neutral adapter in each stored native and double-only record. It verifies the snapshot input hash before solving. The inputs are the exact binary64 versions of the saved observations and holds. For joint interior records it compares against the joint root and separately refits shape at the emitted rate. The latter refit is a diagnostic, with the joint root as the reference for the accuracy contract. For the exponential record it uses $n/S$ and checks the inward score. For the positive held-coordinate records the held value is checked exactly. [Snapshot provenance](2026-11-15-fit-snapshot-data/native.provenance.json) · [Independent public coefficient replay](2026-11-25-sgomp-math-data/accuracy.py)

| Record | Route | Largest normalized coefficient error across the two modes |
|---|---|---:|
| 006222 | Joint interior | $9.96\times10^{-16}$ |
| 006223 | Joint interior, two observations | $4.45\times10^{-15}$ |
| 006239 | Joint exponential | $1.25\times10^{-16}$ |
| 006241 | Held rate | $3.38\times10^{-17}$ |
| 006260 | Held positive shape | $7.48\times10^{-17}$ |
| golden | Joint interior | $6.58\times10^{-16}$ |

Errors use the exact rate as rate denominator and $\max(1,\eta_*)$ for shape. The largest refit shift in normalized shape was $3.25\times10^{-15}$ rounded up. These checks support the default tolerance on the archived samples. They use preserved public outputs and do not establish the stopping rule on new inputs.

**Proved source-level counterexample at a tight tolerance.** For $(1,2)$ with $\eta=0$ held and `tol = 1e-18`, $b_*=2/3$. Even its nearest binary64 representation has relative error $2^{-54}$, exceeding the request. The closed-form route in `sgomp_mle` enters with `Ok` and checks the finiteness of $b$ and $\ell$ without testing `tol`. The same helper restores the rate on a selected joint exponential face. This tight-tolerance sample requires a tolerance failure. Default-tolerance results in the table satisfy the contract. [Current source](../../../univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp) · [Accuracy contract](../../design/accuracy-contract.md)

**Proved representability cases.** On $(0,2^{-1074})$ all-free fit selects the exponential by the half-zero rule, but $b_*=2^{1075}$ exceeds binary64 range. This is a finite mathematical maximum with a public evaluation failure. For near-constant positive samples, the dossier's shape-range test addresses the corresponding obstruction in $\eta$. To see a simple sufficient condition, let $C$ cap $\eta$, $L=1+\log(1+C)$ and $d=\max x_i-m>0$. Each density with $\eta\le C$ is at most $b\min(1,(1+C)e^{-bm})\le L/m$. The first inequality uses $(1+u)e^{-u}\le1$ for $u=\eta v_i$. The final inequality splits at $bm=L$, beyond which $bm e^{-bm}$ decreases. At $b=1/d,\eta=e^{bm}$ each density is at least $b e^{-2}$ because $0\le b(x_i-m)\le1$. Thus $d/m<e^{-2}/L$ proves all winners have shape above $C$. An example is $(1,1+2^{-52})$ with $C$ the largest finite binary64 value. Such range proofs give an evaluation failure, while the all-equal cases give unbounded likelihood.

## Reproduction

The three final Python checks exited zero on the workstation through the one-slot lock. Run them sequentially from the worktree root. The archive supplies already recorded public outputs.

```sh
d=dev/research/evaluations/2026-11-25-sgomp-math-data
/tmp/umvl-cpu/run nice -n 10 python3 -B -u "$d/certificate.py"
/tmp/umvl-cpu/run nice -n 10 python3 -B -u "$d/profile.py"
/tmp/umvl-cpu/run nice -n 10 python3 -B -u "$d/accuracy.py"
```

## What this changes for the fitter

- Dispatch constant samples and zero sums exactly by hold mask. At held rate, use the signed conditional shape decision, refining ambiguous signs where possible. With $2z\ge n$, $S>0$ and both free, the certified singleton is $(n/S,0)$. Apply the same public-coordinate acceptance to all exponential routes, including the fast path.
- Use $b_0$ versus $\beta$ to assess exponential admissibility, followed by the compact global comparison when admissible. A nonpositive inward score plus a finite likelihood-gap tolerance cannot select the exponential. The exact sample $(1^{[9]},8)$ and both dyadic neighbors give regression cases with known winner types.
- On an isolated winning root, verify root count and bound the final public coordinates at the requested tolerance. Count all distinct retained roots before setting `unique = TRUE`. If coefficient accuracy and global optimality are established while cardinality remains undetermined, use `certified = TRUE, unique = NA`. An exact proved tie of attained points requires the complete accurate typed maximizer set with `certified = TRUE, unique = FALSE`. The current `Ok` mapping defaults to unique, while the joint cover in the dossier gives coefficient proximity and the subsequent polish is local.
- Route an unresolved global comparison or default certificate exhaustion to `certified = FALSE, unique = NA` when the candidate search passed its own local accuracy check. Preserve separated candidates and their bounds as diagnostics. The existing coarse region centers do not pass the declared accuracy tolerance merely by surviving the cover. If a strict comparison disproves the returned branch, use a checked surviving estimate or the justified failure. The dyadic gaps show why a fixed likelihood tolerance cannot prove equality. [Outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter) · [Wrapper handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- General interior multiplicity remains undecided away from the proved slices. A sharper profile theorem or an exhaustive root certificate handling degenerate stationary points is needed. Likewise, at real exponential tangency with $K=0$, the analytic derivative rule does not bound the required order over all samples.
- Unconditional rational or binary64 exclusion of an exponential/interior tie outside the displayed path remains open. The arithmetic proof needs a replacement for Schanuel or an exact rational equality construction. It gives no exclusion of an interior/interior tie. Neither branch comparison has a general terminating exact equality predicate here.
- The default-tolerance replay covers six inputs in each archived mode. General public-coordinate acceptance, including restoration near the representability boundary and sensitivity of shape to a rounded rate, needs a bound through each winning root's bracket or a checked joint coordinate neighborhood. The exact tight-tolerance counterexample is independent of that numerical replay.
