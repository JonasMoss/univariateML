# Exponentiated Weibull likelihood: multimodality, ties and faces

TASK-344 · 2026-11-25

**Proved.** The all-free likelihood has at least two separated finite local maxima on the exact dyadic sample with levels $(1,2,4)$ and counts $(510976,513024,1)$. Moving its largest observation gives an exact real sample with separated global maximizers strictly above both excluded faces. With Weibull shape held, every nonconstant positive sample has a unique finite optimum. The same concavity argument reduces the all-free problem to a scalar profile.

We use the prior structural results and extend the face comparisons and held-mask guarantees. [First analysis](2026-11-10-ew-theory-report.md) · [Boundary exhaustion](2026-11-11-ew-theory2-report.md) · [Fitter study](2026-11-12-ew-fit-report.md) · [Face estimates](2026-11-15-expweibull-limits-report.md) · [Outcome and equality rules](2026-11-25-ties-math-report.md)

## Held shape and the scalar profile

**Proved.** Write $y=\log x$, or use any common positive affine rescaling of the log data, with corresponding shape $b$ and log scale $\mu$. Set $u_i=e^{b(y_i-\mu)}$, $Q(u)=1-e^{-u}$, $R(u)=u/(e^u-1)$ and use $E$ for empirical averaging, including multiplicities. All displayed values are per observation on the log-coordinate density scale. Their difference from the actual mean log likelihood is common to all competitors on the same sample.

For a fixed $b>0$ set $r_i=e^{b(y_i-y_{\min})}$, $t=e^{b(y_{\min}-\mu)}$ and $H(t)=E[-\log Q(r_i t)]$. The exponent optimum is $c=1/H(t)$. The remaining profile, score and curvature are
\[
\begin{aligned}
L_b(t)&=\log(bt)+E\log r_i-tE r_i+H-\log H-1
      =\log b-\log H-1+E\log R(r_i t),\\
L'_b(t)&=\{1-E R(r_i t)+E R(r_i t)/H\}/t-E r_i,\\
L''_b(t)&=H''-t^{-2}-(\log H)''<0 .
\end{aligned}
\]
Indeed, $H=E\sum_{j\ge1}e^{-j r_i t}/j$ is a positive Laplace transform, log-convex by Cauchy–Schwarz. Its derivatives converge uniformly on compact positive intervals, and $H''=E[r_i^2/(4\sinh^2(r_i t/2))]<1/t^2$. At zero, $H=-\log t-E\log r_i+O(t)$ and $H'=-1/t+O(1)$, so $L'_b\to+\infty$. At infinity $H'/H\to-1$ and $H'\to0$, giving $L'_b\to1-E r_i<0$ on nonconstant data. Thus a signed score bracket gives the unique root $t_b$. In the physical chart, restore $a=x_{\min}t_b^{-1/b}$ and $c=1/H(t_b)$. They are respectively decreasing and increasing functions of the bracket variable. On constant data the upper score limit is zero and the strictly decreasing score stays positive, consistent with the prior unbounded concentration result.

For the all-free fit put $F(b)=L_b(t_b)$. Strict curvature and the implicit function theorem give real analytic $t_b$ and $F$. For $Q_c(u)=1-u+(c-1)R(u)$,
\[
 F'(b)=1/b+E[(y_i-y_{\min})Q_c(u_i)]
\]
at the nuisance optimum. Every interior stationary point corresponds to a zero of this score. A strict local profile maximum lifts to a strict local likelihood maximum because the nuisance optimum is unique. Holding $b=1$ on $(1,2)$, for example, gives a unique exponentiated-exponential fit.

## Faces and finite attainment

**Proved previously.** For nonconstant data the excluded faces are power and inverse Weibull. Put $D=y_+-Ey>0$ and let $w_+$ denote the share at $y_+=\max y$. Their values on the same scale are
\[
 P=-\log D-1,\qquad I=\max_{k>0}\{\log k-k Ey-\log E e^{-ky}-1\}.
\]
The inverse face has strictly decreasing shape score with derivative $-1/k^2-\operatorname{Var}_k(y)$, weighted by $e^{-ky}$, and optimal log scale $s$ given by $ks=-\log E e^{-ky}$. The power shape is $1/D$ with endpoint $y_+$, including every top tie via the common path approaching from above. The exhaustion theorem and the explicit common paths give $F(0+)=I$ and $F(+\infty)=P$. In particular the inverse trial takes $T=\log c=k/b$ and $\mu=s-\log(T)/b$ for each small $b$, and any subsequence at that end retaining likelihood has an inverse-Weibull limit. The analogous upper bound at large $b$ is the joint power envelope. A finite MLE exists iff a finite point reaches or exceeds $\max(P,I)$. [Joint envelope and attainment](2026-11-11-ew-theory2-report.md#joint-face-likelihoods-and-attainment)

**Proved, explicit strict power bound.** For all nonconstant samples,
\[
 F(b)<P\quad\text{when}\quad b\ge (e-1)/(w_+D).
\]
For $\mu\ge y_+$ the log-coordinate density is $bcR(u)Q(u)^c<bc\exp[bc(y-\mu)]$. Optimizing this upper likelihood over $bc$ gives at most $P$. At $\mu=y_+$ the derivative of the exponent-profiled likelihood in $\log t$ is $E[1-u_i-R(u_i)]+E R(u_i)/H$. Since $R(u)>1-u$, $R(1)=1/(e-1)$, $E R<1$ and $H\ge bD$, the derivative is strictly below $-w_+/(e-1)+1/(bD)$. Under the stated condition strict concavity in $t$ excludes larger $t$, which corresponds to $\mu<y_+$. This excludes pointwise equality even when the tail supremum equals $P$.

**Proved, finite cardinality.** The analytic profile is nonconstant by its strict power bound and its limit $P$. It is definable in the real exponential field, including the nuisance graph given by the unique positive score root. Its derivative is definable by the limit formula. O-minimality implies that the derivative's zero set is a finite union of points and intervals. Analyticity excludes an interval, so the finite stationary set is finite. Cell decomposition gives a uniform finite bound for fixed $n$ by applying it to the definable family in the observations. This gives no effective root count. It applies also to the analytic nonconstant held-scale branch below. [Wilkie's theorem](https://doi.org/10.1090/S0894-0347-96-00216-0)

### Inverse-Weibull signs and the balanced equality

**Proved.** Extend the prior first-order expansion in the chart $k=bT$, $d=ks$, $w_i=ky_i-d$, where $u_i=T e^{w_i/T}$. For $\Phi=\log k-Ew-Ee^{-w}$, the held-$T$ likelihood expands as
\[
\begin{aligned}
 L_T(k,d)&=\Phi+A/T+B/T^2+O(T^{-3}),\\
 A&=E[w+w^2(e^{-w}-1)/2],\\
 B&=E[(e^{-w}-1)w^3/6-e^{-w}w^4/8].
\end{aligned}
\]
Use $u_i-T=w_i+w_i^2/(2T)+w_i^3/(6T^2)+O(T^{-3})$ and expand the exponent contribution $-E e^{-(u_i-T)}+O(e^{-T})$. The log hazard adds exactly $w_i/T$ beyond $\log T$. The remainders are uniform on compact face-coordinate neighborhoods, also for the first two derivatives. The held-$c$ optimum exists by the previous analysis. Its face coordinates converge to the unique inverse optimum by exhaustion and the explicit trial approaching $I$. Write $*$ for this face optimum and $J=\nabla^2\Phi_*$, which is negative definite. The score gives displacement $-J^{-1}\nabla A_*/T+O(T^{-2})$. Consequently the optimized second-order coefficient is
\[
 C_*=B_*-\nabla A_*^{\mathsf T}J^{-1}\nabla A_*/2 .
\]
A positive first nonzero term gives a finite witness above $I$, sufficient for finite existence when $I\ge P$. A negative term excludes reaching $I$ for large $T$. It also gives $F(b)<I$ for sufficiently small $b$, since a competing sequence with $b\to0$ and value at least $I$ would have $T\to\infty$. An implementation still needs an effective inverse cutoff to use the negative sign as a tail certificate.

For log levels $0,1$, with lower share $p$ and upper share $q=1-p$, the inverse scores yield $Ee^{-w}=1$ and $E[w(1-e^{-w})]=1$. The identity $w^2=(w_L+w_H)w-w_Lw_H$ on two levels gives $A_*=k(q-1/2)$. Thus $(1,1,2),(1,2),(1,2,2)$ respectively give negative, zero and positive signs. At balanced counts center the levels at $\pm1/2$, write $w=u\pm z$ with $z=k/2$, and set $\eta=u-\log\cosh z$. The optimum has $z\tanh z=1$ and $u_0=\log\cosh z$. In these coordinates $\Phi=\log(2z)-\log\cosh z-\eta-e^{-\eta}$ has Hessian $-\mathrm{Id}$ there. On $\eta=0$, $A=u(1-z\tanh z)$, and at the optimum its partials are $-u_0z$ and $-E[e^{-w}w^2]/2$. Therefore
\[
 C_*=-\frac{3u_0^2+z^2}{6}-\frac{\operatorname{Var}_{e^{-w}}(w^2)}8+\frac{u_0^2z^2}{2}
     =-z^2/6 ,
\]
using tilted variance $4u_0^2(z^2-1)$. The balanced inverse equality is resolved at second order, with strict approach from below.

### Equality of the two excluded faces on two levels

**Proved.** On the unit log range,
\[
 I-P=\max_{k>0}\{\log k-\log(1+(q/p)e^{-k})-qk\}.
\]
Every trial strictly increases with $p$, and so does the attained maximum. It is continuous by the strictly concave inverse solve. At $p=1/2$ each trial is $\log[k/(2\cosh(k/2))]<0$, and as $p\uparrow1$ the trial $k=1/q$ tends to $+\infty$. There is one exact crossing $p_*$. For example $(1,2)$ ranks power higher and $(1,1,2)$ ranks inverse higher, the latter by the trial $k=3$ and $2e+e^{-2}<6$.

**Proved, integer counts exclude equality.** At the inverse optimum set $z=e^{-k}$. The score and value give
\[
 z=\frac{p(kq-1)}{q(1+pk)},\qquad I-P=\log(1+pk)-qk .
\]
If $p,q$ are rational, $k$ is transcendental by Hermite–Lindemann and the first equation. For $q=m/d$ equality would imply $z^m(1+pk)^d=1$. Substitution gives a nonzero polynomial over $\mathbb Q$ after clearing denominators. Its underlying rational expression on the left diverges as $k\to+\infty$ since the expression for $z$ tends to one. This contradicts transcendence. The exact face tie at $p_*$ is a real-weighted construction and ordinary sample multiplicities cannot realize it. This comparison leaves the finite profile to be checked. [Hermite–Lindemann–Weierstrass theorem](https://isa-afp.org/entries/Hermite_Lindemann.html)

**Numerical, 65 decimals.** $p_*=0.6428778088603729173577\ldots$. On $(1,2)$ with lower counts $5790528520856672$ and $5790528520856673$ out of $2^{53}$, the gaps are about $-2.94\cdot10^{-16}$ and $+5.41\cdot10^{-17}$. At $p=1/3,1/2,p_*,2/3$, the 49-point grid $b=2^{j/4}$, $-16\le j\le32$, gave only values below the larger face. **Proved by interval comparison**, the exact sample $(1^{[9]},2^{[5]})$ has $F(1)$ more than $0.147$ below each face. Both profile ends approach from below by the signed bounds. This gives disconnected superlevel sets, without deciding finite attainment on two levels. [Profile and face replay](2026-11-25-expweibull-math-data/check.out) · [Valley check](2026-11-25-expweibull-math-data/valley.out)

## Two finite local maxima and an exact real global tie

**Proved with interval comparisons.** Use the ordinary sample
\[
 x(v)=(1^{[510976]},2^{[513024]},2^v),\qquad 2\le v\le20,
\]
and work on $y=\log_2 x$. The three shares are $p,q,w$. Physical Weibull shape is $b/\log 2$ in this chart. Let $F_v$ denote the scalar profile and define
\[
 M_L(v)=\max_{0\le b\le1}F_v(b),\qquad
 M_R(v)=\max\{P(v),\sup_{b\ge1}F_v(b)\},\qquad F_v(0)=I(v).
\]
The exact tie sample is $x(v_*)$ for the smallest $v_*\in[2,20]$ satisfying $M_L(v_*)=M_R(v_*)$. The argument below establishes existence, finite attainment on both sides and strict separation above the excluded faces. It classifies as a real crossing. The integer counts give an ordinary real sample, with binary64 representability of its largest level open.

1. **Uniform separation.** For fixed $b$, moving the largest log observation up strictly decreases $F_v(b)$. The derivative is $wbQ_c(u_{\max})<0$ by the envelope derivative, $E Q_c=0$ at the scale optimum and the strictly decreasing $Q_c$. The profiled inverse value at fixed $k$ decreases by the analogous scale-score argument. Thus the inverse trial at $v=20,k=2.4$ lower-bounds every $I(v)$, and $F_2(1)$ bounds the trough from above. Also $P(v)=-\log((1-w)v-q)-1$ decreases. The signed brackets give $I(v)-F_v(1)>0.0122$ and $I(v)-P(v)>0.6858$. On 72 cells of width $1/4$, enclosing the face root by its decreasing score gives $A_*(v)>0.00137$. Hence $M_L(v)>I(v)$, attained at $0<b<1$.
2. **Right wins at $v=2$.** The two-level profile at $v=1$ bounds the left region from above by monotonicity. Set $q'=q+w$ and $\Delta=t(e^b-1)$. For $t\ge3$ use $H\ge e^{-t}(p+q'e^{-\Delta})$ and $H\le-\log Q(t)$. Substitution in $L_b$ bounds it by the inverse trial at shape $\Delta$ plus $\log[b/(e^b-1)]+bq'+H$. Since $e^b-1\ge b e^{b/2}$ and $b\le1$, the correction is at most $q'-1/2-\log Q(3)$. A tangent upper on the inverse maximum verifies this total bound below $-0.65$. For $t\le1/1024$ use $H\ge p\log1024$. For $b\le1/8192,\ t\le3$ use $H\ge-\log Q(3e)$. These bounds also give $L_b<-0.65$. Interval subdivision certifies the remaining rectangle $[1/8192,1]\times[1/1024,3]$, bisecting the larger relative width until each box upper lies below $-0.65$. The cover visited 4,051 cells. At $v=2$ the right trial $b=12,\ t=(2/5)e^{-12}$ gives more than $-0.629$.
3. **Left wins at $v=20$.** For $b\ge1,\ \mu\ge2$, $H\ge b[2(1-w)-q]$ from the two lower levels, so $L_b\le P(2)<I(20)$. For $\mu<2$ the top hazard is $u_+\ge e^{18b}$ and $H\ge p\exp(-u_+e^{-20b})$. For $u\ge8$, $\log R(u)\le-u/2$ by $-\log Q(u)\le1$ and $\log u+1\le u/2$. The likelihood is bounded above by
   \[
     \log b-\log p-1+u_+(e^{-20b}-w/2)
     \le \log b-\log p-1-(w/4)e^{18b}.
   \]
   The checked inequalities $e^{-20}<w/4$ and $we^{18}/4>1$ justify the bound and make it decreasing for $b\ge1$. At $b=1$ it is strictly below the fixed inverse trial at $v=20$. Hence $M_R(20)<I(20)$.
4. **Continuity and global attainment.** The explicit power cutoff is bounded on the deformation by $(e-1)/[w(2(1-w)-q)]$. Thus $M_R$ is a maximum over a fixed compact interval and $P(v)$. The extension $F_v(0)=I(v)$ is jointly continuous. For a sequence $b_j\downarrow0,\ v_j\to v$, the previous height and mode bounds are uniform because the two fixed levels $0,1$ have positive fixed shares. At least one lies at distance $1/2$ or more from the mode, bounding the mean likelihood above by $\log M+1-\min(p,q)M/2$ for mode height $M$. Uniform density and mode bounds give the same quantile exhaustion as on fixed data. Any subsequence retaining likelihood has an inverse-Weibull limit, and its log densities converge at the bounded varying level by the explicit face hazard formula. The inverse trial at the limiting face optimum, with $T=k/b_j$, gives the matching lower bound. Thus $M_L$ is continuous as well. The strict endpoint signs give a nonempty closed crossing set contained in $(2,20)$. At equality $M_L=M_R>I>P$ and the split has value below $I$, so both maxima occur at finite interior shapes in their respective regions. At $v=2$ the same argument gives the two local maxima, with the right region strictly higher. The analytic, nonconstant profiles give strict local maxima and finite cardinality. The proof allows further isolated maximizers within the two regions at equality.

The proof uses 45-decimal interval enclosures for every comparison in steps 1–3. No subdivision guard supplies an accepted comparison. Independent 60-decimal interval checks at $v=2$ give $A_*>0.00239$ and the right trial above $I$ by more than $0.0906$. Any left-region fit is refuted on that dyadic sample. At $v_*$, at least two finite global winners are proved. A complete typed set still requires isolating every winning root. [Uniform proof checks](2026-11-25-expweibull-math-data/tie.py) · [Retained signs and cover](2026-11-25-expweibull-math-data/tie.out) · [Independent valley checks](2026-11-25-expweibull-math-data/valley.py)

**Additional evidence.** On the exact dyadic sample $(1,2,4,8,32)$ use $y=\log(x)/\log32$, $b=0.1178,\ t=28.68$. A 50-decimal interval evaluation beats power by more than $0.1034$ and an upper bound on inverse Weibull by more than $0.00063$. The inverse upper is a tangent at $k=3.54$ over the signed bracket $[3.53,3.55]$. This proves finite attainment. A **numerical** binary64 scan on 113 log-spaced shapes from $2^{-8}$ to $2^6$ found two grid peaks near $b=1$ and $38$ on $x=2^j$ for $j=(-27,-20,-2,1,18,32,43,48)$, counts $(9,55,6,129,32,68,59,2)$, in a unit log-range chart. These are candidate regions on a smaller sample. [Finite witness](2026-11-25-expweibull-math-data/check.out) · [Seeded scan](2026-11-25-expweibull-math-data/free_scan.py)

## Weibull, exponentiated exponential and collapse

**Proved.** Weibull ($c=1$) and exponentiated exponential ($b=1$) are attained interior slices. The excluded faces on nonconstant data are power and inverse Weibull; collapse gives unboundedness on the applicable constant-data masks.

- With $b$ held and $a,c$ free, compute the Weibull optimum hazards $t_i=x_i^b/E x^b$. Equality with the Weibull value holds iff $1+E\log Q(t_i)=0$. The scale score is already zero, and the exponent score is sufficient by the proved uniqueness of the stationary point. Its sign also decides the side of $c=1$: the scale-profiled score in $c$ has exactly one zero, and the profiled value tends to $-\infty$ at both ends by the prior held-shape height bounds. Thus it increases before the zero and decreases after it.
- The exact real sample $(1-\delta,1+\delta)$ with $\delta=\operatorname{arcosh}(e/2)\in(0,1)$ and $b=1$ gives the unique fit $(a,b,c)=(1,1,1)$. At $a=1$ the average hazard is one and the product of CDF factors is $1-2e^{-1}\cosh\delta+e^{-2}=e^{-2}$. **Numerical, 65 decimals:** the dyadics $\delta=3710986536487807/2^{52}$ and $3710986536487808/2^{52}$ give opposite exponent scores, about $+4.18\cdot10^{-16}$ and $-1.37\cdot10^{-16}$. [Score replay](2026-11-25-expweibull-math-data/check.out)
- For positive algebraic inputs and rational held shape, the Weibull optimum hazards are positive algebraic and equality is excluded. More generally at any Weibull candidate with positive algebraic hazards, expand $\prod_i(1-e^{-t_i})=e^{-n}$ over subsets. The coefficient of $e^0$ stays one after grouping equal exponents, since every nonempty subset has positive total hazard. Lindemann–Weierstrass forbids the resulting nontrivial algebraic linear combination of exponentials of distinct algebraic numbers. The scope includes a held scale when its candidate hazards are algebraic. An unrestricted estimated Weibull shape need not yield algebraic hazards. [Lindemann–Weierstrass theorem](https://isa-afp.org/entries/Hermite_Lindemann.html)
- On a wider mask, release the coordinate and evaluate its score at the slice optimum. For Weibull use the $c$ score above. For exponentiated exponential use the $b$ score, $F'(1)$ in the physical log chart when scale and exponent are free. A nonzero score refutes slice equality. A zero requires the global comparison against the other finite roots and reachable faces. An exact real exponentiated-exponential equality example follows from $(1,2,4,8,32)$: let $b_0$ be the smallest physical shape in its proved nonempty finite maximizer set, and replace each $x_i$ by $x_i^{b_0}$. Power equivariance maps a winner to shape one, preserving all likelihood rankings. This construction gives no representability result.

## Held masks and sample predicates

**Proved.** The prior reports supply the structural cases below. The held-shape result above and the held-scale bounds below extend them. These predicates assume a nonempty sample and finite positive observations and holds. For constant data denote the common observation by $x_0$.

| Held | Exact predicate and outcome |
|---|---|
| None | Constant gives unbounded concentration, e.g. $(1,1)$. Otherwise compare the finite scalar roots and $P,I$. Equality of a globally winning finite root with the highest face is finite attainment. |
| $a$ | Constant with $x_0\ge a$ gives unbounded concentration, e.g. $(2,2)$ at $a=1$ or $2$. Otherwise all $x_i\le a$ gives the power envelope. For nonconstant data with some $x_i>a$, a finite optimum exists and needs a root comparison. |
| $b$ | Constant gives unbounded concentration. Otherwise solve the unique finite root of $L_b'$. |
| $c$ | Constant gives unbounded concentration. Otherwise strict concavity in $(b,-b\log a)$ gives a unique finite optimum. |
| $a,b$ | Unique exponent $1/H$, including constant data. |
| $a,c$ | Unbounded iff all $x_i=a$. Otherwise the strictly decreasing shape score gives a unique finite root. For $(1,1)$ at $a=2$ this mask has a finite fit and holding only $a$ gives a power limit. |
| $b,c$ | Unique finite scale by the monotone $Q_c$ score, including constant data. |
| $a,b,c$ | Evaluate the fixed point. |

With only $a$ held and all $x_i\le a$ with some below, let $D_a=E\log(a/x)$ and $w_a=\#\{x_i=a\}/n$. The previous proof gives the strict envelope $-\log D_a-1-w_a\log(e-1)$ in the physical log chart. At $w_a=0$ return the held power delegate, and at $w_a>0$ retain the penalized endpoint envelope in the tied power status. For $(1,2)$, holds $a=3,2,1$ give respectively the strict endpoint, tied endpoint and finite cases. All equal to $a$ has $D_a=0$ and gives unbounded concentration. [Held endpoint proof](2026-11-11-ew-theory2-report.md#held-scale-a)

**Proved, effective held-scale end bounds.** On the nonconstant finite-existence branch set $d_i=\log(x_i/a)$ with $d_+>0$, $d_-<d_+$, and $u_i=e^{bd_i}$. Write $w_-,w_+$ for the extreme shares. If $b\max_i|d_i|\le\log2$, then the profile is at most $\log b-1-\log[-\log Q(2)]$. At the large end $H\ge w_-e^{-u_{\min}}$ and $-\log Q(u)\le\log(1+1/u)$ by $e^u\ge1+u$. Using $E(u_i-u_{\min})\ge w_+(u_{\max}-u_{\min})$ in the profile gives
\[
 L(b)\le \log b-1-\log w_-+\log2+bE(d_i)_+
    -w_+e^{bd_+}(1-e^{-b(d_+-d_-)}).
\]
Above $\log2/(d_+-d_-)$ replace the parenthesis by $1/2$. This upper bound has negative second derivative, eventually has negative first derivative, and tends to $-\infty$. Signed height and slope comparisons close the ends relative to a finite witness. This gives a computable compact cover even with observations at the hold. Analyticity and o-minimality give finitely many stationary points, while uniqueness still requires ordering the roots.

## Verification

Five Python runs exited zero on the workstation through the one-slot lock. The scalar checks used 65 digits, with $b=2^{j/4}$, $j=-12,\ldots,16$ for the five-point witness and $j=-16,\ldots,32$ for the two-level cases. At balanced counts the replay with $T=100,1000,10000$ gave $T^2$ times the face gap tending to $-0.23987147\ldots$. The separate exploratory held-mask search found no pair of grid peaks in up to 10,000 seeded draws per mask, on a 401-point grid in $[-7,7]$ in log shape or log hazard. The proof uses the explicit inequalities and signed interval evaluations, while the grid searches only nominate candidates.

    mkdir -p build/tmp
    export TMPDIR=$PWD/build/tmp PYTHONDONTWRITEBYTECODE=1
    d=dev/research/evaluations/2026-11-25-expweibull-math-data
    /tmp/umvl-cpu/run nice -n 10 python3 -u "$d/scan.py"
    /tmp/umvl-cpu/run nice -n 10 python3 -u "$d/check.py"
    /tmp/umvl-cpu/run nice -n 10 python3 -u "$d/free_scan.py"
    /tmp/umvl-cpu/run nice -n 10 python3 -u "$d/valley.py"
    /tmp/umvl-cpu/run nice -n 10 python3 -u "$d/tie.py"

## What this changes for the fitter

1. **Held shape.** On nonconstant data bracket the decreasing hazard score, profile the exponent and propagate the bracket to restored scale and exponent. Check both public coefficients at the requested relative tolerance and use stable relative logs and tail evaluation of $H$. A representation failure is distinct from lack of a finite mathematical optimum. Return a certified singleton when the checks close. Preserve the proved constant-data stop.
2. **All free.** Cover the scalar shape profile with the signed nuisance solve. Use the explicit power cutoff for strict pointwise exclusion and the inverse coefficients for signed local comparisons with a validated cutoff, or to seed finite lower witnesses. For two levels the counts order the two excluded faces with no exact tie, but close numerical comparisons still need refinement. Certified nonattainment requires excluding finite attainment. On $x(2)$ a left-region fit loses, while on $x(v_*)$ both winning regions survive.
3. **Held scale and nested fits.** Use exact comparisons with the hold for the existing unbounded and strict or tied power outcomes. Use the end bounds and scalar root comparison on the finite branch. For Weibull and exponentiated exponential on a wider mask use the released-coordinate score and global height comparison; equality then preserves the finite fit.
4. **Value and coefficients.** A feasible finite or reachable face witness can refute an entire candidate root region by strict separation. At a proved finite/face tie return the finite fit. With proved multiplicity and a complete typed set return `unique = FALSE`; unresolved cardinality uses `unique = NA`. If a global certificate is inconclusive, retain an accuracy-checked successful search fit as uncertified. Actual search failure follows R6. Use the assigned solver and certificate budgets. [Outcome rule](2026-11-25-ties-math-report.md#what-this-changes-for-the-fitter) · [Accuracy contract](../../design/accuracy-contract.md) · [Search failure rule](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- **Complete finite tie and arithmetic.** The deformation proves at least two distinct global maxima. An effective root count for each winning region would complete the set, and validated continuation could narrow the first crossing. It is open whether the varying level is representable in binary64 or another binary64 sample has an exact finite/finite tie.
- **Two-level nonattainment and finite/face equality.** The counts order the excluded faces. The two-level conjecture still needs a strict global upper over the finite profile, and the general finite/face equality test needs an exact critical-value comparison. The arithmetic exclusions proved here are restricted to the stated face/face and algebraic-hazard scopes.
- **Budgeted covers and degeneracy.** Quantify inverse cutoffs by uniform remainders and bound score and value error on original inputs. Simultaneous $A_*=C_*=0$ needs higher terms or an independent bound. Held-scale uniqueness on the finite two-free branch needs stationary curvature or root isolation. O-minimal finiteness supplies neither a computable work bound nor a finite critical-value equality test.
