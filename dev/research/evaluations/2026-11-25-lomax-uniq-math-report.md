# Distinct Lomax rates can tie for the global maximum

TASK-292 · 2026-11-25 · Base revision ba3aca131ca165bdc10e1043d473bf289e8533f9

**Proved.** Ten copies each of 1 and 100, together with one copy of the exact real number $a_*$ defined below, give exactly two finite Lomax MLEs. Both strictly beat the exponential limit. The integer sample $(1,3000,3000,22000)$ already has two separated strict local maxima. Every sample with at most two distinct positive observations has at most one finite global maximizer, including with arbitrary positive weights.

**Correction to prior evidence.** The November 15 margins report states that a finite MLE exists iff the profile supremum is positive. The later Lean theorem includes a finite point attaining gap zero, which also gives a finite MLE. This job concerns equality between two finite fits at strictly positive gap. The earlier uniqueness audit's displaced-root proof obligation applies because distinct competing roots can both be global maximizers. [Margins report](2026-11-15-margins-report.md) · [Lean existence report](2026-11-25-lean-lomax-report.md) · [Lomax dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md) · [Uniqueness audit](2026-11-25-uniq-cover-math-report.md)

## Score and a cheap uniqueness condition

**Proved.** Write $r=\lambda$ for rate in the original units, $m=\bar x$, and $q=rm$. Bars denote weighted means with weights equal to sample proportions for unweighted fits. Set
$$
 A_r=\overline{\log(1+rx)},\quad B=\overline{\frac{rx}{1+rx}},\quad
 U=1-B,\quad C=\overline{\frac{rx}{(1+rx)^2}}.
$$
The prior profile theorem gives the unique conditional shape $1/A_r$ and the gap $F(r)=g(rm)=\log(rm)-\log A_r-A_r$. In $z=\log r$, the derivatives of $A_r,B$ are $B,C$ respectively, so
$$
 \frac{dF}{dz}=\frac{U}{A_r}H(r),\qquad H(r)=A_r-\frac{B}{U},
 \qquad \frac{dH}{dz}=B-\frac{C}{U^2}.
$$
Thus $H$ determines the profile score sign and $H(0)=0$ by continuous extension.

For distinct support points $x_j>0$ with weights $w_j>0$, set $D=\prod_j(1+rx_j)$ and $N=\sum_j w_jD/(1+rx_j)$. Then $U=N/D$, and with primes denoting $r$-derivatives,
$$
 \frac{dH}{dz}=\frac{P}{DN^2},\qquad
 P=(D-N)N^2-rD(ND'-N'D).
$$
Between successive zeros of $H$, counting 0 as the first, Rolle's theorem gives a distinct positive zero of $P$. Two strict local maxima would require a stationary minimum between them and therefore at least three positive zeros of $H$. Every local maximum is strict because the profile is real analytic on $r>0$ and nonconstant by its right-tail behavior.

For two support points $x_1,x_2$, let $s=x_1+x_2,\ d=x_1x_2,\ b=s-m$. Then $D=1+sr+dr^2$, $N=1+br$, and
$$
 \frac{P}{r^2}=m(b-m)-d+[mb^2-d(b+3m)]r-d(bm+2d)r^2-bd^2r^3 .
$$
The cubic is strictly concave for $r>0$ since $b,d,m>0$, so it has at most two positive zeros. Thus the profile has at most one local maximum. The constant case has no finite joint maximum by the known constant-sample result. Checking for at most two distinct values takes one pass. For more support points, at most two sign variations in the nonzero coefficient sequence of $P$ suffice for uniqueness by Descartes' rule and the same Rolle argument. These criteria guarantee uniqueness conditional on attainment. [Lean profile and constant-sample results](../../lean/UnivariateML/Family/Lomax.lean)

## Exact tie construction

**Proved.** Use the sample $X_a=(a,1^{[10]},100^{[10]})$, with multiplicities in brackets. Set $w=1/21,\ p=20/21,\ m_a=(1010+a)/21,\ r_0=1024$, and $I=[2^{-512},2^{-32}]$. Define
$$
 L(a)=\max_{0\le r\le r_0}F_a(r),\qquad
 R(a)=\max_{r\ge r_0}F_a(r),\qquad F_a(0)=0.
$$
The exact sample is $X_{a_*}$ with $a_*$ the unique solution of $R(a)=L(a)$ in $I$. The following bounds and root count prove that this definition specifies one real number and that its two global maxima are strict.

### Separating the peaks and reversing their order

The extension of $F_a$ to 0 is jointly continuous on this family since $\log(1+rx)/r$ extends to $x$ and $m_a$ stays positive. The right tail tends uniformly to $-\infty$ on $I$. Indeed, with $s_a=p\log 10+w\log a=\overline{\log x}$, one has $A_r\ge z+s_a$ and $F_a(r)\le\log m_a-s_a-\log(z+s_a)$ when $z+s_a>0$. Both $s_a$ and $\log m_a$ are bounded on $I$. Take the omitted tail below the minimum of $F_a(r_0)$ over $I$ to compute $R$ on a fixed compact interval. Joint continuity on the two fixed compact intervals makes $L,R$ continuous.

Let $m_{\rm lo}=1010/21,\ m_{\rm hi}=1011/21,\ z_0=10\log 2,\ T=32\log 2$.

| Claim | Bound |
| --- | --- |
| $L(a)>1/10$ on $I$ | At $r=1/10$, $A_r\le V=(11\log(11/10)+10\log 11)/21$ and $F_a(r)\ge\log(m_{\rm lo}/10)-\log V-V>1/10$. |
| $F_a(r_0)<0$ on $I$ | For $z\ge z_0$, the observations at 1 and 100 give $A_r\ge p(z+\log 10)$. Hence $F_a(r)\le E(z)=z+\log m_{\rm hi}-\log[p(z+\log 10)]-p(z+\log 10)$, and $E(z_0)<0$. |
| $R(2^{-32})<0$ | $E$ is convex and $E(T)<0$, so $E<0$ from $z_0$ to $T$. For $z\ge T$, the remaining observation gives $A_r\ge z+p\log 10-wT$. The resulting bound for $F$ decreases strictly in $z$ and equals $E(T)$ at $T$. |
| $L(a)<11$ on $I$ | $A_r\ge p\log(1+r)\ge pr/(1+r)$, hence $F_a(r)\le\log(m_{\rm hi}(1+r_0)/p)<11$ for positive $r\le r_0$. |
| $R(2^{-512})>11$ | Evaluate at $r=2^{512}$, with $z=512\log 2$. Then $ra=1$ and $1+rx\le2rx$ for $x=1,100$, giving $A_r\le W=p(z+\log 10)+\log 2$ and $F_a(r)\ge z+\log m_{\rm lo}-\log W-W>11$. |

The log comparisons are exact rational inequalities. Reduce a positive rational log argument by a power of two to $1\le y\le2$. For $v=(y-1)/(y+1)$, enclose $\log y$ between $2\sum_{j=0}^{23}v^{2j+1}/(2j+1)$ and that sum plus $2v^{49}/[49(1-v^2)]$. Use the same enclosure at 2 and monotonicity for nested logs. The retained checker implements these bounds with fractions. The intermediate value theorem gives a tie in the interior of $I$. At any tie the common value exceeds $1/10$, so both maxima are interior and separated by $r_0$. At the explicit dyadic input $a=2^{-512}$, each side also has a strict local maximum, and the right side wins globally. [Rational checks](2026-11-25-lomax-uniq-math-data/exact_checks.py)

### Counting the roots and specifying the exact tie uniquely

Put $\widetilde N=21N$ and $c_j(a)=[r^j]\,21^3P$. With the factors $1+r,1+100r,1+ar$,
$$
 21^3P=(21D-\widetilde N)\widetilde N^2-
       21^2rD(\widetilde ND'-\widetilde N'D).
$$
It has degree at most 8 in $r$. The coefficients $c_0,c_1$ vanish since $D(0)=N(0)=1$ and the linear terms cancel. The leading coefficient is $c_8=-21^2(100+1010a)(100a)^2<0$. The coefficients $c_2,c_3,c_6$ are positive throughout $I$.

- At $a=0$, $c_2(0)=21^3(\overline{x^2}-2m^2)=1{,}260{,}210$, obtained by expanding $BU^2-C$ to second order.
- At $a=0$, let $s=101,\ d=100,\ m=1010/21,\ b=s-m,\ e=100/21$. Substituting $D=1+sr+dr^2,\ N=1+br+er^2$ gives $c_3(0)/21^3=m(b^2+4e-3d)-bd+es>48(52^2-300)-5300>100{,}000$.
- At $a=0$, the derivative term has degree at most 5 and the first term gives $c_6(0)=2000\cdot100^2=20{,}000{,}000$.
- The sums of absolute bivariate coefficients of $D,\widetilde N$ are 404 and 2262, given by evaluation at $r=a=1$. For $21^3P$ the sum is at most
  $$
   (21\cdot404+2262)2262^2+
   21^2\cdot404(2262\cdot3\cdot404+2\cdot2262\cdot404)<10^{12}.
  $$
  Therefore $|c_j(a)-c_j(0)|\le10^{12}2^{-32}<233$ throughout $I$.

The signs at $2,3,6,8$ allow at most three sign variations across the sequence. Descartes and Rolle give at most three positive roots of $H$. When each side has an interior maximum, the two maxima and intervening minimum exhaust those roots. They are simple, since a vanishing $r$-derivative at one of the three roots of $H$ would give a fourth positive root of $P$ in addition to the three Rolle roots from 0 onward. At any tie the whole maximizer set consists of the pairs $(1/A_{r_-},r_-)$, $(1/A_{r_+},r_+)$ with $r_-<r_0<r_+$. The shapes differ because $A_r$ increases strictly.

At a zero of $R-L$, the unique regional maxima have simple score roots, so the implicit function theorem gives smooth local root branches. Compactness and joint continuity force any sequence of regional maximizers approaching the given $a$ to converge to the unique regional maximizer there. Thus the maxima remain on those branches locally. At a stationary rate $r$, $1+1/A_r=1/B$, so differentiation of the peak height gives
$$
 w/m_a-w\frac{r}{(1+ar)B}.
$$
The reciprocal of $r/[(1+ar)B]$ is $\overline{x(1+ar)/(1+rx)}$, strictly decreasing in $r$ because all $x\ge a$ and some $x>a$. Thus $(R-L)'<0$ at every zero. The zeros are isolated, finite by compactness and the strict endpoint signs, and each crosses from positive to negative. Two successive such crossings would force another zero between them. This proves that $a_*$ is unique.

## Small samples and numerical evidence

**Proved.** The integer sample $(1,3000,3000,22000)$ has strict score signs $+,-,+,-$ at $r=10^{-5},10^{-3},10^{-1},1$. Applying the rational log bounds above and exact $B/U$ gives $H$ respectively in $(0.0010,0.0011),(-0.117,-0.115),(1.43,1.44),(-0.313,-0.311)$. Each disjoint positive-to-negative bracket contains an interior maximum. Analyticity and nonconstancy give strictness. Thus three distinct support points are necessary and sufficient for the existence of a sample with two local maxima. [Rational checks](2026-11-25-lomax-uniq-math-data/exact_checks.py)

**Numerical.** The binary64 scan varied consecutive log-support gaps over $\{0.125,0.5,1,2,3,5,8,12,20,40,64,96,128,192\}$. It used all positive multiplicity vectors through total sample size 12, all denominator-20 weight vectors and denominator-100 vectors with at least one unit numerator. After deduplication the two-point search had 55 weight vectors and 770 profiles, with at most one detected maximum. The three-point search had 619 weight vectors and 121,324 profiles, with at most two detected maxima. It sampled $\log(r\max x)$ in steps of 0.125 from $-16$ through $\log(\max x/\min x)+12$ and counted positive-to-negative score crossings. The smallest detected sample size with two maxima was four, with weights $(1/4,1/2,1/4)$ and log gaps $(8,2)$. Rounding its support to $(1,3000,22000)$ led to the exact sign check above.

**Numerical.** At 100 decimal digits, signed score brackets and a scalar equality solve gave $-\log a_*\approx59.86347060008475280935$, or $a_*\approx1.00374882710034187804\times10^{-26}$.

| Peak | Log rate in original units | Gap per observation | Log-rate curvature |
| --- | ---: | ---: | ---: |
| Left | 0.10385621537032009696 | 0.41861232464268468649 | $-0.09312020883557065446$ |
| Right | 60.50449562160899540112 | 0.41861232464268468649 | $-0.01067076005132024758$ |

The numerical score and equality residuals were below $10^{-80}$. Changing $-\log a$ by $-1$ and $+1$ from the reported value gave right-minus-left gaps of $-0.0315795595215268$ and $+0.031839071453804$, respectively. The exact construction supplies the proof of equality and globality. [Search and refinement](2026-11-25-lomax-uniq-math-data/search.py)

Both retained scripts completed with exit zero on the workstation through the single-slot wrapper. This math-only job used no library builds, native fitter runs or simbox runs. From the worktree root, run

    d=dev/research/evaluations/2026-11-25-lomax-uniq-math-data
    /tmp/umvl-cpu/run python3 "$d/exact_checks.py"
    /tmp/umvl-cpu/run python3 "$d/search.py"

## Transfer and Lean

**Proved.** Each exact transformation adds a parameter-independent log Jacobian to the Lomax log likelihood and gives a bijection on the two free coefficients. It transfers the full maximizer set, the two-support uniqueness theorem and the counterexamples by inverse transformation of the data. [Inverse Pareto dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/README.md) · [Pareto II dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md) · [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md)

| Route | Lomax data $y$ | Map from Lomax shape and rate $(\kappa,r)$ | Added log Jacobian |
| --- | --- | --- | --- |
| Inverse Pareto | $y=1/x$ | shape $\kappa$, scale $r$ | $-2\sum_i\log x_i$ |
| Pareto II with threshold $\mu$ held and positive excesses | $y=x-\mu$ | shape $\kappa$, scale $1/r$ | $0$ |
| Burr with power $c>0$ held | $y=x^c$ | first shape $\kappa$, scale $r^{-1/c}$ | $n\log c+(c-1)\sum_i\log x_i$ |

The Burr face with power fixed receives the same additive term, so its tied finite fits strictly beat the corresponding Weibull. Zero excesses and a free threshold in Pareto II have separate behavior. Holding one of the two remaining coefficients inherits the existing conditional uniqueness theorem. A certificate for the original machine inputs has to account for transform rounding in all comparisons, including those for competing roots. [Certificate replay](2026-11-22-replay-covers-report.md)

For Lean, add a theorem giving $\exists! p$ in the existing domain with IsMaxOn for logLik, under positivity, at most two distinct sample values and existence. Reuse shape_profile, profile_gap and exists_max_iff. New lemmas express the two-point averages by a multiplicity weight, differentiate $H$, expand the rational numerator into the strictly concave cubic, and use an intervening minimum and Rolle to bound the number of stationary rates. The counterexample theorem can assert a positive Fin 21 → ℝ sample whose maximizing set consists of two pairs of unequal rates with log likelihood above bestExp. Additional lemmas would prove continuity of the two compact regional maxima, the rational log bounds, the three-root bound, the height crossing and the lift by shape profiling. An existential counterexample theorem can use a height equality supplied by the intermediate value theorem.

## What this changes for the fitter

- For at most two distinct positive observations, a signed positive-to-negative score bracket with a feasible positive-gap witness inside it proves the bracketed local maximum is the unique finite global maximum. A higher point would force a finite global maximum by the existing tail and compactness proof, and the two-support theorem permits only one local maximum. Continue to verify the requested coefficient accuracy. An exactly verified zero gap at the bracketed maximum also gives a unique finite MLE.
- For general samples, retain every distinct candidate root enclosure until its maximum-value upper is strictly below a feasible lower witness for the winner, or until a proof identifies it with an already retained root. The prior audit gives a bracket-level Taylor bound and an endpoint tangent bound. One isolated survivor after all global exclusions supports certified singleton status. A proved finite tie requires a complete typed maximizer set with known nonuniqueness, or a nonuniqueness diagnostic where the interface cannot return the set.
- Overlapping value bounds leave equality and uniqueness unresolved. When the certificate cannot close and the search supplies an accuracy-checked candidate that has survived refutation, return it with certified = false and uniqueness unknown. A strictly better separated feasible witness refutes that candidate. An exponential margin supplies a bound on gain, while this positive tie requires finite-root comparisons. [Wrapper failure handling](../../validation/diagnostics.md#wrapper-failure-handling-rule-r6)

## Open

- The minimum sample size for a finite global tie lies between 3 and 21. For two local maxima the bounds are 3 and 4. The structured scan did not detect a size-three example, so the equal-weight three-point case remains open.
- The exact tie is specified over the reals. Exact binary64 representability and a production trace in which a distinct root displaces an earlier root and changes the returned answer remain open. The need for competing-root comparison follows mathematically regardless of queue order.
