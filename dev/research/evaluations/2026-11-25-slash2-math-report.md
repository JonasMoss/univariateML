# Slash likelihood at a zero-sum half tie

**Proved.** The absolute residual multiset, even with the positive and negative counts and the signed mean and range, cannot decide finite attainment. Two exact samples with those features in common have opposite outcomes. On rational data, the optimal scale at any fixed rational location gives a likelihood strictly different from the collapse value. Exact equality at a finite free-location maximum remains open for rational data.

## Correction to the paired-collapse proof

The [preceding slash report](2026-11-25-slash-math-report.md) dropped a factor of four in the Cauchy-envelope proof for a block of two tied observations and two opposite residuals. Its envelope bounds the normalized block likelihood by \(4/D\), where \(D\) is the denominator in that proof. The following replacement proves the same strict-collapse result and supplies the collapsing endpoint for the preceding report's equality example. The compact stationary-value comparison and corner exclusion from that report apply directly.

**Proved.** Translate the half tie to zero. Write \(A(t)=(1-e^{-t})/t\), with \(A(0)=1\). In the block \((0,0,-d,d)\), \(d>0\), let \(x=|m|/d\), \(y=d^2/(2s^2)>0\), and \(w=x^2 y\). The ratio to the block's collapse value is
\[
P=y^2 A(w)^2 A\bigl(y(1-x)^2\bigr)A\bigl(y(1+x)^2\bigr).
\]
Use \(A(t)\le1\), \(A(t)<1/t\) for \(t>0\), and \(A(t)\le(1+t/2)^{-1}\). The last bound follows from \(\tanh(t/2)\le t/2\).

- For \(x\ge1/2\), bound the nearer off-point factor by 1 and the farther one by the tail bound. Since \(w/(1+w/2)^2\le1/2\),
  \[
  P\le\frac{y A(w)^2}{(1+x)^2}
  \le\frac{1}{2x^2(1+x)^2}\le\frac89.
  \]
- For \(x<1/2\) and \(y\ge8/3\), use both tail bounds. Here \(y(1-x^2)/2\ge1\), so
  \[
  (1-x^2)(1+w/2)=1+x^2\bigl(y(1-x^2)/2-1\bigr)\ge1.
  \]
  Consequently \(P<A(w)^2/(1-x^2)^2\le1\).
- For \(0<x<1/2\) and \(y\le8/3\), keep the nearer off-point exponential. The function \(e^{-(8/3)(1-x)^2}/(2x^2)\) decreases on \((0,1/2]\), since its log derivative is \((16/3)(1-x)-2/x<0\). Its value at \(1/2\) exceeds 1 by \(\log 2>2/3\), which follows from strict Jensen on \(\int_1^2 t^{-1}dt\). Thus \(e^{-y(1-x)^2}>2x^2\) and \(1-e^{-y(1-x)^2}<(1-x^2)^2\). Express the off-point factors in tail form, bound the farther exponential factor by 1, and use \(A(w)\le1\) to get \(P<1\). At \(x=0\) the exact ratio is \((1-e^{-y})^2<1\).

Normalized likelihood ratios multiply across blocks with the same tied location, since their collapse values multiply. Pair all off-point residuals as \((d,-d)\) and allocate two zeros to each block. Every finite likelihood is strictly below the collapse value. The vertical path approaches that value, so the supremum is unattained.

## Exact residual counterexample

**Proved.** Let \(b=300000/10029\). Superscripts in square brackets denote multiplicities. The following samples each have 120 zeros and 120 off-point residuals.

| Sample | Off-point residuals | Outcome |
|---|---|---|
| \(W\) | \((-1)^{[2]},\,(-10000)^{[58]},\,b^{[60]}\) | Finite MLE strictly above collapse |
| \(C\) | \((\pm1)^{[1]},\,(\pm10000)^{[29]},\,(\pm b)^{[30]}\), multiplicities for each sign | Unattained collapse |

The reciprocal sum for \(W\) is \(2(-1-29/10000+30/b)=0\). For \(C\) it vanishes pairwise. Both have 60 positives and 60 negatives, and their absolute residual multisets agree. This matches the boundary value, all absolute moments and quantiles, and \(k,a,r,Q\) in the previous report's profile expansion. Multiplying both samples by 10029 puts every observation on exactly representable binary64 integers.

For the strict win, \(W\) is twice the sample \(X(1/10000)\) from the preceding report. At \(m=b/5,\ s=3b/10\), that sample's normalized product exceeds 2 by the exact rational check below. It follows that \(W\) exceeds its collapse value by a factor greater than 4, and the preceding compactness result gives finite attainment. The repaired paired bound proves collapse for \(C\).

For reproducibility, with \(\lambda=1/(2s^2)\), the witness product is
\[
A(\lambda m^2)^k\prod_{i=1}^k \lambda d_i^2 A(\lambda(d_i-m)^2).
\]
For \(0<t<20\), a rational lower bound on \(A(t)\) is \((u/t)\sum_{j=0}^{39}(-u)^j/(j+1)!\), \(u=\min(t,4)\). Integrating the degree-39 Taylor lower bound for \(e^{-uz}\) on \(0\le z\le1\) proves the bound, using monotonicity of \(1-e^{-t}\) to cap \(t\). For \(t\ge20\) use \(t^{-1}(1-1/\sum_{j=0}^{40}20^j/j!)\) by the positive series for \(e^t\). The verification script multiplies the positive rational lower bounds and checks the claimed threshold exactly. [Verification script](2026-11-25-slash2-math-data/check.py)

The counterexample can also match the signed mean and both extrema. Form \(W'\) by pooling 100 copies of \(W\), the sample \((-100)W\) obtained by multiplying its observations by \(-100\), and a block \((0,0,-10^6,10^6)\). Form \(C'\) with the same number of zeros and the off-point magnitudes of \(W'\) paired by sign. All their magnitude multiplicities are even. The signed mean is zero in both by cancellation and their ranges are \([-10^6,10^6]\). Each block has zero reciprocal sum. At the same witness \(m=b/5,\ s=3b/10\) one has \(m<6,\ s<9\). For each \(d\) with \(|d|\ge100\), the product of one tied factor and its off-point factor exceeds
\[
(8/9)(9/10)^2(3/4)>1/2 .
\]
Indeed \(A(2/9)\ge8/9\), \(|d/(d-m)|>9/10\), and \((d-m)^2/(2s^2)>2\). Thus the ratio for \(W'\) exceeds \(4^{100}2^{-120}2^{-2}>1\). The ratio for \(C'\) is strictly below one everywhere by pairing. This rules out any decision inequality whose inputs are functions of the absolute residual multiset together with sign counts, signed mean and signed extrema.

## Exact equality and representable inputs

**Proved exclusion at commensurate squared distances.** Translate the half tie to zero. Let \(r_j=x_j-m\) for all \(2k\) observations and let \(\ell\) count the zero \(r_j\). At \(m=0\) the vertical likelihood is strictly below collapse. For \(m\ne0\), \(\nu=k-\ell>0\) and the normalized ratio has the form
\[
R= \frac{L}{B}
 =C_0\lambda^{-\nu}\prod_{r_j\ne0}(1-e^{-\lambda r_j^2}),
\qquad C_0=\frac{\prod_i d_i^2}{\prod_{r_j\ne0}r_j^2},
\qquad \lambda=\frac1{2s^2}.
\]
Suppose the residuals \(d_i\) are algebraic and \(r_j^2=qN_j\) for the nonzero \(r_j\), with \(q>0\) algebraic and positive integers \(N_j\). Put \(\tau=q\lambda,\ z=e^{-\tau}\),
\[
H(z)=\sum_{r_j\ne0}\frac{N_j z^{N_j}}{1-z^{N_j}},
\qquad D(z)=\prod_{r_j\ne0}(1-z^{N_j}).
\]
Scale stationarity gives \(\nu=\tau H(z)\) by differentiating \(\log R\). If also \(R=1\), then \(C_0(qH(z)/\nu)^\nu D(z)=1\). Its left side is a rational function with algebraic coefficients tending to zero at \(z=0\). Clearing denominators gives a nonzero polynomial, so \(z\) is algebraic. Then \(\tau=\nu/H(z)\) is nonzero algebraic, contradicting Hermite–Lindemann for \(z=e^{-\tau}\). This applies in particular to dyadic input and any exactly rational location, including a lower-multiplicity observed location. [Formal proof of Hermite–Lindemann–Weierstrass](https://isa-afp.org/entries/Hermite_Lindemann.html)

In that case refinement of the unique scale bracket and convergent likelihood enclosures eventually separates the sign. A proof that the *true* location is rational suffices for this exclusion. A rounded iterate supplies a rational test location at which a strictly positive likelihood gap can witness attainment.

**Proved exclusion at algebraic parameter pairs.** For algebraic data and algebraic \(m,s\) with \(s>0\), equality would give
\[
\prod_{r_j\ne0}(1-e^{-\lambda r_j^2})-\lambda^\nu/C_0=0
\]
away from the half-mass value. Expand the product and group identical exponents. All exponents and coefficients are algebraic. The most negative exponent includes all the nonzero squared distances and occurs only once, with coefficient \(+1\) or \(-1\). The resulting nontrivial linear relation contradicts Lindemann–Weierstrass. This excludes exact equality at any represented finite parameter pair on binary64 observations. The exact maximizer can have nonalgebraic parameters. [Formal proof of Hermite–Lindemann–Weierstrass](https://isa-afp.org/entries/Hermite_Lindemann.html)

**Proved form of a positive equality certificate.** In coordinates \(\theta=(m,\log s)\), set \(F=\log R\) and \(g=\nabla F\). An explicit analytic identity \(F=h_1g_1+h_2g_2\) on an isolating neighborhood of a stationary root certifies its equality to collapse. A proof of global maximality is also needed, for example strict local concavity there and a strict upper bound on the compact remainder after the corner exclusion. If several equality roots exist, the global comparison must cover each one. At a nondegenerate stationary root this identity is also necessary locally. The analytic inverse function theorem makes \(y=g(\theta)\) local coordinates, and \(\widetilde F=F\circ g^{-1}\) obeys \(\widetilde F(y)=\sum_i y_i\int_0^1\partial_i\widetilde F(ty)\,dt\) when \(\widetilde F(0)=0\). This proves existence of analytic multipliers in the equality case. A practical positive certificate requires an independently verifiable identity or common-root relation for the given input. Numerical root isolation and small residuals supply evidence of proximity to equality.

## Numerical checks

The final run exited 0 on the workstation. All computation used one slot.

    TMPDIR=$PWD/build/tmp /tmp/umvl-cpu/run nice -n 10 python3 -u \
      dev/research/evaluations/2026-11-25-slash2-math-data/check.py

**Numerical.** At 80 decimal digits, continuation of the positive-location stationary branch of \(X(e)\) used steps of \(10^{-4}\) from \(e=10^{-4}\), followed by a joint solve of both scores and the zero-gap equation. It gave
\[
(e,m,s)=(0.0036748723303330863412531268980079,\quad
5.669464302832150786159359741259,\quad
8.8880780789796470498328115467098).
\]
The numerical Hessian was negative definite and the three equation residuals were below \(10^{-75}\). Shifting \(e\) by \(-10^{-12}\) and \(+10^{-12}\), then re-solving the scores, gave log gaps \(2.397356207896473\times10^{-10}\) and \(-2.397356207644603\times10^{-10}\), respectively. The global equality example in the earlier report is defined as the supremum of strict-win parameters. The computed root is a candidate for that parameter.

**Numerical.** The profile survey used 45 decimal digits, 16 equal subdivisions per gap between distinct observations, and extra locations \(\pm a\,2^{-j}\) for \(1\le j\le20\). It bisected 54 times in log scale at each location and refined sampled positive-to-negative location-score crossings on each side of the tie. The positive-gap column counts a grid or refined log gap above \(10^{-25}\). Grid values and refinements probe for competing modes between observations. Negative results have no global upper bound from the scan.

| Sample set | Positive gap found | No positive gap found |
|---|---:|---:|
| Original paired example \((0,0,-1,1)\) | 0 | 1 |
| Original strict-win example, ten zeros, one \(-1\), nine \(9\)'s | 1 | 0 |
| Exact counterexample pair \(W,C\) | 1 | 1 |
| Computed equality parameter | 0 | 1 |
| \(X(e)\) at 38 rational parameters between \(10^{-4}\) and 1 | 5 | 33 |
| Seed 326, random paired samples | 0 | 12 |
| Seed 326, two groups with random reciprocal weights and balanced reciprocal sums | 0 | 12 |
| Seed 326, one negative residual against a positive cluster of random reciprocal weights | 12 | 0 |

All rational samples passed an exact zero-reciprocal-sum check. The five sweep wins were at \(e=0.0001,0.0005,0.001,0.002,0.003\). The paired and strict-win results agree with the proofs. The mixed and clustered counts characterize this seeded survey.

**Proved strict win near the transition.** For \(e=p/q=367487/100000000\), multiplying \(X(e)\) by \(p(q+29p)\) makes all observations exactly representable binary64 integers, checked against their exact rational values. In the original units the witness \((5.6694643,8.8880781)\) gives \(R>1+4\times10^{-7}\) by the rational lower bound, with numerical log gap \(5.586637617217239\times10^{-7}\) at 80 digits. This is a witness gap. It establishes attainment for the exact binary64 sample independently of the numerical transition solve.

## What this changes for the fitter

- At an exactly-half tie, if the off-point multiset pairs exactly about the tied location, return **CollapseLimit** with the unattained-supremum reason. Exact comparison can use the dyadic observations and the identity \(x_i+x_j=2v\). With two off-point residuals, verified zero reciprocal sum already implies pairing.
- For other verified zero-sum samples, a finite likelihood witness strictly above collapse proves attainment. Certifying returned coefficients additionally requires the interior global comparison and the accuracy check. A strict upper bound below collapse over the compact complement of the excluded corner proves **CollapseLimit**.
- If an exact identity and global bound certify equality at a finite maximizer, return the finite MLE and preserve any independently proved multiplicity. A tolerance on the likelihood gap supplies no evidence of exact equality.
- If the comparison remains unresolved or the certificate budget runs out, keep a successful search result with **certified = FALSE** when it passes the applicable checks and has not been refuted. Report a failed search, a refuted candidate without a valid replacement, or proved nonexistence according to its reason.

## Open

A closed decision inequality in the full signed residuals remains open. The counterexample rules out the stated summary class. The finite stationary-value test is the general exact criterion available here.

For rational observations, exact equality at a nonrational stationary location remains open. Resolving it requires either an exclusion that permits incommensurate squared distances, or a rational sample with an exact common-root identity and a global upper bound. The local analytic representation gives the form of an identity certificate at a nondegenerate root. Its existence proof is implicit in the unknown root.

Identification of the computed transition with the earlier report's exact \(e_*\) requires a certified global comparison near the numerical crossing and exclusion of any later strict-win interval in \(e\).
