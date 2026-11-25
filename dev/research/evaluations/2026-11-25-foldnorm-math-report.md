# Folded-normal joint fit: winner tests, root count and ties

TASK-304. For nonconstant nonnegative samples, a test using the first, second, fourth and sixth raw moments certifies a **unique half-normal maximum**, including some samples at quartic equality. A separate variance-ratio test certifies a **positive-location winner**, even when the half-normal is locally maximal. The profile has at most \((3^k-1)/2\) interior maxima for \(k\) distinct positive observations. The real tie in the 99-to-1 family is unique along that path. Two adjacent binary64 outliers give opposite winners in the numerical replay. An exact rational-data tie is excluded conditional on Schanuel’s conjecture.

Use the joint profile and existence result from the [October analysis](2026-10-29-foldnorm-report.md). Write \(m_j=\overline{x^j}\), \(z=x/\sqrt{m_2}\), \(M=\overline z\), \(A=\overline{z^4}\) and \(B=\overline{z^6}\). In formulas using \(z\), \(a\) denotes the normalized profile coordinate, equal to the physical \(\mu/\sigma^2\) multiplied by \(\sqrt{m_2}\). Write \(s\) and \(v\) for location and variance in these units. The profiled values satisfy \(v=1-s^2\) and \(a=s/v\). With \(S(a)=\overline{z\tanh(az)}\), the per-observation profile score is \(S(a)-s(a)\). Constant samples retain the unbounded-collapse outcome.

## Uniform winner tests

**Proved, sufficient half-normal test.** Set \(b=s^2\) and
\[
Q(b)=3(1-b)^2+\frac{6B}{5A}b.
\]
If \(A\ge 3\) and \(A\ge Q(M^2)\), the profile is strictly decreasing for \(a>0\). The unique maximum is \((0,\sqrt{m_2})\) in physical units. Both comparisons allow equality. For \(\{0,0,1\}\), \(A=3\), \(B=9\), \(M^2=1/3\), and \(Q(M^2)=38/15\). The test also covers samples with strictly positive observations, for example nine copies of \(1/64\) and one \(1\), where substitution of the rational moments gives \(A>9\) and \(Q(M^2)<4\).

For the proof let \(R(t)=t(15+t^2)/(15+6t^2)\). Direct differentiation gives
\[
R'(t)-(1-R(t)^2)=\frac{t^6}{(15+6t^2)^2}.
\]
For \(D=R-\tanh\), this means \(D'+(R+\tanh)D=t^6/(15+6t^2)^2\) with \(D(0)=0\). An integrating factor gives \(D(t)>0\) for \(t>0\). Therefore
\[
 a-S(a)> a^3\overline{\frac{z^4}{3+(6/5)a^2z^2}}
 \ \ge\ \frac{a^3 A}{3+(6/5)a^2 B/A}.
\]
The last inequality is Jensen’s inequality for the reciprocal, with weights proportional to \(z_i^4\). At least one \(z_i>0\), so the first inequality is strict. Since \(a-s=ab\) and \(b=a^2(1-b)^2\), \(A\ge Q(b)\) implies \(S(a)<s(a)\) for \(a>0\). Convexity bounds \(Q(b)\) on \([0,M^2]\) by the larger endpoint value. For \(b\ge M^2\), \(S(a)<M\le s(a)\) at finite \(a>0\). This proves the global and equality claims. Using any \(b_0<M^2\) in the same endpoint test also certifies the score sign from zero through \(a_0=\sqrt{b_0}/(1-b_0)\) even if the global test is inconclusive.

**Proved, sufficient positive-location tests.**

- The prior inward expansion gives a positive-location winner if \(A<3\), or if \(A=3\) and \(B>15\). Attainment then gives at least one positive global maximizer.
- If \(M^2\ge 3/4\), evaluate at the ordinary normal estimates \(\mu=\bar x,\ \sigma^2=m_2-\bar x^2\). The ordinary normal per-observation log likelihood there exceeds the fitted half-normal value by \(-\tfrac12\log[4(1-M^2)]\), with equality at \(M^2=3/4\). The folded density adds a strictly positive reflected normal density at every observation, so the folded candidate wins strictly even at equality. This test covers the prior sample of 99 ones and one five, where \(A>3\).
- Any feasible positive profile point with a separated positive likelihood difference also excludes the half-normal. In normalized units its difference per observation is \(\Delta(a)=\overline{\log\cosh(az)}-a s-\tfrac12\log v\), by substitution of \(v=1-s^2\) into the likelihood. A witness resolves the face comparison while the positive branch still needs a global search.

## Number of interior maxima

**Proved.** For \(k\) distinct positive sample levels, at most \(3^k-2\) positive stationary points occur, counted with multiplicity, and at most \((3^k-1)/2\) of them are local maxima. A potentially smaller bound replaces \(3^k\) by the number of distinct signed sums specified below.

The physical and normalized coordinates have the same root count. In normalized units eliminate \(s\) with
\[
 F(a)=a(S(a)^2-1)+S(a)
     =(S(a)-s(a))[1+a(S(a)+s(a))].
\]
The second factor is positive for \(a>0\), so it preserves signs and multiplicities. If \(\lambda_j\) are the distinct positive normalized levels, multiply \(F\) by \(\prod_j\cosh^2(a\lambda_j)\). This clears the denominators of the terms in \(S\) and \(S^2\). The resulting exponential polynomial is a sum of linear polynomials times \(\exp(a\eta)\), where \(\eta\) ranges over \(2\sum_j e_j\lambda_j\), \(e_j\in\{-1,0,1\}\). Each cleared factor is a linear combination of \(\exp(-2a\lambda_j),1,\exp(2a\lambda_j)\). There are at most \(3^k\) distinct exponents. The polynomial is odd, is nonzero by the strict tail score sign, and vanishes to at least order three at zero since \(S(a)=a+O(a^3)\).

A nonzero sum of linear polynomials times \(J\) distinct real exponentials has at most \(2J-1\) real zeros counted with multiplicity. For \(J=1\) this is the linear polynomial bound. Otherwise divide by the first exponential and differentiate twice, eliminating its polynomial. Each remaining term retains a distinct real exponent and degree at most one. The operator on its polynomial is \((d/da+\eta-\eta_1)^2\), which is invertible on polynomials of degree at most one for \(\eta\ne\eta_1\). Distinct real exponentials with polynomial factors are linearly independent, as is seen by dividing by the largest exponential and taking the limit as the argument tends to positive infinity. By induction the differentiated sum has at most \(2J-3\) zeros. Rolle’s theorem with multiplicities gives the desired bound on the original sum, applied on any finite interval containing the zeros under consideration. Subtract the zero at the origin and pair the positive and negative roots by oddness. Between any two local maxima there is at least one more stationary point, since the profile is nonconstant and analytic. This gives the maxima bound.

**Proved lower example.** The 14 exact decimal observations in the [replay](2026-11-25-foldnorm-math-data/replay.py), taken from the prior regression, have score signs \(+,-,+,-\) at physical \(a=1,3/2,5/2,4\). The script verifies these signs with rational arithmetic. It bounds \(\exp(y)\) for \(y=2ax_i\) between its Taylor polynomial of degree 40 and that polynomial plus the first omitted term divided by \(1-y/42\). Successive omitted term ratios are at most \(y/42<1\). The map \(E\mapsto(E-1)/(E+1)\) is increasing, as is \(S\mapsto a(S^2-m_2)+S\) on \(S\ge0\). This yields rigorous signs and proves at least two interior maxima. The numerical cover below counts exactly two for this sample, conditional on its computed comparisons.

## Exact and near ties

**Proved, unique real transition in the prior family.** The [November analysis](2026-11-25-frechet-math-report.md#other-masks-and-the-half-normal-face) proves existence of a real tie for 99 copies of \(\epsilon_*\) and one \(1\). Equivalently write the sample as 99 ones and one \(c_*=1/\epsilon_*\). For any fixed normalized \(a>0\), the profile gap is strictly decreasing in \(c>1\). To see this, set \(p=99/100\), \(q=1/100\), \(l=1/(p+qc^2)\) and \(h=c^2/(p+qc^2)\). Then \(pl+qh=1\), \(h'>0\), and the data term in \(\Delta\) is \(p g(l)+q g(h)\) with \(g(u)=\log\cosh(a\sqrt u)\). Here \(g'\) strictly decreases because \(\tanh t/t\) strictly decreases for \(t>0\). Indeed \(\tanh t-t\,\mathrm{sech}^2 t\) vanishes at zero and has positive derivative for \(t>0\). The data term has derivative \(qh'[g'(h)-g'(l)]<0\). At the prior real tie there is a positive maximizer. Below \(c_*\) its fixed normalized coordinate beats the half-normal, and above \(c_*\) every positive normalized coordinate loses strictly. This identifies the unique real crossover.

**Proved conditional on Schanuel’s conjecture.** No nonconstant rational sample has a positive stationary point at the same likelihood as the half-normal. Here use physical \(a,\mu\) and write \(v=\sigma^2\). Let \(x_i=d k_i\) with \(d>0\) rational and \(k_i\) nonnegative integers. Put \(Y=\exp(2ad)\). At a positive stationary point,
\[
 \mu=\overline{x_i(Y^{k_i}-1)/(Y^{k_i}+1)},\qquad
 v=m_2-\mu^2,\qquad a=\mu/v.
\]
Thus \(\mu,v,a\) are rational functions of \(Y\) over the rationals. The function for \(\mu\) is nonconstant, since it increases strictly along \(Y>1\). In fact \(\mu\) cannot be algebraic, unconditionally. Otherwise the nonconstant rational equation makes \(Y\) algebraic, and \(a=\mu/(m_2-\mu^2)\) is nonzero algebraic, contradicting the Hermite–Lindemann theorem.

Equality of the profile values gives
\[
  \exp(2na\mu)=(m_2/v)^n\prod_i\cosh^2(ax_i).
\]
The right side is rational in \(Y\), since \(\cosh^2(ax_i)=(Y^{k_i}+2+Y^{-k_i})/4\). Take \(u=2ad\) and \(w=2na\mu\). Their ratio \(w/u=n\mu/d\) is irrational by the preceding argument. Hence \(u,w\) are linearly independent over \(\mathbb Q\). The field generated by \(u,w,e^u,e^w\) is contained in \(\mathbb Q(Y)\), with transcendence degree at most one. Schanuel’s conjecture requires degree at least two. This is a conditional obstruction to a binary64 construction, since binary64 observations are rational. The argument uses the number-field statement of the conjecture, which is distinct from its proved differential version. [Ax, *On Schanuel’s conjectures*](https://annals.math.princeton.edu/1971/93-2/p03) · [Bourbaki account of the conjecture](https://www.numdam.org/item/SB_1970-1971__13__1_0/)

**Numerical.** The 85-digit replay gives \(c_*\approx 7.1985107804358752710820316621\) and physical \(a_*\approx2.60934088260058666164\). The immediate binary64 neighbors have one positive local minimum and one positive local maximum each. At the maximum they give the following gaps per observation.

| Exact binary64 outlier | Interior minus half-normal | Winner |
| --- | ---: | --- |
| `0x1.ccb4668f78088p+2` | \(+5.16424298741619\times10^{-17}\) | Positive location |
| `0x1.ccb4668f78089p+2` | \(-4.14752825087555\times10^{-17}\) | Half-normal |

The interior pair is near \((1.051322207001814,0.6347497078576325)\), whereas the half-normal has zero location. An objective tolerance alone can therefore admit a materially different coefficient pair.

## Golden and replay

**Numerical.** The checked-in foldnorm golden contains one joint-fit sample of 500 observations. Interpreting the JSON observations as binary64, \(A=1.68242687469\) and \(M^2=0.81043458507\), so both the quartic and the normal witness tests exclude the half-normal. The normal witness lower bound on the improvement is \(0.138363377689\) per observation. The full cover finds one positive stationary point, a maximum at physical \(a=1.91897319892153864115\), with
\[
(\mu,\sigma)=(2.02485325283146576137,\ 1.02721729384285371474),\qquad
\ell=-688.19521052874873331.
\]
The improvement is \(0.169404476758\) per observation. Both coefficients and total log likelihood agree with the stored values within relative \(10^{-10}\).

The numerical cover starts at normalized \(a=0.01\). For negative initial signs it uses the local \(Q\) bound above. For positive initial signs it uses \(\tanh t>t-t^3/3\) for \(t>0\), which follows by integration of \(1-\tanh^2 t>1-t^2\). This gives \(S-s>0\) when \(3(1-b)^2\ge A\). From \(M/(1-M^2)\) onward the score is negative. Within that range, bisected cells \([l,h]\) use \([S(l)-s(h),S(h)-s(l)]\) to exclude zero, or \([S'(h)-s'(l),S'(l)-s'(h)]\) to certify strict monotonicity and use the endpoint signs. Here \(S'\) is an average of decreasing squared hyperbolic secants, and \(s'=v/\sqrt{1+4a^2}\) decreases. These are exact-arithmetic bounds evaluated at 85 decimal digits with a \(10^{-30}\) guard on cell decisions, without outward rounding. The golden used 63 processed cells, the prior two-maxima sample 4,277, and each binary64 tie neighbor 57. The cover of the two-maxima sample gives physical roots \(1.32116653770428,\ 2.01842452929910,\ 3.84304939708637\), with derivatives crossing down, up, down. The last root has the largest likelihood.

The final replay exited green on the workstation via one CPU slot. Run from the worktree root.

~~~sh
/tmp/umvl-cpu/run nice -n 10 python3 -u dev/research/evaluations/2026-11-25-foldnorm-math-data/replay.py
~~~

## What this changes for the fitter

- After the constant check, the two endpoint inequalities for \(Q\) allow an early certified, unique \(\mathrm{Ok}\) estimate at \((0,\sqrt{m_2})\), subject to the coefficient reconstruction check. Use a practical margin for inexact moment comparisons. An exact quartic equality may use the weak inequality if the other comparison also closes. A rounded sign inside its uncertainty margin is unresolved.
- A separated \(A<3\), an established \(A=3,B>15\), or a separated feasible likelihood witness certifies that the half-normal loses. The ordinary normal candidate is an inexpensive witness when \(M^2\) is clearly at least \(3/4\). Still isolate and globally compare positive branches before certifying a coefficient pair. The exponential root bound is a finiteness result. It gives no practical enumeration budget and permits multiple interior maxima.
- For competing branches, use separated likelihood bounds to establish a winner and score brackets to establish coefficient accuracy. The endpoint \(\Delta(0)=0\) can be compared directly with a positive branch’s likelihood enclosure. A tie preference based on a likelihood tolerance cannot certify the preferred coefficients. If comparison remains unresolved after the certificate allowance and the search succeeded, retain its checked result marked uncertified. Reject a retained candidate if the bounds refute it, and retain the existing failure outcomes when the search itself fails. If distinct global maximizers are proved equal, the target outcome reports all of them with the nonunique flag. A near-zero computed gap provides no proof of equality.

## Open

- Unconditional existence or exclusion of an exact half-normal/interior tie on binary64 data remains open. For a rational sample, the missing step is an unconditional obstruction to the two dependent exponential relations above, or an explicit dyadic sample with proved equality and global domination. The neighboring inputs locate the transition numerically but do not prove irrationality of this particular \(c_*\).
- The sharp root bound is open, including whether a bound independent of the number of positive levels holds. The exponential-polynomial argument establishes a bound for each support size and the rational sign example requires any uniform bound to allow at least two maxima.
- The moment tests are sufficient. A general cheap criterion that also closes every remaining face comparison would need a sharper uniform bound or a complete signed comparison over the compact positive profile range.
