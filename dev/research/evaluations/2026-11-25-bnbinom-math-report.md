# Beta-negative-binomial canonical roots, label orbits, and face equality

TASK-291 · base `ba3aca13` · all three shapes free.

- **Proved.** The size/beta swap is the entire ambiguity of the law. Each canonical maximizing point gives one labeled maximum on the diagonal or two off the diagonal. The frozen 500-count sample has two distinct canonical **score roots**, one of them a saddle. The archived global cover plus a new rational contraction proves that this sample has one canonical **maximizer** and exactly two labeled maxima, conditional on the archived cover's global comparisons.
- **Proved.** At a finite fitted NB face, the first-order inward coefficient cannot vanish for an integer sample. At a Poisson face with sample variance equal to the mean, a uniform second-order bound makes the Poisson corner strictly worse at nearby finite parameters. Together with the prior exterior bounds, these give a compact per-input finite-versus-face test, including a finite tie to the best face.
- **Open.** Universal uniqueness of the canonical maximizer. The score-root counterexample establishes a separate fact. The numerical search did not find a second canonical local maximum.

## Canonical parameters and the exact orbit test

Write \((r,a,b)=(\mathrm{size},\alpha,\beta)\). The [original analysis](2026-10-29-bnbinom.md) expands the beta mixture to
\[
p_x=\frac{\Gamma(r+x)\Gamma(b+x)\Gamma(a+r)\Gamma(a+b)}
{\Gamma(r)\Gamma(b)\Gamma(x+1)\Gamma(a+r+b+x)\Gamma(a)}.
\]
Thus \((r,a,b)\mapsto(b,a,r)\) preserves each mass and hence every sample likelihood. The canonical region is \(0<r\le b\), \(a>0\), including the seam \(r=b\).

**Proved identification and fixed-point test.** Put \(s=r+b,\ q=rb,\ t=a+s\), and let \(c_j=(j+1)p_{j+1}/p_j\). The recurrence gives
\[
c_j=\frac{j^2+sj+q}{j+t}
=j-a+\frac{(a+r)(a+b)}{j+t}.
\]
The second difference \(c_2-2c_1+c_0\) is strictly positive, since the last term has second difference \(2(a+r)(a+b)/[t(t+1)(t+2)]\). The first three recurrence ratios determine the invariants:
\[
t=\frac{2-2(c_2-c_1)}{c_2-2c_1+c_0},\qquad
q=tc_0,\qquad s=(1+t)c_1-1-q,\qquad a=t-s .
\]
The unordered pair consists of the roots of \(z^2-sz+q=0\). This proves that the swap exhausts the label ambiguity. The exact discriminant \(s^2-4q=(b-r)^2\) distinguishes the two orbits. With one canonical winner, a zero discriminant gives the singleton \(\{(s/2,a,s/2)\}\), and a positive discriminant gives \(\{(r,a,b),(b,a,r)\}\). In general the full maximizer set is the union of these orbits over all canonical winners.

**Proved certificate tests for a fitted point.** Disjoint size and beta intervals certify a positive discriminant. To certify a fixed point, enclose the unique full score root in a swap-invariant box and prove existence in that box. Reflection gives a second root in the same box, so uniqueness forces them to coincide. Alternatively prove existence of a seam root by isolating \((S_r(s/2,a,s/2),S_a(s/2,a,s/2))=(0,0)\) in two dimensions and show it lies in the full unique-root neighborhood. Symmetry supplies \(S_b=S_r\) exactly. Either test can use a certified global maximizer for the full-root existence step. Rounded equality of the shapes or a discriminant interval containing zero supplies no equality proof.

For a seam point written \(r=b=d\), the curvature in the antisymmetric direction, divided by the squared norm of \((1,0,-1)\), is
\[
H_{rr}-H_{rb}=\psi_1(a+d)-\sum_{j\ge0}\frac{w_j}{(d+j)^2},
\qquad w_j=n^{-1}\#\{i:x_i>j\},
\]
where \(H\) is the average log-likelihood Hessian. It must be nonpositive at a seam maximum. A positive bound rejects the seam point even if the two restricted scores vanish. In local coordinates \(r=d-u,\ b=d+u\), the likelihood is analytic and even in \(u\). The displayed curvature is the derivative with respect to \(u^2\) at zero. A strictly negative value plus a negative-definite restricted Hessian certifies a strict local maximum. Global coverage is still required.

## What can be proved about canonical uniqueness

The [uniqueness evaluation](2026-11-25-uniq-cover-math-report.md) identifies the gap in the existing cover. A small convex box containing all canonical global maxima does not by itself count its score roots. The following additional test closes the gap on a per-input basis.

**Proved local counting criterion.** For any fixed matrix \(M\), let \(T(\theta)=\theta-MS(\theta)\), where \(S\) is the full score. If \(\sup_{B}\|I-MH\|_\infty\le k<1\) on a convex box \(B\), the segment identity shows that \(T\) is \(k\)-Lipschitz there. There is at most one stationary point in \(B\). If all canonical global maxima lie in \(B\) and attainment is proved, that point exists and is the unique canonical global maximum. If using the root test independently of attainment, a cube with center \(c\) and radius \(\rho\) is mapped strictly into itself when \(\|MS(c)\|_\infty+k\rho<\rho\). The derivative bound also implies \(M\) is invertible, so the resulting fixed point solves the score equation. Both arguments extend to nonuniform box widths by scaling the norm.

**Exact sample refuting uniqueness of canonical stationary roots.** Use the [frozen 500 observations](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json) under the `data` key. The [rational output](2026-11-25-bnbinom-math-data/golden_roots.json) records the entire histogram. The [rational check](2026-11-25-bnbinom-math-data/golden_roots.py) verifies both rows for the average score.

| Root system | Center in its solved coordinates | Cube radius | Upper bound on \(k\) | Upper bound on \(\|MS(c)\|_\infty\) |
|---|---|---:|---:|---:|
| Full | \((1.067223439275812,\,2.141979220255064,\,5.364734296253785)\) | \(2\cdot10^{-8}\) | \(4.730\cdot10^{-5}\) | \(5.723\cdot10^{-10}\) |
| Seam \((d,a)\) | \((2.099578384354076,\,1.833901265112290)\) | \(10^{-8}\) | \(4.884\cdot10^{-7}\) | \(6.653\cdot10^{-12}\) |

The seam root has \(H_{rr}-H_{rb}>0.000944508\) throughout its enclosure. The alpha curvature at that root is negative by the [scalar alpha integral proof](2026-11-06-bnbinom-cert-report.md). Indeed, differentiate the alpha score and subtract the integral of its zero score times minus the integrand crossing location. The integrand then has strictly negative sign away from the crossing. The seam root is therefore a saddle of the original likelihood. The other root lies strictly off the seam. Both roots lie in the canonical region.

The prior [all-free cover](2026-11-08-bnbinom-allfree-report.md#global-finite-proof) proves attainment and confines every canonical global maximum to a relative \(2.5\cdot10^{-9}\) box about the archived center. The new checker verifies that the entire archived box lies strictly in the full contraction cube above. Thus the full root is the unique canonical maximum and its orbit is exactly the two labeled maxima. The local comparisons and disjointness are rational proofs. The global premise uses the [archived cover record](2026-11-08-bnbinom-allfree-data/joint.json) and its conditional exact-arithmetic comparisons with practical floating-point margins.

**Rational enclosure details.** For \(z>0\), the trigamma integral and the inequalities
\[
1+t/2+t^2/12-t^4/720\ \le\ t/(1-e^{-t})\ \le\ 1+t/2+t^2/12
\]
give bounds \(z^{-1}+(2z^2)^{-1}+(6z^3)^{-1}-(30z^5)^{-1}\) and \(z^{-1}+(2z^2)^{-1}+(6z^3)^{-1}\). For the kernel inequalities, put \(y=t/2\) and multiply \(y\coth y\) and its proposed bounds \(1+y^2/3-y^4/45,\ 1+y^2/3\) by \(\sinh y\). At \(y^{2k+1}\), the upper-bound difference, multiplied by \((2k+1)!\), is \(2k(2k-2)/3\). The term subtracted to get the lower bound is that value times \((2k-1)(2k+1)/15\) for \(k\ge2\), which is at least the upper-bound difference. The lower coefficients match. Integrating the trigamma bounds using \(\psi(z)-\log z\to0\) gives digamma between \(\log z-1/(2z)-1/(12z^2)\) and that expression plus \(1/(120z^4)\). The checker shifts by 512 and uses the recurrence, monotonicity of the two special functions, a geometrically bounded log series, and rational interval endpoints rounded out to multiples of \(2^{-100}\). Floating point proposes \(M\) only. All acceptance inequalities are rational.

**Proved partial profile result.** At fixed \(s=r+b>0\) and fixed positive integer \(a\), the likelihood has a unique maximizing imbalance \(q=rb\) in \(0<q\le s^2/4\) on every sample containing a positive count. Euler's gamma product gives
\[
\log p_0=-\sum_{k\ge0}\log\left(1+\frac{q}{(a+k)(a+s+k)}\right).
\]
With \(D(j)=j(s+j)\), recurrence and termwise differentiation give the average \(q\)-score
\[
L_q=\sum_{j\ge0}\frac{w_j}{q+D(j)}-\sum_{k\ge0}\frac{1}{q+D(a+k)}.
\]
The series and derivative converge locally uniformly for \(q>0\). At integer \(a\) the terms combine, with nonnegative coefficients below \(a\) and nonpositive coefficients at and above it. Multiplying by \(q+D(a)\) gives a strictly decreasing function. Each term below \(a\) has nonpositive derivative, strictly negative for \(j=0\) because \(w_0>0\). Each term above \(a\) also has nonpositive derivative. The score starts positive at \(q\downarrow0\). It selects the seam when \(L_q(s^2/4)\ge0\), or its sole interior zero otherwise. At noninteger \(a\) the two sequences interlace, and this ordered coefficient argument no longer applies. Even the integer result leaves joint maximization in \(s,a\) unresolved.

**Numerical search.** The [mode search](2026-11-25-bnbinom-math-data/search_modes.py) used 21 distinct canonical starts per sample on 62 fixed samples. These were the golden, 27 histograms with specified frequencies at zero, one and three plus one extreme count, and 34 archived BNB, NB and Poisson samples with positive maximum at most 1024. The proposal stage used binary64 Newton on log shapes in \([-6,6]^3\), followed by 70-digit score and Hessian checks. It returned one canonical strict local mode on 41 samples and zero candidates on 21. The [output](2026-11-25-bnbinom-math-data/search_modes.json) lists the candidates and the exact input identifiers. Neither root coverage nor a comparison with faces is inferred from the search.

## Faces and the finite-versus-face test

**Proved closure, using the prior exhaustion argument.** After ordering \(r\le b\), proper boundary laws are NB with a finite positive size, Poisson with a finite positive mean, and \(\delta_0\). For the NB route \(r\) stays finite and \(a,b\) diverge at fixed \(a/(a+b)\) in \((0,1)\). The mirror is the same face. For Poisson both numerator shapes diverge with \(rb/a\) tending to its mean. All-zero data have the \(\delta_0\) supremum and no finite MLE. For positive-count data, \(\delta_0\), defective escapes with fixed positive masses vanishing, and mass escaping to infinity have likelihood tending to zero. The subsequence proof, including simultaneous boundary paths, is in the [closure analysis](2026-10-29-bnbinom.md#proved-proper-hellinger-closure).

For positive sample mean \(m\), write \(V=n^{-1}\sum_i(x_i-m)^2\) and \(F\) for the largest face log likelihood. The [NB size-profile theorem](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md#existence-and-uniqueness-proof) identifies that face. When \(V>m\), there is a unique fitted finite NB size \(r_*\) and its height strictly exceeds Poisson at \(m\). When \(V\le m\), every finite-size NB is strictly below Poisson at \(m\), which supplies \(F\).

**Proved exclusion of first-order equality at a fitted finite NB face.** With \(p_*=r_* /(r_*+m)\), the coefficient of \(1/(a+b)\) from the [prior signed bound](2026-11-09-bnbinom-face-report.md#what-is-proved) is
\[
C_*=\frac{nr_*}{2m}\left(V-m-\frac{m^2}{r_*}\right).
\]
For \(V>m\), zero would force \(r_*=m^2/(V-m)\) to be rational. The NB profiled score equation is
\[
\sum_{j\ge0}\frac{w_j}{r_*+j}=\log(1+m/r_*).
\]
Its left side would be a positive rational, while its right side is the log of a rational greater than one. For completeness, if a nonzero rational has rational exponential, raising to the power of its denominator and taking a reciprocal if needed gives \(e^k=A/B\) rational for a positive integer \(k\). Let \(f_N(t)=t^N(k-t)^N/N!\). All derivatives of \(f_N\) at both endpoints are integers by expansion about the respective endpoint. Repeated integration by parts makes \(B\int_0^k e^t f_N(t)\,dt\) an integer, and \(0\le t(k-t)\le k^2/4\) makes it positive and less than one for large \(N\). This contradiction rules out a zero \(C_*\). A positive sign proves finite attainment by the prior inward expansion. A negative sign supplies the uniform NB exterior exclusion via that report's signed remainder and NB profile deficit.

**Proved Poisson exterior exclusion at equidispersion.** Let \(u=1/r,\ v=1/b,\ h=u+v,\ \lambda=rb/a\), and \(f_j=n^{-1}\sum_i(x_i)_j^{\rm fall}\), where the superscript denotes a falling factorial. In average log likelihood the uniform expansion for \(0<h\le1\) and bounded positive \(\lambda\) is
\[
\frac{\ell-\ell_{\rm Pois}(\lambda)}n
=\frac h2\{(\lambda-m)^2+V-m\}+Q_\lambda(u,v)+O(h^3),
\]
where
\[
\begin{aligned}
Q_\lambda={}&-\frac{\lambda^2uv}{2}
-\lambda^3\left(\frac{u^2+v^2}{3}+\frac{uv}{2}\right)
-\frac{u^2+v^2}{2}\left(\frac{f_3}{3}+\frac{f_2}{2}\right)
-\frac{\lambda uv f_2}{2}
+\frac{m\lambda^2h^2}{2}.
\end{aligned}
\]
To derive it, write \(\log p_0=-\int_0^r\int_0^b\psi_1(a+y+z)\,dz\,dy\). Use the two leading trigamma terms and expand \((1+(y+z)/a)^{-1}\) through degree two. This gives
\[
\log p_0=-\lambda+\frac{\lambda^2h}{2}
-\frac{\lambda^2uv}{2}
-\lambda^3\left(\frac{u^2+v^2}{3}+\frac{uv}{2}\right)+O(h^3).
\]
The absolute remainder bound for this step is \((\lambda^4+\lambda^3/4+\lambda^3/96)h^3\). It follows from \((y+z)/a\le\lambda h\), the geometric remainder for the reciprocal, \(|(1+w)^{-2}-1|\le2w\) for \(w\ge0\), and the trigamma remainder at most \(1/(6a^3)\) before integration. The log count-ratio correction is the sum over \(0\le j<x\) of
\[
\log(1+ju)+\log(1+jv)-\log(1+\lambda h+j\lambda uv).
\]
Expanding these logs through degree two gives the other terms of \(Q_\lambda\) after averaging. In particular \(\sum_{j<x}j^2=(x)_3^{\rm fall}/3+(x)_2^{\rm fall}/2\). An absolute coefficient for the remaining \(h^3\) error in each summand is
\[
j^3/3+\lambda^3(1+j/4)^3/3+j\lambda^2/4+j^2\lambda^2/32 .
\]
This uses \(|\log(1+y)-y+y^2/2|\le y^3/3\) for \(y\ge0\), and \(uv\le h^2/4\). These bounds are uniform in the relative rate of the two diverging shapes.

At \(V=m\) we have \(f_2=m^2\). Weighted Cauchy–Schwarz gives \(m(f_3+f_2)\ge f_2^2\) by applying it to \(x-1\) with weights proportional to \(x\). Hence \(f_3\ge m^3-m^2\) and
\[
Q_m=\left(\frac{m^3-f_3}{6}-\frac{m^2}{4}\right)(u^2+v^2)-\frac{m^2uv}{2}
\le-\frac{m^2h^2}{12}.
\]
Continuity of \(Q_\lambda/h^2\) in \(\lambda\) and \(u/h\in[0,1]\) gives a strictly negative uniform bound in a neighborhood of \(m\). On that neighborhood the Poisson profile has deficit at least \(c(\lambda-m)^2\) for some \(c>0\), by its strictly negative second derivative. For sufficiently small \(h\), this deficit absorbs the positive \(h(\lambda-m)^2/2\) term, and the negative quadratic absorbs the cubic error. On bounded \(\lambda\) intervals outside the neighborhood, the strict Poisson deficit absorbs the \(O(h)\) terms for sufficiently small \(h\). Thus the entire Poisson corner is strictly below \(F\) at equidispersion after the small and large rate exclusions below. This is a local-in-parameters face result. The interior can still require a global comparison.

The [face checker](2026-11-25-bnbinom-math-data/face_expansion.py) verifies the algebra symbolically and checks 108 evaluations at 90 decimal digits. It crosses four exact samples with \(\lambda=m,m\pm0.1\), \(h=10^{-2},10^{-4},10^{-8}\), and \(u/h=0.01,0.2,0.5\). The [largest observed error](2026-11-25-bnbinom-math-data/face_expansion.json) was \(0.401\) times the proved remainder bound, rounded up. The grid is numerical corroboration.

**Proved compact per-input criterion.** For \(C_*>0\), the strict witness above \(F\) already proves a finite maximum. In the remaining cases, all points with \(\ell\ge F\) lie in an effectively bounded closed positive canonical box \(K\). To see how to construct it, use the [all-free exterior inequalities](2026-11-08-bnbinom-allfree-report.md#global-finite-proof) at height \(F\).

1. The positive-count small-shape bound keeps \(r\) away from zero. The finite-count mass bound with \(m_0=rb/(a+r+b)\) keeps \(m_0\le M\) for some finite \(M\) and rules out \(\lambda\downarrow0\), since \(m_0\le\lambda\). When \(r\le b\) are both large, \(1/m_0=1/\lambda+h\) and \(h\le1/(2M)\) imply \(\lambda\le2M\). On this remaining rate range the Poisson deficit when \(F>\ell_{\rm Pois}(m)\), the prior signed bound when \(V<m\), or the new quadratic bound when \(V=m\), excludes large \(r\) uniformly. This bounds \(r\) inside a positive compact interval.
2. On that interval the uniform pair bounds exclude \(p=a/(a+b)\) close to zero or one. The remaining NB profile is on a compact rectangle in \(r,p\). When \(V\le m\) this rectangle has a strict height deficit below \(F\), so the uniform NB remainder excludes large \(a+b\). When \(V>m\) and \(C_*<0\), use a neighborhood of the fitted NB point with a strict negative coefficient bound. On the compact complement, the strict NB height deficit beats the absolute remainder. Now \(a+b\) is bounded above, and the lower bound on \(b\) and the lower cutoff on \(p\) bound \(a\) away from zero. These comparisons give \(K\).

The cutoff construction needs strict comparisons only and may use conservative rational bounds. On this box the exact test is whether any canonical full score root has \(\ell\ge F\). A finite value strictly above \(F\) implies an attained stationary global maximum by compactness. A root equal to \(F\) implies attainment as well. Either some finite point exceeds it and attains the global maximum elsewhere, or that root itself attains the maximum. If there is no root with \(\ell\ge F\), every finite value is below \(F\), and the winning face supplies the unattained supremum. Thus when the highest finite root equals \(F\), the outcome is an attained BNB fit. The face point has no finite BNB coordinates. The exact test leaves equality of distinct heights to a proof or an unresolved comparison. A numerical interval overlap alone cannot decide it.

## Reproduction

From the worktree root, with `e=dev/research/evaluations/2026-11-25-bnbinom-math-data`, run each of the following separately. All three final runs exited zero on the workstation through the single-slot wrapper.

~~~sh
/tmp/umvl-cpu/run nice -n 10 python3 "$e/golden_roots.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$e/face_expansion.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$e/search_modes.py"
~~~

The retained JSON files are their standard output. Only this Markdown report and its Python evidence directory were added.

## What this changes for the fitter

- Keep the canonical coefficient vector in public order \((\mathrm{size},\alpha,\mathrm{beta})\). For a certified finite winner, add a root-count step to the completed global cover, using a full Jacobian contraction on the surviving convex neighborhood or another uniqueness proof. Carry the full three-coordinate enclosure at the requested relative tolerance for the canonical root and reflect that enclosure for its partner.
- For a proved single canonical orbit, return a typed set of one or two labeled BNB maximizers with the same certified likelihood interval, `uniqueness_known=true`, and `unique=true` only for cardinality one. Prove the orbit size by separated intervals or exact seam existence and uniqueness. Keep singular-diagonal inference disabled even for an exactly known singleton. A disjoint reflection of a localization box alone establishes at least two labels. It supplies no upper bound on the number of canonical roots in that box.
- If the cover isolates multiple canonical contenders, exclude strictly lower heights or prove their exact equality and global optimality before claiming a complete maximizer set. If several canonical winners are proved, return the union of their typed orbits. Do not preallocate the mathematical result to two members without canonical uniqueness evidence. When only global coefficient localization is proved, preserve that evidence with `uniqueness_known=false` and leave candidates as diagnostic witnesses.
- Compute the \(V-m\) sign from integer count moments when feasible. Use the quadratic bound at exact equidispersion and the prior strict first-order bounds elsewhere. At a finite fitted NB face, refine the sign of \(C_*\) until resolved within the allowance. Its exact sign is nonzero, though a floating-point margin can still be inconclusive. A completed exclusion of all finite competitors at height \(F\) certifies `NegativeBinomialLimit` or `PoissonLimit`. All-zero data give `PointMass`. A proved finite tie to \(F\) is a finite fit, with the same completeness requirements as any other finite fit.
- If a certificate runs out of budget, cannot isolate roots, or cannot resolve a height or orbit comparison, retain a successful accuracy-checked legacy result with the appropriate uncertified or unknown-cardinality flag. A proved-wrong fallback or a genuine search failure retains the established structured handling. A rounded tie never certifies a face or a complete maximizer set.

## Open

1. **Universal canonical maximizer uniqueness.** The exact golden sample disproves uniqueness of stationary roots on the canonical region. It has a unique canonical global winner under the completed cover. Neither fixed-sum integer-alpha uniqueness nor the bounded mode search settles the all-free maximum. What is missing is a global shape or profile theorem that excludes equal-height canonical winners, or an integer histogram with two rigorously isolated global winners and an exact equality proof.
2. **Equality and degenerate roots in a finite implementation.** The compact test classifies a finite tie exactly, but the job supplies neither an example of an exact finite-to-face tie nor a general finite-budget equality decision. Likewise the contraction criterion can fail at singular observed curvature, including a seam where the derivative in \(u^2\) vanishes. These cases need higher-order isolation or a structural inequality before completeness can be claimed.
