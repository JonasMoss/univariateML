# TASK-244 — Fixed-size beta-binomial MLE existence

**Proved.** For fixed integer size \(m\ge2\), both shapes free, and a
sample with at least one count in \(\{1,\ldots,m-1\}\), a finite MLE
exists exactly when
\[
V>m u(1-u),\qquad
u=\frac{\bar x}{m},\qquad V=\frac1n\sum_i(x_i-\bar x)^2,
\]
where \(n\) is the number of observations. If \(V\le m u(1-u)\),
**every** finite positive shape pair has likelihood strictly below the best
binomial, including at equality. There is no counterexample at any size or
sample size. The proof below resolves the necessity conjecture in
[TASK-117](2026-10-31-bbinom-existence-report.md).

With \(S=\sum_i x_i\) and \(Q=\sum_i x_i^2\), the exact sign test uses
\[
\Delta=mn Q-(m-1)S^2-mn S
      = mn^2\{V-mu(1-u)\}.
\]
The earlier [existence proof](2026-10-31-open-existence-report.md#1-beta-binomial-fixed-size-exact-algebraic-decision)
gives sufficiency from the positive boundary score in
\(s=1/(\alpha+\beta)\) at \(p=\alpha/(\alpha+\beta)=u\), and compactness
once a finite point beats the binomial face. Its endpoint exceptions still
apply. A sample containing only zeros or only \(m\)'s has a point-mass
supremum. A sample using both endpoints and no interior count has a
two-point supremum, despite strict overdispersion. For \(m=0\) all
positive shapes are equivalent. For \(m=1\) with both counts present,
every positive shape pair with ratio \(\alpha/(\alpha+\beta)=u\) fits the
optimal Bernoulli. A constant endpoint sample at \(m=1\) still escapes to a
point mass.

## Global necessity proof

Write \(X\) for a uniformly selected observation, \(Y=m-X\), and
\(v=1-u\). Assume \(0<u<1\) and \(V\le m u v\). The proof bounds the
empirical averages of the two numerator rising factorials separately,
using their respective first and second moments.

For \(a>0\) and real \(z\ge1\), define
\[
F_a(z)=\log a+\int_0^\infty \frac{e^{-at}}{t}\,[1-K_z(t)]\,dt,\qquad
K_z(t)=\frac{1-e^{-zt}}{z(1-e^{-t})}.
\]
The integral converges since \(1-K_z(t)=O(t)\) at zero and is bounded at
infinity. At an integer \(k\ge1\), \(K_k(t)=k^{-1}\sum_{j=0}^{k-1}e^{-jt}\).
Integrating each difference gives \(F_a(k)=k^{-1}\log(a)_k\), where
\((a)_k=\prod_{j=0}^{k-1}(a+j)\) and \((a)_0=1\). In fact,
\(\int_0^\infty e^{-at}(1-e^{-jt})\,dt/t=\int_0^j dq/(a+q)\)
by writing the integrand as \(\int_0^j e^{-(a+q)t}\,dq\) and exchanging
nonnegative integrals. The same interpolation is
\([\log\Gamma(a+z)-\log\Gamma(a)]/z\) by integration of the
[digamma integral](https://dlmf.nist.gov/5.9.E16), used for an independent
numerical check of the scaling inequality below.

For each \(t>0\), write
\[
K_z(t)=\frac{t}{1-e^{-t}}\int_0^1 e^{-ztw}\,dw.
\]
This is strictly decreasing and strictly convex in \(z\). Integrating the
corresponding pointwise inequalities proves that \(F_a\) is strictly
increasing and strictly concave. Under weights proportional to \(X\)
on the positive observations, the mean count is \(E[X^2]/E[X]\). Jensen's
inequality under these weights, then monotonicity, give
\[
\frac{E[\log(a)_X]}{m u}
 \le F_a\!\left(\frac{E[X^2]}{E[X]}\right)
 = F_a\!\left(m u+\frac{V}{m u}\right)
 \le F_a(1+(m-1)u).
\]
The same argument for \(Y\) bounds \(E[\log(b)_Y]/(mv)\) by
\(F_b(1+(m-1)v)\).

The inequality that makes both bounds strict relative to the binomial
is, for \(0<w<1\),
\[
F_a(1+(m-1)w)<\log w+F_{a/w}(m).
\]
To prove it, put \(L=m-1>0\), \(d=1/w>1\), and \(z=1+L/d\). Change
variables to express \(F_a(z)-\log a\) as the integral with weight
\(e^{-(a/w)t}/t\) and kernel \(1-K_z(dt)\). For fixed \(t>0\),
\[
K_{1+L/d}(dt)
=e^{-Lt/2}\frac{\sinh((L+d)t/2)/((L+d)t/2)}
                   {\sinh(dt/2)/(dt/2)}.
\]
This kernel increases strictly in \(d\). Indeed, if
\(h(r)=\log(\sinh(rt/2)/(rt/2))\), then
\[
h''(r)=\frac1{r^2}-\frac{(t/2)^2}{\sinh^2(rt/2)}>0
\quad(r>0)
\]
by \(\sinh y>y\) for \(y>0\). Thus \(h(L+d)-h(d)\) increases strictly
in \(d\). At \(d=1\) the kernel is \(K_m(t)\), so the integral at
\(d>1\) is strictly smaller than \(F_{a/w}(m)-\log(a/w)\).
This proves the scaling inequality.

Apply that inequality with \(w=u,v\), respectively. For the final
comparison, \(c\mapsto F_c(m)=m^{-1}\sum_{j=0}^{m-1}\log(c+j)\) is
concave, so
\[
uF_{\alpha/u}(m)+vF_{\beta/v}(m)
  \le F_{\alpha+\beta}(m).
\]
Consequently, after cancelling the binomial coefficients, the average
log likelihood of every finite positive shape pair satisfies
\[
\begin{aligned}
E[\log(\alpha)_X+\log(\beta)_Y]-\log(\alpha+\beta)_m
&<m\{u\log u+v\log v\\
&\quad+uF_{\alpha/u}(m)+vF_{\beta/v}(m)-F_{\alpha+\beta}(m)\}\\
&\le m\{u\log u+v\log v\}.
\end{aligned}
\]
The last expression is the reduced average log likelihood of the best
binomial. It is approached along
\((\alpha,\beta)=(tu,tv)\) as \(t\to\infty\). The strict comparison rules
out a finite tie even at exact equidispersion.

## Profile curvature at equality

**Proved.** At \(\Delta=0\) and \(0<u<1\), the profiled log likelihood
per observation has second derivative at \(s=0\) at most
\(-m(m-1)/6\). The global comparison above already settles existence.
The curvature gives a quantitative local bound.

Use \(A=m(m-1)\) and \(T_Z=E[Z(Z-1)(Z-2)]\) for \(Z=X,Y\).
Equidispersion gives \(E[X(X-1)]=Au^2\) and
\(E[Y(Y-1)]=Av^2\). Denote the reduced average log likelihood by
\(\ell(p,s)\) in the rising-factorial coordinates of the prior reports.
At \((u,0)\) its mixed derivative vanishes, since
\[
\ell_{ps}=-\frac{E[X(X-1)]}{2u^2}+
           \frac{E[Y(Y-1)]}{2v^2}=0.
\]
The mean score has a smooth root near \(s=0\), by the implicit function
theorem and \(\ell_{pp}(u,0)=-m/(uv)<0\). This root maximizes in \(p\)
for small \(s\ge0\), because the sum of log numerator factors is
strictly concave in \(p\). Thus the profile curvature is \(\ell_{ss}\).
Using \(\sum_{j=0}^{k-1}j^2=k(k-1)/2+k(k-1)(k-2)/3\) gives
\[
\ell_{ss}=\sum_{j=0}^{m-1}j^2-A
          -\frac13\left(\frac{T_X}{u^2}+\frac{T_Y}{v^2}\right).
\]
Cauchy–Schwarz under weights proportional to \(X\) gives
\(E[X(X-1)^2]\ge E[X(X-1)]^2/E[X]\). Subtracting
\(E[X(X-1)]\) yields \(T_X/u^2\ge A((m-1)u-1)\), and reflection gives
\(T_Y/v^2\ge A((m-1)v-1)\). Substitution proves the stated bound.
Since the face score is zero at equality, the quadratic coefficient of
the profile per observation is at most \(-A/12\).

## Held shapes

**Proved, fixed size.** With one positive shape held, the existing
classification applies for \(m\ge1\). When the free shape tends to zero
or infinity the model approaches one of the two endpoint point masses.
Unless the sample is constant at an endpoint, both limits give
likelihood zero, whereas every finite free shape gives positive
likelihood. Continuity then gives a finite MLE. For an endpoint-constant
sample the corresponding point mass gives the unattained supremum one.
With both shapes held there is no shape to optimize. At size zero the
model is a point mass for every allowed shape value.

The dispersion test between the two-free-shape fit and the binomial
therefore has no routing role for a held shape. For example, an
interior-constant sample is strictly underdispersed and admits a finite
held-shape MLE. Nothing changes in the classifications for unknown size
in the [size](2026-11-01-bbinom-size-report.md),
[held-alpha](2026-11-02-bbinom-alpha-report.md), and
[held-beta](2026-11-02-bbinom-beta-report.md) reports. In particular,
reflecting \(X\) to \(m-X\) is valid within each fixed size and changes
the sample across different candidate sizes.

## Numerical validation

The deterministic [check script](2026-11-25-bbinom-math-data/check.py)
passed on the workstation in one CPU slot with mpmath 1.3.0 at 80 decimal
digits.

- The scaling inequality held at all 180 combinations of trial sizes
  \(2,3,4,10,100,1000\), weights
  \(10^{-6},0.01,0.2,0.5,0.9,1-10^{-6}\), and shapes
  \(10^{-12},10^{-3},1,10^3,10^{12}\).
- Direct rising-factorial likelihoods were strictly below the face in
  all 157,675 comparisons. This enumerated every count-frequency table
  with trial size \(2\le m\le8\), sample size \(1\le n\le8\), an interior
  count, and exact \(\Delta\le0\). There were 6,118 strictly
  underdispersed and 189 equidispersed tables, each compared at all
  pairs of shapes in \(\{10^{-8},10^{-3},1,10^3,10^8\}\).
  The largest log-likelihood differences per observation were
  \(-7.14285883929\times10^{-10}\) and \(-1.2499999875\times10^{-17}\),
  respectively.
- At equality, 135 rational tables from binomial frequencies and
  moment-preserving four-cell perturbations passed the exact curvature
  bound. Direct high-precision numerical differentiation at \(p=u\)
  agreed within \(10^{-65}\). Trial sizes were \(2,3,4,10,30\) and
  means used every interior fraction with denominators \(2,3,7\).

The finite grids corroborate the proof. The strict comparisons over
the full parameter space follow from the integral and Jensen
inequalities.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
/tmp/umvl-cpu/run nice -n 10 python3 dev/research/evaluations/2026-11-25-bbinom-math-data/check.py
```

## What this changes for the fitter

- After the existing size-zero, size-one and endpoint-only routes,
  classify the fixed-size both-free fit by the exact sign of \(\Delta\).
  For \(\Delta\le0\), return proved `BinomialLimit` with the
  `mlbinom` diagnostic. An exact zero takes the same route as a strictly
  negative sign. The numerical near-face and global exclusion searches
  are unnecessary for this stop when the sign is established.
- Compute the sign with exact integer accumulation and overflow-safe
  products, or use a bounded-error sign and an exact fallback. A rounded
  near-zero value alone does not establish equality. If the sign is
  unresolved, retain the existing certificate and successful uncertified
  search fallback.
- For \(\Delta>0\), finite attainment and strict separation of the true
  maximizer from the binomial face are proved. A sign check alone gives
  neither an enclosure of the shapes nor a certificate for a returned
  candidate. Keep the global interior search and coefficient-inclusion
  checks, and retain the successful uncertified estimate if those checks
  cannot close. The face remains useful as a lower bound and as a way to
  refute a candidate shown strictly below it.
- Preserve the fixed-size held-shape routes. At an individual candidate
  size, a proved sign can classify the binomial face if both shapes are
  free. Ordering different sizes and bounding the integer tail require
  separate results.

## Open

Fixed-size existence is settled, including equality. For \(m\ge3\) and
\(\Delta>0\), uniqueness of the finite maximizer and a general
single-crossing theorem for the dispersion profile remain open. The proof
controls the likelihood relative to the binomial in the nonpositive-sign
regime and gives only local information at equality about the profile
derivatives. Settling uniqueness would require control of the profiled
score throughout the positive-sign regime or a certified example with
distinct global maximizers. Free-size tail and face comparisons remain
separate problems.
