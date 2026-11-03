# Burr XII likelihood bounds (TASK-138)

These are self-contained exact-arithmetic results for a finite positive,
nonconstant sample. They concern sample likelihood, not merely weak convergence.
Numerical corroboration and its tolerances are recorded separately.

Write `n` for sample size, `y_i=log(x_i)`, `J=sum y_i`, and
`sp(z)=log(1+exp(z))`. The Burr shape names are `k=shape1`, `c=shape2`.
In coordinates `c>0`, `b=-c log(s)`, and `z_i=c y_i+b`, the log likelihood is

\[
 \ell(k,c,b)=n\log k+n\log c+\sum_i z_i
                 -(k+1)\sum_i\operatorname{sp}(z_i)-J.
\]

Let `F(k)=max_(c,b) ell(k,c,b)`; the maximum exists and is unique for each
finite positive `k`, as proved below. Let `P` and `W` denote the maximized
Pareto and Weibull sample log likelihoods, including their Jacobians.

The nonconstant hypothesis is essential. If all observations equal x0,
holding `k=1,s=x0` gives density `c/(4*x0)` at every observation, which
diverges with c. Thus the all-free constant-sample likelihood is unbounded.

## 1. Fixed-k concavity and compactness

The Hessian quadratic form in a nonzero direction `(u,v)` is

\[
 -n u^2/c^2-(k+1)\sum_i p_i(1-p_i)(u y_i+v)^2<0,
 \qquad p_i=(1+e^{-z_i})^{-1}.
\]

If `u` is nonzero, the first term is strictly negative. Otherwise the
second is strictly negative. Thus the objective is strictly concave.
For every real `z`,

\[
 z-(k+1)\operatorname{sp}(z)\le-\min(1,k)|z|.
\]

If `R=max y-min y>0`, the two extreme observations give
`sum |c y_i+b| >= c R`. On any fixed compact k-interval `[a,d]` inside
`(0,infinity)`,

\[
 \ell\le n\log d+n\log c-\min(1,a)cR-J.
\]

Consequently every nonempty fixed likelihood superlevel has `c` bounded
above and bounded away from zero, uniformly over that k-interval. With
those bounds, the earlier absolute-value inequality also bounds `b`.
This proves existence, uniqueness at fixed k, uniform compactness of
superlevels, and continuity of `F`. It does not prove concavity of `F`.

The scale score is `sum p_i=n/(k+1)`. For a fixed `c` it has a unique
root. Its bracket is
`b in [-log k-c max y, -log k-c min y]`.
After profiling b, the strictly concave c-profile has a strictly decreasing
score, positive near zero and negative eventually by coercivity. A bracket
can therefore be expanded until it encloses its unique zero. A finite
expansion budget must stop with failure when it does not find the bracket.

## 2. An entire small-k region lies strictly below Pareto

Put `v=min y`, `d_i=y_i-v`, `D=sum d_i>0`, and let `m<n` count the
observations at `v`. Set `q=kc>0` and `t=c(v-log s)`. Using
`sp(z)=z+sp(-z)` gives the exact identity

\[
 \ell=n\log q-J-qD-nkt
       -(k+1)\sum_i\operatorname{sp}(-t-qd_i/k).
\]

Retain only the `m` terms at the minimum. If `0<k<m/(n-m)`, write
`A=nk` and `B=m(k+1)>A`. Then

\[
 \ell\le P+C(k),\qquad
 P=n\log(n/D)-n-J,
\]
\[
 C(k)=A\log(A/B)+(B-A)\log(1-A/B)<0.
\]

Indeed `n log q-qD` is maximized at `q=n/D`; maximizing
`-At-B sp(-t)` gives `exp(t)=(B-A)/A` and the displayed entropy expression.
At `k=m/(n-m)`, `A=B` and the retained penalty is
`-A sp(t)<0` for every finite t. Thus **every finite fit with
`k<=m/(n-m)` is strictly worse than P**. For the endpoint of this region
the supremum bound is `P`; uniform strict separation is not asserted there.

The limit `F(k)->P` as `k->0` follows from this upper bound and a feasible
path: choose `q=n/D`, `t=log(1/k)`. Then `kt->0`, each minimum penalty
vanishes, and all nonminimum penalties vanish as well. Also
`C(k)=nk log k+O(k)` for fixed data. This is a proved necessary condition
on the tail shape for a finite global MLE, not a sample existence criterion.

## 3. A uniform large-k upper bound against Weibull

For fixed c, use `t_i=k exp(z_i)>0`. The likelihood becomes

\[
 \ell=n\log c+\sum_i\log t_i
                 -(k+1)\sum_i\log(1+t_i/k)-J.
\]

At its maximizing intercept,

\[
 \sum_i\frac{t_i}{1+t_i/k}=\frac{nk}{k+1}.
\]

For `k>n-1`, each summand is no greater than the right-hand side,
so `t_i<=T(k)=nk/(k+1-n)`. Since
`t_i <= (1+T/k)t_i/(1+t_i/k)`, also `sum t_i<=T`.
Thus `sum t_i^2<=T^2`. The inequality
`log(1+u)>=u-u^2/2` for `u>=0` follows by differentiating the difference
(its derivative is `u^2/(1+u)>=0`). Consequently

\[
 \ell-\ell_{\rm Weibull}(c,t)
 \le-\frac{\sum t_i}{k}+\frac{k+1}{2k^2}\sum t_i^2
 \le E(k):=\frac{k+1}{2k^2}T(k)^2
           =\frac{n^2(k+1)}{2(k+1-n)^2}.
\]

The Weibull comparison uses the same c and positive t-scale, hence is
no greater than `W`. Profiling gives **`F(k)<=W+E(k)` for `k>n-1`**.
`E` is decreasing there and is `n^2/(2k)+O(k^-2)`.
This bound holds over every c and maximizing intercept, not a fixed-shape
path or a moment-matched neighborhood. A feasible path through the fitted
Weibull, holding its `c,t_i` fixed, proves `liminf F(k)>=W`; therefore
`F(k)->W` uniformly in the needed profile-upper-bound sense.

This is a loose bound near `k=n-1`. It gives finite tail exclusion whenever
a retained candidate is strictly above W. It cannot exclude all large k
against W itself, since its error is strictly positive at every finite k.

The exact W appearing in the bound is unambiguous and has a monotone scalar
solver. After profiling Weibull scale, its c-score is
`n/c+sum y_i-n sum w_i y_i`, where
`w_i=exp(c y_i)/sum exp(c y_j)`. Its derivative is
`-n/c^2-n Var_w(y)<0`; it is positive near zero and tends to
`sum y_i-n max y_i<0` at infinity. Thus a finite bracket encloses its
unique root. Tangents to this concave scalar profile at the two bracket
ends give an upper bound for W by maximizing the smaller tangent over
the bracket. The study uses a numerical root value rather than computing
this additional upper allowance; production tail comparisons would need it.

## 4. Likelihood closure and existence

For nonconstant data, `F` is continuous, `F(0+)=P`, `F(infinity)=W`, and
has compact parameter superlevels on each interior k-interval. Thus the
only possible positive maximizing escape *values* are bounded above by
`P` or `W`; both are achievable along escaping sequences. This is exhaustive
for sample likelihood. It is not a classification of every weak limit.

In particular, a finite global maximum exists **if and only if some finite
fit has likelihood at least `max(P,W)`**. If the inequality is strict,
the two tail bounds give compact containment of a global maximum. If a
candidate equals the boundary value, either it is already a global maximum
or some better finite candidate gives the strict case. Necessity follows
because every attained global maximum dominates both feasible limit paths.
No uniqueness of the all-free MLE follows. Equality does not justify
claiming that every maximizing sequence escapes.

## 5. Fixed-k dual upper certificates

For `0<=p<=1` let `H(p)=-p log p-(1-p)log(1-p)`, with `0 log 0=0`. The tangent inequality
`sp(z)>=pz+H(p)` is the supporting-line inequality for this convex function
(equality at `p=sigmoid(z)`, with endpoint inequalities by continuity). Choose any `p_i` in `[0,1]` satisfying

\[
 \sum_i p_i=n/(k+1),\qquad
 A=\sum_i((k+1)p_i-1)y_i>0.
\]

The intercept cancels from the tangent upper bound, and maximizing its
remaining c dependence gives

\[
 F(k)\le U(k,p)=n\log k+n\log(n/A)-n
                   -(k+1)\sum_i H(p_i)-J.
\]

At the exact fixed-k solution, these p's satisfy `A=n/c` by the c-score,
and equality holds. Thus a numerical optimizer need not be trusted to
provide an upper bound: any feasible dual vector provides one. Centering
the y's leaves A unchanged by the equality constraint. A numerical
near-equality of the p sum is **not** that exact constraint; practical
floating-point treatment must be stated. The research code repairs one
component in extended precision and records residuals; this is numerical
evidence, not a production error analysis.

## 6. An upper envelope between two profiled k values

Define

\[
 G(k)=F(k)+J-n\log k
 =\sup_{c>0,b}\{n\log c+\sum z_i-(k+1)\sum\operatorname{sp}(z_i)\}.
\]

This is a supremum of affine functions of k, so it is convex. Given
upper certificates `U(a),U(d)` at `0<a<d`, linearly interpolate
`U(a)+J-n log a` and `U(d)+J-n log d`; call that line `L(k)`.
Then `F(k)<=n log k+L(k)-J` throughout `[a,d]`. Its maximum is at an
endpoint or at `k=-n/L'` if `L'<0` and that point lies in the interval.
This bounds the whole interval, irrespective of local modes or score roots.
Numerical lower candidates and these upper bounds can drive subdivision.

## 7. Weibull inward slope

In `xi=1/k`, with c and Weibull scale held,

\[
 \ell=\ell_W+\xi\sum_i(t_i^2/2-t_i)+O(\xi^2).
\]

The finite-sample Taylor expansion is valid near zero since the t's are
fixed and finite. At the uniquely fitted Weibull, `sum t_i=n`; its inward
profile slope is `sum t_i^2/2-n`. The implicit function theorem applies
to its nonsingular c/intercept Hessian, and the envelope theorem gives
the same slope for the local profiled branch. If `W>=P` and this slope
is positive, a finite fit exceeds both faces and existence follows.
A negative slope proves only a local result. At c=1 the expression
reduces to `n*(m2/(2*xbar^2)-1)` as in the job brief. The all-free result
must use the fitted Weibull power, not c=1 indiscriminately.

## 8. What this does and does not certify

Sections 2, 3, 5 and 6 give a global **likelihood-value enclosure** scheme:
retain lower candidates, obtain dual upper endpoint values, subdivide
interior intervals, and exclude the two tails against a candidate strictly
above W (the small-k region is already below P). With finite budgets this
can either bound the gap or declare the budget exhausted.

It does not certify that the retained floating-point coefficients are an
approximation to an identified global maximizing stationary point. A bound `sup ell <= ell(candidate)+epsilon` is
an epsilon-optimality certificate even in exact arithmetic. An arbitrarily
close unlocated maximum is not excluded. To return a maximizer with the
contract in the brief still needs root isolation/local maximizer enclosure
and dominance, or a theorem about all-free stationary points. No such
theorem is proved here. This does not demand an exactly representable root:
a proved root enclosure, local uniqueness, and global dominance with the
stated practical margin would suffice. Also, when the best candidate equals W, E(k)>0
does not certify a winning or tied Weibull face. Budget failure is sound;
a winning-face assertion from this bound would not be. None of these gaps
requires outward-rounded arithmetic; they exist in exact arithmetic.

In particular, strict fixed-k concavity does not extend automatically to
the remaining coordinate. At its fixed-k solution, put
`H=-ell_((c,b),(c,b))` (positive definite) and
`v=sum p_i*(y_i,1)`. Differentiation of the profile scores gives

\[
 F''(k)=-n/k^2+v^\mathsf{T}H^{-1}v.
\]

The second term is nonnegative; the displayed expression has no proved
uniform negative sign. Thus using a single root search in k, or assuming
that fixed-k uniqueness establishes all-free uniqueness, would leave a
mathematical gap. The chord bound avoids that assumption for values but
does not locate and separate every competing maximum.
