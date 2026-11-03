# A quantitative Burr XII Weibull seam bound

These results are exact-arithmetic statements, conditional on the stated
comparisons. The numerical study does not turn approximate nuisance roots
into certified ones. No distributional or inference claim is made here.

Let `y_i` be finite, nonconstant log observations, `n>=2`, and omit the
constant transformation Jacobian throughout. Centering and rescaling the
log observations changes only that constant and the nuisance coordinates.
Put `theta=(c,eta)`, `r_i=(y_i,1)`, `v_i=exp(c*y_i+eta)`, and

\[
 L(\theta,\xi)=n\log c+n\eta+c\sum y_i
 -(1+\xi)\sum_i\int_0^{v_i}\frac{du}{1+\xi u},\qquad c>0,\quad\xi\ge0.
\]

For positive xi this is exactly the Burr likelihood with `k=1/xi` and
`eta=-c log(s)+log(k)`; at zero it is the Weibull likelihood. Denote its
unique conditional maximum by `H(xi)`. Let `theta_W=(c_W,eta_W)` be the
exact Weibull solution, `W=L(theta_W,0)`, and, in all quantities below,
let `v_i` without an argument mean its value at that solution.

## 1. Smooth extension and nuisance concavity

The integral representation is smooth for xi in a neighborhood of zero
on every compact positive-c nuisance set. Differentiation gives

\[
 -L_{\theta\theta}=
 \begin{pmatrix}n/c^2&0\\0&0\end{pmatrix}
 +\sum_i\frac{(1+\xi)v_i}{(1+\xi v_i)^2}r_i r_i^T.
\]

This is positive definite, including at xi=0. Thus L is strictly concave
in theta. This is the same fixed-k concavity as in the previous report;
the xi-dependent intercept translation preserves it. For positive xi,
conditional existence follows from the earlier coercivity proof; for zero
the Weibull scalar profile is coercive. Concavity in xi does not follow.

## 2. Uniform containment near the Weibull solution

Choose `0<R<c_W`, `h>0`, and use the Euclidean ball of radius R about
theta_W. Define

\[
 a_i=v_i e^{-R\|r_i\|},\qquad b_i=v_i e^{R\|r_i\|},\qquad
 M=\begin{pmatrix}n/(c_W+R)^2&0\\0&0\end{pmatrix}
       +\sum_i\frac{a_i}{(1+h b_i)^2}r_i r_i^T,
 \qquad \mu=\lambda_{\min}(M)>0.
\]

The last inequality follows since a direction with nonzero c-component
has the positive diagonal term, and a nonzero pure-intercept direction
has positive weights. On the entire ball and `0<=xi<=h`, the negative
Hessian is at least M, hence at least mu times the identity.

At theta_W the exact score identity is

\[
 g_\xi:=L_\theta(\theta_W,\xi)
 =\xi\sum_i\frac{v_i(v_i-1)}{1+\xi v_i}r_i.
\]

Set

\[
 d=\sum_i v_i(v_i-1)r_i,\quad
 E=\sum_i v_i^2|v_i-1|\|r_i\|,\quad D=\|d\|+hE.
\]

Using `1/(1+xi*v)=1-xi*v/(1+xi*v)` gives `||g_xi||<=xi*D`.
If **`h*D<mu*R`**, every conditional maximizer on `[0,h]` is inside
the ball. Indeed, the radial derivative at a boundary displacement u is

\[
 u^T L_\theta(\theta_W+u,\xi)
 \le R\|g_\xi\|-\mu R^2<0.
\]

Along each ray, concavity makes the derivative nonincreasing, so no point
outside the ball can maximize. This also supplies a direct existence
argument on this interval without any unquantified continuity assertion.
Taylor's strong-concavity inequality inside the ball now gives

\[
 H(\xi)\le L(\theta_W,\xi)+\frac{\xi^2D^2}{2\mu}.
\]

## 3. Signed upper tail, with an explicit remainder

For fixed v, differentiating the integral in section 1 twice gives

\[
 L_{\xi\xi}(\theta,\xi)
 =2\sum_i\int_0^{v_i}\frac{u(1-u)}{(1+\xi u)^3}\,du.
\]

Discard the negative part for `u>1` and bound the remaining denominator
below by one. At theta_W, put `t_i=min(v_i,1)`. Then

\[
 L_{\xi\xi}(\theta_W,\xi)\le 2B_0,\qquad
 B_0=\sum_i(t_i^2/2-t_i^3/3)\le n/6.
\]

The exact first derivative at zero is
`A=sum(v_i^2)/2-n`, since `sum v_i=n`. Integration of the derivative
bound and the nuisance improvement bound prove

\[
 \boxed{H(\xi)\le W+A\xi+B\xi^2,\qquad
 B=B_0+D^2/(2\mu),\qquad 0\le\xi\le h.}
\]

If **`A<0`, `h*D<mu*R`, and `h*B<=-A/2`**, this yields
`H(xi)<=W+A*xi/2<W` throughout `(0,h]`. Thus every finite k at least
`1/h` is strictly worse than Weibull. This is a signed exclusion, unlike
the earlier always-positive uniform bound. It is valid whether or not
Weibull is the global winner, and whether or not Pareto exceeds Weibull.

For every sample with strictly negative A, this construction terminates
in exact arithmetic by halving h for any fixed admissible R. As h tends
to zero, mu tends to a positive limit, D remains finite, and B remains
finite. The implementation tries five radii and allows 100 halvings each;
failure must be reported if that finite budget does not meet the tests.
No lower bound on the useful cutoff size is claimed.

The study uses a sharper version of the same bound. Retaining part of the
negative integral above one gives

\[
 \widetilde B_0(h)=B_0+
 \sum_{v_i>1}\frac{v_i^2/2-v_i^3/3-1/6}{(1+h v_i)^3}.
\]

For `1<=u<=v_i`, the negative numerator divided by its actual denominator
is at most that numerator divided by `(1+h*v_i)^3`, which proves this
upper bound. Keep the matrix M rather than replacing it by mu in the
quadratic improvement. The triangle inequality in the `M^{-1}` norm gives

\[
 g_\xi^T M^{-1}g_\xi\le \xi^2
 \left(\sqrt{d^T M^{-1}d}+hE/\sqrt\mu\right)^2.
\]

Thus B can be replaced everywhere by

\[
 \widetilde B=\widetilde B_0(h)+\tfrac12
 \left(\sqrt{d^T M^{-1}d}+hE/\sqrt\mu\right)^2.
\]

The containment condition still uses the Euclidean D. The sharper B can
be negative, which only strengthens the stated signed exclusion. The
halving termination proof still holds. In the joint limit `R,h ->0`,
this sharper quadratic coefficient approaches the exact reoptimized
Weibull coefficient C_W from the closure report.

## 4. What a curvature bound supplies

On the contained branch the implicit function theorem gives

\[
 H''=L_{\xi\xi}
       +L_{\xi\theta}(-L_{\theta\theta})^{-1}L_{\theta\xi}.
\]

For example, over the ball one may bound
`||L_theta,xi||` by
`D_box=sum b_i*(b_i+1)*||r_i||` and obtain the explicit upper bound
`H''<=n/3+D_box^2/mu`. This follows from
`L_theta,xi=sum v_i(v_i-1)/(1+xi*v_i)^2*r_i` and the preceding Hessian
bound. It justifies Taylor upper envelopes with an exactly bounded
endpoint derivative. It does **not** imply H'' is negative.

The exact negative-slope finite-winner counterexample in the closure report
has `H''(0)=2*C_W>0.59`. Thus even its seam neighborhood is convex rather
than concave. A negative seam slope and conditional concavity cannot
justify a monotone profile score or a single unrestricted root solve.
The new exact rational check in [exact.py](exact.py) encloses C_W in
`(0.2952035320,0.2952035333)` and proves that curvature sign. It also
instantiates the basic signed-tail theorem for the exact log data with
`R=1/50`, `h=1/8192`: all `k>=8192` have likelihood strictly below W.
Its conservative eigenvalue bound is `det(M)/trace(M)`, and it bounds
Euclidean vector norms by their one-norms. The assertions compare rational
endpoints using the earlier exact exp/log enclosures; displayed decimals
in [exact.json](exact.json) are summaries, not the arithmetic used to decide.

The signed bound uses the exact theta_W. To use a computed root instead,
one must enclose that root and propagate errors in W, A, mu and D. In
particular a nonzero nuisance score at xi=0 adds a constant error term to
the quadratic estimate, which cannot be ignored for arbitrarily small xi.
Evaluating many points or adding a likelihood pad does not repair that
particular logical gap.

## 5. Remaining certificate requirements

The new result settles an explicit signed tail for every strictly negative
first-order slope, conditional on exact nuisance constants. It leaves the
following work before a production estimator can satisfy the brief:

* A validated Weibull root enclosure, with propagated allowances rather
  than the numerical point constants used by study.py.
* Finite stationary-root enclosures and local uniqueness/maximum tests
  in each surviving region, followed by global dominance comparisons.
  A value enclosure or mesh of score sign changes does not locate every
  competing stationary point or bound coefficient error.
* Joint propagation of arithmetic/comparison margins through primal,
  dual, tail and root comparisons, including dual intercept residuals.
* Signed higher-order handling for `A=0`, if a face decision is wanted
  there. An explicit budget failure is also legitimate in that regime.

These are missing steps of this attempted estimator, not an impossibility
theorem. General root-enclosure techniques could in principle complete
them. Neither outward-rounded arithmetic nor a proof of joint global
uniqueness is a prerequisite imposed by the task.

For clarity about the coefficient requirement, suppose a future procedure
encloses the winning root in `|xi-xi_hat|<=delta<xi_hat`, and encloses its
conditional nuisances by `|c-c_hat|<=epsilon_c`,
`|eta-eta_hat|<=epsilon_eta`, with `c>=c_min>0`. Then

\[
 \left|k/\widehat k-1\right|\le
 \frac{\delta}{\widehat\xi-\delta},\qquad
 |\log k-\log\widehat k|\le
 -\log(1-\delta/\widehat\xi).
\]

Since `log s=(log k-eta)/c`, one also has

\[
 |\log s-\log\widehat s|\le
 \frac{-\log(1-\delta/\widehat\xi)+\epsilon_\eta}{c_{\min}}
 +\frac{|\log\widehat k-\widehat\eta|\epsilon_c}
             {c_{\min}\widehat c}.
\]

These provide explicit accuracy targets for a root-enclosure procedure.
At a finite stationary point the likelihood loss is quadratic in root
displacement, whereas these coefficient errors are first order. The current
value-gap tolerance alone therefore cannot establish the golden's `1e-6`
coefficient tolerance. Numerical score crossings in roots.csv supply no
proved delta or epsilon values for these inequalities.
