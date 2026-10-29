# Positive-power Stacy gamma: profile and closure

This note derives the formulas used by `mltrgamma` and its reciprocal.
Write `a = shape1 > 0`, `tau = shape2 > 0`, `theta = scale > 0`.
The inspected actuar 3.3-7 C source uses

\[
 U=(X/\theta)^\tau\sim\operatorname{Gamma}(a,1),\qquad
 f(x)=\frac{\tau U^a e^{-U}}{x\Gamma(a)}.
\]

Thus the coefficient names and scale agree with actuar. Gamma (`tau=1`),
Weibull (`a=1`), exponential (`a=tau=1`), and the zero-endpoint
half-normal (`a=1/2,tau=2`) are interior members.

## Proper closure

On the logarithmic axis this is the location-scale family
`Y = log(theta) + log(G_a)/tau`. For `a` in a compact subset of `(0,infinity)`,
nondegenerate tight limits require the location and scale to converge;
the limit remains in the family. The other shape regimes are:

* `a -> infinity`: `sqrt(a)*(log(G_a)-log(a))` converges to standard
  normal. A proper nondegenerate limit therefore requires
  `tau*sqrt(a) -> 1/sigma` and `log(theta)+log(a)/tau -> mu`.
  The resulting law is lognormal `(mu,sigma)`.
* `a -> 0`: `a*log(G_a)` converges to a negative unit exponential.
  If `a*tau -> d > 0` and `theta -> A > 0`, the limit has density
  `d*x^(d-1)/A^d` on `(0,A)`: the power family. Uniform is its `d=1`
  member. To see the exponential limit directly,
  `P(a log(G_a) <= z) -> exp(z)` for `z<0`, using the small-argument
  gamma integral and `Gamma(1+a) -> 1`.

These exhaust the proper nondegenerate location-scale limits: normalise
by two fixed interior quantiles; their difference fixes the scale and
one quantile fixes the location. The standardised shapes above exhaust
subsequential limits of `a` in the extended positive half-line.
Collapsed point masses are weak limits but not Hellinger limits of
Lebesgue densities. Escapes of mass give no probability density limit.
For the two stated faces, the original densities converge almost
 everywhere to a normalised density; Scheffe's lemma gives L1 and hence
Hellinger convergence. Positive powers have no Pareto face. Under
reciprocation, lognormal stays lognormal and power becomes Pareto.

At the moving power endpoint, take `theta` just above the sample maximum:
`theta=A*exp(c_tau/tau)` with `c_tau -> infinity` and `c_tau/tau -> 0`.
This avoids the spurious `exp(-1)` factor at the maximum observation.
The face likelihood is its supremum, without mlpower's default endpoint
 offset. Its fitted endpoint is `max(x)` and `d=1/(log(max(x))-mean(log(x)))`.

## Exact nuisance profile

Let `y=log(x)`, `m=mean(y)`, `s^2=mean((y-m)^2)`, `u=(y-m)/s`,
`t=tau*s`, and `K(t)=log(mean(exp(t*u)))`. For fixed positive `tau`,
`x^tau` is gamma. Differentiating the gamma likelihood gives

\[
 \log a-\psi(a)=K(t),\qquad
 \log\theta=m+\frac{K(t)-\log a}{\tau}.
\]

The shape equation has a unique positive root: its derivative is
`1/a-trigamma(a)<0`, with endpoint values infinity and zero. This is an
exact gamma fit, not an approximation to a three-dimensional optimiser.
Writing `R(a)=lgamma(a)-(a-1/2)log(a)+a-log(2*pi)/2`, the profile improvement
per observation over the fitted lognormal is

\[
 \Delta(t)=\log t+\tfrac12\log a-R(a)-aK(t)+\tfrac12,
 \qquad \Delta'(t)=1/t-aK'(t).
\]

`Delta(0)=0`, `Delta'(0+)=-mean(u^3)/6`, and
`Delta(infinity)=log(2*pi)/2-1/2-log(max(u))`, the power improvement.
The implementation uses log-sum-exp, `expm1` remainders and asymptotic
series for large gamma shapes. For fixed `a`, the score in `log(t)` is
`1-a*t*K'(t)`, strictly decreasing, so the remaining problem has one
maximum. With both shapes fixed the only unknown scale is closed form.

For checking between grid points, put
`H(K)=sup_a(a*log(a)-a-lgamma(a)-a*K)`. It is convex in `K`, with
`H'=-a`. Its secant on a cell is an upper bound. Substitution gives an
upper bound `log(t)-a_sec*K(t)+constant`, concave in `t`. Its endpoint
tangents bound its maximum; subdividing cells whose bound can beat the
incumbent prevents reliance on grid sign changes alone.

The large-t cutoff is analytic. Let `M=max(u)` and `v=t*M`.
`K(t)>=v-log(n)`. When `v>=4*(n+log(n)+10)`, the maximising shape in this
upper bound is below `1/n`, since `log(1/n)-psi(1/n)<n-log(n)+EulerGamma`.
Using `log(Gamma(1+a))>=-EulerGamma*a`, its excess over the power face is
at most

`log(z)+1-z+a*(log(a*n)+EulerGamma-1) <= 0`, with `z=a*v`.

Thus no maximum beyond the cutoff can beat that face. The small-t
implementation starts at `0.05/max(abs(u))`, uses the explicit directional
score and searches downwards when a near-lognormal maximum is bracketed.
This low-end procedure is numerical: no interval certificate for every
possible sample below the initial point is claimed. The oracle tests and
near-lognormal tests specifically exercise that region.

## Nonattainment and decisions

A positive third centred log moment makes the lognormal face locally
maximal in the positive-power direction; it is a necessary local condition
for that face to win, not a sufficient global classifier. The power
profile approaches its face from below at large t. Neither a small score
nor a converged finite optimiser determines the winning face. All located
interior maxima and both maximised face likelihoods are compared; faces
win ties at `1e-8*n`. Constant samples have unbounded free likelihood.

Positive-probability nonattainment also follows locally at the lognormal
face, without treating skewness as a global classifier. Use the smooth
coordinate `Q=1/sqrt(a)>=0`, with log location and log standard deviation
as nuisance coordinates. To first order `t=Q+O(Q^2)` at the profiled fit.
After projecting out the normal location and variance scores, the inward
score at Q=0 is `-sum(H_3(z_i))/6`, where `H_3(z)=z^3-3*z`.
Under standard normal data its mean is zero and its variance is `n/6`,
because `E(H_3(Z)^2)=6`. The profiled expected log likelihood therefore
has strictly negative second derivative `-n/6` at zero. In a sufficiently
small neighbourhood the empirical Hessian is negative with probability
tending to one, while the normalised efficient score converges to a
nondegenerate centred normal. On its negative half the constrained local
maximum is exactly Q=0. The expected log likelihood is strictly smaller
away from Q=0 (identifiability/KL inequality); uniform likelihood separation
rules out distant competitors asymptotically. This gives the usual
one-sided boundary argument: face nonattainment has limiting probability
one half under lognormal sampling, and hence positive probability for
sufficiently large finite samples. This is an asymptotic proof argument,
not a finite-n skewness classifier; it requires uniform separation of
remote parameters as well as the local expansion. The closure above
identifies the remote power candidates. Reciprocal data give the same
argument for inverse transformed gamma.

The finite-sample tests additionally exhibit both face outcomes and
strict face preferences. Global numerical search reliability is checked
against independent optimisers, with the stated small-t limitation.

For `Z=1/X`, use `-log(z)` directly, reciprocal scale, and add
`-2*sum(log(z))` to the transformed-gamma log likelihood. This avoids
forming a reciprocal that could overflow. Every face and fixed-shape
subset is transported by the same identity. `trgamma_faces_log` exposes
both maximised faces, their fitted parameters and validity independently
of the outer fit, for reuse by other families.

## Evaluation at a small gamma argument

When the latent gamma argument underflows, forming it first destroys the
power/Pareto limit. For log argument `v<-700`, the regularised lower gamma
CDF obeys `log P(exp(v)) = log P(exp(-700)) + a*(v+700) + O(exp(-700))`.
This follows from the gamma integral `P(u)=u^a/Gamma(1+a)*(1+O(u))`;
the relative correction is bounded by order `u` for every positive a.
The implementation anchors at the reference gamma log-CDF, so no gamma
normalisation is subtracted inaccurately. Inverting this identity recovers
log quantiles when the gamma quantile itself rounds to zero. Small-shape
sampling uses this recovered quantile instead of a zero gamma draw.
