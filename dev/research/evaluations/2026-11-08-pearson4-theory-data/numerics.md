# Pearson IV numerical design and error accounting

This is a design plus high-precision prototypes, not a claim that binary64
implementations already meet the package accuracy contract. All sums below
must be scaled or evaluated as signed log sums; all probabilities have
separate lower/upper and log-probability paths. A cap or undecidable sign
returns numerical failure (or the caller's fitting budget stop), never an
unchecked finite approximation. The proofs are independent of mpmath's
empirical quadrature error estimates.

## 1. Real complex log gamma and the normalizer

Only Re log Γ(z) is needed; no complex phase needs unwrapping. For Re z>0
shift by a nonnegative integer J, evaluate Stirling at w=z+J, and subtract
Σ_(j=0)^(J−1) log|z+j|. With terms through B_(2N−2), use

$$\log\Gamma(w)=(w-\tfrac12)\log w-w+\tfrac12\log(2\pi)
 +\sum_{j=1}^{N-1}{B_{2j}\over2j(2j-1)w^{2j-1}}+R_N,$$
$$ |R_N|\le {|B_{2N}|\sec^{2N}(\arg(w)/2)
             \over2N(2N-1)|w|^{2N-1}}. \tag{1}$$

This is the DLMF §5.11(ii) right-half-plane bound; it also bounds the real
part error. Increase the shift if the asymptotic terms cease decreasing
before the required bound. No fixed shift threshold substitutes for (1).
Pairwise sums and scaled complex reciprocals avoid intermediate overflow;
`hypot`, `atan2`, `log1p` avoid squaring huge arguments. The prototype
shifts in blocks of 32 and searches a research cap, raising an exception
if it cannot meet the remainder target. Production evaluator exhaustion
must use a no-throw status. There is no proposed arbitrary parameter cap.

Use the duplication form A in proofs.md. Near a=2m−1=0 evaluate
log Γ(a)=log Γ(1+a)−log a, so its pole is explicit and A has the correct
log a behavior. When ν=0 bypass complex gamma and use a stable real gamma
ratio for Γ(m)/Γ(m−1/2), as the Student-t seam already does.

At large |ν| do not first form A≈−π|ν|/2 and cancel it in the density.
Compute A*=A+π|ν|/2 directly from Stirling: for w=u+iv,
−2v atan(v/u)+π|v| = 2|v| atan(u/|v|). Then

$$\log f=A^*-\log\alpha-m\log(1+z^2)
 -|\nu|[\pi/2+\operatorname{sign}(\nu)\arctan z].$$

On the favored tail compute the bracket with atan(1/|z|), not a difference
from π/2. In the normal regime, even this can subtract large m-dependent
terms. Use a combined gamma-ratio expansion or normalize the centred kernel
from equation (4) of proofs.md, whose mass tends smoothly to √(2π).
For small |v|/m the convergent identity

$$2\Re[\log\Gamma(m+iv)-\log\Gamma(m)]
 =-\sum_{j=0}^\infty\log[1+v^2/(m+j)^2] \tag{2}$$

avoids subtracting two nearly equal log gammas. The positive decreasing
summand has integral-tail brackets. If T=m+J,

$$I(T)=2|v|\arctan(|v|/T)-T\log(1+v^2/T^2),$$

then I(T)≤Σ_(j=J)^∞ log[1+v²/(m+j)²]≤I(T)+log[1+v²/T²].
Use a series for I at |v|≪T. Euler–Maclaurin can accelerate this only with
an explicit remainder; the simple brackets are the safe fallback.

The absolute error of A is the sum of two complex-gamma remainder bounds
and real-gamma evaluation errors. A log-density error δ gives relative
density error at most exp(|δ|)−1. Arithmetic cancellation is separate:
track the sum of absolute term magnitudes times the practical ε margin;
if it dominates the requested δ, switch formula/centred quadrature or
fail. Arbitrary precision checks here do not establish a uniform double
roundoff bound. **Open engineering G:** implement the combined normal-regime
branch and measure its floating-point margin, including m,|ν| near the
largest representable values. No silent use of unscaled Stirling is licensed.

## 2. Positive CDF quadrature in both tails

At an outward standardized point z=sR, R≥1, s=±1, let b=1/R and a=2m−1.
The outward tail (upper for s=+1, lower for s=−1) is exactly

$$T_s(R)=C e^{-s\nu\pi/2}
 \int_0^b t^{a-1}h_s(t)\,dt,\qquad
 h_s(t)=(1+t^2)^{-m}e^{s\nu\arctan t}. \tag{3}$$

This is the angular endpoint integral after reciprocal-tangent substitution.
The positive change t=b u^(1/a) removes the algebraic singularity:

$$\log T_s=A-s\nu\pi/2+a\log b-\log a
 +\log\int_0^1 h_s(bu^{1/a})\,du. \tag{4}$$

Never underflow u^(1/a) needlessly: retain its log and evaluate the limiting
h_s(0)=1 when that is accurate within the error budget. At huge |ν| the
integrand may still be sharply concentrated: split at its known mode and
use the mode-centred chart when needed, rather than trusting a fixed mesh.
The reference script uses the independent substitution t=b exp(−u/a),
then u=v/(1−v), for comparison with (4).

There is a simple rigorous positive quadrature fallback. On each t panel
[l,r], bound h by its extrema at the endpoints and at
sν/(2m) if that point lies in the panel, since
h′/h=(sν−2mt)/(1+t²). Multiply these bounds by (r^a−l^a)/a.
For l=0 use r^a/a; otherwise compute the weight using log/expm1.
Add positive bounds with log-sum-exp. Subdivide panels until
(log upper−log lower) is below the required log-probability error.
Integrability a>0 and continuity of h on [0,b] prove convergence, including
m arbitrarily close to 1/2. Efficiency may require higher-order analytic
panel bounds; embedded Gauss–Kronrod differences alone are not a certificate.
The fallback supplies explicit error analysis even where an empirical
quadrature estimate is misleading.

For |z|<1 integrate the smooth angular kernel on the bounded central
interval, adding a directly computed outward tail at ±1. To evaluate a
small lower tail use its positive integral from the left; for a small
upper tail use its positive integral from the right. Only form a complement
once a computed tail is known to be ≤1/2, using `log1mexp`; never form 1−F
to recover a tiny tail. Both log tails are valid even if their exponentials
underflow. Extremely small a can imply log|quantile|≫log DBL_MAX; carry
log-distance internally and report representational overflow explicitly.

## 3. Tail, central, and complex hypergeometric series

For |t|<1 expand h_s(t)=Σ c_j t^j. Its ODE gives the stable recurrence

$$c_0=1,\ c_1=s\nu,\quad
 (j+1)c_{j+1}=s\nu c_j-(j-1+2m)c_{j-1}. \tag{5}$$

Then T_s=C exp(−sνπ/2)b^a Σ c_j b^j/(a+j).
This is a convergent tail series, not merely its first power-law term;
the leading log tail is A−sνπ/2−log a−a log R. For b<ρ<1, Cauchy's
estimate uses

$$M_\rho=(1-\rho^2)^{-m}\exp(|\nu|\operatorname{atanh}\rho).$$

After terms through j=N, an absolute bound on the scaled sum remainder is

$$E_N\le {M_\rho(b/\rho)^{N+1}
              \over(a+N+1)(1-b/\rho)}. \tag{6}$$

If S_N>E_N, relative error is at most E_N/(S_N−E_N). Otherwise continue
or switch to positive quadrature. Bound rounding using the *absolute*
term sum as well; series with catastrophic cancellation are rejected,
not accepted because the last term is small. The prototype fixes ρ=1/2
on its selected grid; production should choose ρ adaptively. Large |ν|b
can make this bound expensive even for small b, a legitimate switch case.

The same recurrence with sν replaced by −ν generates the central density
series g(z)/C, and integrating gives Σ c_j z^(j+1)/(j+1). For |z|<1
an analogous Cauchy bound applies. This avoids complex branches at the
origin and handles exceptional hypergeometric parameters.

For comparison with the Heinrich/Willink class of formulas, set
P=m−iν/2,Q=m+iν/2,w=(1−iz)/2. A branch-continuous antiderivative is

$$J(z)=2iC2^{-2m}{w^{1-Q}\over1-Q}
 {}_2F_1(1-Q,P;2-Q;w). \tag{7}$$

Its derivative is C(1+iz)^(−P)(1−iz)^(−Q)=g(z). Use real[J(z)−J(0)]
only with consistent principal branches along the central path; the CDF
constant at zero must be supplied independently. The prototype compares
this *derived primitive*, not an asserted verbatim transcription of either
author's formula, against real integration and (5). At Q=1 or forbidden
hypergeometric denominator parameters use the limiting central real series
or quadrature. An arbitrary primitive with a complex constant is not a CDF.

The 2F1 term ratio is (A+j)(B+j)w/[(C+j)(j+1)]. To certify a remainder,
find a uniform q<1 bounding its absolute value for all subsequent j;
then the next-term/(1−q) bound applies. Bounding only the current ratio
is insufficient. Transform to a disk with |w|<1, explicitly retaining
branches and gamma constants, or fall back. Record cancellation condition
number Σ|term|/|sum| before accepting. A double-double implementation could
help speed, but is not necessary for correctness because positive
quadrature supplies the fallback. No claim of a checked implementation of
each published global Heinrich/Willink transformation is made here;
that optimization is **open engineering H**, separate from the checked
convergent real series and the checked local hypergeometric primitive.

## 4. Quantiles

Use the smaller requested log tail and its orientation, never exponentiating
a tiny input probability. Bracket by doubling standardized distance (or log
distance in the far tails), with CDF enclosures deciding each direction.
Use tail-series leading powers only as proposals. Maintain a true sign
bracket; an undecidable CDF comparison refines the integral or fails.
Safeguarded Newton in x uses d log T/dx=±f/T; in log R it multiplies this
by |x−λ|. Fall back to bisection whenever a Newton step leaves the bracket
or arithmetic is nonfinite. Work on separate sides of the mode if useful;
the CDF itself is strictly increasing on the whole real line.

Terminate on a quantile-coordinate bracket within the requested accuracy,
not a probability residual alone. A log-tail error η implies a coordinate
error bounded by η/inf(f/T) on the bracket; use that lower bound or return
the bracket. Near a flat tail this distinction is substantial. Location
addition can erase a small residual: retain standardized/log-distance
witnesses and verify the final representable quantile convention before
returning. No hard 30-step cap constitutes a correctness argument.
The prototype uses conservative bisection with its convergence count stated.
Every sign comparison includes the analytic series remainder and a stated
90-digit working-arithmetic margin; an unresolved sign raises a failure.
It retains relative distance-bracket widths below 10^(−50), and checks
log-CDF residuals at independently integrated reference tails.
Those residual checks are numerical evidence, not a general coordinate
certificate; the bracket construction is what justifies its selected
quantiles.

## 5. An all-domain rejection sampler using only uniforms

The envelope in proofs.md yields a particularly simple alternative to
Heinrich's m>1 generator. In the mode-centred u chart choose
M=min(m,1), D0=√(2M), a0=2M−1>0. Use the normalized proposal

$$q_0(u)={a_0\over2D_0}(1+|u|/D_0)^{-2M}.$$

Draw its sign from one uniform and its magnitude from
D0 expm1[−log U/a0]. Draw another uniform W and accept iff

$$\log W\le \log[h(u)/h(0)]-2M+2M\log(1+|u|/D_0). \tag{8}$$

Equation (5) of proofs.md proves that the right side is ≤0. Its integral
shows expected proposals R=2D0 h(0)exp(2M)/a0, finite at every proper
parameter. For m≥1 the explicit h(0)≤exp(1/2) gives R≤2√2 exp(5/2)<35.
Thus large m or large skew does not cause exponentially bad acceptance.
For m↓1/2 the proposal follows the correct arbitrarily heavy power tail;
there is no m>1 restriction. Only uniforms are needed, no R gamma state,
complex gamma evaluation or inverse CDF. The exact rejection argument proves
the target law; the retained seed checks the transformation and envelope,
not a statistical distribution test masquerading as a proof.

To evaluate the kernel without a normalizer, write D=√(2m) and internal k
as in proofs.md:

$$\log[h(u)/h(0)]=-m\log(1-2ku/D+u^2/D^2)-\nu\Delta,$$
$$\Delta=\operatorname{atan2}\left({u\sqrt{1-k^2}\over D},1-ku/D\right).$$

The atan2 branch gives the true angular difference in (−π,π), including
root crossing. Cancellation of the two terms near the normal face requires
a power series or direct positive-kernel integration
−∫_0^u t/q(t)dt; it cannot be ignored. In the heavy-tail proposal retain
log|u|, combine common log powers before subtraction, and only materialize
the final x=c+wu if representable. If an accepted mathematical value is
outside the output type's range, report representational overflow; rejecting
it and drawing again would bias the law. Exact acceptance comparisons under
floating point require refining the log acceptance interval if log W lies
inside it; unresolved arithmetic stops, never forces an acceptance.
The production generator needs a finite documented evaluator/rejection cap
and status propagation rather than silently retrying forever or returning
the last proposal. Expected bounded work is not a deterministic work cap.
Inverse-CDF generation is a second, slower universal fallback with the
same log-tail guarantees as section 4.

The prototype's Python uniform stream stands in for the R adapter's uniform
callback; it does not access or change the user's R RNG. Production C++
should take a uniform callback supplied by the binding, leaving R RNG state
outside the core, as required by the project.

## 6. What Boost can supply

The inspected `special/special.hpp` already exposes real lgamma, beta,
incomplete beta and complements/inverses, digamma/trigamma/polygamma,
Student-t and normal tail helpers, and gamma tails. Those support real
normalizer pieces, ν=0 delegation, V/normal reference calculations and
proposal comparisons. Stable real gamma ratios, Bernoulli coefficients,
or Boost quadrature could be exposed through the same seam if adopted.
They are not permission to include Boost directly in a new family header.

Boost does not supply the needed complex log gamma or general complex
2F1 in this design. Write the bounded real-part evaluator, combined/log-ratio
branches, complex-series machinery only if worthwhile, positive quadrature
with error bounds, quantile bracketing and the uniform-driven sampler.
The fitter also needs real and imaginary complex-digamma combinations:
differentiate the analytic gamma representations and bound derivative
remainders (for example by Cauchy bounds on a small right-half-plane disk).
The existing real digamma seam alone does not supply those shape scores.
Finite differences of a cancellation-prone normalizer cannot serve as a
certified score or Hessian.
D/p/q/r and special-function files are settled in this task: no file there
has been changed. A future implementation must separately validate binary64,
no-long-double and bindings before claiming production accuracy.

## 7. Reproduction and interpretation

Run `check.py` and `sampler.py` through `/tmp/umvl-cpu/run`, with one BLAS/OMP
thread and `nice -n 10`. `checks.json` records 90-digit reference comparisons
(65-digit working gamma/tail prototypes), terms, explicit mathematical bounds,
V/normal paths, scores and a scale-monotonicity counterexample. `sampler.json`
records 80-digit envelope/proposal checks and 320 accepted samples covering
m<1, m=1, large m and large skew. The scripts are research evidence and may
use exceptions and mpmath; the no-throw/Boost-only rules apply to production.

The first check run used a skewed quantile probability outside its tail-only
bracket and stopped at that setup assertion. The retained final script
constructs each requested log probability below the tail mass at R=4,
then uses the convergent series for efficient bracketing; it preserves an
independent positive-integral reference. Final validation records the corrected
run, rather than hiding the initial test-setup failure.

`structure.py` additionally checks full log normalizers, the gamma product,
explicit positive quadrature enclosures, the three-root scale example,
the Cauchy continuum, the mode-chart score and density envelope, and score
evaluation rank at a symmetric and an asymmetric interior point. Its
90-digit comparisons supplement, and do not replace, the proofs.

`hyper.py` implements its own complex 2F1 partial sums and the uniform
future-term ratio bound from section 3. Four 65-digit evaluations, at both
anchors of the two checked primitives, agree with independent 90-digit
mpmath 2F1 evaluations within the retained remainder and working-arithmetic
allowances. `hyper.json` records term counts and cancellation measures.

In the JSON files, `bound` is an assertion threshold. For Stirling and the
convergent tail/complex series it includes the stated analytic remainder
plus a working-arithmetic allowance. For independent integration agreement,
score differences and boundary-path convergence it is a numerical test
tolerance, not a newly proved universal error bound. In particular the
normal-path 10/√m threshold is a convergence check on that grid. Positive
panel enclosures instead retain their explicit lower and upper integrals.
