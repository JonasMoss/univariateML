# Corner enclosures and retained certificate

For `z>=0` and `1+xi*z>0`, let

$$L(z,\xi)=\int_0^1\frac{z}{1+\xi zt}\,dt.$$

Differentiation gives

$$-L_\xi=\int_0^1\frac{z^2t}{(1+\xi zt)^2}\,dt,
\qquad L_{\xi\xi}=2\int_0^1\frac{z^3t^2}{(1+\xi zt)^3}\,dt.$$

All three integrands decrease in xi. Their z derivatives are respectively
`1/(1+xi*z*t)^2`, `2*z*t/(1+xi*z*t)^3` and
`6*z^2*t^2/(1+xi*z*t)^4`, which are nonnegative even for negative xi.
Thus two corners bound L, -L_xi and L_xixi on every feasible rectangle:
`(z.lo,xi.hi)` gives their lower bounds and `(z.hi,xi.lo)` their upper
bounds. This is an exact-arithmetic argument, including xi=0 by continuity.
The sign reversal for L_xi is retained when forming its interval.

For |xi*z|<=1/4 the scalar expansions are

$$L=z\sum_{j\ge0}\frac{(-\xi z)^j}{j+1},\quad
L_\xi=-z^2\sum_{j\ge0}\frac{j+1}{j+2}(-\xi z)^j,\quad
L_{\xi\xi}=z^3\sum_{j\ge0}\frac{(j+1)(j+2)}{j+3}(-\xi z)^j.$$

After degree J, the existing absolute remainder bounds remain valid:
`z*r^(J+1)/((J+2)*(1-r))`, `z^2*r^(J+1)/(1-r)` and
`z^3*r^(J+1)*((J+3)/(1-r)+r/(1-r)^2)`.
Success requires each remainder <=4*epsilon. Recurrence replaces pow for
r^(J+1), and each corner retains its own remainder. A cap still returns
an invalid enclosure. This tightens dependencies and reduces arithmetic;
it does not approximate the profile by a truncated unchecked series.

The nuisance-only evaluator uses exact corner bounds for L and the existing
interval expressions for L_v and L_vv. It computes exactly the same
f_v and f_vv formulas as the full evaluator, with compensated sums and the
same 128*epsilon practical margins. The stable log1p quotient also handles
zero and subnormal products. The inner solver still verifies both boundary
score signs; interval Newton still requires a negative nuisance curvature.
The nuisance-only evaluator also checks the original series remainder at the
caller’s iteration cap, even though it does not need those shape derivatives.
This preserves a series-cap iteration stop at small maxit; omitting it lets
some low-budget golden fits reach a later numerical stop instead.
No proposal can establish a certificate. Uniform sign checks in region use
the same restricted evaluator; contractions and outer tests retain full jets.

Interval multiplication selects the extremizing products by operand signs;
scalar multiplication selects endpoints by scalar sign. These are algebraic
rearrangements of the original min/max of four products. The repeated slab
midpoint jet is reused without changing its arguments. The endpoint-root
cache, slab cover, face envelope, likelihood comparisons, coefficient
isolation, public restoration, statuses and work caps retain their rules.
Sorting was already done once by the caller; no additional sort is needed.

As before, these are certificates in exact arithmetic conditional on computed
comparisons, with practical floating-point margins, not outward rounding.
Independent 70-digit checks test computed enclosures; they do not prove
coverage. The prior joint-cover proof supplies that argument.
