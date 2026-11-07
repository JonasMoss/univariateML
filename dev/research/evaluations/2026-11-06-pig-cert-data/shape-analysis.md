# Initial free-shape obstruction and rejected shortcuts

This records the initial route. The stronger variance barrier in
[the completed concavity proof](concavity-proof.md) closes the obstruction;
the final fitter replaces the mesh with a global score certificate.

Let `mu` be the fitted sample mean when both coefficients are free, and
let `E_k(z)` be the polynomial-degree expectation defined in the pig dossier.
For a positive sample, the reduced profile score has the sign of

\[
 n\{\sqrt{z^2+\mu^2}-z\}-\sum_i E_{x_i}(z).
\]

This follows by maximizing `n*t + S*log(lambda)` at fixed `z`, giving
`lambda = mu*t`, `t = sqrt(z^2+mu^2)-mu`, and differentiating the reduced
likelihood. Each summand and the square-root difference decrease with `z`.
That fact does **not** determine their difference's signs or its number of
zeros. The variance bound used in the held-shape proof gives
`d E_k / d log(z) = -Var(J_k)` and `Var(J_k) <= E_k`; it does not compare the
weighted variance to the square-root term's derivative. Thus applying the
held-shape bisection proof to this different equation would be unjustified.

At the shape-zero end the existing derivation proves minus-infinite
likelihood; at the Poisson end the first inward derivative distinguishes
strict overdispersion from the global underdispersion/equality case. These
limit signs prove existence where stated in the dossier. They do not bound
the first omitted terms uniformly over a computed exterior interval, nor
exclude additional stationary points inside an unsearched mesh cell.

For held mean the same reduction `mu=xbar` is unavailable. A useful compact
coordinate is

\[
 w=\frac{\sqrt{1+2\mu^2/\lambda}-1}
         {\sqrt{1+2\mu^2/\lambda}+1}\in(0,1),\qquad
 \lambda=\frac{\mu^2(1-w)^2}{2w}.
\]

Here `p0=exp(-mu*(1-w))`, `p1/p0=mu*(1-w)/(1+w)` and

\[
 p_k=\frac{4w}{(1+w)^2}(1-3/(2k))p_{k-1}
       +\frac{\mu^2(1-w)^2}{k(k-1)(1+w)^2}p_{k-2}.
\]

Consequently the finite-sample held-mean score can be reduced to rational
polynomials on a compact interval after clearing positive denominators.
This supplies a possible route to root isolation with positive-polynomial
or Bernstein bounds, but the degree grows with the observed counts and
the number of distinct counts. Neither a root-isolation implementation nor
certified exterior comparisons was completed here. Replacing the old grid
by an unchecked fixed-degree polynomial solve would repeat the same gap.

`profile-search.cpp` scans 294 held-mean `{0,k}` samples and supplied means
on a log-shape mesh: k in {2,3,4,5,10,20,50}, zero multiplicity in
{0,1,2,5,20,100}, and mean/sample-mean in {.02,.1,.5,1,2,10,100}.
It tests 161 shape points from `mu^2*exp(-20)` through `mu^2*exp(20)` and
requires adjacent gains above 1e-9 to count a peak. Its output is
`profile-search.csv`; it finds no competing peaks. This is diagnostic
numerical evidence only. It establishes neither uniqueness nor globality,
and it deliberately does not feed a successful fit decision.

A denser/wider mesh, a small score, agreement of starts, or a finite gain
over Poisson is insufficient to close these gaps. That initial fallback was superseded after deriving the stronger variance
bound. The final default and held-mean searches use the monotone-score
certificate and the dossier declares `certified: yes`.
