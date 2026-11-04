# Global likelihood localization at the BB Poisson corner

This supplies the global step missing from the local cone calculation. The
argument is for the **mathematical likelihood supremum** over BB(N,a,b) with
integer N and positive shapes, admitting Poisson as a closure face. It does
not certify any production maximizer. Observations are iid Pois(mu0), mu0>0.
Approximate suprema with likelihood error o_P(1) suffice.

## 1. Law consistency without a parametric compactness assumption

Let Pn be the empirical pmf and g the true Poisson pmf. For any candidate q
whose log likelihood is at least that of g minus o(1),

$$D(P_n\Vert q)\le D(P_n\Vert g)+o(1/n).$$

Take K_n=ceil(C log n), for a fixed C>0. With probability tending to one all
observations are <=K_n. Conditional on this event they are iid from g
truncated and normalized to this finite set. On K_n+1 cells,
$E D(P_n\Vert g_{\le K_n})\le E\chi^2(P_n,g_{\le K_n})=K_n/n$.
The difference from D(Pn||g) is -log P_g(X<=K_n), negligible faster than
every power of n. Hence $D(P_n\Vert g)=O_P(\log n/n)$.

The inequalities $H^2\le D$ and the Hellinger triangle inequality imply

$$H(q,g)=O_P(\sqrt{\log n/n}).$$

This holds for every likelihood-competitive candidate simultaneously; no
uniform empirical-process theorem over all shapes or sizes was assumed.
It is essential that this is D(empirical||q), not D(g||q): the latter is
infinite for every finite-size BB because Poisson has mass above N.

The [BB closure theorem](../2026-11-03-bbinom-closure-report.md), Section 2.2,
now forces N->infinity, a->infinity and the actual mean mu->mu0 for all
competitive candidates. Bounded N cannot approximate Poisson's infinite
support. The fixed positive-count probability assumption of that theorem
holds automatically near g. Its finite-shape limiting possibility would give
a proper NB different from g, so a must diverge. The remaining ratio and
zero-probability limits identify the Poisson mean. The implication is uniform
by the sequential contradiction principle.

## 2. An initial parameter rate from square-root geometry

Put u=1/a,k=1/N and d=u-k-(mu0+1)k^2. The joint root expansion, including
the regular Poisson mean direction, gives locally

$$H^2(q,g)\asymp (\mu-\mu_0)^2+d^2+k^4.$$

The constants can be chosen uniform in a sufficiently small neighborhood of
the corner. In the cancelling region this follows from independence and
positive norms of the first three Poisson–Charlier polynomials. Outside it,
the `(u-k)C2` term dominates; its root remainder is smaller uniformly when
|u-k|/(u+k)^2 tends to infinity. Combining these two cases by a subsequence
argument proves comparability. Using mu rather than mu0 in d changes it by
a negligible product of small coordinates.

It follows, uniformly for competitive candidates, that

$$|\mu-\mu_0|+|d|+k^2=O_P(\sqrt{\log n/n}),\qquad
 r:=u+k=O_P((\log n/n)^{1/4}).$$

In particular r max(X_i)=o_P(1); the candidate sizes greatly exceed the
sample maximum. This is enough of an initial rate to justify a uniform
fourth-order likelihood expansion, without first assuming root-n effective
coordinates.

## 3. Uniform expansion on that initial region

For the zero probability use

$$\log p(0)=\sum_{j=0}^{N-1}
 \{\log(1+\mu u k j-\mu k)-\log(1+\mu u k j)\}.$$

Taylor expansion through total degree four in u,k uses terms of the logs
through degree five before summing. Since kj<=1, the sum of the omitted
terms is bounded by C r^5 for bounded positive mu. One can see the extra
factor k before summation by writing each log difference as the integral
over its decrement mu*k; the fifth-order remainder of the resulting
reciprocal has that factor. Sums of powers of j give polynomial coefficients
by Faulhaber's identity. The nonzero probabilities follow by summing the
exact log-ratio increments

$$\log(1-xk)+\log(1+xu)
 -\log\{1+\mu(u-k)-\mu(x+1)uk\}.$$

On xr<=1/2 their remainder is bounded by C r^5 times a fixed polynomial
in x, uniformly in the actual integer N. The initial region and the
logarithmic sample-maximum bound ensure this condition with probability
tending to one. The sum of remainders is
$O_P(nr^5)=O_P(n^{-1/4}(\log n)^{5/4})=o_P(1)$.
The same expansion in mu-mu0 has negligible mixed remainders at its initial
rate. All centered polynomial sums are O_P(sqrt(n)); centered terms of
degree three or higher in r vanish uniformly, as do second-order terms
containing d*r or (mu-mu0)*r. Only the effective linear scores survive.

To identify deterministic coefficients, normalize on x<=K_n and use the
factorial-moment exponential bound. On the initial parameter region it gives
uniform bounds on every *fixed* exponential moment; choose its exponent so
the omitted mass is o(n^-M) for any needed fixed M. Both g and q then have
negligible omitted normalization error. Expanding that normalization through
degree four forces the mean log likelihood to be minus one half the squared
leading score norm. This avoids taking the infinite D(g||q).
The independent [symbolic check](symbolic.py) verifies all fourth-order
normalization coefficients and the cancellation drift exactly; it checks
algebra, whereas the preceding remainder bounds justify the limit.

After whitening the mean score, write
$U=\sqrt{n/\mu_0}(\mu-\mu_0)$ and use A,B from the Gaussian derivation.
Uniformly over the initial region,

$$\ell_n(q)-\ell_n(g)
 =U Z_{1n}+A Z_{2n}+B Z_{3n}
 -\tfrac12(U^2+A^2+B^2)+o_P(1),\quad B\ge0,$$

where the three Z's converge jointly to independent standard normals. Since
the quadratic part is coercive, likelihood-competitive maximizers have
(U,A,B)=O_P(1), improving the preliminary logarithmic rate. Every bounded
point with B>=0 is approximated by actual integer sizes and positive shapes.
Consequently the **unrestricted mathematical BB-vs-Poisson closure LR**, with
the mean fitted under both hypotheses, converges to

$$Z_2^2+\max(0,Z_3)^2\sim\tfrac12\chi_1^2+\tfrac12\chi_2^2.$$

The proof concerns supremum values; it does not require existence of a finite
positive-shape MLE, uniqueness of shape/size estimates on the limiting edge,
or accuracy of an optimizer. A numerical search returning a capped size
would not implement this theorem. The argument is uniform on compact positive
mu0 sets and bounded effective local alternatives by the same moment bounds
and local contiguity. It does not cover mu0 tending to zero.
