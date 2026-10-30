# TASK-95: free-size beta-binomial

No free-shape subset was enabled: the available numerical sweep does not
certify the unbounded integer tail. The three structural `holds:` remain,
with distinct reasons and wrapper diagnostics. Default fits are unchanged.
This report separates the proved held-beta result from the incomplete
closure and numerical evidence for the other subsets.

## Limits and existence

Write N for size, a for alpha and b for beta. At fixed N, both shapes free
admit the binomial face (a+b -> infinity at fixed ratio), and the two-point
law on {0,N} (a,b -> 0 at fixed ratio), including point masses. The latter
is the supremum for samples containing both endpoints and no interior
counts. N=1 is an attained but unidentified Bernoulli family, and N=0
is an attained but unidentified point mass at zero. With one shape held,
the free shape tending to zero or infinity gives a point mass at an endpoint.
These fixed-N faces must be compared across all N; a boundary at one N is
not evidence of global non-existence.

For N -> infinity, a held or tending to a positive finite value and
b/N -> c>0 gives NB(shape=a, mean=a/c), by the beta-binomial mass formula.
Thus the held-alpha subset has an NB face with *that exact shape*; both
shapes free allow arbitrary NB shape, including its Poisson limit. Binomial
faces with N -> infinity and Np -> lambda also tend to Poisson. Paths can
lose mass at infinity: for example a -> 0 with a log(N/b) -> t and suitable
b, the finite-count mass tends to a point mass at zero of weight exp(-t),
with the remaining mass escaping. These defective limits cannot fit a sample
containing a positive finite observation. The classification of **all**
joint paths is not proved here; no exhaustive closure claim is made.

For held beta=b>0, the following is proved. If x>0, N>=2x and
H=sum_{j=0}^{N-1} 1/(b+1+j), uniformly over all a>0,

    P(N,a,b; X=x) <= max(2^x/(e H), (2^(x+1)-1) exp(-H)).

For a<=1, P(X=0)<=exp(-a H), and the mass ratio P(X=x)/P(X=0)
is at most 2^x a. Maximizing a exp(-a H) gives the first term. For a>=1,
stochastic monotonicity in a bounds P(X=x) by P(X<=x) at a=1;
the same ratio bound gives the second term. H grows to infinity, so the
bound tends to zero. Multiplying by the remaining observations' masses,
each at most one, gives a bound on the **whole** likelihood for all sizes
past any N0: H grows with N and both terms decrease. Given any positive
finite likelihood, this supplies a computable stopping point in principle.
For a nonconstant sample with a positive observation, each fixed-N profile
attains a maximum because a -> 0 and a -> infinity give endpoint masses
and at least one observation disagrees with each endpoint. Hence a finite
maximum over N exists. All-zero data attain likelihood one at N=0 (shapes
unidentified). Constant positive data have supremum one at N=x, a ->
infinity, but no finite positive-shape maximizer. The bound is extremely
loose: the first term decays only as 1/log N, and the required N can be
astronomical. It does not make enumeration practical.

For held alpha, fixed-size point-mass limits and the held-shape NB limit
compete; for neither held shape, fixed-size binomial and two-point faces and
the free-shape NB/Poisson limits compete. A finite candidate greater than
all limit values is not by itself a proof of existence without a uniform
integer-tail bound. No such useful bound or reliable numerical stopping
criterion was established. No numerical margin from a finite grid is
claimed to certify a result. In particular the endpoint-only face is
omitted by the proposed P0b criterion.

## Independent checks

`2026-10-30-holds-bbinom/oracle.R` performs four-start R optimization of
`extraDistr::dbbinom` for each of 3,132 sample/hold/size combinations:
12 samples (n=20 and 100, overdispersed, underdispersed, negative-binomial,
Poisson, ties, endpoint-only, and constant), all three held subsets,
consecutive feasible N through 60 then a logarithmic grid through 2,000.
The CSV also records exact `dbinom` likelihoods at each N, exact `dpois`
and held-shape `dnbinom` likelihoods, and a numerical free-NB shape profile.
The shapes are deliberately bounded to avoid inaccurate reference densities;
281 boundary hits and 33 nonconverged optimizations are recorded. This is a diagnostic
sweep, not a reliable oracle for global optimization: several grid fits hit
bounds or fail convergence; grid gaps, shape bounds, and reference precision
prevent a certified comparison. In particular the held-alpha tied samples
still improve at N=2000. Full rows and summaries are next to the script.

`bounds.R` passed 527 checks of the uniform bound and binomial, NB,
Poisson, two-point and point-mass limit paths against extraDistr and R's base densities. These are
numerical checks of examples, not proofs. The R tests check each retained
wrapper diagnostic; an appended C++ doctest checks that unsupported paths
return failure rather than a fake fit.

## Validation

Simbox `just cpp-test`: 6/6 CTest targets passed (239.78 seconds).
Simbox `just test`: FAIL 0, WARN 0, SKIP 0, PASS 30,743 (460.8 seconds);
the bbinom file contributed 96 passing expectations. The first R run had
two diagnostic wording mismatches; the corrected complete rerun passed.
Simbox `just quality bbinom`: 1/1 passed. `git diff --check`: clean.

## Corrections for a P0b revision

- Replace the claim that held beta mirrors held alpha: the reflection
  x -> N-x changes the observed data with N. With beta held, positive-count
  probabilities vanish uniformly as shown above; there is no nondegenerate
  held-beta NB face at infinite N.
- The proposed comparison against binomial-over-N and NB alone omits
  fixed-N endpoint two-point and point-mass faces. Compare those as well.
- The proposed adaptive geometric search and refinement has no proved
  global integer-tail bound and can miss peaks; a finite-grid maximum or
  finite-grid margin is not a certificate. The proposed iff criterion also
  requires an exhaustive closure argument, which remains open.
- Distinguish attained but unidentified N=0 and N=1 laws from unattained
  shape limits, and distinguish proper limit laws from mass escaping to
  infinity. Existing wrapper behavior for unidentified fixed-size fits is
  unchanged.

## Open items

Derive a practical uniform held-beta tail bound, and exhaustive closure and
usable tail bounds for free and held-alpha shapes. A future implementation
must compare fixed-N boundary statuses as likelihood candidates and stop on
budget or unresolved numerical certification. An uncapped large-N fitter
would also require compatible large-size density evaluation; `dpqr.hpp`,
`dpqr.R`, `special/`, `truth.*`, code generation and generated d/p/q/r are
owned by TASK-78 and were not changed. No default golden was changed.
