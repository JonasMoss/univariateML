# Exact size ties in the ordinary binomial

TASK-332, base `a8919f50`. For held interior probability, an adjacent integer
identity completely decides an exact tie and needs only bounded integer
arithmetic. With both parameters free, a different adjacent integer identity
decides the tie for a given sample. The exact searches found no positive-mean
both-free tie in the sample grid or at any of the tested size pairs. A
positive-mean Poisson face cannot tie a finite profiled binomial.

The [Lean discrete report](2026-11-25-lean-discrete-report.md) already proves
the held-probability crossing theorem and its counterexample, with checked
statements in [Discrete.lean](../../lean/UnivariateML/Family/Discrete.lean).
The [binomial dossier](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md)
supplies the ordinary both-free shape and existence result. The
[face study](2026-11-07-binom-faces-report.md) and the
[size-search study](2026-10-31-size-search-report.md) use separate arguments
for modified and truncated models. The criteria below concern the ordinary
binomial likelihood.

Write \(n>0\) for sample length, \(M=\max_i x_i\), \(S=\sum_i x_i\),
\(Q=\sum_i x_i^2\), and \(\mu=S/n\). All comparisons in the proofs concern the
exact likelihood of the observations.

## Held probability and free size

**Proved.** For \(0<p<1\), put \(q=1-p\) and \(t=N+1\). The binomial
coefficient ratio gives
\[
R_N=\frac{L(N+1)}{L(N)}
   =q^n\prod_i\frac{t}{t-x_i}.
\]
Each factor \(t/(t-x_i)=1+x_i/(t-x_i)\) decreases strictly with \(t\) for
\(x_i>0\), and is constant for \(x_i=0\). If \(M>0\), the product is
strictly decreasing and tends to \(q^n<1\). More explicitly, every factor
\(qt/(t-x_i)\) is strictly below one when \(t>M/p\). Thus the first feasible
\(N\) with \(R_N\le1\) exists. All preceding increments are positive and all
following increments negative. The maximizing set is \(\{N,N+1\}\) if
\(R_N=1\), and \(\{N\}\) if \(R_N<1\). If \(M=0\), all ratios equal \(q^n<1\)
and size zero alone maximizes the likelihood.

**Proved exact algorithm.** Reduce \(q=A/B\), with positive coprime
integers \(A<B\). The tie identity is
\[
\prod_i(t-x_i)=(At/B)^n.
\]
A positive rational with an integral \(n\)th power is an integer, by
reducing its numerator and denominator. Hence \(B\mid t\) is necessary.
After that check set \(h=A(t/B)\), which lies in \([1,t-1]\). To decide
\(\prod_i(t-x_i)=h^n\) with no large products, store the \(n\) factors
\(t-x_i\). For each of the \(n\) copies of \(h\), cancel it against the
remaining stored factors using GCD and exact division until the copy is
one or the array is exhausted. An uncancelled copy rules out equality.
After the last copy, equality holds exactly when all stored factors are
one. Each cancellation preserves the product ratio and leaves the two
factors coprime. Once all stored factors have been tried against a copy,
none can cancel any of its remainder. The algorithm uses \(O(n)\) words
and at most \(n^2\) GCDs of integers at most \(t\), or
\(O(n^2\log t)\) elementary word divisions with Euclid. Every intermediate
fits the word holding \(t\). A work allowance can interrupt cancellation
with an indeterminate verdict. For a signed comparison, one option is
checked integer multiplication of the original two sides in a bounded
limb array, returning indeterminate on capacity or work exhaustion.
The equality result from cancellation alone supplies no ordering when
the products differ.

**Proved binary64 specialization.** Decode \(p=m2^{-k}\) from its
exponent and significand, removing powers of two from the significand
until \(m\) is odd. The unreduced denominator exponent for a normal
number is \(1075-E\), where \(E\) is the biased exponent, and it is
1074 for a subnormal with nonzero fraction field. The reduced complement
also has denominator \(2^k\), since \(2^k-m\) is odd. Do the divisibility
test as \(k\le v_2(t)\) before forming any power or subtracting any
significand. If it passes, compute \(h=t-m(t/2^k)\). The product is
strictly less than \(t\), so this expression fits the same word, even
when computing a floating-point `1-p` would have rounded to one. If \(t\)
is outside the supported integer domain, handle that as a representation
issue before using word arithmetic. A dyadic argument close to \(1/3\)
or \(2/3\) represents a different hold from the exact rational.

The endpoint cases follow directly from point-mass compatibility. Holding
\(p=0\) on all-zero data leaves every size admissible and maximizing.
Holding \(p=1\) on a constant sample fixes the size to the common count.
Incompatible endpoint data fail the existing input check.

## Both size and probability free

**Proved criterion, using the published profile shape.** At each positive
feasible size the fixed-size Bernoulli objective gives \(p_N=\mu/N\).
For positive constant observations the unique maximizing pair is
\((M,1)\), which attains likelihood one. For nonconstant positive-mean
data, \(0<\mu<M\). DeRiggi's continuous-size profile result gives
unimodality and at most one finite derivative root on the feasible
interior. Thus the increasing and decreasing branches are strict. In the
finite-fit regime, this restricts multiple integer maxima to adjacent
sizes straddling the continuous peak. Any exact adjacent equality is a
global tie, since strictness prevents the two sizes from belonging to the
same branch. The dossier's dispersion condition is
\(nS+S^2>nQ\) for a positive-mean finite MLE. The continuous shape
underlies the repository's search and also implies that the first
nonpositive forward difference identifies the smaller maximizing size.
[DeRiggi (1983)](https://doi.org/10.1080/01621459.1983.10477949).

Put \(F=nN-S\) and \(G=F+n\). Let \(L_N^*\) be the likelihood evaluated
at \(p_N\). Since
\[
L_N^*=\left(\prod_i\binom{N}{x_i}\right)
      \frac{S^S F^F}{(nN)^{nN}},
\]
division at \(N+1\) and \(N\) gives
\[
\frac{L_{N+1}^*}{L_N^*}
=\frac{N^{nN}G^G}{(N+1)^{nN}\,n^n\,F^F\prod_i(N+1-x_i)}.
\]
Use \(0^0=1\) in the constant endpoint formula. Equality of the two
displayed integer products decides the exact tie, and their ordering
gives the exact sign of the increment. In particular, this uses the
exact rational profile probabilities before either is rounded for
output. Straight powering can require \(O(nN\log(nN+1))\) bits and
therefore serves as an oracle or a bounded optional check, with capacity
checked before multiplying. Positive-mean both-free ties, if present,
return two points with different probabilities. For all-zero data the
entire maximizing set is the union of the \(p=0\) and \(N=0\) axes,
restricted to the model domain.

### Which adjacent sizes can tie

**Proved finite arithmetic test for any specified pair, over arbitrary
sample lengths.** This is also a way to search for a counterexample to
positive-mean uniqueness without limiting the sample length. Write
\(\mu=a/b\) in lowest terms, \(t=N+1\), \(u=bN-a\), \(v=bt-a\), and
\(r=v/b=t-\mu\). For the nonconstant case, \(1<r<t\). Let \(K=t-X\)
under the empirical law. Then \(1\le K\le t\), \(E K=r\), and
\(u,v,b\) are pairwise coprime. In the adjacent ratio, factoring \(F\)
and \(G\) using \(b\mid n\) and cancelling powers of \(n/b\) gives, for
each prime \(l\), the tie equations
\[
E\,v_l(K)=N[v_l(N)-v_l(t)]+r v_l(v)-(r-1)v_l(u)-v_l(b).
\tag{1}
\]
Here \(v_l\) is the integer prime valuation. These equations are necessary
and sufficient by unique factorization applied to the integer-product
identity. The following bounds make the candidate means finite for each
\(N\).

- If \(l\mid v\) and \(l\nmid t\), the right side of (1) is at least \(r\).
  Yet \(v_l(k)\le k-1\) for every positive integer \(k\), so the left side
  is at most \(r-1\). Conversely, if \(l\mid t\) and \(l\nmid v\), the
  right side is strictly negative. Thus \(v\) and \(t\) have identical
  prime support. For each \(l\mid t\), the left side lies in \([0,r-1]\).
  Dividing the two bounds on \(r v_l(v)-Nv_l(t)\) by \(r\) forces
  \(v_l(v)=\lceil Nv_l(t)/r\rceil\).
- Every prime dividing \(u\) or \(b\) divides \(N\). Otherwise its negative
  contribution on the right of (1) has no positive term to cancel it.
  For \(l\mid b\), (1) reduces to
  \(v_l(b)=Nv_l(N)-E\,v_l(K)\). Thus the allowed positive exponents of
  \(b\) run from \(\max(1,Nv_l(N)-\lfloor\log_l t\rfloor)\) to
  \(Nv_l(N)\). Exponent zero is also allowed.

To enumerate \(v\), evaluate the forced exponent vector at all breakpoints
\(r=Nv_l(t)/j\) in \((1,t)\) and at one rational interior point between
each consecutive pair of breakpoints and endpoints. For each such \(v\)
and each permitted \(b\), set \(u=v-b\), check \(1<v/b<t\), coprimality,
the prime support of \(u\) and the forced exponent vector at the actual
\(r=v/b\). The remaining question is a rational linear feasibility
problem in the frequencies of \(K=1,\ldots,t\). Require nonnegative
weights summing to one, mean \(r\), and all the equations (1) for the
primes up to \(t\). All primes in the equation are in that range by the
support checks. A feasible rational frequency vector scales to an
integer sample. Feasibility over the reals suffices, since the simplex
intersection is a bounded rational polytope and any nonempty such
polytope has a rational vertex. Any reconstructed sample solves the
integer-product identity and so gives a global tie by the profile shape
result.

An immediate exclusion is **every positive integer mean**. Then \(b=1\)
and \(r=v=t-\mu\le N\). Every prime of \(t\) occurs in \(v\) to at
least its exponent in \(t\), so \(t\mid v\), contradicting \(0<v<t\).
This argument concerns nonconstant data. The constant case was settled
by its point mass.

### Poisson face

**Proved.** At positive mean the maximized Poisson likelihood is
\(e^{-S}\mu^S/\prod_i x_i!\). Its factor multiplying \(e^{-S}\) is a
nonzero rational, whereas \(e^S\) is irrational for positive integer
\(S\). To see the latter directly, let \(f_j(z)=z^j(S-z)^j/j!\) for
positive integers \(j\). All derivatives of \(f_j\) at 0 and \(S\) are
integers, since terms of degree below \(j\) vanish in either endpoint
expansion and \(k!/j!\) is an integer for \(k\ge j\).
Repeated integration by parts expresses
\(\int_0^S e^z f_j(z)\,dz\) as an integer multiple of \(e^S\) plus an
integer. The integral is positive and bounded above by
\(e^S S^{2j+1}/j!\), which tends to zero. A rational \(e^S\) would give
these positive integrals a common denominator, a contradiction.
Every finite binomial profile value is rational, so it cannot equal
the positive-mean face. In particular, the exact mean-equals-variance
boundary chooses the Poisson escape with no tied finite maximizer, by
the existing existence theorem. At zero mean, the rate-zero Poisson
and the finite binomial point masses all attain likelihood one.

## Exact enumeration and replay

**Exhaustive finite computation with exact arithmetic.** The grid takes
each sorted sample of length 1 through 4 on counts 0 through 10 once.
That is 1,364 equally counted multisets. The frequencies below describe
this grid, with no sampling-law weights. For held \(p\), the checker
evaluates the likelihood directly at every integer from \(M\) through
\(\lceil M/p\rceil\). The next ratio there is strictly below one and
the proved tail criterion excludes all larger sizes. It compares direct
likelihood quotients to the adjacent-ratio formula and the GCD test at
every scanned increment.

| Exact \(p\) | Fits | Constant ties | Nonconstant ties | All ties |
|---|---:|---:|---:|---:|
| \(1/2\) | 1,364 | 40 | 24 | 64 |
| \(1/3\) | 1,364 | 40 | 16 | 56 |
| \(1/4\) | 1,364 | 40 | 4 | 44 |
| \(2/3\) | 1,364 | 20 | 12 | 32 |
| Total | 5,456 | 140 | 56 | 196 |

The [full tie list](2026-11-25-binom-ties-math-data/held-ties.csv) marks
each of the 56 nonconstant rows with `constant_branch_misses=True`.
The source returns a two-point set only on the positive constant-count
interior branch. The general branch leaves the default uniqueness flag
set, regardless of which side its numerical search selects. Of the
grid's nonconstant rows, 28 use the exactly representable inputs
\(1/2\) and \(1/4\). The other 28 are identities for the exact rational
holds. Their displayed tie pairs cease to tie at the rounded binary64
inputs, whose reduced complement denominators exceed all the tabulated
\(N+1\). [Fitter source](../../../univariateML-cpp/include/cppunivariate/distributions/binom/mle.hpp).

On the same sample grid with both parameters free, there were 4
all-zero samples, 780 positive-mean face cases, and 580 positive-mean
finite cases. The checker locates the first nonpositive exact forward
increment for each finite case and independently evaluates the direct
rational profile at the winner and its feasible neighbors. The largest
winning size was 641. No exact finite tie occurred. The separate
arbitrary-length search at \(1\le N\le100\) found **zero candidate means**
after the necessary integer filters, so no feasibility-solver verdict
was needed to exclude ties on that size grid. Both results have exact
integer or rational precision.

The two final Python runs exited zero on the workstation through the
single-slot wrapper. From the worktree root, replay sequentially with
the installed Python and SymPy.

```sh
mkdir -p build/tmp
export TMPDIR="$PWD/build/tmp" OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1
data=dev/research/evaluations/2026-11-25-binom-ties-math-data
/tmp/umvl-cpu/run nice -n 10 python3 "$data/check.py"
/tmp/umvl-cpu/run nice -n 10 python3 "$data/size_pairs.py"
```

[Direct likelihood and binary64 checks](2026-11-25-binom-ties-math-data/check.py)
and [arbitrary-length size-pair search](2026-11-25-binom-ties-math-data/size_pairs.py).

## What this changes for the fitter

- **Held interior probability.** Use the search to propose a size and
  consider the adjacent increment on each feasible side. Testing both
  sides matters because the numerical search can land on either tied
  size. Exact equality on one side suffices to certify the full pair.
  To certify a singleton, obtain a strict positive incoming sign
  where the predecessor is feasible and a strict negative outgoing
  sign, using the project comparison margin or an exact ordering.
  Inequality from the GCD test alone cannot certify that the search
  landed on the right size.
- **Outcome and binding contract.** For a proved pair return `Ok`,
  a certified estimate, `unique = FALSE`, and a typed
  `MaximizerSet` with two points in coefficient order `(size, prob)`.
  Use the smaller size as the representative and report the held
  probability bit-for-bit at both points. Both points share the same
  maximized log likelihood and pass through the existing common
  binding attachment. A hard search failure keeps its diagnostic.
  If a successful candidate's certificate exhausts its work allowance
  or cannot order a neighbor at the margin, retain the candidate with
  `certified = FALSE` and avoid asserting proven uniqueness. If the
  comparison refutes the candidate, use the proved winner if available
  or report the failure. Use the existing work-budget units with
  `min(iterlim, 100000)`. Require both members of an exact pair to be
  representable by the typed result before calling it complete.
- **Both free.** Keep the dispersion escape, evaluated exactly when
  possible. The integer product signs can resolve an ambiguous
  finite comparison when the arithmetic budget allows. A zero sign
  gives a certified pair with probabilities \(\mu/N\) and \(\mu/(N+1)\).
  The all-zero maximizing axes and constant positive point mass have
  their direct outcomes. A positive-mean finite-versus-Poisson
  comparison within the numerical margin cannot be labeled an exact
  tie. For \(S>0\) and \(nS+S^2\le nQ\), the existing `Underdispersed`
  status maps to the proved Poisson limit.
- **Fixtures under normal entry points.** At held `0.5`, use \((0,6)\)
  for the interior crossing at sizes 7 and 8, and \((0,0,7)\) for the
  support-boundary crossing at those same sizes. At held `0.25` use
  \((0,7)\) for sizes 15 and 16. Check the exact two rows, common
  likelihood, preserved held probability and false uniqueness flag.
  The immediate binary64 predecessor and successor of `0.5` on
  \((0,6)\) have unique sizes 8 and 7 respectively. The exact rational
  hold \(p=1/3\) on \((0,1,6)\) ties sizes 8 and 9 and is an arithmetic
  oracle fixture for an exact rational input. The usual binary64
  conversion is an exact non-equality control for that pair.
  Keep the constant tie \((1,1,1,1)\) at `0.5`, the zero-data singleton
  at positive held probability, and the all-zero axes with both free.
  For a positive-mean Poisson boundary fixture use \((0,2)\), whose
  mean and variance with divisor \(n\) both equal one.
  [Common binomial binding](../../../bindings/common/distributions/binom/fit.hpp),
  [existing R fixtures](../../../bindings/r/distributions/binom/test.R).

## Open

- Existence of a nonconstant, positive-mean both-free tie without any
  size or sample restriction. The exact test decides a given sample or
  specified size pair, and the two searches above give bounded
  exclusions. A global impossibility proof or a feasible valuation
  system with \(N>100\) and an explicit scaled integer sample would
  settle the existence question. The bounded exclusions do not give
  the fitter a general uniqueness guarantee.
