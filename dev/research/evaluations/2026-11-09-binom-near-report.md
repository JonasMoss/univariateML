# Binomial relatives: near-critical faces and higher-order boundaries

TASK-116.56, binom-near. Base: 2ed5514b32ec72ded95809c834453d24f025f850.
All changes are uncommitted in the assigned worktree. No merge, rebase,
dependency installation, board edit or change to a frozen golden was made.

## Delivered outcomes

The rate-four example in the previous investigation now returns a certified
finite global maximizer. A second exact boundary, with a negative third
coefficient, now returns a certified truncated-Poisson limit. Both certificates
are called by the public C++ fitter and therefore by the common R/JS route.
The adjacent held-inflation ZIP inputs retain structured unresolved stops;
neither is returned as a certified limit or an unchecked finite estimate.

| Input and free coordinates | Implemented outcome | Global coverage |
|---|---|---|
| tbinom counts (242187,60672,121344,1488980,406274), window 0,…,4, size and probability free | Ok: size 7, probability 0.44715659335602981 | Every size 4,…,906 profiled; uniform Jensen exclusion for sizes ≥907 |
| tbinom counts (21251,11790,0,244158,168771), same window and free coordinates | TpoisLimit: rate 6, finite likelihood upper bound, comparison flag cleared | Every size 4,…,26592 excluded; uniform third-order exclusion for sizes ≥26593 |
| zib sample (0,0,0,0,0,1,1,2,2,4), held inflation 0.3772146069463776, size/probability free | Unresolved; ZIP and finite witnesses retained, upper bound infinite | No global certificate claimed |
| Same zib sample, held inflation 0.37721460694637765 | Unresolved; same structured-stop contract | Exact leading sign proves a finite improvement exists, but its global maximizing coefficients remain uncertified |

The four dossiers continue to say “certified: partial”. The tbinom fit.note
now names the new certified subsets. The ztbinom, zmbinom and zib notes were
checked and retain their existing qualifications; their production fitters
were not changed. In particular, the unresolved ZIP cases do not justify
changing any family to “yes”.

This resolves the listed rate-four example in
[Open 3 of the faces report](2026-11-07-binom-faces-report.md), and supplies
both signs of the third-order boundary. The remaining ZIP obligations from
Open 2 are stated precisely below. The
[binom-rel report](2026-11-07-binom-rel-report.md) supplies the earlier routing
and retained-support context.

## Uniform higher-order comparison

Fix a finite window 0,…,U. Let P be the empirical law, Q its mean-matched
truncated-Poisson law, and n the sample size. Write

$$
c_N(k)=\sum_{j<k}\log(1-j/N),\quad
a(k)=-k(k-1)/2,\quad
b(k)=-k(k-1)(2k-1)/12,\quad
c(k)=-k^2(k-1)^2/12.
$$

The sums run from j=0 to k−1. Thus c_N=a/N+b/N²+c/N³ plus a
nonpositive fourth-order remainder.
Define

$$
\mu=E_QX,\quad
\beta=\frac{\operatorname{Cov}_Q(a,X)}{\operatorname{Var}_QX},\quad
h=a-E_Qa-\beta(X-\mu),\quad H=\max|h|.
$$

Then E_Qh=E_Q(Xh)=0 and E_Q(ha)=E_Qh². For N≥2H,
Q_N=Q(1+h/N) is a probability law with the same mean as P.
In the full-support range N≥U, changing measure to Q_N in the binomial
normalizer and applying Jensen gives

$$
\frac{\ell_N-\ell_\infty}{n}
\le J_N
=D_{\mathrm{KL}}(Q_N\Vert Q)+E_Pc_N-E_{Q_N}c_N.
$$

The natural-parameter term cancels because the means match. This inequality
holds for every probability, hence also after profiling it. It is not an
expansion at a fixed, unprofiled rate.

The first coefficient is A=E_Pa−E_Qa. At A=0 the second is
B=E_Pb−E_Qb−E_Qh²/2. When both vanish exactly,

$$
J_N\le C/N^3+R/N^4,\qquad
C=E_Pc-E_Qc-E_Q(hb)-E_Q(h^3)/6,
$$

$$
R=\tfrac23 E_Qh^4+E_Q|hc|+\tfrac34 E_Q\sum_{j<X}j^4,
\qquad N\ge\max(2U,2H).
$$

Here is the remainder proof. On |z|≤1/2, the fourth derivative of
(1+z)log(1+z) is at most 16, so its remainder after the cubic term is
at most 2z⁴/3. For j/N≤1/2, the log remainder after its cubic term lies
between −j⁴/(2N⁴) and zero. Its empirical contribution is nonpositive;
Q_N≤3Q/2 bounds its other contribution by 3E_QΣj⁴/(4N⁴).
The cross term involving hc contributes at most E_Q|hc|/N⁴.
These establish the displayed uniform inequality.

The coefficient C also equals the third coefficient of the profiled
likelihood. The finite-support mean equation has positive derivative
Var_QX, so its root is analytic near 1/N=0. Its fitted law has first
perturbation Qh/N. The difference between J_N and the fitted likelihood
gap is D_KL(Q_N || fitted law), which is O(N⁻⁴).

The exact recognition uses
E_Q[(X)₂]=λ(μ−U)+Uμ. At A=0 it gives the rational candidate
λ=(US−F₂)/(Un−S), where S=Σx and F₂=Σx(x−1).
Checked integer products verify the mean equation exactly.
Reduced, checked signed rational arithmetic verifies B=0 exactly.
Factorial/product/rational overflow declines this sufficient criterion;
it never turns a small coefficient into zero.

For padded C<0, the implemented cutoff is
ceil max(2U,4H,4R/(−C)). The factor four leaves a strict negative tail.
Every smaller feasible integer must still be covered.

For that prefix, the code evaluates N³J_N after removing the two exact
zeros. It uses the parent series through degree eight, with error bounds

$$
\frac{|h|^3|h/N|^6}{72(1-|h/N|)},\qquad
\sum_{j<k}\frac{j^3(j/N)^6}{9(1-j/N)}
$$

for entropy and log-product terms respectively. Absolute coefficients
multiply the latter errors. This avoids subtracting quantities of order
N⁻² when deciding a gap of order N⁻³.

## Finite winners and coefficient accuracy

There is also a simpler uniform bound that uses only A=0:

$$
(\ell_N-\ell_\infty)/n\le M/N^2,\qquad
M=E_Q\sum_{j<X}j^2,\qquad N\ge2U.
$$

Indeed, c_N−a/N lies between −Σj²/N² and zero. Apply the same Jensen
argument with Q itself, then cancel A. If a finite witness has a strict
improvement g exceeding four likelihood margins, this bounds the entire
tail below it. The implemented cutoff is
ceil max(2U,sqrt(4nM/(g−2m_LL))).
Every smaller feasible size is profiled, and the winner must be separated
from all competitor upper bounds. No unimodality in integer size is assumed.

The histogram profile uses exactly the original sufficient statistics.
It includes changing supports when N<U. In Poisson-scale coordinates
η=log(Np/(1−p)), its mean is strictly increasing with derivative Var(X).
Since |d log Var/dη|≤U, the variance at the bracket midpoint supplies
the lower bound Var(mid)exp(−U×bracket width/2). The stopping check includes
the practical mean pad 64ε(1+U) and requires the resulting bracket/error
width below 2.5×10⁻¹¹. Because |d log p/dη|≤1, this controls relative
probability error within the default 10⁻¹⁰ contract.
Concavity gives the profile likelihood upper bound from the midpoint score
and bracket radius.

The likelihood comparison margin is
m_LL=10⁻⁹n+8192ε(1+|ℓ_face|). Moment and normalized-series comparisons
use 8192ε times their absolute-component scales. The inherited returned
face upper bound adds 10⁻⁹+4096ε(1+|ℓ_face|).
These are package certificates in exact arithmetic conditional on the
computed comparisons, with stated practical floating-point margins.
They are not outward-rounded interval-arithmetic certificates.

For the rate-four example, Q has weights (24,96,192,256,256), total 824.
Exact arithmetic gives

$$
A=B=0,\quad C=443772828460/24004266924673>0,\quad
R=332713258722483074/11031675242381863.
$$

The finite certificate uses cutoff 907 and 3,946,841 enumeration cells.
The independent 80-digit size-seven root and likelihood are

    probability  0.44715659335717363398349989050467513766336290681562514618481259998595556323627519
    logLik      -3379063.9196042544210509670861429783099630596477678299305305004621851762922671748

The implementation returns probability 0.44715659335602981 and
logLik −3379063.9196042535. Relative probability error is
2.56×10⁻¹²; relative likelihood error is below 10⁻¹⁵.
The reference calculation independently profiles sizes 4,…,24.
Global coverage comes from the implemented tail/prefix certificate,
rather than that finite reference sweep.

For the rate-six example, Q has weights (24,144,432,864,1296), total 2760.
Exact arithmetic gives

$$
A=B=0,\quad C=-226332657/36450495095,\quad
R=26842248788378739/650233091900686.
$$

Its cutoff is 26,593 and its ledger uses 11,163,059 cells.
The independent face likelihood is
−546802.89364569380543840691443836; the returned upper bound is
−546802.89364519541. Size 669 is a retained finite witness, not an
asserted finite maximizer.

## Near-critical held inflation: proved signs, retained stops

Let π be held inflation for the stated ten-observation ZIP sample.
Its face score is

$$
F(\lambda)=10/\lambda-5-5w(\lambda),\qquad
w(\lambda)=\frac{(1-\pi)e^{-\lambda}}{\pi+(1-\pi)e^{-\lambda}}.
$$

Every root lies in [1,2], where
F′≤−10/4+5/4<0; signs outside this interval establish a unique global
face rate. At that root the first finite-size coefficient is
A=5λ−8. At λ=8/5 the sign of F is the sign of
D=π−3(1−π)exp(−8/5). Consequently sign(A)=sign(D).

The oracle brackets exp(−8/5) between its alternating Taylor sums of orders
63 and 64 using exact fractions. Substitution of the exact binary64
inflations gives a strictly negative rational upper bound for the lower
input's D and a strictly positive rational lower bound for the upper
input's D. Thus the opposite signs are proved offline, rather than inferred
by rounding a near-zero double calculation.

| Exact binary64 inflation | 80-digit face rate, rounded here | First coefficient, rounded here |
|---|---|---|
| 6795294253129657 / 18014398509481984 | 1.59999999999999995263339828986847 | −2.36833008550657625×10⁻¹⁶ |
| 3397647126564829 / 9007199254740992 | 1.60000000000000002725266277856942 | +1.36263313892847083×10⁻¹⁶ |

The signs in this table have the exact proof above; the displayed magnitudes
are numerical. At the real critical inflation
3exp(−8/5)/(1+3exp(−8/5)), the rate is 8/5 and w=1/4.
The exact face curvature, mixed derivative and unprofiled second coefficient
are −95/32, 6/5 and −1112/375. The envelope correction therefore gives
B=−1112/375−(6/5)²/(2(−95/32))=−776/285.

Analyticity near 1/N=0 and A>0 prove that the upper input has a finite
improvement over its ZIP face. Since its profile tends to the face as
N grows, a finite global maximizing size exists. This is an existence
statement, not a computed maximizer or coefficient certificate. The finite-size
score likewise confines its maximizing rate Np to [1,2]; the finite-size
likelihood converges uniformly to the ZIP likelihood on that interval.
For A<0 the face wins locally at infinity; that alone says nothing about
the entire finite prefix.

The full 80-digit likelihood calculation independently profiles seven
sizes from 10⁴ through 10¹⁸ for each input. At N=4×10¹⁶ the upper input's
likelihood exceeds the face numerically by
1.704828461356264776×10⁻³³. The lower input's seven sampled gaps are
negative. These are numerical spot checks, not global coverage.
The upper input's formal quadratic scale −2B/A is
3.9963904293197859×10¹⁶, with formal gain 1.7048298496205746×10⁻³³.
Neither formal value is returned as an estimate.

Both production fits still expose kind “unresolved”, model “zip”, the
exact held inflation, at least two candidates, an infinite likelihood upper
bound and no budget-exhausted flag. The legacy C++ ZipLimit enum is qualified
by comparison_tie=true; the binding gives that flag precedence and does
not claim a proved limit. R and JS regression tests check this distinction.

## Implementation, stops and scope

The new arithmetic and remainder machinery lives in
[tbinom/face_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/face_certificate.hpp).
Histogram profiling and complete coverage are called directly from
[tbinom_design_mle](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/mle.hpp).
There are no unused production certificate helpers.
The [binding-neutral tbinom route](../../../bindings/common/distributions/tbinom/fit.hpp)
now describes an unresolved “global likelihood comparison”, since competing
finite sizes can also be ambiguous.

Incomplete coverage, arithmetic refusal and numerical failure cannot accept
the proposer as Ok. Budget stops retain witnesses and an infinite upper
bound. Tests force exhaustion before the certificate and during its prefix,
including retention of the face, finite incumbent and finite competitor.
A found finite improvement in the negative-tail route is still passed
through the mandatory finite global certificate.

The internal work ledger uses the standard solver, certificate and enumeration
units. The existing tbinom API has no iterlim argument; the explicit
[no-iterlim exception](../../design/work-budgets.md) retains its documented
default maxit 512: 65,536 certificate evaluations and 51,200,000 cells.
New solver loops use that ledger's solver budget. Degree-eight truncation
is an analytic remainder formula, not a capped search. Cutoffs at or above
2⁵² refuse a numerical certificate. Overflow refuses exact recognition.

No shared core header changed. The shared JS test file is append-only, as
are the two family C++ tests and two R test files. No changes were made to
dpqr.hpp, special/, truth files, code generators, NEWS, status files or roadmap.
All four frozen goldens are byte-identical to the base and their regression
checks pass within tolerance. No disagreeing certified golden fit was found.
The new boundary reference values above are recorded independently instead
of introducing or editing a golden.

Evaluator consistency is preserved: a held size of 10¹⁰⁰ has fitted
likelihood agreeing with the sum of dzib log densities; qzib satisfies
both sides of its CDF jump at probabilities .01, .5 and .99 in the new
check. The existing large-size representability and exact-jump tests also
pass. No settled evaluator change is needed for the delivered certificates.

## Counterexamples to uniqueness

These statements are exact, independent of numerical search.

* ztbinom: with size 1 and all observations 1, every probability in (0,1)
  gives the same point mass and likelihood one.
* zmbinom: with all observations zero and zero probability p0=1, the
  positive-count base parameters are unidentified.
* zib: at size 1, data (0,1,1) have the same saturated Bernoulli law for
  (prob,π)=(2/3,0) and (1,1/3).
* tbinom: on the held window {2,3}, data (2,2,3,3) attain likelihood
  2⁻⁴ for every integer N≥3 with p=3/(N+1). The ratio of the two
  retained masses is ((N−2)/3)p/(1−p)=1. Thus each law attains the
  empirical multinomial upper bound.

The size-seven result has a separated integer winner and a unique interior
probability on its held window. This does not imply uniqueness across all
hold subsets or across equivalent real truncation endpoints.

## Timings

Local GCC 15.2, C++17, -O2, one CPU slot under nice 10. Sample construction
and compilation are excluded. Boundary probes are one-shot measurements;
small-sample entries are means of three calls. The workstation was shared,
so these are measurements rather than a statistical speed claim.

| Probe | Before, ms | After, ms | Outcome change |
|---|---:|---:|---|
| Rate-four boundary | 15886.229 | 12839.300 | Budget stop → certified size 7 |
| Rate-six boundary | 2527.158 | 2109.564 | Unresolved → certified limit |
| Lower adjacent ZIP inflation | 10.737 | 7.551 | Unresolved in both |
| Upper adjacent ZIP inflation | 9.461 | 7.136 | Unresolved in both |
| ztbinom face | 4.400 | 4.369 | Regression |
| zmbinom face | 4.425 | 4.394 | Regression |
| zib face | 6.228 | 6.411 | Regression |
| tbinom face | 8.472 | 8.464 | Regression |
| ztbinom finite | 1.152 | 1.149 | Regression |
| tbinom finite | 5.207 | 5.766 | Regression |
| tbinom zero-inclusive | 4.704 | 4.484 | Regression |

Before uses the base versions of the two changed tbinom library headers;
all other library headers are unchanged. Inputs, probe code and raw output:
[boundary.cpp](2026-11-09-binom-near-data/boundary.cpp),
[before](2026-11-09-binom-near-data/boundary-before.txt),
[after](2026-11-09-binom-near-data/boundary-after.txt),
[timing.cpp](2026-11-09-binom-near-data/timing.cpp),
[small before](2026-11-09-binom-near-data/timing-before.txt),
[small after](2026-11-09-binom-near-data/timing-after.txt).

## Final validation

Every required final command finished with exit code zero. Commands were
run individually in the foreground and waited through their CPU reservations.
R and the long standalone full suites used simbox; local commands used
run4. The independent oracle used one local run slot. No simbox fallback
was needed for these final runs.

| Command | Machine | Result | Test elapsed, s |
|---|---|---|---:|
| [just test](2026-11-09-binom-near-data/logs/r-dev.log.gz) | simbox, R/GCC 15.2 | FAIL 0, WARN 0, SKIP 154, PASS 22172 | 89.953 |
| [just test-full](2026-11-09-binom-near-data/logs/r-full.log.gz) | simbox, R/GCC 15.2 | FAIL 0, WARN 0, SKIP 0, PASS 45004 | 366.276 |
| [just cpp-test](2026-11-09-binom-near-data/logs/cpp-dev.log.gz) | Workstation, GCC 15.2 | 106/106 | 199.09 |
| [UMVL_TEST_TIER=full just cpp-test](2026-11-09-binom-near-data/logs/cpp-full.log.gz) | simbox, GCC 13.3 | 129/129 | 207.04 |
| [just cpp-test-no-long-double](2026-11-09-binom-near-data/logs/cpp-no-long-double-simbox.log.gz) | simbox, GCC 13.3 | 129/129 | 206.11 |
| [just cpp-test-no-long-double](2026-11-09-binom-near-data/logs/cpp-no-long-double-local.log.gz) | Workstation, GCC 15.2 | 129/129 | 311.25 |
| [just js-build](2026-11-09-binom-near-data/logs/js-build.log.gz) | Workstation, Emscripten | Passed | Build wall 96.32 |
| [just js-test](2026-11-09-binom-near-data/logs/js-test.log.gz) | Workstation, Node/WASM | 556/556, zero failures/skips | 170.276 |
| [just site-check](2026-11-09-binom-near-data/logs/site-check.log.gz) | Workstation | 99 pages and 99 C++ snippets; links, MathML and assets passed | Build/check wall 100.17 |
| [Independent Fraction/mpmath oracle](2026-11-09-binom-near-data/logs/oracle.log.gz) | Workstation | Exact identities/signs and all numerical checks passed | Wall 0.82 |

The C++ full, simbox no-long-double and workstation no-long-double
build/test wall times inside their reservations were 291.17, 285.30 and
429.26 seconds. R dev/full outer wall times were 4357.64/804.72 seconds;
C++ dev outer wall was 6069.53 seconds. Those outer measurements include
waiting/build overhead and are not fitter timings.

The [machine-readable matrix](2026-11-09-binom-near-data/final-tests.json)
records each compressed final log. The
[oracle](2026-11-09-binom-near-data/check.py) uses exact fractions and 80-digit
mpmath. Its [results](2026-11-09-binom-near-data/check.json) include exact
third coefficients, 48 numerical checks of the uniform Jensen inequality,
the rational ZIP sign bounds, and 14 huge-size ZIP spot checks.
[Independent profiles](2026-11-09-binom-near-data/profiles.json) contain
42 finite-window profiles and face likelihoods. These numerical checks
support the algebra; they do not replace its global proof.

Reproduce the independent calculations from this worktree with:

    /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-09-binom-near-data/check.py > dev/research/evaluations/2026-11-09-binom-near-data/check.json
    /tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-09-binom-near-data/validate.py

The [validator](2026-11-09-binom-near-data/validate.py) checks frozen
golden bytes, append-only tests, independent coefficient agreement,
certificate cutoffs/ledgers, source scope and report links.
[Validation results](2026-11-09-binom-near-data/validation.json),
[golden hashes](2026-11-09-binom-near-data/golden-integrity.json) and
[source hashes](2026-11-09-binom-near-data/source-manifest.json) are retained
with the report. Final log archives live beside these files, not under build/.

## Open entries

1. **Runtime global certification of the adjacent held-inflation ZIP inputs.**
   The exact leading signs and the upper input's finite-improvement existence
   are settled above. What is missing from the fitter is a portable,
   sufficiently accurate enclosure of the leading coefficient and profiled
   rate, a quantitative remainder uniform over that rate, complete finite-size
   exclusion/comparison, and a maximizing-size/probability enclosure meeting
   the coefficient tolerance. The scale beyond 2⁵³ needs explicit integer
   treatment or a justified tolerance enclosure; rounding the formal scale
   is not a certificate. A gain of order 10⁻³³ cannot be resolved by ordinary
   binary64 likelihood subtraction. For the lower input, even a negative
   first coefficient does not exclude a remote finite winner. Both calls
   correctly stop unresolved today, preserving their evidence.

2. **General higher-order boundaries beyond the recognized rational window.**
   The new certificates require a window starting at zero, a checked rational
   mean identity, exact second-order cancellation and successful complete
   coverage. Overflow, unrecognized identities, a third coefficient whose
   padded sign is undecided, or vanishing third coefficient do not acquire
   a certificate. A zero third coefficient needs a further signed expansion
   and uniform remainder, followed by global finite-size coverage; positive
   lower truncation windows need a suitable exact recognizer or different
   proof. The existing mandatory routes or structured budget/numerical stops
   remain in force. No general uniqueness or all-subset certification is
   claimed for any of the four families.
