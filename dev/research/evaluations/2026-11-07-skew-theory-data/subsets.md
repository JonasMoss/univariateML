# Every public free subset

The tables cover all 16 subsets of each family, including all-held evaluation.
An omitted coefficient is held at its supplied proper value. These are routes
for a future implementation, not claims that the present multistart fitters
are certified. Proof section references refer to [proofs.md](proofs.md).

`P` = fixed-nu polynomial/algebraic enumeration (sections 1–3).
`C` = uniform nuisance compactness (sections 4–5 for sstd; 8 for st).
`N` = normal-face bound/profile (section 9 for st).
`H` = half-face with correct endpoint layer (sections 6 and 8).
`B` = compact-box upper/lower bound search, with certified root counts where useful.
`E` = collapse equality: section 13 gives the exact scalar sstd face;
section 14 closes the st reduced objective by off-tie sign cases. Ordinary
compactness does not apply at equality.
“Complete route” means the mathematical candidate and escape structure is
specified in the stated strict regime; arithmetic and implementation remain.
No numerical grid supplies a missing upper bound.

## sstd: public (M,S,nu,xi)

Free-nu strict regime is 3k_max<2n; for held nu use k_max<nu*n/(nu+1),
or a smaller reachable tie count when justified by the holds. These conditions
are sufficient for the uniform exterior argument, not necessary for every
held subset. S held plus nu held already precludes collapse.

| Free subset | Concrete route | What remains open / implementation obligation |
|---|---|---|
| none | Evaluate the finite density, no optimization. | None mathematically. |
| M | P: degree <=2n-1 on location cells, all roots and seams; tails zero. | Root isolation with computed coefficients and tie comparison; complete route for all data. |
| S | P: equation (4), degree <=2n on each positive inverse-scale cell, explicit infinity limit. | Implement root isolation and finite equality-limit comparison; complete scalar route, no assumed unimodality. |
| nu | One shape coordinate nu>2 with M,S,xi fixed; N-type snorm limit, nu=2 forces W->0. | Uniform shape-face remainder under the held moments; E at critical ties at M. No polynomial df reduction. |
| xi | P: r=xi², y²=Q(r), scalar algebraic stationary equation; H at r=0,infinity uses (10). | Algebraic root isolation and endpoint tie factors; complete fixed-nu scalar route, including both held moments. |
| M,S | P: fixed ratio b=xi²a, 2D rational scores; C; no half-face. | Polynomial critical-component enumeration and collapse E if threshold equality; complete in strict regime. |
| M,nu | Fixed S,xi: enumerate location branches conditionally on nu, shape B, normal face; nu=2 width collapse. | Uniform df bounds including the moment-constrained normal face, and E. |
| M,xi | P: held S ellipse, c free; 2D critical strata; both H endpoints reachable with multiplier 1. | Critical-component and half-face optimization; complete fixed-nu route for all data since S precludes collapse. |
| S,nu | M,xi held: inverse-scale polynomial branch enumeration for each nu; B over shape, C. | Shape-tail remainder, nu=2 ordinary-scale t2 constrained by M, and E at M. |
| S,xi | P: eliminate c=M-h(b-a); 2D critical strata, C; H with M constraint, multiplier 1 attainable. | Critical-component and constrained half-face enumeration; complete in strict regime; E otherwise. |
| nu,xi | Both moments held: shape/skew B, H and normal corners; nu=2 excluded in strict regime. | Uniform face remainders and E. Section 6 proves full endpoint factor at finite free-nu faces, but a penalty at the joint normal corner; neither can be replaced by the fixed-nu factor. |
| M,S,nu | Held xi: ordinary two-piece c,scale,df search; C; t2 and snorm faces, no half-face. | Uniform df remainder and certified shape profile over polynomial inner branches; E. xi=1 delegates exactly to std, with its own parent limitations. |
| M,S,xi | P: 3D rational critical strata; C; H with free mode, multiplier 1. | Implement 3D component and half-face enumeration; complete fixed-nu structure in strict regime; E otherwise. |
| M,nu,xi | S held: C, H and normal faces; no positive-width t2. M free makes endpoint multiplier 1 attainable. | Uniform df/skew corner enclosures, joint B search, E. No extra held-moment endpoint obstruction. |
| S,nu,xi | M held: C, t2/H/normal intersections respecting M; endpoint multiplier 1 attainable by adjusting scale. | Uniform shape/skew boundary error bounds and joint B search; E at held M. |
| M,S,nu,xi | C, full coordinate closure of section 5, all t2/H/normal faces and their intersections; free endpoint multiplier 1. | Uniform face remainders, global B/profile implementation; E. No proof of unique all-free MLE is asserted. |

A held xi=1 removes skewness entirely, and every remaining subset is exactly
a std held subset. Use the appropriate parent certificate if one exists;
do not silently treat all parent held-subset algorithms as already certified.
For fixed nu, P means finite semialgebraic critical **components**, not a
claim that a generic resultant has only simple isolated roots.

## st: public (l=xi, omega, alpha, nu)

Free nu means the existing restriction nu>=1, including its attained lower
face with a one-sided score condition. Held nu may be any positive value.
C applies for 2k_max<n with free nu, or nu(n-k)>k with held nu. If omega
is held, collapse is impossible and C's lower-width argument is unnecessary.
When l is held, only its actual tie can collapse.

| Free subset | Concrete route | What remains open / implementation obligation |
|---|---|---|
| none | Finite density evaluation. | None mathematically. |
| l | Scalar compact location search; for alpha=0 degree <=2n-1 polynomial; otherwise analytic roots/B. | Root counts or upper bounds for Student-CDF objective; complete location tails, ties allowed because omega held. |
| omega | Scalar B; explicit collapse exponent, infinity tail; alpha and nu held. | Root/enclosure implementation; E if nu(n-k)=k; monotonicity is not presumed. |
| alpha | Section 7: one-sign monotonic face stop; all-zero flat objective; mixed-sign explicit bracket and finite analytic stationary set. | A Student-CDF root-count/enclosure implementation, including multiple roots; complete mathematical scalar route. |
| nu | Scalar nu>=1 B plus N; omega,l,alpha held. | Uniform error majorants from section 9 and certified scalar shape search; include nu=1. |
| l,omega | C, 2D B with fixed alpha,nu; no shape/skew faces. At alpha=0 exact lst submodel. | Compact global bounds/root exclusion; E; no joint uniqueness claim. |
| l,alpha | omega,nu held: compact location; conditional alpha bracket and H, endpoint factor 2^k reachable because l free. | Uniform boxes crossing l=x_j; global finite-box and H profiling; complete face structure. |
| l,nu | omega,alpha held: compact location; B and N, nu=1. | Certified 2D compact search and normal-face profiler; omega held removes E. |
| omega,alpha | l,nu held: C, conditional alpha bracket, H. If l is held at endpoint use factor 1 per tie, not 2. | Uniform compact search; E if collapse exponent zero; complete half-face values in strict regime. |
| omega,nu | l,alpha held: C, B and N, nu=1. | Compact search and normal-face profiler; E at held l. |
| alpha,nu | l,omega held: conditional alpha search; H, N and HN corners with endpoint correction for held l. | Certified compact shape search and normal-face profiler; uniform nu->infinity bound already covers unbounded alpha. |
| l,omega,alpha | nu held: C, B and H with free endpoint multiplier 1. | Certified 3D compact search/conditional profiling; E; complete escape structure in strict regime. |
| l,omega,nu | alpha held: C, B, N, nu=1. | Certified 3D compact search and constrained sn face; E. |
| l,alpha,nu | omega held: location compact, B,H,N,HN,nu=1; free endpoint multiplier 1. | Certified compact search and normal-face profiler; no scale-collapse gap. |
| omega,alpha,nu | l held: C,B,H,N,HN,nu=1; held-endpoint factor 1. | Certified compact search/normal-face profiler; E at held l; uniform normal error covers joint shape/skew. |
| l,omega,alpha,nu | C,B,H,N,HN,nu=1, free endpoint multiplier 1. | Global compact search and face profilers; E. Escape coordinates are exhausted in the strict tie regime; uniqueness is not established. |

For held alpha=0, likelihoods reduce exactly to lst in the remaining public
coordinates, with any free-nu restriction retained. An estimated alpha close
to zero does not license that reduction. For fixed alpha,nu the conditional
scale tail can be handled directly with the fixed-shape exponent; passing
an unrelated symmetric monotone-scale proof is not justified.

## Non-strict regimes and exact decisions

* Strictly negative reachable collapse exponent proves unboundedness, subject
  to the actual public holds. sstd with S held requires the nu->2 construction,
  not changing the held sd. st with omega held cannot use that construction.
* Zero exponent: evaluate the reduced collapse faces of proof section 10 and
  compare with all finite branches. This job leaves their **uniform** moving-
  layer arithmetic to the implementation. Section 14 closes st face suprema
  by sign cases, and section 13 closes sstd by a ratio profile, including
  held sd. A finite path limit is never a no-MLE proof.
* One-sided alpha-only st data have a strict monotonic likelihood: a face
  supremum is exact, with the held-endpoint correction. All data at held l
  make alpha unidentifiable; returning a purported unique alpha is wrong.
* Two symmetric location maxima occur in both families even in elementary
  held submodels. Exact or numerically unresolved ties require explicit
  witnesses and a tie/numerical stop under the eventual API.
