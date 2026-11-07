# All eight free subsets

F denotes free coefficients; the complement is held. l is lambda. This is
the mathematical likelihood problem, including samples rejected by the
current sufficient tie guard. It does not authorize changing default fits.
Proof section numbers refer to [proofs.md](proofs.md).

| F | Domain and existence/escape decision | Concrete certificate route | Remaining issue |
|---|---|---|---|
| empty | Finite held parameters, sigma>0. No maximization; density may be zero because support misses data. | Closed-support evaluation with inverse brackets (1,7). | Robust numerical brackets must be implemented/validated; no mathematical optimization gap. |
| mu | Arbitrary finite held l and held sigma>0. If l>0 require range<2sigma/l for 0<l<1, and range<=2sigma/l for l>=1. Otherwise no positive-likelihood fit. At l<=0 always attained. | l in [0,1): strict concavity and scalar score bracket. l=1,2: exact feasible location interval. 1<l<2: compare its two endpoints. l<0 or l>2: finite location bound and scalar box certificate (3,4,8). | No general uniqueness; exact counterexamples in section 10. No missing escape face. |
| sigma | Held mu and arbitrary finite l. For l<0 let a=-l, k=count(x=mu), e=(n-k)/a-k. e<0: unbounded; e=0: unattained collapse supremum (5), strictly dominates every positive scale; e>0: attained. For l>=0: all x=mu gives unbounded likelihood; otherwise attained. | Negative l with e>0: explicit two-ended scale bounds and scalar boxes. 0<=l<1: concave inverse-scale objective. l=1,2: radius uniform fit. 1<l<2: exact minimum feasible scale l*radius. l>2: support lower bound, modal upper bound, scalar boxes. | No unresolved existence case. General negative-shape scale uniqueness is not proved or needed. |
| l | mu and sigma held. Free l in (-infinity,1], including lower-bound exemption even when data contain ties. An attained maximum exists. | Explicit negative-shape cutoff from modal density (5), then scalar boxes plus exact l=1 evaluation. Bounded-support feasibility changes are retained (7,8). | No shape-profile unimodality/uniqueness proof; box certificate does not require one. |
| mu,sigma | l held, arbitrary finite value. l>=0: nonconstant data have an attained maximum; constant data are unbounded. l<0: compute e_j=(n-k_j)/a-k_j. Any e_j<0 gives unboundedness. All e_j>0 give attainment. At l=-1 equality is proved nonattainment by the sharper Cauchy envelope (6). Other zero e_j give the equality faces (5); a finite value strictly above all faces proves attainment. | l in [0,1): joint concavity in A=1/sigma,B=-mu/sigma. l=1,2: exact midrange uniform fit. Negative l strict case: explicit finite box. Positive l>1: support plus nuisance bounds and contact-stratum boxes; 1<l<2 reduces to the two contact curves. | Free-location equality at other held negative shapes, with no strict improvement over the largest collapse face, remains open for a practical general solver; rational nonzero held shape admits an exact algebraic decision route (9). A tolerance tie cannot decide nonattainment. |
| mu,l | sigma held. Free l in [-1,1], even though scale collapse is impossible (this is the existing restriction policy). Always attained if n>0; l=0 gives a positive likelihood for any finite data. | Uniform location-tail exclusion (1), compact shape range, two-dimensional boxes plus restriction faces and exact uniform feasible interval. At each fixed nonnegative l the unique location profile may accelerate proposals. | No general uniqueness or profile unimodality. No missing escape face. Do not extend the free-shape range merely because scale is held. |
| sigma,l | mu held. Free l in (-infinity,1]. Any observation at mu gives unboundedness through a sufficiently negative fixed shape and sigma->0. If no observation equals mu, an attained maximum exists. | Uniform joint shape/scale exclusion (4) yields a data-dependent negative cutoff, then (2) yields explicit positive lower/upper scale bounds. Two-dimensional boxes plus l=1 conditional uniform fit. | No missing nonzero escape face in the no-tie case. General uniqueness is unproved. The joint cutoff is required; a modal bound alone fails with sigma free. |
| mu,sigma,l | Free l in [-1,1]. If 2k_max<n, an attained maximum exists. If 2k_max>n, likelihood is unbounded. If 2k_max=n, compare explicit l=-1 collapse faces; a strict finite improvement proves attainment. | Uniform envelope (1), explicit nuisance bounds (3,4), three-dimensional boxes (8). Optimize fixed nonnegative-shape slices by concavity for proposals; include the full l=-1 face and exact l=1 midrange uniform candidate. | Free-shape/free-location equality-collapse exact decision remains open if no strict finite improvement is established. No general joint uniqueness claim. No need for a hypothetical l=-infinity face, since that range is not eligible. |

The location-only support condition refers to *positive* likelihood. An
infeasible support fit must stop with that reason; it is not an arbitrary
zero-likelihood optimizer. The all-held path can evaluate log likelihood
-infinity without confusing it with a positive-likelihood fit.

For held l>1, zero endpoint densities must not be inherited from the
principal branch: contacts have positive density. For l=1 or 2, all-held
samples outside support are infeasible, and a constant sample with free
scale collapses if its location can coincide with that value. Holding mu
away from a constant sample prevents this collapse.

The equality rows concern only free-scale, free-location negative-tail
problems. Fixed mu and fixed negative l equality is fully settled by the
strict tail inequality. Fixed mu with both scale and shape free is fully
settled by its tie/no-tie split. These cases should not be left behind an
unnecessary general equality comparison.
