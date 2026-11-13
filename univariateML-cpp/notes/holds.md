# Held parameters

## Meaning and allowed subsets

### Policy: supplied values and estimates

Holding a coefficient supplies its value; `NULL` requests estimation. Each subset defines a conditional optimization problem, so a statement for the joint fit need not apply to it. Design and threshold coefficients require an explicit value or `NULL`; regular coefficients and endpoints default to estimation. No coefficient defaults to a held value. A required argument is therefore not the same as a coefficient that must always be held. [Parameter rules R2–R4](../../dev/roadmap/2026-10-29-parameter-rules.md).

The catalogue below lists every family's coefficients and its declared structural exceptions. The listed held set is exact: an empty set describes all coefficients free. A `no-mle` row reports a structural obstacle; `not-implemented` records missing computation, not a mathematical nonexistence theorem. Other subsets are permitted by the rule, subject to the actual sample, valid support and numerical success. Conformance tests exercise held subsets on golden samples, not all samples. [Schema](../include/cppunivariate/distributions/README_SCHEMA.md); [parameter rules A7](../../dev/roadmap/2026-10-29-parameter-rules.md).

## Conditional limits

### Worked example (proved limit; detection depends on the sample)

Consider a normal law truncated to fixed finite bounds $a<b$. With its mean and standard deviation free, one possible large-standard-deviation limit is a truncated exponential: as $\sigma\to\infty$ and $\mu/\sigma^2\to r$, its density on the interval approaches a density proportional to $e^{rx}$. The quadratic term in the log density vanishes while the linear term remains.

If instead $\mu$ is held at a finite value and only $\sigma$ increases, $\mu/\sigma^2\to0$. The same path now yields the *uniform* distribution on the fixed interval. The normal dossier’s truncation scope has a `fixed: [mean]` uniform limit and a data-dependent detection condition; this does not mean that the uniform face wins for every sample. The held face intersects a smaller part of the original closure and changes the limit that can be reached. [Truncated-normal dossier](../include/cppunivariate/distributions/norm/README.md); [Section “Existence under holds and restrictions” of ML behaviour](../../dev/research/ml-behaviour.tex).

A face need not be a literal finite coordinate boundary: in this example it is the subset with a coordinate fixed before a different coordinate tends to infinity. Conversely, a `fixed` record states its required held set; it does not prove that no other path reaches the same model. Some faces can be unbounded or numerically unresolved rather than a named fit. The [limit catalogue](limits.md#declared-limits) preserves those distinctions.

## Rules in use

### User consequences (R1–R4)

R1 follows the density's coefficient names, with declared translations and reparameterizations. For example, the hypergeometric population size `N` is a design quantity even though reference parameters may be expressed differently. R2 permits any held subset unless the dossier declares a structural exception; it does not promise existence for every sample. Under R3 and R4, a threshold or design argument must be supplied explicitly, while a regular parameter or endpoint is estimated by default. Passing `NULL` explicitly requests estimation where that conditional fit is available. [Parameter rules](../../dev/roadmap/2026-10-29-parameter-rules.md).

### User consequences (R5–R6)

R5 restricts a free shape for existence where the dossier specifies a `fit_domain`. Holding another coefficient can remove the need for a restriction only in an explicitly listed exempt set. For Fréchet, held `mu` exempts free `lambda` from its lower fitting bound; threshold ties and the actual sample still matter. A held value of the restricted shape outside that fitting range may be accepted if its conditional problem has a maximum. See [fit-domain scope](restrictions.md#scope-and-exemptions).

R6 calls for a stop with a reason or “use mlX” when the sample has no finite fit. An optimizer failure also stops unless its documented free-coordinate score gate is met. A passing score gate is a local numerical criterion; it does not prove global optimality or the existence of a maximum. Some current profile searches have explicit certification gaps, including the [Lomax search](existence.md#numerical-limitation). [Parameter rules R5–R6](../../dev/roadmap/2026-10-29-parameter-rules.md).

### User consequences (R7–R8)

R7 distinguishes finite proper members from excluded infinite parameters and continuous point masses. A discrete zero-rate Poisson point mass is a proper finite member; a normal law with zero scale is not a continuous density member. R7a takes the density at a parameter-dependent support endpoint to be its inside limit, so an attained endpoint can be returned consistently with evaluation. An R5 restriction hit carries the restricted-fit warning and attribute. R8 requires fits to respect the family's location-scale transformations; the held values must be transformed along with the data. These are interface and mathematical design rules, not a universal floating-point error bound. [Parameter rules R7–R8](../../dev/roadmap/2026-10-29-parameter-rules.md); [limits and escapes](limits.md#collapse-and-endpoints).
