# Limit models and escapes

## Closure and escape

### Definition: Hellinger distance

For densities with respect to a common measure, $H^2(P,Q)=\frac12\int(\sqrt p-\sqrt q)^2$. The research source calls a family Hellinger-closed when every Hellinger limit is a family member or a point mass. An MLE *escapes* when the likelihood supremum is not attained and every maximizing sequence approaches a limit outside the family. Non-closure is necessary for this kind of escape, but is not sufficient: inverse Gaussian has a nontrivial limit and nevertheless has a finite MLE on its ordinary data domain. [Section “Definitions” and subsection “Not closed, but the MLE always exists” of ML behaviour](../../dev/research/ml-behaviour.tex).

### Proposition (proved): topology and point masses

Hellinger and total variation induce the same topology on probability laws: for the normalization above, $H^2\leq\lVert P-Q\rVert_{\mathrm{TV}}\leq\sqrt2 H$. Thus convergence in either metric implies convergence in the other. The inequalities follow from factoring $p-q=(\sqrt p-\sqrt q)(\sqrt p+\sqrt q)$ and applying Cauchy–Schwarz.

A continuous location-scale density collapsing to a point can converge *weakly* to a point mass without converging to it in either metric: each continuous law gives the singleton probability zero, while the point mass gives it probability one. Consequently “closure apart from point masses” is a convention about degeneracies, not a claim that a point mass is a Hellinger limit of those continuous laws. Discrete point masses can instead be attained or approached in total variation. [Section “Definitions” of ML behaviour](../../dev/research/ml-behaviour.tex); [parameter rules R7](../../dev/roadmap/2026-10-29-parameter-rules.md).

## Three paths to familiar models

### Limits (proved for the stated paths)

For Student $t$, degrees of freedom $\nu\to\infty$ with location and ordinary scale fixed give a normal density. This is a limit of distributions, not a statement that every data set selects the normal face. The standardized `std` family also has a distinct lower-degree face; [Section “Escape, formerly silent; now detected by comparing faces” of ML behaviour](../../dev/research/ml-behaviour.tex) explains the parameterization and caveats.

For Lomax, sending $\kappa\to\infty$ and $\lambda\to0$ with $\lambda\kappa\to\theta>0$ gives an exponential law of rate $\theta$. The survival function $(1+\lambda x)^{-\kappa}$ tends to $e^{-\theta x}$. Whether the fitted exponential actually beats every finite Lomax fit is a sample-dependent global question, answered mathematically by the [Lomax criterion](existence.md#lomax-theorem), not by the existence of the limiting path alone.

For a negative binomial of size $r$ and success probability $p$, $r\to\infty$, $p\to1$ with $r(1-p)/p\to m>0$ gives Poisson$(m)$. The probability generating function tends to that of Poisson. For positive sample mean, Aragón, Eberly and Eberly (1992) prove that the joint finite MLE exists uniquely exactly when the empirical variance with divisor $n$ exceeds the mean; otherwise the Poisson face supplies the supremum. The all-zero sample is a separate attained point-mass case. The [negative-binomial report](../../dev/research/evaluations/2026-10-31-existence-2-report.md) also records a numerical large-size diagnostic gap. These three limits and their declarations are listed in the [dossier catalogue](#declared-limits).

## Collapse and endpoints

### Proposition (proved in the stated examples)

For a normal sample with every observation equal to $c$, taking location $c$ and scale $\sigma\downarrow0$ sends likelihood to infinity. The limiting point mass is weak, not Hellinger, as explained above. Other families respond differently to ties: in Cauchy the relevant exponent depends on multiplicity, and equality needs separate analysis. See [existence and uniqueness](existence.md#what-can-fail) and [Section “Closed apart from point masses” of ML behaviour](../../dev/research/ml-behaviour.tex).

A moving support endpoint is another boundary mechanism. For GPD and GEV, shape values below $-1$ can produce a density singularity when the upper endpoint is placed at the sample maximum. R5 restricts the fit; R7a defines the density at a parameter-dependent support endpoint by the limit from within the support. Thus a finite endpoint with an attained proper law can be returned as a member, rather than treated as an escape merely because an older reference used an open support. This convention does not make a singular or improper law a finite MLE. [Section “Existence under holds and restrictions” of ML behaviour](../../dev/research/ml-behaviour.tex); [restrictions](restrictions.md#why-restrict-a-fit).

## Statistical interpretation

### Interpretation (conditional, not a universal existence theorem)

Under misspecification, expected log likelihood differs from minus the Kullback–Leibler divergence from the data-generating distribution by a constant, when the relevant expectations and densities are well defined. A sequence of fitted family members can approach a KL-optimal law in an enlarged closure even though no finite original parameter represents it. Such a closure projection is an informative interpretation of a population escape, but Hellinger convergence alone does not guarantee convergence of KL divergences or empirical log likelihoods. Nor does a finite-sample stop by itself prove a population projection or its uniqueness. [Section “Definitions” of ML behaviour](../../dev/research/ml-behaviour.tex).

### Policy: interpreting “use mlX”

D1 reports a detected escape by stopping and naming the corresponding limit fitter, when one exists. Fit that model separately if it answers the question: for example, `mlexp(x)` fits the exponential face of Lomax. Its finite parameter values belong to a different family, so they should not be read as a very large estimated original shape. A stop without a named target reports a reason or an unavailable limit instead. Numerical comparisons can have tolerances or incomplete global guarantees; consult the family dossier and do not infer an exact existence proof from the message. [Parameter rules R6–R7](../../dev/roadmap/2026-10-29-parameter-rules.md); [Lomax numerical limitation](existence.md#numerical-limitation).

## Declared limits

The catalogue below is generated from `limits` in the dossiers. Rows with a model name point to a package family; other rows describe named limits or failure boundaries. A `fixed` set makes a row conditional on held coefficients. A declaration is not itself a proof of global optimality or a claim that every boundary is detected. The [schema](../include/cppunivariate/distributions/README_SCHEMA.md) specifies these fields.
