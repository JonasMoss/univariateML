# Restrictions and fit domains

## Why restrict a fit

### Policy: existence, not regularity

R5 restricts a shape when the unrestricted family has no MLE for almost any sample, to make fitting possible. The restriction is chosen for existence, not to assert differentiability, finite information, or valid Wald inference. Even a restricted family can have exceptional tied samples or conditional problems without an MLE. The exact domain of proper laws and the domain used for fitting are distinct. [Parameter rules R5](../../dev/roadmap/2026-10-29-parameter-rules.md); [Section “Existence under holds and restrictions” of ML behaviour](../../dev/research/ml-behaviour.tex).

The dossier's `fit_domain` records the bound, mechanism, reason, and any held-set exemptions. It does not redefine the density's full parameter domain. A finite endpoint of the proper domain can be attained; a fitting restriction can also be attained, in which case the R interface flags the restricted fit. Exact bounds and held cases belong to the relevant dossier, and the generated table below reproduces their declarations. [Schema](../include/cppunivariate/distributions/README_SCHEMA.md).

## Four mechanisms

### Collapse (proved for the described paths)

When a location approaches a value repeated $k$ times and scale $\sigma$ tends to zero, the likelihood can behave like $\sigma^{c(n-k)-k}$, with tail exponent $c>0$. A negative exponent gives an unbounded likelihood. Requiring $c\ge1$ prevents this strict divergence whenever fewer than half the observations coincide at the collapsing point; equality and larger ties need their own analysis. For example, Fréchet's free threshold and scale lead to the declared $\lambda\ge1$ fit bound. Holding its threshold changes the problem and is an explicit exemption. [Section “The collapse exponent: a weakest bound” of ML behaviour](../../dev/research/ml-behaviour.tex); [Fréchet dossier](../include/cppunivariate/distributions/frechet/README.md).

### Spike (proved mechanism)

GED can concentrate a spike at one observation as its shape $\nu\to0$. There is no weakest positive bound that eliminates this path in the stated design, so the fit is restricted at the named Laplace member $\nu=1$. Skew-GED has the corresponding restriction; holding standard deviation does not remove the spike. A finite bound chosen this way is a policy choice supported by the mechanism, not an assertion that every excluded value defines an improper density. [Section “No MLE at all: ged and sged” of ML behaviour](../../dev/research/ml-behaviour.tex); [GED dossier](../include/cppunivariate/distributions/ged/README.md).

### Branch (design choice)

Tukey lambda changes the character of its density beyond the principal branch. The fit uses $\lambda\le1$ for that branch; this is not a claim that every distribution beyond one is improper. Its separate lower bound addresses collapse. The dossier records no held-set exemption for the upper bound. [Tukey lambda dossier](../include/cppunivariate/distributions/tlambda/README.md); [parameter rules R5](../../dev/roadmap/2026-10-29-parameter-rules.md).

### Endpoint singularity (proved mechanism)

For GPD and GEV, $\xi<-1$ can make the density diverge at a finite upper support endpoint. Moving that endpoint to the sample maximum makes the likelihood unbounded. Their declared lower fit bound is $\xi\ge-1$. R7a assigns an endpoint density by its limit from inside, allowing an attained proper boundary where appropriate; it does not cure the singularity below the bound. GEV also has a separate upper bound for a scale-collapse mechanism. [GEV dossier](../include/cppunivariate/distributions/gev/README.md); [GPD dossier](../include/cppunivariate/distributions/gpd/README.md); [Section “Spike, branch and endpoint” of ML behaviour](../../dev/research/ml-behaviour.tex).

## Scope and exemptions

### Policy: which fits are restricted

A `fit_domain` applies whenever its coefficient is free, regardless of other holds, except for an explicitly declared `exempt_when_held` set. Holding the restricted coefficient at a value outside the fitting range is a different conditional problem: it can be accepted if that held subset has an MLE and stops otherwise. Holding an exempt coefficient does not remove sample-specific failures or prove the numerical search globally correct. For example, Fréchet with `mu` held is an inverse Weibull on the shifted data for positive shape, but observations at or outside the held support require separate handling. [Parameter rules R5](../../dev/roadmap/2026-10-29-parameter-rules.md); [held parameters](holds.md#meaning-and-allowed-subsets).

### Current declarations

The present dossiers declare restrictions on eight family coefficients: Fréchet `lambda`; GED and skew-GED `nu`; GEV and GPD `xi`; half-t `nu`; location-scale t `df`; and Tukey lambda `lambda`. GEV and Tukey lambda each declare two bounds with different mechanisms. Fréchet, half-t, location-scale t, and the lower Tukey lambda bound list an exemption when `mu` is held. The table is generated from the dossiers, so it provides the precise current bounds and exemption lists. Absence from this table means no `fit_domain` is declared, not that every sample or held subset has a finite MLE. [Schema](../include/cppunivariate/distributions/README_SCHEMA.md); [open existence inventory](../../dev/research/evaluations/2026-10-31-open-existence-report.md).
