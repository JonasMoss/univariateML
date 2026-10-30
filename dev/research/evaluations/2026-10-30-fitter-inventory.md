# Fitter inventory — preliminary source index

Revision `5f11fb9e878219ac47a9a372e985eb69da6198e3`; dossier edits are documentation only. This is a **preliminary index, not a completed fitter audit**. The CSV has one row per dossier, preserving declared domains, fit restrictions, structural `holds:` exceptions, support and declared limits. Empty `holds:` does not establish that every subset is implemented, and missing `limits:` does not prove existence. The broad method labels are navigation aids and may omit hold-specific paths.

Parameter-specific and sample-dependent failure handling must be checked against adapters and implementation; R6 requires a stop or named limit, and R7a gives parameter-dependent endpoint closure. Historical [P0 summary](2026-10-29-p0-summary.md) explicitly distinguishes proofs from numerical comparisons and identifies unresolved global searches. A successful optimizer, small score, golden agreement or multistart oracle does not certify the global maximum. The referenced reports are historical and must not be treated as validation at this revision.

| Family | Method index | Result strength currently reviewed |
|---|---|---|
| [bbinom](../../../univariateML-cpp/include/cppunivariate/distributions/bbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [beta](../../../univariateML-cpp/include/cppunivariate/distributions/beta/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [betapr](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [bhatt](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [binom](../../../univariateML-cpp/include/cppunivariate/distributions/binom/README.md) | integer search | free-size integer profile unimodality and held-probability tail bound documented; budget failure |
| [bnbinom](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [cauchy](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [cmb](../../../univariateML-cpp/include/cppunivariate/distributions/cmb/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [cmp](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [dgamma](../../../univariateML-cpp/include/cppunivariate/distributions/dgamma/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [dlaplace](../../../univariateML-cpp/include/cppunivariate/distributions/dlaplace/README.md) | integer search | not independently classified for every hold; consult cited source |
| [dnorm](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [dunif](../../../univariateML-cpp/include/cppunivariate/distributions/dunif/README.md) | closed form | not independently classified for every hold; consult cited source |
| [dweibull](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [emg](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [exp](../../../univariateML-cpp/include/cppunivariate/distributions/exp/README.md) | closed form | unique closed form for positive sample sum |
| [fatigue](../../../univariateML-cpp/include/cppunivariate/distributions/fatigue/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [foldnorm](../../../univariateML-cpp/include/cppunivariate/distributions/foldnorm/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [fpareto](../../../univariateML-cpp/include/cppunivariate/distributions/fpareto/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [frechet](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [gamma](../../../univariateML-cpp/include/cppunivariate/distributions/gamma/README.md) | scalar solve / profile | unique shape root for nonconstant data; numerical solves |
| [gb2](../../../univariateML-cpp/include/cppunivariate/distributions/gb2/README.md) | profile / multistart search | inner concavity proved; outer search numerical, global certificate open |
| [ged](../../../univariateML-cpp/include/cppunivariate/distributions/ged/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [genbeta](../../../univariateML-cpp/include/cppunivariate/distributions/genbeta/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [geom](../../../univariateML-cpp/include/cppunivariate/distributions/geom/README.md) | closed form | not independently classified for every hold; consult cited source |
| [gev](../../../univariateML-cpp/include/cppunivariate/distributions/gev/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [gompertz](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [gpd](../../../univariateML-cpp/include/cppunivariate/distributions/gpd/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [gumbel](../../../univariateML-cpp/include/cppunivariate/distributions/gumbel/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [hcauchy](../../../univariateML-cpp/include/cppunivariate/distributions/hcauchy/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [hnorm](../../../univariateML-cpp/include/cppunivariate/distributions/hnorm/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [ht](../../../univariateML-cpp/include/cppunivariate/distributions/ht/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [huber](../../../univariateML-cpp/include/cppunivariate/distributions/huber/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [hyper](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md) | integer search | not independently classified for every hold; consult cited source |
| [invburr](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invexp](../../../univariateML-cpp/include/cppunivariate/distributions/invexp/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invgamma](../../../univariateML-cpp/include/cppunivariate/distributions/invgamma/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invgauss](../../../univariateML-cpp/include/cppunivariate/distributions/invgauss/README.md) | closed form | not independently classified for every hold; consult cited source |
| [invparalogis](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invpareto](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invtrgamma](../../../univariateML-cpp/include/cppunivariate/distributions/invtrgamma/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invweibull](../../../univariateML-cpp/include/cppunivariate/distributions/invweibull/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [kumar](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [laplace](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/README.md) | closed form | not independently classified for every hold; consult cited source |
| [lgamma](../../../univariateML-cpp/include/cppunivariate/distributions/lgamma/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [lgser](../../../univariateML-cpp/include/cppunivariate/distributions/lgser/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [llogis](../../../univariateML-cpp/include/cppunivariate/distributions/llogis/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [lnorm](../../../univariateML-cpp/include/cppunivariate/distributions/lnorm/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [logis](../../../univariateML-cpp/include/cppunivariate/distributions/logis/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [logitnorm](../../../univariateML-cpp/include/cppunivariate/distributions/logitnorm/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [lomax](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [lst](../../../univariateML-cpp/include/cppunivariate/distributions/lst/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [naka](../../../univariateML-cpp/include/cppunivariate/distributions/naka/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [nbinom](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [nhyper](../../../univariateML-cpp/include/cppunivariate/distributions/nhyper/README.md) | integer search | not independently classified for every hold; consult cited source |
| [norm](../../../univariateML-cpp/include/cppunivariate/distributions/norm/README.md) | closed form | not independently classified for every hold; consult cited source |
| [nsbeta](../../../univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [paralogis](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pareto](../../../univariateML-cpp/include/cppunivariate/distributions/pareto/README.md) | closed form | not independently classified for every hold; consult cited source |
| [pareto2](../../../univariateML-cpp/include/cppunivariate/distributions/pareto2/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pareto3](../../../univariateML-cpp/include/cppunivariate/distributions/pareto3/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pareto4](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pig](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pois](../../../univariateML-cpp/include/cppunivariate/distributions/pois/README.md) | closed form | not independently classified for every hold; consult cited source |
| [power](../../../univariateML-cpp/include/cppunivariate/distributions/power/README.md) | closed form | not independently classified for every hold; consult cited source |
| [rayleigh](../../../univariateML-cpp/include/cppunivariate/distributions/rayleigh/README.md) | closed form | not independently classified for every hold; consult cited source |
| [sged](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [sgomp](../../../univariateML-cpp/include/cppunivariate/distributions/sgomp/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [skellam](../../../univariateML-cpp/include/cppunivariate/distributions/skellam/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [slash](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [sn](../../../univariateML-cpp/include/cppunivariate/distributions/sn/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [snorm](../../../univariateML-cpp/include/cppunivariate/distributions/snorm/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [sstd](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [std](../../../univariateML-cpp/include/cppunivariate/distributions/std/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tbinom](../../../univariateML-cpp/include/cppunivariate/distributions/tbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tlambda](../../../univariateML-cpp/include/cppunivariate/distributions/tlambda/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tnorm](../../../univariateML-cpp/include/cppunivariate/distributions/tnorm/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tpois](../../../univariateML-cpp/include/cppunivariate/distributions/tpois/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [trgamma](../../../univariateML-cpp/include/cppunivariate/distributions/trgamma/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [triang](../../../univariateML-cpp/include/cppunivariate/distributions/triang/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [unif](../../../univariateML-cpp/include/cppunivariate/distributions/unif/README.md) | closed form | not independently classified for every hold; consult cited source |
| [weibull](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [yulesimon](../../../univariateML-cpp/include/cppunivariate/distributions/yulesimon/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [zeta](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [zib](../../../univariateML-cpp/include/cppunivariate/distributions/zib/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zinb](../../../univariateML-cpp/include/cppunivariate/distributions/zinb/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zip](../../../univariateML-cpp/include/cppunivariate/distributions/zip/README.md) | closed form | not independently classified for every hold; consult cited source |
| [zipf](../../../univariateML-cpp/include/cppunivariate/distributions/zipf/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [zmbinom](../../../univariateML-cpp/include/cppunivariate/distributions/zmbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmgeom](../../../univariateML-cpp/include/cppunivariate/distributions/zmgeom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmlogarithmic](../../../univariateML-cpp/include/cppunivariate/distributions/zmlogarithmic/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmnbinom](../../../univariateML-cpp/include/cppunivariate/distributions/zmnbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmpois](../../../univariateML-cpp/include/cppunivariate/distributions/zmpois/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [ztbinom](../../../univariateML-cpp/include/cppunivariate/distributions/ztbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [ztgeom](../../../univariateML-cpp/include/cppunivariate/distributions/ztgeom/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [ztnbinom](../../../univariateML-cpp/include/cppunivariate/distributions/ztnbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [ztpois](../../../univariateML-cpp/include/cppunivariate/distributions/ztpois/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |

The CSV records unknown rationale explicitly. Complete this audit by checking each method label and each held subset against source and family-specific reports, documenting actual boundaries, stopping and failure behavior, and recovering historical rationale. In particular free-size bbinom/cmb and truncated or inflated binomial searches, global profile search, and unresolved existence questions remain open.
