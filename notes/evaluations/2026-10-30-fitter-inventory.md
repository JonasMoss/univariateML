# Fitter inventory — preliminary source index

Revision `5f11fb9e878219ac47a9a372e985eb69da6198e3`; dossier edits are documentation only. This is a **preliminary index, not a completed fitter audit**. The CSV has one row per dossier, preserving declared domains, fit restrictions, structural `holds:` exceptions, support and declared limits. Empty `holds:` does not establish that every subset is implemented, and missing `limits:` does not prove existence. The broad method labels are navigation aids and may omit hold-specific paths.

Parameter-specific and sample-dependent failure handling must be checked against adapters and implementation; R6 requires a stop or named limit, and R7a gives parameter-dependent endpoint closure. Historical [P0 summary](2026-10-29-p0-summary.md) explicitly distinguishes proofs from numerical comparisons and identifies unresolved global searches. A successful optimizer, small score, golden agreement or multistart oracle does not certify the global maximum. The referenced reports are historical and must not be treated as validation at this revision.

| Family | Method index | Result strength currently reviewed |
|---|---|---|
| [bbinom](../../cppunivariate/include/cppunivariate/distributions/bbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [beta](../../cppunivariate/include/cppunivariate/distributions/beta/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [betapr](../../cppunivariate/include/cppunivariate/distributions/betapr/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [bhatt](../../cppunivariate/include/cppunivariate/distributions/bhatt/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [binom](../../cppunivariate/include/cppunivariate/distributions/binom/README.md) | integer search | free-size integer profile unimodality and held-probability tail bound documented; budget failure |
| [bnbinom](../../cppunivariate/include/cppunivariate/distributions/bnbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [burr](../../cppunivariate/include/cppunivariate/distributions/burr/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [cauchy](../../cppunivariate/include/cppunivariate/distributions/cauchy/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [cmb](../../cppunivariate/include/cppunivariate/distributions/cmb/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [cmp](../../cppunivariate/include/cppunivariate/distributions/cmp/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [dgamma](../../cppunivariate/include/cppunivariate/distributions/dgamma/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [dlaplace](../../cppunivariate/include/cppunivariate/distributions/dlaplace/README.md) | integer search | not independently classified for every hold; consult cited source |
| [dnorm](../../cppunivariate/include/cppunivariate/distributions/dnorm/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [dunif](../../cppunivariate/include/cppunivariate/distributions/dunif/README.md) | closed form | not independently classified for every hold; consult cited source |
| [dweibull](../../cppunivariate/include/cppunivariate/distributions/dweibull/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [emg](../../cppunivariate/include/cppunivariate/distributions/emg/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [exp](../../cppunivariate/include/cppunivariate/distributions/exp/README.md) | closed form | unique closed form for positive sample sum |
| [fatigue](../../cppunivariate/include/cppunivariate/distributions/fatigue/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [foldnorm](../../cppunivariate/include/cppunivariate/distributions/foldnorm/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [fpareto](../../cppunivariate/include/cppunivariate/distributions/fpareto/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [frechet](../../cppunivariate/include/cppunivariate/distributions/frechet/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [gamma](../../cppunivariate/include/cppunivariate/distributions/gamma/README.md) | scalar solve / profile | unique shape root for nonconstant data; numerical solves |
| [gb2](../../cppunivariate/include/cppunivariate/distributions/gb2/README.md) | profile / multistart search | inner concavity proved; outer search numerical, global certificate open |
| [ged](../../cppunivariate/include/cppunivariate/distributions/ged/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [genbeta](../../cppunivariate/include/cppunivariate/distributions/genbeta/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [geom](../../cppunivariate/include/cppunivariate/distributions/geom/README.md) | closed form | not independently classified for every hold; consult cited source |
| [gev](../../cppunivariate/include/cppunivariate/distributions/gev/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [gompertz](../../cppunivariate/include/cppunivariate/distributions/gompertz/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [gpd](../../cppunivariate/include/cppunivariate/distributions/gpd/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [gumbel](../../cppunivariate/include/cppunivariate/distributions/gumbel/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [hcauchy](../../cppunivariate/include/cppunivariate/distributions/hcauchy/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [hnorm](../../cppunivariate/include/cppunivariate/distributions/hnorm/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [ht](../../cppunivariate/include/cppunivariate/distributions/ht/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [huber](../../cppunivariate/include/cppunivariate/distributions/huber/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [hyper](../../cppunivariate/include/cppunivariate/distributions/hyper/README.md) | integer search | not independently classified for every hold; consult cited source |
| [invburr](../../cppunivariate/include/cppunivariate/distributions/invburr/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invexp](../../cppunivariate/include/cppunivariate/distributions/invexp/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invgamma](../../cppunivariate/include/cppunivariate/distributions/invgamma/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invgauss](../../cppunivariate/include/cppunivariate/distributions/invgauss/README.md) | closed form | not independently classified for every hold; consult cited source |
| [invparalogis](../../cppunivariate/include/cppunivariate/distributions/invparalogis/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invpareto](../../cppunivariate/include/cppunivariate/distributions/invpareto/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invtrgamma](../../cppunivariate/include/cppunivariate/distributions/invtrgamma/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [invweibull](../../cppunivariate/include/cppunivariate/distributions/invweibull/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [kumar](../../cppunivariate/include/cppunivariate/distributions/kumar/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [laplace](../../cppunivariate/include/cppunivariate/distributions/laplace/README.md) | closed form | not independently classified for every hold; consult cited source |
| [lgamma](../../cppunivariate/include/cppunivariate/distributions/lgamma/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [lgser](../../cppunivariate/include/cppunivariate/distributions/lgser/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [llogis](../../cppunivariate/include/cppunivariate/distributions/llogis/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [lnorm](../../cppunivariate/include/cppunivariate/distributions/lnorm/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [logis](../../cppunivariate/include/cppunivariate/distributions/logis/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [logitnorm](../../cppunivariate/include/cppunivariate/distributions/logitnorm/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [lomax](../../cppunivariate/include/cppunivariate/distributions/lomax/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [lst](../../cppunivariate/include/cppunivariate/distributions/lst/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [naka](../../cppunivariate/include/cppunivariate/distributions/naka/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [nbinom](../../cppunivariate/include/cppunivariate/distributions/nbinom/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [nhyper](../../cppunivariate/include/cppunivariate/distributions/nhyper/README.md) | integer search | not independently classified for every hold; consult cited source |
| [norm](../../cppunivariate/include/cppunivariate/distributions/norm/README.md) | closed form | not independently classified for every hold; consult cited source |
| [nsbeta](../../cppunivariate/include/cppunivariate/distributions/nsbeta/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [paralogis](../../cppunivariate/include/cppunivariate/distributions/paralogis/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pareto](../../cppunivariate/include/cppunivariate/distributions/pareto/README.md) | closed form | not independently classified for every hold; consult cited source |
| [pareto2](../../cppunivariate/include/cppunivariate/distributions/pareto2/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pareto3](../../cppunivariate/include/cppunivariate/distributions/pareto3/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pareto4](../../cppunivariate/include/cppunivariate/distributions/pareto4/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pig](../../cppunivariate/include/cppunivariate/distributions/pig/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [pois](../../cppunivariate/include/cppunivariate/distributions/pois/README.md) | closed form | not independently classified for every hold; consult cited source |
| [power](../../cppunivariate/include/cppunivariate/distributions/power/README.md) | closed form | not independently classified for every hold; consult cited source |
| [rayleigh](../../cppunivariate/include/cppunivariate/distributions/rayleigh/README.md) | closed form | not independently classified for every hold; consult cited source |
| [sged](../../cppunivariate/include/cppunivariate/distributions/sged/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [sgomp](../../cppunivariate/include/cppunivariate/distributions/sgomp/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [skellam](../../cppunivariate/include/cppunivariate/distributions/skellam/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [slash](../../cppunivariate/include/cppunivariate/distributions/slash/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [sn](../../cppunivariate/include/cppunivariate/distributions/sn/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [snorm](../../cppunivariate/include/cppunivariate/distributions/snorm/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [sstd](../../cppunivariate/include/cppunivariate/distributions/sstd/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [std](../../cppunivariate/include/cppunivariate/distributions/std/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tbinom](../../cppunivariate/include/cppunivariate/distributions/tbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tlambda](../../cppunivariate/include/cppunivariate/distributions/tlambda/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tnorm](../../cppunivariate/include/cppunivariate/distributions/tnorm/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [tpois](../../cppunivariate/include/cppunivariate/distributions/tpois/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [trgamma](../../cppunivariate/include/cppunivariate/distributions/trgamma/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [triang](../../cppunivariate/include/cppunivariate/distributions/triang/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [unif](../../cppunivariate/include/cppunivariate/distributions/unif/README.md) | closed form | not independently classified for every hold; consult cited source |
| [weibull](../../cppunivariate/include/cppunivariate/distributions/weibull/README.md) | delegation / transform | not independently classified for every hold; consult cited source |
| [yulesimon](../../cppunivariate/include/cppunivariate/distributions/yulesimon/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [zeta](../../cppunivariate/include/cppunivariate/distributions/zeta/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [zib](../../cppunivariate/include/cppunivariate/distributions/zib/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zinb](../../cppunivariate/include/cppunivariate/distributions/zinb/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zip](../../cppunivariate/include/cppunivariate/distributions/zip/README.md) | closed form | not independently classified for every hold; consult cited source |
| [zipf](../../cppunivariate/include/cppunivariate/distributions/zipf/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [zmbinom](../../cppunivariate/include/cppunivariate/distributions/zmbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmgeom](../../cppunivariate/include/cppunivariate/distributions/zmgeom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmlogarithmic](../../cppunivariate/include/cppunivariate/distributions/zmlogarithmic/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmnbinom](../../cppunivariate/include/cppunivariate/distributions/zmnbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [zmpois](../../cppunivariate/include/cppunivariate/distributions/zmpois/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [ztbinom](../../cppunivariate/include/cppunivariate/distributions/ztbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [ztgeom](../../cppunivariate/include/cppunivariate/distributions/ztgeom/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |
| [ztnbinom](../../cppunivariate/include/cppunivariate/distributions/ztnbinom/README.md) | profile / multistart search | not independently classified for every hold; consult cited source |
| [ztpois](../../cppunivariate/include/cppunivariate/distributions/ztpois/README.md) | scalar solve / profile | not independently classified for every hold; consult cited source |

The CSV records unknown rationale explicitly. Complete this audit by checking each method label and each held subset against source and family-specific reports, documenting actual boundaries, stopping and failure behavior, and recovering historical rationale. In particular free-size bbinom/cmb and truncated or inflated binomial searches, global profile search, and unresolved existence questions remain open.
