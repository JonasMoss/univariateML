# Generic scores, observed information and sandwich covariance (TASK-102.7)

2026-11-01. Base revision `fd905f1b1c155ad03c6dd2305124f3156421788e`; worktree changes were uncommitted. The source hashes for independent references are in their result files. The Monte Carlo source hash is `003016fe6c146eeac6fd902541d87a1c971fc48178539c97af8d0070fc5ab526`.

## Classification and scope

98 dossiers: 90 have at least one eligible scope, 8 have none. Eligibility means positive DQM classification and twice-continuous pointwise log likelihood in the specified free coefficients; it is not a claim of full rank, estimator asymptotics, or coverage. Design sizes, known endpoints and other declared held coefficients are not free. A fitted endpoint or size is not known: its inclusion in the free mask is rejected. Conditions and classification links below are the dossier claims; the detailed derivations and scope limitations are in [the inference note](../../../univariateML-cpp/include/cppunivariate/core/inference.md).

| Family | Scope | Eligible | Condition or reason |
|---|---|---:|---|
| bbinom | size zero | yes | known integer size = 0 held; the law is a point mass and every regular score is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| bbinom | size one with one shape known | yes | known size = 1; exactly one positive finite shape free; the other shape known; rank one for the two-shape model; the scalar known-nuisance submodel is identifiable ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| bbinom | size one with both shapes free | yes | known size = 1; both positive finite shapes free; DQM and pointwise C2 hold, but joint information has rank one and covariance is singular ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| bbinom | primary interior | yes | known integer size >= 2 held; finite alpha > 0 and beta > 0; either or both free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| beta | interior | yes | shape1 > 0 and shape2 > 0 finite; either or both free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| beta | boundary or limit | no | beta boundary and limit models are outside the interior derivative regime ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| betapr | primary interior | yes | finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| bhatt | normal member with width known | yes | finite mu and sigma > 0; a = 0 held; mu and/or sigma free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| bhatt | primary interior | yes | finite mu, sigma > 0 and a > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| binom | interior | yes | known integer size >= 1, 0 < prob < 1; prob free and size held ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| binom | boundary or limit | no | the law is degenerate, so the probability information is zero; unknown size is discrete ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| bnbinom | primary interior | yes | finite size > 0, alpha > 0 and beta > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| burr | primary interior | yes | finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| cauchy | interior | yes | finite location and finite scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| cauchy | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| cmb | size zero | yes | known integer size = 0 held; the law is a point mass and every regular score is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| cmb | size one with dispersion known | yes | known size = 1; 0 < prob < 1 free and finite nu held; the Bernoulli probability is identifiable; dispersion is not ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| cmb | size one with dispersion free | yes | known size = 1; nu free; DQM and pointwise C2 hold but the dispersion score is identically zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| cmb | primary interior | yes | known integer size >= 2 held, 0 < prob < 1, finite nu; prob and/or nu free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| cmp | geometric member with dispersion known | yes | 0 < lambda < 1 free; nu = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| cmp | primary interior | yes | finite lambda > 0 and nu > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| dgamma | primary interior | yes | finite shape > 0 and rate > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| dlaplace | primary interior | yes | integer location known and held; 0 < scale < 1 free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates)) |
| dnorm | primary interior | yes | finite mean and sd > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| dunif | unreviewed or unavailable | no | integer endpoints are discrete and excluded from the free inference vector ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates)) |
| dweibull | primary interior | yes | 0 < shape1 < 1 and finite shape2 > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| emg | primary interior | yes | finite mu, sigma > 0 and lambda > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| exp | interior | yes | finite rate > 0; rate free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| exp | boundary or limit | no | rate is on a boundary or outside the proper finite model ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| fatigue | primary interior | yes | finite alpha > 0, beta > 0 and mu; mu held; alpha and/or beta free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| foldnorm | symmetric fixed location | yes | mu = 0 held, finite sigma > 0 free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| foldnorm | primary interior | yes | finite mu > 0 and sigma > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| fpareto | primary interior | yes | finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; finite min held; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| frechet | primary interior | yes | finite lambda > 0, sigma > 0 and mu; mu held; lambda and/or sigma free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| gamma | interior | yes | shape > 0 and scale > 0 finite; shape and/or scale free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| gamma | boundary or limit | no | gamma boundary and limit models are outside the interior derivative regime ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| gb2 | primary interior | yes | finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| ged | cusp moving with smooth exponent | yes | finite mean, sd > 0 and nu > 2; any regular subset free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels)) |
| ged | held location | yes | finite mean held and sd > 0, nu > 0; if nu is free require nu > 1 ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels)) |
| ged | normal member with shape held | yes | finite mean and sd > 0, nu = 2 held; mean and/or sd free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels)) |
| ged | unreviewed or unavailable | no | the C2 and held-subset classification at the moving cusp has not yet been completed ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#cusps-and-singularities)) |
| genbeta | primary interior | yes | finite shape1 > 0, shape2 > 0, shape3 > 0 and scale > 0; scale held; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| geom | interior | yes | 0 < prob < 1; prob free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| geom | boundary or limit | no | probability is on a boundary; the count distribution is improper or degenerate ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| gev | gumbel member with shape held | yes | finite mu and sigma > 0; xi = 0 held; mu and/or sigma free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels)) |
| gev | unreviewed or unavailable | no | the moving-support C2 and fitted-subset classification for this numerical procedure is not yet completed ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| gompertz | primary interior | yes | finite a > 0 and b > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| gpd | positive shape fixed support | yes | finite mu held, sigma > 0 and xi > 0; sigma and/or xi free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels)) |
| gpd | exponential member with shape held | yes | finite mu held, sigma > 0 free and xi = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels)) |
| gpd | unreviewed or unavailable | no | the moving-support C2 and fitted-subset classification for this numerical procedure is not yet completed ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| gumbel | interior | yes | finite mu and finite sigma > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| gumbel | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| hcauchy | known threshold scale | yes | finite mu held and sigma > 0 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels)) |
| hcauchy | unreviewed or unavailable | no | a free threshold changes support with positive endpoint density and is non-DQM ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| hnorm | primary interior | yes | finite mu held and sigma > 0 free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| ht | known threshold | yes | finite mu held, nu > 0 and sigma > 0; nu and/or sigma free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-threshold-and-fixed-shape-submodels)) |
| ht | unreviewed or unavailable | no | a free threshold changes support with positive endpoint density and is non-DQM ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| huber | unreviewed or unavailable | no | the piecewise quadratic log density has a moving second-derivative kink, so the joint pointwise C2 condition fails ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| hyper | unreviewed or unavailable | no | all coefficients are integer valued; there is no continuous free inference coordinate ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates)) |
| invburr | primary interior | yes | finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invexp | interior | yes | finite scale > 0; scale free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invexp | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invgamma | interior | yes | finite alpha > 0 and beta > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invgamma | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invgauss | interior | yes | finite mean > 0 and shape > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invgauss | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invparalogis | primary interior | yes | finite shape > 0 and scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invpareto | primary interior | yes | finite shape > 0 and scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invtrgamma | primary interior | yes | finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invweibull | interior | yes | finite shape > 0 and scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| invweibull | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| kumar | primary interior | yes | finite a > 0 and b > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| laplace | scale with location held | yes | finite mu held; finite sigma > 0 free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#laplace)) |
| laplace | location free | no | the log density has a moving absolute-value kink; the pointwise Hessian misses sensitivity ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#laplace)) |
| lgamma | interior | yes | finite shapelog > 0 and ratelog > 0; either or both free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| lgamma | boundary or limit | no | a gamma parameter is on a boundary or outside the finite model ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| lgser | interior | yes | 0 < theta < 1; theta free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| lgser | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| llogis | interior | yes | finite shape > 0 and scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| llogis | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| lnorm | interior | yes | finite meanlog and sdlog > 0; either or both free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| lnorm | boundary or limit | no | zero scale and infinite coefficients are outside the interior model ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| logis | interior | yes | finite location and finite scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| logis | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| logitnorm | interior | yes | finite mu and sigma > 0; either or both free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| logitnorm | boundary or limit | no | zero scale and infinite coefficients are outside the interior model ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| lomax | primary interior | yes | finite lambda > 0 and kappa > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| lst | primary interior | yes | finite mu, sigma > 0 and df > 0; when df and mu are both free require df > 1; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| naka | interior | yes | finite shape > 0 and scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| naka | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| nbinom | primary interior | yes | finite size > 0 and 0 < prob < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| nhyper | unreviewed or unavailable | no | all coefficients are integer valued; there is no continuous free inference coordinate ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates)) |
| norm | interior | yes | finite mean and sd > 0; mean and/or sd free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| norm | boundary or limit | no | zero scale is outside the interior continuous model ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| nsbeta | primary interior | yes | finite shape1 > 0 and shape2 > 0; finite min < max held; either shape free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| paralogis | primary interior | yes | finite shape > 0 and scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| pareto | shape with endpoint held | yes | finite a > 0 free; finite b > 0 held ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| pareto | endpoint free | no | the fitted endpoint is non-DQM and is not a free inference coordinate ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| pareto2 | primary interior | yes | finite shape > 0, scale > 0 and min; min held; shape and/or scale free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| pareto3 | primary interior | yes | finite shape > 0, scale > 0 and min; min held; shape and/or scale free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| pareto4 | primary interior | yes | finite shape1 > 0, shape2 > 0, scale > 0 and min; min held; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| pig | unreviewed or unavailable | no | DQM and score tail domination are open in the cited classification ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| pois | interior | yes | finite lambda > 0; lambda free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| pois | boundary or limit | no | zero rate is an attained boundary and infinite rate is not an interior model ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| power | primary interior | yes | finite alpha > 0 held and beta > 0 free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| rayleigh | interior | yes | finite sigma > 0; sigma free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| rayleigh | boundary or limit | no | zero scale and infinite scale are outside the interior model ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#standard-models)) |
| sged | symmetric member with held location | yes | finite mean held, sd > 0 and nu > 0; xi = 1 held; if nu is free require nu > 1 ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels)) |
| sged | symmetric normal member | yes | finite mean and sd > 0; nu = 2 and xi = 1 held; mean and/or sd free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels)) |
| sged | cusp moving with smooth exponent | yes | finite mean, sd > 0, xi > 0 and nu > 2; any regular subset free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#pointwise-smoothness-for-ged-submodels)) |
| sged | unreviewed or unavailable | no | the C2 and held-subset classification at the moving cusp has not yet been completed ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| sgomp | exponential member with shape known | yes | finite b > 0 free; eta = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| sgomp | primary interior | yes | finite b > 0 and eta > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| skellam | primary interior | yes | finite mu1 > 0 and mu2 > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| slash | primary interior | yes | finite mu and sigma > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| sn | primary interior | yes | finite xi, omega > 0 and finite alpha; any regular subset free; information is singular at alpha = 0 when xi and alpha are both free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#cusps-and-singularities)) |
| snorm | symmetric member with skew held | yes | finite mean and sd > 0; xi = 1 held; mean and/or sd free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| snorm | unreviewed or unavailable | no | the two-piece log density has a moving second-derivative kink ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| sstd | symmetric member with skew held | yes | finite mean, sd > 0 and nu > 2; xi = 1 held; any other subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| sstd | unreviewed or unavailable | no | the two-piece log density has a moving second-derivative kink ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| std | primary interior | yes | finite mean, sd > 0 and nu > 2; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| tbinom | singleton retained support | yes | known design and truncation bounds with exactly one retained integer; probability free in (0,1); the conditional law is a point mass, so information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| tbinom | primary interior | yes | known integer size, known a and b held; 0 < prob < 1 free; retained support has at least two integers ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| tlambda | unreviewed or unavailable | no | DQM and support-dependent regularity remain open in the cited classification ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| tnorm | primary interior | yes | finite mean and sd > 0; a < b known and held with positive truncation probability; mean and/or sd free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| tpois | singleton retained support | yes | known truncation bounds with exactly one retained integer; finite lambda > 0 free; the conditional law is a point mass, so information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| tpois | primary interior | yes | finite lambda > 0 free; known a and b held; retained support has at least two integers ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| trgamma | primary interior | yes | finite shape1 > 0, shape2 > 0 and scale > 0; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| triang | interior mode | no | the moving kink invalidates ordinary observed-Hessian sensitivity despite DQM ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#triangular)) |
| triang | endpoints free | no | moving endpoints have divergent squared scores and are not free inference coordinates ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#triangular)) |
| unif | endpoint free | no | moving endpoints are non-DQM and no regular free coefficient remains when both are held ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#moving-support)) |
| weibull | interior | yes | finite shape > 0 and scale > 0; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| weibull | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| yulesimon | primary interior | yes | finite rho > 0; rho free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zeta | interior | yes | finite s > 1; s free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zeta | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zib | size zero | yes | known integer size = 0 held; the law is a point mass and every regular score is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| zib | known unit binomial probability | yes | known integer size >= 1, prob = 1 held, 0 < pi < 1 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zib | binomial member with inflation known | yes | integer size >= 1 held, 0 < prob < 1 free, pi = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zib | known point-mass member | yes | known integer size >= 1, 0 < prob < 1 free, pi = 1 held; information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zib | size one with one coefficient known | yes | known size = 1; exactly one of prob and pi free, the other held in (0,1); the remaining scalar Bernoulli parameter is identifiable ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| zib | size one with both coefficients free | yes | known size = 1; prob and pi both free in (0,1); DQM and pointwise C2 hold but the joint information has rank one ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| zib | primary interior | yes | known integer size >= 2 held; 0 < prob < 1 and 0 < pi < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zinb | negative-binomial member with inflation known | yes | finite size > 0 and 0 < prob < 1; pi = 0 held; size and/or prob free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zinb | known point-mass member | yes | finite size > 0 and 0 < prob < 1; pi = 1 held; remaining information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zinb | primary interior | yes | finite size > 0, 0 < prob < 1 and 0 < pi < 1; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zip | Poisson member with inflation known | yes | finite lambda > 0 free; pi = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zip | known point-mass member | yes | finite lambda > 0 free; pi = 1 held; information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zip | primary interior | yes | finite lambda > 0 and 0 < pi < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zipf | size one | yes | known N = 1; finite s > 0 free; the law is a point mass, so information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| zipf | primary interior | yes | known integer N >= 2 held and finite s > 0 free ([classification](../../../dev/roadmap/2026-10-30-inference-and-dqm.md#discrete-coordinates)) |
| zmbinom | known boundary binomial probability | yes | known integer size >= 1; prob = 0 or prob = 1 held; 0 < p0 < 1 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmbinom | positive-count member with zero mass known | yes | known integer size >= 2 held, 0 < prob < 1 free, p0 = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmbinom | known point-mass member | yes | known integer size >= 1 held, 0 < prob < 1 free, p0 = 1 held; information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmbinom | size one with probability known | yes | known size = 1; prob held in (0,1), p0 free in (0,1); the remaining zero-mass parameter is identifiable ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| zmbinom | size one with probability free | yes | known size = 1; prob free; the conditional positive distribution is a point mass and the probability score vanishes ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| zmbinom | primary interior | yes | known integer size >= 2 held; 0 < prob < 1 and 0 < p0 < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zmgeom | known unit geometric probability | yes | prob = 1 held; 0 < p0 < 1 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmgeom | positive-count member with zero mass known | yes | 0 < prob < 1 free, p0 = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmgeom | known point-mass member | yes | 0 < prob < 1 free, p0 = 1 held; information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmgeom | primary interior | yes | 0 < prob < 1 and 0 < p0 < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zmlogarithmic | known zero positive-count parameter | yes | prob = 0 held; 0 < p0 < 1 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmlogarithmic | positive-count member with zero mass known | yes | 0 < prob < 1 free, p0 = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmlogarithmic | known point-mass member | yes | 0 < prob < 1 free, p0 = 1 held; information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmlogarithmic | primary interior | yes | 0 < prob < 1 and 0 < p0 < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zmnbinom | known zero negative-binomial size | yes | size = 0 held; 0 < prob < 1 and 0 < p0 < 1; prob and/or p0 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmnbinom | primary interior | yes | finite size > 0, 0 < prob < 1 and 0 < p0 < 1; any regular subset free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| zmpois | known zero Poisson rate | yes | lambda = 0 held; 0 < p0 < 1 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmpois | positive-count member with zero mass known | yes | finite lambda > 0 free, p0 = 0 held ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmpois | known point-mass member | yes | finite lambda > 0 free, p0 = 1 held; information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| zmpois | primary interior | yes | finite lambda > 0 and 0 < p0 < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| ztbinom | size one | yes | known size = 1; 0 < prob < 1 free; the zero-truncated law is a point mass, so information is zero ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#finite-support-rank-and-known-design-submodels)) |
| ztbinom | primary interior | yes | known integer size >= 2 held; 0 < prob < 1 free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| ztgeom | interior | yes | 0 < prob < 1; prob free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| ztgeom | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| ztnbinom | known zero negative-binomial size | yes | size = 0 held; 0 < prob < 1 free ([classification](../../../univariateML-cpp/include/cppunivariate/core/inference.md#known-boundary-submodels)) |
| ztnbinom | primary interior | yes | finite size > 0 and 0 < prob < 1; either or both free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| ztpois | interior | yes | finite lambda > 0; lambda free ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |
| ztpois | boundary or limit | no | this scope requires the stated finite interior parameters; boundary behavior is separate ([classification](../../../dev/research/evaluations/2026-10-31-inference-1-report.md#family-by-family-classification)) |

DQM, finite information, rank, observed-Hessian suitability and estimator limit law are separate properties. In particular, smooth but rank-deficient scopes can return `Singular`; moving-support and nonsmooth scopes remain unavailable unless a stated regular submodel holds. For beta-binomial with fixed size N≥2, the first two factorial moments identify mean and concentration locally, giving a full-rank score on the interior; N=1 carries only one identifiable combination and N=0 is degenerate. This is an analytic rank argument, not a simulation result. The GED moving cusp is C² for shape >2; the known-location scale/shape submodel is smooth at the held cusp. Known-threshold half-t and half-Cauchy inherit DQM from their folded symmetric parents. The note states the remaining regime arguments and open classifications. Positive definiteness of a particular observed matrix is checked numerically and is not guaranteed by DQM.

## Numerical method

The four analytic score headers for normal, gamma, beta and Poisson take precedence. Other allowed scopes evaluate the family C++ log density. Free parameters use identity, log, logit or log-distance-above-fixed-bound coordinates. With internal coordinate z, scores use central differences at `h=1e-4*(1+abs(z))` and `h/2`, extrapolated as `(4 D(h/2)-D(h))/3`, then divided by the natural-coordinate Jacobian. H differentiates total natural scores with `h=3e-4*(1+abs(z))`, half step, the same extrapolation and Jacobian, and symmetrizes mixed entries. J is the sum of per-observation outer products, not an outer product of the total score. Observed covariance is H⁻¹; sandwich covariance is H⁻¹JH⁻¹, both on the total-likelihood scale via the existing conditioning-aware solve, without a pseudoinverse or an extra n factor. Held coordinates are excluded. Under sufficient smoothness the exact-arithmetic Richardson truncation is fourth order; cancellation, nested differences, density accuracy and ill-conditioned transformations can dominate, and these orders are not certified bounds. Nonfinite or collapsed stencils fail. Unavailable family/regime, boundary/limit fit, singularity, indefiniteness and ill-conditioning are explicit statuses with reasons. Numerical Fisher information is not claimed.

## Independent derivative validation

Seed 20261101; mpmath 40 decimal digits and SymPy-derived or independent explicit references. Scaled error is `abs(actual-reference)/(1+abs(reference))`; tolerances are 2e-6 for score, 2e-5 for observed information, and 1e-3 absolute for each golden total-score component. The 11-family standard validator checked 5,676 seeded and golden observations, including full-data H and scores; the 66-family registry and 13 conditional-family validators used five seeded points each and golden observations where the fitted regime is eligible. A pilot separately compared the numerical engine with the four analytic headers at 2,064 points. The table reports maxima over the tested points, not uniform bounds. A dash means no eligible default golden fit, rather than a zero residual. Where a golden structural coefficient was fitted, conditioning on its estimate checks only that conditional mathematical submodel, not joint inference for that fit.

| Family | Max score error | Max H error | Full-data score error | Full-data H error | Max absolute golden score | Scope |
|---|---:|---:|---:|---:|---:|---|
| bbinom | 9.98e-12 | 8.45e-09 | 1.92e-09 | 1.98e-08 | 1.92e-09 | eligible scope |
| beta | 1.87e-16 | 6.78e-17 | 3.74e-14 | 1.07e-14 | 8.06e-07 | analytic |
| betapr | 1.01e-11 | 4.72e-08 | 2.53e-09 | 1.21e-08 | 5.48e-09 | eligible scope |
| bhatt | 4.56e-12 | 2.64e-08 | 3.19e-11 | 2.71e-08 | 1.47e-06 | eligible scope |
| binom | 8.05e-12 | 7.08e-08 | 1.74e-09 | 6.64e-09 | 1.74e-09 | size conditioned on fitted estimate |
| bnbinom | 4.12e-11 | 1.19e-07 | 7.35e-10 | 5.08e-09 | 7.4e-07 | eligible scope |
| burr | 8.51e-12 | 3.23e-08 | 2.35e-10 | 3.03e-09 | 8.55e-06 | eligible scope |
| cauchy | 2.47e-11 | 2.15e-07 | 2.59e-10 | 9.35e-09 | 2.44e-10 | eligible scope |
| cmb | 3.86e-12 | 3.03e-08 | 2.38e-10 | 3.39e-09 | 4.06e-08 | eligible scope |
| cmp | 6.77e-13 | 4.67e-09 | 7.45e-11 | 6.32e-10 | 7.44e-11 | eligible scope |
| dgamma | 4.95e-12 | 3.65e-08 | 2.77e-10 | 3.05e-09 | 1.89e-07 | eligible scope |
| dlaplace | 4.11e-12 | 1.88e-08 | 7.52e-10 | 3.65e-09 | 7.52e-10 | eligible scope |
| dnorm | 4.18e-12 | 3.38e-08 | 1.59e-09 | 9.49e-07 | 1.59e-09 | eligible scope |
| dweibull | 2.36e-12 | 1.9e-08 | 2.8e-10 | 1.11e-09 | 2.79e-10 | eligible scope |
| emg | 4.38e-12 | 2.89e-08 | 6.07e-11 | 1.49e-09 | 4.44e-06 | eligible scope |
| exp | 3.87e-12 | 2.28e-08 | 1.98e-10 | 1.4e-09 | 1.98e-10 | default |
| fatigue | 5.92e-12 | 2.01e-08 | 3.48e-11 | 3.51e-08 | 3.89e-07 | eligible scope |
| foldnorm | 5.63e-12 | 2.63e-08 | 5.44e-11 | 8.24e-10 | 2.14e-06 | eligible scope |
| fpareto | 2.5e-11 | 1.56e-07 | 1.96e-10 | 2.08e-08 | 1.45e-09 | eligible scope |
| frechet | 8.36e-12 | 4.39e-08 | 4.8e-10 | 4.44e-09 | 4.8e-10 |  conditional on fitted mu |
| gamma | 4.26e-16 | 8.04e-16 | 1.36e-14 | 8.47e-15 | 2.15e-13 | analytic |
| gb2 | 7.04e-11 | 1.38e-07 | 3.53e-08 | 1.59e-07 | 3.53e-08 | eligible scope |
| ged | 8.32e-12 | 6.18e-08 | 8.46e-10 | 5.49e-07 | 1.94e-05 | eligible scope |
| genbeta | 8.87e-11 | 1.07e-07 | 4.24e-08 | 3.2e-08 | 1.53e-06 | eligible scope |
| geom | 7.24e-12 | 4.47e-08 | 1.08e-09 | 4.26e-09 | 1.08e-09 | default |
| gev | 1.41e-12 | 1.6e-08 | — | — | — | RegimeUnavailable |
| gompertz | 6.42e-12 | 3.99e-08 | 1.56e-10 | 2.16e-09 | 1.84e-10 | eligible scope |
| gpd | 1.87e-11 | 1.74e-07 | 3.7e-10 | 1.1e-08 | 3.87e-10 | eligible scope |
| gumbel | 2.18e-12 | 4.93e-09 | 2.49e-11 | 1.26e-09 | 6.25e-08 | eligible scope |
| hcauchy | 1.44e-11 | 9.58e-08 | 3.76e-10 | 1.66e-09 | 3.75e-10 | eligible scope |
| hnorm | 3.69e-12 | 7.91e-09 | 1.89e-11 | 2.27e-10 | 1.9e-11 |  conditional on fitted mu |
| ht | 7.17e-12 | 3.29e-08 | 4.34e-11 | 9.7e-09 | 1.96e-06 | eligible scope |
| invburr | 6.81e-12 | 1.15e-07 | 5.54e-11 | 1.06e-08 | 1.16e-05 | eligible scope |
| invexp | 1.4e-11 | 7.11e-08 | 1.11e-10 | 1.09e-09 | 1.11e-10 | eligible scope |
| invgamma | 2.07e-12 | 5.06e-09 | 7.86e-10 | 5e-09 | 7.86e-10 | eligible scope |
| invgauss | 2.29e-12 | 5.4e-09 | 1.17e-10 | 1.77e-09 | 1.17e-10 | eligible scope |
| invparalogis | 1.63e-11 | 8.87e-08 | 1.53e-10 | 3.61e-09 | 7.79e-05 | eligible scope |
| invpareto | 1.28e-11 | 8.78e-08 | 2.26e-10 | 6.15e-09 | 1.25e-09 | eligible scope |
| invtrgamma | 4.19e-12 | 2.85e-08 | 5.28e-10 | 2.68e-10 | 4.74e-10 | eligible scope |
| invweibull | 4.49e-12 | 2.11e-08 | 7.69e-11 | 3.81e-10 | 7.63e-11 | eligible scope |
| kumar | 2.39e-12 | 4.38e-09 | 4.86e-10 | 4.8e-10 | 1.03e-06 | eligible scope |
| laplace | 5.08e-12 | 2.23e-08 | 1.43e-10 | 8.72e-10 | 1.42e-10 |  conditional on fitted mu |
| lgamma | 1.69e-12 | 6.61e-09 | 9.19e-11 | 2.94e-09 | 9.19e-11 | default |
| lgser | 1.96e-11 | 9.52e-08 | 3.56e-10 | 1.72e-09 | 3.56e-10 | eligible scope |
| llogis | 1.3e-11 | 6.36e-08 | 6.45e-10 | 1.28e-07 | 7.05e-10 | eligible scope |
| lnorm | 6.74e-12 | 6.15e-08 | 6.42e-11 | 2.65e-07 | 6.4e-11 | default |
| logis | 7.36e-12 | 9e-08 | 4.6e-10 | 9.33e-08 | 4.6e-10 | eligible scope |
| logitnorm | 5.74e-12 | 1.31e-08 | 4.96e-10 | 2.43e-09 | 4.96e-10 | default |
| lomax | 7.11e-12 | 6.3e-08 | 2.73e-10 | 5.15e-09 | 2.16e-06 | eligible scope |
| lst | 2.37e-11 | 1.54e-07 | 1.73e-10 | 8.71e-08 | 2.27e-07 | eligible scope |
| naka | 4.95e-12 | 1.1e-08 | 2.29e-11 | 4.77e-08 | 2.32e-11 | eligible scope |
| nbinom | 1.7e-11 | 1.22e-08 | 4.24e-09 | 6.68e-09 | 4.24e-09 | eligible scope |
| norm | 5.14e-16 | 6.03e-16 | 1.28e-14 | 2.23e-14 | 1.75e-13 | analytic |
| nsbeta | 5.42e-12 | 6.57e-09 | 1.92e-09 | 1.11e-08 | 3.5e-09 | eligible scope |
| paralogis | 1.37e-11 | 1.2e-07 | 1.41e-11 | 1.19e-09 | 3.34e-05 | eligible scope |
| pareto | 8.82e-12 | 8.77e-08 | 1.79e-10 | 4.95e-09 | 1.79e-10 | eligible scope |
| pareto2 | 3.92e-11 | 2e-07 | 8.58e-10 | 1.42e-08 | 8.59e-10 | eligible scope |
| pareto3 | 1.06e-11 | 5.56e-08 | 4.02e-10 | 2.55e-08 | 8.09e-06 | eligible scope |
| pareto4 | 7.56e-11 | 2.92e-07 | 2.39e-09 | 1.7e-08 | 3.74e-05 | eligible scope |
| pois | 5.73e-17 | 9.54e-17 | 9.46e-15 | 7.5e-15 | 3.1e-14 | analytic |
| power | 2e-12 | 7.59e-09 | 3.33e-11 | 8.47e-10 | 3.33e-11 | eligible scope |
| rayleigh | 3.48e-12 | 4.21e-09 | 1.58e-11 | 8.79e-11 | 1.59e-11 | default |
| sged | 5.92e-11 | 1.35e-06 | 1.1e-09 | 4.19e-08 | 2.39e-05 | eligible scope |
| sgomp | 1.29e-11 | 4.67e-08 | 4.05e-10 | 1.18e-09 | 9.08e-06 | eligible scope |
| skellam | 5.96e-13 | 1.42e-09 | 5.81e-11 | 1.44e-09 | 1.28e-06 | eligible scope |
| slash | 2.6e-11 | 1.72e-07 | 3.14e-10 | 5.01e-09 | 1.27e-05 | eligible scope |
| sn | 5.38e-12 | 4.88e-08 | 3.31e-10 | 8.2e-10 | 1.7e-06 | eligible scope |
| snorm | 3.26e-12 | 1.92e-08 | — | — | — | RegimeUnavailable |
| sstd | 2.73e-12 | 3.2e-08 | — | — | — | RegimeUnavailable |
| std | 7.97e-12 | 1.38e-07 | 2.74e-10 | 6.92e-08 | 4.78e-05 | eligible scope |
| tbinom | 3.07e-11 | 5.27e-08 | 2.99e-08 | 2.76e-08 | 2.99e-08 | eligible scope |
| tnorm | 5.36e-12 | 4.93e-08 | 1.68e-10 | 2.13e-07 | 1.67e-10 | eligible scope |
| tpois | 1.03e-12 | 1.72e-09 | 2.24e-11 | 2.49e-09 | 2.24e-11 | eligible scope |
| trgamma | 3.39e-12 | 2.2e-08 | 3.08e-10 | 5.43e-09 | 3.07e-10 | eligible scope |
| weibull | 1.19e-12 | 3.06e-09 | 2.7e-11 | 2.17e-10 | 8.12e-07 | eligible scope |
| yulesimon | 2.72e-12 | 1.13e-08 | 3.4e-11 | 1.73e-09 | 5.33e-09 | eligible scope |
| zeta | 5.87e-12 | 7.68e-08 | 1.35e-09 | 1.06e-08 | 3.53e-09 | eligible scope |
| zib | 1.94e-11 | 2.34e-07 | 1.01e-08 | 4.9e-08 | 1.01e-08 |  conditional on fitted size |
| zinb | 8.62e-12 | 1.8e-07 | 2.06e-09 | 1.49e-08 | 4.93e-07 | eligible scope |
| zip | 1.2e-11 | 1.87e-07 | 1.56e-09 | 1.57e-08 | 1.56e-09 | eligible scope |
| zipf | 1.91e-12 | 2.53e-08 | 1.01e-09 | 3.58e-08 | 1.01e-09 | eligible scope |
| zmbinom | 1.27e-11 | 1.04e-07 | 1e-08 | 2.27e-06 | 1e-08 |  conditional on fitted size |
| zmgeom | 7.23e-12 | 1.91e-07 | 9.03e-10 | 1.23e-05 | 9.03e-10 | eligible scope |
| zmlogarithmic | 8.11e-12 | 1.39e-07 | 6.52e-10 | 6.15e-06 | 6.52e-10 | eligible scope |
| zmnbinom | 9.52e-12 | 1.18e-07 | 3.79e-09 | 1.11e-05 | 3.96e-06 | eligible scope |
| zmpois | 1.98e-12 | 6.38e-08 | 2.26e-09 | 1.13e-06 | 2.26e-09 | eligible scope |
| ztbinom | 7.9e-12 | 2.95e-08 | 2.23e-10 | 1.58e-08 | 2.23e-10 |  conditional on fitted size |
| ztgeom | 2.84e-12 | 2.73e-08 | 1.67e-09 | 1.76e-09 | 1.67e-09 | eligible scope |
| ztnbinom | 8.94e-12 | 3.81e-08 | 7.78e-10 | 1.1e-08 | 3.9e-06 | eligible scope |
| ztpois | 9.93e-13 | 4.82e-09 | 9.16e-11 | 1.17e-09 | 9.16e-11 | eligible scope |

The conditional validator additionally exercised known-boundary reductions, singular supports, and transformed charts. Default GEV, skew-normal and skew-t golden regimes are unavailable as documented; their eligible restricted scopes were tested at seeded points. The pilot maximum scaled score error was 5.35e-12 and maximum H error 5.85e-8. These tests support local numerical agreement, not coverage or general accuracy at extreme parameters.

**Open — fitter-specific score tolerances.** The checks above establish the stated common absolute threshold of 1e-3, not that each residual meets its own fitter's stopping rule: fitters use different objectives, coordinate systems and convergence criteria. For example, the largest recorded registry residual is 7.79e-5 for inverse paralogistic. Establishing the requested fitter-by-fitter equivalence requires mapping each stopping criterion and its objective normalization to the natural total score; the three unavailable default regimes also have no joint golden-score check. Conditional checks at fitted structural values do not certify joint stationarity.

## Monte Carlo sanity check

Model sampling and actual C++ fitters, n=500, 200 replications, single-threaded; seed is `20261101 + 1000*family_index + replication` with alphabetical family order in the saved summary. Held values and truth are recorded there. A probe over 0.25 CPU seconds caused a slow-fitter skip. Actual simulation CPU time was 247.4 seconds; the source-hashed run is resumable. The driver exited 1 because failed replications were retained. The independent protocol checker passed accounting: 77 families had all 200 usable, six had failures (122 replications total), and seven were skipped as slow. Comparisons in the six incomplete families are conditional on success and may be biased. Two-MCSE flags use paired uncertainty for mean-SE minus empirical SD; many coordinates were tested, so these are diagnostics rather than multiple-testing-adjusted conclusions. Mean-SE MCSE and empirical-SD MCSE are shown separately; the paired difference MCSE is the relevant flag denominator.

| Family | Coordinate | Usable | Empirical SD ± MCSE | Model mean SE ± MCSE | Sandwich mean SE ± MCSE | Paired difference MCSE (model, sandwich) | Flag >2 MCSE |
|---|---|---:|---:|---:|---:|---:|---|
| bbinom | alpha | 200/200 | 0.0819 ± 0.00511 | 0.0839 ± 0.000533 | 0.084 ± 0.000569 | 0.00499, 0.00501 | — |
| bbinom | beta | 200/200 | 0.0857 ± 0.00415 | 0.0838 ± 0.000554 | 0.0835 ± 0.000585 | 0.00412, 0.00413 | — |
| beta | shape1 | 200/200 | 0.063 ± 0.00303 | 0.0586 ± 0.000278 | 0.0582 ± 0.000358 | 0.003, 0.00299 | — |
| beta | shape2 | 200/200 | 0.0635 ± 0.00306 | 0.0588 ± 0.000281 | 0.0584 ± 0.000348 | 0.00306, 0.00307 | — |
| betapr | shape1 | 200/200 | 0.106 ± 0.00524 | 0.103 ± 0.00131 | 0.102 ± 0.00146 | 0.00501, 0.00499 | — |
| betapr | shape2 | 200/200 | 0.101 ± 0.00481 | 0.1 ± 0.00129 | 0.0994 ± 0.00126 | 0.00453, 0.00463 | — |
| betapr | scale | 200/200 | 0.282 ± 0.0157 | 0.269 ± 0.00563 | 0.268 ± 0.00568 | 0.0141, 0.0142 | — |
| bhatt | — | 0/200 | — | — | — | — | skipped_slow |
| binom | prob | 200/200 | 0.00721 ± 0.000343 | 0.00707 ± 7.07e-08 | 0.00704 ± 1.59e-05 | 0.000344, 0.000346 | — |
| bnbinom | — | 0/200 | — | — | — | — | skipped_slow |
| burr | shape1 | 200/200 | 0.167 ± 0.00929 | 0.17 ± 0.00291 | 0.168 ± 0.00283 | 0.00864, 0.00879 | — |
| burr | shape2 | 200/200 | 0.0706 ± 0.00366 | 0.0695 ± 0.000498 | 0.0694 ± 0.000576 | 0.00349, 0.00353 | — |
| burr | scale | 200/200 | 0.272 ± 0.0169 | 0.278 ± 0.00758 | 0.276 ± 0.00747 | 0.014, 0.0142 | — |
| cauchy | location | 200/200 | 0.0625 ± 0.00324 | 0.0636 ± 0.000282 | 0.0638 ± 0.000328 | 0.00327, 0.00328 | — |
| cauchy | scale | 200/200 | 0.0623 ± 0.00308 | 0.0635 ± 0.00029 | 0.0637 ± 0.000347 | 0.00306, 0.00306 | — |
| cmb | prob | 200/200 | 0.00711 ± 0.00046 | 0.00709 ± 1.47e-05 | 0.00709 ± 1.51e-05 | 0.000459, 0.000459 | — |
| cmb | nu | 200/200 | 0.0657 ± 0.00302 | 0.0675 ± 0.000283 | 0.0669 ± 0.000353 | 0.00301, 0.00299 | — |
| cmp | — | 0/200 | — | — | — | — | skipped_slow |
| dgamma | shape | 200/200 | 0.12 ± 0.00546 | 0.122 ± 0.000994 | 0.122 ± 0.00112 | 0.00571, 0.00574 | — |
| dgamma | rate | 200/200 | 0.118 ± 0.00619 | 0.118 ± 0.000978 | 0.117 ± 0.00123 | 0.00626, 0.00629 | — |
| dlaplace | scale | 200/200 | 0.0149 ± 0.000787 | 0.015 ± 1.13e-05 | 0.0149 ± 4.37e-05 | 0.000791, 0.000793 | — |
| dnorm | mean | 200/200 | 0.0473 ± 0.00236 | 0.0466 ± 0.000107 | 0.0466 ± 0.000107 | 0.00237, 0.00237 | — |
| dnorm | sd | 200/200 | 0.0351 ± 0.00176 | 0.0342 ± 7.23e-05 | 0.0343 ± 0.000152 | 0.00178, 0.00179 | — |
| dweibull | shape1 | 200/200 | 0.0213 ± 0.00112 | 0.0217 ± 1.04e-05 | 0.0217 ± 3e-05 | 0.00113, 0.00113 | — |
| dweibull | shape2 | 200/200 | 0.0446 ± 0.00226 | 0.0482 ± 0.000139 | 0.0479 ± 0.000246 | 0.00226, 0.00227 | — |
| emg | — | 0/200 | — | — | — | — | skipped_slow |
| exp | rate | 200/200 | 0.0492 ± 0.00251 | 0.0448 ± 0.000156 | 0.0446 ± 0.000214 | 0.00248, 0.00249 | — |
| fatigue | alpha | 200/200 | 0.0297 ± 0.00151 | 0.0315 ± 6.64e-05 | 0.0311 ± 0.000122 | 0.00152, 0.00154 | — |
| fatigue | beta | 200/200 | 0.0341 ± 0.00186 | 0.0393 ± 0.000116 | 0.0391 ± 0.000124 | 0.00186, 0.00187 | model, sandwich |
| foldnorm | mu | 200/200 | 0.0693 ± 0.00608 | 0.0752 ± 0.0022 | 0.0746 ± 0.0021 | 0.00443, 0.00454 | — |
| foldnorm | sigma | 200/200 | 0.0648 ± 0.00451 | 0.0636 ± 0.00114 | 0.0621 ± 0.0012 | 0.00383, 0.00395 | — |
| fpareto | shape1 | 186/200 | 2.34 ± 0.292 | 3.38 ± 0.51 | 3.2 ± 0.467 | 0.25, 0.213 | model, sandwich |
| fpareto | shape2 | 186/200 | 0.377 ± 0.0294 | 0.432 ± 0.00699 | 0.426 ± 0.00879 | 0.0246, 0.0247 | model |
| fpareto | shape3 | 186/200 | 2.07 ± 0.218 | 3.05 ± 0.414 | 2.9 ± 0.38 | 0.214, 0.184 | model, sandwich |
| fpareto | scale | 186/200 | 0.857 ± 0.296 | 0.898 ± 0.306 | 0.834 ± 0.275 | 0.0573, 0.0587 | — |
| frechet | lambda | 200/200 | 0.0363 ± 0.00183 | 0.035 ± 9.27e-05 | 0.0347 ± 0.000168 | 0.00184, 0.00183 | — |
| frechet | sigma | 200/200 | 0.0492 ± 0.00235 | 0.0473 ± 0.000233 | 0.0473 ± 0.000234 | 0.00237, 0.00237 | — |
| gamma | shape | 200/200 | 0.0553 ± 0.00251 | 0.0561 ± 0.000237 | 0.0559 ± 0.000308 | 0.00251, 0.0025 | — |
| gamma | scale | 200/200 | 0.0685 ± 0.00318 | 0.0711 ± 0.000378 | 0.0708 ± 0.000465 | 0.00319, 0.00319 | — |
| gb2 | shape1 | 168/200 | 2.11 ± 0.266 | 3.11 ± 0.455 | 2.9 ± 0.407 | 0.221, 0.179 | model, sandwich |
| gb2 | shape2 | 168/200 | 0.378 ± 0.0274 | 0.423 ± 0.00754 | 0.416 ± 0.00954 | 0.0212, 0.0199 | model |
| gb2 | shape3 | 168/200 | 2.3 ± 0.288 | 3.33 ± 0.512 | 3.12 ± 0.459 | 0.249, 0.202 | model, sandwich |
| gb2 | scale | 168/200 | 0.586 ± 0.101 | 0.69 ± 0.149 | 0.647 ± 0.138 | 0.0556, 0.0432 | — |
| ged | mean | 200/200 | 0.0386 ± 0.00188 | 0.0418 ± 0.000108 | 0.0418 ± 0.000119 | 0.00188, 0.00188 | — |
| ged | sd | 200/200 | 0.0267 ± 0.00117 | 0.0264 ± 7.73e-05 | 0.0266 ± 8.09e-05 | 0.00118, 0.00118 | — |
| ged | nu | 200/200 | 0.347 ± 0.0155 | 0.366 ± 0.00442 | 0.355 ± 0.00484 | 0.0143, 0.0144 | — |
| genbeta | shape1 | 151/200 | 0.657 ± 0.0513 | 1.54 ± 0.212 | 1.54 ± 0.218 | 0.165, 0.173 | model, sandwich |
| genbeta | shape2 | 151/200 | 0.244 ± 0.0169 | 0.261 ± 0.00773 | 0.264 ± 0.00887 | 0.0133, 0.0136 | — |
| genbeta | shape3 | 151/200 | 0.671 ± 0.0366 | 0.913 ± 0.0176 | 0.911 ± 0.0238 | 0.0314, 0.0339 | model, sandwich |
| geom | prob | 200/200 | 0.0163 ± 0.000781 | 0.0158 ± 1.8e-05 | 0.0157 ± 5.17e-05 | 0.000782, 0.000784 | — |
| gev | mu | 200/200 | 0.0437 ± 0.00225 | 0.0468 ± 0.00012 | 0.0468 ± 0.00012 | 0.00226, 0.00226 | — |
| gev | sigma | 200/200 | 0.036 ± 0.00166 | 0.0347 ± 9.24e-05 | 0.0346 ± 0.00015 | 0.00166, 0.00167 | — |
| gompertz | a | 200/200 | 0.0755 ± 0.00376 | 0.0776 ± 0.000315 | 0.0774 ± 0.000371 | 0.00377, 0.00374 | — |
| gompertz | b | 200/200 | 0.117 ± 0.00552 | 0.107 ± 0.000293 | 0.106 ± 0.000662 | 0.00547, 0.00547 | — |
| gpd | sigma | 199/200 | 0.0744 ± 0.00355 | 0.0697 ± 0.000336 | 0.0694 ± 0.000414 | 0.00353, 0.00353 | — |
| gpd | xi | 199/200 | 0.0539 ± 0.00269 | 0.0543 ± 0.000241 | 0.0521 ± 0.000324 | 0.00268, 0.00275 | — |
| gumbel | mu | 200/200 | 0.0471 ± 0.00253 | 0.0469 ± 0.000124 | 0.0469 ± 0.000123 | 0.00254, 0.00254 | — |
| gumbel | sigma | 200/200 | 0.0371 ± 0.00205 | 0.0348 ± 9.37e-05 | 0.0345 ± 0.00016 | 0.00206, 0.00206 | — |
| hcauchy | sigma | 200/200 | 0.0625 ± 0.00371 | 0.0631 ± 0.000296 | 0.0632 ± 0.00037 | 0.0037, 0.0037 | — |
| hnorm | sigma | 200/200 | 0.0317 ± 0.00152 | 0.0315 ± 7.09e-05 | 0.0313 ± 0.000135 | 0.00154, 0.00154 | — |
| ht | nu | 200/200 | 1.33 ± 0.101 | 1.34 ± 0.0513 | 1.29 ± 0.046 | 0.0701, 0.074 | — |
| ht | sigma | 200/200 | 0.0544 ± 0.00243 | 0.0548 ± 0.000248 | 0.0542 ± 0.000339 | 0.00239, 0.00238 | — |
| invburr | shape1 | 200/200 | 0.17 ± 0.0093 | 0.171 ± 0.00292 | 0.168 ± 0.0029 | 0.00816, 0.0083 | — |
| invburr | shape2 | 200/200 | 0.0661 ± 0.00362 | 0.0688 ± 0.000478 | 0.0684 ± 0.000569 | 0.00357, 0.00356 | — |
| invburr | scale | 200/200 | 0.247 ± 0.0131 | 0.259 ± 0.00307 | 0.256 ± 0.00322 | 0.0131, 0.0129 | — |
| invexp | scale | 200/200 | 0.0414 ± 0.00232 | 0.0447 ± 0.000131 | 0.0444 ± 0.000189 | 0.0023, 0.00231 | — |
| invgamma | alpha | 200/200 | 0.0534 ± 0.00288 | 0.0559 ± 0.000229 | 0.0557 ± 0.000307 | 0.00287, 0.00285 | — |
| invgamma | beta | 200/200 | 0.0687 ± 0.0033 | 0.0716 ± 0.00032 | 0.0717 ± 0.000441 | 0.00329, 0.00328 | — |
| invgauss | mean | 200/200 | 0.043 ± 0.00199 | 0.0448 ± 0.000227 | 0.0447 ± 0.000289 | 0.00204, 0.00206 | — |
| invgauss | shape | 200/200 | 0.0692 ± 0.00317 | 0.0633 ± 0.000309 | 0.0627 ± 0.000378 | 0.00316, 0.0032 | sandwich |
| invparalogis | shape | 200/200 | 0.0283 ± 0.00135 | 0.0274 ± 6.07e-05 | 0.0275 ± 0.00011 | 0.00134, 0.00136 | — |
| invparalogis | scale | 200/200 | 0.0917 ± 0.00413 | 0.0875 ± 0.000711 | 0.0876 ± 0.00075 | 0.00423, 0.00425 | — |
| invpareto | shape | 200/200 | 0.0971 ± 0.00652 | 0.0917 ± 0.000952 | 0.0913 ± 0.00105 | 0.00613, 0.00608 | — |
| invpareto | scale | 200/200 | 0.154 ± 0.00781 | 0.155 ± 0.00162 | 0.155 ± 0.00178 | 0.00754, 0.00761 | — |
| invtrgamma | shape1 | 200/200 | 0.298 ± 0.0208 | 0.262 ± 0.00773 | 0.255 ± 0.00756 | 0.0171, 0.017 | model, sandwich |
| invtrgamma | shape2 | 200/200 | 0.166 ± 0.00837 | 0.15 ± 0.00149 | 0.147 ± 0.00149 | 0.00786, 0.00795 | sandwich |
| invtrgamma | scale | 200/200 | 0.539 ± 0.0941 | 0.432 ± 0.0355 | 0.424 ± 0.0367 | 0.0655, 0.0634 | — |
| invweibull | shape | 200/200 | 0.0338 ± 0.0017 | 0.0351 ± 8.84e-05 | 0.0349 ± 0.000144 | 0.00171, 0.00172 | — |
| invweibull | scale | 200/200 | 0.048 ± 0.00204 | 0.0469 ± 0.000223 | 0.0469 ± 0.000223 | 0.00203, 0.00203 | — |
| kumar | a | 200/200 | 0.0594 ± 0.00319 | 0.0588 ± 0.000208 | 0.0584 ± 0.000297 | 0.00316, 0.00317 | — |
| kumar | b | 200/200 | 0.0641 ± 0.003 | 0.0592 ± 0.000313 | 0.0592 ± 0.0004 | 0.00299, 0.00304 | — |
| laplace | sigma | 200/200 | 0.0439 ± 0.00213 | 0.0447 ± 0.000139 | 0.0444 ± 0.000199 | 0.00215, 0.00215 | — |
| lgamma | shapelog | 200/200 | 0.0583 ± 0.00322 | 0.056 ± 0.00025 | 0.0561 ± 0.000337 | 0.00316, 0.00317 | — |
| lgamma | ratelog | 200/200 | 0.072 ± 0.00352 | 0.0718 ± 0.000334 | 0.0716 ± 0.000471 | 0.00349, 0.00346 | — |
| lgser | theta | 200/200 | 0.0215 ± 0.00116 | 0.0249 ± 2.64e-05 | 0.0247 ± 9.64e-05 | 0.00116, 0.00117 | model, sandwich |
| llogis | shape | 200/200 | 0.0363 ± 0.00215 | 0.0375 ± 9.73e-05 | 0.0375 ± 0.000153 | 0.00214, 0.00215 | — |
| llogis | scale | 200/200 | 0.0827 ± 0.00431 | 0.0773 ± 0.000491 | 0.0773 ± 0.0005 | 0.00427, 0.00427 | — |
| lnorm | meanlog | 200/200 | 0.0438 ± 0.00203 | 0.0446 ± 9.86e-05 | 0.0446 ± 9.86e-05 | 0.00203, 0.00203 | — |
| lnorm | sdlog | 200/200 | 0.0312 ± 0.00155 | 0.0315 ± 6.97e-05 | 0.0316 ± 0.000161 | 0.00156, 0.00158 | — |
| logis | location | 200/200 | 0.0824 ± 0.00406 | 0.0775 ± 0.000191 | 0.0776 ± 0.000208 | 0.00407, 0.00407 | — |
| logis | scale | 200/200 | 0.0346 ± 0.00184 | 0.0374 ± 9.13e-05 | 0.0374 ± 0.000149 | 0.00185, 0.00185 | — |
| logitnorm | mu | 200/200 | 0.0442 ± 0.00227 | 0.0447 ± 0.000104 | 0.0447 ± 0.000104 | 0.00228, 0.00228 | — |
| logitnorm | sigma | 200/200 | 0.0329 ± 0.00154 | 0.0316 ± 7.36e-05 | 0.0312 ± 0.000134 | 0.00155, 0.00154 | — |
| lomax | lambda | 200/200 | 0.151 ± 0.00704 | 0.157 ± 0.00162 | 0.157 ± 0.0018 | 0.00685, 0.00686 | — |
| lomax | kappa | 200/200 | 0.0932 ± 0.00463 | 0.0907 ± 0.000895 | 0.0902 ± 0.000979 | 0.00435, 0.00439 | — |
| lst | mu | 200/200 | 0.052 ± 0.0024 | 0.0517 ± 0.000148 | 0.0518 ± 0.000154 | 0.00242, 0.00242 | — |
| lst | sigma | 200/200 | 0.0512 ± 0.00299 | 0.0546 ± 0.000215 | 0.0545 ± 0.000304 | 0.00298, 0.003 | — |
| lst | df | 200/200 | 1.45 ± 0.179 | 1.35 ± 0.0577 | 1.33 ± 0.0587 | 0.137, 0.134 | — |
| naka | shape | 200/200 | 0.0542 ± 0.0026 | 0.0564 ± 0.000233 | 0.0557 ± 0.000302 | 0.00253, 0.00253 | — |
| naka | scale | 200/200 | 0.0436 ± 0.0022 | 0.0446 ± 0.000167 | 0.0445 ± 0.000186 | 0.00225, 0.00225 | — |
| nbinom | size | 200/200 | 0.172 ± 0.0102 | 0.166 ± 0.00288 | 0.165 ± 0.00291 | 0.00904, 0.00914 | — |
| nbinom | prob | 200/200 | 0.0435 ± 0.00203 | 0.0424 ± 0.000177 | 0.0419 ± 0.000275 | 0.00201, 0.00203 | — |
| norm | mean | 200/200 | 0.0438 ± 0.00226 | 0.0448 ± 9.1e-05 | 0.0448 ± 9.1e-05 | 0.00228, 0.00228 | — |
| norm | sd | 200/200 | 0.0288 ± 0.00155 | 0.0317 ± 6.44e-05 | 0.0316 ± 0.000129 | 0.00155, 0.00156 | — |
| nsbeta | shape1 | 200/200 | 0.0591 ± 0.00259 | 0.059 ± 0.000262 | 0.0588 ± 0.000346 | 0.00255, 0.00255 | — |
| nsbeta | shape2 | 200/200 | 0.0568 ± 0.00247 | 0.0589 ± 0.000251 | 0.0587 ± 0.000343 | 0.00245, 0.00245 | — |
| paralogis | shape | 200/200 | 0.0265 ± 0.00134 | 0.0273 ± 5.87e-05 | 0.0273 ± 0.000117 | 0.00133, 0.00133 | — |
| paralogis | scale | 200/200 | 0.0981 ± 0.00478 | 0.0881 ± 0.000532 | 0.0879 ± 0.000568 | 0.00479, 0.0048 | model, sandwich |
| pareto | a | 200/200 | 0.0412 ± 0.00199 | 0.0448 ± 0.00013 | 0.0447 ± 0.000197 | 0.002, 0.00201 | — |
| pareto2 | shape | 200/200 | 0.101 ± 0.00648 | 0.0923 ± 0.00102 | 0.0921 ± 0.00101 | 0.0062, 0.00628 | — |
| pareto2 | scale | 200/200 | 0.165 ± 0.00848 | 0.162 ± 0.00206 | 0.161 ± 0.00209 | 0.00798, 0.00804 | — |
| pareto3 | shape | 200/200 | 0.0393 ± 0.00187 | 0.0375 ± 0.000103 | 0.0376 ± 0.000146 | 0.00187, 0.00188 | — |
| pareto3 | scale | 200/200 | 0.079 ± 0.00393 | 0.0775 ± 0.000506 | 0.0775 ± 0.000512 | 0.00382, 0.00382 | — |
| pareto4 | shape1 | 200/200 | 0.164 ± 0.00799 | 0.174 ± 0.00291 | 0.172 ± 0.00284 | 0.00756, 0.00765 | — |
| pareto4 | shape2 | 200/200 | 0.0705 ± 0.00352 | 0.0683 ± 0.000489 | 0.0681 ± 0.00055 | 0.00335, 0.00335 | — |
| pareto4 | scale | 200/200 | 0.28 ± 0.0163 | 0.29 ± 0.00782 | 0.287 ± 0.00776 | 0.014, 0.0141 | — |
| pois | lambda | 200/200 | 0.0438 ± 0.00207 | 0.0447 ± 6.94e-05 | 0.0448 ± 0.000114 | 0.00208, 0.00209 | — |
| power | beta | 200/200 | 0.0447 ± 0.00241 | 0.0447 ± 0.000141 | 0.0447 ± 0.000218 | 0.00238, 0.00238 | — |
| rayleigh | sigma | 200/200 | 0.0229 ± 0.00113 | 0.0224 ± 3.62e-05 | 0.0222 ± 8.26e-05 | 0.00114, 0.00113 | — |
| sged | — | 0/200 | — | — | — | — | skipped_slow |
| sgomp | b | 200/200 | 0.0534 ± 0.00271 | 0.0555 ± 0.000157 | 0.055 ± 0.000294 | 0.00272, 0.00276 | — |
| sgomp | eta | 200/200 | 0.181 ± 0.00904 | 0.181 ± 0.000572 | 0.18 ± 0.000701 | 0.00891, 0.00891 | — |
| skellam | mu1 | 200/200 | 0.0788 ± 0.00386 | 0.0745 ± 0.000319 | 0.0742 ± 0.000413 | 0.00383, 0.00385 | — |
| skellam | mu2 | 200/200 | 0.0691 ± 0.00313 | 0.0744 ± 0.000308 | 0.0743 ± 0.000401 | 0.00313, 0.00315 | — |
| slash | — | 0/200 | — | — | — | — | skipped_slow |
| sn | — | 0/200 | — | — | — | — | skipped_slow |
| snorm | mean | 200/200 | 0.0459 ± 0.00241 | 0.0446 ± 9.01e-05 | 0.0446 ± 9.01e-05 | 0.00242, 0.00242 | — |
| snorm | sd | 200/200 | 0.0285 ± 0.00145 | 0.0316 ± 6.37e-05 | 0.0313 ± 0.000131 | 0.00145, 0.00146 | model |
| sstd | mean | 200/200 | 0.0388 ± 0.00192 | 0.0399 ± 0.000107 | 0.04 ± 0.000108 | 0.00192, 0.00192 | — |
| sstd | sd | 200/200 | 0.0566 ± 0.00343 | 0.0562 ± 0.0011 | 0.0553 ± 0.00118 | 0.00307, 0.00301 | — |
| sstd | nu | 200/200 | 2.17 ± 0.406 | 1.55 ± 0.127 | 1.47 ± 0.112 | 0.294, 0.306 | model, sandwich |
| std | mean | 200/200 | 0.0384 ± 0.00198 | 0.0401 ± 0.000105 | 0.0402 ± 0.000107 | 0.00198, 0.00198 | — |
| std | sd | 200/200 | 0.0526 ± 0.00313 | 0.056 ± 0.00096 | 0.0548 ± 0.000974 | 0.00303, 0.00305 | — |
| std | nu | 200/200 | 1.67 ± 0.259 | 1.42 ± 0.0821 | 1.36 ± 0.074 | 0.19, 0.198 | — |
| tbinom | prob | 200/200 | 0.00658 ± 0.000301 | 0.0071 ± 2.28e-07 | 0.00711 ± 1.36e-05 | 0.000302, 0.000302 | — |
| tnorm | mean | 200/200 | 0.0458 ± 0.0023 | 0.0447 ± 9.91e-05 | 0.0447 ± 9.91e-05 | 0.0023, 0.0023 | — |
| tnorm | sd | 200/200 | 0.0313 ± 0.00147 | 0.0316 ± 7e-05 | 0.0314 ± 0.000141 | 0.00145, 0.00146 | — |
| tpois | lambda | 200/200 | 0.0531 ± 0.00302 | 0.0549 ± 7.98e-05 | 0.0549 ± 0.000151 | 0.00302, 0.00303 | — |
| trgamma | shape1 | 200/200 | 0.257 ± 0.0169 | 0.252 ± 0.0063 | 0.246 ± 0.00599 | 0.0144, 0.0146 | — |
| trgamma | shape2 | 200/200 | 0.158 ± 0.0118 | 0.149 ± 0.00149 | 0.145 ± 0.00153 | 0.0111, 0.0111 | — |
| trgamma | scale | 200/200 | 0.288 ± 0.0142 | 0.29 ± 0.00166 | 0.284 ± 0.00196 | 0.0149, 0.015 | — |
| weibull | shape | 200/200 | 0.0332 ± 0.00182 | 0.0349 ± 8.71e-05 | 0.0347 ± 0.000146 | 0.0018, 0.00179 | — |
| weibull | scale | 200/200 | 0.0502 ± 0.00272 | 0.047 ± 0.000151 | 0.047 ± 0.000151 | 0.00271, 0.0027 | — |
| yulesimon | rho | 200/200 | 0.0528 ± 0.00281 | 0.0565 ± 0.000262 | 0.0562 ± 0.000312 | 0.00274, 0.00274 | — |
| zeta | s | 200/200 | 0.0503 ± 0.0024 | 0.0476 ± 0.000187 | 0.0474 ± 0.000255 | 0.00241, 0.00242 | — |
| zib | prob | 200/200 | 0.0113 ± 0.000544 | 0.01 ± 1.58e-05 | 0.00998 ± 3.16e-05 | 0.000545, 0.000543 | model, sandwich |
| zib | pi | 200/200 | 0.0223 ± 0.00111 | 0.0224 ± 2.32e-06 | 0.0224 ± 2.32e-06 | 0.00112, 0.00112 | — |
| zinb | size | 175/200 | 1.73 ± 0.261 | 1.61 ± 0.205 | 1.51 ± 0.178 | 0.0912, 0.114 | — |
| zinb | prob | 175/200 | 0.124 ± 0.00616 | 0.123 ± 0.0014 | 0.116 ± 0.00166 | 0.00599, 0.00619 | — |
| zinb | pi | 175/200 | 0.142 ± 0.00958 | 0.175 ± 0.0119 | 0.17 ± 0.0116 | 0.00724, 0.00698 | model, sandwich |
| zip | lambda | 200/200 | 0.0971 ± 0.00497 | 0.098 ± 0.000338 | 0.0974 ± 0.000521 | 0.00496, 0.00496 | — |
| zip | pi | 200/200 | 0.0417 ± 0.00202 | 0.0441 ± 0.000346 | 0.044 ± 0.000357 | 0.00207, 0.00207 | — |
| zipf | s | 200/200 | 0.0545 ± 0.00263 | 0.0563 ± 1.62e-05 | 0.0563 ± 8.18e-05 | 0.00264, 0.00263 | — |
| zmbinom | prob | 200/200 | 0.00992 ± 0.000476 | 0.01 ± 1.62e-05 | 0.00997 ± 3.26e-05 | 0.00048, 0.00048 | — |
| zmbinom | p0 | 200/200 | 0.0228 ± 0.00127 | 0.0223 ± 2.6e-06 | 0.0223 ± 2.6e-06 | 0.00128, 0.00128 | — |
| zmgeom | prob | 200/200 | 0.0224 ± 0.000918 | 0.0223 ± 5.01e-05 | 0.0222 ± 0.000109 | 0.000922, 0.00092 | — |
| zmgeom | p0 | 200/200 | 0.0221 ± 0.00114 | 0.0223 ± 2.25e-06 | 0.0223 ± 2.25e-06 | 0.00115, 0.00115 | — |
| zmlogarithmic | prob | 200/200 | 0.0383 ± 0.00189 | 0.0353 ± 8.04e-05 | 0.0351 ± 0.000193 | 0.00191, 0.00192 | — |
| zmlogarithmic | p0 | 200/200 | 0.0203 ± 0.00138 | 0.0223 ± 2.52e-06 | 0.0223 ± 2.52e-06 | 0.00139, 0.00139 | — |
| zmnbinom | size | 199/200 | 0.638 ± 0.0458 | 0.616 ± 0.0229 | 0.594 ± 0.0219 | 0.0325, 0.0331 | — |
| zmnbinom | prob | 199/200 | 0.0813 ± 0.00386 | 0.083 ± 0.000596 | 0.0794 ± 0.000762 | 0.00381, 0.00387 | — |
| zmnbinom | p0 | 199/200 | 0.0222 ± 0.00115 | 0.0223 ± 2.3e-06 | 0.0223 ± 2.3e-06 | 0.00116, 0.00116 | — |
| zmpois | lambda | 200/200 | 0.0767 ± 0.00384 | 0.0778 ± 0.000191 | 0.0772 ± 0.000314 | 0.00388, 0.0039 | — |
| zmpois | p0 | 200/200 | 0.0212 ± 0.000949 | 0.0223 ± 1.84e-06 | 0.0223 ± 1.84e-06 | 0.000953, 0.000953 | — |
| ztbinom | prob | 200/200 | 0.00678 ± 0.000312 | 0.0071 ± 2.42e-07 | 0.00711 ± 1.46e-05 | 0.000313, 0.000313 | — |
| ztgeom | prob | 200/200 | 0.0155 ± 0.000748 | 0.0158 ± 1.72e-05 | 0.0157 ± 4.96e-05 | 0.000749, 0.000752 | — |
| ztnbinom | size | 200/200 | 0.391 ± 0.0178 | 0.406 ± 0.00869 | 0.405 ± 0.00882 | 0.0165, 0.0164 | — |
| ztnbinom | prob | 200/200 | 0.0578 ± 0.00256 | 0.0586 ± 0.000301 | 0.0583 ± 0.00044 | 0.00262, 0.00262 | — |
| ztpois | lambda | 200/200 | 0.0552 ± 0.00267 | 0.055 ± 8.29e-05 | 0.0549 ± 0.000145 | 0.00268, 0.00269 | — |

Failures by family, with verbatim status counts (details per replication are retained in the source-hashed JSON):

- fpareto: 14/200; fit_failure 7, inference_failure 7.

- gb2: 32/200; fit_failure 14, inference_failure 18.

- genbeta: 49/200; fit_failure 47, inference_failure 2.

- gpd: 1/200; inference_failure 1.

- zinb: 25/200; fit_failure 1, inference_failure 24.

- zmnbinom: 1/200; inference_failure 1.

The Feller–Pareto fit failures selected truncated-gamma (4) or inverse-truncated-gamma (3) limits; its covariance failures were singular (2) or ill-conditioned (5). GB2 selected truncated-gamma (10) or inverse-truncated-gamma (4) limits, with singular (4) or ill-conditioned (14) covariance failures. Generalized beta selected the unit-gamma limit 47 times and had two ill-conditioned covariance failures. The GPD failure fitted outside the recorded positive-shape regime. Zero-inflated negative binomial selected a zero-inflated Poisson limit once, had one ill-conditioned covariance, and reached an excluded boundary/limit 23 times; zero-modified negative binomial reached an excluded boundary/limit once. These are outcomes of the existing fitters and inference checks, not replacement estimates.

Seven slow skips: bhatt, bnbinom, cmp, emg, sged, slash and sn. Simulation agreement under one model point does not prove Wald coverage, robust sandwich validity under misspecification, or validity outside these free subsets. No inference is made from skipped families.

## Verification and R handoff

All commands ran foreground under `/tmp/umvl-cpu/run4` except the resumable simulation, which used one `/tmp/umvl-cpu/run` slot. `just cpp-test`: 105/105; `just cpp-test-no-long-double`: 126/126; `just test`: 19,843 pass, 0 fail, 0 warn, 151 tier skips; `just test-full`: 41,289 pass, 0 fail, 0 warn, one optional `{sn}` skip; `just js-build` and `just js-test`: 400/400; `just site-check`: green for 98 pages. The pilot, standard, registry, special-regime and Monte Carlo accounting validators passed. The simulation driver itself returned 1 for its recorded failed replications; this is not counted as a clean simulation. C++ build warnings in existing density/test files and an initial Boost lookup fallback to installed BH headers were observed. Shell startup printed a missing optional Cargo environment path. The 90-family golden doctest checks finite symmetric H and covariance positivity or documented status; a separate unavailable family tests its explicit status.

A later R `vcov` method needs the original observations retained in fit objects (the current call and count are insufficient), explicit known/held versus fitted structural coefficients and their free mask, coefficient ordering and names matching README natural coordinates, selection of observed or sandwich covariance, and conversion of unavailable/boundary/nonfinite/singular/indefinite/ill-conditioned statuses and their reasons to informative R errors. It must distinguish a fitted nuisance endpoint from a known one, respect analytic precedence, and avoid advertising Fisher, root-n inference or misspecification robustness without their own assumptions and validation.

## Retained results

Copied at merge from the job's ignored scratch directory:
[Monte Carlo summary](2026-11-01-vcov-scores-data/summary.json),
[protocol check](2026-11-01-vcov-scores-data/protocol-check.json), and the
validator results ([pilot](2026-11-01-vcov-scores-data/validator-pilot.json),
[standard](2026-11-01-vcov-scores-data/validator-standard.json),
[registry](2026-11-01-vcov-scores-data/validator-registry.json),
[special regimes](2026-11-01-vcov-scores-data/validator-special.json)).
Per-replication records (2.6 MB) were not kept; the run is reproducible from
the source hash and seeds.

## Follow-up on simbox (2026-11-01)

The seven slow-skipped families (200 replications) and the five flagged ones
(1,000 replications) were run on simbox with `UMVL_MC_REPS`,
`UMVL_MC_SLOW_LIMIT=1e9` and per-family output directories (master 3b62fb2;
same truth points, seeds and n=500). Summaries: [simbox/](2026-11-01-vcov-scores-data/simbox/).
Entries are mean SE minus empirical SD in paired MCSE units (model, sandwich).

| Family | Reps (ok) | Result |
|---|---:|---|
| fatigue, lgser, paralogis, invgauss | 1000 | all coordinates within 1.5 MCSE: the pilot flags were chance |
| invtrgamma | 1000 | SEs about 5-13% low (shape1 -1.9/-2.9, scale -2.8/-3.3): finite-sample bias of a three-shape family at n=500 |
| emg, sged, slash | 200 | within 2.5 MCSE (sged xi sandwich -2.5) |
| cmp | 200 | lambda +2.8/+2.7, nu +1.5/+1.3 |
| sn (alpha = 0.5) | 200 | omega +14.5/+13.7; xi, alpha heavy-tailed SEs (max 40, 51): the truth point is near alpha = 0, where the direct-parameter information is singular |
| bhatt (a = 0.7, sigma = 1) | 119/200 | sigma +7.4/+7.0, a +3.6/+5.8; 81 failed replications: near the normal limit a -> 0 |
| bnbinom (size = alpha = beta = 1) | 200 | size: model median SE 0.52, sandwich 0.093, empirical SD 0.25; beta <= 1 gives an infinite mean, so the score's second moments are suspect at this point |

No derivative error is indicated: the independent derivative validation above
passes for these families. The sn, bhatt and bnbinom rows show that Wald
covariance is unreliable near singular-information members and in heavy
tails at n=500; an R `vcov` should report the conditioning diagnostic and
say so. A bnbinom check at a point with beta > 2 would separate a heavy-tail
effect from an implementation problem.

### Second simbox follow-up: truth points away from singular members

`UMVL_MC_TRUTH` (master) moved the truth points; n = 500, paired MCSE
units (model, sandwich).

| Family, truth | Ok | Result |
|---|---:|---|
| sn, alpha = 3 | 200/200 | all within 0.5 MCSE: the alpha = 0.5 discrepancy was the singular point |
| bhatt, a = 2 | 199/200 | mu, sigma within 0.8; a +3.6 / +2.2 |
| bnbinom, size 5, alpha 6, beta 4 | 196/200 | size: model +5.9, sandwich +0.2; beta +3.3 / -1.4 |
| bnbinom, size 2, alpha 6, beta 10 | 393/400 | size +2.6 / +0.2 (median SE 0.65 model, 0.43 sandwich); beta +4.2 / +1.8 |

Open: bnbinom observed-information SEs exceed the sandwich and the empirical
SD under the model even away from the size = beta symmetry ridge, while the
derivative validation passes. Size and beta are weakly identified at
n = 500 (heavy-tailed estimates); a larger-n run (the MC driver fixes
n = 500) should show whether the two covariances converge before bnbinom's
observed vcov is trusted.
