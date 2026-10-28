# Behaviour sweep against reference-density optimisation

Run: `OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 MAKEFLAGS=-j1 nice -n 10 Rscript notes/ml-behaviour-sweep.R` after `just test`.

Six starts per oracle: defaults, fitter when available, and seeded Gaussian perturbations in transformed coordinates (a data-scaled Gompertz start is used when its fitter fails). Each start runs Nelder–Mead (800 iterations), then BFGS (400), reltol 1e-10. Positive parameters use log, probabilities logit, Student-t nu uses 2+exp; fatigue location is min(x)-exp. Other locations/endpoints are unconstrained with infeasible densities rejected. All finite start candidates are retained. Binomial and Zipf integer size is fixed at the fitter value; skipped if unavailable. Discrete-uniform endpoints are evaluated by the reference PMF. Convergence is the best candidate's optim code (NA denotes a retained start). No global-optimality claim.

Family seeds: 100000+100*family_index+n+replicate, alphabetic metadata order; edge seeds 200000+index; limit seeds 300000+index; oracle seeds 400000+100*index+dataset_index. Ties duplicate rounded pairs and preserve open support. Outliers on bounded support approach the upper endpoint instead of leaving support. Exact closed finite metadata bounds are tested, plus uniform and Zipf parameter-dependent endpoints. Open endpoints are not inserted. betaExtreme retains floating-point endpoints; betaClipped is the note's clipped control. Limit samples have n=200 before zero deflation. nearPareto reproduces the note's burr stress design.

NOT_MAX compares oracle logLik with the fitter's stored logLik using 1e-4*(abs(logLik)+1). 'ref LL' independently evaluates the returned parameters. ESCAPE tests all large parameters, and small non-location, non-probability parameters. ERROR/BOOST include caught fitter diagnostics; synthetic ERROR also marks nonfinite returns, including legitimate -Inf likelihood at a zero-density support endpoint (Rayleigh), which is not by itself a fitter bug; oracle density exceptions/nonfinite values are infeasible candidates.

## Summary

340 rows across 44 fitters; elapsed 28.1 seconds on one core. Flags may overlap.

- **beta**: betaExtreme [ERROR] — x not in the support of the data;  NO FINITE ORACLE.
- **binom**: outlier [ERROR] — The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument.; SKIP: integer size unavailable.
- **burr**: limit200 [ERROR] — The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlpareto` to fit a Pareto distribution..
- **fatigue**: family20-s1 [NOT_MAX]; family20-s2 [NOT_MAX]; family200-s1 [NOT_MAX]; family200-s2 [NOT_MAX]; small3 [NOT_MAX]; ties [NOT_MAX]; outlier [NOT_MAX].
- **ged**: family20-s1 [ERROR] — GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.; small3 [ERROR] — GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.; outlier [NOT_MAX]; limit200 [ERROR] — GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead..
- **gompertz**: outlier [ERROR] — The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed. The exponential distribution will yield a better fit..
- **invburr**: limit200 [ERROR] — The inverse Burr maximum likelihood estimator does not exist when all observations <=1. Use `mlpower` to fit a power distribution..
- **kumar**: betaExtreme [ERROR] — x not in the support of the data;  NO FINITE ORACLE; betaClipped [ERROR] — Kumaraswamy MLE failed.
- **lgser**: small3 [ERROR] — All observations equals 1; the maximum likelihood estimator does not exist..
- **lomax**: family20-s1 [ERROR] — The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution.; small3 [ERROR] — The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution.; limit200 [ERROR] — The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution..
- **lst**: family20-s2 [ERROR] — The location-scale t likelihood is maximised as df -> Inf. Use `mlnorm` to fit the normal distribution.; small3 [ERROR] — The location-scale t likelihood is maximised as df -> Inf. Use `mlnorm` to fit the normal distribution..
- **naka**: outlier [ESCAPE].
- **nbinom**: small3 [ERROR] — The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead.; limit200 [ERROR] — The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead..
- **rayleigh**: bound1 [ERROR] — Nonfinite fitter return (no exception): sigma=1.981377 logLik -Inf;  NO FINITE ORACLE.
- **sged**: family20-s1 [ERROR] — SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.; family20-s2 [ERROR] — SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.; small3 [ERROR] — SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.; ties [ERROR] — The likelihood is maximised by a translated half-GED with mode at the sample minimum; no MLE in the skewed family.; outlier [ERROR] — The likelihood is maximised by a translated half-GED with mode at the sample minimum; no MLE in the skewed family.;  NO FINITE ORACLE; limit200 [ERROR] — SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead..
- **snorm**: family20-s1 [ERROR] — The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.; family20-s2 [ERROR] — The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.; small3 [ERROR] — The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.; ties [ERROR] — The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.; outlier [ERROR] — The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.;  NO FINITE ORACLE; limit200 [ERROR] — The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal..
- **sstd**: family20-s1 [ERROR] — The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, i.e. by a translated half-normal with endpoint at the sample minimum. Use `mlhnorm` to fit this model.; family20-s2 [ERROR] — The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, i.e. by a translated half-normal with endpoint at the sample minimum. Use `mlhnorm` to fit this model.; small3 [ERROR] — The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, i.e. by a translated half-normal with endpoint at the sample minimum. Use `mlhnorm` to fit this model.; limit200 [ERROR] — The skew Student-t likelihood is maximised as nu -> Inf, i.e. by the skew-normal distribution. Use `mlsnorm` to fit this model..
- **std**: family20-s2 [ERROR] — The Student-t likelihood is maximised as nu -> Inf, i.e. by the normal distribution. Use `mlnorm` to fit this model.; small3 [ERROR] — The Student-t likelihood is maximised as nu -> Inf, i.e. by the normal distribution. Use `mlnorm` to fit this model.; outlier [ERROR] — The Student-t likelihood is maximised as nu -> 2 (infinite variance); the MLE does not exist in the mean/sd parameterisation. Use `mllst` to fit a location-scale t distribution with free degrees of freedom.; limit200 [ERROR] — The Student-t likelihood is maximised as nu -> Inf, i.e. by the normal distribution. Use `mlnorm` to fit this model..
- **zipf**: family20-s1 [ERROR] — Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.; SKIP: integer size unavailable; small3 [ERROR] — Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.; SKIP: integer size unavailable.

### Reading the flags

Interpretation is in notes/ml-behaviour.tex. ESCAPE is a magnitude threshold, not a proof of nonexistence. NOT_MAX is a counterexample from a finite search by an unrestricted oracle, so it also fires where the fitter deliberately restricts the space (GED and SGED fit nu >= 1, decision D6; fatigue fixes mu = 0). OK does not certify optimality. ERROR rows include the intended limit-model stops (decision D1).

## All fits

| id | dataset (n) | flags | estimates | LL | ref LL | oracle estimates | oracle LL | starts / conv | diagnostics |
|---|---|---|---|---:|---:|---|---:|---|---|
| beta | family20-s1 (20) | OK | shape1=1.882879, shape2=2.762103 | 4.13123863 | 4.13123863 | shape1=1.882879, shape2=2.762103 | 4.13123863 | 6 / 0 |  |
| beta | family20-s2 (20) | OK | shape1=1.677582, shape2=3.055995 | 5.20097838 | 5.20097838 | shape1=1.677582, shape2=3.055995 | 5.20097838 | 6 / 0 |  |
| beta | family200-s1 (200) | OK | shape1=1.816983, shape2=2.894262 |  45.339346 |  45.339346 | shape1=1.816983, shape2=2.894262 |  45.339346 | 6 / 0 |  |
| beta | family200-s2 (200) | OK | shape1=2.328615, shape2=3.523496 | 58.4354653 | 58.4354653 | shape1=2.328615, shape2=3.523496 | 58.4354653 | 6 / 0 |  |
| beta | small3 (3) | OK | shape1=37.50472, shape2=572.4509 | 9.66632094 | 9.66632094 | shape1=37.50472, shape2=572.4509 | 9.66632094 | 6 / 0 |  |
| beta | ties (200) | OK | shape1=1.910625, shape2=2.913422 | 45.1700125 | 45.1700125 | shape1=1.910624, shape2=2.913422 | 45.1700125 | 6 / 0 |  |
| beta | outlier (200) | OK | shape1=1.542646, shape2= 2.25417 | 29.0685878 | 29.0685878 | shape1=1.542646, shape2= 2.25417 | 29.0685878 | 6 / 0 |  |
| beta | betaExtreme (200) | ERROR | — |         NA |         NA | — |       -Inf | 6 / NA | x not in the support of the data;  NO FINITE ORACLE |
| beta | betaClipped (200) | OK | shape1=0.06567505, shape2=0.0713187 | 2092.48859 | 2092.48859 | shape1=0.06567505, shape2=0.0713187 | 2092.48859 | 6 / 0 |  |
| betapr | family20-s1 (20) | OK | shape1=1.935055, shape2=3.935258 | -10.5972792 | -10.5972792 | shape1=1.935055, shape2=3.935258 | -10.5972792 | 6 / 0 |  |
| betapr | family20-s2 (20) | OK | shape1=2.422118, shape2=5.251421 | -6.9651595 | -6.9651595 | shape1=2.422118, shape2=5.251421 | -6.9651595 | 6 / 0 |  |
| betapr | family200-s1 (200) | OK | shape1=1.956949, shape2=2.997168 | -182.594752 | -182.594752 | shape1=1.956949, shape2=2.997168 | -182.594752 | 6 / 0 |  |
| betapr | family200-s2 (200) | OK | shape1=1.893786, shape2=2.771717 | -198.820471 | -198.820471 | shape1=1.893786, shape2=2.771717 | -198.820471 | 6 / 0 |  |
| betapr | small3 (3) | OK | shape1=12.90395, shape2=14.25233 | -1.13351001 | -1.13351001 | shape1=12.90395, shape2=14.25233 | -1.13351001 | 6 / 0 |  |
| betapr | ties (200) | OK | shape1=2.155096, shape2=3.071295 | -193.382115 | -193.382115 | shape1=2.155096, shape2=3.071295 | -193.382115 | 6 / 0 |  |
| betapr | outlier (200) | OK | shape1=1.638507, shape2=2.155449 | -245.39294 | -245.39294 | shape1=1.638507, shape2=2.155448 | -245.39294 | 6 / 0 |  |
| binom | family20-s1 (20) | OK | size=       7, prob=0.6142857 | -33.6925875 | -33.6925875 | size=       7, prob=0.6142857 | -33.6925875 | 6 / 0 | integer size held at fitter value |
| binom | family20-s2 (20) | OK | size=       8, prob=     0.6 | -46.0725449 | -46.0725449 | size=       8, prob=     0.6 | -46.0725449 | 6 / 0 | integer size held at fitter value |
| binom | family200-s1 (200) | OK | size=       9, prob=0.5583333 | -376.41813 | -376.41813 | size=       9, prob=0.5583333 | -376.41813 | 6 / 0 | integer size held at fitter value |
| binom | family200-s2 (200) | OK | size=       8, prob=  0.6075 | -374.264473 | -374.264473 | size=       8, prob=  0.6075 | -374.264473 | 6 / 0 | integer size held at fitter value |
| binom | small3 (3) | OK | size=       5, prob=       1 |          0 |          0 | size=       5, prob=       1 |          0 | 6 / 0 | integer size held at fitter value |
| binom | ties (200) | OK | size=       9, prob=    0.56 | -370.135181 | -370.135181 | size=       9, prob=    0.56 | -370.135181 | 6 / 0 | integer size held at fitter value |
| binom | outlier (200) | ERROR | — |         NA |         NA | — |       -Inf | 0 / NA | The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument.; SKIP: integer size unavailable |
| binom | bound1 (200) | OK | size=      10, prob=   0.507 | -378.570501 | -378.570501 | size=      10, prob=   0.507 | -378.570501 | 6 / 0 | integer size held at fitter value |
| binom | limit200 (200) | OK | size=      11, prob=0.3586364 | -428.762146 | -428.762146 | size=      11, prob=0.3586364 | -428.762146 | 6 / 0 | integer size held at fitter value |
| burr | family20-s1 (20) | OK | shape1=0.3838842, shape2=6.756366 | -17.705144 | -17.705144 | shape1=0.3838842, shape2=6.756366 | -17.705144 | 6 / 0 |  |
| burr | family20-s2 (20) | OK | shape1=0.5948927, shape2=2.963853 | -29.3818293 | -29.3818293 | shape1=0.5948927, shape2=2.963853 | -29.3818293 | 6 / 0 |  |
| burr | family200-s1 (200) | OK | shape1=0.9717599, shape2=1.924188 | -276.348236 | -276.348236 | shape1=0.9717599, shape2=1.924188 | -276.348236 | 6 / 0 |  |
| burr | family200-s2 (200) | OK | shape1=1.056699, shape2=1.888316 | -258.911773 | -258.911773 | shape1=1.056699, shape2=1.888316 | -258.911773 | 6 / 0 |  |
| burr | small3 (3) | OK | shape1=1.586085, shape2=1.793703 | -2.59356566 | -2.59356566 | shape1=1.586085, shape2=1.793703 | -2.59356566 | 6 / 0 |  |
| burr | ties (200) | OK | shape1=0.8306885, shape2=2.283269 | -279.614247 | -279.614247 | shape1=0.8306885, shape2=2.283269 | -279.614247 | 6 / 0 |  |
| burr | outlier (200) | OK | shape1=0.8359636, shape2=2.172019 | -290.096138 | -290.096138 | shape1=0.8359636, shape2=2.172019 | -290.096138 | 6 / 0 |  |
| burr | limit200 (200) | ERROR | — |         NA |         NA | shape1=0.009573089, shape2=62.62218 | -635.954028 | 6 / 0 | The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlpareto` to fit a Pareto distribution. |
| burr | nearPareto (200) | OK | shape1=0.01051526, shape2=160.2565 | -217.262783 | -217.262783 | shape1=0.01051526, shape2=160.2565 | -217.262783 | 6 / 0 |  |
| cauchy | family20-s1 (20) | OK | location=0.4870035, scale=0.5296971 | -35.7486058 | -35.7486058 | location=0.4870035, scale=0.5296971 | -35.7486058 | 6 / 0 |  |
| cauchy | family20-s2 (20) | OK | location=0.03179257, scale=0.5648902 | -40.6207598 | -40.6207598 | location=0.03179257, scale=0.5648902 | -40.6207598 | 6 / 0 |  |
| cauchy | family200-s1 (200) | OK | location=-0.1687973, scale= 1.02075 | -496.967514 | -496.967514 | location=-0.1687973, scale= 1.02075 | -496.967514 | 6 / 0 |  |
| cauchy | family200-s2 (200) | OK | location=0.03984636, scale=0.8200693 | -486.008641 | -486.008641 | location=0.03984636, scale=0.8200693 | -486.008641 | 6 / 0 |  |
| cauchy | small3 (3) | OK | location=-2.36947, scale=0.272742 | -4.99686179 | -4.99686179 | location=-2.36947, scale=0.272742 | -4.99686179 | 6 / 0 |  |
| cauchy | ties (200) | OK | location=-0.1627303, scale=1.020042 | -518.26648 | -518.26648 | location=-0.1627303, scale=1.020042 | -518.26648 | 6 / 0 |  |
| cauchy | outlier (200) | OK | location=-0.06839027, scale=1.077238 | -529.044166 | -529.044166 | location=-0.06839027, scale=1.077238 | -529.044166 | 6 / 0 |  |
| dunif | family20-s1 (20) | OK | min=       2, max=      10 | -43.9444915 | -43.9444915 | min=       2, max=      10 | -43.9444915 | 6 / 0 |  |
| dunif | family20-s2 (20) | OK | min=       2, max=      10 | -43.9444915 | -43.9444915 | min=       2, max=      10 | -43.9444915 | 6 / 0 |  |
| dunif | family200-s1 (200) | OK | min=       2, max=      10 | -439.444915 | -439.444915 | min=       2, max=      10 | -439.444915 | 6 / 0 |  |
| dunif | family200-s2 (200) | OK | min=       2, max=      10 | -439.444915 | -439.444915 | min=       2, max=      10 | -439.444915 | 6 / 0 |  |
| dunif | small3 (3) | OK | min=       2, max=      10 | -6.59167373 | -6.59167373 | min=       2, max=      10 | -6.59167373 | 6 / 0 |  |
| dunif | ties (200) | OK | min=       2, max=      10 | -439.444915 | -439.444915 | min=       2, max=      10 | -439.444915 | 6 / 0 |  |
| dunif | outlier (200) | OK | min=       2, max=    1010 |  -1383.343 |  -1383.343 | min=       2, max=    1010 |  -1383.343 | 6 / 0 |  |
| dunif | bounds (200) | OK | min=       2, max=      10 | -439.444915 | -439.444915 | min=       2, max=      10 | -439.444915 | 6 / 0 |  |
| exp | family20-s1 (20) | OK | rate=1.033505 | -19.3408837 | -19.3408837 | rate=1.033505 | -19.3408837 | 6 / 0 |  |
| exp | family20-s2 (20) | OK | rate=1.101411 | -18.0681535 | -18.0681535 | rate=1.101411 | -18.0681535 | 6 / 0 |  |
| exp | family200-s1 (200) | OK | rate=1.075134 | -185.510926 | -185.510926 | rate=1.075134 | -185.510926 | 6 / 0 |  |
| exp | family200-s2 (200) | OK | rate=1.002362 | -199.528168 | -199.528168 | rate=1.002362 | -199.528168 | 6 / 0 |  |
| exp | small3 (3) | OK | rate=0.7542708 | -3.84601153 | -3.84601153 | rate=0.7542708 | -3.84601153 | 6 / 0 |  |
| exp | ties (200) | OK | rate=1.140942 | -173.629111 | -173.629111 | rate=1.140942 | -173.629111 | 6 / 0 |  |
| exp | outlier (200) | OK | rate=0.04222953 | -832.927127 | -832.927127 | rate=0.04222953 | -832.927127 | 6 / 0 |  |
| exp | bound1 (200) | OK | rate=1.039325 | -192.285621 | -192.285621 | rate=1.039325 | -192.285621 | 6 / 0 |  |
| fatigue | family20-s1 (20) | NOT_MAX | alpha=2.695492, beta=1.043702, mu=       0 | -30.0789909 | -30.0789909 | alpha=3.492221, beta=0.6295699, mu=0.03812651 | -28.1491537 | 6 / 0 |  |
| fatigue | family20-s2 (20) | NOT_MAX | alpha=2.231773, beta=2.511365, mu=       0 | -58.2276542 | -58.2276542 | alpha=3.410906, beta=1.080813, mu=0.1337636 | -57.0370725 | 6 / 0 |  |
| fatigue | family200-s1 (200) | NOT_MAX | alpha=3.088485, beta=1.913226, mu=       0 | -518.73277 | -518.73277 | alpha=3.124117, beta=1.869444, mu=0.002739018 | -518.662138 | 6 / 0 |  |
| fatigue | family200-s2 (200) | NOT_MAX | alpha=2.866368, beta=1.930562, mu=       0 | -552.009358 | -552.009358 | alpha=2.752478, beta=2.094545, mu=-0.01113943 | -551.543059 | 6 / 0 |  |
| fatigue | small3 (3) | NOT_MAX | alpha=1.023909, beta=6.096777, mu=       0 | -9.70584077 | -9.70584077 | alpha=   15138, beta=5.088408e-08, mu=1.712996 | 24.7994388 | 6 / 0 |  |
| fatigue | ties (200) | NOT_MAX | alpha=2.867508, beta=3.187083, mu=       0 | -667.246289 | -667.246289 | alpha=3.189715, beta=2.569925, mu=0.04183659 | -665.611559 | 6 / 0 |  |
| fatigue | outlier (200) | NOT_MAX | alpha= 9.51885, beta=26.03501, mu=       0 | -757.732359 | -757.732359 | alpha=8.904843, beta=33.09684, mu=-0.0340658 | -753.471174 | 6 / 0 |  |
| gamma | family20-s1 (20) | OK | shape=1.507514, rate=1.932172 | -14.1263107 | -14.1263107 | shape=1.507514, rate=1.932172 | -14.1263107 | 6 / 0 |  |
| gamma | family20-s2 (20) | OK | shape=1.316054, rate= 1.76955 | -13.646995 | -13.646995 | shape=1.316054, rate= 1.76955 | -13.646995 | 6 / 0 |  |
| gamma | family200-s1 (200) | OK | shape=2.050844, rate=1.813912 | -199.895603 | -199.895603 | shape=2.050844, rate=1.813912 | -199.895603 | 6 / 0 |  |
| gamma | family200-s2 (200) | OK | shape=2.061477, rate=1.846282 | -197.085335 | -197.085335 | shape=2.061477, rate=1.846282 | -197.085335 | 6 / 0 |  |
| gamma | small3 (3) | OK | shape=6.849756, rate= 5.65974 | -1.79159077 | -1.79159077 | shape=6.849756, rate= 5.65974 | -1.79159077 | 6 / 0 |  |
| gamma | ties (200) | OK | shape=1.895702, rate=1.843535 | -185.425102 | -185.425102 | shape=1.895702, rate=1.843535 | -185.425102 | 6 / 0 |  |
| gamma | outlier (200) | OK | shape=0.2187359, rate=0.009901941 | -496.778866 | -496.778866 | shape=0.2187359, rate=0.009901941 | -496.778866 | 6 / 0 |  |
| ged | family20-s1 (20) | ERROR | — |         NA |         NA | mean=-0.1693578, sd=0.8921449, nu=1.310397e+10 | -22.5665332 | 6 / 0 | GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead. |
| ged | family20-s2 (20) | OK | mean=0.2311242, sd=0.9809279, nu=2.041987 | -27.9927958 | -27.9927958 | mean=0.231121, sd=0.9809285, nu=2.042006 | -27.9927958 | 6 / 0 |  |
| ged | family200-s1 (200) | OK | mean=0.008941059, sd=0.9341586, nu=2.672474 | -268.746929 | -268.746929 | mean=0.00894106, sd=0.9341587, nu=2.672474 | -268.746929 | 6 / 0 |  |
| ged | family200-s2 (200) | OK | mean=0.1261826, sd=0.9481109, nu=2.370705 | -272.605875 | -272.605875 | mean=0.1261826, sd=0.9481109, nu=2.370705 | -272.605875 | 6 / 0 |  |
| ged | small3 (3) | ERROR | — |         NA |         NA | mean=-0.2814174, sd=0.707152, nu=4.021259e+11 | -2.68783101 | 6 / 0 | GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead. |
| ged | ties (200) | OK | mean=0.1573841, sd=  1.0181, nu=3.850451 | -281.580056 | -281.580056 | mean=0.1573841, sd=  1.0181, nu=3.850451 | -281.580056 | 6 / 0 |  |
| ged | outlier (200) | NOT_MAX | mean=0.0320165, sd=8.247442, nu=       1 | -691.295344 | -691.295344 | mean=0.007007992, sd=3.440885, nu=0.3711634 | -384.604927 | 6 / 0 |  |
| ged | limit200 (200) | ERROR | — |         NA |         NA | mean=0.01061674, sd=0.5711922, nu=3.674771e+10 | -136.484777 | 6 / 0 | GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead. |
| geom | family20-s1 (20) | OK | prob=0.5555556 | -24.7306168 | -24.7306168 | prob=0.5555556 | -24.7306168 | 6 / 0 |  |
| geom | family20-s2 (20) | OK | prob=0.5128205 | -27.0199181 | -27.0199181 | prob=0.5128205 | -27.0199181 | 6 / 0 |  |
| geom | family200-s1 (200) | OK | prob=0.5141388 | -269.478706 | -269.478706 | prob=0.5141388 | -269.478706 | 6 / 0 |  |
| geom | family200-s2 (200) | OK | prob=0.5333333 | -259.096241 | -259.096241 | prob=0.5333333 | -259.096241 | 6 / 0 |  |
| geom | small3 (3) | OK | prob=    0.75 | -2.24934058 | -2.24934058 | prob=    0.75 | -2.24934058 | 6 / 0 |  |
| geom | ties (200) | OK | prob=0.5102041 | -271.632056 | -271.632056 | prob=0.5102041 | -271.632056 | 6 / 0 |  |
| geom | outlier (200) | OK | prob=0.02380386 | -945.182036 | -945.182036 | prob=0.02380386 | -945.182036 | 6 / 0 |  |
| geom | bound1 (200) | OK | prob=0.4975124 | -278.640191 | -278.640191 | prob=0.4975124 | -278.640191 | 6 / 0 |  |
| gompertz | family20-s1 (20) | OK | a=0.7296506, b=1.446791 | -7.96461793 | -7.96461793 | a=0.7296506, b=1.446791 | -7.96461793 | 6 / 0 |  |
| gompertz | family20-s2 (20) | OK | a=0.7174015, b=1.280716 | -9.32873994 | -9.32873994 | a=0.7174015, b=1.280716 | -9.32873994 | 6 / 0 |  |
| gompertz | family200-s1 (200) | OK | a=0.8329667, b=1.261813 | -80.6502332 | -80.6502332 | a=0.8329667, b=1.261813 | -80.6502332 | 6 / 0 |  |
| gompertz | family200-s2 (200) | OK | a=0.9117502, b=1.037099 | -88.2158631 | -88.2158631 | a=0.9117502, b=1.037099 | -88.2158631 | 6 / 0 |  |
| gompertz | small3 (3) | OK | a=2.457701, b=3.944852 | 2.09880808 | 2.09880808 | a=2.457701, b=3.944852 | 2.09880808 | 6 / 0 |  |
| gompertz | ties (200) | OK | a=1.043937, b=0.9395113 | -80.4888315 | -80.4888315 | a=1.043937, b=0.9395113 | -80.4888315 | 6 / 0 |  |
| gompertz | outlier (200) | ERROR | — |         NA |         NA | a=0.1046662, b=6.073164e-12 | -651.407397 | 6 / 0 | The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed. The exponential distribution will yield a better fit. |
| gompertz | bound1 (200) | OK | a=0.9674775, b=1.164518 | -71.6706872 | -71.6706872 | a=0.9674775, b=1.164518 | -71.6706872 | 6 / 0 |  |
| gompertz | limit200 (200) | OK | a=0.8958494, b=0.006055923 | -220.65362 | -220.65362 | a=0.8958494, b=0.006055923 | -220.65362 | 6 / 0 |  |
| gumbel | family20-s1 (20) | OK | mu=1.592561, sigma=2.817861 | -53.0762366 | -53.0762366 | mu=1.592561, sigma=2.817861 | -53.0762366 | 6 / 0 |  |
| gumbel | family20-s2 (20) | OK | mu=2.359536, sigma=3.673178 | -56.7859201 | -56.7859201 | mu=2.359536, sigma=3.673178 | -56.7859201 | 6 / 0 |  |
| gumbel | family200-s1 (200) | OK | mu=2.819789, sigma= 2.87308 | -525.440133 | -525.440133 | mu=2.819789, sigma= 2.87308 | -525.440133 | 6 / 0 |  |
| gumbel | family200-s2 (200) | OK | mu=2.745462, sigma=2.893911 | -525.408441 | -525.408441 | mu=2.745462, sigma=2.893911 | -525.408441 | 6 / 0 |  |
| gumbel | small3 (3) | OK | mu= 2.23253, sigma=0.2620181 | -0.649972545 | -0.649972545 | mu= 2.23253, sigma=0.2620181 | -0.649972545 | 6 / 0 |  |
| gumbel | ties (200) | OK | mu=3.071849, sigma=2.938979 | -531.105034 | -531.105034 | mu=3.071849, sigma=2.938979 | -531.105034 | 6 / 0 |  |
| gumbel | outlier (200) | OK | mu=3.937706, sigma=6.788036 | -758.35726 | -758.35726 | mu=3.937706, sigma=6.788036 | -758.35726 | 6 / 0 |  |
| hnorm | family20-s1 (20) | OK | mu=0.02398772, sigma= 1.09531 | -16.3365674 | -16.3365674 | mu=0.02398772, sigma= 1.09531 | -16.3365674 | 6 / 0 |  |
| hnorm | family20-s2 (20) | OK | mu=0.1452993, sigma=0.9532912 | -13.5591309 | -13.5591309 | mu=0.1452993, sigma=0.9532912 | -13.5591309 | 6 / 0 |  |
| hnorm | family200-s1 (200) | OK | mu=0.00192176, sigma=0.9502445 | -134.951071 | -134.951071 | mu=0.00192176, sigma=0.9502445 | -134.951071 | 6 / 0 |  |
| hnorm | family200-s2 (200) | OK | mu=0.001881486, sigma=0.9674822 | -138.546625 | -138.546625 | mu=0.001881486, sigma=0.9674822 | -138.546625 | 6 / 0 |  |
| hnorm | small3 (3) | OK | mu=0.5743726, sigma=0.1871267 | 2.85053474 | 2.85053474 | mu=0.5743726, sigma=0.1871267 | 2.85053474 | 6 / 0 |  |
| hnorm | ties (200) | OK | mu=-4.940656e-324, sigma=0.897775 | -123.591117 | -123.591117 | mu=-4.940656e-324, sigma=0.897775 | -123.591117 | 6 / 0 |  |
| hnorm | outlier (200) | OK | mu=0.005892193, sigma=70.95054 | -997.554884 | -997.554884 | mu=0.005892193, sigma=70.95054 | -997.554884 | 6 / 0 |  |
| invburr | family20-s1 (20) | OK | shape1=0.8463122, shape2=2.439858 | -19.9711605 | -19.9711605 | shape1=0.8463122, shape2=2.439858 | -19.9711605 | 6 / 0 |  |
| invburr | family20-s2 (20) | OK | shape1=0.683288, shape2= 3.35117 | -16.1383164 | -16.1383164 | shape1=0.683288, shape2= 3.35117 | -16.1383164 | 6 / 0 |  |
| invburr | family200-s1 (200) | OK | shape1=0.9027267, shape2=2.069899 | -246.60572 | -246.60572 | shape1=0.9027267, shape2=2.069899 | -246.60572 | 6 / 0 |  |
| invburr | family200-s2 (200) | OK | shape1=1.075405, shape2= 1.94408 | -273.188685 | -273.188685 | shape1=1.075405, shape2= 1.94408 | -273.188685 | 6 / 0 |  |
| invburr | small3 (3) | OK | shape1=6.206194, shape2=2.885541 | -3.9738944 | -3.9738944 | shape1=6.206194, shape2=2.885541 | -3.9738944 | 6 / 0 |  |
| invburr | ties (200) | OK | shape1=0.8206162, shape2=2.725563 | -188.977009 | -188.977009 | shape1=0.8206162, shape2=2.725563 | -188.977009 | 6 / 0 |  |
| invburr | outlier (200) | OK | shape1=0.8866074, shape2=2.309344 | -227.840874 | -227.840874 | shape1=0.8866074, shape2=2.309344 | -227.840874 | 6 / 0 |  |
| invburr | limit200 (200) | ERROR | — |         NA |         NA | shape1=9.3116e-05, shape2=20679.86 | 34.9125555 | 6 / 0 | The inverse Burr maximum likelihood estimator does not exist when all observations <=1. Use `mlpower` to fit a power distribution. |
| invgamma | family20-s1 (20) | OK | alpha=3.344981, beta=5.536029 | -30.5085058 | -30.5085058 | alpha=3.344981, beta=5.536029 | -30.5085058 | 6 / 0 |  |
| invgamma | family20-s2 (20) | OK | alpha=2.749845, beta=3.778753 | -29.6765768 | -29.6765768 | alpha=2.749845, beta=3.778753 | -29.6765768 | 6 / 0 |  |
| invgamma | family200-s1 (200) | OK | alpha=3.584211, beta=4.283745 | -230.232764 | -230.232764 | alpha=3.584211, beta=4.283745 | -230.232764 | 6 / 0 |  |
| invgamma | family200-s2 (200) | OK | alpha=2.752637, beta=3.719515 | -293.248003 | -293.248003 | alpha=2.752637, beta=3.719515 | -293.248003 | 6 / 0 |  |
| invgamma | small3 (3) | OK | alpha=9.665954, beta=12.58835 | -1.85593516 | -1.85593516 | alpha=9.665954, beta=12.58835 | -1.85593516 | 6 / 0 |  |
| invgamma | ties (200) | OK | alpha=3.612438, beta=4.912663 | -254.967618 | -254.967618 | alpha=3.612438, beta=4.912663 | -254.967618 | 6 / 0 |  |
| invgamma | outlier (200) | OK | alpha=2.624754, beta=3.630304 | -305.22337 | -305.22337 | alpha=2.624754, beta=3.630304 | -305.22337 | 6 / 0 |  |
| invgauss | family20-s1 (20) | OK | mean=2.325296, shape=4.103154 | -32.447487 | -32.447487 | mean=2.325296, shape=4.103154 | -32.447487 | 6 / 0 |  |
| invgauss | family20-s2 (20) | OK | mean= 3.12799, shape=3.923526 | -40.5385673 | -40.5385673 | mean= 3.12799, shape=3.923526 | -40.5385673 | 6 / 0 |  |
| invgauss | family200-s1 (200) | OK | mean=2.670216, shape=4.149569 | -362.277954 | -362.277954 | mean=2.670216, shape=4.149569 | -362.277954 | 6 / 0 |  |
| invgauss | family200-s2 (200) | OK | mean=3.059574, shape=4.093179 | -393.435237 | -393.435237 | mean=3.059574, shape=4.093179 | -393.435237 | 6 / 0 |  |
| invgauss | small3 (3) | OK | mean=3.722709, shape=31.59127 | -4.73901149 | -4.73901149 | mean=3.722709, shape=31.59127 | -4.73901149 | 6 / 0 |  |
| invgauss | ties (200) | OK | mean=   3.065, shape=3.417347 | -390.226658 | -390.226658 | mean=   3.065, shape=3.417347 | -390.226658 | 6 / 0 |  |
| invgauss | outlier (200) | OK | mean=127.6294, shape=1.787777 | -483.910089 | -483.910089 | mean=127.6294, shape=1.787777 | -483.910089 | 6 / 0 |  |
| invgauss | limit200 (200) | OK | mean=765.3349, shape=0.9775628 | -681.557532 | -681.557532 | mean=765.3349, shape=0.9775628 | -681.557532 | 6 / 0 |  |
| invweibull | family20-s1 (20) | OK | shape= 2.07152, rate=3.244765 | -0.731136584 | -0.731136584 | shape= 2.07152, rate=3.244765 | -0.731136584 | 6 / 0 |  |
| invweibull | family20-s2 (20) | OK | shape= 1.36938, rate= 2.99459 | -12.3942812 | -12.3942812 | shape= 1.36938, rate= 2.99459 | -12.3942812 | 6 / 0 |  |
| invweibull | family200-s1 (200) | OK | shape=2.188246, rate=2.964793 | 3.42590995 | 3.42590995 | shape=2.188246, rate=2.964793 | 3.42590995 | 6 / 0 |  |
| invweibull | family200-s2 (200) | OK | shape=2.109534, rate=2.860511 | -8.34150716 | -8.34150716 | shape=2.109534, rate=2.860511 | -8.34150716 | 6 / 0 |  |
| invweibull | small3 (3) | OK | shape=3.790931, rate=3.114379 | 2.39979348 | 2.39979348 | shape=3.790931, rate=3.114379 | 2.39979348 | 6 / 0 |  |
| invweibull | ties (200) | OK | shape=2.026181, rate=3.000987 | -15.6025341 | -15.6025341 | shape=2.026181, rate=3.000987 | -15.6025341 | 6 / 0 |  |
| invweibull | outlier (200) | OK | shape=1.785987, rate=2.859863 | -60.9829456 | -60.9829456 | shape=1.785987, rate=2.859863 | -60.9829456 | 6 / 0 |  |
| kumar | family20-s1 (20) | OK | a=1.752849, b=2.821805 | 3.77717605 | 3.77717605 | a=1.752849, b=2.821805 | 3.77717605 | 6 / 0 |  |
| kumar | family20-s2 (20) | OK | a=1.546461, b=1.445212 | 1.13054846 | 1.13054846 | a=1.546461, b=1.445212 | 1.13054846 | 6 / 0 |  |
| kumar | family200-s1 (200) | OK | a=2.069979, b=3.153637 | 44.2911925 | 44.2911925 | a=2.069979, b=3.153637 | 44.2911925 | 6 / 0 |  |
| kumar | family200-s2 (200) | OK | a=1.936542, b=2.884325 | 40.5035129 | 40.5035129 | a=1.936542, b=2.884325 | 40.5035129 | 6 / 0 |  |
| kumar | small3 (3) | OK | a=1.562351, b=3.512836 | 0.86767967 | 0.86767967 | a=1.562351, b=3.512836 | 0.86767967 | 6 / 0 |  |
| kumar | ties (200) | OK | a=1.976649, b=2.486462 | 32.4898693 | 32.4898693 | a=1.976649, b=2.486462 | 32.4898693 | 6 / 0 |  |
| kumar | outlier (200) | OK | a=1.793459, b=2.210674 | 26.4735475 | 26.4735475 | a=1.793459, b=2.210674 | 26.4735475 | 6 / 0 |  |
| kumar | betaExtreme (200) | ERROR | — |         NA |         NA | — |       -Inf | 6 / NA | x not in the support of the data;  NO FINITE ORACLE |
| kumar | betaClipped (200) | ERROR | — |         NA |         NA | a=0.02226046, b=0.09844177 | 2072.94756 | 6 / 0 | Kumaraswamy MLE failed |
| laplace | family20-s1 (20) | OK | mu=-0.1431219, sigma=1.671885 | -44.1419745 | -44.1419745 | mu=-0.1431219, sigma=1.671885 | -44.1419745 | 6 / 0 |  |
| laplace | family20-s2 (20) | OK | mu=0.02068775, sigma=1.078285 | -35.3703833 | -35.3703833 | mu=0.02068775, sigma=1.078285 | -35.3703833 | 6 / 0 |  |
| laplace | family200-s1 (200) | OK | mu=0.1179301, sigma=0.9523273 | -328.860143 | -328.860143 | mu=0.1179301, sigma=0.9523273 | -328.860143 | 6 / 0 |  |
| laplace | family200-s2 (200) | OK | mu=-0.06399813, sigma=0.9508091 | -328.541045 | -328.541045 | mu=-0.06399813, sigma=0.9508091 | -328.541045 | 6 / 0 |  |
| laplace | small3 (3) | OK | mu=0.1133339, sigma=1.155218 | -5.51230917 | -5.51230917 | mu=0.1133339, sigma=1.155218 | -5.51230917 | 6 / 0 |  |
| laplace | ties (200) | OK | mu=       0, sigma=   1.026 | -343.762985 | -343.762985 | mu=       0, sigma=   1.026 | -343.762985 | 6 / 0 |  |
| laplace | outlier (200) | OK | mu=-0.06829554, sigma=5.933393 | -694.748666 | -694.748666 | mu=-0.06829554, sigma=5.933393 | -694.748666 | 6 / 0 |  |
| lgamma | family20-s1 (20) | OK | shapelog=2.070662, ratelog=3.211541 | -21.5945476 | -21.5945476 | shapelog=2.070662, ratelog=3.211541 | -21.5945476 | 6 / 0 |  |
| lgamma | family20-s2 (20) | OK | shapelog=2.248407, ratelog=2.919068 | -27.1537749 | -27.1537749 | shapelog=2.248407, ratelog=2.919068 | -27.1537749 | 6 / 0 |  |
| lgamma | family200-s1 (200) | OK | shapelog=1.912286, ratelog=2.755401 | -245.11171 | -245.11171 | shapelog=1.912286, ratelog=2.755401 | -245.11171 | 6 / 0 |  |
| lgamma | family200-s2 (200) | OK | shapelog=1.684127, ratelog=2.479596 | -244.440251 | -244.440251 | shapelog=1.684127, ratelog=2.479596 | -244.440251 | 6 / 0 |  |
| lgamma | small3 (3) | OK | shapelog=3.853901, ratelog=13.51028 | 0.950901932 | 0.950901932 | shapelog=3.853901, ratelog=13.51028 | 0.950901932 | 6 / 0 |  |
| lgamma | ties (200) | OK | shapelog=1.545376, ratelog=2.167607 | -264.787188 | -264.787188 | shapelog=1.545376, ratelog=2.167607 | -264.787188 | 6 / 0 |  |
| lgamma | outlier (200) | OK | shapelog=1.523981, ratelog=2.035208 | -282.361595 | -282.361595 | shapelog=1.523981, ratelog=2.035208 | -282.361595 | 6 / 0 |  |
| lgser | family20-s1 (20) | OK | theta=0.8821225 | -42.2716593 | -42.2716593 | theta=0.8821225 | -42.2716593 | 6 / 0 |  |
| lgser | family20-s2 (20) | OK | theta=0.9130687 | -46.5146738 | -46.5146738 | theta=0.9130687 | -46.5146738 | 6 / 0 |  |
| lgser | family200-s1 (200) | OK | theta=0.8846042 | -406.498389 | -406.498389 | theta=0.8846042 | -406.498389 | 6 / 0 |  |
| lgser | family200-s2 (200) | OK | theta=0.8919573 | -430.142958 | -430.142958 | theta=0.8919573 | -430.142958 | 6 / 0 |  |
| lgser | small3 (3) | ERROR | — |         NA |         NA | theta=6.662463e-18 |          0 | 6 / 0 | All observations equals 1; the maximum likelihood estimator does not exist. |
| lgser | ties (200) | OK | theta=0.8860492 | -389.195473 | -389.195473 | theta=0.8860492 | -389.195473 | 6 / 0 |  |
| lgser | outlier (200) | OK | theta=0.9993668 | -614.253641 | -614.253641 | theta=0.9993668 | -614.253641 | 6 / 0 |  |
| lgser | bound1 (200) | OK | theta=0.9135089 | -434.21881 | -434.21881 | theta=0.9135089 | -434.21881 | 6 / 0 |  |
| llogis | family20-s1 (20) | OK | shape=0.9392093, rate=1.858651 | -27.1502576 | -27.1502576 | shape=0.9392093, rate=1.858651 | -27.1502576 | 6 / 0 |  |
| llogis | family20-s2 (20) | OK | shape= 1.12766, rate=1.825458 | -24.0147323 | -24.0147323 | shape= 1.12766, rate=1.825458 | -24.0147323 | 6 / 0 |  |
| llogis | family200-s1 (200) | OK | shape=1.007686, rate=2.170186 | -240.385007 | -240.385007 | shape=1.007686, rate=2.170186 | -240.385007 | 6 / 0 |  |
| llogis | family200-s2 (200) | OK | shape= 1.01227, rate=2.207707 | -238.172094 | -238.172094 | shape= 1.01227, rate=2.207707 | -238.172094 | 6 / 0 |  |
| llogis | small3 (3) | OK | shape=1.327073, rate=0.6504117 | -6.53181749 | -6.53181749 | shape=1.327073, rate=0.6504117 | -6.53181749 | 6 / 0 |  |
| llogis | ties (200) | OK | shape=1.021041, rate=2.216429 | -235.992431 | -235.992431 | shape=1.021041, rate=2.216429 | -235.992431 | 6 / 0 |  |
| llogis | outlier (200) | OK | shape=1.031735, rate=1.940466 | -265.849246 | -265.849246 | shape=1.031735, rate=1.940466 | -265.849246 | 6 / 0 |  |
| lnorm | family20-s1 (20) | OK | meanlog=0.1900505, sdlog=1.116071 | -34.3760667 | -34.3760667 | meanlog=0.1900505, sdlog=1.116071 | -34.3760667 | 6 / 0 |  |
| lnorm | family20-s2 (20) | OK | meanlog=0.4020252, sdlog=1.014096 | -36.6992315 | -36.6992315 | meanlog=0.4020252, sdlog=1.014096 | -36.6992315 | 6 / 0 |  |
| lnorm | family200-s1 (200) | OK | meanlog=-0.01220681, sdlog=1.021482 | -285.59717 | -285.59717 | meanlog=-0.01220681, sdlog=1.021482 | -285.59717 | 6 / 0 |  |
| lnorm | family200-s2 (200) | OK | meanlog=-0.006036334, sdlog=0.9294014 | -267.937521 | -267.937521 | meanlog=-0.006036334, sdlog=0.9294014 | -267.937521 | 6 / 0 |  |
| lnorm | small3 (3) | OK | meanlog=-0.4123799, sdlog=0.7142422 | -2.01007637 | -2.01007637 | meanlog=-0.4123799, sdlog=0.7142422 | -2.01007637 | 6 / 0 |  |
| lnorm | ties (200) | OK | meanlog=-0.2025813, sdlog=0.9687726 | -236.926361 | -236.926361 | meanlog=-0.2025813, sdlog=0.9687726 | -236.926361 | 6 / 0 |  |
| lnorm | outlier (200) | OK | meanlog=-0.06446098, sdlog=1.176469 | -303.399083 | -303.399083 | meanlog=-0.06446098, sdlog=1.176469 | -303.399083 | 6 / 0 |  |
| logis | family20-s1 (20) | OK | location=0.9674018, scale=0.6642755 | -31.9877789 | -31.9877789 | location=0.9674018, scale=0.6642755 | -31.9877789 | 6 / 0 |  |
| logis | family20-s2 (20) | OK | location=0.1457225, scale=0.9267163 | -38.134185 | -38.134185 | location=0.1457225, scale=0.9267163 | -38.134185 | 6 / 0 |  |
| logis | family200-s1 (200) | OK | location=0.03045361, scale=0.9086531 | -380.143548 | -380.143548 | location=0.03045361, scale=0.9086531 | -380.143548 | 6 / 0 |  |
| logis | family200-s2 (200) | OK | location=0.02488716, scale=0.9606148 | -391.439545 | -391.439545 | location=0.02488716, scale=0.9606148 | -391.439545 | 6 / 0 |  |
| logis | small3 (3) | OK | location=-0.4980511, scale=0.5547214 | -4.10596487 | -4.10596487 | location=-0.4980511, scale=0.5547214 | -4.10596487 | 6 / 0 |  |
| logis | ties (200) | OK | location=-0.01461082, scale=0.8578083 | -368.261932 | -368.261932 | location=-0.01461082, scale=0.8578083 | -368.261932 | 6 / 0 |  |
| logis | outlier (200) | OK | location=0.1649959, scale=5.337012 | -805.207073 | -805.207073 | location=0.1649959, scale=5.337012 | -805.207073 | 6 / 0 |  |
| logitnorm | family20-s1 (20) | OK | mu=1.903696, sigma=3.370474 | 14.4930217 | 14.4930217 | mu=1.903696, sigma=3.370474 | 14.4930217 | 6 / 0 |  |
| logitnorm | family20-s2 (20) | OK | mu= 1.73923, sigma=2.870701 | 12.4848306 | 12.4848306 | mu= 1.73923, sigma=2.870701 | 12.4848306 | 6 / 0 |  |
| logitnorm | family200-s1 (200) | OK | mu=1.783042, sigma=2.882561 | 119.076864 | 119.076864 | mu=1.783042, sigma=2.882561 | 119.076864 | 6 / 0 |  |
| logitnorm | family200-s2 (200) | OK | mu=1.969252, sigma=2.995718 | 145.479418 | 145.479418 | mu=1.969252, sigma=2.995718 | 145.479418 | 6 / 0 |  |
| logitnorm | small3 (3) | OK | mu= 1.71947, sigma=1.504515 | 1.39600965 | 1.39600965 | mu= 1.71947, sigma=1.504515 | 1.39600965 | 6 / 0 |  |
| logitnorm | ties (200) | OK | mu=2.095721, sigma=2.715221 |  148.11558 |  148.11558 | mu=2.095721, sigma=2.715221 |  148.11558 | 6 / 0 |  |
| logitnorm | outlier (200) | OK | mu=2.213956, sigma=3.285657 | 160.339249 | 160.339249 | mu=2.213956, sigma=3.285657 | 160.339249 | 6 / 0 |  |
| lomax | family20-s1 (20) | ERROR | — |         NA |         NA | lambda=2.273009e-11, kappa=8.815004e+10 | -6.10058094 | 6 / 0 | The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution. |
| lomax | family20-s2 (20) | OK | lambda=0.8007976, kappa=2.247065 | -17.1509456 | -17.1509456 | lambda=0.8007976, kappa=2.247065 | -17.1509456 | 6 / 0 |  |
| lomax | family200-s1 (200) | OK | lambda=0.4456557, kappa=3.633297 | -158.660038 | -158.660038 | lambda=0.4456557, kappa=3.633297 | -158.660038 | 6 / 0 |  |
| lomax | family200-s2 (200) | OK | lambda=0.5500546, kappa=3.434924 | -130.97405 | -130.97405 | lambda=0.5500546, kappa=3.434924 | -130.97405 | 6 / 0 |  |
| lomax | small3 (3) | ERROR | — |         NA |         NA | lambda=2.334e-11, kappa=1.518862e+11 | 0.796634223 | 6 / 0 | The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution. |
| lomax | ties (200) | OK | lambda=1.529001, kappa= 1.34627 | -204.168219 | -204.168219 | lambda=1.529001, kappa= 1.34627 | -204.168219 | 6 / 0 |  |
| lomax | outlier (200) | OK | lambda=1.968961, kappa=1.130952 | -216.729004 | -216.729004 | lambda=1.968961, kappa=1.130952 | -216.729004 | 6 / 0 |  |
| lomax | limit200 (200) | ERROR | — |         NA |         NA | lambda=3.014038e-11, kappa=3.098043e+10 | -213.706884 | 6 / 0 | The Lomax maximum likelihood estimator does not exist for data with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an exponential distribution. |
| lst | family20-s1 (20) | OK | mu=-0.4961059, sigma=1.242644, df=6.824005 | -35.7505807 | -35.7505807 | mu=-0.4961059, sigma=1.242644, df=6.824005 | -35.7505807 | 6 / 0 |  |
| lst | family20-s2 (20) | ERROR | — |         NA |         NA | mu=-0.3046267, sigma=1.005864, df=9.468527e+09 | -28.495711 | 6 / 0 | The location-scale t likelihood is maximised as df -> Inf. Use `mlnorm` to fit the normal distribution. |
| lst | family200-s1 (200) | OK | mu=0.0643059, sigma=0.9923766, df=2.607166 | -364.344895 | -364.344895 | mu=0.0643059, sigma=0.9923766, df=2.607166 | -364.344895 | 6 / 0 |  |
| lst | family200-s2 (200) | OK | mu=-0.0273355, sigma=0.8285044, df=2.215273 | -343.467187 | -343.467187 | mu=-0.0273355, sigma=0.8285044, df=2.215273 | -343.467187 | 6 / 0 |  |
| lst | small3 (3) | ERROR | — |         NA |         NA | mu=-0.546311, sigma=2.166979, df=8.905312e+08 | -6.57681468 | 6 / 0 | The location-scale t likelihood is maximised as df -> Inf. Use `mlnorm` to fit the normal distribution. |
| lst | ties (200) | OK | mu=0.02949201, sigma=1.026721, df=4.808878 | -332.490806 | -332.490806 | mu=0.02949201, sigma=1.026721, df=4.808878 | -332.490806 | 6 / 0 |  |
| lst | outlier (200) | OK | mu=-0.01277995, sigma=0.8925996, df=1.962528 | -371.491565 | -371.491565 | mu=-0.01277995, sigma=0.8925996, df=1.962528 | -371.491565 | 6 / 0 |  |
| naka | family20-s1 (20) | OK | shape=  2.1633, scale=2.330904 | -14.2967689 | -14.2967689 | shape=  2.1633, scale=2.330904 | -14.2967689 | 6 / 0 |  |
| naka | family20-s2 (20) | OK | shape=2.200435, scale=2.171599 | -13.4383406 | -13.4383406 | shape=2.200435, scale=2.171599 | -13.4383406 | 6 / 0 |  |
| naka | family200-s1 (200) | OK | shape=1.833287, scale= 2.96101 | -181.271109 | -181.271109 | shape=1.833287, scale= 2.96101 | -181.271109 | 6 / 0 |  |
| naka | family200-s2 (200) | OK | shape=2.096834, scale=2.778112 | -163.266514 | -163.266514 | shape=2.096834, scale=2.778112 | -163.266514 | 6 / 0 |  |
| naka | small3 (3) | OK | shape=11.95238, scale=1.708847 | 0.762102887 | 0.762102887 | shape=11.95238, scale=1.708847 | 0.762102887 | 6 / 0 |  |
| naka | ties (200) | OK | shape=2.470157, scale=  2.6906 | -145.476027 | -145.476027 | shape=2.470157, scale=  2.6906 | -145.476027 | 6 / 0 |  |
| naka | outlier (200) | ESCAPE | shape=0.08009865, scale=71147.32 | -657.284952 | -657.284952 | shape=0.08009865, scale=71147.32 | -657.284952 | 6 / 0 |  |
| naka | limit200 (200) | OK | shape=0.2845906, scale=0.2707932 | 15.8568709 | 15.8568709 | shape=0.2845906, scale=0.2707932 | 15.8568709 | 6 / 0 |  |
| nbinom | family20-s1 (20) | OK | size=6.478218, prob=0.2247179 | -73.2631874 | -73.2631874 | size=6.478218, prob=0.2247179 | -73.2631874 | 6 / 0 |  |
| nbinom | family20-s2 (20) | OK | size=13.58237, prob=0.4194371 | -65.9198207 | -65.9198207 | size=13.58237, prob=0.4194371 | -65.9198207 | 6 / 0 |  |
| nbinom | family200-s1 (200) | OK | size=12.07158, prob=0.3500878 | -694.690908 | -694.690908 | size=12.07158, prob=0.3500878 | -694.690908 | 6 / 0 |  |
| nbinom | family200-s2 (200) | OK | size=8.749211, prob=0.2737618 | -719.292113 | -719.292113 | size=8.749211, prob=0.2737618 | -719.292113 | 6 / 0 |  |
| nbinom | small3 (3) | ERROR | — |         NA |         NA | size=2.44789e+11, prob=       1 | -9.12654008 | 6 / 0 | The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead. |
| nbinom | ties (200) | OK | size=9.187058, prob=0.2797771 | -720.17027 | -720.17027 | size=9.187058, prob=0.2797771 | -720.17027 | 6 / 0 |  |
| nbinom | outlier (200) | OK | size=0.2757299, prob=0.0009357698 | -1135.48611 | -1135.48611 | size=0.2757299, prob=0.0009357698 | -1135.48611 | 6 / 0 |  |
| nbinom | bound1 (200) | OK | size=8.724576, prob=0.2635459 | -731.037422 | -731.037422 | size=8.724576, prob=0.2635459 | -731.037422 | 6 / 0 |  |
| nbinom | limit200 (200) | ERROR | — |         NA |         NA | size=5.961329e+09, prob=       1 | -406.282968 | 6 / 0 | The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead. |
| norm | family20-s1 (20) | OK | mean=0.1186686, sd=0.8126776 | -24.2303551 | -24.2303551 | mean=0.1186686, sd=0.8126776 | -24.2303551 | 6 / 0 |  |
| norm | family20-s2 (20) | OK | mean=-0.02013199, sd=1.091615 | -30.1319393 | -30.1319393 | mean=-0.02013199, sd=1.091615 | -30.1319393 | 6 / 0 |  |
| norm | family200-s1 (200) | OK | mean=0.01993558, sd=0.9280099 | -268.845138 | -268.845138 | mean=0.01993558, sd=0.9280099 | -268.845138 | 6 / 0 |  |
| norm | family200-s2 (200) | OK | mean=0.07062602, sd=1.077807 | -298.773453 | -298.773453 | mean=0.07062602, sd=1.077807 | -298.773453 | 6 / 0 |  |
| norm | small3 (3) | OK | mean=0.2059433, sd=0.3157051 | -0.797975227 | -0.797975227 | mean=0.2059433, sd=0.3157051 | -0.797975227 | 6 / 0 |  |
| norm | ties (200) | OK | mean=  -0.122, sd=0.9442012 | -272.304517 | -272.304517 | mean=  -0.122, sd=0.9442012 | -272.304517 | 6 / 0 |  |
| norm | outlier (200) | OK | mean=5.003272, sd=  70.711 | -1135.50793 | -1135.50793 | mean=5.003272, sd=  70.711 | -1135.50793 | 6 / 0 |  |
| paralogis | family20-s1 (20) | OK | shape=1.046962, scale=1.126747 | -39.6221847 | -39.6221847 | shape=1.046962, scale=1.126747 | -39.6221847 | 6 / 0 |  |
| paralogis | family20-s2 (20) | OK | shape=0.8386358, scale=0.8532368 | -53.0331572 | -53.0331572 | shape=0.8386362, scale=0.8532365 | -53.0331572 | 6 / 0 |  |
| paralogis | family200-s1 (200) | OK | shape=1.027148, scale=0.9958013 | -381.230537 | -381.230537 | shape=1.027148, scale=0.9958013 | -381.230537 | 6 / 0 |  |
| paralogis | family200-s2 (200) | OK | shape=0.9164464, scale=0.9040613 | -439.723633 | -439.723633 | shape=0.9164464, scale=0.9040613 | -439.723633 | 6 / 0 |  |
| paralogis | small3 (3) | OK | shape=2.796557, scale=2.161589 | -2.99903537 | -2.99903537 | shape=2.796557, scale=2.161589 | -2.99903537 | 6 / 0 |  |
| paralogis | ties (200) | OK | shape=1.002522, scale=0.8411946 | -355.543642 | -355.543642 | shape=1.002522, scale=0.8411946 | -355.543642 | 6 / 0 |  |
| paralogis | outlier (200) | OK | shape=0.9731003, scale=0.9513536 | -415.732906 | -415.732906 | shape=0.9731003, scale=0.9513536 | -415.732906 | 6 / 0 |  |
| pareto | family20-s1 (20) | OK | a=1.116996, b=2.141921 | -50.926367 | -50.926367 | a=1.116996, b=2.141921 | -50.926367 | 6 / 0 |  |
| pareto | family20-s2 (20) | OK | a=1.264481, b=2.017687 | -45.1625709 | -45.1625709 | a=1.264481, b=2.017687 | -45.1625709 | 6 / 0 |  |
| pareto | family200-s1 (200) | OK | a= 1.03388, b=2.021582 | -527.558231 | -527.558231 | a= 1.03388, b=2.021582 | -527.558231 | 6 / 0 |  |
| pareto | family200-s2 (200) | OK | a=0.9257838, b=2.004901 | -570.575053 | -570.575053 | a=0.9257838, b=2.004901 | -570.575053 | 6 / 0 |  |
| pareto | small3 (3) | OK | a=0.871042, b=3.710413 | -10.7917757 | -10.7917757 | a=0.8710406, b=3.710413 | -10.7917757 | 6 / 0 |  |
| pareto | ties (200) | OK | a=1.297845, b=     2.1 | -450.347981 | -450.347981 | a=1.297845, b=     2.1 | -450.347981 | 6 / 0 |  |
| pareto | outlier (200) | OK | a=1.069628, b=2.010552 | -513.200691 | -513.200691 | a=1.069628, b=2.010552 | -513.200691 | 6 / 0 |  |
| pois | family20-s1 (20) | OK | lambda=     2.2 | -31.672036 | -31.672036 | lambda=     2.2 | -31.672036 | 6 / 0 |  |
| pois | family20-s2 (20) | OK | lambda=    1.95 | -29.1326057 | -29.1326057 | lambda=    1.95 | -29.1326057 | 6 / 0 |  |
| pois | family200-s1 (200) | OK | lambda=    2.05 | -353.880546 | -353.880546 | lambda=    2.05 | -353.880546 | 6 / 0 |  |
| pois | family200-s2 (200) | OK | lambda=    2.06 | -348.196143 | -348.196143 | lambda=    2.06 | -348.196143 | 6 / 0 |  |
| pois | small3 (3) | OK | lambda=1.666667 | -3.83216624 | -3.83216624 | lambda=1.666667 | -3.83216624 | 6 / 0 |  |
| pois | ties (200) | OK | lambda=    1.82 | -319.577491 | -319.577491 | lambda=    1.82 | -319.577491 | 6 / 0 |  |
| pois | outlier (200) | OK | lambda=   31.94 | -30665.5276 | -30665.5276 | lambda=   31.94 | -30665.5276 | 6 / 0 |  |
| pois | bound1 (200) | OK | lambda=    1.94 | -333.179711 | -333.179711 | lambda=    1.94 | -333.179711 | 6 / 0 |  |
| power | family20-s1 (20) | OK | alpha=0.9970594, beta=1.766105 | 2.75878477 | 2.75878477 | alpha=0.9970594, beta=1.766101 |  2.7587853 | 6 / 0 |  |
| power | family20-s2 (20) | OK | alpha=0.9702235, beta=2.053955 | 4.73723307 | 4.73723307 | alpha=0.9702235, beta=2.053953 |  4.7372337 | 6 / 0 |  |
| power | family200-s1 (200) | OK | alpha=0.993664, beta=2.006936 | 40.2474647 | 40.2474647 | alpha=0.993664, beta=2.006932 | 40.2474707 | 6 / 0 |  |
| power | family200-s2 (200) | OK | alpha=0.996232, beta=2.126586 | 45.7060683 | 45.7060683 | alpha=0.996232, beta=2.126582 | 45.7060747 | 6 / 0 |  |
| power | small3 (3) | OK | alpha=0.9694933, beta=8.110844 | 3.74242589 | 3.74242589 | alpha=0.9694933, beta=8.110843 | 3.74242626 | 6 / 0 |  |
| power | ties (200) | OK | alpha=       1, beta= 2.04309 |  40.783598 |  40.783598 | alpha=       1, beta=2.043085 | 40.7836041 | 6 / 0 |  |
| power | outlier (200) | OK | alpha=998.2901, beta=0.1352507 | -502.598788 | -502.598788 | alpha=998.2901, beta=0.1352507 | -502.598788 | 6 / 0 |  |
| rayleigh | family20-s1 (20) | OK | sigma=2.109339 | -34.0456571 | -34.0456571 | sigma=2.109339 | -34.0456571 | 6 / 0 |  |
| rayleigh | family20-s2 (20) | OK | sigma=2.098131 | -37.1290437 | -37.1290437 | sigma=2.098131 | -37.1290437 | 6 / 0 |  |
| rayleigh | family200-s1 (200) | OK | sigma= 2.11829 | -336.493572 | -336.493572 | sigma= 2.11829 | -336.493572 | 6 / 0 |  |
| rayleigh | family200-s2 (200) | OK | sigma=2.042198 | -324.011289 | -324.011289 | sigma=2.042198 | -324.011289 | 6 / 0 |  |
| rayleigh | small3 (3) | OK | sigma=1.545532 | -3.65489634 | -3.65489634 | sigma=1.545532 | -3.65489634 | 6 / 0 |  |
| rayleigh | ties (200) | OK | sigma=1.973157 | -326.919288 | -326.919288 | sigma=1.973157 | -326.919288 | 6 / 0 |  |
| rayleigh | outlier (200) | OK | sigma=310.1344 | -2338.16267 | -2338.16267 | sigma=310.1344 | -2338.16267 | 6 / 0 |  |
| rayleigh | bound1 (200) | ERROR | sigma=1.981377 |       -Inf |       -Inf | — |       -Inf | 6 / NA | Nonfinite fitter return (no exception): sigma=1.981377 logLik -Inf;  NO FINITE ORACLE |
| sged | family20-s1 (20) | ERROR | — |         NA |         NA | mean=0.1163337, sd=0.692874, nu=3.003622e+09, xi=6.526577e-13 | -17.510925 | 6 / 0 | SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead. |
| sged | family20-s2 (20) | ERROR | — |         NA |         NA | mean=0.05305337, sd=0.7220297, nu=61.87684, xi=8165.275 | -18.6804403 | 6 / 1 | SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead. |
| sged | family200-s1 (200) | OK | mean=0.01824284, sd=0.9802573, nu=3.235319, xi=2.599189 | -264.142894 | -264.142894 | mean=0.01824284, sd=0.9802573, nu=3.235319, xi=2.599189 | -264.142894 | 6 / 0 |  |
| sged | family200-s2 (200) | OK | mean=-0.005014336, sd=0.9291671, nu=4.748839, xi=2.853381 | -252.497924 | -252.497924 | mean=-0.005014336, sd=0.9291671, nu=4.748839, xi=2.853381 | -252.497924 | 6 / 0 |  |
| sged | small3 (3) | ERROR | — |         NA |         NA | mean=-0.137124, sd=0.6297553, nu=2.979554e+08, xi=0.009933107 | -2.34008824 | 6 / 1 | SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead. |
| sged | ties (200) | ERROR | — |         NA |         NA | mean=0.00258299, sd=1.110978, nu=2.736671, xi=5084.615 | -277.087373 | 6 / 1 | The likelihood is maximised by a translated half-GED with mode at the sample minimum; no MLE in the skewed family. |
| sged | outlier (200) | ERROR | — |         NA |         NA | — |       -Inf | 6 / NA | The likelihood is maximised by a translated half-GED with mode at the sample minimum; no MLE in the skewed family.;  NO FINITE ORACLE |
| sged | limit200 (200) | ERROR | — |         NA |         NA | mean=0.004556882, sd=0.572902, nu=1.861275e+10, xi=0.5947389 | -137.082531 | 6 / 0 | SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead. |
| snorm | family20-s1 (20) | ERROR | — |         NA |         NA | mean=0.2361407, sd=1.124363, xi=593.9743 | -26.9697928 | 6 / 1 | The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal. |
| snorm | family20-s2 (20) | ERROR | — |         NA |         NA | mean=-0.0588013, sd=0.7507732, xi=430.9713 | -18.9223876 | 6 / 0 | The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal. |
| snorm | family200-s1 (200) | OK | mean=0.07022684, sd= 1.00098, xi=2.088192 | -267.581734 | -267.581734 | mean=0.07022684, sd= 1.00098, xi=2.088192 | -267.581734 | 6 / 0 |  |
| snorm | family200-s2 (200) | OK | mean=-0.08682112, sd=1.006543, xi=3.392772 | -257.320488 | -257.320488 | mean=-0.08682112, sd=1.006543, xi=3.392772 | -257.320488 | 6 / 0 |  |
| snorm | small3 (3) | ERROR | — |         NA |         NA | mean=-0.1736363, sd=0.9669703, xi=340.2497 | -3.59515959 | 6 / 0 | The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal. |
| snorm | ties (200) | ERROR | — |         NA |         NA | mean=-0.2064287, sd=0.8476631, xi=3.323707 | -223.31581 | 6 / 0 | The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal. |
| snorm | outlier (200) | ERROR | — |         NA |         NA | — |       -Inf | 6 / NA | The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.;  NO FINITE ORACLE |
| snorm | limit200 (200) | ERROR | — |         NA |         NA | mean=1.123968, sd=0.8445203, xi=1096.489 | -212.522292 | 6 / 0 | The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal. |
| sstd | family20-s1 (20) | ERROR | — |         NA |         NA | mean=-0.02947201, sd=0.657931, nu=267.7273, xi=70.15371 | -16.2552952 | 6 / 0 | The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, i.e. by a translated half-normal with endpoint at the sample minimum. Use `mlhnorm` to fit this model. |
| sstd | family20-s2 (20) | ERROR | — |         NA |         NA | mean=-0.1499151, sd=0.6533151, nu= 246.038, xi=104.0828 | -16.0425306 | 6 / 0 | The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, i.e. by a translated half-normal with endpoint at the sample minimum. Use `mlhnorm` to fit this model. |
| sstd | family200-s1 (200) | OK | mean=0.00305645, sd=0.8506224, nu=4.420327, xi=2.091565 | -207.688534 | -207.688534 | mean=0.00305645, sd=0.8506224, nu=4.420327, xi=2.091565 | -207.688534 | 6 / 0 |  |
| sstd | family200-s2 (200) | OK | mean=0.07526192, sd=0.9798905, nu=3.323173, xi=2.009388 | -214.877303 | -214.877303 | mean=0.07526193, sd=0.9798905, nu=3.323173, xi=2.009388 | -214.877303 | 6 / 0 |  |
| sstd | small3 (3) | ERROR | — |         NA |         NA | mean=-0.02519537, sd=0.3154867, nu=7.826334, xi=43.11678 | 0.170234659 | 6 / 0 | The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, i.e. by a translated half-normal with endpoint at the sample minimum. Use `mlhnorm` to fit this model. |
| sstd | ties (200) | OK | mean=-0.05240842, sd=0.9027908, nu=3.178794, xi=1.730272 | -203.230919 | -203.230919 | mean=-0.05240841, sd=0.9027908, nu=3.178794, xi=1.730272 | -203.230919 | 6 / 0 |  |
| sstd | outlier (200) | OK | mean=0.03814742, sd=3.162935, nu=2.069997, xi=1.946004 | -214.078216 | -214.078216 | mean=0.03814742, sd=3.162935, nu=2.069997, xi=1.946004 | -214.078216 | 6 / 0 |  |
| sstd | limit200 (200) | ERROR | — |         NA |         NA | mean=-0.08327883, sd=0.8962027, nu=3.115774e+08, xi=1.141808 | -261.085077 | 6 / 0 | The skew Student-t likelihood is maximised as nu -> Inf, i.e. by the skew-normal distribution. Use `mlsnorm` to fit this model. |
| std | family20-s1 (20) | OK | mean=0.1194614, sd=0.6127605, nu=10.86194 | -18.4290823 | -18.4290823 | mean=0.1194617, sd=0.6127605, nu=10.86194 | -18.4290823 | 6 / 0 |  |
| std | family20-s2 (20) | ERROR | — |         NA |         NA | mean=0.09293993, sd=0.4476277, nu=2.045466e+09 | -12.302856 | 6 / 0 | The Student-t likelihood is maximised as nu -> Inf, i.e. by the normal distribution. Use `mlnorm` to fit this model. |
| std | family200-s1 (200) | OK | mean=0.06544953, sd=0.9312939, nu= 3.19288 | -237.545995 | -237.545995 | mean=0.06544939, sd=0.931294, nu= 3.19288 | -237.545995 | 6 / 0 |  |
| std | family200-s2 (200) | OK | mean=-0.002679864, sd= 1.45468, nu=2.290715 | -246.279104 | -246.279104 | mean=-0.00267981, sd= 1.45468, nu=2.290715 | -246.279104 | 6 / 0 |  |
| std | small3 (3) | ERROR | — |         NA |         NA | mean=-0.2223542, sd=0.4682303, nu=4.249195e+11 | -1.98043054 | 6 / 0 | The Student-t likelihood is maximised as nu -> Inf, i.e. by the normal distribution. Use `mlnorm` to fit this model. |
| std | ties (200) | OK | mean=-0.08268111, sd=0.8485258, nu=3.847013 | -232.301048 | -232.301048 | mean=-0.08268111, sd=0.8485258, nu=3.847013 | -232.301048 | 6 / 0 |  |
| std | outlier (200) | ERROR | — |         NA |         NA | mean=-0.001285905, sd=14634.61, nu=       2 | -261.771627 | 6 / 0 | The Student-t likelihood is maximised as nu -> 2 (infinite variance); the MLE does not exist in the mean/sd parameterisation. Use `mllst` to fit a location-scale t distribution with free degrees of freedom. |
| std | limit200 (200) | ERROR | — |         NA |         NA | mean=0.0680234, sd=1.049321, nu=1.01958e+09 | -293.415998 | 6 / 0 | The Student-t likelihood is maximised as nu -> Inf, i.e. by the normal distribution. Use `mlnorm` to fit this model. |
| unif | family20-s1 (20) | OK | min=0.03088595, max=0.9809958 | 1.02355273 | 1.02355273 | min=0.03088595, max=0.9809958 | 1.02355273 | 6 / 0 |  |
| unif | family20-s2 (20) | OK | min=0.01032741, max=0.9602818 | 1.02682546 | 1.02682546 | min=0.01032741, max=0.9602818 | 1.02682546 | 6 / 0 |  |
| unif | family200-s1 (200) | OK | min=0.003690645, max=0.9951705 | 1.71132621 | 1.71132621 | min=0.003690645, max=0.9951705 | 1.71132621 | 6 / 0 |  |
| unif | family200-s2 (200) | OK | min=0.000195185, max=0.9965801 | 0.724335742 | 0.724335742 | min=0.000195185, max=0.9965801 | 0.724335742 | 6 / 0 |  |
| unif | small3 (3) | OK | min=0.02237538, max=0.775707 | 0.849749269 | 0.849749269 | min=0.02237538, max=0.775707 | 0.849749269 | 6 / 0 |  |
| unif | ties (200) | OK | min=       0, max=       1 |         -0 |          0 | min=       0, max=       1 |          0 | 6 / 0 |  |
| unif | outlier (200) | OK | min=0.002075212, max=1000.992 | -1381.74902 | -1381.74902 | min=0.002075212, max=1000.992 | -1381.74902 | 6 / 0 |  |
| unif | bounds (200) | OK | min=       0, max=       1 |         -0 |          0 | min=       0, max=       1 |          0 | 6 / 0 |  |
| weibull | family20-s1 (20) | OK | shape=3.414027, scale=4.386434 | -33.2944095 | -33.2944095 | shape=3.414027, scale=4.386434 | -33.2944095 | 6 / 0 |  |
| weibull | family20-s2 (20) | OK | shape=3.763651, scale=4.047209 | -29.6694104 | -29.6694104 | shape=3.763651, scale=4.047209 | -29.6694104 | 6 / 0 |  |
| weibull | family200-s1 (200) | OK | shape=2.862549, scale=3.901917 | -336.364812 | -336.364812 | shape=2.862549, scale=3.901917 | -336.364812 | 6 / 0 |  |
| weibull | family200-s2 (200) | OK | shape=3.046075, scale=3.841331 | -323.598145 | -323.598145 | shape=3.046075, scale=3.841331 | -323.598145 | 6 / 0 |  |
| weibull | small3 (3) | OK | shape=6.059002, scale= 5.77082 | -4.50324813 | -4.50324813 | shape=6.059002, scale= 5.77082 | -4.50324813 | 6 / 0 |  |
| weibull | ties (200) | OK | shape=2.870904, scale=4.164135 | -352.302812 | -352.302812 | shape=2.870904, scale=4.164135 | -352.302812 | 6 / 0 |  |
| weibull | outlier (200) | OK | shape=0.5404485, scale=5.730131 | -626.03077 | -626.03077 | shape=0.5404485, scale=5.730131 | -626.03077 | 6 / 0 |  |
| zip | family20-s1 (20) | OK | lambda=1.195758, pi=0.4982261 | -20.1180968 | -20.1180968 | lambda=1.195758, pi=0.4982261 | -20.1180968 | 6 / 0 |  |
| zip | family20-s2 (20) | OK | lambda=     0.1, pi=       0 | -6.60517019 | -6.60517019 | lambda=     0.1, pi=   1e-12 | -6.60517019 | 6 / 0 |  |
| zip | family200-s1 (200) | OK | lambda=0.9241575, pi=0.4860183 | -184.36107 | -184.36107 | lambda=0.9241575, pi=0.4860183 | -184.36107 | 6 / 0 |  |
| zip | family200-s2 (200) | OK | lambda=0.7838707, pi=0.4386829 | -176.074947 | -176.074947 | lambda=0.7838707, pi=0.4386829 | -176.074947 | 6 / 0 |  |
| zip | small3 (3) | OK | lambda=       1, pi=       1 |          0 |          0 | lambda=8.661646e-11, pi=       1 |          0 | 6 / 0 | all observations are zero |
| zip | ties (200) | OK | lambda=0.9781565, pi=0.4070478 | -207.201511 | -207.201511 | lambda=0.9781565, pi=0.4070478 | -207.201511 | 6 / 0 |  |
| zip | outlier (200) | OK | lambda=53.48052, pi=   0.615 | -17079.1815 |       -Inf | — |       -Inf | 6 / NA |  NO FINITE ORACLE |
| zip | bound1 (200) | OK | lambda=0.9553976, pi=0.3824561 | -207.357623 | -207.357623 | lambda=0.9553976, pi=0.3824561 | -207.357623 | 6 / 0 |  |
| zip | limit200 (195) | OK | lambda=3.189744, pi=       0 | -371.161723 | -371.161723 | lambda=3.189743, pi=3.17605e-17 | -371.161723 | 6 / 0 |  |
| zipf | family20-s1 (20) | ERROR | — |         NA |         NA | — |       -Inf | 0 / NA | Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.; SKIP: integer size unavailable |
| zipf | family20-s2 (20) | OK | N=       3, s=0.8785255 | -20.3585009 | -20.3585009 | N=       3, s=0.8785255 | -20.3585009 | 6 / 0 | integer size held at fitter value |
| zipf | family200-s1 (200) | OK | N=       3, s=0.6482924 | -210.858279 | -210.858279 | N=       3, s=0.6482924 | -210.858279 | 6 / 0 | integer size held at fitter value |
| zipf | family200-s2 (200) | OK | N=       3, s=0.5854624 | -212.487547 | -212.487547 | N=       3, s=0.5854624 | -212.487547 | 6 / 0 | integer size held at fitter value |
| zipf | small3 (3) | ERROR | — |         NA |         NA | — |       -Inf | 0 / NA | Optimal shape parameter is less than 0. The maximum likelihood estimator does not exist.; SKIP: integer size unavailable |
| zipf | ties (200) | OK | N=       3, s=0.9016853 | -202.746301 | -202.746301 | N=       3, s=0.9016853 | -202.746301 | 6 / 0 | integer size held at fitter value |
| zipf | outlier (200) | OK | N=    3000, s=2.090261 | -297.513576 | -297.513576 | — |       -Inf | 6 / NA | integer size held at fitter value NO FINITE ORACLE |
| zipf | bound1 (200) | OK | N=       3, s=0.6692136 | -210.280821 | -210.280821 | N=       3, s=0.6692136 | -210.280821 | 6 / 0 | integer size held at fitter value |
| zipf | bounds (200) | OK | N=       3, s=0.6429997 | -211.001622 | -211.001622 | N=       3, s=0.6429997 | -211.001622 | 6 / 0 | integer size held at fitter value |
