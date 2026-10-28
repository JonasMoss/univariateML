# Behaviour sweep against reference-density optimisation

Run: `OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 MAKEFLAGS=-j1 nice -n 10 Rscript notes/ml-behaviour-sweep.R` after `just test`.

Six starts per oracle: defaults, fitter when available, and seeded Gaussian perturbations in transformed coordinates (a data-scaled Gompertz start is used when its fitter fails). Each start runs Nelder–Mead (800 iterations), then BFGS (400), reltol 1e-10. Positive parameters use log, probabilities logit, Student-t nu uses 2+exp; fatigue location is min(x)-exp. Other locations/endpoints are unconstrained with infeasible densities rejected. All finite start candidates are retained. Binomial and Zipf integer size is fixed at the fitter value; skipped if unavailable. Discrete-uniform endpoints are evaluated by the reference PMF. Convergence is the best candidate's optim code (NA denotes a retained start). No global-optimality claim.

Family seeds: 100000+100*family_index+n+replicate, alphabetic metadata order; edge seeds 200000+index; limit seeds 300000+index; oracle seeds 400000+100*index+dataset_index. Ties duplicate rounded pairs and preserve open support. Outliers on bounded support approach the upper endpoint instead of leaving support. Exact closed finite metadata bounds are tested, plus uniform and Zipf parameter-dependent endpoints. Open endpoints are not inserted. betaExtreme retains floating-point endpoints; betaClipped is the note's clipped control. Limit samples have n=200 before zero deflation. nearPareto reproduces the note's burr stress design.

NOT_MAX compares oracle logLik with the fitter's stored logLik using 1e-4*(abs(logLik)+1). 'ref LL' independently evaluates the returned parameters. ESCAPE tests all large parameters, and small non-location, non-probability parameters. ERROR/BOOST include caught fitter diagnostics; synthetic ERROR also marks nonfinite returns, including legitimate -Inf likelihood at a zero-density support endpoint (Rayleigh), which is not by itself a fitter bug; oracle density exceptions/nonfinite values are infeasible candidates.

## Summary

330 rows across 42 fitters; elapsed 5.5 seconds on one core. Flags may overlap.

- **beta**: betaExtreme [ERROR] — x not in the support of the data;  NO FINITE ORACLE.
- **binom**: outlier [ERROR] — The maximum likelihood estimator does not exist. Use `mlpois` to fit a Poisson or supply a `size` argument.; SKIP: integer size unavailable.
- **burr**: limit200 [ERROR] — The Burr maximum likelihood estimator does not exist when all observations >=1. Use `mlpareto` to fit a Pareto distribution..
- **fatigue**: family20-s1 [NOT_MAX]; family20-s2 [NOT_MAX]; family200-s1 [NOT_MAX]; family200-s2 [NOT_MAX]; small3 [NOT_MAX]; ties [NOT_MAX]; outlier [NOT_MAX]; bound1 [ERROR] — Nonfinite fitter return (no exception): alpha=     NaN, beta=     NaN, mu=       0 logLik NaN.
- **ged**: family20-s1 [NOT_MAX]; small3 [ESCAPE]; limit200 [ESCAPE].
- **gompertz**: outlier [ERROR] — The maximum likelihood estimator of the b parameter Gompertz distribution does not exist since the data is overdispersed. The exponential distribution will yield a better fit..
- **invburr**: limit200 [ERROR] — The inverse Burr maximum likelihood estimator does not exist when all observations <=1. Use `mlpower` to fit a power distribution..
- **invgauss**: limit200 [ESCAPE].
- **kumar**: betaExtreme [ERROR] — x not in the support of the data;  NO FINITE ORACLE; betaClipped [ERROR] — Kumaraswamy MLE failed.
- **llogis**: bound1 [ERROR] — Nonfinite fitter return (no exception): shape=     NaN, rate=     NaN logLik NaN.
- **naka**: outlier [ESCAPE].
- **nbinom**: small3 [ERROR] — The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead.; limit200 [ERROR] — The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead..
- **pareto**: bound1 [ERROR] — Nonfinite fitter return (no exception): a=     NaN, b=     NaN logLik NaN;  NO FINITE ORACLE.
- **power**: bound1 [ERROR] — The power distribution is only defined for positive numbers.;  NO FINITE ORACLE.
- **rayleigh**: bound1 [ERROR] — Nonfinite fitter return (no exception): sigma=1.999314 logLik -Inf;  NO FINITE ORACLE.
- **sged**: family20-s1 [NOT_MAX]; family20-s2 [NOT_MAX]; small3 [ESCAPE]; outlier [ESCAPE,NOT_MAX]; limit200 [NOT_MAX].
- **snorm**: outlier [ESCAPE,NOT_MAX]; limit200 [ESCAPE,NOT_MAX].
- **sstd**: family20-s1 [NOT_MAX]; small3 [NOT_MAX].
- **std**: small3 [ESCAPE].

### Comparison with the note

Finite seeded samples need not escape even when generated from a limit model. ESCAPE is the requested numerical threshold, not a proof of nonexistence; NOT_MAX is a lower-bound counterexample from a finite search, while OK does not certify optimality. Reference-density and stored likelihood differences are visible in the table. Fatigue's unrestricted location oracle and extreme Beta data require particular care; the interpretation below explains the differences.

- Fatigue resolves the note’s open question: the adapter hard-codes mu=0 (univariateML/src/ml_exports.cpp), although metadata lists it among parameters. Its NOT_MAX flags compare that two-parameter fit against a three-parameter oracle; they do not establish failure to optimise alpha/beta conditional on mu=0. The small-sample location oracle approaches a singular boundary and is not a trustworthy finite MLE.
- ZIP returns lambda=0, pi=-Inf and NaN likelihood on two nondegenerate samples here, extending the note’s boundary-only description. Fatigue returns NaNs at zero. Binomial small3 returns prob=1 with stored logLik=-Inf while the reference logLik is 0. These silent invalid outputs are flagged ERROR with a synthetic diagnostic.
- The std small3 stored logLik and reference logLik disagree substantially at enormous nu. Numerical cancellation makes that stored value unreliable; this is additional to the note’s escape observation. Other likelihood discrepancies, including reference underflow, remain visible in the table. Nonfinite parameters also occur for cauchy small3, logis outlier, and llogis/pareto at zero; the note’s closure classification does not guarantee numerical robustness on those samples.
- The seeded normal limit samples for std/sstd have finite fitted nu; gompertz, lomax and binom likewise return finite fits on their limit samples. This is sample dependence, not a contradiction of possible escape. GED and snorm reproduce large-parameter escapes; sged’s uniform fit falls below the 1e4 flag threshold but is still improved by the oracle. invburr’s generic failure and burr’s raw Boost pole error reproduce the note.
- Nakagami limit200 returns shape about 0.320, below 1/2, confirming the note’s convention issue. Levy data yield a finite inverse-Gaussian mean about 905340: flagged ESCAPE by magnitude but consistent with the note’s finite-MLE claim.
- Raw Beta(0.05,0.05) samples hit floating-point support endpoints and beta/kumar reject them. On clipped samples beta fits successfully and kumar still fails, reproducing the note’s numerical concern.
- The extraDistr ZIP reference log density underflows on the outlier row, leaving no finite oracle candidate; the Gompertz outlier requires a data-scaled start to avoid overflow. Treat those rows as oracle limitations, not evidence for optimality. OK elsewhere means only that no requested flag fired; it is not an optimality certificate.

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
| fatigue | bound1 (200) | ERROR | alpha=     NaN, beta=     NaN, mu=       0 |        NaN |       -Inf | alpha=1.28399e+81, beta=8.144931e-162, mu=-4.940656e-324 | -24.2535405 | 6 / 0 | Nonfinite fitter return (no exception): alpha=     NaN, beta=     NaN, mu=       0 logLik NaN |
| gamma | family20-s1 (20) | OK | shape=1.507514, rate=1.932172 | -14.1263107 | -14.1263107 | shape=1.507514, rate=1.932172 | -14.1263107 | 6 / 0 |  |
| gamma | family20-s2 (20) | OK | shape=1.316054, rate= 1.76955 | -13.646995 | -13.646995 | shape=1.316054, rate= 1.76955 | -13.646995 | 6 / 0 |  |
| gamma | family200-s1 (200) | OK | shape=2.050844, rate=1.813912 | -199.895603 | -199.895603 | shape=2.050844, rate=1.813912 | -199.895603 | 6 / 0 |  |
| gamma | family200-s2 (200) | OK | shape=2.061477, rate=1.846282 | -197.085335 | -197.085335 | shape=2.061477, rate=1.846282 | -197.085335 | 6 / 0 |  |
| gamma | small3 (3) | OK | shape=6.849756, rate= 5.65974 | -1.79159077 | -1.79159077 | shape=6.849756, rate= 5.65974 | -1.79159077 | 6 / 0 |  |
| gamma | ties (200) | OK | shape=1.895702, rate=1.843535 | -185.425102 | -185.425102 | shape=1.895702, rate=1.843535 | -185.425102 | 6 / 0 |  |
| gamma | outlier (200) | OK | shape=0.2187359, rate=0.009901941 | -496.778866 | -496.778866 | shape=0.2187359, rate=0.009901941 | -496.778866 | 6 / 0 |  |
| ged | family20-s1 (20) | NOT_MAX | mean=-0.1041397, sd=0.807587, nu=1.580238 | -23.9764167 | -23.9764167 | mean=-0.1693578, sd=0.8921449, nu=6.332334e+09 | -22.5665333 | 6 / 0 |  |
| ged | family20-s2 (20) | OK | mean=0.2311214, sd=0.9809284, nu=2.042006 | -27.9927958 | -27.9927958 | mean=0.2311214, sd=0.9809284, nu=2.042006 | -27.9927958 | 6 / 0 |  |
| ged | family200-s1 (200) | OK | mean=0.008941057, sd=0.9341586, nu=2.672474 | -268.746929 | -268.746929 | mean=0.008941058, sd=0.9341586, nu=2.672474 | -268.746929 | 6 / 0 |  |
| ged | family200-s2 (200) | OK | mean=0.1261824, sd=0.9481108, nu=2.370705 | -272.605875 | -272.605875 | mean=0.1261824, sd=0.9481108, nu=2.370705 | -272.605875 | 6 / 0 |  |
| ged | small3 (3) | ESCAPE | mean=-0.2814174, sd=0.7071521, nu=2.626773e+08 | -2.68783127 | -2.68783127 | mean=-0.2814174, sd=0.707152, nu=7.625419e+09 | -2.68783102 | 6 / 0 |  |
| ged | ties (200) | OK | mean=0.1573839, sd=  1.0181, nu=3.850441 | -281.580056 | -281.580056 | mean=0.1573839, sd=  1.0181, nu=3.850441 | -281.580056 | 6 / 0 |  |
| ged | outlier (200) | OK | mean=0.02075233, sd=3.521146, nu=0.367305 | -384.587352 | -384.587352 | mean=0.02075233, sd=3.520977, nu=0.3673021 | -384.587172 | 6 / 0 |  |
| ged | limit200 (200) | ESCAPE | mean=0.01061672, sd=0.5711923, nu=7.798952e+08 | -136.484786 | -136.484786 | mean=0.01061674, sd=0.5711922, nu=1.136246e+10 | -136.484777 | 6 / 0 |  |
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
| invburr | family20-s1 (20) | OK | shape1=1.000379, shape2=1.971179 | -26.6834696 | -26.6834696 | shape1=1.000379, shape2=1.971179 | -26.6834696 | 6 / 0 |  |
| invburr | family20-s2 (20) | OK | shape1=0.5984887, shape2=3.212115 | -13.8762277 | -13.8762277 | shape1=0.5984887, shape2=3.212115 | -13.8762277 | 6 / 0 |  |
| invburr | family200-s1 (200) | OK | shape1=0.9177998, shape2=2.143216 | -239.889992 | -239.889992 | shape1=0.9177998, shape2=2.143216 | -239.889992 | 6 / 0 |  |
| invburr | family200-s2 (200) | OK | shape1=0.9526164, shape2=2.047063 | -253.717284 | -253.717284 | shape1=0.9526164, shape2=2.047063 | -253.717284 | 6 / 0 |  |
| invburr | small3 (3) | OK | shape1=1.204023, shape2=1.398713 | -5.28591569 | -5.28591569 | shape1=1.204023, shape2=1.398713 | -5.28591569 | 6 / 0 |  |
| invburr | ties (200) | OK | shape1=0.9608794, shape2=1.981109 | -260.312369 | -260.312369 | shape1=0.9608794, shape2=1.981109 | -260.312369 | 6 / 0 |  |
| invburr | outlier (200) | OK | shape1=0.9991725, shape2=2.023809 | -267.309481 | -267.309481 | shape1=0.9991725, shape2=2.023809 | -267.309481 | 6 / 0 |  |
| invburr | limit200 (200) | ERROR | — |         NA |         NA | shape1=0.000556112, shape2=3463.604 | 34.9388901 | 6 / 0 | The inverse Burr maximum likelihood estimator does not exist when all observations <=1. Use `mlpower` to fit a power distribution. |
| invgamma | family20-s1 (20) | OK | alpha= 3.58102, beta=4.388145 | -23.5351049 | -23.5351049 | alpha= 3.58102, beta=4.388145 | -23.5351049 | 6 / 0 |  |
| invgamma | family20-s2 (20) | OK | alpha=3.122027, beta=4.268769 | -27.6838399 | -27.6838399 | alpha=3.122027, beta=4.268769 | -27.6838399 | 6 / 0 |  |
| invgamma | family200-s1 (200) | OK | alpha=3.247875, beta=4.508498 | -274.135873 | -274.135873 | alpha=3.247875, beta=4.508498 | -274.135873 | 6 / 0 |  |
| invgamma | family200-s2 (200) | OK | alpha=3.047363, beta=3.996467 | -272.041623 | -272.041623 | alpha=3.047363, beta=3.996467 | -272.041623 | 6 / 0 |  |
| invgamma | small3 (3) | OK | alpha=5.062852, beta=6.250894 | -2.86080121 | -2.86080121 | alpha=5.062852, beta=6.250894 | -2.86080121 | 6 / 0 |  |
| invgamma | ties (200) | OK | alpha=3.594485, beta= 5.19858 | -267.972365 | -267.972365 | alpha=3.594485, beta= 5.19858 | -267.972365 | 6 / 0 |  |
| invgamma | outlier (200) | OK | alpha=2.705112, beta=3.984845 | -313.175641 | -313.175641 | alpha=2.705112, beta=3.984845 | -313.175641 | 6 / 0 |  |
| invgauss | family20-s1 (20) | OK | mean= 2.56677, shape= 6.64832 | -33.5854599 | -33.5854599 | mean= 2.56677, shape= 6.64832 | -33.5854599 | 6 / 0 |  |
| invgauss | family20-s2 (20) | OK | mean=3.679482, shape=3.274924 | -43.9678402 | -43.9678402 | mean=3.679482, shape=3.274924 | -43.9678402 | 6 / 0 |  |
| invgauss | family200-s1 (200) | OK | mean=3.301783, shape=4.408217 | -410.716814 | -410.716814 | mean=3.301783, shape=4.408217 | -410.716814 | 6 / 0 |  |
| invgauss | family200-s2 (200) | OK | mean=3.185565, shape=3.303161 | -402.482597 | -402.482597 | mean=3.185565, shape=3.303161 | -402.482597 | 6 / 0 |  |
| invgauss | small3 (3) | OK | mean=1.502409, shape=59.92852 | 0.105107372 | 0.105107372 | mean=1.502409, shape=59.92852 | 0.105107372 | 6 / 0 |  |
| invgauss | ties (200) | OK | mean=   2.901, shape=4.374141 | -380.893279 | -380.893279 | mean=   2.901, shape=4.374141 | -380.893279 | 6 / 0 |  |
| invgauss | outlier (200) | OK | mean= 70.9351, shape=1.708891 | -468.635767 | -468.635767 | mean= 70.9351, shape=1.708891 | -468.635767 | 6 / 0 |  |
| invgauss | limit200 (200) | ESCAPE | mean=  905340, shape=0.8732877 | -724.682304 | -724.682304 | mean=  905340, shape=0.8732877 | -724.682304 | 6 / 0 |  |
| invweibull | family20-s1 (20) | OK | shape=1.778559, rate=3.156994 | -2.21455153 | -2.21455153 | shape=1.778559, rate=3.156994 | -2.21455153 | 6 / 0 |  |
| invweibull | family20-s2 (20) | OK | shape= 1.37609, rate=2.792496 | -13.4085195 | -13.4085195 | shape= 1.37609, rate=2.792496 | -13.4085195 | 6 / 0 |  |
| invweibull | family200-s1 (200) | OK | shape=1.902963, rate= 3.18309 | -13.1701227 | -13.1701227 | shape=1.902963, rate= 3.18309 | -13.1701227 | 6 / 0 |  |
| invweibull | family200-s2 (200) | OK | shape=2.003388, rate=2.985908 | -20.5422863 | -20.5422863 | shape=2.003388, rate=2.985908 | -20.5422863 | 6 / 0 |  |
| invweibull | small3 (3) | OK | shape=5.729381, rate=4.018689 | 4.27276671 | 4.27276671 | shape=5.729381, rate=4.018689 | 4.27276671 | 6 / 0 |  |
| invweibull | ties (200) | OK | shape=1.707544, rate=3.036363 | -51.9809739 | -51.9809739 | shape=1.707544, rate=3.036363 | -51.9809739 | 6 / 0 |  |
| invweibull | outlier (200) | OK | shape=1.812619, rate=2.871941 | -62.2478135 | -62.2478135 | shape=1.812619, rate=2.871941 | -62.2478135 | 6 / 0 |  |
| kumar | family20-s1 (20) | OK | a=1.652611, b=2.269101 | 2.52128572 | 2.52128572 | a=1.652611, b=2.269101 | 2.52128572 | 6 / 0 |  |
| kumar | family20-s2 (20) | OK | a=1.904291, b=2.666263 | 3.53185394 | 3.53185394 | a=1.904291, b=2.666263 | 3.53185394 | 6 / 0 |  |
| kumar | family200-s1 (200) | OK | a=2.163005, b=3.358813 | 48.7335621 | 48.7335621 | a=2.163005, b=3.358813 | 48.7335621 | 6 / 0 |  |
| kumar | family200-s2 (200) | OK | a=2.149757, b=3.701036 | 53.8916649 | 53.8916649 | a=2.149757, b=3.701036 | 53.8916649 | 6 / 0 |  |
| kumar | small3 (3) | OK | a=2.205887, b=4.537373 | 1.02505859 | 1.02505859 | a=2.205887, b=4.537373 | 1.02505859 | 6 / 0 |  |
| kumar | ties (200) | OK | a=1.922633, b=2.894134 | 39.5051923 | 39.5051923 | a=1.922633, b=2.894134 | 39.5051923 | 6 / 0 |  |
| kumar | outlier (200) | OK | a=1.822544, b=2.535906 | 33.1235724 | 33.1235724 | a=1.822544, b=2.535906 | 33.1235724 | 6 / 0 |  |
| kumar | betaExtreme (200) | ERROR | — |         NA |         NA | — |       -Inf | 6 / NA | x not in the support of the data;  NO FINITE ORACLE |
| kumar | betaClipped (200) | ERROR | — |         NA |         NA | a=0.02148306, b=0.1013046 | 2112.65487 | 6 / 0 | Kumaraswamy MLE failed |
| laplace | family20-s1 (20) | OK | mu=0.2918164, sigma=1.085093 | -35.4962629 | -35.4962629 | mu=0.2918164, sigma=1.085093 | -35.4962629 | 6 / 0 |  |
| laplace | family20-s2 (20) | OK | mu=0.07312485, sigma=1.427451 | -40.9807564 | -40.9807564 | mu=0.07312485, sigma=1.427451 | -40.9807564 | 6 / 0 |  |
| laplace | family200-s1 (200) | OK | mu=0.05078176, sigma=1.018661 | -342.327172 | -342.327172 | mu=0.05078176, sigma=1.018661 | -342.327172 | 6 / 0 |  |
| laplace | family200-s2 (200) | OK | mu=0.1139798, sigma=0.9020691 | -318.016598 | -318.016598 | mu=0.1139798, sigma=0.9020691 | -318.016598 | 6 / 0 |  |
| laplace | small3 (3) | OK | mu=0.3852197, sigma=0.6796709 | -3.92100191 | -3.92100191 | mu=0.3852197, sigma=0.6796709 | -3.92100191 | 6 / 0 |  |
| laplace | ties (200) | OK | mu=    -0.3, sigma=   0.977 | -333.975711 | -333.975711 | mu=    -0.3, sigma=   0.977 | -333.975711 | 6 / 0 |  |
| laplace | outlier (200) | OK | mu=-0.1876476, sigma=6.002355 | -697.059821 | -697.059821 | mu=-0.1876476, sigma=6.002355 | -697.059821 | 6 / 0 |  |
| lgamma | family20-s1 (20) | OK | shapelog=2.935428, ratelog=4.091903 | -22.8438149 | -22.8438149 | shapelog=2.935428, ratelog=4.091903 | -22.8438149 | 6 / 0 |  |
| lgamma | family20-s2 (20) | OK | shapelog=2.994633, ratelog=4.017161 | -24.0266819 | -24.0266819 | shapelog=2.994633, ratelog=4.017161 | -24.0266819 | 6 / 0 |  |
| lgamma | family200-s1 (200) | OK | shapelog=2.179006, ratelog=3.155999 | -235.664278 | -235.664278 | shapelog=2.179006, ratelog=3.155999 | -235.664278 | 6 / 0 |  |
| lgamma | family200-s2 (200) | OK | shapelog=  2.2061, ratelog=3.166542 | -237.949708 | -237.949708 | shapelog=  2.2061, ratelog=3.166542 | -237.949708 | 6 / 0 |  |
| lgamma | small3 (3) | OK | shapelog=2.766133, ratelog= 2.68381 | -5.51801877 | -5.51801877 | shapelog=2.766133, ratelog= 2.68381 | -5.51801877 | 6 / 0 |  |
| lgamma | ties (200) | OK | shapelog= 1.86625, ratelog=2.858765 | -225.973003 | -225.973003 | shapelog= 1.86625, ratelog=2.858765 | -225.973003 | 6 / 0 |  |
| lgamma | outlier (200) | OK | shapelog=1.679635, ratelog=2.482606 | -243.265321 | -243.265321 | shapelog=1.679635, ratelog=2.482606 | -243.265321 | 6 / 0 |  |
| lgser | family20-s1 (20) | OK | theta=0.8547379 | -36.2400001 | -36.2400001 | theta=0.8547379 | -36.2400001 | 6 / 0 |  |
| lgser | family20-s2 (20) | OK | theta=0.9235847 |  -45.04441 |  -45.04441 | theta=0.9235847 |  -45.04441 | 6 / 0 |  |
| lgser | family200-s1 (200) | OK | theta=0.8803286 | -409.939096 | -409.939096 | theta=0.8803286 | -409.939096 | 6 / 0 |  |
| lgser | family200-s2 (200) | OK | theta=0.8977386 | -421.78548 | -421.78548 | theta=0.8977386 | -421.78548 | 6 / 0 |  |
| lgser | small3 (3) | OK | theta=0.8731269 | -6.17056393 | -6.17056393 | theta=0.8731269 | -6.17056393 | 6 / 0 |  |
| lgser | ties (200) | OK | theta=0.8711814 | -386.080029 | -386.080029 | theta=0.8711814 | -386.080029 | 6 / 0 |  |
| lgser | outlier (200) | OK | theta=0.9991039 | -596.744919 | -596.744919 | theta=0.9991039 | -596.744919 | 6 / 0 |  |
| lgser | bound1 (200) | OK | theta=0.8989065 | -416.799388 | -416.799388 | theta=0.8989065 | -416.799388 | 6 / 0 |  |
| llogis | family20-s1 (20) | OK | shape=1.058079, rate=2.245402 | -26.7445551 | -26.7445551 | shape=1.058079, rate=2.245402 | -26.7445551 | 6 / 0 |  |
| llogis | family20-s2 (20) | OK | shape=1.180868, rate=3.198409 | -12.2751729 | -12.2751729 | shape=1.180868, rate=3.198409 | -12.2751729 | 6 / 0 |  |
| llogis | family200-s1 (200) | OK | shape=1.031987, rate=1.806489 | -280.395857 | -280.395857 | shape=1.031987, rate=1.806489 | -280.395857 | 6 / 0 |  |
| llogis | family200-s2 (200) | OK | shape=1.064523, rate=2.187946 | -238.598851 | -238.598851 | shape=1.064523, rate=2.187946 | -238.598851 | 6 / 0 |  |
| llogis | small3 (3) | OK | shape=1.924107, rate=0.5810142 | -5.78923785 | -5.78923785 | shape=1.924107, rate=0.5810142 | -5.78923785 | 6 / 0 |  |
| llogis | ties (200) | OK | shape=0.958976, rate=1.370296 | -324.110818 | -324.110818 | shape=0.958976, rate=1.370296 | -324.110818 | 6 / 0 |  |
| llogis | outlier (200) | OK | shape=0.8661341, rate=1.847891 | -303.986488 | -303.986488 | shape=0.8661341, rate=1.847891 | -303.986488 | 6 / 0 |  |
| llogis | bound1 (200) | ERROR | shape=     NaN, rate=     NaN |        NaN |       -Inf | shape=       1, rate=1.894392 | -281.025069 | 6 / 0 | Nonfinite fitter return (no exception): shape=     NaN, rate=     NaN logLik NaN |
| lnorm | family20-s1 (20) | OK | meanlog=-0.3447884, sdlog=0.974101 | -20.9581968 | -20.9581968 | meanlog=-0.3447884, sdlog=0.974101 | -20.9581968 | 6 / 0 |  |
| lnorm | family20-s2 (20) | OK | meanlog=-0.3183484, sdlog=1.103981 | -23.9902638 | -23.9902638 | meanlog=-0.3183484, sdlog=1.103981 | -23.9902638 | 6 / 0 |  |
| lnorm | family200-s1 (200) | OK | meanlog=0.0843903, sdlog=0.9915307 | -298.964693 | -298.964693 | meanlog=0.0843903, sdlog=0.9915307 | -298.964693 | 6 / 0 |  |
| lnorm | family200-s2 (200) | OK | meanlog=0.1496144, sdlog=1.039799 | -321.51599 | -321.51599 | meanlog=0.1496144, sdlog=1.039799 | -321.51599 | 6 / 0 |  |
| lnorm | small3 (3) | OK | meanlog=0.08188887, sdlog=0.5392705 | -2.64986807 | -2.64986807 | meanlog=0.08188887, sdlog=0.5392705 | -2.64986807 | 6 / 0 |  |
| lnorm | ties (200) | OK | meanlog=-0.03378702, sdlog=0.8836325 | -252.28749 | -252.28749 | meanlog=-0.03378702, sdlog=0.8836325 | -252.28749 | 6 / 0 |  |
| lnorm | outlier (200) | OK | meanlog=0.0166084, sdlog=1.161744 | -317.093782 | -317.093782 | meanlog=0.0166084, sdlog=1.161744 | -317.093782 | 6 / 0 |  |
| logis | family20-s1 (20) | OK | location=0.7617401, scale=1.009982 | -40.2020099 | -40.2020099 | location=0.7617401, scale=1.009982 | -40.2020099 | 6 / 0 |  |
| logis | family20-s2 (20) | OK | location=0.5934155, scale=1.480403 | -48.3451014 | -48.3451014 | location=0.5934155, scale=1.480403 | -48.3451014 | 6 / 0 |  |
| logis | family200-s1 (200) | OK | location=-0.113402, scale=0.9969924 | -397.111057 | -397.111057 | location=-0.113402, scale=0.9969924 | -397.111057 | 6 / 0 |  |
| logis | family200-s2 (200) | OK | location=0.08852113, scale=0.896656 | -381.239487 | -381.239487 | location=0.08852113, scale=0.896656 | -381.239487 | 6 / 0 |  |
| logis | small3 (3) | OK | location=-0.3843994, scale=0.7015574 | -4.80188287 | -4.80188287 | location=-0.3843994, scale=0.7015574 | -4.80188287 | 6 / 0 |  |
| logis | ties (200) | OK | location=-0.2532962, scale=1.095632 | -417.871439 | -417.871439 | location=-0.2532962, scale=1.095632 | -417.871439 | 6 / 0 |  |
| logis | outlier (200) | OK | location=-0.1092069, scale=5.338691 | -805.074789 | -805.074789 | location=-0.1092069, scale=5.338691 | -805.074789 | 6 / 0 |  |
| logitnorm | family20-s1 (20) | OK | mu= 1.99465, sigma=2.741339 |  12.274157 |  12.274157 | mu= 1.99465, sigma=2.741339 |  12.274157 | 6 / 0 |  |
| logitnorm | family20-s2 (20) | OK | mu=1.642853, sigma=2.697968 | 9.70743655 | 9.70743655 | mu=1.642853, sigma=2.697968 | 9.70743655 | 6 / 0 |  |
| logitnorm | family200-s1 (200) | OK | mu=1.983818, sigma= 2.80764 | 133.953205 | 133.953205 | mu=1.983818, sigma= 2.80764 | 133.953205 | 6 / 0 |  |
| logitnorm | family200-s2 (200) | OK | mu=2.042055, sigma=2.932801 | 150.236779 | 150.236779 | mu=2.042055, sigma=2.932801 | 150.236779 | 6 / 0 |  |
| logitnorm | small3 (3) | OK | mu=1.955213, sigma= 2.81597 | 1.83224228 | 1.83224228 | mu=1.955213, sigma= 2.81597 | 1.83224228 | 6 / 0 |  |
| logitnorm | ties (200) | OK | mu=2.493591, sigma=3.025001 | 193.162542 | 193.162542 | mu=2.493591, sigma=3.025001 | 193.162542 | 6 / 0 |  |
| logitnorm | outlier (200) | OK | mu=2.443724, sigma=3.255337 | 188.316697 | 188.316697 | mu=2.443724, sigma=3.255337 | 188.316697 | 6 / 0 |  |
| lomax | family20-s1 (20) | OK | lambda=1.078605, kappa=2.114623 | -12.9670405 | -12.9670405 | lambda=1.078605, kappa=2.114623 | -12.9670405 | 6 / 0 |  |
| lomax | family20-s2 (20) | OK | lambda=1.075043, kappa=2.424715 | -9.08690952 | -9.08690952 | lambda=1.075043, kappa=2.424715 | -9.08690952 | 6 / 0 |  |
| lomax | family200-s1 (200) | OK | lambda=1.113657, kappa=1.822329 | -168.196761 | -168.196761 | lambda=1.113657, kappa=1.822329 | -168.196761 | 6 / 0 |  |
| lomax | family200-s2 (200) | OK | lambda=1.510644, kappa=1.396623 | -193.883817 | -193.883817 | lambda=1.510644, kappa=1.396623 | -193.883817 | 6 / 0 |  |
| lomax | small3 (3) | OK | lambda=1.374353, kappa=3.092074 | 0.370253359 | 0.370253359 | lambda=1.374353, kappa=3.092074 | 0.370253359 | 6 / 0 |  |
| lomax | ties (200) | OK | lambda=0.8472221, kappa=2.330371 | -149.776262 | -149.776262 | lambda=0.8472221, kappa=2.330371 | -149.776262 | 6 / 0 |  |
| lomax | outlier (200) | OK | lambda=1.660873, kappa=1.431573 | -166.483043 | -166.483043 | lambda=1.660873, kappa=1.431573 | -166.483043 | 6 / 0 |  |
| lomax | limit200 (200) | OK | lambda=0.002330331, kappa=400.5425 | -214.284352 | -214.284352 | lambda=0.002330331, kappa=400.5425 | -214.284352 | 6 / 0 |  |
| naka | family20-s1 (20) | OK | shape=1.822375, scale=2.915857 | -18.0243265 | -18.0243265 | shape=1.822375, scale=2.915857 | -18.0243265 | 6 / 0 |  |
| naka | family20-s2 (20) | OK | shape=1.331939, scale=2.933763 | -20.6345235 | -20.6345235 | shape=1.331939, scale=2.933763 | -20.6345235 | 6 / 0 |  |
| naka | family200-s1 (200) | OK | shape=2.244239, scale=3.020501 | -165.630769 | -165.630769 | shape=2.244239, scale=3.020501 | -165.630769 | 6 / 0 |  |
| naka | family200-s2 (200) | OK | shape=1.991229, scale=3.174861 | -181.128976 | -181.128976 | shape=1.991229, scale=3.174861 | -181.128976 | 6 / 0 |  |
| naka | small3 (3) | OK | shape=6.755026, scale=2.009046 | -0.318583676 | -0.318583676 | shape=6.755026, scale=2.009046 | -0.318583676 | 6 / 0 |  |
| naka | ties (200) | OK | shape=1.995125, scale=   3.158 | -180.426618 | -180.426618 | shape=1.995125, scale=   3.158 | -180.426618 | 6 / 0 |  |
| naka | outlier (200) | ESCAPE | shape=0.08146801, scale=68047.61 | -669.172463 | -669.172463 | shape=0.08146801, scale=68047.61 | -669.172463 | 6 / 0 |  |
| naka | limit200 (200) | OK | shape=0.3197403, scale=0.2950861 | -5.03003997 | -5.03003997 | shape=0.3197403, scale=0.2950861 | -5.03003997 | 6 / 0 |  |
| nbinom | family20-s1 (20) | OK | size=10.47331, prob=0.2919528 | -72.5872331 | -72.5872331 | size=10.47331, prob=0.2919528 | -72.5872331 | 6 / 0 |  |
| nbinom | family20-s2 (20) | OK | size= 10.9921, prob=0.3267365 | -69.850212 | -69.850212 | size= 10.9921, prob=0.3267365 | -69.850212 | 6 / 0 |  |
| nbinom | family200-s1 (200) | OK | size=10.03883, prob=0.2921337 | -719.281217 | -719.281217 | size=10.03883, prob=0.2921337 | -719.281217 | 6 / 0 |  |
| nbinom | family200-s2 (200) | OK | size=9.867789, prob=0.3081008 | -704.60554 | -704.60554 | size=9.867789, prob=0.3081008 | -704.60554 | 6 / 0 |  |
| nbinom | small3 (3) | ERROR | — |         NA |         NA | size=4.233559e+10, prob=       1 | -8.7017617 | 6 / 0 | The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead. |
| nbinom | ties (200) | OK | size=9.804089, prob=0.2946464 | -714.552001 | -714.552001 | size=9.804089, prob=0.2946464 | -714.552001 | 6 / 0 |  |
| nbinom | outlier (200) | OK | size=0.281628, prob=0.001090961 | -1118.08931 | -1118.08931 | size=0.281628, prob=0.001090961 | -1118.08931 | 6 / 0 |  |
| nbinom | bound1 (200) | OK | size=8.397687, prob=0.2686169 | -722.765313 | -722.765313 | size=8.397687, prob=0.2686169 | -722.765313 | 6 / 0 |  |
| nbinom | limit200 (200) | ERROR | — |         NA |         NA | size=8.301925e+08, prob=       1 | -400.226755 | 6 / 0 | The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead. |
| norm | family20-s1 (20) | OK | mean=-0.0926504, sd=0.8562613 | -25.2751769 | -25.2751769 | mean=-0.0926504, sd=0.8562613 | -25.2751769 | 6 / 0 |  |
| norm | family20-s2 (20) | OK | mean=-0.0668406, sd=0.955562 | -27.4696585 | -27.4696585 | mean=-0.0668406, sd=0.955562 | -27.4696585 | 6 / 0 |  |
| norm | family200-s1 (200) | OK | mean=-0.1394324, sd=1.053294 | -294.172174 | -294.172174 | mean=-0.1394324, sd=1.053294 | -294.172174 | 6 / 0 |  |
| norm | family200-s2 (200) | OK | mean=0.005933424, sd=0.9975271 | -283.292522 | -283.292522 | mean=0.005933424, sd=0.9975271 | -283.292522 | 6 / 0 |  |
| norm | small3 (3) | OK | mean=-0.1255367, sd=0.2592355 | -0.206761007 | -0.206761007 | mean=-0.1255367, sd=0.2592355 | -0.206761007 | 6 / 0 |  |
| norm | ties (200) | OK | mean=   0.018, sd=0.9682334 | -277.331294 | -277.331294 | mean=   0.018, sd=0.9682334 | -277.331294 | 6 / 0 |  |
| norm | outlier (200) | OK | mean=4.913408, sd=70.72734 | -1135.55415 | -1135.55415 | mean=4.913408, sd=70.72734 | -1135.55415 | 6 / 0 |  |
| paralogis | family20-s1 (20) | OK | shape=0.9744179, scale=1.181559 | -43.3226978 | -43.3226978 | shape=0.9744179, scale=1.181559 | -43.3226978 | 6 / 0 |  |
| paralogis | family20-s2 (20) | OK | shape=1.278283, scale=1.325124 | -34.7772848 | -34.7772848 | shape=1.278283, scale=1.325124 | -34.7772848 | 6 / 0 |  |
| paralogis | family200-s1 (200) | OK | shape=1.025228, scale= 1.04598 | -397.766758 | -397.766758 | shape=1.025228, scale= 1.04598 | -397.766758 | 6 / 0 |  |
| paralogis | family200-s2 (200) | OK | shape=0.9969582, scale=1.229942 | -447.53149 | -447.53149 | shape=0.9969581, scale=1.229942 | -447.53149 | 6 / 0 |  |
| paralogis | small3 (3) | OK | shape=2.338637, scale=0.6721075 | 0.0403837779 | 0.0403837779 | shape=2.338607, scale=0.6720969 | 0.0403837791 | 6 / 0 |  |
| paralogis | ties (200) | OK | shape=1.049318, scale=0.9448726 | -363.448137 | -363.448137 | shape=1.049318, scale=0.9448726 | -363.448137 | 6 / 0 |  |
| paralogis | outlier (200) | OK | shape= 1.01465, scale=0.8621869 | -371.099949 | -371.099949 | shape= 1.01465, scale=0.8621869 | -371.099949 | 6 / 0 |  |
| pareto | family20-s1 (20) | OK | a=1.064272, b=2.209774 | -53.4041874 | -53.4041874 | a=1.064272, b=2.209774 | -53.4041874 | 6 / 0 |  |
| pareto | family20-s2 (20) | OK | a=0.8282182, b=2.091606 | -62.6764368 | -62.6764368 | a=0.8282182, b=2.091606 | -62.6764368 | 6 / 0 |  |
| pareto | family200-s1 (200) | OK | a= 1.18091, b=2.000928 | -474.82596 | -474.82596 | a= 1.18091, b=2.000928 | -474.82596 | 6 / 0 |  |
| pareto | family200-s2 (200) | OK | a=1.000863, b=2.004804 | -538.76434 | -538.76434 | a=1.000863, b=2.004804 | -538.76434 | 6 / 0 |  |
| pareto | small3 (3) | OK | a=3.401905, b=2.869873 | -3.37165572 | -3.37165572 | a=3.402085, b=2.869873 | -3.37165574 | 6 / 0 |  |
| pareto | ties (200) | OK | a=1.103924, b=       2 | -500.027249 | -500.027249 | a=1.103924, b=       2 | -500.027249 | 6 / 0 |  |
| pareto | outlier (200) | OK | a=1.016474, b=2.008737 | -532.99168 | -532.99168 | a=1.016474, b=2.008737 | -532.99168 | 6 / 0 |  |
| pareto | bound1 (200) | ERROR | a=     NaN, b=     NaN |        NaN |       -Inf | — |       -Inf | 6 / NA | Nonfinite fitter return (no exception): a=     NaN, b=     NaN logLik NaN;  NO FINITE ORACLE |
| pois | family20-s1 (20) | OK | lambda=    1.85 | -34.4458886 | -34.4458886 | lambda=    1.85 | -34.4458886 | 6 / 0 |  |
| pois | family20-s2 (20) | OK | lambda=    1.95 | -34.4433456 | -34.4433456 | lambda=    1.95 | -34.4433456 | 6 / 0 |  |
| pois | family200-s1 (200) | OK | lambda=    2.02 | -341.604158 | -341.604158 | lambda=    2.02 | -341.604158 | 6 / 0 |  |
| pois | family200-s2 (200) | OK | lambda=   1.985 | -345.697832 | -345.697832 | lambda=   1.985 | -345.697832 | 6 / 0 |  |
| pois | small3 (3) | OK | lambda=1.333333 | -3.54241889 | -3.54241889 | lambda=1.333333 | -3.54241889 | 6 / 0 |  |
| pois | ties (200) | OK | lambda=    2.17 | -352.339292 | -352.339292 | lambda=    2.17 | -352.339292 | 6 / 0 |  |
| pois | outlier (200) | OK | lambda=  32.005 | -30638.3125 | -30638.3125 | lambda=  32.005 | -30638.3125 | 6 / 0 |  |
| pois | bound1 (200) | OK | lambda=   2.005 | -342.177587 | -342.177587 | lambda=   2.005 | -342.177587 | 6 / 0 |  |
| power | family20-s1 (20) | OK | alpha=0.9663029, beta=2.233992 | 5.71394646 | 5.71394646 | alpha=0.9663028, beta=2.233988 | 5.71394715 | 6 / 0 |  |
| power | family20-s2 (20) | OK | alpha=0.9956075, beta=2.528961 | 6.55260187 | 6.55260187 | alpha=0.9956075, beta=2.528956 | 6.55260262 | 6 / 0 |  |
| power | family200-s1 (200) | OK | alpha=0.9946478, beta=2.067761 | 43.0895474 | 43.0895474 | alpha=0.9946478, beta=2.067756 | 43.0895536 | 6 / 0 |  |
| power | family200-s2 (200) | OK | alpha=0.9987769, beta=1.851568 | 31.4678742 | 31.4678742 | alpha=0.9987769, beta=1.851564 | 31.4678797 | 6 / 0 |  |
| power | small3 (3) | OK | alpha=0.7341822, beta=1.742084 | 1.31431581 | 1.31431581 | alpha=0.7341822, beta=1.742085 | 1.31431591 | 6 / 0 |  |
| power | ties (200) | OK | alpha=       1, beta=2.408527 | 58.8413661 | 58.8413661 | alpha=       1, beta=2.408522 | 58.8413732 | 6 / 0 |  |
| power | outlier (200) | OK | alpha=997.3723, beta=0.1367207 | -516.151941 | -516.151941 | alpha=997.3723, beta=0.1367207 | -516.151941 | 6 / 0 |  |
| power | bound1 (200) | ERROR | — |         NA |         NA | — |       -Inf | 6 / NA | The power distribution is only defined for positive numbers.;  NO FINITE ORACLE |
| rayleigh | family20-s1 (20) | OK | sigma=1.686187 | -27.8987714 | -27.8987714 | sigma=1.686187 | -27.8987714 | 6 / 0 |  |
| rayleigh | family20-s2 (20) | OK | sigma=1.776577 | -26.9186392 | -26.9186392 | sigma=1.776577 | -26.9186392 | 6 / 0 |  |
| rayleigh | family200-s1 (200) | OK | sigma=2.038904 | -341.818798 | -341.818798 | sigma=2.038904 | -341.818798 | 6 / 0 |  |
| rayleigh | family200-s2 (200) | OK | sigma=1.978182 | -327.462766 | -327.462766 | sigma=1.978182 | -327.462766 | 6 / 0 |  |
| rayleigh | small3 (3) | OK | sigma=1.999929 | -4.30912773 | -4.30912773 | sigma=1.999929 | -4.30912773 | 6 / 0 |  |
| rayleigh | ties (200) | OK | sigma=2.045519 | -317.237254 | -317.237254 | sigma=2.045519 | -317.237254 | 6 / 0 |  |
| rayleigh | outlier (200) | OK | sigma=338.7383 | -2365.04413 | -2365.04413 | sigma=338.7383 | -2365.04413 | 6 / 0 |  |
| rayleigh | bound1 (200) | ERROR | sigma=1.999314 |       -Inf |       -Inf | — |       -Inf | 6 / NA | Nonfinite fitter return (no exception): sigma=1.999314 logLik -Inf;  NO FINITE ORACLE |
| sged | family20-s1 (20) | NOT_MAX | mean=-0.2380714, sd=0.8846378, nu=1.539748, xi=2.259652 | -23.2774275 | -23.2774275 | mean=-0.2501732, sd=0.8874484, nu=2.567344, xi=526.1075 | -23.0506946 | 6 / 0 |  |
| sged | family20-s2 (20) | NOT_MAX | mean=-0.2296502, sd=0.6536058, nu=2.371098, xi=1.106541 | -19.7842785 | -19.7842785 | mean=-0.2939349, sd=0.6957401, nu=5.235076, xi=3223.306 | -19.0208287 | 6 / 0 |  |
| sged | family200-s1 (200) | OK | mean=0.01853131, sd=0.9881931, nu=4.001124, xi=2.747493 | -265.480524 | -265.480524 | mean=0.01853131, sd=0.9881931, nu=4.001124, xi=2.747493 | -265.480524 | 6 / 0 |  |
| sged | family200-s2 (200) | OK | mean=-0.05184844, sd=1.084146, nu=2.768689, xi=748.3269 | -272.409096 | -272.409096 | mean=-0.05184779, sd=1.084145, nu=2.768672, xi=15550.12 | -272.408742 | 6 / 0 |  |
| sged | small3 (3) | ESCAPE | mean=0.3769464, sd=0.9277716, nu=6.55911e+08, xi=266.4386 | -3.50245072 | -3.50245072 | mean=0.3769463, sd=0.9277715, nu=2.540253e+10, xi=2582.179 | -3.50245056 | 6 / 0 |  |
| sged | ties (200) | OK | mean=0.1451468, sd=1.085008, nu=3.467884, xi=507.8555 | -276.38915 | -276.38915 | mean=0.1449577, sd=1.084445, nu=3.475355, xi=  8234.3 | -276.388324 | 6 / 0 |  |
| sged | outlier (200) | ESCAPE,NOT_MAX | mean=1.240765, sd=3.508152, nu=0.6570666, xi=59368.99 | -409.580979 | -409.580979 | mean=1.135239, sd=4.499919, nu=0.4382774, xi=5315.949 | -378.038947 | 6 / 0 |  |
| sged | limit200 (200) | NOT_MAX | mean=0.0007024652, sd=0.5750653, nu=6095.679, xi=191.1838 | -137.90885 | -137.90885 | mean=0.0003203872, sd=0.5748446, nu=1.34967e+10, xi=1.016032 | -137.759554 | 6 / 0 |  |
| snorm | family20-s1 (20) | OK | mean=-0.1323077, sd=0.7741673, xi=241.8062 | -19.5509278 | -19.5509278 | mean=-0.1307018, sd=0.7753625, xi=3584.992 | -19.5505388 | 6 / 0 |  |
| snorm | family20-s2 (20) | OK | mean=-0.04765924, sd=1.126405, xi=253.0115 | -26.8900013 | -26.8900013 | mean=-0.05733564, sd=1.119063, xi= 1194.73 | -26.8887653 | 6 / 0 |  |
| snorm | family200-s1 (200) | OK | mean=0.06259152, sd=1.007961, xi=2.689763 | -262.264121 | -262.264121 | mean=0.06259152, sd=1.007961, xi=2.689763 | -262.264121 | 6 / 0 |  |
| snorm | family200-s2 (200) | OK | mean=0.02163935, sd=0.9785501, xi= 2.55274 | -257.595126 | -257.595126 | mean=0.02163935, sd=0.9785501, xi= 2.55274 | -257.595126 | 6 / 0 |  |
| snorm | small3 (3) | OK | mean=-0.5160997, sd=0.2970817, xi=177.6971 | -0.0484103155 | -0.0484103155 | mean=-0.5160932, sd=0.2970736, xi=13599.62 | -0.0483151375 | 6 / 0 |  |
| snorm | ties (200) | OK | mean=0.05247867, sd=1.097391, xi=233.0243 | -265.215928 | -265.215928 | mean=0.05386491, sd=1.098411, xi=1421.199 | -265.212073 | 6 / 0 |  |
| snorm | outlier (200) | ESCAPE,NOT_MAX | mean=52.30821, sd=40.60604, xi=1.703865e+86 | -998.769438 | -998.769438 | mean=53.34504, sd=41.40286, xi=6.124804e+90 | -998.406809 | 6 / 0 |  |
| snorm | limit200 (200) | ESCAPE,NOT_MAX | mean=1.049986, sd=0.7926065, xi=6.222067e+11 | -218.361079 | -218.361079 | mean=1.142714, sd=0.8626613, xi=5.466866e+13 | -216.841585 | 6 / 0 |  |
| sstd | family20-s1 (20) | NOT_MAX | mean=0.09172896, sd=0.5390653, nu=343.2487, xi=2.123981 | -14.3179942 | -14.3179942 | mean=0.06048441, sd=0.5869774, nu=623774.8, xi=365.3832 | -13.9636361 | 6 / 1 |  |
| sstd | family20-s2 (20) | OK | mean=-0.0001319833, sd=0.6648425, nu=2.827916, xi=1.572654 | -13.0830189 | -13.0830189 | mean=-0.0001319833, sd=0.6648425, nu=2.827916, xi=1.572654 | -13.0830189 | 6 / 0 |  |
| sstd | family200-s1 (200) | OK | mean=0.06646691, sd=2.518834, nu=2.113867, xi=2.065391 | -209.152278 | -209.152278 | mean=0.06646691, sd=2.518834, nu=2.113867, xi=2.065391 | -209.152278 | 6 / 0 |  |
| sstd | family200-s2 (200) | OK | mean=-0.08596535, sd=0.8269576, nu=3.523485, xi=1.975314 | -188.448302 | -188.448302 | mean=-0.08596536, sd=0.8269576, nu=3.523485, xi=1.975314 | -188.448302 | 6 / 0 |  |
| sstd | small3 (3) | NOT_MAX | mean=-0.4464706, sd=0.2969781, nu=5.170442, xi=0.009554475 | 0.415771304 | 0.415771304 | mean=-0.3988152, sd=1.632199, nu=2.029325, xi=0.0629456 | 0.536669966 | 6 / 0 |  |
| sstd | ties (200) | OK | mean=-0.04477695, sd=0.8584631, nu=  3.5857, xi=2.932715 | -177.636831 | -177.636831 | mean=-0.04477695, sd=0.8584631, nu=  3.5857, xi=2.932715 | -177.636831 | 6 / 0 |  |
| sstd | outlier (200) | OK | mean=0.1141497, sd=3.126675, nu=2.087061, xi=2.208116 | -220.165875 | -220.165875 | mean=0.1141496, sd=3.126675, nu=2.087061, xi=2.208116 | -220.165875 | 6 / 0 |  |
| sstd | limit200 (200) | OK | mean=0.0504222, sd=0.9328396, nu=15.16593, xi=0.9754147 | -269.102329 | -269.102329 | mean=0.0504222, sd=0.9328396, nu=15.16593, xi=0.9754147 | -269.102329 | 6 / 0 |  |
| std | family20-s1 (20) | OK | mean=-0.1097052, sd=145.5067, nu=2.000019 | -24.4142164 | -24.4142164 | mean=-0.1097068, sd=21058.78, nu=       2 | -24.4142074 | 6 / 0 |  |
| std | family20-s2 (20) | OK | mean=-0.02482514, sd=0.6797795, nu=5.976988 | -20.0545169 | -20.0545169 | mean=-0.02482173, sd=0.6797764, nu=5.977509 | -20.0545169 | 6 / 0 |  |
| std | family200-s1 (200) | OK | mean=0.06410829, sd=1.008661, nu=3.442831 | -259.977801 | -259.977801 | mean=0.06410825, sd=1.008661, nu=3.442831 | -259.977801 | 6 / 0 |  |
| std | family200-s2 (200) | OK | mean=-0.02871718, sd=0.9100634, nu=3.725047 | -244.557675 | -244.557675 | mean=-0.02871718, sd=0.9100634, nu=3.725047 | -244.557675 | 6 / 0 |  |
| std | small3 (3) | ESCAPE | mean=-0.2448382, sd=0.2866713, nu=4.604662e+13 | -0.166072064 | -0.514779469 | mean=-0.2446771, sd=0.2872652, nu=2.369365e+14 | -0.514766133 | 6 / 0 |  |
| std | ties (200) | OK | mean=-0.06697421, sd=0.9062449, nu=2.997707 | -225.048552 | -225.048552 | mean=-0.0669741, sd=0.9062449, nu=2.997707 | -225.048552 | 6 / 0 |  |
| std | outlier (200) | OK | mean=-0.08715448, sd=616.6761, nu=2.000001 | -249.021835 | -249.021835 | mean=-0.08715526, sd=11367.56, nu=       2 | -249.021834 | 6 / 0 |  |
| std | limit200 (200) | OK | mean=-0.04363867, sd=0.9961013, nu=8.885978 | -280.597889 | -280.597889 | mean=-0.04363906, sd=0.9961012, nu=8.885977 | -280.597889 | 6 / 0 |  |
| unif | family20-s1 (20) | OK | min=0.02744261, max=0.9678757 | 1.22829653 | 1.22829653 | min=0.02744261, max=0.9678757 | 1.22829653 | 6 / 0 |  |
| unif | family20-s2 (20) | OK | min=0.04295023, max=0.9055574 | 2.95591707 | 2.95591707 | min=0.04295023, max=0.9055574 | 2.95591707 | 6 / 0 |  |
| unif | family200-s1 (200) | OK | min=7.970515e-05, max=0.9847086 | 3.09809079 | 3.09809079 | min=7.970515e-05, max=0.9847086 | 3.09809079 | 6 / 0 |  |
| unif | family200-s2 (200) | OK | min=0.003467075, max=0.9997985 | 0.735056824 | 0.735056824 | min=0.003467075, max=0.9997985 | 0.735056824 | 6 / 0 |  |
| unif | small3 (3) | OK | min=0.2995538, max=0.586268 | 3.74780898 | 3.74780898 | min=0.2995538, max=0.586268 | 3.74780898 | 6 / 0 |  |
| unif | ties (200) | OK | min=       0, max=       1 |         -0 |          0 | min=       0, max=       1 |          0 | 6 / 0 |  |
| unif | outlier (200) | OK | min=0.00871976, max=1000.999 | -1381.74893 | -1381.74893 | min=0.00871976, max=1000.999 | -1381.74893 | 6 / 0 |  |
| unif | bounds (200) | OK | min=       0, max=       1 |         -0 |          0 | min=       0, max=       1 |          0 | 6 / 0 |  |
| weibull | family20-s1 (20) | OK | shape=3.519394, scale=3.562506 | -28.4829397 | -28.4829397 | shape=3.519394, scale=3.562506 | -28.4829397 | 6 / 0 |  |
| weibull | family20-s2 (20) | OK | shape=4.980825, scale=3.683005 | -23.5539988 | -23.5539988 | shape=4.980825, scale=3.683005 | -23.5539988 | 6 / 0 |  |
| weibull | family200-s1 (200) | OK | shape=3.142644, scale=3.950406 | -326.009721 | -326.009721 | shape=3.142644, scale=3.950406 | -326.009721 | 6 / 0 |  |
| weibull | family200-s2 (200) | OK | shape=3.319447, scale=4.077617 | -324.29222 | -324.29222 | shape=3.319447, scale=4.077617 | -324.29222 | 6 / 0 |  |
| weibull | small3 (3) | OK | shape=11.89005, scale= 4.88244 | -2.09035469 | -2.09035469 | shape=11.89005, scale= 4.88244 | -2.09035469 | 6 / 0 |  |
| weibull | ties (200) | OK | shape=2.973538, scale=3.855261 | -327.483504 | -327.483504 | shape=2.973538, scale=3.855261 | -327.483504 | 6 / 0 |  |
| weibull | outlier (200) | OK | shape=0.5408429, scale= 5.42949 | -615.449194 | -615.449194 | shape=0.5408429, scale= 5.42949 | -615.449194 | 6 / 0 |  |
| zip | family20-s1 (20) | OK | lambda=0.4642128, pi=0.4614538 | -12.4328848 | -12.4328848 | lambda=0.4642128, pi=0.4614538 | -12.4328848 | 6 / 0 |  |
| zip | family20-s2 (20) | OK | lambda=1.423262, pi=0.4730416 | -23.0097617 | -23.0097617 | lambda=1.423262, pi=0.4730416 | -23.0097617 | 6 / 0 |  |
| zip | family200-s1 (200) | OK | lambda=0.8463127, pi=0.5096375 | -169.484168 | -169.484168 | lambda=0.8463127, pi=0.5096375 | -169.484168 | 6 / 0 |  |
| zip | family200-s2 (200) | OK | lambda=0.8294785, pi=0.379128 | -195.00088 | -195.00088 | lambda=0.8294785, pi=0.379128 | -195.00088 | 6 / 0 |  |
| zip | small3 (3) | OK | lambda= 3.92069, pi=0.6599238 | -3.52318909 | -3.52318909 | lambda= 3.92069, pi=0.6599238 | -3.52318909 | 6 / 0 |  |
| zip | ties (200) | OK | lambda=0.5435858, pi=0.3561274 | -153.646166 | -153.646166 | lambda=0.5435858, pi=0.3561274 | -153.646166 | 6 / 0 |  |
| zip | outlier (200) | OK | lambda=57.05556, pi=    0.73 | -11783.4696 |       -Inf | — |       -Inf | 6 / NA |  NO FINITE ORACLE |
| zip | bound1 (200) | OK | lambda=0.9180645, pi=0.5588545 | -165.650597 | -165.650597 | lambda=0.9180645, pi=0.5588545 | -165.650597 | 6 / 0 |  |
| zip | limit200 (188) | OK | lambda=3.191489, pi=       0 | -357.64314 | -357.64314 | lambda= 3.19149, pi=1.232604e-17 | -357.64314 | 6 / 0 |  |
| zipf | family20-s1 (20) | OK | N=       3, s=0.8083349 | -20.6011228 | -20.6011228 | N=       3, s=0.8083349 | -20.6011228 | 6 / 0 | integer size held at fitter value |
| zipf | family20-s2 (20) | OK | N=       3, s=0.669861 | -21.0262675 | -21.0262675 | N=       3, s=0.669861 | -21.0262675 | 6 / 0 | integer size held at fitter value |
| zipf | family200-s1 (200) | OK | N=       3, s=0.3158368 | -217.626281 | -217.626281 | N=       3, s=0.3158368 | -217.626281 | 6 / 0 | integer size held at fitter value |
| zipf | family200-s2 (200) | OK | N=       3, s=0.3065686 | -217.748245 | -217.748245 | N=       3, s=0.3065686 | -217.748245 | 6 / 0 | integer size held at fitter value |
| zipf | small3 (3) | OK | N=       2, s=       1 | -1.9095425 | -1.9095425 | N=       2, s=       1 | -1.9095425 | 6 / 0 | integer size held at fitter value |
| zipf | ties (200) | OK | N=       3, s=0.8446088 | -204.779373 | -204.779373 | N=       3, s=0.8446088 | -204.779373 | 6 / 0 | integer size held at fitter value |
| zipf | outlier (200) | OK | N=    3000, s=2.071018 | -303.418788 | -303.418788 | — |       -Inf | 6 / NA | integer size held at fitter value NO FINITE ORACLE |
| zipf | bound1 (200) | OK | N=       3, s=0.601602 | -212.084186 | -212.084186 | N=       3, s=0.601602 | -212.084186 | 6 / 0 | integer size held at fitter value |
| zipf | bounds (200) | OK | N=       3, s=0.5919707 | -212.326161 | -212.326161 | N=       3, s=0.5919707 | -212.326161 | 6 / 0 | integer size held at fitter value |
