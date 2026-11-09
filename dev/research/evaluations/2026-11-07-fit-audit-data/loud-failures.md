# Loud failures: finite-MLE evidence

These are non-Ok native outcomes with an explicit finite-attainment argument. A loud numerical failure is permitted by the library contract; it is not equivalent to a silent wrong result. Some real-parameter MLEs cannot be represented as double (notably a Nakagami scale below the double range).

| Case | Family / held subset / sample | Native status | Finite-MLE evidence |
|---|---|---|---|
| 258 | betapr / `scale` / scale-1e+150 | 1 | held-scale beta-prime is a bijective beta transformation; its interior shape MLE is finite |
| 273 | betapr / `shape1,scale` / lower-1e-12 | 1 | held-scale beta-prime is a bijective beta transformation; its interior shape MLE is finite |
| 274 | betapr / `shape1,scale` / lower-1e-17 | 1 | held-scale beta-prime is a bijective beta transformation; its interior shape MLE is finite |
| 275 | betapr / `shape1,scale` / lower-1e-200 | 1 | held-scale beta-prime is a bijective beta transformation; its interior shape MLE is finite |
| 276 | betapr / `shape1,scale` / scale-1e-150 | 1 | held-scale beta-prime is a bijective beta transformation; its interior shape MLE is finite |
| 296 | betapr / `shape2,scale` / scale-1e+150 | 1 | held-scale beta-prime is a bijective beta transformation; its interior shape MLE is finite |
| 795 | cauchy / `scale` / tiny-2 | 4 | positive scale held: continuous coercive location likelihood attains a maximum |
| 806 | cauchy / `scale` / scale-1e+150 | 3 | positive scale held: continuous coercive location likelihood attains a maximum |
| 807 | cauchy / `scale` / offset-1e8 | 3 | positive scale held: continuous coercive location likelihood attains a maximum |
| 941 | cmp / `free` / large-count | 3 | CMP dossier convex-support criterion: empirical sufficient statistic is not on an adjacent-count face, or all-zero rate is included |
| 955 | cmp / `lambda` / large-count | 3 | CMP fixed-positive-rate dispersion mean map: positive mean log-factorial gives a finite root |
| 1438 | fatigue / `mu` / constant-0.904599-1e-08 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1439 | fatigue / `mu` / constant-0.904599-1e-12 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1441 | fatigue / `mu` / constant-1-1e-08 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1442 | fatigue / `mu` / constant-1-1e-12 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1449 | fatigue / `mu` / offset-1e8 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1457 | fatigue / `alpha,mu` / constant-0.904599-0.0001 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1458 | fatigue / `alpha,mu` / constant-0.904599-1e-08 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1459 | fatigue / `alpha,mu` / constant-0.904599-1e-12 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1460 | fatigue / `alpha,mu` / constant-1-0.0001 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1461 | fatigue / `alpha,mu` / constant-1-1e-08 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1462 | fatigue / `alpha,mu` / constant-1-1e-12 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1469 | fatigue / `alpha,mu` / offset-1e8 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 1471 | fatigue / `alpha,mu` / limit-concentration | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 2426 | ged / `nu` / constant--0.0213396-1e-08 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2427 | ged / `nu` / constant--0.0213396-1e-12 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2429 | ged / `nu` / constant-1-1e-08 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2430 | ged / `nu` / constant-1-1e-12 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2433 | ged / `nu` / offset-1e8 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2458 | ged / `sd,nu` / constant--0.0213396-1e-08 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2459 | ged / `sd,nu` / constant--0.0213396-1e-12 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2461 | ged / `sd,nu` / constant-1-1e-08 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2462 | ged / `sd,nu` / constant-1-1e-12 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2465 | ged / `sd,nu` / offset-1e8 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 2865 | gompertz / `a,b` / scale-1e+150 | 2 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 2866 | gompertz / `a,b` / limit-outlier | 2 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 2904 | gpd / `mu,sigma` / scale-1e+150 | 3 | strictly positive threshold excesses and xi >= -1: finite maximum exists (GPD dossier existence) |
| 3417 | huber / `mu,epsilon` / scale-1e-150 | 3 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 4293 | kumar / `free` / constant-0.472831-0.0001 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4294 | kumar / `free` / constant-0.472831-1e-08 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4295 | kumar / `free` / constant-0.472831-1e-12 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4296 | kumar / `free` / constant-0.25-0.0001 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4297 | kumar / `free` / constant-0.25-1e-08 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4298 | kumar / `free` / constant-0.25-1e-12 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4304 | kumar / `free` / lower-1e-200 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4305 | kumar / `free` / scale-1e-150 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4306 | kumar / `free` / limit-concentration | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4323 | kumar / `a` / lower-1e-200 | 3 | Kumaraswamy dossier: the profiled interior likelihood has a finite maximum unless both shapes are free on constant data |
| 4718 | logis / `scale` / scale-1e+150 | 2 | location-scale dossier existence theorem; collapse condition excluded (median/strictly concave profile) |
| 4719 | logis / `scale` / offset-1e8 | 2 | location-scale dossier existence theorem; collapse condition excluded (median/strictly concave profile) |
| 4960 | lst / `df` / constant-0.404631-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4961 | lst / `df` / constant-0.404631-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4963 | lst / `df` / constant-1-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4964 | lst / `df` / constant-1-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4967 | lst / `df` / offset-1e8 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4968 | lst / `df` / limit-outlier | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4992 | lst / `sigma,df` / constant-0.404631-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4993 | lst / `sigma,df` / constant-0.404631-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4995 | lst / `sigma,df` / constant-1-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4996 | lst / `sigma,df` / constant-1-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4998 | lst / `sigma,df` / scale-1e+150 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 4999 | lst / `sigma,df` / offset-1e8 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 5000 | lst / `sigma,df` / limit-outlier | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 5032 | naka / `free` / lower-1e-200 | 4 | Nakagami profiled shape score has a positive Jensen/deviance statistic, or shape is held; its scale solution is analytic |
| 5051 | naka / `shape` / lower-1e-200 | 4 | Nakagami profiled shape score has a positive Jensen/deviance statistic, or shape is held; its scale solution is analytic |
| 5070 | naka / `scale` / lower-1e-200 | 4 | Nakagami profiled shape score has a positive Jensen/deviance statistic, or shape is held; its scale solution is analytic |
| 6165 | sged / `nu,xi` / constant-0.106436-1e-12 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6168 | sged / `nu,xi` / constant-1-1e-12 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6171 | sged / `nu,xi` / offset-1e8 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6185 | sged / `mean,nu,xi` / scale-1e-150 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6196 | sged / `sd,nu,xi` / constant-0.106436-1e-08 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6197 | sged / `sd,nu,xi` / constant-0.106436-1e-12 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6199 | sged / `sd,nu,xi` / constant-1-1e-08 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6200 | sged / `sd,nu,xi` / constant-1-1e-12 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6201 | sged / `sd,nu,xi` / scale-1e-150 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6202 | sged / `sd,nu,xi` / scale-1e+150 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6203 | sged / `sd,nu,xi` / offset-1e8 | 9 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6218 | sged / `mean,sd,nu,xi` / scale-1e+150 | 1 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6489 | sn / `alpha` / constant-0.640976-1e-12 | 2 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6526 | sn / `omega,alpha` / scale-1e+150 | 2 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6528 | sn / `omega,alpha` / limit-outlier | 2 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6543 | sn / `xi,omega,alpha` / offset-1e8 | 2 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6544 | sn / `xi,omega,alpha` / limit-outlier | 2 | held finite shape leaves exponential-power/Gaussian/Laplace tails: scale-zero likelihood vanishes away from constant data; all other parameter escapes are coercive |
| 6893 | sstd / `mean,nu,xi` / scale-1e-150 | 12 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 6910 | sstd / `sd,nu,xi` / scale-1e+150 | 12 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 6911 | sstd / `sd,nu,xi` / offset-1e8 | 12 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 6912 | sstd / `sd,nu,xi` / limit-outlier | 12 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7166 | st / `omega,alpha,nu` / scale-1e+150 | 4 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7168 | st / `omega,alpha,nu` / limit-outlier | 4 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7256 | std / `nu` / constant-0.374928-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7257 | std / `nu` / constant-0.374928-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7259 | std / `nu` / constant-1-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7260 | std / `nu` / constant-1-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7263 | std / `nu` / offset-1e8 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7264 | std / `nu` / limit-outlier | 8 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7288 | std / `sd,nu` / constant-0.374928-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7289 | std / `sd,nu` / constant-0.374928-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7291 | std / `sd,nu` / constant-1-1e-08 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7292 | std / `sd,nu` / constant-1-1e-12 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7294 | std / `sd,nu` / scale-1e+150 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7295 | std / `sd,nu` / offset-1e8 | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7296 | std / `sd,nu` / limit-outlier | 11 | fixed finite Student shape(s): polynomial-tail scale-collapse exponent nu*(n-k)-k is positive, or scale is held; likelihood is coercive on the remaining location/scale model |
| 7645 | tlambda / `mu,sigma,lambda` / scale-1e-150 | 1 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 7676 | tnorm / `mean` / constant-1-1e-12 | 10 | held-mean quadratic statistic minus its finite-window uniform-boundary value = -25488509312.12012569425566 (strict-concavity boundary score; 120+ digits) |
| 7688 | tnorm / `sd` / constant--0.327424-1e-08 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7689 | tnorm / `sd` / constant--0.327424-1e-12 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7691 | tnorm / `sd` / constant-1-1e-08 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7692 | tnorm / `sd` / constant-1-1e-12 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7694 | tnorm / `sd` / scale-1e+150 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7697 | tnorm / `sd` / limit-concentration | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7709 | tnorm / `mean,sd` / scale-1e-150 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7720 | tnorm / `a` / constant--0.327424-1e-08 | 10 | empirical variance minus one-sided exponential-boundary variance = -5.231670903329166697949315e-17 (strict-concavity boundary score; 120+ digits) |
| 7721 | tnorm / `a` / constant--0.327424-1e-12 | 10 | empirical variance minus one-sided exponential-boundary variance = -5.231715814273640253006223e-25 (strict-concavity boundary score; 120+ digits) |
| 7723 | tnorm / `a` / constant-1-1e-08 | 10 | empirical variance minus one-sided exponential-boundary variance = -4.880000025060833207885887e-16 (strict-concavity boundary score; 120+ digits) |
| 7724 | tnorm / `a` / constant-1-1e-12 | 10 | empirical variance minus one-sided exponential-boundary variance = -4.879997253984056132989377e-24 (strict-concavity boundary score; 120+ digits) |
| 7727 | tnorm / `a` / offset-1e8 | 10 | empirical variance minus one-sided exponential-boundary variance = -0.6276554487037945119344408 (strict-concavity boundary score; 120+ digits) |
| 7737 | tnorm / `mean,a` / constant--0.327424-1e-12 | 10 | held mean, at least one infinite bound: nonconstant-data likelihood vanishes at both scale escapes |
| 7753 | tnorm / `sd,a` / constant--0.327424-1e-12 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7756 | tnorm / `sd,a` / constant-1-1e-12 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7757 | tnorm / `sd,a` / scale-1e-150 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7758 | tnorm / `sd,a` / scale-1e+150 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7784 | tnorm / `b` / constant--0.327424-1e-08 | 10 | empirical variance minus one-sided exponential-boundary variance = -2.015479746344136187852671e-17 (strict-concavity boundary score; 120+ digits) |
| 7785 | tnorm / `b` / constant--0.327424-1e-12 | 10 | empirical variance minus one-sided exponential-boundary variance = -2.015681217069957827669531e-25 (strict-concavity boundary score; 120+ digits) |
| 7787 | tnorm / `b` / constant-1-1e-08 | 10 | empirical variance minus one-sided exponential-boundary variance = -1.879999986030493337142309e-16 (strict-concavity boundary score; 120+ digits) |
| 7788 | tnorm / `b` / constant-1-1e-12 | 10 | empirical variance minus one-sided exponential-boundary variance = -1.879952348617150183680571e-24 (strict-concavity boundary score; 120+ digits) |
| 7791 | tnorm / `b` / offset-1e8 | 10 | empirical variance minus one-sided exponential-boundary variance = -2.16822316649672677196175 (strict-concavity boundary score; 120+ digits) |
| 7804 | tnorm / `mean,b` / constant-1-1e-12 | 10 | held mean, at least one infinite bound: nonconstant-data likelihood vanishes at both scale escapes |
| 7805 | tnorm / `mean,b` / scale-1e-150 | 10 | held mean, at least one infinite bound: nonconstant-data likelihood vanishes at both scale escapes |
| 7817 | tnorm / `sd,b` / constant--0.327424-1e-12 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7820 | tnorm / `sd,b` / constant-1-1e-12 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7821 | tnorm / `sd,b` / scale-1e-150 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 7822 | tnorm / `sd,b` / scale-1e+150 | 10 | held positive sd: conditional-normal mean map/coercivity gives finite attainment; free endpoints are observed extrema |
| 8155 | trgamma / `shape1,shape2,scale` / scale-1e+150 | 6 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 8270 | triang / `a,c` / lower-1e-17 | 2 | positive triangular candidate plus nonzero data range: compactness and upper-semicontinuity prove attainment (dossier) |
| 8687 | zinb / `size,prob,pi` / large-count | 3 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 8819 | zmbinom / `size` / zero-but-one | 5 | fixed finite support: monotone (possibly positive-truncated) binomial mean equation, with included proper endpoint members |
| 8903 | zmgeom / `free` / constant-1-0.0001 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8904 | zmgeom / `free` / constant-1-1e-08 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8905 | zmgeom / `free` / constant-1-1e-12 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8906 | zmgeom / `free` / constant-1-0.0001 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8907 | zmgeom / `free` / constant-1-1e-08 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8908 | zmgeom / `free` / constant-1-1e-12 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8910 | zmgeom / `free` / zero-but-one | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8959 | zmlogarithmic / `free` / constant-1-0.0001 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8960 | zmlogarithmic / `free` / constant-1-1e-08 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8961 | zmlogarithmic / `free` / constant-1-1e-12 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8962 | zmlogarithmic / `free` / constant-1-0.0001 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8963 | zmlogarithmic / `free` / constant-1-1e-08 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8964 | zmlogarithmic / `free` / constant-1-1e-12 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 8966 | zmlogarithmic / `free` / zero-but-one | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9119 | zmnbinom / `size,prob,p0` / large-count | 6 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 9127 | zmpois / `free` / constant-0-0.0001 | 4 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9128 | zmpois / `free` / constant-0-1e-08 | 4 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9129 | zmpois / `free` / constant-0-1e-12 | 4 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9130 | zmpois / `free` / constant-1-0.0001 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9131 | zmpois / `free` / constant-1-1e-08 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9132 | zmpois / `free` / constant-1-1e-12 | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9134 | zmpois / `free` / zero-but-one | 5 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9155 | zmpois / `p0` / constant-0-0.0001 | 4 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9156 | zmpois / `p0` / constant-0-1e-08 | 4 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9157 | zmpois / `p0` / constant-0-1e-12 | 4 | zero mass is a binomial proportion; positive-count mean map has a finite root or an included one-point endpoint |
| 9169 | zmpois / `lambda,p0` / constant-0-0.0001 | 4 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 9170 | zmpois / `lambda,p0` / constant-0-1e-08 | 4 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 9171 | zmpois / `lambda,p0` / constant-0-1e-12 | 4 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 9290 | ztnbinom / `size,prob` / large-count | 4 | all coordinates held: the sole admissible parameter point has a finite independently evaluated log likelihood |
| 9357 | fatigue / `alpha,mu` / face-point-mass-100 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 9817 | fatigue / `alpha,mu` / face-point-mass-1e+06 | 3 | fixed-threshold Birnbaum–Saunders profile: finite positive scale, and nonzero shape residual (family dossier existence) |
| 11127 | triang / `a,c` / support-lower-ulp | 2 | positive triangular candidate plus nonzero data range: compactness and upper-semicontinuity prove attainment (dossier) |
