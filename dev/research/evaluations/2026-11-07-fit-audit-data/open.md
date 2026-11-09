# Open numerical and existence entries

Every entry below is unresolved evidence, not a pass. Non-Ok results can be correct limit/nonexistence stops. Native status numbers are family-specific. The dossier existence statement is supplied beside each unresolved family to make the missing application explicit.

## bbinom

Take $n$ known and both shapes free. If every count is 0 or $n$, no MLE exists: the likelihood is maximised in the limit by a two-point law on 0 and $n$ when both values occur, and by a point mass when only one does. Otherwise the only possible escape is to the binomial, and an MLE exists exactly when some beta-binomial fits at least as well as the best binomial. Overdispersion, $S^2 > n\hat p(1-\hat p)$ with $\hat p = \bar x/n$ and $S^2$ the sample variance with the number of observations as divisor, is sufficient. For $n = 2$ it is also necessary; for $n \ge 3$ that is a conjecture. Holding one shape leaves only the point-mass case. With $n = 1$ the shapes are not identified, because the model is a Bernoulli distribution. With beta held and size free, constant positive data have only a point-mass supremum; all-zero data attain likelihood one at size zero with alpha unidentified. For other samples a finite maximum exists, though the numerical search may stop without resolving it. With alpha held and size free, constant positive data have a point-mass supremum and all-zero data attain likelihood one at size zero with beta unidentified. Other samples may remain unresolved against the negative-binomial face or the size budget.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5 | `size` / constant-5-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6 | `size` / constant-5-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7 | `size` / constant-5-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8 | `size` / constant-5-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9 | `size` / constant-5-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10 | `size` / constant-5-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 11 | `size` / zero-but-one | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 23 | `alpha` / large-count | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 25 | `alpha` / constant-count | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 49 | `beta` / large-count | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 51 | `beta` / constant-count | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 75 | `alpha,beta` / large-count | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9699 | `size` / face-endpoint-two-point-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9700 | `alpha` / face-endpoint-two-point-100 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 10159 | `size` / face-endpoint-two-point-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10160 | `alpha` / face-endpoint-two-point-1e+06 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 10904 | `size` / support-upper-count | 3 | open: finite-MLE existence not inferred from a finite candidate |

## betapr

With all coefficients free, constant positive data have unbounded likelihood. For nonconstant data the gamma and inverse-gamma limits can compete with finite fits. A complete joint sample criterion and joint uniqueness are not established. With scale held, a unique finite MLE exists exactly when the sample is nonconstant if both shapes are free. With scale and either shape held, the remaining shape has a unique finite maximum even for constant data. With both shapes held, scale has a unique finite maximum for every positive sample. With just $p$ held, the gamma limit remains; with just $q$ held, the inverse-gamma limit remains. Constant samples select these respective limits, and a general sample criterion and uniqueness for these two conditional problems are not established. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 166 | `free` / interior | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 167 | `free` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 168 | `free` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 169 | `free` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 170 | `free` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 171 | `free` / constant-0.651002-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 172 | `free` / constant-0.651002-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 173 | `free` / constant-0.651002-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 174 | `free` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 175 | `free` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 176 | `free` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 181 | `free` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 182 | `free` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 183 | `free` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 184 | `free` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 186 | `shape1` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 187 | `shape1` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 188 | `shape1` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 189 | `shape1` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 190 | `shape1` / constant-0.651002-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 191 | `shape1` / constant-0.651002-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 192 | `shape1` / constant-0.651002-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 193 | `shape1` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 194 | `shape1` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 195 | `shape1` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 203 | `shape1` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 205 | `shape2` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 206 | `shape2` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 207 | `shape2` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 208 | `shape2` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 209 | `shape2` / constant-0.651002-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 210 | `shape2` / constant-0.651002-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 211 | `shape2` / constant-0.651002-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 212 | `shape2` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 213 | `shape2` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 214 | `shape2` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 222 | `shape2` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9404 | `free` / face-gamma-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9405 | `shape1` / face-gamma-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9412 | `free` / face-inverse-gamma-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9864 | `free` / face-gamma-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9865 | `shape1` / face-gamma-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9872 | `free` / face-inverse-gamma-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |

## bhatt

With all coefficients free, constant samples have unbounded likelihood. For nonconstant samples the only unattained nonzero limiting face is uniform; the normal case $a=0$ is an attained member. General joint uniqueness and a complete data-only existence criterion are not established. With positive $a$ held and location free, an MLE exists exactly when the sample range exceeds $2a$; otherwise the uniform supremum is unattained. Holding $\sigma>0$ guarantees attainment, whether location is held or free. With both scales held, free location has a unique finite maximum. Holding $a=0$ gives the corresponding normal fitting problem.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 318 | `free` / interior | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 319 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 320 | `free` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 321 | `free` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 322 | `free` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 323 | `free` / constant-0.231334-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 324 | `free` / constant-0.231334-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 325 | `free` / constant-0.231334-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 326 | `free` / constant-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 327 | `free` / constant-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 328 | `free` / constant-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 329 | `free` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 330 | `free` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 331 | `free` / offset-1e8 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 332 | `free` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 333 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 334 | `mu` / interior | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 335 | `mu` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 336 | `mu` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 337 | `mu` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 338 | `mu` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 339 | `mu` / constant-0.231334-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 340 | `mu` / constant-0.231334-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 341 | `mu` / constant-0.231334-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 342 | `mu` / constant-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 343 | `mu` / constant-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 344 | `mu` / constant-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 345 | `mu` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 346 | `mu` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 347 | `mu` / offset-1e8 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 349 | `mu` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 383 | `a` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 387 | `a` / constant-0.231334-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 388 | `a` / constant-0.231334-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 389 | `a` / constant-0.231334-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 390 | `a` / constant-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 391 | `a` / constant-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 392 | `a` / constant-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 393 | `a` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 394 | `a` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 397 | `a` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 403 | `mu,a` / constant-0.231334-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 404 | `mu,a` / constant-0.231334-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 405 | `mu,a` / constant-0.231334-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 406 | `mu,a` / constant-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 407 | `mu,a` / constant-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 408 | `mu,a` / constant-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 409 | `mu,a` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 410 | `mu,a` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 413 | `mu,a` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9722 | `free` / face-uniform-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9723 | `mu` / face-uniform-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9726 | `a` / face-uniform-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9727 | `mu,a` / face-uniform-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10182 | `free` / face-uniform-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10183 | `mu` / face-uniform-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10186 | `a` / face-uniform-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10187 | `mu,a` / face-uniform-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |

## binom

For a nonempty sample of nonnegative integer counts with positive mean, a finite MLE with both $n$ and $p$ free exists exactly when $\bar x>v$, where $v=m^{-1}\sum_i(x_i-\bar x)^2$ and $m$ is sample size. All-zero data also attain a maximum, but do not identify the parameters. Integer maximizers need not be unique. With $n>0$ held at an integer at least $\max_i x_i$, $\hat p=\bar x/n$ is unique. Holding $n=0$ requires all-zero data and leaves $p$ unidentified. With $0<p<1$ held, a finite integer maximum exists, possibly at two adjacent sizes. Holding $p=0$ requires all-zero data and leaves $n$ unidentified. Holding $p=1$ requires constant counts and fixes $n$ to that count. With both coefficients held, the observations must have positive probability under the supplied model.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 9644 | `free` / face-Poisson-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10104 | `free` / face-Poisson-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |

## bnbinom

With all coefficients free, no general sample criterion for a finite maximum is asserted here. Negative binomial and Poisson limits can compete with finite fits. Exchanging $r$ and $\beta$ leaves the law unchanged, so an off-diagonal maximizer cannot give a unique labeled coefficient vector. All-zero samples have no finite MLE whenever any coefficient is free. Holding coefficients removes some limiting paths, and every one-free-shape fit has a unique finite maximum if at least one observation is positive. With alpha held and both other shapes free, a finite maximum exists for such samples, but labeled uniqueness need not hold. The remaining multivariate searches have no global certificate. Holding all coefficients leaves only likelihood evaluation, including for all-zero data.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 498 | `free` / interior | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 499 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 502 | `free` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 503 | `free` / constant-2-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 504 | `free` / constant-2-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 505 | `free` / constant-2-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 506 | `free` / constant-2-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 507 | `free` / constant-2-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 508 | `free` / constant-2-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 509 | `free` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 510 | `free` / zero-but-one | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 511 | `free` / constant-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 513 | `size` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 516 | `size` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 517 | `size` / constant-2-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 518 | `size` / constant-2-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 519 | `size` / constant-2-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 520 | `size` / constant-2-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 521 | `size` / constant-2-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 522 | `size` / constant-2-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 523 | `size` / large-count | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 524 | `size` / zero-but-one | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 525 | `size` / constant-count | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 551 | `size,alpha` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 559 | `beta` / constant-2-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 560 | `beta` / constant-2-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 561 | `beta` / constant-2-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 562 | `beta` / constant-2-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 563 | `beta` / constant-2-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 564 | `beta` / constant-2-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 565 | `beta` / large-count | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 566 | `beta` / zero-but-one | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 567 | `beta` / constant-count | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 593 | `alpha,beta` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |

## burr

With all coefficients free, constant positive data have unbounded likelihood. For nonconstant data, a finite MLE exists if and only if some finite point reaches the larger of the optimized Pareto and Weibull log likelihoods (equality is included). These are the only likelihood-retaining escape faces. At least half the sample at its minimum precludes a finite MLE. General joint uniqueness is not established; a successful certificate proves uniqueness for that sample. Exact finite/face ties can stop unresolved. With only scale $s$ held, a unique finite maximum exists for nonconstant data if some observation is below $s$. Constant data below $s$ have unbounded likelihood. If all observations are at least $s$, the supremum is on the Pareto path, except that all observations equal to $s$ give unbounded likelihood. Holding $k$ gives a unique finite fit for nonconstant data, but constant data remain unbounded when $c$ and $s$ are free. Holding $c$ reduces existence to the Lomax criterion on $x_i^{c}$, with a possible Weibull limit. Holding both shapes gives a unique scale estimate for any positive sample. Holding $k$ and $s$ gives a unique finite $c$ estimate unless all observations equal $s$. Holding $c$ and $s$ gives a unique finite $k$ estimate. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 610 | `free` / interior | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 611 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 612 | `free` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 613 | `free` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 614 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 615 | `free` / constant-2.04285-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 616 | `free` / constant-2.04285-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 617 | `free` / constant-2.04285-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 618 | `free` / constant-2.04285-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 619 | `free` / constant-2.04285-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 620 | `free` / constant-2.04285-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 621 | `free` / lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 622 | `free` / lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 623 | `free` / lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 624 | `free` / lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 625 | `free` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 626 | `free` / scale-1e+150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 627 | `free` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 628 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 650 | `shape2` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 652 | `shape2` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 653 | `shape2` / constant-2.04285-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 654 | `shape2` / constant-2.04285-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 655 | `shape2` / constant-2.04285-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 656 | `shape2` / constant-2.04285-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 657 | `shape2` / constant-2.04285-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 658 | `shape2` / constant-2.04285-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 666 | `shape2` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 691 | `scale` / constant-2.04285-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 692 | `scale` / constant-2.04285-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 693 | `scale` / constant-2.04285-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 694 | `scale` / constant-2.04285-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 695 | `scale` / constant-2.04285-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 696 | `scale` / constant-2.04285-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 697 | `scale` / lower-1e-06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 698 | `scale` / lower-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 699 | `scale` / lower-1e-17 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 700 | `scale` / lower-1e-200 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 701 | `scale` / scale-1e-150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 702 | `scale` / scale-1e+150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 704 | `scale` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 738 | `shape2,scale` / lower-1e-200 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9420 | `free` / face-weibull-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9422 | `shape2` / face-weibull-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9880 | `free` / face-weibull-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9882 | `shape2` / face-weibull-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10238 | `free` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10239 | `free` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10240 | `free` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10244 | `shape2` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10245 | `shape2` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10246 | `shape2` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10250 | `scale` / constant-extra-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10251 | `scale` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10252 | `scale` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |

## cmb

With $n$, $p$ and $\nu$ free, constant samples attain likelihood one. Observations on zero and one attain the Bernoulli maximum, which also equals the adjacent CMP limiting supremum. Observations on two adjacent positive integers, or on zero and an integer at least two, have an unattained saturated supremum. A uniform empirical distribution on zero through a maximum of at least two attains its saturated likelihood. General free-size existence and uniqueness do not have a simple closed-form criterion here. With positive $\nu$ held and $n,p$ free, a finite candidate strictly above the CMP supremum ensures finite attainment by the tail bound below. With $p$ held and $n,\nu$ free, the constant and symmetric two-point cases below are settled, but no complete criterion is asserted. With $n\ge2$ held, the joint finite interior maximum exists exactly when the empirical sufficient statistic lies in the polygon interior; endpoint vertices are also attained through endpoint probabilities. For held $0<p<1$ and $n\ge2$, a finite dispersion maximum exists when the empirical mean log binomial coefficient lies strictly between its minimum and maximum. With finite $\nu$ and positive $n$ held, the free probability has a unique mean-matching maximum, including endpoint probabilities. With $p,\nu$ held, a feasible free-size maximum is attained when $0<p<1$: for positive dispersion the fixed odds force mass away from bounded counts as $n$ grows, and for negative dispersion the normalizer's endpoint terms bound the tail. At zero dispersion the smallest feasible size is optimal. Endpoint probability holds require compatible data and can leave size unidentified. At $n=0$ both remaining coefficients are unidentified, and at $n=1$ dispersion is unidentified. Holding all coefficients only evaluates the specified law.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 826 | `free` / interior | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 831 | `free` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 832 | `free` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 833 | `free` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 834 | `free` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 835 | `free` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 836 | `free` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 837 | `free` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 838 | `free` / zero-but-one | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 839 | `free` / constant-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 845 | `size` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 846 | `size` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 847 | `size` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 848 | `size` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 849 | `size` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 850 | `size` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 851 | `size` / zero-but-one | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 852 | `prob` / interior | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 857 | `prob` / constant-4-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 858 | `prob` / constant-4-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 859 | `prob` / constant-4-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 860 | `prob` / constant-4-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 861 | `prob` / constant-4-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 862 | `prob` / constant-4-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 863 | `prob` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 864 | `prob` / zero-but-one | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 865 | `prob` / constant-count | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 878 | `nu` / interior | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 879 | `nu` / tiny-2 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 880 | `nu` / tiny-3 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 881 | `nu` / tiny-5 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 889 | `nu` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 890 | `nu` / zero-but-one | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 915 | `prob,nu` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 10919 | `free` / support-upper-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10920 | `size` / support-upper-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10921 | `prob` / support-upper-count | 6 | open: finite-MLE existence not inferred from a finite candidate |

## cmp

With both coefficients free, all-zero samples attain their maximum at $\lambda=0$, with $\nu$ unidentified. Positive constants and samples supported on two adjacent integers have an unattained supremum. Other samples have a unique finite maximum, possibly at the geometric boundary $\nu=0$. With finite $\nu$ held, the free rate has a unique maximum, including zero for all-zero data. With positive $\lambda$ held, a unique finite dispersion maximum exists when some observation exceeds one. Samples supported on zero and one instead approach their supremum as $\nu\to\infty$. Holding $\lambda=0$ requires all-zero data and leaves dispersion unidentified. Holding both coefficients requires only a valid law and compatible data.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 969 | `nu` / large-count | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |

## dgamma

With both coefficients free, a finite MLE exists if and only if the integer sample range is at least two. Constant samples and samples on two adjacent integers have unattained suprema. With either coefficient held, a finite MLE exists if and only if at least one observation is positive. With shape held this maximum is unique. Uniqueness with rate held or both coefficients free has not been established. Holding both coefficients evaluates the likelihood without fitting.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 987 | `free` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 988 | `free` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 989 | `free` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 990 | `free` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 991 | `free` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 992 | `free` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 993 | `free` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 994 | `free` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 995 | `free` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 996 | `free` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 997 | `free` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 998 | `free` / zero-but-one | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 999 | `free` / constant-count | 4 | open: finite-MLE existence not inferred from a finite candidate |

## dlaplace

For a nonconstant integer sample with both coefficients free, a finite MLE exists. Every integer in the sample median interval maximizes location, with the same unique scale. Location is unique exactly when that interval contains one integer. Constant samples have only an unattained point-mass supremum. With integer location held, a unique interior scale MLE exists unless all observations equal that location. With scale held strictly between zero and one, the integer medians maximize location even for constant samples. Holding both coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 1043 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1047 | `free` / constant-0-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1048 | `free` / constant-0-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1049 | `free` / constant-0-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1057 | `location` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1061 | `location` / constant-0-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1062 | `location` / constant-0-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1063 | `location` / constant-0-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |

## dweibull

With both coefficients free, a unique finite MLE exists exactly when $\max_i x_i-\min_i x_i\ge2$. Constant samples and samples on two adjacent counts have only an excluded limiting maximum. Holding $\beta$ gives a unique finite estimate of $q$ exactly when at least one observation is positive. Holding $q$ and observing at least one positive count gives a unique finite estimate of $\beta$ exactly when some observation is at least two. If $q$ is held and all observations are zero, every positive $\beta$ maximizes the likelihood. Holding both leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 1211 | `free` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1215 | `free` / constant-2-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1216 | `free` / constant-2-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1217 | `free` / constant-2-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1218 | `free` / constant-2-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1219 | `free` / constant-2-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1220 | `free` / constant-2-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1221 | `free` / large-count | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 1222 | `free` / zero-but-one | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1223 | `free` / constant-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1236 | `shape1` / zero-but-one | 6 | open: finite-MLE existence not inferred from a finite candidate |

## emg

With all coefficients free, constant samples have unbounded likelihood. For nonconstant samples a finite maximum need not exist, because a normal or translated-exponential limit can compete with interior fits. A complete existence criterion and joint uniqueness result are not established. Holding $\lambda$ removes the normal escape; holding $\sigma$ removes the translated-exponential escape. With both held, free location has a unique finite maximum. Other held subsets are compared with their compatible limits numerically.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 1266 | `free` / interior | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1267 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1268 | `free` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1269 | `free` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1270 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1271 | `free` / constant-1.31838-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1272 | `free` / constant-1.31838-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1273 | `free` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1274 | `free` / constant-1.31838-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1275 | `free` / constant-1.31838-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1276 | `free` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1277 | `free` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1278 | `free` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1279 | `free` / offset-1e8 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1280 | `free` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1281 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1289 | `mu` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1292 | `mu` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1293 | `mu` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1296 | `mu` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1299 | `sigma` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1300 | `sigma` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1302 | `sigma` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1303 | `sigma` / constant-1.31838-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1304 | `sigma` / constant-1.31838-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1305 | `sigma` / constant-1.31838-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1306 | `sigma` / constant-1.31838-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1307 | `sigma` / constant-1.31838-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1308 | `sigma` / constant-1.31838-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1309 | `sigma` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1313 | `sigma` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1325 | `mu,sigma` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1331 | `lambda` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1332 | `lambda` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1335 | `lambda` / constant-1.31838-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1336 | `lambda` / constant-1.31838-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1337 | `lambda` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1338 | `lambda` / constant-1.31838-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1339 | `lambda` / constant-1.31838-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1340 | `lambda` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1341 | `lambda` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1345 | `lambda` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1353 | `mu,lambda` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1356 | `mu,lambda` / constant-1.31838-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1374 | `sigma,lambda` / scale-1e+150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9706 | `free` / face-normal-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9707 | `mu` / face-normal-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9708 | `sigma` / face-normal-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9709 | `mu,sigma` / face-normal-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9714 | `free` / face-translated-exponential-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9716 | `sigma` / face-translated-exponential-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9718 | `lambda` / face-translated-exponential-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10166 | `free` / face-normal-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10167 | `mu` / face-normal-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10168 | `sigma` / face-normal-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10169 | `mu,sigma` / face-normal-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10174 | `free` / face-translated-exponential-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10176 | `sigma` / face-translated-exponential-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10178 | `lambda` / face-translated-exponential-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10262 | `free` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10263 | `free` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10264 | `free` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10267 | `mu` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10268 | `sigma` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10269 | `sigma` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10270 | `sigma` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10274 | `lambda` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10275 | `lambda` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10276 | `lambda` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10279 | `mu,lambda` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |

## fpareto

With all coefficients free, every nonempty sample has unbounded likelihood. Holding $\mu$ gives the generalized beta of the second kind (GB2) on excesses. Its joint likelihood can escape to several proper limit models; a complete joint existence criterion and joint uniqueness are not established. Holding both beta shapes $\alpha$ and $\tau$ gives a unique finite power/scale fit for nonconstant excesses, while constant data remain unbounded if both power and scale are free. With power and scale held, the free beta shapes have a unique finite fit for nonconstant excesses, but both free beta shapes can concentrate on constant data. Further holds can remove these concentration paths. Free-threshold fits are not supported. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 1588 | `min` / interior | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 1589 | `min` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1590 | `min` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1591 | `min` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1592 | `min` / ties | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 1593 | `min` / constant-1.11255-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1594 | `min` / constant-1.11255-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1595 | `min` / constant-1.11255-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1596 | `min` / constant-1.11255-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1597 | `min` / constant-1.11255-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1598 | `min` / constant-1.11255-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1599 | `min` / scale-1e-150 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 1600 | `min` / scale-1e+150 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 1601 | `min` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1602 | `min` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1604 | `shape1,min` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1605 | `shape1,min` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1606 | `shape1,min` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1607 | `shape1,min` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1608 | `shape1,min` / constant-1.11255-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1609 | `shape1,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1610 | `shape1,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1611 | `shape1,min` / constant-1.11255-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1612 | `shape1,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1613 | `shape1,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1616 | `shape1,min` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1617 | `shape1,min` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1619 | `shape2,min` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1620 | `shape2,min` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1621 | `shape2,min` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1622 | `shape2,min` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1623 | `shape2,min` / constant-1.11255-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1624 | `shape2,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1625 | `shape2,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1626 | `shape2,min` / constant-1.11255-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1627 | `shape2,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1628 | `shape2,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1631 | `shape2,min` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1632 | `shape2,min` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1634 | `shape1,shape2,min` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1635 | `shape1,shape2,min` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1636 | `shape1,shape2,min` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1637 | `shape1,shape2,min` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1638 | `shape1,shape2,min` / constant-1.11255-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1639 | `shape1,shape2,min` / constant-1.11255-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1640 | `shape1,shape2,min` / constant-1.11255-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1641 | `shape1,shape2,min` / constant-1.11255-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1642 | `shape1,shape2,min` / constant-1.11255-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1643 | `shape1,shape2,min` / constant-1.11255-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1646 | `shape1,shape2,min` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1647 | `shape1,shape2,min` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1649 | `shape3,min` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1650 | `shape3,min` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1651 | `shape3,min` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1652 | `shape3,min` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1653 | `shape3,min` / constant-1.11255-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1654 | `shape3,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1655 | `shape3,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1656 | `shape3,min` / constant-1.11255-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1657 | `shape3,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1658 | `shape3,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1661 | `shape3,min` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1662 | `shape3,min` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1679 | `shape2,shape3,min` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1680 | `shape2,shape3,min` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1683 | `shape2,shape3,min` / constant-1.11255-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1684 | `shape2,shape3,min` / constant-1.11255-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1685 | `shape2,shape3,min` / constant-1.11255-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1686 | `shape2,shape3,min` / constant-1.11255-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1687 | `shape2,shape3,min` / constant-1.11255-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1688 | `shape2,shape3,min` / constant-1.11255-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1692 | `shape2,shape3,min` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1710 | `scale,min` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1713 | `scale,min` / constant-1.11255-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1714 | `scale,min` / constant-1.11255-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1715 | `scale,min` / constant-1.11255-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1716 | `scale,min` / constant-1.11255-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1717 | `scale,min` / constant-1.11255-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1718 | `scale,min` / constant-1.11255-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1719 | `scale,min` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1720 | `scale,min` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1722 | `scale,min` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1728 | `shape1,scale,min` / constant-1.11255-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1729 | `shape1,scale,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1730 | `shape1,scale,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1731 | `shape1,scale,min` / constant-1.11255-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1732 | `shape1,scale,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1733 | `shape1,scale,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1734 | `shape1,scale,min` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 1735 | `shape1,scale,min` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 1737 | `shape1,scale,min` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1743 | `shape2,scale,min` / constant-1.11255-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1744 | `shape2,scale,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1745 | `shape2,scale,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1746 | `shape2,scale,min` / constant-1.11255-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1747 | `shape2,scale,min` / constant-1.11255-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1748 | `shape2,scale,min` / constant-1.11255-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1750 | `shape2,scale,min` / scale-1e+150 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 1752 | `shape2,scale,min` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 1765 | `shape1,shape2,scale,min` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 1773 | `shape3,scale,min` / constant-1.11255-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1774 | `shape3,scale,min` / constant-1.11255-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1775 | `shape3,scale,min` / constant-1.11255-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1776 | `shape3,scale,min` / constant-1.11255-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1777 | `shape3,scale,min` / constant-1.11255-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1778 | `shape3,scale,min` / constant-1.11255-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1780 | `shape3,scale,min` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 1782 | `shape3,scale,min` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9532 | `min` / face-trgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9534 | `shape2,min` / face-trgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9535 | `shape1,shape2,min` / face-trgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9538 | `shape2,shape3,min` / face-trgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9540 | `scale,min` / face-trgamma-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9548 | `min` / face-invtrgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9550 | `shape2,min` / face-invtrgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9551 | `shape1,shape2,min` / face-invtrgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9554 | `shape2,shape3,min` / face-invtrgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9556 | `scale,min` / face-invtrgamma-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9564 | `min` / face-lnorm-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9572 | `scale,min` / face-lnorm-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9580 | `min` / face-pareto-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9581 | `shape1,min` / face-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9582 | `shape2,min` / face-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9583 | `shape1,shape2,min` / face-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9584 | `shape3,min` / face-pareto-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9586 | `shape2,shape3,min` / face-pareto-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9588 | `scale,min` / face-pareto-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9592 | `shape3,scale,min` / face-pareto-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9596 | `min` / face-power-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9597 | `shape1,min` / face-power-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9598 | `shape2,min` / face-power-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9599 | `shape1,shape2,min` / face-power-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9600 | `shape3,min` / face-power-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9602 | `shape2,shape3,min` / face-power-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9604 | `scale,min` / face-power-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9605 | `shape1,scale,min` / face-power-100 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9612 | `min` / face-double-pareto-100 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9615 | `shape1,shape2,min` / face-double-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9618 | `shape2,shape3,min` / face-double-pareto-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9992 | `min` / face-trgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9994 | `shape2,min` / face-trgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9995 | `shape1,shape2,min` / face-trgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9998 | `shape2,shape3,min` / face-trgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10000 | `scale,min` / face-trgamma-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10008 | `min` / face-invtrgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10010 | `shape2,min` / face-invtrgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10011 | `shape1,shape2,min` / face-invtrgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10014 | `shape2,shape3,min` / face-invtrgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10016 | `scale,min` / face-invtrgamma-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10024 | `min` / face-lnorm-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10032 | `scale,min` / face-lnorm-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10040 | `min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10041 | `shape1,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10042 | `shape2,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10043 | `shape1,shape2,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10044 | `shape3,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10045 | `shape1,shape3,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10046 | `shape2,shape3,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10048 | `scale,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10049 | `shape1,scale,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10050 | `shape2,scale,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10052 | `shape3,scale,min` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10056 | `min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10057 | `shape1,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10058 | `shape2,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10059 | `shape1,shape2,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10060 | `shape3,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10061 | `shape1,shape3,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10062 | `shape2,shape3,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10064 | `scale,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10065 | `shape1,scale,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10066 | `shape2,scale,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10068 | `shape3,scale,min` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10072 | `min` / face-double-pareto-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10073 | `shape1,min` / face-double-pareto-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10074 | `shape2,min` / face-double-pareto-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10075 | `shape1,shape2,min` / face-double-pareto-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10076 | `shape3,min` / face-double-pareto-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10078 | `shape2,shape3,min` / face-double-pareto-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10080 | `scale,min` / face-double-pareto-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10081 | `shape1,scale,min` / face-double-pareto-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10298 | `min` / constant-extra-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10299 | `min` / constant-extra-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10300 | `min` / constant-extra-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10301 | `shape1,min` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10302 | `shape1,min` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10303 | `shape1,min` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10304 | `shape2,min` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10305 | `shape2,min` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10306 | `shape2,min` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10307 | `shape1,shape2,min` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10308 | `shape1,shape2,min` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10309 | `shape1,shape2,min` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10310 | `shape3,min` / constant-extra-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10311 | `shape3,min` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10312 | `shape3,min` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10316 | `shape2,shape3,min` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10317 | `shape2,shape3,min` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10318 | `shape2,shape3,min` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10322 | `scale,min` / constant-extra-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10323 | `scale,min` / constant-extra-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10324 | `scale,min` / constant-extra-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10325 | `shape1,scale,min` / constant-extra-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10326 | `shape1,scale,min` / constant-extra-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10327 | `shape1,scale,min` / constant-extra-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10328 | `shape2,scale,min` / constant-extra-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10329 | `shape2,scale,min` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10330 | `shape2,scale,min` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10335 | `shape3,scale,min` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10336 | `shape3,scale,min` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10714 | `min` / support-lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10715 | `min` / support-lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10716 | `min` / support-lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10717 | `min` / support-lower-1e-200 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 10718 | `shape1,min` / support-lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10719 | `shape1,min` / support-lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10720 | `shape1,min` / support-lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10721 | `shape1,min` / support-lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10726 | `shape1,shape2,min` / support-lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10727 | `shape1,shape2,min` / support-lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10728 | `shape1,shape2,min` / support-lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10729 | `shape1,shape2,min` / support-lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10730 | `shape3,min` / support-lower-1e-06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10731 | `shape3,min` / support-lower-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10732 | `shape3,min` / support-lower-1e-17 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10733 | `shape3,min` / support-lower-1e-200 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10738 | `shape2,shape3,min` / support-lower-1e-06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10739 | `shape2,shape3,min` / support-lower-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10740 | `shape2,shape3,min` / support-lower-1e-17 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10741 | `shape2,shape3,min` / support-lower-1e-200 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10748 | `scale,min` / support-lower-1e-17 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10749 | `scale,min` / support-lower-1e-200 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10750 | `shape1,scale,min` / support-lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10751 | `shape1,scale,min` / support-lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10752 | `shape1,scale,min` / support-lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10753 | `shape1,scale,min` / support-lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10757 | `shape2,scale,min` / support-lower-1e-200 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10765 | `shape3,scale,min` / support-lower-1e-200 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10773 | `shape2,shape3,scale,min` / support-lower-1e-200 | 9 | open: finite-MLE existence not inferred from a finite candidate |

## frechet

With all coefficients free and unrestricted shape, the likelihood is unbounded for every sample. The fitted model therefore restricts $\lambda\ge1$. A minimum repeated more than $n/2$ times gives unbounded likelihood. At exactly $n/2$ repeats, a finite collapse face must be compared with finite candidates. A Gumbel limit can also prevent attainment. A complete existence criterion and joint uniqueness are not established. With $\mu$ held and observations strictly above it, both remaining coefficients have a unique finite MLE exactly for nonconstant data. Holding $\mu$ and $\lambda$ gives a unique finite scale estimate even for constants. Holding $\mu$ and $\sigma$ gives a unique finite shape estimate unless every $x_i-\mu=\sigma$, when likelihood is unbounded. With only $\lambda$ held, a finite MLE exists exactly when $\lambda(n-k)>k$, where $k$ is the minimum multiplicity. Otherwise the supremum is an unattained collapse face or is unbounded. With only $\sigma$ held, constant data have unbounded shape likelihood. Holding $\lambda$ and $\sigma$ gives a unique finite location maximum. Uniqueness for the remaining free-location fits is not established. Holding all coefficients leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 1829 | `free` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 1830 | `free` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1831 | `free` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1832 | `free` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1833 | `free` / constant-1.04938-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1834 | `free` / constant-1.04938-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1835 | `free` / constant-1.04938-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1836 | `free` / constant-1.04938-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1837 | `free` / constant-1.04938-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1838 | `free` / constant-1.04938-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1846 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 1915 | `sigma` / lower-1e-17 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1916 | `sigma` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1917 | `sigma` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 1936 | `lambda,sigma` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10346 | `free` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10347 | `free` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10348 | `free` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10931 | `sigma` / support-lower-ulp | 3 | open: finite-MLE existence not inferred from a finite candidate |

## gb2

With all four coefficients free, constant positive samples have unbounded likelihood. For nonconstant samples, a supremum may occur on a limiting model rather than at finite coefficients. No general existence or uniqueness criterion is established. Holding coefficients can remove particular escapes. For nonconstant data, holding both beta shapes gives a unique finite maximum in power and scale. Holding power and scale instead gives a unique finite maximum in the beta shapes. With power, scale and either beta shape held, the remaining shape has a unique finite maximum even for constant data.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 2052 | `free` / interior | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2053 | `free` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2054 | `free` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2055 | `free` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2056 | `free` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2057 | `free` / constant-3.62937-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2058 | `free` / constant-3.62937-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 2059 | `free` / constant-3.62937-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 2060 | `free` / constant-3.62937-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2061 | `free` / constant-3.62937-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 2062 | `free` / constant-3.62937-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 2063 | `free` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2064 | `free` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2065 | `free` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2066 | `free` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2067 | `free` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2068 | `free` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2069 | `free` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2070 | `free` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2071 | `shape1` / interior | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2072 | `shape1` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2073 | `shape1` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2074 | `shape1` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2075 | `shape1` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2076 | `shape1` / constant-3.62937-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2077 | `shape1` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2078 | `shape1` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2079 | `shape1` / constant-3.62937-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2080 | `shape1` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2081 | `shape1` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2082 | `shape1` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2083 | `shape1` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2084 | `shape1` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2085 | `shape1` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2086 | `shape1` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2087 | `shape1` / scale-1e+150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2088 | `shape1` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2089 | `shape1` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2091 | `shape2` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2092 | `shape2` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2093 | `shape2` / tiny-5 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2094 | `shape2` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2095 | `shape2` / constant-3.62937-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2096 | `shape2` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2097 | `shape2` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2098 | `shape2` / constant-3.62937-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2099 | `shape2` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2100 | `shape2` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2107 | `shape2` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2108 | `shape2` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2114 | `shape1,shape2` / constant-3.62937-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2115 | `shape1,shape2` / constant-3.62937-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2116 | `shape1,shape2` / constant-3.62937-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2117 | `shape1,shape2` / constant-3.62937-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2118 | `shape1,shape2` / constant-3.62937-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2119 | `shape1,shape2` / constant-3.62937-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2120 | `shape1,shape2` / lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2121 | `shape1,shape2` / lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2122 | `shape1,shape2` / lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2123 | `shape1,shape2` / lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2127 | `shape1,shape2` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2129 | `shape3` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2130 | `shape3` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2131 | `shape3` / tiny-5 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2132 | `shape3` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2133 | `shape3` / constant-3.62937-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2134 | `shape3` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2135 | `shape3` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2136 | `shape3` / constant-3.62937-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2137 | `shape3` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2138 | `shape3` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2139 | `shape3` / lower-1e-06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2140 | `shape3` / lower-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2141 | `shape3` / lower-1e-17 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2142 | `shape3` / lower-1e-200 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2145 | `shape3` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2146 | `shape3` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2168 | `shape2,shape3` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2170 | `shape2,shape3` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2171 | `shape2,shape3` / constant-3.62937-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2172 | `shape2,shape3` / constant-3.62937-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2173 | `shape2,shape3` / constant-3.62937-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2174 | `shape2,shape3` / constant-3.62937-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2175 | `shape2,shape3` / constant-3.62937-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2176 | `shape2,shape3` / constant-3.62937-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2177 | `shape2,shape3` / lower-1e-06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2178 | `shape2,shape3` / lower-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2179 | `shape2,shape3` / lower-1e-17 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2180 | `shape2,shape3` / lower-1e-200 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2184 | `shape2,shape3` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2204 | `scale` / interior | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2205 | `scale` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2206 | `scale` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2207 | `scale` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2208 | `scale` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2209 | `scale` / constant-3.62937-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2210 | `scale` / constant-3.62937-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2211 | `scale` / constant-3.62937-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2212 | `scale` / constant-3.62937-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2213 | `scale` / constant-3.62937-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2214 | `scale` / constant-3.62937-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2217 | `scale` / lower-1e-17 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2218 | `scale` / lower-1e-200 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2219 | `scale` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2220 | `scale` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2222 | `scale` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2228 | `shape1,scale` / constant-3.62937-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2229 | `shape1,scale` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2230 | `shape1,scale` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2231 | `shape1,scale` / constant-3.62937-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2232 | `shape1,scale` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2233 | `shape1,scale` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2234 | `shape1,scale` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2235 | `shape1,scale` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2236 | `shape1,scale` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2237 | `shape1,scale` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2238 | `shape1,scale` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2239 | `shape1,scale` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 2241 | `shape1,scale` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2248 | `shape2,scale` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2249 | `shape2,scale` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2251 | `shape2,scale` / constant-3.62937-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2252 | `shape2,scale` / constant-3.62937-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2257 | `shape2,scale` / scale-1e-150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 2258 | `shape2,scale` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 2260 | `shape2,scale` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2277 | `shape1,shape2,scale` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 2285 | `shape3,scale` / constant-3.62937-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2286 | `shape3,scale` / constant-3.62937-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2287 | `shape3,scale` / constant-3.62937-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2288 | `shape3,scale` / constant-3.62937-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2289 | `shape3,scale` / constant-3.62937-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2290 | `shape3,scale` / constant-3.62937-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2294 | `shape3,scale` / lower-1e-200 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2295 | `shape3,scale` / scale-1e-150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 2296 | `shape3,scale` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 2298 | `shape3,scale` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9436 | `free` / face-trgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9438 | `shape2` / face-trgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9439 | `shape1,shape2` / face-trgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9442 | `shape2,shape3` / face-trgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9444 | `scale` / face-trgamma-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9452 | `free` / face-invtrgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9454 | `shape2` / face-invtrgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9455 | `shape1,shape2` / face-invtrgamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9458 | `shape2,shape3` / face-invtrgamma-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9460 | `scale` / face-invtrgamma-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9468 | `free` / face-lnorm-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9471 | `shape1,shape2` / face-lnorm-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9474 | `shape2,shape3` / face-lnorm-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9476 | `scale` / face-lnorm-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9484 | `free` / face-pareto-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9485 | `shape1` / face-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9486 | `shape2` / face-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9487 | `shape1,shape2` / face-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9488 | `shape3` / face-pareto-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9490 | `shape2,shape3` / face-pareto-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9492 | `scale` / face-pareto-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9496 | `shape3,scale` / face-pareto-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9500 | `free` / face-power-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9501 | `shape1` / face-power-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9502 | `shape2` / face-power-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9503 | `shape1,shape2` / face-power-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9504 | `shape3` / face-power-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9506 | `shape2,shape3` / face-power-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9508 | `scale` / face-power-100 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 9509 | `shape1,scale` / face-power-100 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9516 | `free` / face-double-pareto-100 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9519 | `shape1,shape2` / face-double-pareto-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9522 | `shape2,shape3` / face-double-pareto-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9896 | `free` / face-trgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9898 | `shape2` / face-trgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9899 | `shape1,shape2` / face-trgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9902 | `shape2,shape3` / face-trgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9904 | `scale` / face-trgamma-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9912 | `free` / face-invtrgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9914 | `shape2` / face-invtrgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9915 | `shape1,shape2` / face-invtrgamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9918 | `shape2,shape3` / face-invtrgamma-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9920 | `scale` / face-invtrgamma-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9928 | `free` / face-lnorm-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9931 | `shape1,shape2` / face-lnorm-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9934 | `shape2,shape3` / face-lnorm-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9936 | `scale` / face-lnorm-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9944 | `free` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9945 | `shape1` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9946 | `shape2` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9947 | `shape1,shape2` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9948 | `shape3` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9949 | `shape1,shape3` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9950 | `shape2,shape3` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9952 | `scale` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9953 | `shape1,scale` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9954 | `shape2,scale` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9956 | `shape3,scale` / face-pareto-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9960 | `free` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9961 | `shape1` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9962 | `shape2` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9963 | `shape1,shape2` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9964 | `shape3` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9965 | `shape1,shape3` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9966 | `shape2,shape3` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9968 | `scale` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9969 | `shape1,scale` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9970 | `shape2,scale` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9972 | `shape3,scale` / face-power-1e+06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9976 | `free` / face-double-pareto-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9977 | `shape1` / face-double-pareto-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9978 | `shape2` / face-double-pareto-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9979 | `shape1,shape2` / face-double-pareto-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9980 | `shape3` / face-double-pareto-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9982 | `shape2,shape3` / face-double-pareto-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9984 | `scale` / face-double-pareto-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9985 | `shape1,scale` / face-double-pareto-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9986 | `shape2,scale` / face-double-pareto-1e+06 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10382 | `free` / constant-extra-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10383 | `free` / constant-extra-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10384 | `free` / constant-extra-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10385 | `shape1` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10386 | `shape1` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10387 | `shape1` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10388 | `shape2` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10389 | `shape2` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10390 | `shape2` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10391 | `shape1,shape2` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10392 | `shape1,shape2` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10393 | `shape1,shape2` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10394 | `shape3` / constant-extra-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10395 | `shape3` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10396 | `shape3` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10400 | `shape2,shape3` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10401 | `shape2,shape3` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10402 | `shape2,shape3` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10406 | `scale` / constant-extra-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10407 | `scale` / constant-extra-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10408 | `scale` / constant-extra-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10409 | `shape1,scale` / constant-extra-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10410 | `shape1,scale` / constant-extra-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10411 | `shape1,scale` / constant-extra-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10412 | `shape2,scale` / constant-extra-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10413 | `shape2,scale` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10414 | `shape2,scale` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10419 | `shape3,scale` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10420 | `shape3,scale` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |

## ged

With all coefficients free, fitting restricts $\nu\ge1$; constant data have unbounded scale likelihood, and nonconstant data may select the unattained uniform limit. Without the fitting restriction, the all-free likelihood is unbounded for every sample. Global uniqueness with free shape in the restricted domain is unknown. For held $\nu>1$, nonconstant data have a unique location-scale maximum; at $\nu=1$, median intervals can give nonunique locations. With shape held, holding $\mu$ gives a unique scale maximum unless all residuals vanish. Holding $\sigma$ removes scale collapse; with shape held above one its location maximum is unique, while shape one permits median intervals. Free shape retains its fitting restriction and conditional uniform comparison. Held positive shapes below one also admit a location maximum at an observation and an explicit scale profile when residual spread is positive, but locations can tie. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 2356 | `free` / interior | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2357 | `free` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2358 | `free` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2359 | `free` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2361 | `free` / constant--0.0213396-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2362 | `free` / constant--0.0213396-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2363 | `free` / constant--0.0213396-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2364 | `free` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2365 | `free` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2366 | `free` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2367 | `free` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2368 | `free` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2369 | `free` / offset-1e8 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2370 | `free` / limit-outlier | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2371 | `free` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2373 | `mean` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2374 | `mean` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2375 | `mean` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2376 | `mean` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2377 | `mean` / constant--0.0213396-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2378 | `mean` / constant--0.0213396-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2379 | `mean` / constant--0.0213396-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2380 | `mean` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2381 | `mean` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2382 | `mean` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2383 | `mean` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2385 | `mean` / offset-1e8 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2387 | `mean` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 2394 | `sd` / constant--0.0213396-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2395 | `sd` / constant--0.0213396-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2397 | `sd` / constant-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2398 | `sd` / constant-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2401 | `sd` / offset-1e8 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9730 | `free` / face-uniform-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9731 | `mean` / face-uniform-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10190 | `free` / face-uniform-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10191 | `mean` / face-uniform-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10194 | `nu` / face-uniform-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |

## genbeta

With all coefficients free, the likelihood is unbounded for every positive sample. With the upper endpoint $b$ held above the data, constant samples can still have unbounded likelihood when concentration remains free. For nonconstant samples the gamma and power limits may prevent attainment. No general uniqueness theorem is established for the three-shape fit. With $b$ and $a$ held, the two beta shapes have a unique finite MLE for nonconstant data; holding either beta shape as well gives a unique finite MLE even for constant data. When $q=1$, only the product $pa$ is identified if both factors are free.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 2485 | `scale` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2488 | `scale` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2489 | `scale` / constant-0.974335-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2490 | `scale` / constant-0.974335-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2491 | `scale` / constant-0.974335-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2493 | `scale` / constant-0.25-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2494 | `scale` / constant-0.25-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2495 | `scale` / lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2496 | `scale` / upper-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2497 | `scale` / lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2498 | `scale` / upper-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2499 | `scale` / lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2500 | `scale` / lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2501 | `scale` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2502 | `scale` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2508 | `shape1,scale` / constant-0.974335-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2509 | `shape1,scale` / constant-0.974335-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2510 | `shape1,scale` / constant-0.974335-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2511 | `shape1,scale` / constant-0.25-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2512 | `shape1,scale` / constant-0.25-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2513 | `shape1,scale` / constant-0.25-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2514 | `shape1,scale` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2516 | `shape1,scale` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2518 | `shape1,scale` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2519 | `shape1,scale` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2520 | `shape1,scale` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2521 | `shape1,scale` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2522 | `shape2,scale` / interior | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2523 | `shape2,scale` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2526 | `shape2,scale` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2527 | `shape2,scale` / constant-0.974335-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2528 | `shape2,scale` / constant-0.974335-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2529 | `shape2,scale` / constant-0.974335-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2530 | `shape2,scale` / constant-0.25-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2531 | `shape2,scale` / constant-0.25-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2532 | `shape2,scale` / constant-0.25-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2533 | `shape2,scale` / lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2534 | `shape2,scale` / upper-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2535 | `shape2,scale` / lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2536 | `shape2,scale` / upper-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2537 | `shape2,scale` / lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2538 | `shape2,scale` / lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2539 | `shape2,scale` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2540 | `shape2,scale` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2566 | `shape3,scale` / constant-0.974335-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2567 | `shape3,scale` / constant-0.974335-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2569 | `shape3,scale` / constant-0.25-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2570 | `shape3,scale` / constant-0.25-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2571 | `shape3,scale` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2573 | `shape3,scale` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2575 | `shape3,scale` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2576 | `shape3,scale` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2577 | `shape3,scale` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2578 | `shape3,scale` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2590 | `shape1,shape3,scale` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2592 | `shape1,shape3,scale` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2594 | `shape1,shape3,scale` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2595 | `shape1,shape3,scale` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 2596 | `shape1,shape3,scale` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9628 | `scale` / face-log-gamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9630 | `shape2,scale` / face-log-gamma-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9636 | `scale` / face-power-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 9638 | `shape2,scale` / face-power-100 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10088 | `scale` / face-log-gamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10090 | `shape2,scale` / face-log-gamma-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10096 | `scale` / face-power-1e+06 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 10097 | `shape1,scale` / face-power-1e+06 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 10098 | `shape2,scale` / face-power-1e+06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10100 | `shape3,scale` / face-power-1e+06 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 10935 | `scale` / support-upper-ulp | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10937 | `shape2,scale` / support-upper-ulp | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10939 | `shape3,scale` / support-upper-ulp | 5 | open: finite-MLE existence not inferred from a finite candidate |

## gev

Without restrictions the likelihood is unbounded for every sample, so fits keep the shape in $-1 \le \xi \le 1$. Within that range the only known obstacle is a tie at the sample minimum. If more than half of the observations equal the minimum, no MLE exists. If exactly half do, the supremum is approached as $\xi \to 1$ and the scale shrinks, unless an interior fit does better.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 2665 | `free` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2668 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2724 | `mu,sigma` / scale-1e+150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2729 | `xi` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |

## gompertz

With both coefficients free, a unique finite MLE exists for a nonconstant nonnegative sample exactly when $\overline{x^2}<2\bar x^2$. Otherwise a nonconstant sample selects the excluded exponential face. Constant samples have unbounded likelihood. With $a$ held and a positive sample sum, a unique finite estimate of $b$ exists exactly when $\sum_i x_i>a\sum_i x_i^2/2$. Otherwise the supremum is at $b=0$. With $a$ held and all observations zero, every positive $b$ is a maximizer. With $b$ held, a unique finite estimate of $a$ exists exactly when the sample sum is positive. Holding both leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 2797 | `free` / constant-0.880006-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2798 | `free` / constant-0.880006-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2799 | `free` / constant-0.880006-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2800 | `free` / constant-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2801 | `free` / constant-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2802 | `free` / constant-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2809 | `free` / limit-outlier | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2810 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2827 | `a` / scale-1e+150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2828 | `a` / limit-outlier | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 2846 | `b` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 2847 | `b` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |

## gumbel

With both coefficients free, a unique finite MLE exists for every nonconstant finite sample. Constant samples have unbounded likelihood. With location held, a unique positive scale MLE exists unless every observation equals that location. With positive scale held, the location MLE is unique even for constant data. Holding both coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 2992 | `sigma` / scale-1e+150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |

## ht

With all coefficients free, the endpoint is the sample minimum and estimated $\nu$ is restricted to $\nu\ge1$. A finite MLE is not guaranteed: excessive endpoint ties cause collapse and a half-normal limit can prevent attainment. Global shape uniqueness is unknown. Holding the endpoint permits any estimated $\nu>0$; with scale free, any observation at that held endpoint then makes the free-shape likelihood unbounded. With $\nu$ held and scale free, a unique scale maximum exists exactly when $\nu(n-k)>k$, where $k$ counts endpoint observations; equality gives a finite unattained supremum and the reverse inequality gives unbounded likelihood. Holding scale removes collapse, but free shape still requires boundary comparisons. With both shape and scale held, a free endpoint is uniquely the sample minimum. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 3167 | `free` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3168 | `free` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3170 | `free` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3171 | `free` / constant-1.5768-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3172 | `free` / constant-1.5768-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3173 | `free` / constant-1.5768-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3174 | `free` / constant-1.5768-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3175 | `free` / constant-1.5768-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3176 | `free` / constant-1.5768-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3185 | `free` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3187 | `mu` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3188 | `mu` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3190 | `mu` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3191 | `mu` / constant-1.5768-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3192 | `mu` / constant-1.5768-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3193 | `mu` / constant-1.5768-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3194 | `mu` / constant-1.5768-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3195 | `mu` / constant-1.5768-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3196 | `mu` / constant-1.5768-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3197 | `mu` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3198 | `mu` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3199 | `mu` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3200 | `mu` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3203 | `mu` / offset-1e8 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3205 | `mu` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3247 | `sigma` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3248 | `sigma` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3250 | `sigma` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3251 | `sigma` / constant-1.5768-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3252 | `sigma` / constant-1.5768-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3253 | `sigma` / constant-1.5768-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3254 | `sigma` / constant-1.5768-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3255 | `sigma` / constant-1.5768-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3256 | `sigma` / constant-1.5768-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3257 | `sigma` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3258 | `sigma` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3259 | `sigma` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3260 | `sigma` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3261 | `sigma` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3262 | `sigma` / scale-1e+150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3264 | `sigma` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3265 | `sigma` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3267 | `mu,sigma` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3268 | `mu,sigma` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3270 | `mu,sigma` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3277 | `mu,sigma` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3278 | `mu,sigma` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3279 | `mu,sigma` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3280 | `mu,sigma` / lower-1e-200 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3281 | `mu,sigma` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3282 | `mu,sigma` / scale-1e+150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3283 | `mu,sigma` / offset-1e8 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3284 | `mu,sigma` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 9376 | `free` / face-half-normal-100 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9377 | `mu` / face-half-normal-100 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9380 | `sigma` / face-half-normal-100 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9381 | `mu,sigma` / face-half-normal-100 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9836 | `free` / face-half-normal-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9837 | `mu` / face-half-normal-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9840 | `sigma` / face-half-normal-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9841 | `mu,sigma` / face-half-normal-1e+06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10454 | `free` / constant-extra-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10455 | `free` / constant-extra-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10456 | `free` / constant-extra-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10457 | `mu` / constant-extra-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10458 | `mu` / constant-extra-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10459 | `mu` / constant-extra-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10466 | `sigma` / constant-extra-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10467 | `sigma` / constant-extra-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10468 | `sigma` / constant-extra-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10469 | `mu,sigma` / constant-extra-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10470 | `mu,sigma` / constant-extra-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10471 | `mu,sigma` / constant-extra-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |

## huber

With location, scale and shape free, a finite MLE need not exist: the supremum can lie at a normal or Laplace limit, and constant data permit unbounded scale collapse. No general uniqueness theorem is asserted. With shape held, a finite maximum is attained for nonconstant data, including ties. For constant data it is attained when scale is held or location is held away from the observation. With shape free, held location and scale are respected in the eligible limit comparisons; the general conditional classification remains numerical.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 3326 | `free` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3327 | `free` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3328 | `free` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3329 | `free` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3330 | `free` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3331 | `free` / constant-0.456413-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3332 | `free` / constant-0.456413-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3333 | `free` / constant-0.456413-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3334 | `free` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3335 | `free` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3336 | `free` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3337 | `free` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3338 | `free` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3339 | `free` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3340 | `free` / limit-outlier | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3341 | `free` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3342 | `mu` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3343 | `mu` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3344 | `mu` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3345 | `mu` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3346 | `mu` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3347 | `mu` / constant-0.456413-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3348 | `mu` / constant-0.456413-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3349 | `mu` / constant-0.456413-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3350 | `mu` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3351 | `mu` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3352 | `mu` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3353 | `mu` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3354 | `mu` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3355 | `mu` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3356 | `mu` / limit-outlier | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3357 | `mu` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3358 | `sigma` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3359 | `sigma` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3360 | `sigma` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3361 | `sigma` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3362 | `sigma` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3363 | `sigma` / constant-0.456413-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3364 | `sigma` / constant-0.456413-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3365 | `sigma` / constant-0.456413-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3366 | `sigma` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3367 | `sigma` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3368 | `sigma` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3369 | `sigma` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3371 | `sigma` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3372 | `sigma` / limit-outlier | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 3373 | `sigma` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3374 | `mu,sigma` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3375 | `mu,sigma` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3376 | `mu,sigma` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3377 | `mu,sigma` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3378 | `mu,sigma` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3379 | `mu,sigma` / constant-0.456413-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3380 | `mu,sigma` / constant-0.456413-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3381 | `mu,sigma` / constant-0.456413-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3382 | `mu,sigma` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3383 | `mu,sigma` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3384 | `mu,sigma` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3385 | `mu,sigma` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3386 | `mu,sigma` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 3387 | `mu,sigma` / offset-1e8 | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 3388 | `mu,sigma` / limit-outlier | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 3389 | `mu,sigma` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9738 | `free` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9739 | `mu` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9740 | `sigma` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9741 | `mu,sigma` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10198 | `free` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10199 | `mu` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10200 | `sigma` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10201 | `mu,sigma` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |

## hyper

With all three counts free, constant samples attain likelihood one. No complete sample criterion for existence or uniqueness is asserted for nonconstant samples. With $N$ held, every feasible sample has a finite maximum, although distinct count vectors may tie. With $K$ and $n$ held positive and only $N$ free, a finite maximum exists exactly when some observation is positive. With either held count zero, compatible all-zero data leave $N$ unidentified. With only $K$ or only $n$ held, a candidate strictly above the corresponding binomial supremum ensures finite attainment. With all counts held, there is only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 3454 | `free` / interior | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3456 | `free` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3457 | `free` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3458 | `free` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3465 | `free` / large-count | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 3466 | `free` / zero-but-one | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 3467 | `free` / constant-count | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 3493 | `N` / large-count | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 3495 | `N` / constant-count | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 3525 | `m,k` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10971 | `free` / support-upper-count | 9 | open: finite-MLE existence not inferred from a finite candidate |

## invburr

With all coefficients free, constant positive data have unbounded likelihood. For nonconstant data, the inverse Weibull and power limits can compete with finite fits. Each accepted finite fit is certified unique and global for that sample; each accepted limit is certified globally optimal. A universal joint sample criterion and universal joint uniqueness theorem are not established. With only scale $s$ held, a unique finite maximum exists for nonconstant data if some observation is above $s$. Constant data above $s$ have unbounded likelihood. If all observations are at most $s$, the supremum is on the power path, except that all observations equal to $s$ give unbounded likelihood. Holding $k$ gives a unique finite fit for nonconstant data, but constant data remain unbounded when $c$ and $s$ are free. Holding $c$ reduces existence to the Lomax criterion on $x_i^{-c}$, with a possible inverse Weibull limit. Holding both shapes gives a unique scale estimate for any positive sample. Holding $k$ and $s$ gives a unique finite $c$ estimate unless all observations equal $s$. Holding $c$ and $s$ gives a unique finite $k$ estimate. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 3563 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3564 | `free` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3565 | `free` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3567 | `free` / constant-1.12109-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3568 | `free` / constant-1.12109-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3569 | `free` / constant-1.12109-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3570 | `free` / constant-1.12109-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3571 | `free` / constant-1.12109-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3572 | `free` / constant-1.12109-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3573 | `free` / lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3574 | `free` / lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3575 | `free` / lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3576 | `free` / lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3579 | `free` / limit-outlier | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3580 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 3601 | `shape2` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3605 | `shape2` / constant-1.12109-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3606 | `shape2` / constant-1.12109-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3607 | `shape2` / constant-1.12109-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3608 | `shape2` / constant-1.12109-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3609 | `shape2` / constant-1.12109-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3610 | `shape2` / constant-1.12109-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3618 | `shape2` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3643 | `scale` / constant-1.12109-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3644 | `scale` / constant-1.12109-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3645 | `scale` / constant-1.12109-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3646 | `scale` / constant-1.12109-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3647 | `scale` / constant-1.12109-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3648 | `scale` / constant-1.12109-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3649 | `scale` / lower-1e-06 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3650 | `scale` / lower-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3651 | `scale` / lower-1e-17 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3652 | `scale` / lower-1e-200 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3653 | `scale` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3654 | `scale` / scale-1e+150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 3656 | `scale` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 3692 | `shape2,scale` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9428 | `free` / face-inverse-weibull-100 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 9888 | `free` / face-inverse-weibull-1e+06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10478 | `free` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10479 | `free` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10480 | `free` / constant-extra-1-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10484 | `shape2` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10485 | `shape2` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10486 | `shape2` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10490 | `scale` / constant-extra-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10491 | `scale` / constant-extra-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10492 | `scale` / constant-extra-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |

## invparalogis

With both coefficients free, constant positive samples have unbounded likelihood. A finite maximum exists for nonconstant samples, but joint uniqueness is not established. Holding scale gives a finite shape maximum for every positive sample, including constant data. Uniqueness of this conditional shape maximum is not established here. Holding shape gives a unique finite scale maximum for every positive sample. Holding both leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 3913 | `free` / constant-1.06813-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3914 | `free` / constant-1.06813-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3915 | `free` / constant-1.06813-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3916 | `free` / constant-1.06813-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3917 | `free` / constant-1.06813-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3918 | `free` / constant-1.06813-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3924 | `free` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 3926 | `free` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10532 | `free` / constant-extra-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10533 | `free` / constant-extra-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10534 | `free` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |

## invpareto

With both coefficients free, a finite MLE exists exactly when the Lomax MLE for the reciprocals $1/x_i$ exists. Otherwise the supremum is the inverse exponential limit. Joint uniqueness is not established. Holding either shape or scale gives a unique finite MLE for every nonempty positive sample, including constant data.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 3984 | `free` / interior | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3985 | `free` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3986 | `free` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3987 | `free` / tiny-5 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3988 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3989 | `free` / constant-1.93501-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3990 | `free` / constant-1.93501-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3991 | `free` / constant-1.93501-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3992 | `free` / constant-1.93501-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3993 | `free` / constant-1.93501-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3994 | `free` / constant-1.93501-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3995 | `free` / lower-1e-06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3996 | `free` / lower-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3997 | `free` / lower-1e-17 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3998 | `free` / lower-1e-200 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 3999 | `free` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4000 | `free` / scale-1e+150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4002 | `free` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10544 | `free` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10545 | `free` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10546 | `free` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |

## invtrgamma

With all coefficients free, constant positive samples have unbounded likelihood. For nonconstant samples, the lognormal or Pareto limit can prevent a finite MLE. General uniqueness of an interior maximum is not established. With scale free and either shape held, a unique finite MLE exists for nonconstant samples. With both shapes held, the scale MLE is unique even for constant samples. With scale and power held, the remaining gamma shape has a unique finite MLE. With scale and gamma shape held, the power MLE is unique unless every observation equals the held scale, when likelihood is unbounded. With only scale held, the reciprocal transformed-gamma certificate assesses attainment against the fixed-endpoint Pareto limit. Constant samples below scale have unbounded likelihood when both shapes are free.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 4060 | `free` / interior | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4061 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4062 | `free` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4063 | `free` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4064 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4065 | `free` / constant-0.217779-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4066 | `free` / constant-0.217779-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4067 | `free` / constant-0.217779-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4068 | `free` / constant-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4069 | `free` / constant-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4070 | `free` / constant-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4071 | `free` / lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4072 | `free` / lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4073 | `free` / lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4074 | `free` / lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4075 | `free` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4076 | `free` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4077 | `free` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4078 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 4085 | `shape1` / constant-0.217779-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4086 | `shape1` / constant-0.217779-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4097 | `shape1` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4104 | `shape2` / constant-0.217779-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4105 | `shape2` / constant-0.217779-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4107 | `shape2` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4108 | `shape2` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4116 | `shape2` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4141 | `scale` / constant-0.217779-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4142 | `scale` / constant-0.217779-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4143 | `scale` / constant-0.217779-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4144 | `scale` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4145 | `scale` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4146 | `scale` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4148 | `scale` / lower-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4149 | `scale` / lower-1e-17 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4150 | `scale` / lower-1e-200 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4151 | `scale` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4154 | `scale` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4188 | `shape2,scale` / lower-1e-200 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4189 | `shape2,scale` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |

## lomax

With both coefficients free, a finite MLE exists exactly when $\psi(u) \le 0$ for some $0 < u < \bar u$, with $\psi$ and $\bar u$ defined below. Otherwise the supremum is the exponential limit. Joint uniqueness is not established. Holding either coefficient gives a unique finite MLE for every nonempty positive sample, including constant data.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 4817 | `free` / tiny-5 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4819 | `free` / constant-0.802521-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4820 | `free` / constant-0.802521-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4821 | `free` / constant-0.802521-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4822 | `free` / constant-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4823 | `free` / constant-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4824 | `free` / constant-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4825 | `free` / lower-1e-06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4826 | `free` / lower-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4827 | `free` / lower-1e-17 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4828 | `free` / lower-1e-200 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 4832 | `free` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |

## lst

With all coefficients free, fitting is restricted to $\nu\ge1$; a finite MLE is not guaranteed because scale collapse or the normal limit can intervene. Global uniqueness is unknown. Holding positive $\nu$ removes the normal escape; collapse has exponent $\nu(n-k)-k$ at a location with $k$ ties. With $\mu$ and $\nu$ held, a unique positive scale maximum exists exactly when $\nu(n-k)>k$; equality gives a finite unattained scale-zero supremum. Holding $\mu$ exempts estimated $\nu$ from the lower fitting bound, and any observation at that location then makes the free-scale, free-$\nu$ likelihood unbounded. Holding $\sigma$ removes scale collapse. Remaining location and shape searches have no general uniqueness proof. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 4891 | `free` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4892 | `free` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4894 | `free` / ties | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 4895 | `free` / constant-0.404631-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4896 | `free` / constant-0.404631-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4897 | `free` / constant-0.404631-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 4898 | `free` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4899 | `free` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4900 | `free` / constant-1-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 4903 | `free` / offset-1e8 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 4904 | `free` / limit-outlier | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 4905 | `free` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4907 | `mu` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4908 | `mu` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4910 | `mu` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4911 | `mu` / constant-0.404631-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4912 | `mu` / constant-0.404631-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4913 | `mu` / constant-0.404631-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4914 | `mu` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4915 | `mu` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4916 | `mu` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4917 | `mu` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4919 | `mu` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4921 | `mu` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4923 | `sigma` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4924 | `sigma` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4925 | `sigma` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4926 | `sigma` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4927 | `sigma` / constant-0.404631-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4928 | `sigma` / constant-0.404631-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4929 | `sigma` / constant-0.404631-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4930 | `sigma` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4931 | `sigma` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4932 | `sigma` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4933 | `sigma` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4934 | `sigma` / scale-1e+150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 4935 | `sigma` / offset-1e8 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 4936 | `sigma` / limit-outlier | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 4937 | `sigma` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4939 | `mu,sigma` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4940 | `mu,sigma` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4942 | `mu,sigma` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4943 | `mu,sigma` / constant-0.404631-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4944 | `mu,sigma` / constant-0.404631-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4945 | `mu,sigma` / constant-0.404631-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4946 | `mu,sigma` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4947 | `mu,sigma` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4948 | `mu,sigma` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4949 | `mu,sigma` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 4953 | `mu,sigma` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9368 | `free` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9369 | `mu` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9370 | `sigma` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9371 | `mu,sigma` / face-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9828 | `free` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9829 | `mu` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9830 | `sigma` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9831 | `mu,sigma` / face-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |

## nbinom

With both coefficients free and at least one positive count, a finite MLE exists and is unique if and only if $S^2>\bar x$, where $S^2=n^{-1}\sum_i(x_i-\bar x)^2$. Otherwise the supremum is the Poisson limit. For all-zero data, the maximum is attained by every pair with $r=0$ or $p=1$, so the coefficients are not unique. With $r>0$ held, the free probability has a unique MLE, including $p=1$ for all-zero data. With $0<p<1$ held, the free size has a unique MLE, including $r=0$ for all-zero data. Held $r=0$ or $p=1$ permits only zeros and leaves the other free coefficient unidentified. Holding both coefficients leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5095 | `free` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5096 | `free` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5098 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5099 | `free` / constant-15-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5100 | `free` / constant-15-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5101 | `free` / constant-15-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5102 | `free` / constant-15-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5103 | `free` / constant-15-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5104 | `free` / constant-15-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5105 | `free` / large-count | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5106 | `free` / zero-but-one | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5107 | `free` / constant-count | 1 | open: finite-MLE existence not inferred from a finite candidate |

## nhyper

With all counts free, constant samples attain likelihood one, while mixed zero and positive observations are infeasible. No complete existence or uniqueness criterion is asserted for positive nonconstant samples. With $N$ held, every feasible sample has a finite maximum. With $K$ held, every feasible sample also has a finite maximum, whether the stopping count is held or estimated. Ties can occur. With only positive $r$ held, a candidate strictly above the shifted negative-binomial supremum ensures finite attainment. With $r=0$, only zero observations are possible and composition is unidentified. Holding all counts specifies the law without optimization.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5161 | `free` / large-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5162 | `free` / zero-but-one | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5163 | `free` / constant-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5175 | `m` / large-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5176 | `m` / zero-but-one | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5189 | `N` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5190 | `N` / zero-but-one | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5191 | `N` / constant-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5203 | `m,N` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5204 | `m,N` / zero-but-one | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5205 | `m,N` / constant-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5217 | `r` / large-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5230 | `m,r` / large-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5243 | `N,r` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5244 | `N,r` / constant-count | 4 | open: finite-MLE existence not inferred from a finite candidate |

## paralogis

With both coefficients free, constant positive samples have unbounded likelihood. A finite maximum exists for nonconstant samples, but joint uniqueness is not established. Holding scale gives a finite shape maximum for every positive sample, including constant data. Uniqueness of this conditional shape maximum is not established here. Holding shape gives a unique finite scale maximum for every positive sample. Holding both leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5387 | `free` / constant-0.951757-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5388 | `free` / constant-0.951757-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5389 | `free` / constant-0.951757-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5390 | `free` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5391 | `free` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5392 | `free` / constant-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5394 | `free` / lower-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5395 | `free` / lower-1e-17 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5396 | `free` / lower-1e-200 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5397 | `free` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5400 | `free` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |

## pareto2

With all coefficients free, the likelihood is unbounded for every nonempty sample. With $\mu$ held and all observations strictly above it, existence is exactly the Lomax existence criterion on the excesses. The exponential limit may supply the supremum, and joint uniqueness is not established. Holding either shape or scale as well gives a unique finite estimate of the remaining coefficient. Free-threshold fits with held coefficients are not supported, although some have finite maxima. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5525 | `min` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5526 | `min` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5528 | `min` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5529 | `min` / constant-3.0498-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5530 | `min` / constant-3.0498-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5531 | `min` / constant-3.0498-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5532 | `min` / constant-3.0498-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5533 | `min` / constant-3.0498-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5534 | `min` / constant-3.0498-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5538 | `min` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10616 | `min` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10617 | `min` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10618 | `min` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10814 | `min` / support-lower-1e-06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10815 | `min` / support-lower-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10816 | `min` / support-lower-1e-17 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10817 | `min` / support-lower-1e-200 | 1 | open: finite-MLE existence not inferred from a finite candidate |

## pareto4

With all coefficients free, every nonempty sample has unbounded likelihood. Holding $\mu$ gives the Burr likelihood on the excesses. Constant excesses have unbounded joint likelihood, while nonconstant samples may favour Weibull or Pareto limits. The finite/face existence criterion is inherited from Burr; general joint uniqueness is not established, but accepted finite certificates prove it for the sample. Holding $\alpha$ gives a unique finite fit on nonconstant excesses. Holding $\gamma$ reduces existence to Lomax on powered excesses. Holding both shapes gives a unique scale estimate. Holding scale alone gives a unique finite maximum for nonconstant excesses if some excess is below it, by strict concavity of the shape profile. Other conditional cases are described below. Free-threshold fits are not supported. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5644 | `min` / interior | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5645 | `min` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5646 | `min` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5647 | `min` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5648 | `min` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5649 | `min` / constant-57.9572-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5650 | `min` / constant-57.9572-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5651 | `min` / constant-57.9572-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 5652 | `min` / constant-57.9572-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5653 | `min` / constant-57.9572-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5654 | `min` / constant-57.9572-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 5655 | `min` / scale-1e-150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5656 | `min` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5657 | `min` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5658 | `min` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 5675 | `shape2,min` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5679 | `shape2,min` / constant-57.9572-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5680 | `shape2,min` / constant-57.9572-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5681 | `shape2,min` / constant-57.9572-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5682 | `shape2,min` / constant-57.9572-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5683 | `shape2,min` / constant-57.9572-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5684 | `shape2,min` / constant-57.9572-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5688 | `shape2,min` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5705 | `scale,min` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5709 | `scale,min` / constant-57.9572-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5710 | `scale,min` / constant-57.9572-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5711 | `scale,min` / constant-57.9572-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5712 | `scale,min` / constant-57.9572-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5713 | `scale,min` / constant-57.9572-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5714 | `scale,min` / constant-57.9572-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5715 | `scale,min` / scale-1e-150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 5716 | `scale,min` / scale-1e+150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 5718 | `scale,min` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 10628 | `min` / constant-extra-1-0.0001 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10629 | `min` / constant-extra-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10630 | `min` / constant-extra-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10634 | `shape2,min` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10635 | `shape2,min` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10636 | `shape2,min` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10640 | `scale,min` / constant-extra-1-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10641 | `scale,min` / constant-extra-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10642 | `scale,min` / constant-extra-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 10846 | `min` / support-lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10847 | `min` / support-lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10848 | `min` / support-lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10849 | `min` / support-lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 10854 | `shape2,min` / support-lower-1e-06 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10855 | `shape2,min` / support-lower-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10856 | `shape2,min` / support-lower-1e-17 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10857 | `shape2,min` / support-lower-1e-200 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10862 | `scale,min` / support-lower-1e-06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10863 | `scale,min` / support-lower-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10864 | `scale,min` / support-lower-1e-17 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 10865 | `scale,min` / support-lower-1e-200 | 9 | open: finite-MLE existence not inferred from a finite candidate |

## pig

With both coefficients free and positive sample mean, a finite MLE exists and is unique exactly when the variance with divisor $n$ exceeds the sample mean. Otherwise the Poisson supremum is unattained. With positive mean $\mu$ held and any positive count, the finite shape maximum exists and is unique exactly when $v+(\bar x-\mu)^2>\bar x$; equality also selects the unattained Poisson supremum. With positive shape held and any positive count, the mean maximum always exists and is unique. All-zero data with free shape attain the zero mass at $\lambda=0$, leaving a free mean unidentified; positive held shape instead gives an unattained mean-zero supremum. With $\lambda=0$ held only all-zero data are compatible, and a free mean is unidentified. Holding both coefficients leaves evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5765 | `free` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5766 | `free` / tiny-3 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5768 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5769 | `free` / constant-2-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5770 | `free` / constant-2-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5771 | `free` / constant-2-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5772 | `free` / constant-2-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5773 | `free` / constant-2-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5774 | `free` / constant-2-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5775 | `free` / large-count | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5776 | `free` / zero-but-one | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5777 | `free` / constant-count | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5783 | `mean` / constant-2-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5784 | `mean` / constant-2-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5785 | `mean` / constant-2-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5786 | `mean` / constant-2-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5787 | `mean` / constant-2-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5788 | `mean` / constant-2-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 5789 | `mean` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 5803 | `shape` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |

## sged

With all coefficients free, estimated shape is restricted to $\nu\ge1$; constant data permit scale collapse, and uniform or translated half-GED limits can prevent a finite maximum. Without the fitting restriction, the all-free likelihood is unbounded for every sample. Global existence and uniqueness in the restricted domain outside the stated cases are unresolved. Holding coefficients retains only reachable limiting faces and the corresponding moment constraints. With both shapes held and location free, the location/scale maximum is unique for nu > 1 unless free scale collapses on constant data; nu <= 1 permits ties. These subsets now use a global certificate. With $\mu$ and both shapes held, a scale maximum exists unless every observation equals that location; the scale maximum is unique for nu >= 1 by strict convexity in inverse scale, and for every positive nu when xi=1; the asymmetric scale search below one remains uncertified. With $\sigma$ and both shapes held, a location maximum exists, including for tied samples. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 5966 | `free` / interior | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5967 | `free` / tiny-2 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5968 | `free` / tiny-3 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5969 | `free` / tiny-5 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5970 | `free` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5971 | `free` / constant-0.106436-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5972 | `free` / constant-0.106436-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5973 | `free` / constant-0.106436-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5974 | `free` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5975 | `free` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5976 | `free` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5977 | `free` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5978 | `free` / scale-1e+150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5979 | `free` / offset-1e8 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5980 | `free` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 5981 | `free` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 5982 | `mean` / interior | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 5983 | `mean` / tiny-2 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5984 | `mean` / tiny-3 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5985 | `mean` / tiny-5 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5986 | `mean` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5987 | `mean` / constant-0.106436-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5988 | `mean` / constant-0.106436-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5989 | `mean` / constant-0.106436-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5990 | `mean` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5991 | `mean` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5992 | `mean` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5993 | `mean` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5994 | `mean` / scale-1e+150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5995 | `mean` / offset-1e8 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5997 | `mean` / limit-concentration | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 5998 | `sd` / interior | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 5999 | `sd` / tiny-2 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6000 | `sd` / tiny-3 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6001 | `sd` / tiny-5 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6002 | `sd` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6003 | `sd` / constant-0.106436-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6004 | `sd` / constant-0.106436-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6005 | `sd` / constant-0.106436-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6006 | `sd` / constant-1-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6007 | `sd` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6008 | `sd` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6009 | `sd` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6011 | `sd` / offset-1e8 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6012 | `sd` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6013 | `sd` / limit-concentration | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6014 | `mean,sd` / interior | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6022 | `mean,sd` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6023 | `mean,sd` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6024 | `mean,sd` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6027 | `mean,sd` / offset-1e8 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6028 | `mean,sd` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6030 | `nu` / interior | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6031 | `nu` / tiny-2 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6032 | `nu` / tiny-3 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6033 | `nu` / tiny-5 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6034 | `nu` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6035 | `nu` / constant-0.106436-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6036 | `nu` / constant-0.106436-1e-08 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6037 | `nu` / constant-0.106436-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6038 | `nu` / constant-1-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6039 | `nu` / constant-1-1e-08 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6040 | `nu` / constant-1-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6041 | `nu` / scale-1e-150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6042 | `nu` / scale-1e+150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6043 | `nu` / offset-1e8 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6044 | `nu` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6045 | `nu` / limit-concentration | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6046 | `mean,nu` / interior | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6047 | `mean,nu` / tiny-2 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6048 | `mean,nu` / tiny-3 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6049 | `mean,nu` / tiny-5 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6050 | `mean,nu` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6051 | `mean,nu` / constant-0.106436-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6052 | `mean,nu` / constant-0.106436-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6053 | `mean,nu` / constant-0.106436-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6054 | `mean,nu` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6055 | `mean,nu` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6056 | `mean,nu` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6057 | `mean,nu` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6058 | `mean,nu` / scale-1e+150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6059 | `mean,nu` / offset-1e8 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6060 | `mean,nu` / limit-outlier | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6061 | `mean,nu` / limit-concentration | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6062 | `sd,nu` / interior | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6063 | `sd,nu` / tiny-2 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6064 | `sd,nu` / tiny-3 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6065 | `sd,nu` / tiny-5 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6066 | `sd,nu` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6067 | `sd,nu` / constant-0.106436-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6068 | `sd,nu` / constant-0.106436-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6069 | `sd,nu` / constant-0.106436-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6070 | `sd,nu` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6071 | `sd,nu` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6072 | `sd,nu` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6073 | `sd,nu` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6074 | `sd,nu` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6075 | `sd,nu` / offset-1e8 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6076 | `sd,nu` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6077 | `sd,nu` / limit-concentration | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6079 | `mean,sd,nu` / tiny-2 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6080 | `mean,sd,nu` / tiny-3 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6081 | `mean,sd,nu` / tiny-5 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6082 | `mean,sd,nu` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6083 | `mean,sd,nu` / constant-0.106436-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6084 | `mean,sd,nu` / constant-0.106436-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6085 | `mean,sd,nu` / constant-0.106436-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6086 | `mean,sd,nu` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6087 | `mean,sd,nu` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6088 | `mean,sd,nu` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6089 | `mean,sd,nu` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6090 | `mean,sd,nu` / scale-1e+150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6091 | `mean,sd,nu` / offset-1e8 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6092 | `mean,sd,nu` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6093 | `mean,sd,nu` / limit-concentration | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6094 | `xi` / interior | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6095 | `xi` / tiny-2 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6096 | `xi` / tiny-3 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6097 | `xi` / tiny-5 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6098 | `xi` / ties | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6099 | `xi` / constant-0.106436-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6100 | `xi` / constant-0.106436-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6101 | `xi` / constant-0.106436-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6102 | `xi` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6103 | `xi` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6104 | `xi` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6105 | `xi` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6106 | `xi` / scale-1e+150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6107 | `xi` / offset-1e8 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6109 | `xi` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6110 | `mean,xi` / interior | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6111 | `mean,xi` / tiny-2 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6112 | `mean,xi` / tiny-3 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6113 | `mean,xi` / tiny-5 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6114 | `mean,xi` / ties | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6115 | `mean,xi` / constant-0.106436-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6116 | `mean,xi` / constant-0.106436-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6117 | `mean,xi` / constant-0.106436-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6118 | `mean,xi` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6119 | `mean,xi` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6120 | `mean,xi` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6121 | `mean,xi` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6122 | `mean,xi` / scale-1e+150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6123 | `mean,xi` / offset-1e8 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6125 | `mean,xi` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |

## sgomp

With both coefficients free, a finite MLE exists for every nonconstant nonnegative sample. Constant samples have unbounded likelihood. Holding $b$ gives a unique finite estimate of $\eta$, possibly zero. Holding $\eta$ gives a unique finite estimate of $b$ exactly when the sample sum is positive. An all-zero sample has unbounded likelihood when $b$ is free. Joint uniqueness is not established. With $\eta=0$ held and positive sample sum, the unique rate estimate is the exponential estimate. Holding both leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 6227 | `free` / constant-2.0083-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6228 | `free` / constant-2.0083-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6229 | `free` / constant-2.0083-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6230 | `free` / constant-2.0083-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6231 | `free` / constant-2.0083-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6232 | `free` / constant-2.0083-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6240 | `free` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6257 | `b` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10658 | `free` / constant-extra-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10659 | `free` / constant-extra-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10660 | `free` / constant-extra-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |

## slash

With location and scale free, an MLE exists if every value occurs fewer than half the time. A value occurring more than half the time makes likelihood unbounded. At exactly half, attainment depends on comparison with the finite collapse limits described below. Uniqueness is not guaranteed. With $\mu$ held and scale free, the scale maximum is unique if fewer than half the observations equal $\mu$; equality gives an unattained supremum and more than half gives divergence. Holding $\sigma>0$ guarantees attainment, but location can have multiple maxima.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 6356 | `free` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6358 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6398 | `sigma` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |

## sn

With all three coefficients free, a finite MLE need not exist: the supremum can lie at either half-normal limit. A finite point strictly above both limiting suprema ensures attainment for nonconstant data, but uniqueness of the joint fit is not established here. Constant data have unbounded likelihood when location and scale are free. Holding scale still permits a half-normal escape when shape is free. With a finite shape held, the remaining fit exists and is unique for nonconstant data. With location also held, this remains true whenever at least one observation differs from that location. With shape and scale held, the location fit always exists and is unique. With location held and shape free, observations strictly on both sides of the held location give a unique finite fit, whether scale is held or free. One-sided observations with at least one strict inequality instead give an infinite-shape supremum. If all observations equal the held location, free scale collapses to zero; with scale held, every shape has the same likelihood. Holding all coefficients leaves no estimation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 6418 | `free` / interior | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6419 | `free` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6420 | `free` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6421 | `free` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6422 | `free` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6423 | `free` / constant-0.640976-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6424 | `free` / constant-0.640976-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6425 | `free` / constant-0.640976-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6426 | `free` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6427 | `free` / constant-1-1e-08 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6428 | `free` / constant-1-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6429 | `free` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6430 | `free` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6431 | `free` / offset-1e8 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6432 | `free` / limit-outlier | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6433 | `free` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6435 | `xi` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6436 | `xi` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6438 | `xi` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6439 | `xi` / constant-0.640976-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6440 | `xi` / constant-0.640976-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6441 | `xi` / constant-0.640976-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6442 | `xi` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6443 | `xi` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6444 | `xi` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6445 | `xi` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6447 | `xi` / offset-1e8 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6449 | `xi` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6450 | `omega` / interior | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6451 | `omega` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6452 | `omega` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6453 | `omega` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6454 | `omega` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6455 | `omega` / constant-0.640976-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6456 | `omega` / constant-0.640976-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6457 | `omega` / constant-0.640976-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6458 | `omega` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6459 | `omega` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6460 | `omega` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6461 | `omega` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6462 | `omega` / scale-1e+150 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6463 | `omega` / offset-1e8 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6464 | `omega` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6465 | `omega` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6467 | `xi,omega` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6468 | `xi,omega` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6470 | `xi,omega` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6471 | `xi,omega` / constant-0.640976-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6472 | `xi,omega` / constant-0.640976-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6473 | `xi,omega` / constant-0.640976-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6474 | `xi,omega` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6475 | `xi,omega` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6476 | `xi,omega` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6477 | `xi,omega` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6479 | `xi,omega` / offset-1e8 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6480 | `xi,omega` / limit-outlier | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6481 | `xi,omega` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9384 | `free` / face-half-normal-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9386 | `omega` / face-half-normal-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9392 | `free` / face-reflected-half-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9393 | `xi` / face-reflected-half-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9394 | `omega` / face-reflected-half-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9395 | `xi,omega` / face-reflected-half-normal-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9844 | `free` / face-half-normal-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9846 | `omega` / face-half-normal-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9852 | `free` / face-reflected-half-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9853 | `xi` / face-reflected-half-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9854 | `omega` / face-reflected-half-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9855 | `xi,omega` / face-reflected-half-normal-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |

## snorm

With all coefficients free, nonconstant samples have a finite MLE exactly when the two-piece profile described below attains its global minimum at an interior mode. Otherwise the supremum is a translated half-normal limit. Constant samples have unbounded likelihood. Joint uniqueness is not established. Holding $\xi=1$ gives the usual unique normal MLE whenever scale does not collapse. For other held subsets, half-normal limits must respect the held moments, and no general existence or uniqueness classification is asserted here.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 6546 | `free` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6547 | `free` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6548 | `free` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6549 | `free` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6550 | `free` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6551 | `free` / constant--0.140046-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6552 | `free` / constant--0.140046-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6553 | `free` / constant--0.140046-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6554 | `free` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6555 | `free` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6556 | `free` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6557 | `free` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6558 | `free` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6559 | `free` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6560 | `free` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6561 | `free` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6562 | `mean` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6563 | `mean` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6564 | `mean` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6565 | `mean` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6566 | `mean` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6567 | `mean` / constant--0.140046-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6568 | `mean` / constant--0.140046-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6569 | `mean` / constant--0.140046-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6570 | `mean` / constant-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6571 | `mean` / constant-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6572 | `mean` / constant-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6573 | `mean` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6574 | `mean` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6575 | `mean` / offset-1e8 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6576 | `mean` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6577 | `mean` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6578 | `sd` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6579 | `sd` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6580 | `sd` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6581 | `sd` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6582 | `sd` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6583 | `sd` / constant--0.140046-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6584 | `sd` / constant--0.140046-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6585 | `sd` / constant--0.140046-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6586 | `sd` / constant-1-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6587 | `sd` / constant-1-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6588 | `sd` / constant-1-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6589 | `sd` / scale-1e-150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6590 | `sd` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6591 | `sd` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6592 | `sd` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6593 | `sd` / limit-concentration | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6594 | `mean,sd` / interior | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6595 | `mean,sd` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6596 | `mean,sd` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6597 | `mean,sd` / tiny-5 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6598 | `mean,sd` / ties | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6602 | `mean,sd` / constant-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6603 | `mean,sd` / constant-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6604 | `mean,sd` / constant-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6607 | `mean,sd` / offset-1e8 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6608 | `mean,sd` / limit-outlier | 1 | open: finite-MLE existence not inferred from a finite candidate |

## sstd

With all coefficients free, scale collapse or a skew-normal, half-t, half-normal or two-piece $t_2$ limit can prevent a finite maximum. Global uniqueness is unknown, and the implementation does not certify the full search. The uniform closure theorem exhausts joint escapes in the strict tie regime $3k_{max}<2n$, but its quantitative face profilers are not implemented. Holding coefficients restricts the reachable faces by their moment constraints. Holding $\nu$ removes shape escapes, holding $\xi$ removes skew escapes, and holding $\sigma$ excludes a finite-scale $t_2$ face but does not exclude collapse when $\nu$ is free. With $\mu$ and both shapes held, a positive scale maximum exists when $\nu(n-k)>k$, where $k$ counts observations at that location; the reverse strict inequality gives unbounded likelihood, and equality is an unattained finite supremum at $\xi=1$ and remains numerically unresolved otherwise. With $\sigma$ and both shapes held, a location maximum exists even with ties. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 6674 | `free` / interior | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6675 | `free` / tiny-2 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6676 | `free` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6677 | `free` / tiny-5 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6678 | `free` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6679 | `free` / constant--0.106887-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6680 | `free` / constant--0.106887-1e-08 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6681 | `free` / constant--0.106887-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6682 | `free` / constant-1-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6683 | `free` / constant-1-1e-08 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6684 | `free` / constant-1-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 6685 | `free` / scale-1e-150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6686 | `free` / scale-1e+150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6687 | `free` / offset-1e8 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6688 | `free` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6689 | `free` / limit-concentration | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6690 | `mean` / interior | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6691 | `mean` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6692 | `mean` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6693 | `mean` / tiny-5 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6694 | `mean` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6695 | `mean` / constant--0.106887-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6696 | `mean` / constant--0.106887-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6697 | `mean` / constant--0.106887-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6698 | `mean` / constant-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6699 | `mean` / constant-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6700 | `mean` / constant-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6701 | `mean` / scale-1e-150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6702 | `mean` / scale-1e+150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6703 | `mean` / offset-1e8 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6704 | `mean` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6705 | `mean` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6706 | `sd` / interior | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6707 | `sd` / tiny-2 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6708 | `sd` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6709 | `sd` / tiny-5 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6710 | `sd` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6712 | `sd` / constant--0.106887-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6713 | `sd` / constant--0.106887-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6714 | `sd` / constant-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6715 | `sd` / constant-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6716 | `sd` / constant-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6717 | `sd` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6718 | `sd` / scale-1e+150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6719 | `sd` / offset-1e8 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6720 | `sd` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6722 | `mean,sd` / interior | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6723 | `mean,sd` / tiny-2 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6724 | `mean,sd` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6725 | `mean,sd` / tiny-5 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6726 | `mean,sd` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6727 | `mean,sd` / constant--0.106887-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6728 | `mean,sd` / constant--0.106887-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6729 | `mean,sd` / constant--0.106887-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6730 | `mean,sd` / constant-1-0.0001 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6731 | `mean,sd` / constant-1-1e-08 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6732 | `mean,sd` / constant-1-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6733 | `mean,sd` / scale-1e-150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6735 | `mean,sd` / offset-1e8 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6737 | `mean,sd` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6738 | `nu` / interior | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6739 | `nu` / tiny-2 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6740 | `nu` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6741 | `nu` / tiny-5 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6742 | `nu` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6743 | `nu` / constant--0.106887-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6744 | `nu` / constant--0.106887-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6745 | `nu` / constant--0.106887-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6746 | `nu` / constant-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6747 | `nu` / constant-1-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6748 | `nu` / constant-1-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6749 | `nu` / scale-1e-150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6750 | `nu` / scale-1e+150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6751 | `nu` / offset-1e8 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6752 | `nu` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6753 | `nu` / limit-concentration | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6754 | `mean,nu` / interior | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6755 | `mean,nu` / tiny-2 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6756 | `mean,nu` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6757 | `mean,nu` / tiny-5 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6758 | `mean,nu` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6759 | `mean,nu` / constant--0.106887-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6760 | `mean,nu` / constant--0.106887-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6761 | `mean,nu` / constant--0.106887-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6762 | `mean,nu` / constant-1-0.0001 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6763 | `mean,nu` / constant-1-1e-08 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6764 | `mean,nu` / constant-1-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6765 | `mean,nu` / scale-1e-150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6766 | `mean,nu` / scale-1e+150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6767 | `mean,nu` / offset-1e8 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6768 | `mean,nu` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6769 | `mean,nu` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6770 | `sd,nu` / interior | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6771 | `sd,nu` / tiny-2 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6772 | `sd,nu` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6773 | `sd,nu` / tiny-5 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6774 | `sd,nu` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6775 | `sd,nu` / constant--0.106887-0.0001 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6776 | `sd,nu` / constant--0.106887-1e-08 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6777 | `sd,nu` / constant--0.106887-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6778 | `sd,nu` / constant-1-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6779 | `sd,nu` / constant-1-1e-08 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6780 | `sd,nu` / constant-1-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6781 | `sd,nu` / scale-1e-150 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6782 | `sd,nu` / scale-1e+150 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6783 | `sd,nu` / offset-1e8 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6784 | `sd,nu` / limit-outlier | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6785 | `sd,nu` / limit-concentration | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 6787 | `mean,sd,nu` / tiny-2 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6788 | `mean,sd,nu` / tiny-3 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6790 | `mean,sd,nu` / ties | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6799 | `mean,sd,nu` / offset-1e8 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6803 | `xi` / tiny-2 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6804 | `xi` / tiny-3 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6805 | `xi` / tiny-5 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6806 | `xi` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6807 | `xi` / constant--0.106887-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6808 | `xi` / constant--0.106887-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6809 | `xi` / constant--0.106887-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6810 | `xi` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6811 | `xi` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6812 | `xi` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6816 | `xi` / limit-outlier | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6817 | `xi` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6819 | `mean,xi` / tiny-2 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6820 | `mean,xi` / tiny-3 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6821 | `mean,xi` / tiny-5 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6822 | `mean,xi` / ties | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6823 | `mean,xi` / constant--0.106887-0.0001 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6824 | `mean,xi` / constant--0.106887-1e-08 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6825 | `mean,xi` / constant--0.106887-1e-12 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6826 | `mean,xi` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6827 | `mean,xi` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6828 | `mean,xi` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6829 | `mean,xi` / scale-1e-150 | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6831 | `mean,xi` / offset-1e8 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6832 | `mean,xi` / limit-outlier | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6833 | `mean,xi` / limit-concentration | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 6838 | `sd,xi` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6839 | `sd,xi` / constant--0.106887-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6840 | `sd,xi` / constant--0.106887-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6841 | `sd,xi` / constant--0.106887-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6843 | `sd,xi` / constant-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6844 | `sd,xi` / constant-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6845 | `sd,xi` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6849 | `sd,xi` / limit-concentration | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 6858 | `mean,sd,xi` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6859 | `mean,sd,xi` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6860 | `mean,sd,xi` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |

## st

With all four coefficients free on their full domains, every nonempty sample has unbounded likelihood. Fitting therefore restricts free $\nu$ to $\nu\ge1$. Under this restriction, likelihood is bounded when no value occurs more than half the time, but attainment and uniqueness are not established in general. Holding $\omega>0$ prevents divergence. With $\xi$ held and scale free, collapse depends on the number of observations equal to that location. General held-subset existence and uniqueness remain open.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 6931 | `free` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6932 | `free` / tiny-3 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6933 | `free` / tiny-5 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6934 | `free` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6935 | `free` / constant--0.225995-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6936 | `free` / constant--0.225995-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6937 | `free` / constant--0.225995-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6938 | `free` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6939 | `free` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6940 | `free` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6944 | `free` / limit-outlier | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 6945 | `free` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6947 | `xi` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6949 | `xi` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6951 | `xi` / constant--0.225995-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6952 | `xi` / constant--0.225995-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6953 | `xi` / constant--0.225995-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6954 | `xi` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6955 | `xi` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6956 | `xi` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6957 | `xi` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6959 | `xi` / offset-1e8 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6961 | `xi` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6963 | `omega` / tiny-2 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6964 | `omega` / tiny-3 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6965 | `omega` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6966 | `omega` / ties | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6967 | `omega` / constant--0.225995-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6968 | `omega` / constant--0.225995-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6969 | `omega` / constant--0.225995-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6970 | `omega` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6971 | `omega` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6972 | `omega` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6973 | `omega` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6974 | `omega` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6976 | `omega` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6977 | `omega` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6979 | `xi,omega` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6980 | `xi,omega` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6981 | `xi,omega` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6982 | `xi,omega` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6983 | `xi,omega` / constant--0.225995-0.0001 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6984 | `xi,omega` / constant--0.225995-1e-08 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6985 | `xi,omega` / constant--0.225995-1e-12 | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6986 | `xi,omega` / constant-1-0.0001 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6987 | `xi,omega` / constant-1-1e-08 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6988 | `xi,omega` / constant-1-1e-12 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6989 | `xi,omega` / scale-1e-150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 6991 | `xi,omega` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6992 | `xi,omega` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 6993 | `xi,omega` / limit-concentration | 9 | open: finite-MLE existence not inferred from a finite candidate |
| 6995 | `alpha` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 6997 | `alpha` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 6998 | `alpha` / ties | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 6999 | `alpha` / constant--0.225995-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7000 | `alpha` / constant--0.225995-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7001 | `alpha` / constant--0.225995-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7002 | `alpha` / constant-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7003 | `alpha` / constant-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7004 | `alpha` / constant-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7009 | `alpha` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7011 | `xi,alpha` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7013 | `xi,alpha` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7015 | `xi,alpha` / constant--0.225995-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7016 | `xi,alpha` / constant--0.225995-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7017 | `xi,alpha` / constant--0.225995-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7018 | `xi,alpha` / constant-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7019 | `xi,alpha` / constant-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7020 | `xi,alpha` / constant-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7021 | `xi,alpha` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7023 | `xi,alpha` / offset-1e8 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7025 | `xi,alpha` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7027 | `omega,alpha` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7028 | `omega,alpha` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7029 | `omega,alpha` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7030 | `omega,alpha` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7031 | `omega,alpha` / constant--0.225995-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7032 | `omega,alpha` / constant--0.225995-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7033 | `omega,alpha` / constant--0.225995-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7034 | `omega,alpha` / constant-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7035 | `omega,alpha` / constant-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7036 | `omega,alpha` / constant-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7037 | `omega,alpha` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7038 | `omega,alpha` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7040 | `omega,alpha` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7041 | `omega,alpha` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7043 | `xi,omega,alpha` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7044 | `xi,omega,alpha` / tiny-3 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7045 | `xi,omega,alpha` / tiny-5 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7046 | `xi,omega,alpha` / ties | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7047 | `xi,omega,alpha` / constant--0.225995-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7048 | `xi,omega,alpha` / constant--0.225995-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7049 | `xi,omega,alpha` / constant--0.225995-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7050 | `xi,omega,alpha` / constant-1-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7051 | `xi,omega,alpha` / constant-1-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7052 | `xi,omega,alpha` / constant-1-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7053 | `xi,omega,alpha` / scale-1e-150 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7054 | `xi,omega,alpha` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7055 | `xi,omega,alpha` / offset-1e8 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7056 | `xi,omega,alpha` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7057 | `xi,omega,alpha` / limit-concentration | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7059 | `nu` / tiny-2 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7060 | `nu` / tiny-3 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7061 | `nu` / tiny-5 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7062 | `nu` / ties | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7063 | `nu` / constant--0.225995-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7064 | `nu` / constant--0.225995-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7065 | `nu` / constant--0.225995-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7066 | `nu` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7067 | `nu` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7068 | `nu` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7072 | `nu` / limit-outlier | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7073 | `nu` / limit-concentration | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7079 | `xi,nu` / constant--0.225995-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7080 | `xi,nu` / constant--0.225995-1e-08 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7081 | `xi,nu` / constant--0.225995-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7082 | `xi,nu` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7083 | `xi,nu` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7084 | `xi,nu` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7085 | `xi,nu` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7087 | `xi,nu` / offset-1e8 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7088 | `xi,nu` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7089 | `xi,nu` / limit-concentration | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7091 | `omega,nu` / tiny-2 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7092 | `omega,nu` / tiny-3 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7094 | `omega,nu` / ties | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7095 | `omega,nu` / constant--0.225995-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7096 | `omega,nu` / constant--0.225995-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7097 | `omega,nu` / constant--0.225995-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7098 | `omega,nu` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7099 | `omega,nu` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7100 | `omega,nu` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7101 | `omega,nu` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7102 | `omega,nu` / scale-1e+150 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7104 | `omega,nu` / limit-outlier | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7105 | `omega,nu` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7111 | `xi,omega,nu` / constant--0.225995-0.0001 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7112 | `xi,omega,nu` / constant--0.225995-1e-08 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7113 | `xi,omega,nu` / constant--0.225995-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7114 | `xi,omega,nu` / constant-1-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7115 | `xi,omega,nu` / constant-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7116 | `xi,omega,nu` / constant-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7117 | `xi,omega,nu` / scale-1e-150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7119 | `xi,omega,nu` / offset-1e8 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7121 | `xi,omega,nu` / limit-concentration | 7 | open: finite-MLE existence not inferred from a finite candidate |

## std

With all coefficients free, a finite MLE is not guaranteed: scale collapse, a normal limit or a location-scale $t_2$ limit can prevent attainment. Global uniqueness is unknown. Holding $\nu>2$ removes both shape limits; a tie of multiplicity $k$ gives unbounded likelihood if $k>\nu(n-k)$, while exact equality gives an unattained scale-zero supremum. With $\mu$ and shape held, a unique positive scale maximum exists exactly when $\nu(n-k)>k$, where $k$ counts observations at that location. Holding $\sigma$ removes scale collapse only when shape is also held; free shape can still collapse as $\nu\downarrow2$. Other held free-shape subsets use uncertified numerical searches, with failures stopped. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 7186 | `free` / interior | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7187 | `free` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7188 | `free` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7189 | `free` / tiny-5 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7190 | `free` / ties | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7191 | `free` / constant-0.374928-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7192 | `free` / constant-0.374928-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7193 | `free` / constant-0.374928-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 7194 | `free` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7195 | `free` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7196 | `free` / constant-1-1e-12 | 10 | open: finite-MLE existence not inferred from a finite candidate |
| 7197 | `free` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7198 | `free` / scale-1e+150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7199 | `free` / offset-1e8 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7200 | `free` / limit-outlier | 11 | open: finite-MLE existence not inferred from a finite candidate |
| 7201 | `free` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7203 | `mean` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7204 | `mean` / tiny-3 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7207 | `mean` / constant-0.374928-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7208 | `mean` / constant-0.374928-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7209 | `mean` / constant-0.374928-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7210 | `mean` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7211 | `mean` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7212 | `mean` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7213 | `mean` / scale-1e-150 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7215 | `mean` / offset-1e8 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7216 | `mean` / limit-outlier | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 7217 | `mean` / limit-concentration | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7222 | `sd` / ties | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7230 | `sd` / scale-1e+150 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 7231 | `sd` / offset-1e8 | 8 | open: finite-MLE existence not inferred from a finite candidate |
| 7235 | `mean,sd` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7242 | `mean,sd` / constant-1-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7243 | `mean,sd` / constant-1-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 7244 | `mean,sd` / constant-1-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9360 | `free` / face-normal-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9361 | `mean` / face-normal-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9362 | `sd` / face-normal-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9363 | `mean,sd` / face-normal-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9820 | `free` / face-normal-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9821 | `mean` / face-normal-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9822 | `sd` / face-normal-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9823 | `mean,sd` / face-normal-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |

## tbinom

With all coefficients free, existence and uniqueness are not classified for general nonconstant samples. Held bounds and sizes must permit every observation. Constant samples attain likelihood one at $n=\max_i x_i$ and $p=1$. Free bounds can be tightened to the sample extremes, but their real values are not unique. With size held and nonsingleton retained support, a unique interior probability MLE exists exactly when the sample mean lies strictly between the retained edges. Edge-only samples attain $p=0$ or $p=1$ only when the window contains zero or $n$, respectively. Otherwise they have an unattained point-mass supremum. Singleton support leaves an interior probability unidentified. With an interior probability held and size free, a finite integer maximum exists unless constant data equal the finite upper truncation edge and no feasible size can make the retained support singleton. Integer maxima need not be unique. With both size and probability held, free bounds tighten to the sample extremes whenever the observations and conditioning event have positive probability. A held $p=0$ permits only zeros and leaves a free size unidentified among feasible sizes. A held $p=1$ permits only constant data and fixes a free size to that count. Holding every coefficient leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 7316 | `free` / tiny-3 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7317 | `free` / tiny-5 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7318 | `free` / ties | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7325 | `free` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 7339 | `prob` / large-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 7353 | `a` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 7366 | `prob,a` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |

## tlambda

With all coefficients free over all finite shapes, the likelihood is unbounded. On the fitted branch $-1\le\lambda\le1$, a maximum is attained for at least three observations with every tie multiplicity strictly below half the sample size; uniqueness is unknown. Holding location removes the lower shape bound, and a tie at that location allows collapse when scale and shape are free. Holding scale prevents scale collapse. Held positive shape requires support compatibility. Fixed nonnegative sub-uniform shapes have unique nuisance fits when a finite maximum exists. Uniform plateaus, convex support-contact ties and negative-shape multiple modes preclude a general uniqueness claim. The linked theory inventory gives the remaining conditional existence regimes.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 7523 | `free` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 7526 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 7582 | `mu,sigma` / scale-1e+150 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7583 | `mu,sigma` / offset-1e8 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7584 | `mu,sigma` / limit-outlier | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7613 | `mu,lambda` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 7630 | `sigma,lambda` / scale-1e+150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 7632 | `sigma,lambda` / limit-outlier | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 11055 | `free` / support-lower-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11056 | `free` / support-upper-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11071 | `mu,sigma` / support-lower-1e-06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11072 | `mu,sigma` / support-upper-1e-06 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11073 | `mu,sigma` / support-lower-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11074 | `mu,sigma` / support-upper-1e-12 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11075 | `mu,sigma` / support-lower-ulp | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11076 | `mu,sigma` / support-upper-ulp | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11081 | `lambda` / support-lower-ulp | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 11082 | `lambda` / support-upper-ulp | 7 | open: finite-MLE existence not inferred from a finite candidate |

## tnorm

With all coefficients free, the bounds profile to the sample extremes. For a nonconstant sample, a finite parent-normal MLE exists exactly when its empirical variance is smaller than that of the exponential tilt on those bounds having the same empirical mean. It is then unique. The same criterion applies with bounds held, with the one-sided and untruncated cases described below. Constant samples have unbounded likelihood when both parent coefficients are free and the bounds form a nonempty interval. With $\sigma$ held, a unique finite location estimate exists exactly when the empirical mean is strictly inside the bounds. With $\mu$ held, the squared-distance criterion below characterizes a unique finite scale estimate. With both parent coefficients held, compatible observations on a nonempty interval require only likelihood evaluation after profiling any free bounds. In every case, coincident profiled bounds give no continuous-density MLE.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 7693 | `sd` / scale-1e-150 | 0 | Independent score/refinement completion exceeded the reference budget; Deadline:  |
| 7715 | `a` / tiny-2 | 7 | open: finite-MLE existence not inferred from a finite candidate |
| 7779 | `b` / tiny-2 | 6 | open: finite-MLE existence not inferred from a finite candidate |

## tpois

With all coefficients free, every nonempty nonnegative integer sample has a finite maximum. A nonconstant sample identifies the rate on the smallest retained integer window, but the real truncation bounds are not unique. A constant sample permits singleton support and does not identify the rate. With data-compatible bounds held, tighten each free bound to the corresponding sample extreme. On a nonsingleton window a unique positive finite rate exists exactly when the sample mean lies strictly between its edges. All-zero data attain rate zero if zero is retained. Other edge-only samples have an unattained point-mass supremum. With the rate held, tightening free bounds maximizes the likelihood whenever the conditioning event and observations have positive probability. Holding all coefficients leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 7945 | `a` / large-count | 0 | Independent likelihood evaluation exceeded the reference budget; Deadline:  |
| 7958 | `lambda,a` / large-count | 0 | Independent likelihood evaluation exceeded the reference budget; Deadline:  |

## trgamma

With all coefficients free, constant positive samples have unbounded likelihood. For nonconstant samples, the lognormal or power limit can prevent a finite MLE. General uniqueness of an interior maximum is not established. With scale free and either shape held, a unique finite MLE exists for nonconstant samples. With both shapes held, the scale MLE is unique even for constant samples. With scale and power held, the remaining gamma shape has a unique finite MLE. With scale and gamma shape held, the power MLE is unique unless every observation equals the held scale, when likelihood is unbounded. With only scale held, global profile bounds compare the interior with the fixed-endpoint power limit; constant samples above scale additionally have unbounded likelihood when both shapes are free.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 8006 | `free` / interior | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8007 | `free` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8008 | `free` / tiny-3 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8009 | `free` / tiny-5 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8010 | `free` / ties | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8011 | `free` / constant-4.49714-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8012 | `free` / constant-4.49714-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8013 | `free` / constant-4.49714-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8014 | `free` / constant-4.49714-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8015 | `free` / constant-4.49714-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8016 | `free` / constant-4.49714-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8017 | `free` / lower-1e-06 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8018 | `free` / lower-1e-12 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8019 | `free` / lower-1e-17 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8020 | `free` / lower-1e-200 | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8021 | `free` / scale-1e-150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8022 | `free` / scale-1e+150 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8023 | `free` / limit-outlier | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8024 | `free` / limit-concentration | 2 | open: finite-MLE existence not inferred from a finite candidate |
| 8031 | `shape1` / constant-4.49714-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8032 | `shape1` / constant-4.49714-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8034 | `shape1` / constant-4.49714-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8035 | `shape1` / constant-4.49714-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8043 | `shape1` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8050 | `shape2` / constant-4.49714-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8051 | `shape2` / constant-4.49714-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8053 | `shape2` / constant-4.49714-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8054 | `shape2` / constant-4.49714-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8062 | `shape2` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8087 | `scale` / constant-4.49714-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8088 | `scale` / constant-4.49714-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8089 | `scale` / constant-4.49714-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8090 | `scale` / constant-4.49714-0.0001 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8091 | `scale` / constant-4.49714-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8092 | `scale` / constant-4.49714-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8098 | `scale` / scale-1e+150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8100 | `scale` / limit-concentration | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 8136 | `shape2,scale` / scale-1e+150 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 10670 | `free` / constant-extra-1-0.0001 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10671 | `free` / constant-extra-1-1e-08 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10672 | `free` / constant-extra-1-1e-12 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 10677 | `shape2` / constant-extra-1-1e-08 | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 10678 | `shape2` / constant-extra-1-1e-12 | 6 | open: finite-MLE existence not inferred from a finite candidate |

## zib

With all coefficients free, all-zero data attain a nonunique maximum. For positive data, finite-size attainment must be distinguished from a supremum on the infinite-size zero-inflated Poisson boundary; the numerical search does not settle this globally for every sample. With size held at an integer at least the largest count, a maximum always exists for compatible data, but it need not be unique. With $0<p<1$ held, a finite integer maximum exists. Holding only inflation retains the infinite-size comparison with that same inflation. Held $p=0$ or $\pi=1$ permits only zeros. Held $p=1$ requires every positive count to equal the size. Size zero permits only zeros and leaves both probabilities unidentified. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 8511 | `prob` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: ZIB dossier: fixed-size compactness, or fixed interior probability makes masses vanish as integer size grows |
| 8537 | `pi` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: open |
| 8563 | `prob,pi` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: ZIB dossier: fixed-size compactness, or fixed interior probability makes masses vanish as integer size grows |

## zinb

With all coefficients free, all-zero data attain a nonunique point-mass maximum. For samples containing positive counts, existence depends on comparison with the infinite-size zero-inflated Poisson boundary; no general uniqueness claim is made. A finite nonnegative size held fixed, or a probability held strictly below one, removes that escape and gives an attained maximum for compatible data. Holding only inflation below one retains the escape, with the same inflation held on the limiting boundary. Further holds retain these conclusions. Held size zero, probability one or inflation one permits only zeros. Holding all coefficients leaves likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 8578 | `free` / interior | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8579 | `free` / tiny-2 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8583 | `free` / constant-2-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8584 | `free` / constant-2-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8585 | `free` / constant-2-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8586 | `free` / constant-2-0.0001 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8587 | `free` / constant-2-1e-08 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8588 | `free` / constant-2-1e-12 | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8589 | `free` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8590 | `free` / zero-but-one | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8591 | `free` / constant-count | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8603 | `size` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8617 | `prob` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8631 | `size,prob` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8635 | `pi` / tiny-2 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8638 | `pi` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8639 | `pi` / constant-2-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8640 | `pi` / constant-2-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8641 | `pi` / constant-2-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8642 | `pi` / constant-2-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8643 | `pi` / constant-2-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8644 | `pi` / constant-2-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8645 | `pi` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8646 | `pi` / zero-but-one | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8647 | `pi` / constant-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 8659 | `size,pi` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 8673 | `prob,pi` / large-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9672 | `free` / face-Poisson-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9676 | `pi` / face-Poisson-100 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10132 | `free` / face-Poisson-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 10136 | `pi` / face-Poisson-1e+06 | 4 | open: finite-MLE existence not inferred from a finite candidate |

## zipf

With both coefficients free, a finite MLE exists for every nonempty positive integer sample. If any observation exceeds one, the endpoint $\hat N=\max_i x_i$ and the shape estimate are unique. All-ones data give $\hat N=1$ with every finite $s\ge0$ maximizing the likelihood. With $N>1$ held, the shape MLE is finite and unique unless all observations are one, when only an unattained point-mass supremum exists. With $N=1$ held the shape is unidentified. With shape held, the unique endpoint MLE is the sample maximum. Holding both coefficients leaves only likelihood evaluation on the retained support.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 8747 | `free` / tiny-2 | 1 | open: finite-MLE existence not inferred from a finite candidate |
| 8760 | `N` / tiny-2 | 2 | open: finite-MLE existence not inferred from a finite candidate |

## zmbinom

With all coefficients free, the zero probability is the empirical zero fraction and the remaining problem is zero-truncated binomial fitting on the positive observations. A general finite-MLE criterion and uniqueness result for that free-size problem are not established here. All-zero samples attain $p_0=1$ and leave the base coefficients unidentified. Positive counts all equal to one attain a maximum with nonunique base coefficients. Holding $p_0$ leaves the positive-count problem unchanged when compatible. With $n>1$ held and at least one positive count, the free probability has a unique MLE. At $n=1$, it is unidentified. With interior $p$ held and positive counts present, a finite integer maximum exists. Held $p=0$ permits only ones among positives and leaves size unidentified. Held $p=1$ requires all positive counts to equal $n$. With no positives, all compatible base coefficients remain unidentified. All-held fits only evaluate likelihood.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 8806 | `free` / zero-but-one | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 8831 | `prob` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: held interior binomial probability: positive observed masses vanish with size, so the integer maximum is attained (dossier) |
| 8883 | `prob,p0` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: held interior binomial probability: positive observed masses vanish with size, so the integer maximum is attained (dossier) |
| 9656 | `free` / face-Poisson-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9660 | `p0` / face-Poisson-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10116 | `free` / face-Poisson-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10120 | `p0` / face-Poisson-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |

## zmnbinom

With all coefficients free, all-zero data attain a maximum at $p_0=1$ with unidentified base coefficients. Positive observations all equal to one attain a maximum at $p=1$ with size unidentified. For other samples, finite-size attainment depends on comparison with the infinite-size zero-modified Poisson boundary; general uniqueness is unknown. Holding any finite nonnegative size, or holding $0<p<1$, gives an attained maximum for compatible data. Holding only $p_0$ leaves the same positive-count existence problem. Further holds preserve these conclusions. Holding all coefficients leaves likelihood evaluation. Held zero-probability endpoints must match the observed zero indicators, and held $p=1$ permits positives only at one.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 9011 | `free` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9014 | `free` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9015 | `free` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9016 | `free` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9017 | `free` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9018 | `free` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9019 | `free` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9020 | `free` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9021 | `free` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9022 | `free` / zero-but-one | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9023 | `free` / constant-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9035 | `size` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9036 | `size` / zero-but-one | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9049 | `prob` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9063 | `size,prob` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9067 | `p0` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9070 | `p0` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9071 | `p0` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9072 | `p0` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9073 | `p0` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9074 | `p0` / constant-4-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9075 | `p0` / constant-4-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9076 | `p0` / constant-4-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9077 | `p0` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9078 | `p0` / zero-but-one | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9079 | `p0` / constant-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9091 | `size,p0` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |
| 9092 | `size,p0` / zero-but-one | 5 | open: finite-MLE existence not inferred from a finite candidate |
| 9105 | `prob,p0` / large-count | 6 | open: finite-MLE existence not inferred from a finite candidate |

## zmpois

With both coefficients free, every nonempty count sample has an attained maximum. It is unique if a positive count occurs, including $\lambda=0$ when all positive counts are one. All-zero data give $p_0=1$ and leave the rate unidentified. Holding either coefficient preserves attainment for compatible data. A free zero probability is uniquely the empirical zero fraction. A free rate is unique when positives occur and is unidentified otherwise. Holding all coefficients leaves likelihood evaluation. Held $p_0=0$ excludes zeros, held $p_0=1$ excludes positives, and held $\lambda=0$ permits positive counts only at one.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 9141 | `lambda` / constant-0-0.0001 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9142 | `lambda` / constant-0-1e-08 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9143 | `lambda` / constant-0-1e-12 | 4 | open: finite-MLE existence not inferred from a finite candidate |

## ztbinom

With both trial count and probability free, a general finite-MLE criterion and uniqueness result are not established here. The supremum can lie at the zero-truncated Poisson limit as $n$ tends to infinity. All-one data attain a maximum with nonunique coefficients. With integer $n\ge\max_i x_i$ held and $n>1$, the probability MLE exists and is unique, including its endpoints. At $n=1$, only ones are possible and $p$ is unidentified. With $0<p<1$ held, a finite integer maximum exists but uniqueness is not asserted. Held $p=0$ requires only ones and leaves $n$ unidentified. Held $p=1$ requires constant counts and sets $n$ to that count. Holding both coefficients requires compatible observations.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 9213 | `prob` / large-count | -997 | Native call did not finish within the recorded wall budget; its own iterlim outcome is missing. Finite existence: held interior binomial probability: positive observed masses vanish with size, so the integer maximum is attained (dossier) |
| 9664 | `free` / face-Poisson-100 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 10124 | `free` / face-Poisson-1e+06 | 3 | open: finite-MLE existence not inferred from a finite candidate |

## ztnbinom

With both coefficients free, a general finite-MLE criterion and uniqueness result are not established here. The supremum can occur at the zero-truncated Poisson limit. All-one samples attain $p=1$ and leave $r$ unidentified. With finite $r>0$ held, the probability MLE exists and is unique, including $p=1$ for all-one data. Held $r=0$ gives a logarithmic-series model with a unique probability estimate. With $0<p<1$ held, there is a unique finite size maximum, possibly at $r=0$. Held $p=1$ requires only ones and leaves free size unidentified. Holding both coefficients leaves only likelihood evaluation.

| Case | Holds / sample | Status | Missing evidence |
|---|---|---|---|
| 9241 | `free` / tiny-2 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9242 | `free` / tiny-3 | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9244 | `free` / ties | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9245 | `free` / constant-3-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9246 | `free` / constant-3-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9247 | `free` / constant-3-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9248 | `free` / constant-3-0.0001 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9249 | `free` / constant-3-1e-08 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9250 | `free` / constant-3-1e-12 | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9251 | `free` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9252 | `free` / constant-count | 3 | open: finite-MLE existence not inferred from a finite candidate |
| 9264 | `size` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 9277 | `prob` / large-count | 4 | open: finite-MLE existence not inferred from a finite candidate |
| 11179 | `free` / support-lower-count | 3 | open: finite-MLE existence not inferred from a finite candidate |

## Named limit coverage

There are 118 finite generating sequences in 44 families. They are candidate data sequences toward named faces, not proof that each finite sample has an interior MLE. Entries without a matching sequence below remain open: a path preserving the entry’s held values, and a finite-MLE/limit comparison, is missing. Generic concentration, endpoint and outlier probes do not replace that work.

| Family | Declared limit | Scheduled sequence / gap | Condition |
|---|---|---|---|
| bbinom | binom | candidate finite sequence: binomial; held-face and attainment attribution not automatic | $\alpha + \beta \to \infty$ with $\alpha/(\alpha + \beta)$ fixed |
| bbinom | two-point law on 0 and n | open: no face-specific parameter path preserving this entry’s held values | $\alpha, \beta \to 0$ with $\alpha/(\alpha + \beta)$ fixed |
| bbinom | binom | candidate finite sequence: binomial; held-face and attainment attribution not automatic | $\alpha/(\alpha + \beta) \to 0$ or $1$ |
| beta | point-mass collapse | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | Both shapes grow as $\alpha=tc$ and $\beta=t(1-c)$ with $t\to\infty$ and $0<c<1$ |
| betapr | exp | open: no face-specific parameter path preserving this entry’s held values | With shape1 held at one, shape2 and scale diverge at fixed ratio. |
| betapr | invexp | open: no face-specific parameter path preserving this entry’s held values | With shape2 held at one, shape1 diverges and scale vanishes at fixed product. |
| betapr | positive log-variance | open: no face-specific parameter path preserving this entry’s held values | Both free shapes $p$ and $q$ tend to infinity with their ratio and scale chosen to concentrate at the common observation. |
| betapr | gamma | candidate finite sequence: gamma, inverse-gamma; held-face and attainment attribution not automatic | With $p$ held, $q$ and $b$ tend to infinity while $b/q$ tends to a positive gamma scale. |
| betapr | invgamma | candidate finite sequence: inverse-gamma; held-face and attainment attribution not automatic | With $q$ held, $p$ tends to infinity and $b$ tends to zero while $bp$ tends to a positive inverse-gamma scale. |
| betapr | gamma | candidate finite sequence: gamma, inverse-gamma; held-face and attainment attribution not automatic | The shape $q$ tends to infinity while $p$ tends to a positive finite shape and $b/q$ tends to a positive gamma scale. |
| betapr | invgamma | candidate finite sequence: inverse-gamma; held-face and attainment attribution not automatic | The shape $p$ tends to infinity while $q$ tends to a positive finite shape and $bp$ tends to a positive inverse-gamma scale. |
| bhatt | unif | candidate finite sequence: uniform; held-face and attainment attribution not automatic | The normal scale satisfies $\sigma\to0$ with positive half-width $a$ fixed or approaching a positive limit. |
| binom | pois | candidate finite sequence: Poisson; held-face and attainment attribution not automatic | The trial count tends to infinity and the probability tends to zero with $np=\bar x>0$ fixed |
| bnbinom | nbinom | open: no face-specific parameter path preserving this entry’s held values | The shapes satisfy $\alpha,\beta\to\infty$ with their ratio and $r>0$ fixed. Exchanging $r$ and $\beta$ gives the corresponding path with $\beta>0$ fixed. |
| bnbinom | pois | open: no face-specific parameter path preserving this entry’s held values | All three shapes tend to infinity with $r\beta/\alpha$ tending to a finite positive mean. |
| bnbinom | pois | open: no face-specific parameter path preserving this entry’s held values | A free shape satisfies $r\to0$, $\beta\to0$, or $\alpha\to\infty$, while the other two coefficients stay fixed and positive. |
| burr | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free power $c$ tends to infinity while the free scale follows a concentrating path at the common observation. With scale held, concentration is also possible at a common observation below $s$ by letting $k$ grow, or at $s$ itself. |
| burr | weibull | candidate finite sequence: weibull; held-face and attainment attribution not automatic | With $c$ held, $k$ tends to infinity and $s k^{-1/c}$ stays at a positive Weibull scale. |
| burr | weibull | candidate finite sequence: weibull; held-face and attainment attribution not automatic | The shape $k$ tends to infinity while $s k^{-1/c}$ tends to a positive Weibull scale. |
| burr | pareto | open: no face-specific parameter path preserving this entry’s held values | The power $c$ tends to infinity while $kc$ tends to a positive Pareto shape and $s$ stays at its endpoint. |
| burr | pareto | open: no face-specific parameter path preserving this entry’s held values | The power $c$ tends to infinity while $kc$ tends to a positive Pareto shape and $s$ stays at its endpoint. |
| cauchy | scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | The scale $\sigma$ tends to zero at the estimated or held location $\mu$. |
| cmb | cmp | open: no face-specific parameter path preserving this entry’s held values | As $n\to\infty$ with positive $\nu$ held and odds proportional to $n^{-\nu}$, the law approaches a CMP member. |
| cmb | point mass | open: no face-specific parameter path preserving this entry’s held values | As $\nu$ diverges along an exposing direction, the law concentrates at an interior support point. |
| cmb | adjacent two-point law | open: no face-specific parameter path preserving this entry’s held values | As $\nu\to+\infty$, suitable canonical probabilities concentrate mass on two adjacent integers. |
| cmb | endpoint two-point law | open: no face-specific parameter path preserving this entry’s held values | As $\nu\to-\infty$, mass concentrates on the two support endpoints. |
| cmp | adjacent two-point law | open: no face-specific parameter path preserving this entry’s held values | As $\nu\to\infty$, adjusting $\lambda$ can retain mass on two adjacent integers. |
| cmp | point mass | open: no face-specific parameter path preserving this entry’s held values | As $\nu\to\infty$, a suitable path of $\lambda$ concentrates mass at a positive integer. |
| dgamma | point mass | open: no face-specific parameter path preserving this entry’s held values | The latent shape satisfies $k\to\infty$ while the latent mean $k/\lambda$ is fixed strictly inside the observed cell, so the latent variance $k/\lambda^2$ tends to zero. |
| dgamma | shifted Bernoulli | open: no face-specific parameter path preserving this entry’s held values | The latent shape satisfies $k\to\infty$ and the latent mean approaches the shared integer boundary of two adjacent cells at a rate that preserves their observed proportions. |
| dgamma | point mass at zero | open: no face-specific parameter path preserving this entry’s held values | The rate satisfies $\lambda\to\infty$ with shape held, or shape satisfies $k\to0$ with rate held. |
| dlaplace | point mass at location | open: no face-specific parameter path preserving this entry’s held values | The geometric probability $b$ tends to zero with integer location $\mu$ fixed. |
| dnorm | point mass | open: no face-specific parameter path preserving this entry’s held values | Scale satisfies $\sigma\to0$ with the latent normal centre inside one observed cell. |
| dnorm | shifted Bernoulli | open: no face-specific parameter path preserving this entry’s held values | Scale satisfies $\sigma\to0$ and location approaches the shared boundary of two adjacent cells at a rate that preserves their observed proportions. |
| dnorm | zero-scale limit | open: no face-specific parameter path preserving this entry’s held values | Scale satisfies $\sigma\to0$ with location fixed. |
| dweibull | degenerate | open: no face-specific parameter path preserving this entry’s held values | The shape $\beta$ tends to infinity with $(-\log q)^{-1/\beta}$ fixed between the common positive count and the next integer. For a common count of zero, $q$ tends to zero. |
| dweibull | degenerate | open: no face-specific parameter path preserving this entry’s held values | The free probability $q$ tends to zero with $\beta$ held fixed. |
| dweibull | binom | open: no face-specific parameter path preserving this entry’s held values | The shape $\beta$ tends to infinity and $q$ tends to the sample fraction of ones, giving a binomial law with size one. |
| dweibull | adjacent two-point | open: no face-specific parameter path preserving this entry’s held values | The shape $\beta$ tends to infinity while $q^{(k+1)^\beta}$ tends to the sample fraction of observations equal to $k+1$. |
| dweibull | Bernoulli(shape1) | open: no face-specific parameter path preserving this entry’s held values | The free shape $\beta$ tends to infinity with $q$ held fixed, giving a Bernoulli law with success probability $q$. |
| emg | norm | candidate finite sequence: normal; held-face and attainment attribution not automatic | The rate satisfies $\lambda\to\infty$ with $\mu$ and $\sigma>0$ fixed. |
| emg | translated exponential | open: no face-specific parameter path preserving this entry’s held values | The normal scale satisfies $\sigma\to0$ with location and positive rate fixed. |
| exp | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | The rate grows without bound, $\lambda \to \infty$ |
| fatigue | point mass | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | $\alpha \to 0$ with $\beta$ equal to the common value of $x - \mu$ |
| fatigue | point mass | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | $\alpha \to 0$ |
| foldnorm | scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | As $\sigma\to0$ with $\mu=x_0\ge0$, the density at $x_0$ diverges. |
| fpareto | weibull | open: no face-specific parameter path preserving this entry’s held values | With shape3 held at one, the Burr submodel tends to its Weibull face. |
| fpareto | invweibull | open: no face-specific parameter path preserving this entry’s held values | With shape1 held at one, the inverse Burr submodel tends to its inverse-Weibull face. |
| fpareto | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | With $\alpha$ and $\tau$ positive and fixed, the power $\gamma$ tends to infinity and scale places the concentrating distribution at the common excess. |
| fpareto | shifted trgamma; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The upper beta shape $\alpha$ tends to infinity with $\gamma$ and $\tau$ fixed and $\sigma\alpha^{-1/\gamma}$ fixed at a positive scale. |
| fpareto | shifted invtrgamma; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The lower beta shape $\tau$ tends to infinity with $\alpha$ and $\gamma$ fixed and $\sigma\tau^{1/\gamma}$ fixed at a positive scale. |
| fpareto | shifted lnorm; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | Both beta shapes equal $K\to\infty$, the power is $\gamma=\sqrt{2\psi_1(K)}/s$, and scale is $\sigma=e^m$, with lognormal log-scale $s>0$ and log-location $m$ fixed. Here $\psi_1$ is the trigamma function. |
| fpareto | shifted pareto; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The power $\gamma$ tends to infinity with $\alpha\gamma$ positive and fixed, while $\tau$ and $\sigma$ stay fixed. |
| fpareto | shifted power; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The power $\gamma$ tends to infinity with $\tau\gamma$ positive and fixed, while $\alpha$ and $\sigma$ stay fixed. |
| fpareto | shifted double Pareto; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The power $\gamma$ tends to infinity with both $\alpha\gamma$ and $\tau\gamma$ positive and fixed, while $\sigma$ stays fixed. |
| frechet | fixed-shape collapse | open: no face-specific parameter path preserving this entry’s held values | With shape $\lambda$ held, $\mu=m-\varepsilon$ approaches the sample minimum $m$ and $\sigma=c\varepsilon$ decreases to zero for a positive constant $c$. |
| frechet | shape divergence | open: no face-specific parameter path preserving this entry’s held values | With scale $\sigma$ held, the free shape $\lambda$ tends to infinity and $\mu$ equals the common observation minus $\sigma$. |
| frechet | gumbel | open: no face-specific parameter path preserving this entry’s held values | The shape $\lambda$ tends to infinity with $\sigma/\lambda=a>0$ and $\mu+\sigma=b$ fixed. |
| frechet | free collapse | open: no face-specific parameter path preserving this entry’s held values | The location $\mu$ approaches the sample minimum from below, $\lambda$ tends to one, and $\sigma$ decreases proportionally to the location gap. |
| gamma | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | The free shape tends to infinity while scale tends to zero at fixed mean |
| gb2 | weibull | open: no face-specific parameter path preserving this entry’s held values | With shape3 held at one, the Burr submodel tends to its Weibull face. |
| gb2 | invweibull | open: no face-specific parameter path preserving this entry’s held values | With shape1 held at one, the inverse Burr submodel tends to its inverse-Weibull face. |
| gb2 | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The log-scale dispersion tends to zero at the common observation while the remaining free coefficients allow concentration. |
| gb2 | trgamma | candidate finite sequence: invtrgamma, trgamma; held-face and attainment attribution not automatic | The beta shape $q$ tends to infinity with $a,p$ and $b/q^{1/a}$ fixed. |
| gb2 | invtrgamma | candidate finite sequence: invtrgamma; held-face and attainment attribution not automatic | The beta shape $p$ tends to infinity with $a,q$ and $bp^{1/a}$ fixed. |
| gb2 | lnorm | candidate finite sequence: lnorm; held-face and attainment attribution not automatic | The beta shapes satisfy $p=q=K\to\infty$, the power is $a=\sqrt{2\psi_1(K)}/\sigma$, and $b=e^\mu$, for fixed $\sigma>0$ and $\mu$. Here $\psi_1$ is the trigamma function. |
| gb2 | pareto | candidate finite sequence: double-pareto, pareto; held-face and attainment attribution not automatic | The power $a$ tends to infinity with $qa>0$, $p$ and $b$ fixed. |
| gb2 | power | candidate finite sequence: power; held-face and attainment attribution not automatic | The power $a$ tends to infinity with $pa>0$, $q$ and $b$ fixed. |
| gb2 | double Pareto | open: no face-specific parameter path preserving this entry’s held values | The power $a$ tends to infinity with $qa>0$, $pa>0$ and $b$ fixed. |
| ged | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The free scale tends to zero at the fitted or held location. |
| ged | unif | candidate finite sequence: uniform; held-face and attainment attribution not automatic | The shape $\nu$ tends to infinity, retaining finite location and scale. |
| genbeta | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free shapes concentrate the distribution at the common observation while $b$ remains held. |
| genbeta | log-gamma on scale / x | open: no face-specific parameter path preserving this entry’s held values | The shape $p$ tends to infinity and the power $a$ tends to zero with $pa\to\lambda>0$ and $q,b$ fixed. Then $-\log(X/b)$ tends to a gamma law with shape $q$ and rate $\lambda$. |
| genbeta | power | candidate finite sequence: power; held-face and attainment attribution not automatic | The power $a$ tends to infinity with $pa\to\lambda>0$, while $q$ stays bounded away from zero and $\log(q)/a\to c\ge0$. The limiting power law has upper endpoint $be^{-c}$. |
| gev | scale collapse | open: no face-specific parameter path preserving this entry’s held values | $\sigma \to 0$ with $\mu$ held at the sample minimum |
| gev | scale collapse | open: no face-specific parameter path preserving this entry’s held values | $\xi \to 1$ and $\sigma \to 0$, with the lower end of the support at the sample minimum |
| gompertz | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free initial hazard $a$ tends to infinity with $b$ held fixed. |
| gompertz | exponential face | open: no face-specific parameter path preserving this entry’s held values | The free growth coefficient $b$ decreases to zero with $a$ held fixed. |
| gompertz | exp | candidate finite sequence: exponential; held-face and attainment attribution not automatic | The growth coefficient $b$ decreases to zero and $a$ tends to $1/\bar x$. |
| gpd | threshold scale collapse | open: no face-specific parameter path preserving this entry’s held values | Scale $\sigma$ tends to zero with $\mu$ held. For held $\xi>0$ the likelihood has order $\sigma^{(n-k)/\xi-k}$, where $k$ counts threshold ties. |
| gumbel | scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | The scale $\sigma$ tends to zero at the estimated or held location $\mu$. |
| hcauchy | unbounded scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | The scale $\sigma$ tends to zero at the estimated or held endpoint $\mu$. |
| hcauchy | finite unattained scale-zero supremum | open: no face-specific parameter path preserving this entry’s held values | The scale $\sigma$ tends to zero at the estimated or held endpoint $\mu$. |
| hnorm | scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | The scale tends to zero at the fitted or held endpoint $\mu$. |
| ht | half-normal with the same endpoint; use mlhnorm(x, mu = 0) after a held translation, or mlhnorm(x, mu = NULL) with a free one | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity, retaining finite location and scale. |
| ht | unbounded scale collapse | open: no face-specific parameter path preserving this entry’s held values | The free scale $\sigma$ tends to zero at the free or held endpoint. |
| ht | finite unattained scale-zero supremum | open: no face-specific parameter path preserving this entry’s held values | The free scale $\sigma$ tends to zero at fixed positive shape. |
| huber | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The free scale $\sigma$ tends to zero at the fitted or held location. |
| huber | norm | candidate finite sequence: normal; held-face and attainment attribution not automatic | The free shape satisfies $\epsilon\to\infty$, leaving the normal law at the eligible fitted or held location and scale. |
| huber | laplace | open: no face-specific parameter path preserving this entry’s held values | The free shape and scale tend to zero with $b=\sigma/\epsilon$ fixed, giving a Laplace law. |
| hyper | degenerate | open: no face-specific parameter path preserving this entry’s held values | As $N\to\infty$ with positive $K$ and $n$ held, the law tends to a point mass at zero. |
| invburr | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free power $c$ tends to infinity while the free scale follows a concentrating path at the common observation. With scale held, concentration is also possible at a common observation above $s$ by letting $k$ grow, or at $s$ itself. |
| invburr | invweibull | candidate finite sequence: inverse-weibull; held-face and attainment attribution not automatic | With $c$ held, $k$ tends to infinity and $s k^{1/c}$ stays at a positive inverse Weibull scale. |
| invburr | invweibull | candidate finite sequence: inverse-weibull; held-face and attainment attribution not automatic | The shape $k$ tends to infinity while $s k^{1/c}$ tends to a positive inverse Weibull scale. |
| invburr | power | open: no face-specific parameter path preserving this entry’s held values | The power $c$ tends to infinity while $kc$ tends to a positive power shape and $s$ stays at its endpoint. |
| invburr | power | open: no face-specific parameter path preserving this entry’s held values | The power $c$ tends to infinity while $kc$ tends to a positive power shape and $s$ stays at its endpoint. |
| invgamma | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | $\alpha$ tends to infinity with $\beta/\alpha$ equal to the common observation |
| invgauss | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | $\mu$ equals the common observation and $\lambda$ tends to infinity |
| invgauss | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | the free shape $\lambda$ tends to infinity with mean $\mu$ held |
| invparalogis | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free shape $\alpha$ tends to infinity while the free scale $\theta$ follows a concentrating path at the common observation. |
| invpareto | invexp | open: no face-specific parameter path preserving this entry’s held values | $\alpha$ tends to infinity while $\alpha\theta$ tends to a positive inverse exponential scale |
| invtrgamma | fixed-endpoint Pareto face | open: no face-specific parameter path preserving this entry’s held values | The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held. |
| invtrgamma | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free coefficients concentrate the distribution at the common positive observation. |
| invtrgamma | lnorm | open: no face-specific parameter path preserving this entry’s held values | The gamma shape $k$ tends to infinity with $p\sqrt{k}\to1/\sigma$ and $\log a - \log(k)/p\to\mu$, for fixed $\sigma>0$ and $\mu$. |
| invtrgamma | pareto | open: no face-specific parameter path preserving this entry’s held values | The power $p$ tends to infinity with $kp\to d>0$ and $a$ converging to a positive endpoint. |
| invweibull | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | The free shape $k$ tends to infinity while scale $\lambda$ is held at the common observation value. |
| kumar | point-mass collapse | open: no face-specific parameter path preserving this entry’s held values | $a$ tends to infinity and $b=c^{-a}$ for a fixed $0<c<1$ |
| laplace | scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | The scale $b$ tends to zero at the estimated or held location $\mu$. |
| lgamma | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | As shape and rate grow to infinity with shape/rate fixed at $\log(x_0)$, the density at $x_0>1$ diverges. |
| llogis | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | As shape $\beta\to\infty$ with scale $\alpha=x_0>0$, the density at $x_0$ is $\beta/(4x_0)$ and diverges. |
| llogis | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free shape $\beta$ tends to infinity while scale $\alpha$ is held at the common observation. |
| lnorm | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | $\mu$ equals the common log observation and $\sigma$ tends to zero |
| lnorm | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | the free log scale $\sigma$ tends to zero with log mean $\mu$ held |
| logis | scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | $s$ tends to zero at the fitted or held location $\mu$ |
| logitnorm | zero-scale collapse | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | The free scale $\sigma$ tends to zero at the fitted or held logit location. |
| lomax | exp | open: no face-specific parameter path preserving this entry’s held values | $\kappa$ tends to infinity while $\lambda\kappa$ tends to a positive exponential rate |
| lst | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The free scale tends to zero at the fitted or held location. |
| lst | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The scale tends to zero at the free or held location. |
| lst | tie equality boundary | open: no face-specific parameter path preserving this entry’s held values | The scale tends to zero at the free or held location. |
| lst | norm | candidate finite sequence: normal; held-face and attainment attribution not automatic | The shape $\nu$ tends to infinity, retaining finite location and scale. |
| naka | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | The free shape $m$ tends to infinity with $\Omega$ held at the common squared observation value. |
| nbinom | pois | candidate finite sequence: Poisson; held-face and attainment attribution not automatic | the size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p=\bar x>0$ fixed |
| norm | scale collapse | candidate finite sequence: collapse; held-face and attainment attribution not automatic | Scale tends to zero at the fitted or held location |
| nsbeta | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The shapes satisfy $\alpha=tu$ and $\beta=t(1-u)$ with $t\to\infty$, where $u=(x-a)/(b-a)$ is the common transformed observation and the endpoints are held. |
| paralogis | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free shape $\alpha$ tends to infinity while the free scale $\theta$ follows a concentrating path at the common observation. |
| pareto | zero likelihood | open: no face-specific parameter path preserving this entry’s held values | No parameter path within the held support can give positive likelihood. |
| pareto | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The shape $\alpha$ tends to infinity with $b$ at the common observation. |
| pareto2 | shifted exp; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The shape $\alpha$ and scale $\sigma$ tend to infinity with $\alpha/\sigma$ fixed at a positive rate. |
| pareto3 | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The shape $\gamma$ tends to infinity while $\sigma$ equals the common excess above $\mu$. |
| pareto4 | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | A free power $\gamma$ tends to infinity while the other free coefficients concentrate the excess distribution at its common observed value. |
| pareto4 | shifted weibull; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The shape $\alpha$ tends to infinity with $\gamma$ fixed and $\sigma\alpha^{-1/\gamma}$ fixed at a positive scale. |
| pareto4 | shifted pareto; subtract min before fitting | open: no face-specific parameter path preserving this entry’s held values | The power $\gamma$ tends to infinity with $\alpha\gamma$ fixed at a positive tail exponent and $\sigma$ fixed. |
| pareto4 | shifted Pareto with its endpoint held at min + scale | open: no face-specific parameter path preserving this entry’s held values | The power $\gamma$ tends to infinity with $\alpha\gamma$ fixed at a positive tail exponent and $\sigma$ fixed. |
| pig | pois | open: no face-specific parameter path preserving this entry’s held values | The concentration $\lambda$ tends to infinity with $\mu>0$ fixed. |
| pig | pois | open: no face-specific parameter path preserving this entry’s held values | The mean $\mu$ tends to zero with positive concentration $\lambda$ held. |
| power | zero likelihood | open: no face-specific parameter path preserving this entry’s held values | The held upper endpoint $a$ is below an observation, giving zero likelihood. |
| sged | unresolved constrained uniform endpoint rates | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity at fixed standard deviation, with a limiting uniform endpoint on an observation. |
| sged | unresolved constrained uniform endpoint rates | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity at fixed standard deviation, with a limiting uniform endpoint on an observation. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sged | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | constant sample scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the free or held mean. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sged | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | constant sample scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the free or held mean. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sged | constant sample scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the free or held mean. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sged | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | constant sample scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the free or held mean. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sged | constant sample scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the free or held mean. |
| sged | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sged | constant sample scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the free or held mean. |
| sged | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sged | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the held mean. |
| sged | uncertified scale boundary | open: no face-specific parameter path preserving this entry’s held values | The conditional scale search approaches its numerical tail guard. |
| sged | unif | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity, retaining finite location and scale. |
| sged | right translated half-GED | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity with finite endpoint and right-side scale. |
| sged | left translated half-GED | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero with finite endpoint and left-side scale. |
| slash | point mass collapse | open: no face-specific parameter path preserving this entry’s held values | Scale satisfies $\sigma\to0$ as location approaches a value occurring exactly half the time. |
| sn | hnorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\alpha$ tends to $+\infty$, giving a right translated half-normal limit. |
| sn | reflected translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The shape $\alpha$ tends to $-\infty$, giving a reflected translated half-normal limit. |
| snorm | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The free standard deviation $\sigma$ tends to zero at the fitted or held mean. |
| snorm | hnorm | open: no face-specific parameter path preserving this entry’s held values | As $\xi\to\infty$, the distribution tends to a translated right half-normal with the limiting mean and standard deviation. |
| snorm | reflected translated half-normal | open: no face-specific parameter path preserving this entry’s held values | As $\xi\to0$, the distribution tends to a reflected translated half-normal with the limiting mean and standard deviation. |
| sstd | right translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity and $\xi$ tends to infinity, retaining any held mean and standard deviation. |
| sstd | left translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity and $\xi$ tends to zero, retaining any held mean and standard deviation. |
| sstd | right translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity and $\xi$ tends to infinity, retaining any held mean and standard deviation. |
| sstd | left translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity and $\xi$ tends to zero, retaining any held mean and standard deviation. |
| sstd | right translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity and $\xi$ tends to infinity, retaining any held mean and standard deviation. |
| sstd | left translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity and $\xi$ tends to zero, retaining any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sstd | two-piece t with 2 degrees of freedom | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ decreases to two with finite positive ordinary t scale and any held mean retained; sd diverges. |
| sstd | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sstd | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sstd | two-piece t with 2 degrees of freedom | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ decreases to two with finite positive ordinary t scale and any held mean retained; sd diverges. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sstd | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sstd | two-piece t with 2 degrees of freedom | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ decreases to two with finite positive ordinary t scale and any held mean retained; sd diverges. |
| sstd | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | right translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | left translated half-family | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero, with endpoint and one-sided scale chosen to retain any held mean and standard deviation. |
| sstd | tie collapse | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | uncertified tie equality | open: no face-specific parameter path preserving this entry’s held values | The effective ordinary t scale tends to zero at an attainable concentration point. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity while any held mean and standard deviation are retained. |
| sstd | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The standard deviation $\sigma$ tends to zero at the held mean. |
| sstd | symmetric scale-collapse equality | open: no face-specific parameter path preserving this entry’s held values | Held xi is one and the standard deviation tends to zero at the held mean. |
| sstd | nonunit-skew scale-collapse equality | open: no face-specific parameter path preserving this entry’s held values | Held xi differs from one and the standard deviation tends to zero at the held mean. |
| sstd | snorm | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ tends to infinity, retaining finite location and scale. |
| sstd | hnorm | open: no face-specific parameter path preserving this entry’s held values | The limiting path is $\nu \to \infty$ and $\xi \to \infty$. |
| sstd | reflected translated half-normal | open: no face-specific parameter path preserving this entry’s held values | The limiting path is $\nu \to \infty$ and $\xi \to 0$. |
| sstd | translated half-t | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to infinity with finite one-sided scale and endpoint at the sample minimum. |
| sstd | reflected translated half-t | open: no face-specific parameter path preserving this entry’s held values | The skew $\xi$ tends to zero with finite one-sided scale and endpoint at the sample maximum. |
| sstd | two-piece t with 2 degrees of freedom | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ decreases to two with ordinary t scale and skew fixed; sd diverges. |
| st | sn | open: no face-specific parameter path preserving this entry’s held values | The degrees of freedom satisfy $\nu\to\infty$ with location, scale and skewness fixed. |
| st | ht | open: no face-specific parameter path preserving this entry’s held values | The skewness satisfies $\alpha\to+\infty$ with location, scale and degrees of freedom fixed. |
| st | reflected half-t | open: no face-specific parameter path preserving this entry’s held values | The skewness satisfies $\alpha\to-\infty$ with location, scale and degrees of freedom fixed. |
| st | hnorm | open: no face-specific parameter path preserving this entry’s held values | The degrees of freedom and skewness satisfy $\nu\to\infty$ and $\alpha\to+\infty$ with location and scale fixed. |
| st | reflected half-normal | open: no face-specific parameter path preserving this entry’s held values | The degrees of freedom and skewness satisfy $\nu\to\infty$ and $\alpha\to-\infty$ with location and scale fixed. |
| st | unbounded scale collapse | open: no face-specific parameter path preserving this entry’s held values | Scale satisfies $\omega\to0$ at a location repeated $k$ times, with fixed finite $\alpha$ and $\nu(n-k)<k$. |
| st | scale-collapse equality | open: no face-specific parameter path preserving this entry’s held values | Scale satisfies $\omega\to0$ at a location repeated $k$ times, with fixed finite $\alpha$ and $\nu(n-k)=k$. This finite path limit alone does not establish non-existence. |
| std | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The free scale tends to zero at the fitted or held location. |
| std | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The ordinary t scale tends to zero at the free or held location. |
| std | tie equality boundary | open: no face-specific parameter path preserving this entry’s held values | The ordinary t scale tends to zero at the free or held location. |
| std | norm | candidate finite sequence: normal; held-face and attainment attribution not automatic | The shape $\nu$ tends to infinity with mean and sd fixed. |
| std | lst | open: no face-specific parameter path preserving this entry’s held values | The shape $\nu$ decreases to two with mean and ordinary t scale fixed; sd diverges. |
| tbinom | point mass at lowest support point | open: no face-specific parameter path preserving this entry’s held values | With size fixed, $p\to0$ on a nonsingleton window whose lowest retained count is positive. |
| tbinom | point mass at highest support point | open: no face-specific parameter path preserving this entry’s held values | With size fixed, $p\to1$ on a nonsingleton window whose highest retained count is below $n$. |
| tbinom | point mass at highest support point | open: no face-specific parameter path preserving this entry’s held values | The size satisfies $n\to\infty$ with $0<p<1$ held and a finite upper truncation bound. |
| tbinom | tpois | open: no face-specific parameter path preserving this entry’s held values | The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window. |
| tbinom | tpois | open: no face-specific parameter path preserving this entry’s held values | The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window. |
| tbinom | tpois | open: no face-specific parameter path preserving this entry’s held values | The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window. |
| tbinom | tpois | open: no face-specific parameter path preserving this entry’s held values | The size satisfies $n\to\infty$ and $p\to0$ with $np$ tending to a finite positive rate on the same truncation window. |
| tlambda | scale collapse | open: no face-specific parameter path preserving this entry’s held values | The free scale $\sigma$ tends to zero at a location occupied by observations. |
| tlambda | infeasible held support | open: no face-specific parameter path preserving this entry’s held values | The held scale $\sigma$ and positive shape $\lambda$ leave no positive-likelihood support containing the sample. |
| tnorm | scale collapse | open: no face-specific parameter path preserving this entry’s held values | As $\sigma\to0$, mass concentrates at the projection of $\mu$ onto $[a,b]$. |
| tnorm | endpoint location escape | open: no face-specific parameter path preserving this entry’s held values | With positive scale held, $\mu\to-\infty$ at a finite lower endpoint or $\mu\to\infty$ at a finite upper endpoint concentrates mass there. |
| tnorm | exp | open: no face-specific parameter path preserving this entry’s held values | As $\sigma\to\infty$ and $\mu/\sigma^2\to-1/(\bar x-a)$ with $a$ finite and $b=\infty$, the distribution tends to $a+Y$, where $Y$ is exponential. |
| tnorm | exp | open: no face-specific parameter path preserving this entry’s held values | As $\sigma\to\infty$ and $\mu/\sigma^2\to1/(b-\bar x)$ with $a=-\infty$ and $b$ finite, the distribution tends to $b-Y$, where $Y$ is exponential. |
| tnorm | truncated exponential | open: no face-specific parameter path preserving this entry’s held values | As $\sigma\to\infty$ and $\mu/\sigma^2\to r$ with both bounds finite, the distribution tends to the law with density proportional to $e^{rx}$ on $[a,b]$. It is uniform when $r=0$. |
| tnorm | uniform | open: no face-specific parameter path preserving this entry’s held values | With $\mu$ held and both bounds finite, $\sigma\to\infty$ gives the uniform distribution on $[a,b]$. |
| tpois | point mass at lowest support point | open: no face-specific parameter path preserving this entry’s held values | The rate satisfies $\lambda\to0$ on a fixed nonsingleton window whose lowest retained count is positive. |
| tpois | point mass at highest support point | open: no face-specific parameter path preserving this entry’s held values | The rate satisfies $\lambda\to\infty$ on a fixed nonsingleton window with a finite upper edge. |
| trgamma | fixed-endpoint power face | open: no face-specific parameter path preserving this entry’s held values | The gamma shape $k$ tends to zero and the power $p$ tends to infinity with $kp\to d>0$ and the scale $a$ held. |
| trgamma | unbounded likelihood | open: no face-specific parameter path preserving this entry’s held values | The free coefficients concentrate the distribution at the common positive observation. |
| trgamma | lnorm | open: no face-specific parameter path preserving this entry’s held values | The gamma shape $k$ tends to infinity with $p\sqrt{k}\to1/\sigma$ and $\log a + \log(k)/p\to\mu$, for fixed $\sigma>0$ and $\mu$. |
| trgamma | power | open: no face-specific parameter path preserving this entry’s held values | The power $p$ tends to infinity with $kp\to d>0$ and $a$ converging to a positive endpoint. |
| triang | zero-width collapse | open: no face-specific parameter path preserving this entry’s held values | The free width $b-a$ tends to zero around the common observation. |
| triang | incompatible held parameters | open: no face-specific parameter path preserving this entry’s held values | A valid held endpoint excludes an observation or forces zero density there. |
| unif | zero-width collapse | open: no face-specific parameter path preserving this entry’s held values | The free width $b-a$ tends to zero around the common observation. |
| unif | incompatible held endpoints | open: no face-specific parameter path preserving this entry’s held values | A held endpoint excludes an observation. |
| weibull | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | $\lambda$ equals the common observation and $k$ tends to infinity |
| weibull | unbounded likelihood | candidate finite sequence: point-mass; held-face and attainment attribution not automatic | $k$ tends to infinity with scale $\lambda$ held at the common observation value |
| yulesimon | point mass at one | open: no face-specific parameter path preserving this entry’s held values | The shape satisfies $\rho\to\infty$. |
| zeta | point mass at one | open: no face-specific parameter path preserving this entry’s held values | The exponent satisfies $s\to\infty$. |
| zib | zip | open: no face-specific parameter path preserving this entry’s held values | The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive rate. |
| zib | zip | open: no face-specific parameter path preserving this entry’s held values | The trial count $n$ tends to infinity and $p$ tends to zero with $np$ tending to a finite positive rate. |
| zinb | zip | open: no face-specific parameter path preserving this entry’s held values | The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed and positive inflation. |
| zinb | pois | candidate finite sequence: Poisson; held-face and attainment attribution not automatic | The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ fixed, while inflation tends to zero. |
| zip | pois | open: no face-specific parameter path preserving this entry’s held values | The rate $\lambda$ tends to zero with inflation $\pi<1$ held. |
| zip | undefined zero-rate reference | open: no face-specific parameter path preserving this entry’s held values | The rate $\lambda$ is held at zero, outside the declared domain. |
| zipf | point mass at one | open: no face-specific parameter path preserving this entry’s held values | The shape satisfies $s\to\infty$ with $N>1$ held. |
| zmbinom | zmpois | open: no face-specific parameter path preserving this entry’s held values | As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated Poisson law. The zero probability $p_0$ remains fixed, giving a zero-modified Poisson law. |
| zmbinom | zmpois | open: no face-specific parameter path preserving this entry’s held values | As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated Poisson law. The zero probability $p_0$ remains fixed, giving a zero-modified Poisson law. |
| zmnbinom | zmpois | open: no face-specific parameter path preserving this entry’s held values | The size $r$ tends to infinity and $p$ tends to one with $r(1-p)/p$ tending to a finite positive rate and $p_0$ unchanged. |
| ztbinom | ztpois | open: no face-specific parameter path preserving this entry’s held values | As $n\to\infty$ and $p\to0$ with $np\to\lambda>0$, the positive-count law tends to a zero-truncated Poisson law. |
| ztnbinom | ztpois | open: no face-specific parameter path preserving this entry’s held values | As $r\to\infty$ and $p\to1$ with $r(1-p)/p\to\lambda>0$, the law tends to a zero-truncated Poisson law. |

## Unrepresentable decimal endpoint constructions

A separate ULP grid probes representable interior complements; the following nominal decimal constructions round onto an endpoint and were not treated as approaching samples.

- beta: upper-1e-17: binary64 endpoint rounding or support incompatibility.
- beta: upper-1e-200: binary64 endpoint rounding or support incompatibility.
- frechet: lower-1e-200: binary64 endpoint rounding or support incompatibility.
- genbeta: upper-1e-17: binary64 endpoint rounding or support incompatibility.
- genbeta: upper-1e-200: binary64 endpoint rounding or support incompatibility.
- hnorm: lower-1e-200: binary64 endpoint rounding or support incompatibility.
- kumar: upper-1e-17: binary64 endpoint rounding or support incompatibility.
- kumar: upper-1e-200: binary64 endpoint rounding or support incompatibility.
- lgamma: lower-1e-17: binary64 endpoint rounding or support incompatibility.
- lgamma: lower-1e-200: binary64 endpoint rounding or support incompatibility.
- logitnorm: upper-1e-17: binary64 endpoint rounding or support incompatibility.
- logitnorm: upper-1e-200: binary64 endpoint rounding or support incompatibility.
- pareto: lower-1e-17: binary64 endpoint rounding or support incompatibility.
- pareto: lower-1e-200: binary64 endpoint rounding or support incompatibility.
- power: upper-1e-17: binary64 endpoint rounding or support incompatibility.
- power: upper-1e-200: binary64 endpoint rounding or support incompatibility.
- triang: upper-1e-17: binary64 endpoint rounding or support incompatibility.
- triang: lower-1e-200: binary64 endpoint rounding or support incompatibility.
- triang: upper-1e-200: binary64 endpoint rounding or support incompatibility.
