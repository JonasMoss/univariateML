### Terms by path (native snapshot, all cases of the path; offsets and terms relative, in units of eps = 2^-52 unless noted)

| path | cases | claim fails | max offset | max half width | max conditioning (actual / a priori) | max data (a priori / actual) | max conversion (actual / a priori) | max W actual | max W a priori | max W a priori / 1e-10 |
|---|---|---|---|---|---|---|---|---|---|---|
| gamma_bracket  | 13 | 3 | 3.9 eps | 2.0 eps | 3.0 / 13.5 eps | 0.0 / 0.0 eps | 0.99 / 1.50 eps | 5.6 eps | 17.0 eps | 3.8e-05 |
| location_scale_held frechet | 22 | 4 | 15.3 eps | 16.0 eps | 1.8 / 4.7 eps | 0.6 / 0.8 eps | 0.48 / 1.50 eps | 17.7 eps | 21.4 eps | 4.7e-05 |
| location_scale_held invweibull | 21 | 3 | 11.3 eps | 16.0 eps | 1.8 / 4.1 eps | 0.6 / 1.0 eps | 0.80 / 1.50 eps | 17.8 eps | 21.4 eps | 4.8e-05 |
| location_scale_held llogis | 22 | 7 | 10.0 eps | 16.0 eps | 2.4 / 6.9 eps | 0.4 / 1.2 eps | 0.65 / 1.50 eps | 18.0 eps | 23.4 eps | 5.2e-05 |
| naka_square_held  | 22 | 1 | 3.6 eps | 2.0 eps | 45.6 / 114.3 eps | 0.0 / 0.0 eps | 0.68 / 1.50 eps | 47.9 eps | 117.8 eps | 2.6e-04 |
| weibull_free  | 43 | 2 | 136.5 eps | 22.5 eps | 3100.7 / 3100.7 eps | 150.8 / 3100.7 eps | 0.41 / 1.00 eps | 3123.3 eps | 3124.2 eps | 6.9e-03 |

### What-if variants (not in the C++)

| variant | family | cases | max offset | max half | max W actual | max W a priori | max W a priori / 1e-10 |
|---|---|---|---|---|---|---|---|
| kappa | frechet | 22 | 4.7 eps | 4.0 eps | 5.2 eps | 9.6 eps | 2.1e-05 |
| kappa | invweibull | 21 | 2.8 eps | 4.0 eps | 5.9 eps | 10.1 eps | 2.2e-05 |
| kappa | llogis | 22 | 2.5 eps | 4.0 eps | 6.8 eps | 11.8 eps | 2.6e-05 |
| ratio | weibull | 43 | 19.3 eps | 22.5 eps | 129.7 eps | 130.6 eps | 2.9e-04 |
