# Vision

1. Have C++17 armadillo library for maximum likelihood.
   1. Maybe also densities by merging with extradistr more or less.
   2. No external dependencies except these, ideally.
   3. Might possibly use one, e.g., nlopt?
   4. Partition problems nicely, e.g., core is normal poisson exp etc, actuar is fatigue and stuff, discrete, ... etc.
   5. Speed and accuracy roughly 50-50. Do bespoke if possible.


## Priorities

1. Make well-working benchmarking scaffold.

### Distributions

1. Make paralogis implementation for both parameters.
2. Write C++ estimators for the fGarch stuff: ged, sged, snorm, sstd, std.
   1. Snorm is probably well-studied; maybe sstd too. 
   2. Ged looks absolutely horrible and completely useless though. 
   3. Not sure any of these can be made very efficient. They also seem not supergood. Gah.
3. Beta-binomial. 

### More?

1. Make a bigger package: Include d,p,q,r for all distributions. (Take over extraDistr and duplicate actuar, fGarch, sads, logitnorm, nakagami)
   1. Start with nakagami, sads, logitnorm, and fGarch. Most of these are trivial I think. 
   2. extraDistr and actuar are very large. 
   3. I can reduce imports to:
      1. RcppArmadillo,
      2. assertthat (maybe remove?),
      3. tibble (maybe remove?),
      4. extraDistr,
      5. actuar,
      6. intervals (maybe remove too?),
      7. Rfast can be removed.
      8. rlang can most likely be removed.
      9. pracma definitely.
      10. Remove suggests too. 
2. Port to python.
3. 


## Justfile 
- `clean-r`: remove generated files in `R/` except `RcppExports.R`.
- `gen`: run gens in order (metadata → estimators → wrappers → utils):  
  `Rscript scripts/gen_metadata.R && Rscript scripts/gen_estimators_from_rsrc.R && Rscript scripts/gen_wrappers.R && Rscript scripts/gen_utils_copy.R`
- `regen`: `clean-r` then `gen`.
- `test`: `regen` then `Rscript -e "suppressMessages(devtools::load_all('.')); devtools::test(filter=NULL)"`.
- `check`: `regen` then `Rscript -e "devtools::check(document = FALSE, run_dont_test = FALSE)"`.
