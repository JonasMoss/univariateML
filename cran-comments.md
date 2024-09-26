## Release summary

<<<<<<< Updated upstream
* Fixed error in mlunif.
* Improved documentation for model_select.
* Improved documentation for univariateML_models.
=======
* Add support for discrete distributions.
* Add support for Poisson distributions in `mlpois`.
* Add support for plotting for CDFs and quantile functions.
* Documentation update.
>>>>>>> Stashed changes

## Test environments
Used standard Github actions:
os: macOS-latest,    r: 'release'
os: windows-latest,  r: 'release'
os: ubuntu-latest,   r: 'devel'
os: ubuntu-latest,   r: 'release'
<<<<<<< Updated upstream
os: ubuntu-latest,   r: 'oldrel-1'
=======
os: ubunto-latest,   r: 'oldrel-1'
>>>>>>> Stashed changes

## R CMD check results
There were no ERRORs or WARNINGs. 

NOTEs: Some platforms claim 
"Namespaces in Imports field not imported from:
  ?extraDistr? ?logitnorm? ?nakagami? ?tibble?
  All declared Imports should be used."
These are false alarms. All imported packages are used.

## Downstream dependencies
kdensity: No problems.
svines: No problems.
