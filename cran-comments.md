## Release summary

* Fixed two notes regarding pipes and bibentry.
* Add support for many more distributions.
* Add support for plotting for CDFs and quantile functions.
* Option for returning tables in `model_select`.
* Add model attributes to `ml***` objects.
* Documentation update.

## Test environments
Used standard Github actions:
os: macOS-latest,    r: 'release'
os: windows-latest,  r: 'release'
os: ubuntu-latest,   r: 'devel'
os: ubuntu-latest,   r: 'release'

## R CMD check results
There were no ERRORs or WARNINGs. 

NOTEs: Some platforms claim 
"Namespaces in Imports field not imported from:
  ?extraDistr? ?logitnorm? ?nakagami? ?tibble?
  All declared Imports should be used."
These are false alarms. All imported packages are used.

## Downstream dependencies
There are no problems except for package `kdensity`. I have submitted an updated version of `kdensity` that handles these problems.
