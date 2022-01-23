## Release summary

* Fixed error in mlllogis.
* Removed artifical bound on mlnaka.
* Fixed error in test of model selection that used expect_equal incorrectly,
  inadvertently calling all.equal with four arguments.
* Now passes CRAN on r-devel.

## Test environments
Using Github actions, with:
os: macOS-latest,    r: 'release'
os: windows-latest,  r: 'release'
os: ubuntu-latest,   r: 'devel'
os: ubuntu-latest,   r: 'release'
Does not work with oldrel-1, as `actuar`, a dependency of this package, 
requires R 4.1.0.

## R CMD check results
There were no ERRORs or WARNINGs. 

NOTEs: Some platforms claim 
"Namespaces in Imports field not imported from:
  ?extraDistr? ?logitnorm? ?nakagami? ?tibble?
  All declared Imports should be used."
These are false alarms. All imported packages are used.

## Downstream dependencies
kdensity: No problems.
