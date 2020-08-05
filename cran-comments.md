## Release summary

* Support for densities from the fGarch package.
* Automated model selection. 

## Test environments
* local Windows 10, R version 4.0.2
* Windows Server 2012 (on AppVeyor) R version 4.0.2 Patched (2020-08-04 r78971)
* Ubuntu 16.04 (on Travis-CI), R version R 4.0.2, R-oldrel, R-devel.
* macOS 10.13.6 (on Travis-CI), R version R 4.0.2

## R CMD check results
There were no ERRORs or WARNINGs. 

NOTEs: Some platforms claim 
"Namespaces in Imports field not imported from:
  ‘extraDistr’ ‘logitnorm’ ‘nakagami’ ‘tibble’
  All declared Imports should be used."
These are false alarms. All imported packages are used.

## Downstream dependencies
There are no downstream dependencies.
