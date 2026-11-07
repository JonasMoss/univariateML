#!/bin/bash
set -eu
uname -srmo
c++ --version
cmake --version
Rscript -e 'cat(R.version.string,"\n"); for (p in c("Rcpp","BH","testthat")) cat(p,as.character(packageVersion(p)),"\n")'
