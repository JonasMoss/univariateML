set shell := ["bash", "-lc"]

clean-r:
	find R -maxdepth 1 -type f ! -name 'RcppExports.R' -delete

gen-metadata:
	Rscript scripts/gen_metadata.R

gen-estimators:
	Rscript scripts/gen_estimators_from_rsrc.R

gen-wrappers:
	Rscript scripts/gen_wrappers.R

gen-utils:
	Rscript scripts/gen_utils_copy.R

regen: clean-r gen-metadata gen-estimators gen-wrappers gen-utils

rcpp-attrs:
	Rscript -e "Rcpp::compileAttributes()"

test: regen
	Rscript -e "suppressMessages(devtools::load_all('.')); devtools::test(filter=NULL)"
