set shell := ["bash", "-lc"]

clean-r:
	find R -maxdepth 1 -type f ! -name 'RcppExports.R' -delete

gen:
	Rscript scripts/gen_metadata.R && \
	Rscript scripts/gen_estimators_from_rsrc.R && \
	Rscript scripts/gen_wrappers.R && \
	Rscript scripts/gen_utils_copy.R

regen: clean-r gen

test: regen
	Rscript -e "suppressMessages(devtools::load_all('.')); devtools::test(filter=NULL)"

check: regen
	Rscript -e "devtools::check(document = FALSE, run_dont_test = FALSE)"

bench model="burr" sizes="50,200,1000" reps="5" out="bench_results/bench.csv":
	mkdir -p bench_results
	Rscript scripts/bench_cpp.R --model {{model}} --sizes {{sizes}} --reps {{reps}} --out {{out}}
