set shell := ["bash", "-lc"]

repo_root := justfile_directory()

# Stable generation ordering/formatting, independent of the caller's locale.
export LC_ALL := "C.UTF-8"
export TZ := "UTC"

# Compiler cache for every C++ build (R compiles go through ccache via
# ~/.R/Makevars; CMake gets CMAKE_CXX_COMPILER_LAUNCHER in cpp-test). A
# cache of its own, so other projects cannot evict it, and paths relative
# to the checkout root, so the main checkout and every worktree share it.
# The directory sits inside ~/.cache/ccache, which Codex jobs may write.
export CCACHE_DIR := env_var_or_default("UMVL_CCACHE_DIR", env_var("HOME") + "/.cache/ccache/univariateML")
export CCACHE_BASEDIR := repo_root
export CCACHE_NOHASHDIR := "1"
export CCACHE_MAXSIZE := "8G"

default: test

# Wipe and rebuild univariateML-r/ from workspace sources.
# package/ is the CRAN-shippable artefact, fully generated and
# gitignored; bindings/r/ itself holds only sources, scripts, and
# metadata.
assemble:
	cd bindings/r && \
	  UMVL_DOSSIER_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  Rscript scripts/assemble_package.R

# Mirror univariateML-cpp/include/ into univariateML-r/inst/include/.
# Runs after assemble (which wipes package/), required before R-side
# compilation because the package's PKG_CPPFLAGS = -I../inst/include
# expects the headers there.
sync-headers: assemble
	mkdir -p univariateML-r/inst/include
	rsync -a --delete --prune-empty-dirs --include='*/' --include='*.hpp' --exclude='*' \
	  univariateML-cpp/include/cppunivariate univariateML-r/inst/include/

# Run Rcpp::compileAttributes() in package/ (RcppExports.{R,cpp} from the
# per-distribution adapters), then generate package/R/ml00_metadata.R and
# ml02_wrappers.R (with roxygen docs) from distributions/<id>/README.md.
gen: sync-headers
	cd univariateML-r && Rscript -e "Rcpp::compileAttributes()"
	cd bindings/r && \
	  UMVL_DOSSIER_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  UMVL_PKG_R_DIR={{repo_root}}/univariateML-r/R \
	  Rscript scripts/codegen_metadata.R
	cd bindings/r && \
	  UMVL_DOSSIER_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  UMVL_PKG_R_DIR={{repo_root}}/univariateML-r/R \
	  Rscript scripts/codegen_wrappers.R

regen: gen
	cd univariateML-r && Rscript -e "roxygen2::roxygenise()"

test: regen
	cd univariateML-r && \
	  UMVL_DISTRIBUTIONS_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  Rscript -e "testthat::test_local('.', stop_on_failure = TRUE)"

check: regen
	cd univariateML-r && R CMD build --no-build-vignettes . && \
	  UMVL_DISTRIBUTIONS_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  R CMD check --no-manual --ignore-vignettes univariateML_*.tar.gz

# Check a tarball outside the repository, with repository reads denied
# (Linux Landlock via setpriv).
check-standalone: regen
	bash dev/tools/r-standalone-check.sh

# Regenerate distributions/<id>/golden.json from the current R/C++ pipeline.
# Run only when intentionally accepting a new baseline (e.g. algorithm
# change). After regen, inspect the diff before committing.
goldens: regen
	cd bindings/r && \
	  UMVL_DOSSIER_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  UMVL_PKG_DIR={{repo_root}}/univariateML-r \
	  Rscript scripts/goldens.R

# Rewrite distributions/<id>/reference.json: frozen values from the former
# reference packages (fGarch, logitnorm, nakagami, sads) of the families
# with their own d/p/q/r. Needs those packages; the tests do not.
reference-values: regen
	cd bindings/r && \
	  UMVL_DOSSIER_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  UMVL_PKG_DIR={{repo_root}}/univariateML-r \
	  Rscript scripts/reference_values.R

# Rewrite the distribution tables in bindings/r/README.Rmd and README.md
# from distributions/<id>/README.md.
readme-table:
	cd bindings/r && \
	  UMVL_DOSSIER_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  Rscript scripts/readme_table.R

# Build & run the standalone C++ test harness from univariateML-cpp/.
# R/BH is only a monorepo convenience when system Boost is unavailable.
cpp-test:
    #!/usr/bin/env bash
    set -euo pipefail
    launcher=()
    if command -v ccache >/dev/null; then launcher=(-DCMAKE_CXX_COMPILER_LAUNCHER=ccache); fi
    if ! cmake -S univariateML-cpp -B univariateML-cpp/build "${launcher[@]}"; then
        boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
        test -f "$boost/boost/version.hpp"
        cmake -S univariateML-cpp -B univariateML-cpp/build "${launcher[@]}" -DBoost_INCLUDE_DIR="$boost"
    fi
    cmake --build univariateML-cpp/build -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}"
    ctest --test-dir univariateML-cpp/build --output-on-failure

# Exercise the entire core with double-width extended intermediates.
cpp-test-no-long-double:
    #!/usr/bin/env bash
    set -euo pipefail
    launcher=()
    if command -v ccache >/dev/null; then launcher=(-DCMAKE_CXX_COMPILER_LAUNCHER=ccache); fi
    if ! cmake -S univariateML-cpp -B build/cpp-no-long-double "${launcher[@]}" -DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; then
        boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
        test -f "$boost/boost/version.hpp"
        cmake -S univariateML-cpp -B build/cpp-no-long-double "${launcher[@]}" -DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS -DBoost_INCLUDE_DIR="$boost"
    fi
    cmake --build build/cpp-no-long-double -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}"
    ctest --test-dir build/cpp-no-long-double --output-on-failure

# Check real fitter goldens against symbolic README densities.
quality *ids:
	PYTHONPATH={{repo_root}}/dev/validation python3 -m quality check {{ids}}

# Independent high-precision d/p/q truth from declarative densities.
dpqr-truth *ids:
	PYTHONPATH={{repo_root}}/dev/validation python3 -m quality truth {{ids}}

# --- LaTeX research notes (dev/research/) ---
notes_dir := "dev/research"

# Build one note (named, or the most recently edited) and open the PDF.
note-build name="":
    #!/usr/bin/env bash
    set -euo pipefail
    dir="{{notes_dir}}"
    if [ -n "{{name}}" ]; then
        tex="$dir/$(basename "{{name}}" .tex).tex"
    else
        tex="$(ls -t "$dir"/*.tex 2>/dev/null | head -1 || true)"
    fi
    [ -n "${tex:-}" ] && [ -f "$tex" ] || { echo "no .tex note found in $dir"; exit 1; }
    base="$(basename "$tex" .tex)"
    cd "$dir"
    pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null
    pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null
    rm -f "$base".{aux,log,out,toc}
    if command -v xdg-open >/dev/null 2>&1; then opener=xdg-open; else opener=open; fi
    if command -v setsid >/dev/null 2>&1; then
        setsid "$opener" "$base.pdf" >/dev/null 2>&1 < /dev/null &
    else
        "$opener" "$base.pdf" >/dev/null 2>&1 &
    fi
    echo "built and opened $dir/$base.pdf"

# Rebuild every note (no viewer); report any that fail to compile.
note-build-all:
    #!/usr/bin/env bash
    set -uo pipefail
    dir="{{notes_dir}}"
    fail=0
    shopt -s nullglob
    for tex in "$dir"/*.tex; do
        base="$(basename "$tex" .tex)"
        if ( cd "$dir" && pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null 2>&1 \
                       && pdflatex -interaction=nonstopmode -halt-on-error "$base.tex" >/dev/null 2>&1 ); then
            ( cd "$dir" && rm -f "$base".{aux,log,out,toc} )
            echo "ok   $base"
        else
            echo "FAIL $base"; fail=1
        fi
    done
    exit $fail

# Emscripten pilot; callers reserve CPU slots as for native builds.
js-build:
    bash bindings/js/build.sh

js-test:
    node --v8-pool-size=1 --test bindings/js/test.mjs

# Derived site data from canonical dossiers (no authored second catalog).
site-data:
    python3 site/scripts/data.py

site-build: site-data
    cd site && PATH="$PWD/node_modules/.bin:$PATH" RAYON_NUM_THREADS=1 UV_THREADPOOL_SIZE=1 ASTRO_TELEMETRY_DISABLED=1 node scripts/prepare.mjs
    cd site && PATH="$PWD/node_modules/.bin:$PATH" RAYON_NUM_THREADS=1 UV_THREADPOOL_SIZE=1 ASTRO_TELEMETRY_DISABLED=1 npm run build

site-dev: site-data
    cd site && PATH="$PWD/node_modules/.bin:$PATH" node scripts/prepare.mjs
    cd site && PATH="$PWD/node_modules/.bin:$PATH" RAYON_NUM_THREADS=1 UV_THREADPOOL_SIZE=1 ASTRO_TELEMETRY_DISABLED=1 npm run dev

site-check: site-build
    python3 site/scripts/check.py
