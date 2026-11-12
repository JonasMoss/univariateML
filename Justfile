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

# Explain and test changed families plus transitive header dependants.
test-changed base="master" *args:
    python3 dev/tools/test-changed.py {{base}} {{args}}

# Wipe and rebuild univariateML-r/ from workspace sources.
# UMVL_UNITY_GROUPS=0 selects per-family compilation for debugging; positive
# values select the number of deterministic unity groups (default: 4).
# package/ is the CRAN-shippable artefact, fully generated and
# gitignored; bindings/r/ itself holds only sources, scripts, and
# metadata.
assemble:
	python3 dev/validation/modifier_capabilities.py
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
	cd bindings/r && \
	  UMVL_PKG_R_DIR={{repo_root}}/univariateML-r/R \
	  Rscript scripts/codegen_links.R

regen: gen
	cd univariateML-r && PKG_BUILD_EXTRA_FLAGS=false Rscript -e "options(pkg.build_extra_flags = FALSE); roxygen2::roxygenise()"

# Default development tier; reserve all four CPU slots before invoking.
test: regen
	cd univariateML-r && \
	  UMVL_DISTRIBUTIONS_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  UMVL_TEST_TIER=${UMVL_TEST_TIER:-dev} PKG_BUILD_EXTRA_FLAGS=false TESTTHAT_CPUS=4 \
	  Rscript ../bindings/r/scripts/test_local.R

# Exhaustive pre-merge/release suite, including slow independent oracles.
test-full:
	UMVL_TEST_TIER=full just test

check: regen
	cd univariateML-r && R CMD build --no-build-vignettes . && \
	  UMVL_DISTRIBUTIONS_DIR={{repo_root}}/univariateML-cpp/include/cppunivariate/distributions \
	  R CMD check --no-manual --ignore-vignettes univariateML_*.tar.gz

# Check a tarball outside the repository, with repository reads denied
# (Linux Landlock via setpriv).
check-standalone: regen
	bash dev/tools/r-standalone-check.sh

# Create golden samples for families without one (existing samples are frozen;
# UMVL_GOLDEN_IDS=<id> UMVL_GOLDEN_RESAMPLE=1 replaces one deliberately).
# Then run `just golden-reference --write <id>`.
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
# All targets use -g -O2 -DNDEBUG and gnu++17, as in R (no Release -O3).
# Debug separately: cmake -S univariateML-cpp -B build/cpp-debug
#   -DCPPUNIV_TEST_OPTIMIZE=OFF [-DBoost_INCLUDE_DIR=/path/to/boost].
# R/BH is only a monorepo convenience when system Boost is unavailable.
cpp-test:
    #!/usr/bin/env bash
    set -euo pipefail
    launcher=()
    if command -v ccache >/dev/null; then launcher=(-DCMAKE_CXX_COMPILER_LAUNCHER=ccache); fi
    if ! cmake -S univariateML-cpp -B univariateML-cpp/build -DCPPUNIV_TEST_OPTIMIZE=ON "${launcher[@]}"; then
        boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
        test -f "$boost/boost/version.hpp"
        cmake -S univariateML-cpp -B univariateML-cpp/build -DCPPUNIV_TEST_OPTIMIZE=ON "${launcher[@]}" -DBoost_INCLUDE_DIR="$boost"
    fi
    cmake --build univariateML-cpp/build -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}"
    ctest --test-dir univariateML-cpp/build --output-on-failure -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}" -L "${UMVL_TEST_TIER:-dev}"

# Exercise the entire optimized core with double intermediates.
# This explicit portability audit runs every ctest shard (including full).
cpp-test-no-long-double:
    #!/usr/bin/env bash
    set -euo pipefail
    launcher=()
    if command -v ccache >/dev/null; then launcher=(-DCMAKE_CXX_COMPILER_LAUNCHER=ccache); fi
    if ! cmake -S univariateML-cpp -B build/cpp-no-long-double -DCPPUNIV_TEST_OPTIMIZE=ON "${launcher[@]}" -DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; then
        boost=$(Rscript -e 'cat(system.file("include", package="BH"))')
        test -f "$boost/boost/version.hpp"
        cmake -S univariateML-cpp -B build/cpp-no-long-double -DCPPUNIV_TEST_OPTIMIZE=ON "${launcher[@]}" -DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS -DBoost_INCLUDE_DIR="$boost"
    fi
    cmake --build build/cpp-no-long-double -j "${CMAKE_BUILD_PARALLEL_LEVEL:-1}"
    ctest --test-dir build/cpp-no-long-double --output-on-failure

# Check real fitter goldens against symbolic README densities.
quality *ids:
	PYTHONPATH={{repo_root}}/dev/validation python3 -m quality check {{ids}}

# Independent high-precision d/p/q truth from declarative densities.
dpqr-truth *ids:
	PYTHONPATH={{repo_root}}/dev/validation python3 -m quality truth {{ids}}

# Independent 50-digit reference MLEs for golden samples (report only;
# add --write to update golden.json estimates and its reference block).
golden-reference *args:
	PYTHONPATH={{repo_root}}/dev/validation python3 -m quality reference {{args}}

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
    python3 bindings/js/run-tests.py

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

# Verify declared special cases and transforms against dossier densities/supports.
relations:
    PYTHONPATH={{repo_root}}/dev/validation python3 -m quality.relations
