# svines validation results

Upstream svines 0.3.0 at `main` commit `eb57cd592fa9844249d8b29b6e1916fb06e70801`. The workstation used R 4.6.1, rvinecopulib 0.7.3.1.0, and testthat 3.3.2. Both univariateML builds report 1.5.0. The old build came from `lib-old` and the upcoming build with the support type shim came from `lib-new`.

| svines source | univariateML | Installed testthat suite | Documented bootstrap example, seed 1 |
|---|---|---|---|
| Upstream | CRAN 1.5.0 | 89 passed, 0 failures, 0 warnings | Passed, ten finite quantiles |
| Upstream | Upcoming | 89 passed, 0 failures, 0 warnings | Failed in simulation while evaluating the GEV support |
| Patched | CRAN 1.5.0 | 91 passed, 0 failures, 0 warnings, 1 skip | Passed, ten finite quantiles |
| Patched | Upcoming | 104 passed, 0 failures, 0 warnings, 0 skips | Passed, ten finite quantiles |

Both patched builds also produced ten finite coefficient vectors and ten finite quantiles for each seed from 1 through 30. The documented example selected logistic for both margins with CRAN and GEV followed by logistic with the upcoming build.

The old code failed the new analytic small-scale normal checks with both builds. On the upcoming build, the forced-GEV fixture independently reproduced the non-finite Hessian, non-finite bootstrap coefficients, and simulation error. The patched suite exercises finite scores, both marginal Hessian blocks near a GEV endpoint, rejection when an observation is outside every attempted stencil, and simulation from the bootstrap models. CRAN 1.5.0 skips the GEV fixture because it does not export `mlgev`.

## Reproduction

From the univariateML worktree, use `build=old` or `build=new`. Use a clean upstream svines checkout for the baseline and apply `svines.patch` for the patched run. The commands install svines with its tests into a scratch library and resolve univariateML from the selected shared build.

```bash
pr=$PWD/build/revdep-pr
deps=$(realpath ../validate/build/revdep)
scratch=$pr/lib-$build
export TMPDIR=$PWD/build/tmp MAKEFLAGS=-j1
mkdir -p "$TMPDIR" "$scratch"
export R_LIBS=$deps/lib-$build:$deps/lib-deps
/tmp/umvl-cpu/run nice -n 10 R CMD INSTALL --preclean --install-tests \
  -l "$scratch" "$pr/svines"
export R_LIBS=$scratch:$R_LIBS
cd "$pr"
/tmp/umvl-cpu/run nice -n 10 Rscript --vanilla -e '
  library(svines)
  testthat::test_dir(system.file("tests", "testthat", package = "svines"),
                    package = "svines", load_package = "installed")
'
/tmp/umvl-cpu/run nice -n 10 Rscript --vanilla -e '
  library(svines)
  set.seed(1)
  example("svine_bootstrap_models", run.dontrun = FALSE, ask = FALSE)
  stopifnot(length(mu_boot) == 10L, all(is.finite(mu_boot)))
'
```

Scratch logs from these runs are under `build/revdep-pr/{before,after}-{old,new}/`. Both the forward patch check against upstream and the reverse check against the tested source passed.

The workstation also ran `/tmp/umvl-cpu/run4 just test-changed --no-long-double`. It exited successfully and selected zero univariateML families.
