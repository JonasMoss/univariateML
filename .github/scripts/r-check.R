# rcmdcheck invokes R CMD build followed by R CMD check --as-cran, retains
# the tarball and complete logs, and fails CI for errors or warnings.
dir.create("build/ci-r", recursive = TRUE, showWarnings = FALSE)
# As r-lib/actions check-r-package: skip the CRAN incoming checks (the version
# is already on CRAN; release readiness is checked separately before submission).
rcmdcheck::rcmdcheck("univariateML-r", args = "--as-cran",
                    check_dir = "build/ci-r", error_on = "warning",
                    env = c("_R_CHECK_CRAN_INCOMING_" = "false",
                            "_R_CHECK_CRAN_INCOMING_REMOTE_" = "false"))
