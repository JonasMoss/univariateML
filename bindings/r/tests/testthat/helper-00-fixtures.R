# Resolve once before the existing fixture consumers, including truth tests.
# An explicit dossier override continues to support repository development.
if (!nzchar(Sys.getenv("UMVL_DISTRIBUTIONS_DIR"))) {
  Sys.setenv(UMVL_DISTRIBUTIONS_DIR = normalizePath(testthat::test_path("fixtures"),
                                                 mustWork = TRUE))
}
