# The Justfile normally sets TESTTHAT_CPUS=4. This job owns one CPU slot.
# Rscript reads this startup profile after receiving the recipe environment.
Sys.setenv(TESTTHAT_CPUS = "1", MAKEFLAGS = "-j1",
           OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1",
           MKL_NUM_THREADS = "1", BLIS_NUM_THREADS = "1")
