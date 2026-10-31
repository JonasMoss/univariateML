# Metadata edition 3 enables the parallel scheduler. Keep the existing
# expectation/condition contracts in serial, installed and parallel runs.
# setup runs after testthat resets the edition; source-package helpers do not.
testthat::local_edition(2L, .env = .GlobalEnv)
