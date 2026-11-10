# Validation configuration: leave three cores for the queued workstation run.
# Applied through R_PROFILE_USER on simbox; tiers and tests are unchanged.
Sys.setenv(TESTTHAT_CPUS = "1")
