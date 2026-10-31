# One tier resolver for local runs, installed checks and parallel workers.
umvl_test_tier <- function() {
  tier <- Sys.getenv("UMVL_TEST_TIER", "")
  if (!nzchar(tier)) tier <- if (tolower(Sys.getenv("NOT_CRAN")) %in% c("true", "1")) "dev" else "cran"
  if (!tier %in% c("cran", "dev", "full")) stop("UMVL_TEST_TIER must be cran, dev or full")
  tier
}
tier_at_least <- function(tier) {
  match(umvl_test_tier(), c("cran", "dev", "full")) >= match(tier, c("cran", "dev", "full"))
}
skip_unless_tier <- function(tier) {
  testthat::skip_if(!tier_at_least(tier), paste("requires", tier, "test tier"))
}
