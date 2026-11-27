# Tests that compile rxode2 or nlmixr2 models are slow, especially on Windows.
# They run locally (devtools sets NOT_CRAN = "true") and in CI, which sets
# NOT_CRAN explicitly, but not on CRAN. Whole tests use skip_on_cran(); this
# helper guards backend-specific blocks inside otherwise CRAN-run tests.
compiled_backend_available <- function(package) {
    identical(Sys.getenv("NOT_CRAN"), "true") &&
        requireNamespace(package, quietly = TRUE)
}
