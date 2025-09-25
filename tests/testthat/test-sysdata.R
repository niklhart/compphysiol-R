test_that("sysdata.rda is up to date", {
    raw_dir <- test_path("../../data-raw")   # relative to tests/testthat
    sysdata_file <- test_path("../../R/sysdata.rda")

    # If raw_dir does not exist (e.g. in CRAN build), skip test
    if (!dir.exists(raw_dir)) {
        skip("No data-raw/ directory in installed package")
    }

    raw_files <- list.files(raw_dir, full.names = TRUE, pattern = "\\.R$")
    expect_true(length(raw_files) > 0, info = "No raw data scripts found")

    sysdata_time <- file.info(sysdata_file)$mtime
    raw_times <- file.info(raw_files)$mtime

    expect_true(
        all(raw_times <= sysdata_time),
        info = "sysdata.rda is older than data-raw scripts, run data-raw/make-sysdata.R"
    )
})
