## code to prepare `sysdata.rda`

source("data-raw/build_physiologies.R")
source("data-raw/build_models.R")

#.models <- build_models()      # unused currently
.physiology_data <- build_physiologies()

usethis::use_data(.physiology_data, internal = TRUE, overwrite = TRUE)
