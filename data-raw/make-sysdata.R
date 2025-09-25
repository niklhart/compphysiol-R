## code to prepare `sysdata.rda`
# Currently, this is just proof-of-concept, I don't currently the interface.

source("data-raw/build_physiologies.R")
source("data-raw/build_models.R")

.models <- build_models()
.physiologies <- build_physiologies()

usethis::use_data(.models, .physiologies, internal = TRUE, overwrite = TRUE)
