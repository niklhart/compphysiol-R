## code to prepare `sysdata.rda`

source("data-raw/build_physiologydb.R")
source("data-raw/build_drugdb.R")
source("data-raw/build_models.R")

#.models <- build_models()      # unused currently
.physiologydb <- build_physiologydb()
.drugdb <- build_drugdb()

usethis::use_data(.physiologydb, .drugdb, internal = TRUE, overwrite = TRUE)
