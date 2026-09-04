# Internal package state.
.the <- new.env(parent = emptyenv())
.the$registry <- new.env(parent = emptyenv())
.the$registry$model_units <- data.frame(
    symbol = character(),
    def = character(),
    stringsAsFactors = FALSE
)
