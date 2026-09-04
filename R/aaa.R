# Internal package state.
.the <- new.env(parent = emptyenv())
.the$model_units <- data.frame(
    symbol = character(),
    def = character(),
    stringsAsFactors = FALSE
)
