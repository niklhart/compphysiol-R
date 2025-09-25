
#' Physiology Class
#'
#' Stores physiological parameters in a transparent tabular structure.
#'
#' @export
Physiology <- R6::R6Class("Physiology",
                          public = list(
                              #' @field param_table Data frame with physiology parameters
                              param_table = NULL,

                              #' @description Initialize physiology
                              #' @param params Optional initial parameter table
                              initialize = function(params = NULL) {
                                  self$param_table <- data.frame(
                                      parameter  = character(),
                                      context    = character(),   # "scalar" or tissue name
                                      value      = numeric(),
                                      unit       = character(),
                                      type       = character(),   # "scalar" / "tissue" / "derived"
                                      reference  = character(),
                                      assumption = character(),
                                      stringsAsFactors = FALSE
                                  )
                                  if (!is.null(params)) {
                                      self$param_table <- rbind(self$param_table, params)
                                  }
                              },

                              #' @description Add a scalar parameter
                              #' @param name Name of the parameter
                              #' @param value Numeric value
                              #' @param unit Character, unit of measurement
                              #' @param reference Source reference
                              #' @param assumption Assumptions made
                              add_scalar = function(name, value, unit = "", reference = "", assumption = "") {
                                  self$param_table <- rbind(self$param_table,
                                                            data.frame(parameter=name, context="scalar",
                                                                       value=value, unit=unit, type="scalar",
                                                                       reference=reference, assumption=assumption,
                                                                       stringsAsFactors = FALSE))
                                  invisible(self)
                              },

                              #' @description Add a tissue parameter (e.g. volume, flow)
                              #' @param tissue Tissue name (e.g. "Liver")
                              #' @param name Parameter name (e.g. "V" or "Q")
                              #' @param value Numeric value
                              #' @param unit Character, unit of measurement
                              #' @param reference Source reference
                              #' @param assumption Assumptions made
                              add_tissue_param = function(tissue, name, value, unit = "", reference = "", assumption = "") {
                                  self$param_table <- rbind(self$param_table,
                                                            data.frame(parameter=name, context=tissue,
                                                                       value=value, unit=unit, type="tissue",
                                                                       reference=reference, assumption=assumption,
                                                                       stringsAsFactors = FALSE))
                                  invisible(self)
                              },

                              #' @description Get a parameter by name and context
                              get = function(name, context = "scalar") {
                                  row <- subset(self$param_table,
                                                parameter == name & context == context)
                                  if (nrow(row) == 0) return(NA)
                                  row$value
                              },

                              #' @description Export parameters as a named list
                              #' Scalars: name → value
                              #' Tissues: `paste0(parameter, "[", context, "]")` → value
                              to_param_list = function() {
                                  out <- list()
                                  for (i in seq_len(nrow(self$param_table))) {
                                      row <- self$param_table[i, ]
                                      if (row$context == "scalar") {
                                          out[[row$parameter]] <- row$value
                                      } else {
                                          nm <- paste0(row$parameter, "[", row$context, "]")
                                          out[[nm]] <- row$value
                                      }
                                  }
                                  out
                              }
                          )
)
