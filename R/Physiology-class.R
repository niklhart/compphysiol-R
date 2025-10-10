
#' Physiology Class
#'
#' Stores physiological parameters in a transparent tabular structure.
#'
#' @export
Physiology <- R6::R6Class("Physiology",
                          public = list(

                              #' @field param_table Data frame with physiology parameters
                              param_table = NULL,

                              #' @field meta Categorical metadata (sex, strain, etc.)
                              meta = list(),

                              #' @description Initialize physiology
                              #' @param params Optional initial parameter table
                              #' @param meta Categorical metadata (species, sex, etc.)
                              initialize = function(params = NULL, meta = NULL) {
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

                              #' @description
                              #' Add metadata (categorical attributes) as name-value pairs.
                              #' @param ... Name-value pairs.
                              add_meta = function(...) {
                                  dots <- list(...)
                                  if (length(dots) == 0) return(invisible(self))
                                  self$meta <- modifyList(self$meta, dots)
                                  invisible(self)
                              },

                              #' @description Get numeric parameter or metadata by name (and optionally context)
                              #' @param name Name of the parameter or metadata entry to be queried
                              #' @param context A string, defaulting to `"scalar"`
                              get = function(name, context = "scalar") {
                                  # First check metadata
                                  if (name %in% names(self$meta)) {
                                      return(self$meta[[name]])
                                  }
                                  # Otherwise, numeric parameters
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
                                  # merge metadata at the end
                                  c(out, self$meta)
                              },

                              #' @description Summarize Physiology as a string.
                              summary = function() {
                                  paste0("Physiology: ", nrow(self$param_table), " parameters")
                              },

                              #' @description Display a Physiology object.
                              #' @examples
                              #' p <- Physiology$new()
                              #' p$add_meta(species = "rat", strain = "Wistar", sex = "male")
                              #' P$add_scalar(age = 20, BW = 250)
                              print = function(...) {
                                  # ---- 1. Header with metadata ----
                                  meta_str <- ""
                                  if (length(self$meta) > 0) {
                                      shown_meta <- head(paste0(names(self$meta), "=", self$meta), 3)
                                      meta_str <- paste(shown_meta, collapse = ", ")
                                      if (length(self$meta) > 3) meta_str <- paste0(meta_str, ", ...")
                                  }

                                  header <- "<Physiology>"
                                  if (meta_str != "") {
                                      header <- paste0(header, " (", meta_str, ")")
                                  }

                                  cat(header, "\n", sep = "")

                                  # ---- 2. Scalars ----
                                  scalars <- subset(self$param_table, type == "scalar")
                                  if (nrow(scalars) > 0) {
                                      shown_scalars <- head(paste0(scalars$parameter, "=", scalars$value), 3)
                                      scalar_str <- paste(shown_scalars, collapse = ", ")
                                      if (nrow(scalars) > 3) scalar_str <- paste0(scalar_str, ", ...")
                                  } else {
                                      scalar_str <- "none"
                                  }
                                  cat("  Scalars: ", scalar_str, "\n", sep = "")

                                  # ---- 3. Tissue stats ----
                                  tissues <- subset(self$param_table, type != "scalar")
                                  if (nrow(tissues) > 0) {
                                      ntis <- length(unique(tissues$context))
                                      npar <- length(unique(tissues$parameter))
                                      cat("  Tissue parameters: ", nrow(tissues),
                                          " entries (", ntis, " tissues, ", npar, " parameters)\n", sep = "")
                                  } else {
                                      cat("  Tissue parameters: none\n")
                                  }

                                  invisible(self)
                              }


                          )
)
