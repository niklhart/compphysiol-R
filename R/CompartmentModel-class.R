#' CompartmentModel Class
#'
#' Represents a compartmental model.
#'
#' @export
CompartmentModel <- R6::R6Class(
    "CompartmentModel",
    public = list(
        compartments = list(),
        reactions = list(),
        observables = list(),
        doses = NULL,
        infusionEvents = NULL,

        #' @description
        #' Initialize a new CompartmentModel.
        initialize = function() {
            self$compartments <- list()
            self$reactions <- list()
            self$observables <- list()
            self$doses <- list()
            self$infusionEvents <- data.frame(
                var = character(),
                time = numeric(),
                value = numeric(),
                method = character(),
                stringsAsFactors = FALSE
            )
        },

        #' @description
        #' Get compartment initials as numeric vector.
        getInitials = function() {
            sapply(self$compartments, function(c) c$initial)
        },

        #' @description
        #' Get compartment names.
        getStateNames = function() {
            sapply(self$compartments, function(c) c$name)
        },

        #' @description
        #' Generate ODE function, observables, and free parameters.
        #' @param paramValues Named list of parameter values to inline in ODEs.
        toODE = function(paramValues = list()) {
            stateNames <- self$getStateNames()
            name2idx <- setNames(seq_along(stateNames), stateNames)

            freeParams <- character()  # collect parameters left free

            makeFun <- function(expr) {
                if (is.function(expr)) return(expr)
                e <- expr

                # 1) Replace compartment names with y[index]
                for (nm in stateNames) {
                    pat <- paste0("\\b", nm, "\\b")
                    e <- gsub(pat, paste0("y[", name2idx[[nm]], "]"), e, perl = TRUE)
                }

                # 2) Find bare identifier tokens and either inline scalar paramValues
                #    or convert to params$<name>.  Do not touch reserved tokens.
                tokens <- unique(unlist(regmatches(e, gregexpr("\\b[A-Za-z]\\w*\\b", e, perl = TRUE))))
                reserved <- c("t", "y", "params", "pi", "Inf", "NaN",
                              "exp", "log", "sqrt", "sin", "cos", "tan", "abs")
                for (tk in tokens) {
                    if (!(tk %in% reserved)) {
                        # do not replace tokens that are already params$<tk>
                        patTok <- paste0("(?<!params\\$)\\b", tk, "\\b")
                        if (tk %in% names(paramValues)) {
                            val <- paramValues[[tk]]
                            if (is.numeric(val) && length(val) == 1) {
                                # inline numeric scalar
                                e <- gsub(patTok, as.character(val), e, perl = TRUE)
                            } else {
                                # non-scalar provided -> keep as params$tk (do not inline)
                                e <- gsub(patTok, paste0("params$", tk), e, perl = TRUE)
                                freeParams <<- union(freeParams, tk)
                            }
                        } else {
                            # not provided -> keep as params$tk and record as free
                            e <- gsub(patTok, paste0("params$", tk), e, perl = TRUE)
                            freeParams <<- union(freeParams, tk)
                        }
                    }
                }

                e
            }

            # Collect RHS terms for ODEs
            rhs <- vector("list", length(stateNames))
            for (j in seq_along(self$reactions)) {
                r <- self$reactions[[j]]
                expr <- makeFun(r$rate)
                if (!is.null(r$from) && r$from != "" && r$from %in% stateNames) {
                    idx <- name2idx[[r$from]]
                    rhs[[idx]] <- c(rhs[[idx]], paste0("-(", expr, ")"))
                }
                if (!is.null(r$to) && r$to != "" && r$to %in% stateNames) {
                    idx <- name2idx[[r$to]]
                    rhs[[idx]] <- c(rhs[[idx]], paste0("+(", expr, ")"))
                }
            }

            # Build ODE function body (explicit, human-readable)
            lines <- c("function(t,y,params) {",
                       paste0("    dydt <- numeric(", length(stateNames), ")"))
            for (i in seq_along(stateNames)) {
                if (length(rhs[[i]]) == 0) {
                    lines <- c(lines, paste0("    dydt[", i, "] <- 0"))
                } else {
                    lines <- c(lines, paste0("    dydt[", i, "] <- ", paste(rhs[[i]], collapse = " ")))
                }
            }
            lines <- c(lines, "    list(dydt)", "}")
            odefun <- eval(parse(text = paste(lines, collapse = "\n")))

            # Observables (same substitution logic)
            obsFuncs <- lapply(self$observables, function(o) {
                expr <- makeFun(o$expr)
                eval(parse(text = paste0("function(t,y,params) ", expr)))
            })

            list(
                odefun = odefun,
                stateNames = stateNames,
                obsFuncs = obsFuncs,
                freeParams = sort(unique(freeParams))
            )
        }

    )
)
