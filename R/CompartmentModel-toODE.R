
#' @description
#' Generate ODE function, initial values, observables, and free parameters.
#' @param paramValues Named list of parameter values to inline in ODEs.
CompartmentModel$set("public", "toODE", function(paramValues = list()) {
     stateNames <- self$getStateNames()
     name2idx <- setNames(seq_along(stateNames), stateNames)

     # ---- Validation: check that all reactions point to known compartments ----
     check_comp <- function(nm) {
         if (!is.null(nm) && nzchar(nm) && !(nm %in% stateNames)) {
             stop(
                 "Reaction references unknown compartment '", nm, "'. ",
                 "Compartment names in this model: ",
                 paste(stateNames, collapse = ", "), ". ",
                 "Did you mean to merge this model with another?"
             )
         }
     }
     for (r in self$reactions) {
         check_comp(r$from)
         check_comp(r$to)
     }

     # Environment container for free parameters
     freeParams <- new.env(parent = emptyenv())
     freeParams$list <- character()

     makeFun <- function(expr, obsFunc = FALSE) {
         substitute_expr(expr, stateNames, name2idx,
                         paramValues = paramValues,
                         freeParamsEnv = freeParams,
                         obsFunc = obsFunc)
     }

     # Collect RHS terms for ODEs
     rhs <- vector("list", length(stateNames))
     for (j in seq_along(self$reactions)) {
         r <- self$reactions[[j]]
         expr <- makeFun(r$rate)
         expr_str <- deparse(expr, width.cutoff = 500) |> paste(collapse = " ")
         if (!is.null(r$from) && r$from != "" && r$from %in% stateNames) {
             idx <- name2idx[[r$from]]
             rhs[[idx]] <- c(rhs[[idx]], paste0("-(", expr_str, ")"))
         }
         if (!is.null(r$to) && r$to != "" && r$to %in% stateNames) {
             idx <- name2idx[[r$to]]
             rhs[[idx]] <- c(rhs[[idx]], paste0("+(", expr_str, ")"))
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
         expr_lang <- makeFun(o$expr, obsFunc = TRUE)
         expr_str  <- paste(deparse(expr_lang, width.cutoff = 500), collapse = " ")
         eval(parse(text = paste0("function(t,y,params) ", expr_str)))
     })
     names(obsFuncs) <- vapply(self$observables, function(o) o$name, "")

     # Output list
     list(
         odefun = odefun,
         stateNames = stateNames,
         obsFuncs = obsFuncs,
         freeParams = sort(unique(freeParams$list)),
         y0 = self$getInitialState(),
         events =  self$dosing_to_events()
     )
 })
