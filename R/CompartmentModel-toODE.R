
#' @description
#' Generate ODE function, initial values, observables, and free parameters.
#' @param paramValues Named list of parameter values to inline in ODEs.
CompartmentModel$set("public", "toODE", function(paramValues = list()) {
     stateNames <- self$getStateNames()
     name2idx <- setNames(seq_along(stateNames), stateNames)

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
         expr <- makeFun(o$expr, obsFunc = TRUE)
         eval(parse(text = paste0("function(t,y,params) ", expr)))
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
