#' @description
#' Get initial states for simulation as a named vector.
#' @param named A boolean, should the initial states be named?
CompartmentModel$set("public", "getInitialState", function(named = TRUE) {
    y0 <- sapply(self$compartments, function(c) c$initial)
    if (named) {
        setNames(y0, nm = self$getStateNames())
    } else y0
})

#' @description
#' Get compartment names.
CompartmentModel$set("public", "getStateNames", function() {
    sapply(self$compartments, function(c) c$name)
})

#' @description
#' Add a compartment to the model.
#' @param name Name of the compartment
#' @param initial Initial amount (default 0)
CompartmentModel$set("public", "addCompartment", function(name, initial = 0) {
    self$compartments[[length(self$compartments) + 1]] <- Compartment$new(name, initial)
    invisible(self)
})

#' @description
#' Add a reaction to the model.
#' @param from Source compartment
#' @param to Target compartment
#' @param rate Rate expression as character
#' @param name Optional name
CompartmentModel$set("public", "addReaction", function(from, to, rate) {
    self$reactions[[length(self$reactions) + 1]] <- Reaction$new(from, to, rate)
    invisible(self)
})

#' @description
#' Add an observable.
#' @param name Name of the observable
#' @param expr Expression (character or function)
CompartmentModel$set("public", "addObservable", function(name, expr) {
    self$observables[[length(self$observables) + 1]] <- Observable$new(name, expr)
    invisible(self)
})

#' @description
#' Add a dosing event (bolus or infusion).
#' @param dose A Dosing object
CompartmentModel$set("public", "addDosing", function(dose) {
    if (dose$isBolus()) {
        # simple bolus, store in doses list
        if (is.null(self$doses)) self$doses <- list()
        self$doses[[length(self$doses) + 1]] <- dose
    } else if (dose$isInfusion()) {
        # ensure bag and rate compartments exist for target
        bagName <- paste0("InfusionBag_", dose$target)
        rateName <- paste0("InfusionRate_", dose$target)
        if (!(bagName %in% names(self$compartments))) self$addCompartment(bagName, 0)
        if (!(rateName %in% names(self$compartments))) self$addCompartment(rateName, 0)

        # add zero-order reaction from bag → target, rate = InfusionRate compartment
        self$addReaction(bagName, dose$target, paste0(rateName))

        # create bolus into the bag
        totalAmount <- dose$rate * dose$duration
        bolusToBag <- Dosing$new(bagName, amount = totalAmount, time = dose$time)

        # store both bolus-to-bag and infusion start/end events
        if (is.null(self$doses)) self$doses <- list()
        self$doses[[length(self$doses) + 1]] <- bolusToBag

        # infusion rate modification events
        if (is.null(self$infusionEvents)) self$infusionEvents <- data.frame(var=character(),
                                                                            time=numeric(),
                                                                            value=numeric(),
                                                                            method=character(),
                                                                            stringsAsFactors = FALSE)
        # start
        self$infusionEvents <- rbind(self$infusionEvents,
                                     data.frame(var=rateName, time=dose$time, value=dose$rate, method="add", stringsAsFactors = FALSE)
        )
        # end
        self$infusionEvents <- rbind(self$infusionEvents,
                                     data.frame(var=rateName, time=dose$time + dose$duration, value=-dose$rate, method="add", stringsAsFactors = FALSE)
        )
    } else {
        stop("Invalid dosing: either bolus or infusion with rate+duration")
    }
})

#' @description
#' Generate events data.frame for deSolve from stored dosing.
CompartmentModel$set("public", "dosing_to_events", function() {
    events <- data.frame(var=character(), time=numeric(), value=numeric(), method=character(),
                         stringsAsFactors = FALSE)

    # boluses
    if (!is.null(self$doses)) {
        for (d in self$doses) {
            events <- rbind(events,
                            data.frame(var=d$target, time=d$time, value=d$amount, method="add", stringsAsFactors = FALSE))
        }
    }

    # infusion rate events
    if (!is.null(self$infusionEvents)) {
        events <- rbind(events, self$infusionEvents)
    }

    # sort by time
    events <- events[order(events$time), ]
    list(data = events)
})

