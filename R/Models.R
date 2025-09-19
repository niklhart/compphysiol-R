
#' Pre-built 12-CMT PBPK model with well-stirred tissue distribution
#'
#' This example demonstrates a full workflow from loading the model,
#' setting parameters, running a simulation, and plotting compartment amounts
#' and plasma concentration.
#'
#' @return An R6 object of class `CompartmentModel`
#' @export
#' @examples
#' \donttest{
#' library(compphysiol)
#' library(deSolve)
#'
#' # Load the PBPK model
#' M <- sMD_PBPK_12CMT_wellstirred()
#'
#' # Define parameters
#' paramValues <- list(
#'     Q_L = 1.5, V_C = 5, V_L = 2,
#'     Q_K = 1.0, V_K = 1,
#'     Vc = 5  # volume of central compartment for concentration observable
#' )
#'
#' # Add an observable: plasma concentration = Central / Vc
#' M$addObservable("Plasma", "Central / Vc")
#'
#' # Generate ODE function and observables
#' odeinfo <- M$toODE(paramValues)
#'
#' # Initial state vector
#' y0 <- M$getInitialState()
#'
#' # Dosing events
#' events <- M$dosing_to_events()$data
#'
#' # Run simulation
#' times <- seq(0, 24, by = 0.1)
#' out <- ode(
#'     y = y0,
#'     times = times,
#'     func = odeinfo$odefun,
#'     parms = paramValues,
#'     events = list(data = events)
#' )
#'
#' out_df <- as.data.frame(out)
#'
#' # Evaluate observables at each time point
#' obs_df <- as.data.frame(sapply(odeinfo$obsFuncs, function(f) f(out_df$time, as.matrix(out_df[, -1]), paramValues)))
#' obs_df$time <- out_df$time
#'
#' # Plot compartment amounts
#' matplot(
#'     out_df$time,
#'     out_df[, -1],
#'     type = "l",
#'     lty = 1,
#'     col = c("blue", "green", "red"),
#'     xlab = "Time (h)",
#'     ylab = "Amount",
#'     main = "PBPK Model Compartments"
#' )
#' legend(
#'     "topright",
#'     legend = colnames(out_df)[-1],
#'     col = c("blue", "green", "red"),
#'     lty = 1
#' )
#'
#' # Plot plasma concentration
#' plot(
#'     obs_df$time,
#'     obs_df$Plasma,
#'     type = "l",
#'     col = "black",
#'     lwd = 2,
#'     xlab = "Time (h)",
#'     ylab = "Plasma concentration",
#'     main = "Plasma Concentration"
#' )
#' }
sMD_PBPK_12CMT_wellstirred <- function() {
    M <- CompartmentModel$new()

    # compartments (can be vectorized later)
    M$addCompartment("ven", 0)
    M$addCompartment("art", 0)

    M$addCompartment("adi", 0)
    M$addCompartment("bon", 0)
    M$addCompartment("gut", 0)
    M$addCompartment("hea", 0)
    M$addCompartment("mus", 0)
    M$addCompartment("kid", 0)
    M$addCompartment("liv", 0)
    M$addCompartment("lun", 0)
    M$addCompartment("ski", 0)
    M$addCompartment("spl", 0)

    # organs with arterial inflow
    M$addReaction("art", "adi", "Qadi * art / Vart", "art2adi")
    M$addReaction("art", "bon", "Qbon * art / Vart", "art2bon")
    M$addReaction("art", "gut", "Qgut * art / Vart", "art2gut")
    M$addReaction("art", "hea", "Qhea * art / Vart", "art2hea")
    M$addReaction("art", "mus", "Qmus * art / Vart", "art2mus")
    M$addReaction("art", "kid", "Qkid * art / Vart", "art2kid")
    M$addReaction("art", "ski", "Qski * art / Vart", "art2ski")
    M$addReaction("art", "spl", "Qspl * art / Vart", "art2spl")

    # handle liver topology
    M$addReaction("art", "liv", "(Qliv-Qgut-Qspl) * art / Vart", "art2liv")
    M$addReaction("gut", "liv", "Qgut * gut / (Vgut * Kgut)",    "gut2liv")
    M$addReaction("spl", "liv", "Qspl * spl / (Vspl * Kspl)",    "spl2liv")

    # organs with venous outflow
    M$addReaction("adi", "ven", "Qadi * adi / (Vadi * Kadi)", "adi2ven")
    M$addReaction("bon", "ven", "Qbon * bon / (Vbon * Kbon)", "bon2ven")
    M$addReaction("hea", "ven", "Qhea * hea / (Vhea * Khea)", "hea2ven")
    M$addReaction("liv", "ven", "Qliv * liv / (Vliv * Kliv)", "liv2ven")
    M$addReaction("mus", "ven", "Qmus * mus / (Vmus * Kmus)", "mus2ven")
    M$addReaction("kid", "ven", "Qkid * kid / (Vkid * Kkid)", "kid2ven")
    M$addReaction("ski", "ven", "Qski * ski / (Vski * Kski)", "ski2ven")

    # lung
    M$addReaction("ven", "lun", "co * ven / Vven", "ven2lun")
    M$addReaction("lun", "art", "co * lun / (Vlun * Klun)", "lun2art")

    # liver metabolism
    M$addReaction("liv", "", "CL * liv / (Vliv * Kliv)", "liv2_")

    # output
    M
}
