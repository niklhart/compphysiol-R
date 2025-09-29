
#' 12-CMT PBPK model with well-stirred tissue distribution
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
#'     BP = 1,
#'     CL = 5,
#'     co = 5,
#'     Kadi = 1,
#'     Kbon = 1,
#'     Kgut = 1,
#'     Khea = 1,
#'     Kkid = 1,
#'     Kliv = 1,
#'     Klun = 1,
#'     Kmus = 1,
#'     Kski = 1,
#'     Kspl = 1,
#'     Qadi = 0.5,
#'     Qbon = 0.5,
#'     Qgut = 0.5,
#'     Qhea = 0.5,
#'     Qkid = 0.5,
#'     Qliv = 1,
#'     Qmus = 0.5,
#'     Qski = 0.5,
#'     Qspl = 0.5,
#'     Vadi = 1,
#'     Vart = 1,
#'     Vbon = 1,
#'     Vgut = 1,
#'     Vhea = 1,
#'     Vkid = 1,
#'     Vliv = 1,
#'     Vlun = 1,
#'     Vmus = 1,
#'     Vski = 1,
#'     Vspl = 1,
#'     Vven = 1
#' )
#'
#' # Dosing
#' M$addDosing(Dosing$new(target = "ven", time = 0, amount = 1))
#'
#' # Add plasma concentration as an observable
#' M$addObservable("Cpla", "BP * ven / Vven")
#'
#' # Generate ODE function and auxiliary structures
#' odeinfo <- M$toODE(paramValues)
#'
#' # Run simulation
#' times <- seq(0, 24, by = 0.1)
#' out <- ode(
#'     y = odeinfo$y0,
#'     times = times,
#'     func = odeinfo$odefun,
#'     parms = paramValues,
#'     events = odeinfo$events
#' )
#'
#' out_df <- as.data.frame(out)
#'
#' # Plot compartment amounts
#' matplot(
#'     out_df$time,
#'     out_df[, -1],
#'     type = "l",
#'     lty = 1,
#'     col = 1:12,
#'     xlab = "Time (h)",
#'     ylab = "Amount",
#'     main = "PBPK Model Compartments"
#' )
#' legend(
#'     "topright",
#'     legend = colnames(out_df)[-1],
#'     col = 1:12,
#'     lty = 1
#' )
#'
#' # Evaluate observables at each time point
#' obs_df <- as.data.frame(sapply(odeinfo$obsFuncs, function(f) f(out_df$time, as.matrix(out_df[, -1]), paramValues)))
#' names(obs_df) <- names(odeinfo$obsFuncs)
#' obs_df$time <- out_df$time
#'
#' # Plot plasma concentration
#' plot(
#'     obs_df$time,
#'     obs_df$Cpla,
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

    # handle organ topology w.r.t. liver
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

#' Test model for sysdata workflow
#' @export
test_model_sysdata <- function() {
    .models$test_model_sysdata$clone(deep = TRUE)
}
