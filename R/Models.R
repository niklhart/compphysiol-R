
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
#' # Load the PBPK model
#' M <- sMD_PBPK_12CMT_wellstirred() |>
#'     add_dosing(target = "ven", time = 0, amount = 1) |>   # IV bolus dosing
#'     add_observable(Cpla = BP * ven / Vven)                # plasma concentration observable
#'
#' paramValues <- list(
#'     BP = 1,
#'     CL = 5,
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
#' # Generate ODE function and auxiliary structures
#' odeinfo <- to_ode(M, paramValues)
#'
#' # Run simulation
#' times <- seq(0, 24, by = 0.1)
#' out <- deSolve::ode(
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
#' obs_df <- as.data.frame(sapply(
#'     odeinfo$obsFuncs,
#'     function(f) f(out_df$time, as.matrix(out_df[, -1]), paramValues)
#' ))
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

    compartment_model() |>
        add_compartment(name = c("ven", "art", "adi", "bon", "gut", "hea", "mus", "kid", "liv", "lun", "ski", "spl")) |>
        add_flow(from = "art", to = c("adi", "bon", "gut", "hea", "mus", "kid", "ski", "spl"), const = "Q_to / Vart") |>
        add_flow(from = "art", to = "liv", const = "(Qliv-Qgut-Qspl) / Vart") |> 
        add_flow(from = c("gut","spl"), to = "liv", const = "Q_from / (V_from * K_from)") |>
        add_flow(from = c("adi","bon","hea","liv","mus","kid","ski"), to = "ven", const = "Q_from / (V_from * K_from)") |>
        add_flow(from = "ven", to = "lun", const = "(Qadi+Qbon+Qhea+Qliv+Qmus+Qkid+Qski) / Vven") |>
        add_flow(from = "lun", to = "art", const = "(Qadi+Qbon+Qhea+Qliv+Qmus+Qkid+Qski) / (Vlun * Klun)") |>
        add_flow(from = "liv", to = "", const = "CL / (Vliv * Kliv)")
}


#' 12-CMT PBPK model with permeation-based tissue distribution
#'
#' This example demonstrates a full workflow from loading the model,
#' setting parameters, running a simulation, and plotting compartment amounts
#' and plasma concentration.
#'
#' @return An R6 object of class `CompartmentModel`
#' @export
#' @examples
#' \donttest{
#' # Load the PBPK model
#' M <- sMD_PBPK_12CMT_permbased() |>
#'     add_dosing(target = "ven", time = 0, amount = 1) |>  # IV bolus dosing
#'     add_observable(Cpla = BP * ven / Vven)               # plasma concentration observable
#'
#' paramValues <- list(
#'     BP = 1,
#'     CL = 5,
#'     P = 1,
#'     fuP = 1,
#'     fuCadi = 1,
#'     fuCbon = 1,
#'     fuCgut = 1,
#'     fuChea = 1,
#'     fuCkid = 1,
#'     fuCliv = 1,
#'     fuClun = 1,
#'     fuCmus = 1,
#'     fuCski = 1,
#'     fuCspl = 1,
#'     fuEadi = 1,
#'     fuEbon = 1,
#'     fuEgut = 1,
#'     fuEhea = 1,
#'     fuEkid = 1,
#'     fuEliv = 1,
#'     fuElun = 1,
#'     fuEmus = 1,
#'     fuEski = 1,
#'     fuEspl = 1,
#'     Qadi = 0.5,
#'     Qbon = 0.5,
#'     Qgut = 0.5,
#'     Qhea = 0.5,
#'     Qkid = 0.5,
#'     Qliv = 1,
#'     Qmus = 0.5,
#'     Qski = 0.5,
#'     Qspl = 0.5,
#'     SAadi = 1,
#'     SAbon = 1,
#'     SAgut = 1,
#'     SAhea = 1,
#'     SAmus = 1,
#'     SAkid = 1,
#'     SAliv = 1,
#'     SAlun = 1,
#'     SAski = 1,
#'     SAspl = 1,
#'     Vadi_exc = 1,
#'     Vadi_cel = 1,
#'     Vbon_exc = 1,
#'     Vbon_cel = 1,
#'     Vgut_exc = 1,
#'     Vgut_cel = 1,
#'     Vhea_exc = 1,
#'     Vhea_cel = 1,
#'     Vmus_exc = 1,
#'     Vmus_cel = 1,
#'     Vkid_exc = 1,
#'     Vkid_cel = 1,
#'     Vliv_exc = 1,
#'     Vliv_cel = 1,
#'     Vlun_exc = 1,
#'     Vlun_cel = 1,
#'     Vmus_exc = 1,
#'     Vmus_cel = 1,
#'     Vski_exc = 1,
#'     Vski_cel = 1,
#'     Vspl_exc = 1,
#'     Vspl_cel = 1,
#'     Vart = 1,
#'     Vven = 1
#' )
#'
#' # Generate ODE function and auxiliary structures
#' odeinfo <- to_ode(M, paramValues)
#'
#' # Run simulation
#' times <- seq(0, 24, by = 0.1)
#' out <- deSolve::ode(
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
#' obs_df <- as.data.frame(sapply(
#'     odeinfo$obsFuncs,
#'     function(f) f(out_df$time, as.matrix(out_df[, -1]), paramValues)
#' ))
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
sMD_PBPK_12CMT_permbased <- function() {

    compartment_model() |>
        add_compartment(
            c(
                "ven",
                "art",
                "adi_exc",
                "adi_cel",
                "bon_exc",
                "bon_cel",
                "gut_exc",
                "gut_cel",
                "hea_exc",
                "hea_cel",
                "mus_exc",
                "mus_cel",
                "kid_exc",
                "kid_cel",
                "liv_exc",
                "liv_cel",
                "lun_exc",
                "lun_cel",
                "ski_exc",
                "ski_cel",
                "spl_exc",
                "spl_cel"
            )
        ) |>
        # organs with arterial inflow
        add_flow("art", "adi_exc", const = "Qadi / Vart") |>
        add_flow("art", "bon_exc", const = "Qbon / Vart") |>
        add_flow("art", "gut_exc", const = "Qgut / Vart") |>
        add_flow("art", "hea_exc", const = "Qhea / Vart") |>
        add_flow("art", "mus_exc", const = "Qmus / Vart") |>
        add_flow("art", "kid_exc", const = "Qkid / Vart") |>
        add_flow("art", "ski_exc", const = "Qski / Vart") |>
        add_flow("art", "spl_exc", const = "Qspl / Vart") |>
        # handle organ topology w.r.t. liver
        add_flow("art", "liv_exc", const = "(Qliv-Qgut-Qspl) / Vart") |>
        add_flow("gut_exc", "liv_exc", const = "Qgut * BP * fuEgut / (Vgut_exc * fuP)") |>
        add_flow("spl_exc", "liv_exc", const = "Qspl * BP * fuEspl / (Vspl_exc * fuP)") |>
        # organs with venous outflow
        add_flow("adi_exc", "ven", const = "Qadi * BP * fuEadi / (Vadi_exc * fuP)") |>
        add_flow("bon_exc", "ven", const = "Qbon * BP * fuEbon / (Vbon_exc * fuP)") |>
        add_flow("hea_exc", "ven", const = "Qhea * BP * fuEhea / (Vhea_exc * fuP)") |>
        add_flow("liv_exc", "ven", const = "Qliv * BP * fuEliv / (Vliv_exc * fuP)") |>
        add_flow("mus_exc", "ven", const = "Qmus * BP * fuEmus / (Vmus_exc * fuP)") |>
        add_flow("kid_exc", "ven", const = "Qkid * BP * fuEkid / (Vkid_exc * fuP)") |>
        add_flow("ski_exc", "ven", const = "Qski * BP * fuEski / (Vski_exc * fuP)") |>
        # Redistribution within tissues
        add_flow("adi_exc", "adi_cel", const = "P * SAadi * fuEadi / Vadi_exc") |>
        add_flow("adi_cel", "adi_exc", const = "P * SAadi * fuCadi / Vadi_cel") |>
        add_flow("bon_exc", "bon_cel", const = "P * SAbon * fuEbon / Vbon_exc") |>
        add_flow("bon_cel", "bon_exc", const = "P * SAbon * fuCbon / Vbon_cel") |>
        add_flow("gut_exc", "gut_cel", const = "P * SAgut * fuEgut / Vgut_exc") |>
        add_flow("gut_cel", "gut_exc", const = "P * SAgut * fuCgut / Vgut_cel") |>
        add_flow("hea_exc", "hea_cel", const = "P * SAhea * fuEhea / Vhea_exc") |>
        add_flow("hea_cel", "hea_exc", const = "P * SAhea * fuChea / Vhea_cel") |>
        add_flow("kid_exc", "kid_cel", const = "P * SAkid * fuEkid / Vkid_exc") |>
        add_flow("kid_cel", "kid_exc", const = "P * SAkid * fuCkid / Vkid_cel") |>
        add_flow("liv_exc", "liv_cel", const = "P * SAliv * fuEliv / Vliv_exc") |>
        add_flow("liv_cel", "liv_exc", const = "P * SAliv * fuCliv / Vliv_cel") |>
        add_flow("lun_exc", "lun_cel", const = "P * SAlun * fuElun / Vlun_exc") |>
        add_flow("lun_cel", "lun_exc", const = "P * SAlun * fuClun / Vlun_cel") |>
        add_flow("mus_exc", "mus_cel", const = "P * SAmus * fuEmus / Vmus_exc") |>
        add_flow("mus_cel", "mus_exc", const = "P * SAmus * fuCmus / Vmus_cel") |>
        add_flow("ski_exc", "ski_cel", const = "P * SAadi * fuEski / Vski_exc") |>
        add_flow("ski_cel", "ski_exc", const = "P * SAadi * fuCski / Vski_cel") |>
        add_flow("spl_exc", "spl_cel", const = "P * SAspl * fuEspl / Vspl_exc") |>
        add_flow("spl_cel", "spl_exc", const = "P * SAspl * fuCspl / Vspl_cel") |>
        # lung (co hardcoded currently)
        add_flow("ven", "lun_exc", const = "(Qadi+Qbon+Qhea+Qliv+Qmus+Qkid+Qski) / Vven") |>
        add_flow("lun_exc", "art", const = "(Qadi+Qbon+Qhea+Qliv+Qmus+Qkid+Qski) * BP * fuElun / (Vlun_exc * fuP)") |>
        # liver metabolism
        add_flow("liv_cel", "", const = "CL * fuCliv / Vliv_cel")
}


#' Create a generic multi-compartment PK model
#'
#' @param ncomp Number of compartments (>=1)
#' @param type Style of parameterization: "micro" (rate constants) or "macro" (volumes + clearances)
#' @return A `CompartmentModel` object
#' @examples 
#' multiCompModel(ncomp = 3, type = "macro")
#' @export
multiCompModel <- function(ncomp = 1, type = c("micro", "macro")) {
    type <- match.arg(type)
    stopifnot(ncomp >= 1)

    # Compartment names: central is C1
    compNames <- paste0("C", seq_len(ncomp))
    central_idx <- 1

    # First assemble compartments and observables
    model <- compartment_model() |>
        add_compartment(compNames, initial = 0) |>
        add_observable(obs = observables(name = "C1Conc", expr = "C1/V1"))

    # Next assemble reactions, depending on parameterization style
    if (type == "micro") {
        # Micro: first-order rate constants
        # Elimination from central
        model <- add_flow(model, "C1", "", const = "k10")
        # Inter-compartmental rates
        for (i in seq_len(ncomp)) {
            if (i == central_idx) {
                next
            }
            ki <- paste0("k1", i)
            k_back <- paste0("k", i, "1")
            model <- model |>
                add_flow("C1", compNames[i], const = ki) |>
                add_flow(compNames[i], "C1", const = k_back)
        }
    } else if (type == "macro") {
        # Macro: flows and volumes
        # Elimination from central scaled by volume
        model <- add_flow(model, "C1", "", const = "CL / V1")
        # Inter-compartmental flows
        for (i in seq_len(ncomp)) {
            if (i == central_idx) {
                next
            }
            model <- model |>
                add_flow("C1", compNames[i], const = paste0("Q1", i, " / V1")) |>
                add_flow(compNames[i], "C1", const = paste0("Q1", i, " / V", i))
        }
    }

    model
}




# Old (R6) models to be refactored or removed

# #' Cell-level PK/PD model
# celllevel_pkpd <- function() {

#     CompartmentModel$
#         new()$
#         addCompartment("pla", 0)$
#         addCompartment("int", 0)$
#         addCompartment("R", 0)$
#         addCompartment("Ri", 0)$
#         addCompartment("RL", 0)$
#         addCompartment("RC", 0)$
#         addReaction("pla", "int", "qpi * pla/Vpla")$
#         addReaction("int", "pla", "qip * int/Vint")$
#         addReaction("pla", NULL,  "CLlin * pla/Vpla")$
#         addReaction("R", "RL", "kon * R * L")$
#         addReaction("RL", "R", "koff * RL")$
#         addReaction("RL", "RC", "kint * RL")$
#         addReaction("RC", "Ri", "kdeg * RC")$
#         addReaction("Ri", "R", "krecycle * Ri")$
#         addObservable("Cpla", "pla / Vpla")$
#         addObservable("Cint", "int / Vint")

# }

# #' Receptor binding and internalization module
# #' 
# #' TODO: parametrize initial condition (initialize at steady-state -- ligand-free??)
# #' 
# #' @param ligands Character vector of ligand names (default: "L")
# #' @param dynamic Whether to include ligand dynamics (default: TRUE). If FALSE, ligands 
# #'     are treated as a (constant) parameter instead of a compartment.
# #' @return A CompartmentModel object
# #' @export
# receptor_system <- function(ligands = "L", dynamic = TRUE) {
    
#     M <- CompartmentModel$new()

#     # Receptor compartments  
#     M$addCompartment("R", 0)      # free receptor
#     M$addCompartment("Ri", 0)     # internalized receptor
    
#     # Receptor turnover (synthesis-recycling-degradation)
#     M$addReaction(NULL, "R", "ksynR")
#     M$addReaction("R", "Ri", "kdegR*R")
#     M$addReaction("Ri", "R", "krecyRi * Ri")
#     M$addReaction("Ri", NULL, "kdegRi * Ri")
  
#     # Ligand-related compartments
#     dynamic <- rep_len(dynamic, length.out = length(ligands))
#     for (i in seq_along(ligands)) {
#         lig <- ligands[i]
#         includeLigandDynamics <- dynamic[i]
#         RLcpx <- paste0("R",lig)  
#         M$addCompartment(RLcpx, 0)  # natural ligand  
#         if (includeLigandDynamics){
#             M$addCompartment(lig, 0)  # dynamic ligand compartment
#             RLind <- c("R",lig) 

#         } else {
#             RLind <- c("R",lig) 
#         }
      
#         M$addReaction(RLind, RLcpx, paste0("kon",lig," * R * ",lig))
#         M$addReaction(RLcpx, RLind, paste0("koff",RLcpx," * ",RLcpx))
#         M$addReaction(RLcpx, NULL,  paste0("kdeg",RLcpx," * ",RLcpx))
#     }

#     M
# }

# #' Empirical PK model relative to number of receptors
# #' 
# #' 
# #' 
# #' @return A CompartmentModel object
# #' @export
# empirical_pk_receptor <- function() {
#     CompartmentModel$new()$
#         addCompartment("pla", 0)$
#         addCompartment("int", 0)$
#         addReaction("pla", "int", "qpi * pla/Vpla")$
#         addReaction("int", "pla", "qip * int/Vint")$
#         addReaction("pla", NULL,  "CLlin * pla/Vpla")$
#         addReaction("int", NULL,  "CLrec * R * int/Vint")$
#         addObservable("Cpla", "pla / Vpla")$
#         addObservable("Cint", "int / Vint")
# }



# #' Lammerts van Bueren model
# #' 
# lammertsvanbueren <- function() {
   
#     CompartmentModel$
#         new()$
#         addCompartment("pla", 0)$
#         addCompartment("int", 0)$
#         addCompartment("rec", 0)$
#         addReaction("pla", "int", "qpi * pla/Vpla")$
#         addReaction("int", "pla", "qip * int/Vint")$
#         addReaction("pla", NULL,  "CLlin * pla/Vpla")$
#         addReaction("int", "rec", "kb*BmaxPK*(int/Vint)/(KMPK+int/Vint)")$
#         addReaction("rec", "int", "kb*rec")$
#         addObservable("Cpla", "pla / Vpla")$
#         addObservable("Cint", "int / Vint")
# }

# #' Lumped PBPK model based on sMD_PBPK_12CMT_wellstirred
# #' @param partitioning A named list defining the lumping scheme
# #' @param autonormalize Whether to automatically normalize the lumped volumes
# #' @return A CompartmentModel object
# #' @export
# sMD_PBPK_xCMT_lumped <- function(partitioning, autonormalize = TRUE) {

#     M <- sMD_PBPK_12CMT_wellstirred()

#     if (autonormalize) {
#         normalize_arg <- get_lumping_conditions(M, refstate = "ven", simplify = "Ryacas")
#     } else {
#         normalize_arg <- list(
#             ven = "Vven",
#             art = "Vart",
#             adi = "Vadi*Kadi",
#             bon = "Vbon*Kbon",
#             gut = "Vgut*Kgut",
#             hea = "Vhea*Khea",
#             mus = "Vmus*Kmus",
#             kid = "Vkid*Kkid",
#             liv = "Vliv*Kliv*Q/(Q+CL)",
#             lun = "Vlun*Klun",
#             ski = "Vski*Kski",
#             spl = "Vspl*Kspl"
#         )
#     }
#     L <- lump_model(
#         M,
#         partitioning = partitioning,
#         normalize = normalize_arg
#     )
#     # Specific for first-pass effect
#     warning(
#         "Lumping with first-pass metabolism: please verify that the lumped model is correct."
#     )
# }
