
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
    M$addReaction("art", "adi", const = "Qadi / Vart")
    M$addReaction("art", "bon", const = "Qbon / Vart")
    M$addReaction("art", "gut", const = "Qgut / Vart")
    M$addReaction("art", "hea", const = "Qhea / Vart")
    M$addReaction("art", "mus", const = "Qmus / Vart")
    M$addReaction("art", "kid", const = "Qkid / Vart")
    M$addReaction("art", "ski", const = "Qski / Vart")
    M$addReaction("art", "spl", const = "Qspl / Vart")

    # handle organ topology w.r.t. liver
    M$addReaction("art", "liv", const = "(Qliv-Qgut-Qspl) / Vart")
    M$addReaction("gut", "liv", const = "Qgut / (Vgut * Kgut)")
    M$addReaction("spl", "liv", const = "Qspl / (Vspl * Kspl)")

    # organs with venous outflow
    M$addReaction("adi", "ven", const = "Qadi / (Vadi * Kadi)")
    M$addReaction("bon", "ven", const = "Qbon / (Vbon * Kbon)")
    M$addReaction("hea", "ven", const = "Qhea / (Vhea * Khea)")
    M$addReaction("liv", "ven", const = "Qliv / (Vliv * Kliv)")
    M$addReaction("mus", "ven", const = "Qmus / (Vmus * Kmus)")
    M$addReaction("kid", "ven", const = "Qkid / (Vkid * Kkid)")
    M$addReaction("ski", "ven", const = "Qski / (Vski * Kski)")

    # lung (co hardcoded currently)
    M$addReaction(
        "ven",
        "lun",
        const = "(Qadi+Qbon+Qhea+Qliv+Qmus+Qkid+Qski) / Vven"
    )
    M$addReaction(
        "lun",
        "art",
        const = "(Qadi+Qbon+Qhea+Qliv+Qmus+Qkid+Qski) / (Vlun * Klun)"
    )

    # liver metabolism
    M$addReaction("liv", "", const = "CL / (Vliv * Kliv)")

    # output
    M
}


#' Create a generic multi-compartment PK model
#'
#' @param ncomp Number of compartments (>=1)
#' @param type Style of parameterization: "micro" (rate constants) or "macro" (volumes + clearances)
#' @return A CompartmentModel object
#' @export
multiCompModel <- function(ncomp = 1, type = c("micro", "macro")) {
    type <- match.arg(type)
    stopifnot(ncomp >= 1)

    model <- CompartmentModel$new()

    # Compartment names: central is C1
    compNames <- paste0("C", seq_len(ncomp))
    central_idx <- 1

    # Add compartments
    for (c in compNames) {
        model$addCompartment(c, initial = 0)
    }

    # Add plasma concentration observable
    model$addObservable("C1Conc", "C1/V1")

    if (type == "micro") {
        # Micro: first-order rate constants
        # Elimination from central
        model$addReaction("C1", "", "k10 * C1")
        # Inter-compartmental rates
        for (i in seq_len(ncomp)) {
            if (i == central_idx) next
            ki <- paste0("k1", i)
            k_back <- paste0("k", i, "1")
            model$addReaction("C1", compNames[i], paste0(ki, " * C1"))
            model$addReaction(compNames[i], "C1", paste0(k_back, " * ", compNames[i]))
        }
    } else if (type == "macro") {
        # Macro: flows and volumes
        # Elimination from central scaled by volume
        model$addReaction("C1", "", "CL / V1 * C1")
        # Inter-compartmental flows
        for (i in seq_len(ncomp)) {
            if (i == central_idx) next
            model$addReaction(
                "C1", compNames[i],
                paste0("Q1", i, " / V1 * C1")
            )
            model$addReaction(
                compNames[i], "C1",
                paste0("Q1", i, " / V", i, " * ", compNames[i])
            )
        }
    }

    model
}




#' Cell-level PK/PD model
celllevel_pkpd <- function() {
    M <- CompartmentModel$new()

    # Compartments
    M$addCompartment("pla", 0)
    M$addCompartment("int", 0)
    M$addCompartment("R", 0)
    M$addCompartment("Ri", 0)
    M$addCompartment("RL", 0)
    M$addCompartment("RC", 0)

    # Reactions
    M$addReaction("pla", "int", "qpi * pla/Vpla")
    M$addReaction("int", "pla", "qip * int/Vint")
    M$addReaction("pla", NULL,  "CLlin * pla/Vpla")


    M$addReaction("R", "RL", "kon * R * L")
    M$addReaction("RL", "R", "koff * RL")
    M$addReaction("RL", "RC", "kint * RL")
    M$addReaction("RC", "Ri", "kdeg * RC")
    M$addReaction("Ri", "R", "krecycle * Ri")


    # Observable: effect site concentration
    M$addObservable("Cpla", "pla / Vpla")
    M$addObservable("Cint", "int / Vint")

    M
}

#' Receptor binding and internalization module
#' 
#' TODO: parametrize initial condition (initialize at steady-state -- ligand-free??)
#' 
#' @param ligands Character vector of ligand names (default: "L")
#' @param dynamic Whether to include ligand dynamics (default: TRUE). If FALSE, ligands 
#'     are treated as a (constant) parameter instead of a compartment.
#' @return A CompartmentModel object
#' @export
receptor_system <- function(ligands = "L", dynamic = TRUE) {
    M <- CompartmentModel$new()

    # Receptor compartments  
    M$addCompartment("R", 0)      # free receptor
    M$addCompartment("Ri", 0)     # internalized receptor
    
    # Receptor turnover (synthesis-recycling-degradation)
    M$addReaction(NULL, "R", "ksynR")
    M$addReaction("R", "Ri", "kdegR*R")
    M$addReaction("Ri", "R", "krecyRi * Ri")
    M$addReaction("Ri", NULL, "kdegRi * Ri")
  
    # Ligand-related compartments
    dynamic <- rep_len(dynamic, length.out = length(ligands))
    for (i in seq_along(ligands)) {
        lig <- ligands[i]
        includeLigandDynamics <- dynamic[i]
        RLcpx <- paste0("R",lig)  
        M$addCompartment(RLcpx, 0)  # natural ligand  
        if (includeLigandDynamics){
            M$addCompartment(lig, 0)  # dynamic ligand compartment
            RLind <- c("R",lig) 

        } else {
            RLind <- c("R",lig) 
        }
      
        M$addReaction(RLind, RLcpx, paste0("kon",lig," * R * ",lig))
        M$addReaction(RLcpx, RLind, paste0("koff",RLcpx," * ",RLcpx))
        M$addReaction(RLcpx, NULL,  paste0("kdeg",RLcpx," * ",RLcpx))
    }

    M
}

#' Empirical PK model relative to number of receptors
#' 
#' 
#' 
#' @return A CompartmentModel object
#' @export
empirical_pk_receptor <- function() {
    M <- CompartmentModel$new()

    # Compartments
    M$addCompartment("pla", 0)
    M$addCompartment("int", 0)

    # Reactions
    M$addReaction("pla", "int", "qpi * pla/Vpla")
    M$addReaction("int", "pla", "qip * int/Vint")
    M$addReaction("pla", NULL,  "CLlin * pla/Vpla")
    M$addReaction("int", NULL,  "CLrec * R * int/Vint")

    # Observable: effect site concentration
    M$addObservable("Cpla", "pla / Vpla")
    M$addObservable("Cint", "int / Vint")

    M
}



#' Lammerts van Bueren model
#' 
lammertsvanbueren <- function() {
    M <- CompartmentModel$new()

    # Compartments
    M$addCompartment("pla", 0)
    M$addCompartment("int", 0)
    M$addCompartment("rec", 0)

    # Reactions
    M$addReaction("pla", "int", "qpi * pla/Vpla")
    M$addReaction("int", "pla", "qip * int/Vint")
    M$addReaction("pla", NULL,  "CLlin * pla/Vpla")
    M$addReaction("int", "rec", "kb*BmaxPK*(int/Vint)/(KMPK+int/Vint)")
    M$addReaction("rec", "int", "kb*rec")

    # Observable: effect site concentration
    M$addObservable("Cpla", "pla / Vpla")
    M$addObservable("Cint", "int / Vint")

    M
}


sMD_PBPK_xCMT_lumped <- function(partitioning, autonormalize = TRUE) {

    M <- sMD_PBPK_12CMT_wellstirred()
    # General lumping logic

    if (autonormalize) {
        normalize_arg <- get_lumping_conditions(M, refstate = "ven", simplify = "Ryacas")
    } else {
        normalize_arg <- list(
            ven = "Vven",
            art = "Vart",
            adi = "Vadi*Kadi",
            bon = "Vbon*Kbon",
            gut = "Vgut*Kgut",
            hea = "Vhea*Khea",
            mus = "Vmus*Kmus",
            kid = "Vkid*Kkid",
            liv = "Vliv*Kliv*Q/(Q+CL)",
            lun = "Vlun*Klun",
            ski = "Vski*Kski",
            spl = "Vspl*Kspl"
        )
    }
    L <- lump_model(
        M,
        partitioning = partitioning,
        normalize = normalize_arg
    )
    # Specific for first-pass effect
}
