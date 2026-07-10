# method for prediction of tissue partitioning

#' Rodgers and Rowland's tissue partition prediction method (not implemented)
#'
#' This function is a placeholder for predicting tissue-to-unbound plasma
#' partition coefficients using the method described in Rodgers et al.
#' (2005, 2006). It is not implemented yet and currently errors when called.
#'
#' References:
#'
#' * Rodgers et al. (2005), DOI: 10.1002/jps.20322
#' * Rodgers/Rowland (2006), DOI: 10.1002/jps.20502
#'
#' @param phys An object of class `Physiology`.
#' @param drug An object of class `Drug`.
#' @param organs A character array of valid organ (compartment) names
#'      (e.g. `"adi"`,`"bon"`,...).
#' @param plasmaWaterFraction A value between 0 and 1 (default: `0.93`)
#' @param fupIncludesLipids A boolean (default: `TRUE`)
#' @param treatNegativeBindingAsZero A boolean (default: `FALSE`)
#' @param respectThermodynamics A boolean (default: `TRUE`)
#' @returns This function is not implemented yet and currently errors.
#' @noRd
rodgersrowland <- function(phys, drug, organs, plasmaWaterFraction = 0.93,
                           fupIncludesLipids = TRUE, treatNegativeBindingAsZero = FALSE,
                           respectThermodynamics = TRUE) {

    stop("rodgersrowland() is not implemented yet.")

}
