# method for prediction of tissue partitioning

#' Rodgers and Rowland's tissue partition prediction method
#'
#' Function `rodgersrowland` predicts tissue-to-unbound plasma partition
#' coefficients using the method described in Rodgers et al. (2005,2006).
#' Rodgers/Rowland
#' Input PHYS is a Physiology object, DRUG a DrugData object and ORGANS a
#' cellstr with the compartment names.
#' Output is a struct that can be appended to the initialized model.
#'
#' References:
#'
#' * Rodgers et al. (2005), DOI: 10.1002/jps.20322
#' * Rodgers/Rowland (2006), DOI: 10.1002/jps.20502
#'
#' @param phys An object of class `Physiology` which defines the following
#'      options for each tissue in argument `organs`:
#' @param drug An object of class `DrugData`.
#' @param organs A character array of valid organ (compartment) names
#'      (e.g. `"adi"`,`"bon"`,...).
#' @param plasmaWaterFraction A value between 0 and 1 (default: `0.93`)
#' @param fupIncludesLipids A boolean (default: `TRUE`)
#' @param treatNegativeBindingAsZero A boolean (default: `FALSE`)
#' @param respectThermodynamics A boolean (default: `TRUE`)
#' @returns TO BE DISCUSSED
#' @export
rodgersrowland <- function(phys, drug, organs, plasmaWaterFraction = 0.93,
                           fupIncludesLipids = TRUE, treatNegativeBindingAsZero = FALSE,
                           respectThermodynamics = TRUE) {

    stop('Implement this')

}
