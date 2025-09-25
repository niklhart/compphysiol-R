#' Reference physiology of a male adult human
#'
#' @export
#' @examples
#' # Load reference physiology
#' phys <- human35m()
#'
#' # Inspect table
#' phys$param_table
#'
#' # Export for model
#' param_list <- phys$to_param_list()
#' print(param_list)
human35m <- function() {

    # Create physiology object
    phys <- Physiology$new()

    # Add scalars
    phys$add_scalar("BW", 70, "kg", reference="Smith et al.", assumption="Standard male")
    phys$add_scalar("CO", 5.6, "L/min", reference="Derived", assumption="BW^0.75 scaling")

    # Add tissue parameters
    phys$add_tissue_param("Liver", "V", 1.5, "L", reference="ICRP", assumption="Fraction of BW")
    phys$add_tissue_param("Liver", "Q", 1.35, "L/min", reference="Brown et al.", assumption="25% of CO")

    phys
}

