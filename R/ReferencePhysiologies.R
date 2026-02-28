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
#' param_list <- to_param_list(phys)
#' print(param_list)
human35m <- function() {

    physiology() |>
        add_scalar("BW", 70, "kg", reference = "Smith et al.", assumption = "Standard male") |>
        add_scalar("CO", 5.6, "L/min", reference = "Derived", assumption = "BW^0.75 scaling") |>
        add_tissue_param("Liver", "V", 1.5, "L", reference = "ICRP", assumption = "Fraction of BW") |>
        add_tissue_param("Liver", "Q", 1.35, "L/min", reference = "Brown et al.", assumption = "25% of CO")

}


#' Reference physiology of a young rat
#' @returns A `Physiology` object
#' @export
rat250 <- function() {
    .physiologies$rat250
}


#' Reference physiology of an old rat
#' @returns A `Physiology` object
#' @export
rat475 <- function() {
    .physiologies$rat475
}
