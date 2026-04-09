# Build drug database

#' Build a small drug database with example drugs and parameters
#' This function creates a small drug database with example drugs and their parameters. It is intended for testing and demonstration purposes.
#' @returns A list of `Drug` objects representing the drug database.
build_drugdb_old <- function() {
    drugs <- vector("list", length = 2)
    names(drugs) <- character(length(drugs))

    names(drugs)[1] <- "Amitriptyline"
    drugs[[1]] <- drug(
        class = "small molecule",
        formula = "C20H23N",
        MW = 277.4[g / mol],
        pKa_cat = 9.4,
        logPow = 4.9,
        fuP = c(rat = 0.056, human = 0.05),
        #        fuP = 0.056 %in% "rat",
        #        fuP = 0.05 %in% "human",
        BP = c(rat = 0.86, human = 0.86),
        CLblood_hep = c(human = 12)[mL / min / kg],
        #        CLblood_hep = 12[mL / min / kg] %in% "human",
        #        lambda_po = (log(2) / 1.36)[1 / h] %in% "human",
        Egut = 0.48 %in% "human"
    )

    names(drugs)[2] <- "Warfarin"
    drugs[[2]] <- drug(
        class = "small molecule",
        formula = "C19H16O4",
        MW = 308.33[g / mol],
        pKa_ani = 5.08,
        logPow = 3,
        fuP = c(rat = 0.02, human = 0.08),
        #        fuP = c(rat = 0.02, human = 0.08) %doi% "10.1007/BF01059331",
        #        fuP = 0.02 %in% "rat" %doi% "10.1007/BF01059331",
        #        fuP = 0.08 %in% "human" %doi% "10.1007/BF01059331",
        BP = c(rat = 0.58, human = 0.58),
        CLblood_hep = c(rat = 0.36, human = 0.081)[mL / min / kg],
        #        lambda_po = c(rat = 2 [1/h], human = 47 [1/day]),
        lambda_po = 47[1 / day] # %in% "human"
    )

    structure(drugs, class = "DrugList")
}

#' Different design idea, based on data frame and pivoting
#' @returns A `Drugs` object representing the drug database.
build_drugdb <- function() {
    # Build data frame with drug parameters
    df <- read.csv2("data-raw/drugdata.csv") |>
        dplyr::mutate(
            Value = purrr::pmap(
                list(Value, Unit),
                function(v, u) {
                    num <- suppressWarnings(as.numeric(v))

                    if (is.na(num)) {
                        # not numeric → character
                        v
                    } else if (is.na(u) || u == "") {
                        # numeric without unit
                        num
                    } else {
                        # numeric with unit
                        units::set_units(num, u, mode = "standard")
                    }
                }
            ),
            Unit = NULL
        )

    # TODO: Post-processing: (1) erythrocyte-to-plasma water partition coefficient, (2) intrinsic clearance

    # Return a Drugs object
    structure(df, class = c("Drugs","data.frame"))
}
