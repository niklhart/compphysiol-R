
#' Simulate a covariate distribution in human adults
#'
#' Experimental internal prototype for realized human populations.
#'
#' Generate pairs of body height (BH) and body weight (BW) values for a specified number of individuals.
#' 
#' To generate pairs of (BH,BW) values, independent (BH, BMI) pairs are first generated, assuming
#' BH  ~ Normal(muBH,sdBH^2)
#' BMI ~ logNormal(log_muBMI,log_sdBMI^2)
#' These are then used to generate BW from the relation BMI = BW/BH^2.
#'
#' Reference: de la Grandmaison et al., Forensic Sci. Int. 119 (2001): 149-154
#'
#' @param N Number of individuals (positive integer)
#' @param sex Character, either "male" or "female"
#' @returns An unnamed length `N` `ParameterSets` collection.
#' @examples
#' humans <- humancovdistrib(5, "female")
#' @noRd
humancovdistrib <- function(N, sex) {
    if (!is.numeric(N) || length(N) != 1L || is.na(N) || !is.finite(N) ||
        N <= 0 || N != as.integer(N)) {
        stop("N must be a positive integer", call. = FALSE)
    }
    sex <- match.arg(sex, c("male", "female"))

    # --- Set mean and SD for height and BMI (de la Grandmaison et al.) ---
    switch(sex,
           male = { muBH <- 1.72; sdBH <- 0.075; muBMI <- 22.8; sdBMI <- 3.3 },
           female = { muBH <- 1.61; sdBH <- 0.075; muBMI <- 22.5; sdBMI <- 4.5 }
    )

    # Transform mu and sd to log-normal scale
    log_muBMI <- log(muBMI^2 / sqrt(muBMI^2 + sdBMI^2))
    log_sdBMI <- sqrt(log(sdBMI^2 / muBMI^2 + 1))

    # Generate height (BH) and BMI
    BH <- rnorm(N, mean = muBH, sd = sdBH)      # meters
    BMI <- rlnorm(N, meanlog = log_muBMI, sdlog = log_sdBMI)  # kg/m^2

    # Compute body weight (BW)
    BW <- BMI * BH^2  # kg

    # Age and species/type constants
    age <- 35           # years
    species <- "human"
    type <- "Caucasian"

    # Create realized parameter sets without attaching generation metadata.
    humans <- vector("list", N)
    for (i in seq_len(N)) {
        humans[[i]] <- parameters(
            species = species,
            type = type,
            sex = sex,
            age = units::set_units(age, "year", mode = "standard"),
            BW = units::set_units(BW[[i]], "kg", mode = "standard"),
            BH = units::set_units(BH[[i]], "m", mode = "standard")
        )
    }

    .new_parameter_sets(humans, check_units = FALSE)
}
