# Functions relating to ionization

#' Ionized fractions for a small molecule drug
#'
#' @param pH Numeric vector of (environmental) pH value(s)
#' @param pKa_ani Anionic pKa value(s), i.e. leading to a negative charge
#' @param pKa_cat Cationic pKa value(s), i.e. leading to a positive charge
#' @param Kz Tautomeric constant for ampholytes (substances with non-empty
#'    `pKa_ani` and `pKa_cat`), which defines the ratio of zwitterionic to
#'    neutral fractions, i.e. `Kz = fz/fn` (the value `Kz = +Inf` is allowed
#'    and means that the shift from anionic to cationic species occurs
#'    exclusively via the zwitter ion). For ampholytes, it is required
#'    that `pKa_cat <= pKa_ani`, which can be derived from theory based on
#'    electrostatic interactions.
#' @param respectThermodynamics A boolean that allows to
#'    specify whether pKa values of ampholytes must be ordered in accordance
#'    with thermodynamics (every anionic pKa larger than any cationic pKa)
#'    (default: `TRUE`). This option is mainly provided to be able to reproduce
#'    Rodgers et al.'s results, it is not recommended to change the default.
#' @returns A list with fields `n`,`ani`,`cat`,`z` for neutral, anionic, cationic,
#'     and zwitterionic fractions.
#' @examples
#' # neutral drug
#' ionized_fractions(pH = 7.4)
#' # monoprotic acid
#' ionized_fractions(pH = 7.4, pKa_ani = 5)
#' # monoprotic base
#' ionized_fractions(pH = 7.4, pKa_cat = 10)
#' # diprotic acid
#' ionized_fractions(pH = 7.4, pKa_ani = c(5,7))
#' # diprotic base
#' ionized_fractions(pH = 7.4, pKa_cat = c(8,10))
#' # ordinary ampholyte
#' ionized_fractions(pH = 7.4, pKa_ani = 10, pKa_cat = 5)
#' # purely zwitterionic ampholyte
#' ionized_fractions(pH = 7.4, pKa_ani = 10, pKa_cat = 5, Kz = Inf)
#'
#' @export
ionized_fractions <- function(pH, pKa_ani = NULL, pKa_cat = NULL, Kz = 0,
                              respectThermodynamics = TRUE) {
    if (respectThermodynamics && !is.null(pKa_cat) && !is.null(pKa_ani)) {
        if (max(pKa_cat) > min(pKa_ani)) {
            stop('cationic pKa value(s) must be smaller than anionic pKa value(s).')
        }
    }

    if ((is.null(pKa_ani) || is.null(pKa_cat)) && Kz != 0) {
        stop('Kz must be zero except for ampholytes.')
    }
    if (length(pH)>1) {
        vapply(X = pH,
               FUN = function(x) ionized_fractions(x,
                                                   pKa_ani = pKa_ani,
                                                   pKa_cat = pKa_cat,
                                                   Kz = Kz,
                                                   respectThermodynamics = respectThermodynamics),
               FUN.VALUE = list(n=NULL,ani=NULL,cat=NULL,z=NULL))
    }

    pKa_ani = sort(pKa_ani, decreasing = FALSE)
    pKa_cat = sort(pKa_cat, decreasing = TRUE)

    # unnormalized abundances of net neutral, anionic and cationic species
    snz <- 1
    sani <- sum(10 ^ cumsum(pH - pKa_ani))
    scat <- sum(10 ^ cumsum(pKa_cat - pH))

    stot <- snz + sani + scat

    # allocate fractions net neutral, anionic and cationic
    fnz <- snz / stot
    fani <- sani / stot
    fcat <- scat / stot

    # allocate fractions neutral/zwitterionic by tautomeric constant Kz
    fn <- fnz/(1+Kz)
    fz <- fnz-fn

    # return
    list(n = fn, ani = fani, cat = fcat, z = fz)
}
