# Code to build predefined physiologies (executed at package build time)
build_physiologies <- function() {

    meta_df <- read.csv2("data-raw/phys_metadata.csv")
    rat_df <- read.csv2("data-raw/phys_rat_param.csv")

    # ---- Build rat physiologies ----

    # Step 0: reshape rat meta data to long format (TODO: remove but document?)
    rat_meta <- subset(meta_df, Species == "rat") # |>
        # tidyr::pivot_longer(cols = !ID, names_to = "Parameter", values_to = "Value") |>
        # cbind(Unit = NA, Reference = NA, Assumption = NA)

    # Step 1: Expand "rat" rows to all individual IDs
    rat_long <- rat_df |>
        dplyr::mutate(
            ID = ifelse(ID == "rat", list(rat_meta$ID), ID)
        ) |>
        tidyr::unnest(ID)

    # Step 2: split scalar and per-tissue parameters
    rat_scalar_long <- rat_long |>
        dplyr::filter(is.na(Tissue) | Tissue == "") |>
        dplyr::select(-Tissue)

    rat_tissue_long <- rat_long |>
        dplyr::filter(!is.na(Tissue) | Tissue != "")

    # Step 3: Pivot to wide format for calculations
    rat_tissue_wide <- rat_tissue_long |>
        tidyr::pivot_wider(
            id_cols = c("ID", "Tissue"),
            names_from = "Parameter",
            values_from = "Value"
        )

    # Step 4: Compute derived parameters (to be completed)
    rat_tissue_wide <- rat_tissue_wide |>
        dplyr::mutate(
            fintVtot = fintOWtot,
            fvasVtot = fvasOWtot,
            fcelVtot = 1 - fintVtot - fvasVtot,
            fcelVtis = fcelVtot / (fcelVtot+fintVtot),
            fintVtis = fintVtot / (fcelVtot+fintVtot)
        )

    # Step 5: Pivot back to long format
    rat_tissue_long <- rat_tissue_wide |>
        tidyr::pivot_longer(
            cols = !c("ID", "Tissue"),  # all calculated parameters
            names_to = "Parameter",
            values_to = "Value"
        )

    # Step 6: Merge scalar and tissue data frames
    rat_long <- dplyr::bind_rows(
            rat_scalar_long,
            rat_tissue_long
        ) |>
        dplyr::rename(
            parameter = Parameter,
            context = Tissue,
            value = Value,
            unit = Unit,
            reference = Reference,
            assumption = Assumption
        ) |>
        dplyr::mutate(
            type = ifelse(is.na(context), "scalar", "tissue")
        )

    # Step 7: Create Physiology objects
        rat_long_split <- rat_long |> dplyr::select(-ID) |> split(rat_long$ID)
        rat_meta_split <- rat_meta |> dplyr::select(-ID) |> split(rat_meta$ID)
        rat_list <- Map(
            function(param, meta) list(param = param, meta = meta),
            rat_long_split,
            rat_meta_split
        )


}
