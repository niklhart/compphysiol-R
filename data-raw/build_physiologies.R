# Code to build predefined physiologies (executed at package build time)
build_physiologies <- function() {

    meta_df <- read.csv2("data-raw/phys_metadata.csv")
    rat_df <- read.csv2("data-raw/phys_rat_param.csv")

    # ---- Build rat physiologies ----

    # Step 0: reshape rat meta data to long format (TODO: remove but document?)
    rat_meta <- subset(meta_df, Species == "rat") |>
        tidyr::pivot_longer(cols = !ID, names_to = "Parameter", values_to = "Value") |>
        cbind(Unit = NA, Reference = NA, Assumption = NA)

    # Step 1: Expand "rat" rows to all individual IDs
    rat_long <- rat_df |>
        dplyr::mutate(
            ID = ifelse(ID == "rat", list(unique(rat_meta$ID)), ID)
        ) |>
        tidyr::unnest(ID)

    # Step 2: split scalar and per-tissue parameters
    rat_scalar_long <- rat_long |>
        dplyr::filter(is.na(Tissue) | Tissue == "") |>
        dplyr::select(-Tissue) #|>
    #    rbind(rat_meta)

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
    )

    # Step 7: Create Physiology objects (STILL NOT WORKING. ALSO UPDATE PHYSIOLOGY CLASS DEF)
    rat_list <- base::split(rat_long,rat_long$ID) |>
        lapply(function(df) {
            phys <- Physiology$new()

            # Add metadata scalars
            phys$add_scalar("species", unique(df$species))
            phys$add_scalar("strain", unique(df$strain))
            phys$add_scalar("sex", unique(df$sex))

            # Add all other parameters
            for (i in seq_len(nrow(df))) {
                param <- df$Parameter[i]
                tissue <- df$Tissue[i]
                val <- df$Value[i]
                unit <- ifelse("Unit" %in% names(df), df$Unit[i], "")

                if (is.na(tissue) || tissue == "") {
                    phys$add_scalar(param, val, unit)
                } else {
                    phys$add_tissue_param(tissue, param, val, unit)
                }
            }
            phys
        })


}
