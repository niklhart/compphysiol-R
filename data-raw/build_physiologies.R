# Code to build predefined physiologies (executed at package build time)
build_physiologies <- function() {

    meta_df <- read.csv2("data-raw/phys_metadata.csv")
    rat_df <- read.csv2("data-raw/phys_rat_param.csv")

    # ---- Build rat physiologies ----

    # Step 1: get rat metadata
    rat_meta <- subset(meta_df, Species == "rat") 

    # Step 2: Expand "rat" rows to all individual IDs
    rat_long <- rat_df |>
        dplyr::mutate(
            ID = ifelse(ID == "rat", list(rat_meta$ID), ID)
        ) |>
        tidyr::unnest(ID)

    # Step 3: split scalar and per-tissue parameters
    rat_scalar_wide <- rat_long |>
        dplyr::filter(is.na(Tissue) | Tissue == "") |>
        dplyr::select(-Tissue) |>
        tidyr::pivot_wider(
            id_cols = "ID",
            names_from = "Parameter",
            values_from = "Value"
        )

    rat_tissue_wide <- rat_long |>
        dplyr::filter(!is.na(Tissue) & Tissue != "") |>
        tidyr::pivot_wider(
            id_cols = c("ID", "Tissue"),
            names_from = "Parameter",
            values_from = "Value"
        )
    
    # Step 4: Compute derived parameters
    rat_OWadi_df <- rat_tissue_wide |>
        dplyr::filter(Tissue == "adi") |>
        dplyr::select(ID, fowtisBW) |>
        dplyr::rename(fowadiBW = fowtisBW)

    rat_scalar_wide <- rat_scalar_wide |>
        dplyr::full_join(rat_OWadi_df, by = "ID") |>
        dplyr::mutate(
            OWtbl = ftblBW * BW,
            Vtbl = OWtbl / denstbl,
            LBW = BW * (1 - fowadiBW)
        ) |>
        dplyr::select(-dplyr::any_of(setdiff(names(rat_OWadi_df),"ID")))
    
    rat_tissue_wide <- rat_tissue_wide |>
        dplyr::full_join(rat_scalar_wide, by = "ID") |>
        dplyr::mutate(
            OWtis = fowtisBW * BW,
            OWtot = OWtit / (fintOWtot + fcelOWtot),
            fintVtot = fintOWtot,
            fvasVtot = fvasOWtot,
            fcelVtot = 1 - fintVtot - fvasVtot,
            fcelVtis = fcelVtot / (fcelVtot+fintVtot),
            fintVtis = fintVtot / (fcelVtot+fintVtot),
            Vvas = fvasOWtot * OWtot / denstbl,
            Vtis = OWtis / dens,
            Vtot = Vvas + Vtis,
            OWrbt  = OWtis / (1 - fresOWrbt),
            Vres  = fresOWrbt * OWrbt / denstbl,
            fregVtbl = Vvas / Vtbl,
            Qblo = fqbloCO * co,
            fwecVtis = fwecVrbt,
            fwicVtis = fwtotVtis - fwecVtis
        ) |>
        dplyr::select(-dplyr::any_of(setdiff(names(rat_scalar_wide),"ID")))
    
    # Step 5: Pivot all calculated parameters back to long format
    rat_scalar_long <- rat_scalar_wide |>
        tidyr::pivot_longer(
            cols = !"ID",            
            names_to = "Parameter",
            values_to = "Value"
        )
    
    rat_tissue_long <- rat_tissue_wide |>
        tidyr::pivot_longer(
            cols = !c("ID", "Tissue"),
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

    # Step 7: Create input to Physiology constructors
        rat_long_split <- rat_long |> dplyr::select(-ID) |> split(rat_long$ID)
        rat_meta_split <- rat_meta |> dplyr::select(-ID) |> split(rat_meta$ID)
        rat_list <- Map(
            function(param, meta) list(param = param, meta = meta),
            rat_long_split,
            rat_meta_split
        )


}
