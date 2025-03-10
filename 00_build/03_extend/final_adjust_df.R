df <- df_output |>
    filter(cluster >= min_cluster, shares >= 0.003) |>  
    mutate(
        is_airbnb = ifelse(room_type == "Airbnb", 1, 0),
        clustering_ids = paste0(room_type, ":", cluster),
    )

if(use_nested_logit){
    df <- df |>
        mutate(
            nesting_ids = paste0(room_type, ":", cluster) # reserved name for PyBLP
            # nesting_ids = cluster # reserved name for PyBLP
            # nesting_ids = is_airbnb,
        )
}

if (only_Airbnb) {
    f_name <- "df_only_airbnb.csv"
} else {
    f_name <- "df_all.csv"
}



write_csv(df, here::here("00_build", "output", f_name))


AP_n <- df |>
    filter(merger_ids == "Airbnb") |>
    nrow()

Airbnb_n <- df |>
    filter(room_type == "Airbnb") |>
    nrow()
AP_ratio <- round(AP_n / Airbnb_n * 100, 3)





