# This file calculates how the number of firms changes between scenarios.
# data variable: df
df_summary <- df |>
  dplyr::group_by(market_ids) |>
  dplyr::summarize(
    unique_firm_count = dplyr::n_distinct(firm_ids),
    # room_type == "Airbnb"の行のn_distinct(firm_ids)
    unique_hosts_count = dplyr::n_distinct(firm_ids[room_type == "Airbnb"]),
    unique_hotels_count = dplyr::n_distinct(firm_ids[room_type == "Booking.com"]),
    unique_merger_count = dplyr::n_distinct(merger_ids),
    unique_total_count = dplyr::n_distinct(firm_ids),
    unique_Airbnb_count = dplyr::n_distinct(firm_ids[merger_ids == "Airbnb"]),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    firm_N_diff = unique_merger_count - unique_firm_count
  )

df_summary |>
  ggplot2::ggplot(ggplot2::aes(x = firm_N_diff)) +
  ggplot2::geom_bar() +
  ggplot2::labs(
    x = "Firm Count Difference between Scenarios",
    y = "Count"
  ) +
  ggthemes::theme_economist_white() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10))
  )

# ggsave(here::here("01_analysis", "00_figuretable", "28_Firm_N_Diff_Between_Scenarios.pdf"), width = 7, height = 5)

df_for_datasummary <- df_summary |> 
  select(
    unique_hosts_count,
    unique_hotels_count,
    unique_Airbnb_count,
    unique_total_count
  ) |> 
  rename(
    "Airbnb Hosts" = unique_hosts_count,
    "Hotels" = unique_hotels_count,
    "Airbnb" = unique_Airbnb_count,
    "Total" = unique_total_count
  )
modelsummary::datasummary(
  data = df_for_datasummary,
  formula = `Airbnb Hosts` + `Hotels` + `Airbnb` + Total ~ Mean + SD + Min + Max + Median + N,
  fmt = 3,
  output = "latex"
)
