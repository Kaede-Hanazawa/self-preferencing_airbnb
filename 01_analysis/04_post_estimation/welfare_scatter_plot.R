# product_dataのsocial_welfare_diff_ratio、何らかの指標の変数との散布図で相関を見る
colnames(product_data)

pr_df <- product_data |> 
  dplyr::group_by(market_ids) |>
  dplyr::summarize(
    unique_firm_count = dplyr::n_distinct(firm_ids),
    # room_type == "Airbnb"の行のn_distinct(firm_ids)
    unique_hosts_count = dplyr::n_distinct(firm_ids[room_type == "Airbnb"]),
    unique_hotels_count = dplyr::n_distinct(firm_ids[room_type == "Booking.com"]),
    unique_merger_count = dplyr::n_distinct(merger_ids),
    unique_total_count = dplyr::n_distinct(firm_ids),
    unique_Airbnb_count = dplyr::n_distinct(firm_ids[merger_ids == "Airbnb"]),
    social_welfare_diff_ratio = dplyr::first(social_welfare_diff_ratio),
    mean_price_diff_ratio = mean(price_diff_ratio),
    .groups = "drop"
  ) |> 
  dplyr::mutate(
    AP_ratio = unique_Airbnb_count / unique_total_count,
    hosts_ratio = unique_hosts_count / unique_total_count,
  )

# social_welfare_diff_ratioとAP_ratioの散布図
pr_df |>
  ggplot2::ggplot(ggplot2::aes(x = unique_hotels_count, y = social_welfare_diff_ratio)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = FALSE) +
  ggplot2::labs(
    # x = "Social Welfare Difference Ratio",
    #y = "Airbnb Ratio"
  ) +
  ggthemes::theme_economist_white() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10))
  )
