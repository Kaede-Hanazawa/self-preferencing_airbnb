product_data <- read_csv(
  here::here(
    "01_analysis", 
    "06_output_airbnb_only", 
    "RCNL", 
    # "03_random_coefficient_nested_logit", 
    "product_data_random_coefficient_nested_logit.csv"
  )
)

changed_profits <- read_csv(here::here("01_analysis", "06_output_airbnb_only", "RCNL", "changed_profit.csv"))

product_data <- product_data |> 
  cbind(changed_profits) |>
  dplyr::mutate(
    price_diff_ratio = (changed_prices - prices) / prices * 100,
    cluster = as.factor(cluster),
    date = as.Date(stringr::str_extract(market_ids, "^\\d{4}-\\d{2}-\\d{2}")),
    profits_diff_ratio = (changed_profits - profits) / profits * 100,
    is_AIRBNB = dplyr::case_when(
      merger_ids == "Airbnb" ~ "Airbnb",
      room_type == "Airbnb" ~ "Hosts",
      TRUE ~ "Hotels"
    ),
    with_SP = if_else(room_type=="Airbnb", paste0(room_type, if_else(AP, " with SP", " without SP")), "Booking.com"),
    demand = shares * potential_market_size,
    revenue = demand * prices,
    changed_shares = as.numeric(gsub("\\[|\\]", "", changed_shares)),
    changed_demand = changed_shares * potential_market_size,
    changed_revenue = changed_demand * changed_prices
  ) |> 
  group_by(market_ids) |>  # market_idsごとにグループ化
  mutate(
    Airbnb_revenue = sum(revenue[room_type == "Airbnb"], na.rm = TRUE) * 0.03,  # Airbnbのrevenueを合計
    new_Airbnb_revenue = sum(changed_revenue[room_type == "Airbnb"], na.rm = TRUE) * 0.03,  # Airbnbのchanged_revenueを合計
    Airbnb_revenue_diff_ratio = (new_Airbnb_revenue - Airbnb_revenue) / Airbnb_revenue * 100,  # Airbnbのrevenueの変化率
    n_airbnb = sum(is_AIRBNB == "Airbnb"),
    n_hosts = sum(is_AIRBNB == "Hosts"),
    n_hotels = sum(is_AIRBNB == "Hotel"),
    total_n = n(),
    ratio_airbnb = n_airbnb / total_n,
    ratio_hosts = n_hosts / total_n,
    ratio_hotels = n_hotels / total_n
  ) |> 
  ungroup()  # グループ化を解除


colnames(product_data)

########################
# marginal costs by room type
########################
product_data |> 
  ggplot2::ggplot(ggplot2::aes(x = costs)) + # linetypeを追加
  ggplot2::geom_density(alpha = 0.8) +
  ggthemes::theme_economist_white() +
  ggplot2::scale_x_continuous(limits = c(-0.5, 1.5)) +
  ggplot2::labs(
    x = "Marginal costs",
    y = "Density",
    title = NULL,
    # color = NULL,
    linetype = NULL # 凡例のタイトルを削除
  ) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 12) # 凡例テキストのサイズを小さくする
  )

ggsave(here::here("01_analysis", "00_figuretable", "21_marginal_costs.pdf"), width = 7, height = 5)

########################
# marginal costs by cluster
########################
product_data |> 
  ggplot2::ggplot(ggplot2::aes(x = costs, linetype = cluster)) + # linetypeを追加
  ggplot2::geom_density(alpha = 0.9) +
  ggthemes::theme_economist_white() +
  ggplot2::scale_x_continuous(limits = c(-0.5, 2)) +
  ggplot2::labs(
    x = "Marginal costs",
    y = "Density",
    title = NULL,
    color = "Price Cluster", # 凡例タイトル
    linetype = "Price Cluster" # 線種の凡例タイトル
  ) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 12) # 凡例テキストのサイズを小さくする
  )


ggsave(here::here("01_analysis", "00_figuretable", "22_marginal_costs_by_cluster.pdf"), width = 7, height = 5)


########################
# Price difference ratio by room type
########################
product_data |> 
  ggplot2::ggplot(ggplot2::aes(x = price_diff_ratio, fill = room_type)) +
  # ggplot2::geom_density(alpha = 0.8) +
  facet_wrap(~room_type) +
  ggplot2::geom_histogram(binwidth = 10, color = "black") +
  ggthemes::theme_economist_white() +
  # ggplot2::scale_x_continuous(limits = c(-20, 20)) +
  ggplot2::labs(
    x = "% change in price",
    y = "Count",
    title = NULL,
    fill = NULL
  ) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    legend.position = "none", # 凡例を削除
    # legend.text = ggplot2::element_text(size = 12) 
  )


ggsave(here::here("01_analysis", "00_figuretable", "23_change_in_price_by_room_type.pdf"), width = 7, height = 5)






########################
# Price difference ratio by cluster
########################

product_data |> 
  ggplot2::ggplot(ggplot2::aes(x = price_diff_ratio, fill=cluster)) +
  # ggplot2::geom_density(alpha = 0.8) +
  facet_wrap(~cluster) +
  ggplot2::geom_histogram(binwidth = 10, color = "black") +
  ggthemes::theme_economist_white() +
  # ggplot2::scale_x_continuous(limits = c(-20, 20)) +
  ggplot2::labs(
    x = "% change in price",
    y = "Count",
    title = NULL,
    fill = "Price Cluster"
  ) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    legend.position = "bottom",
    legend.text = element_text(size = 12) # 凡例テキストのサイズを小さくする
  )

ggsave(here::here("01_analysis", "00_figuretable", "24_change_in_price_by_cluster.pdf"), width = 7, height = 5)

########################
# Descriptive statistics
########################
modelsummary::datasummary(
  data = product_data,
  formula = (with_SP) * (Mean + SD + Min + Max + Median) ~ costs + price_diff_ratio + profits_diff_ratio,
  output = "latex",
  fmt = 3
)

modelsummary::datasummary(
  data = product_data,
  formula = (cluster) * (Mean + SD + Min + Max) ~ costs + price_diff_ratio,
  output = "latex"
)




########################
# Marginal costs by cluster, x-axis: date, y-axis: average_costs by cluster
########################
product_data |> 
  dplyr::group_by(date, cluster) |> 
  dplyr::summarize(average_costs = mean(costs), .groups = "drop") |> 
  ggplot2::ggplot(ggplot2::aes(x = date, y = average_costs, color = cluster, shape = cluster)) + # shapeを追加
  ggplot2::geom_point(size = 3, fill = "white", stroke = 1) + # 白抜きポイント
  ggplot2::geom_line() +
  ggthemes::theme_economist_white() +
  ggplot2::labs(
    x = "Date",
    y = "Average marginal costs",
    title = NULL,
    color = "Price Cluster",
    shape = "Price Cluster"
  ) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 12) # 凡例テキストのサイズを小さくする
  )



ggsave(here::here("01_analysis", "00_figuretable", "26_marginal_costs_over_date_by_cluster.pdf"), width = 10, height = 8)





########################
# Compute Airbnb's revenue
########################

modelsummary::datasummary(
  data = product_data,
  formula = Airbnb_revenue + new_Airbnb_revenue + Airbnb_revenue_diff_ratio~ Mean + SD + Min + Max,
  fmt = 3,
  output = "latex"
)



