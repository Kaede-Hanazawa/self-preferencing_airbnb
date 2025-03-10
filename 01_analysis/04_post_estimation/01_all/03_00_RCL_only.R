changed_markups <- read_csv(here::here("01_analysis", "05_output_all", "AP", "02_random_coefficient", "changed_markups.csv"))
changed_profits <- read_csv(here::here("01_analysis", "05_output_all", "AP", "02_random_coefficient", "changed_profit.csv"))
product_data <- read_csv(
  here::here("01_analysis", "05_output_all", "AP", "02_random_coefficient", paste0("product_data_random_coefficient_logit.csv"))
  )
params_data <- read_csv(here::here("01_analysis", "05_output_all", "AP", "02_random_coefficient", "params_output.csv"))

firm_data <- product_data |>
  cbind(changed_profits) |>
  mutate(
    cs = as.numeric(gsub("\\[|\\]", "", cs)),
    changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs)),
    hhi = as.numeric(gsub("\\[|\\]", "", hhi)),
    changed_hhi = as.numeric(gsub("\\[|\\]", "", changed_hhi)),
    changed_shares = as.numeric(gsub("\\[|\\]", "", changed_shares)),
    diff_cs = changed_cs - cs,
    diff_cs_ratio = (changed_cs - cs) / cs * 100,
    diff_profit = changed_profits - profits,
    diff_profit_ratio = (changed_profits - profits) / profits * 100,
    diff_prices = changed_prices - prices,
    diff_prices_ratio = (changed_prices - prices) / prices * 100
  ) 

df <- firm_data |>
  group_by(market_ids) |>
  summarise(
    airbnb_firm_count = n_distinct(firm_ids[room_type == "Airbnb"]),
    booking_firm_count = n_distinct(firm_ids[room_type == "Booking.com"]),
    # airbnb_merger_count = n_distinct(merger_ids[room_type == "Airbnb"]),
    # booking_merger_count = n_distinct(merger_ids[room_type == "Booking.com"])
  )

colnames(df)



# 12_Firm_N_by_room_type_hist.pdf を作成
make_firm_N_plot <- function(){
  
  # データの整形
  df_long <- df |>
    mutate(total_count = airbnb_firm_count + booking_firm_count) |>
    select(market_ids, airbnb_firm_count, booking_firm_count, total_count) |>
    tidyr::pivot_longer(
      cols = c(airbnb_firm_count, booking_firm_count),
      names_to = "type",
      values_to = "count"
    )
  
  # ヒストグラムの作成
  ggplot(df_long, aes(x = total_count, y = count, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      values = c("airbnb_firm_count" = "#fc2fbf", "booking_firm_count" = "#00B9E3"),
      labels = c("Airbnb", "Booking.com")
    ) +
    labs(
      x = "Firm N",
      y = "Count",
      fill = "Firm Type"
    ) +
    theme_economist_white() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 14),
      # axis.text.x = element_text(angle = 90, hjust = 1)  # x軸のラベルを90度回転
    )
  
  ggsave(here::here("01_analysis", "00_figuretable", "12_Firm_N_by_room_type_hist.pdf"), width = 7, height = 5)
  
}



# market_ids ごとにグループ化し、firm_ids と merger_ids のユニークな数をカウント
summary_df <- firm_data |>
  group_by(market_ids) |>
  summarise(
    unique_firm_count = n_distinct(firm_ids),
    unique_merger_count = n_distinct(merger_ids),
    airbnb_N = n_distinct(firm_ids[room_type == "Airbnb"]),
    airbnb_merger_count = n_distinct(firm_ids[merger_ids == "Airbnb"]), # 追加部分
    booking_N = n_distinct(firm_ids[room_type == "Booking.com"]),
    total = n(),
    airbnb_N_ratio = airbnb_N / total * 100,
    SP_N_ratio = airbnb_merger_count / total * 100
  ) |> 
  mutate(n_diff = unique_merger_count - unique_firm_count) 

df <- firm_data |> 
  left_join(summary_df, by = "market_ids") |>
  select(
    market_ids, firm_ids, merger_ids, n_diff, diff_cs_ratio, airbnb_N_ratio, SP_N_ratio
  ) |> 
  mutate(city_ids = str_extract(market_ids, "(?<=:).*")) |> 
  group_by(city_ids) |> 
  summarise(
    mean_airbnb_N_ratio = paste0(as.character(round(mean(airbnb_N_ratio, na.rm = TRUE), 2)), "%"),
    mean_SP_N_ratio = paste0(as.character(round(mean(SP_N_ratio, na.rm = TRUE), 2)), "%"),
    mean_diff_cs_ratio = mean(diff_cs_ratio, na.rm = TRUE)
  )

View(df)



# diff_cs_ratioとn_diffの散布図を作成
# ggplot(df, aes(x = n_diff, y = diff_cs_ratio)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(
#     x = "Merger N - Firm N",
#     y = "Change in CS (%)"
#   ) +
#   theme_economist_white() +
#   theme(
#     axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
#     axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
#     plot.title = element_text(hjust = 0.5, size = 14)
#   )
# 












