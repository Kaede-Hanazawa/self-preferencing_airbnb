# モデル名とファイルパスのリスト
models <- c("00_plain_logit", "01_nested_logit", "02_random_coefficient", "03_random_coefficient_nested_logit")
model_names <- c("Plain Logit", "Nested Logit", "Random Coefficient Logit", "RCNL")
file_names <- c("plain_logit", "nested_logit", "random_coefficient_logit", "random_coefficient_nested_logit")

# 空のリストを準備
summary_list <- list()
descriptive_stats_list <- list() # descriptive stats 用のリスト


changed_markups <- read_csv(here::here("01_analysis", "05_output_all", "AP", "03_random_coefficient_nested_logit",  "changed_markups.csv"))
changed_profits <- read_csv(here::here("01_analysis", "05_output_all", "AP", "03_random_coefficient_nested_logit", "changed_profit.csv"))
product_data <- read_csv(here::here("01_analysis", "05_output_all", "AP", "03_random_coefficient_nested_logit", paste0("product_data_", "random_coefficient_nested_logit", ".csv")))
# params_data <- read_csv(here::here("01_analysis", "05_output_all", "AP", "03_random_coefficient_nested_logit", "params_output.csv"))


# 保存    

firm_data <- product_data |>
  cbind(changed_profits) |>
  mutate(
    cs = as.numeric(gsub("\\[|\\]", "", cs)),
    changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs)),
    hhi = as.numeric(gsub("\\[|\\]", "", hhi)),
    changed_hhi = as.numeric(gsub("\\[|\\]", "", changed_hhi)),
    changed_shares = as.numeric(gsub("\\[|\\]", "", changed_shares)),
    diff_cs = changed_cs - cs,
    diff_profit = changed_profits - profits,
    diff_profit_ratio = (changed_profits - profits) / profits * 100,
    diff_prices = changed_prices - prices,
    diff_prices_ratio = (changed_prices - prices) / prices * 100
  ) 


firm_data |> 
  ggplot(aes(x = diff_cs)) +
  geom_histogram() +
  xlim(-5000, 5000) 


df <- firm_data |> 
  mutate(
    share_2 = shares^2,
    changed_shares_2 = changed_shares^2
  ) |> 
  group_by(market_ids, firm_ids) |>
  summarise(
    HHI_f = sum(share_2) * 10000
  ) |> 
  mutate(HHI = sum(HHI_f)) |> 
  distinct(market_ids, .keep_all = TRUE)


  
  ################################
  # 限界費用のプロット
  ################################
  p <- hist_plot(firm_data, 
                 column_name = "costs",
                 model_name = model_names[i], 
                 binwidth = 0.1,
                 width = 7,
                 height = 6,
                 x_lab = "Marginal Cost",
                 x_axis_limits = c(-4, 1))
  
  mc_plot_list[[cnt]] <<- p
  cnt <- cnt + 1
  
  ################################
  # 市場ごとの社会厚生の変化率をプロット
  ################################
  
  # 価格の変化率をプロットする
  plot_diff_prices_ratio_histogram(firm_data, binwidth = 500)
  
  # データを処理
  product_data <- product_data |>
    cbind(changed_profits) |>
    mutate(
      cs = as.numeric(gsub("\\[|\\]", "", cs)),
      changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs)),
      diff_cs = changed_cs - cs,
      diff_profit = changed_profits - profits,
      diff_profit_ratio = (changed_profits - profits) / profits * 100,
      diff_prices = changed_prices - prices,
      diff_prices_ratio = (changed_prices - prices) / prices * 100,
      date = as.Date(sub(":.*", "", market_ids)) # date列をここで作成
    ) |>
    group_by(market_ids) |>
    summarise(
      city_ids = city_ids,
      date = date,
      day_of_week = day_of_week,
      airbnb_n = sum(room_type == "Airbnb"),
      booking_n = sum(room_type == "Booking.com"),
      n = n(),
      cs = sum(cs),
      changed_cs = sum(changed_cs),
      diff_cs = sum(diff_cs),
      total_profits = sum(profits),
      changed_total_profits = sum(changed_profits),
      diff_total_profits = sum(diff_profit),
      social_welfare = cs + total_profits,
      changed_social_welfare = changed_cs + changed_total_profits,
      diff_social_welfare = diff_cs + diff_total_profits,
    ) |>
    ungroup() |>
    tibble() |> 
    mutate(
      diff_cs_ratio = diff_cs / cs * 100,
      diff_total_profits_ratio = diff_total_profits / total_profits * 100,
      diff_social_welfare_ratio = diff_social_welfare / social_welfare * 100
    ) |> 
    distinct(market_ids, .keep_all = TRUE)
  
  # Welfare plot by City
  plot_city_avg(
    product_data, column_name = "cs",
    x_col = "city_ids",
    model_name = model_names[i]
  )
  
  plot_city_avg(
    product_data, column_name = "cs",
    x_col = "day_of_week",
    model_name = model_names[i]
  )
  # 
  # plot_city_avg(
  #   product_data, 
  #   x_column = "city_ids", 
  #   column1 = "cs", 
  #   column2 = "changed_cs", 
  #   model_name = model_names[i])
  
  
  # Welfare plot
  make_welfare_plot(
    df = product_data,
    alpha = 0.8,
    num_of_bins = 40,
    model_name = model_names[i],
    SP = SP
  )
  
  descriptive_stats_list[[i]] <- make_descriptive_stats(product_data, model_names[i]) # 結果をリストに保存
  
  # 平均値を計算
  mean_cs <- mean(product_data$cs)
  mean_changed_cs <- mean(product_data$changed_cs)
  mean_diff_cs <- mean(product_data$diff_cs)
  mean_diff_cs_ratio <- mean(product_data$diff_cs_ratio)
  
  mean_total_profits <- mean(product_data$total_profits)
  mean_total_profits_changed <- mean(product_data$changed_total_profits)
  mean_diff_total_profits <- mean(product_data$diff_total_profits)
  mean_diff_total_profits_ratio <- mean(product_data$diff_total_profits_ratio)
  
  mean_social_welfare <- mean_cs + mean_total_profits
  mean_changed_social_welfare <- mean_changed_cs + mean_total_profits_changed
  mean_diff_social_welfare <- mean_diff_cs + mean_diff_total_profits
  mean_diff_social_welfare_ratio <- mean_diff_cs_ratio + mean_diff_total_profits_ratio
  
  # tibbleにまとめる
  summary_tibble <- tibble(
    colname = c("mean_cs", "mean_changed_cs", "mean_diff_cs", "mean_diff_cs_ratio",
                "mean_total_profits", "mean_total_profits_changed", "mean_diff_total_profits", 
                "mean_diff_total_profits_ratio", "mean_social_welfare", "mean_changed_social_welfare", 
                "mean_diff_social_welfare", "mean_diff_social_welfare_ratio"),
    value = c(mean_cs, mean_changed_cs, mean_diff_cs, mean_diff_cs_ratio, 
              mean_total_profits, mean_total_profits_changed, mean_diff_total_profits, 
              mean_diff_total_profits_ratio, mean_social_welfare, mean_changed_social_welfare, 
              mean_diff_social_welfare, mean_diff_social_welfare_ratio),
    model = model_names[i]
  )
  
  summary_tibble <- summary_tibble |>
    mutate(
      value = format(round(value, 3), nsmall = 3)
    )
  
  # リストに保存
  summary_list[[i]] <- summary_tibble
}

# すべてのモデルのデータを1つのデータフレームにまとめる
final_summary <- bind_rows(summary_list)

# descriptive stats のデータフレームを1つにまとめる
final_descriptive_stats <- bind_rows(descriptive_stats_list)

return(list(final_summary, final_descriptive_stats))
}
