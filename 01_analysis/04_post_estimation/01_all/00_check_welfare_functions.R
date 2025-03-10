# 散布図作成関数
plot_scatter_by_model <- function(df, alpha = 0.5) {
  # カスタム色と形状を指定
  colors <- c("Plain Logit" = "#93AA00", 
              "Nested Logit" = "#00B9E3", 
              "Random Coefficient Logit" = "red")
  
  shapes <- c("Plain Logit" = 21,  # 丸
              "Nested Logit" = 23,  # 菱形
              "Random Coefficient Logit" = 24)  # 三角形
  
  # 散布図を作成する列の名前
  ratio_columns <- c("% Change in Consumer Surplus", "% Change in Total Profits", "% Change in Social Welfare")
  
  # x軸に使う列の名前
  x_columns <- c("n", "airbnb_n", "booking_n")
  
  # 各x軸の列に対してプロットを作成
  for (x_col in x_columns) {
    for (column in ratio_columns) {
      p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[column]], color = model, shape = model)) +
        geom_point(size = 3, fill = "white", stroke = 1.5, alpha = alpha) +  # alphaを指定
        scale_color_manual(values = colors) +
        scale_shape_manual(values = shapes) +
        labs(
          x = x_col,  # x軸の列名を設定
          y = column  # y軸の列名を設定
        ) +
        theme_minimal() +
        theme(legend.title = element_blank(),  # 凡例のタイトルを非表示
              plot.title = element_text(hjust = 0.5))  # タイトルを中央に配置
      
      # プロットの表示
      print(p)
    }
  }
}

##########################################
# diff_prices_ratioのヒストグラムを作成する関数
##########################################
plot_diff_prices_ratio_histogram <- function(df, binwidth = 50) {
  # ヒストグラムを作成
  p <- ggplot(df, aes(x = diff_prices_ratio, fill = room_type)) +
    geom_histogram(binwidth = binwidth, color = "black", alpha = 0.8, position = "dodge") +  # binwidthを引数から指定
    scale_fill_manual(values = c("#93AA00", "#00B9E3", "red")) +  # room_typeごとに色を分ける
    labs(
      x = "Diff Prices Ratio (%)",
      y = "Count",
      # title = "Histogram of Diff Prices Ratio by Room Type"
    ) +
    theme_economist_white() +  # theme_economist_whiteを適用
    theme(
      legend.title = element_blank(), # 凡例のタイトルを非表示
      legend.position = "bottom"       # 凡例を下に表示
      )
  
  # プロットの表示
  print(p)
}


plot_histogram_with_ci <- function(data, param, f_name) {
  # ヒストグラムのプロット
  p <- data |> 
    filter(param == !!param) |> 
    ggplot(aes(x = param_value)) +
    geom_histogram(fill = "skyblue", color = "black") +
    # 95%信頼区間の計算と縦線の追加
    geom_vline(xintercept = quantile(data |> filter(param == !!param) |> pull(param_value), probs = 0.025),
               color = "blue", linetype = "dashed", size = 1) +
    geom_vline(xintercept = quantile(data |> filter(param == !!param) |> pull(param_value), probs = 0.975),
               color = "red", linetype = "dashed", size = 1) +
    labs(
      x = param,
      y = "Count"
    ) +
    theme_economist_white()
  
  print(p)
  
  # 保存例
  ggsave(
    plpt = p, 
    filename = here::here("01_analysis", "00_figuretable", f_name), 
    width = 6,
    height = 5
  )
}


##############################################
# いろんな処理を含んだ大元の関数
##############################################
make_welfare_result_df_and_plot <- function(SP = FALSE){
  # モデル名とファイルパスのリスト
  models <- c(
    "00_plain_logit",
    "01_nested_logit", "02_random_coefficient" # , 
    # "03_random_coefficient_nested_logit"
    )
  model_names <- c(
    "Plain Logit",
    "Nested Logit", "Random Coefficient Logit"#, 
    # "RCNL"
    )
  file_names <- c(
    "plain_logit", 
    "nested_logit", "random_coefficient_logit"#,
    # "random_coefficient_nested_logit"
    )
  
  # 空のリストを準備
  summary_list <- list()
  descriptive_stats_list <- list() # descriptive stats 用のリスト
  
  # ループで各モデルを処理
  for (i in seq_along(models)) {
    SP <- TRUE
    # i =2
    # ファイルパスを生成してデータ読み込み
    if(SP){
      changed_markups <- read_csv(here::here("01_analysis", "05_output_all", "AP", models[i], "changed_markups.csv"))
      changed_profits <- read_csv(here::here("01_analysis", "05_output_all", "AP", models[i], "changed_profit.csv"))
      product_data <- read_csv(here::here("01_analysis", "05_output_all", "AP", models[i], paste0("product_data_", file_names[i], ".csv")))
      params_data <- read_csv(here::here("01_analysis", "05_output_all", "AP", models[i], "params_output.csv"))
    } else {
      changed_markups <- read_csv(here::here("01_analysis", "05_output_all", models[i], "changed_markups.csv"))
      changed_profits <- read_csv(here::here("01_analysis", "05_output_all", models[i], "changed_profit.csv"))
      product_data <- read_csv(here::here("01_analysis", "05_output_all", models[i], paste0("product_data_", file_names[i], ".csv")))
    }
    
    
    # bootstrap推定値の分布をプロット
    # plot_histogram_with_ci(params_data, "prices", "14_bootstrapped_alpha.pdf")
    
 
    # df に新しいデータを格納
    df <- product_data |>
      dplyr::group_by(market_ids) |>
      dplyr::summarize(
        airbnb_count = sum(merger_ids == "Airbnb"),
        total_count = dplyr::n(),
        airbnb_ratio = airbnb_count / total_count * 100,
        mean_price_diff = mean(prices - changed_prices, na.rm = TRUE)
      )
    
    # 散布図を作成
    ggplot2::ggplot(df, ggplot2::aes(x = airbnb_ratio, y = mean_price_diff)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
      ggplot2::labs(
        #title = "Scatter Plot of Airbnb Ratio vs. Mean Price Difference",
        x = "Airbnb Ratio in S2 (%)",
        y = "Difference in Mean Price (1/10000 yen)"
      ) +
      ggthemes::theme_economist_white() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
        axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10))
      )
    
    ggsave(
      filename = here::here("01_analysis", "00_figuretable", "15_airbnb_ratio_vs_price_diff.pdf"),
      width = 6,
      height = 5
    )
    
    
    # 'date' 列と 'price_difference' 列を一度に作成し、df に格納
    df <- product_data |>
      dplyr::mutate(
        date = as.Date(stringr::str_extract(market_ids, "^\\d{4}-\\d{2}-\\d{2}")),
        price_difference = prices - changed_prices,
      ) |>
      dplyr::group_by(
        city_ids, date
      ) |>
      dplyr::summarise(
        mean_delta = mean(delta),
        mean_xi = mean(xi),
        mean_price_diff = mean(price_difference)
        )
    

    # 保存    
    
    firm_data <- product_data |>
      cbind(changed_profits) |>
      mutate(
        cs = as.numeric(gsub("\\[|\\]", "", cs)),
        changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs)),
        # hhi = as.numeric(gsub("\\[|\\]", "", hhi)),
        # changed_hhi = as.numeric(gsub("\\[|\\]", "", changed_hhi)),
        changed_shares = as.numeric(gsub("\\[|\\]", "", changed_shares)),
        diff_cs = changed_cs - cs,
        diff_profit = changed_profits - profits,
        diff_profit_ratio = (changed_profits - profits) / profits * 100,
        diff_prices = changed_prices - prices,
        diff_prices_ratio = (changed_prices - prices) / prices * 100
      ) 
    
    
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
    
    # View(df)
    
    
    # 市場ごとのhhiの変化をプロットする
    # firm_data |>
    #   ggplot2::ggplot() +
    #   ggplot2::geom_density(ggplot2::aes(x = hhi), color = "blue", linetype = "solid") +
    #   ggplot2::geom_density(ggplot2::aes(x = changed_hhi), color = "red", linetype = "dashed") +
    #   ggthemes::theme_economist_white()
    
    firm_data |> 
      dplyr::group_by(city_ids) |>
      dplyr::summarise(mean_cs = mean(cs)) |> 
      ggplot2::ggplot(ggplot2::aes(x = city_ids, y = mean_cs)) +
      ggplot2::geom_bar(stat = "identity", fill="skyblue", color="black") +
      ggthemes::theme_economist_white() +
      ggplot2::labs(
        y = "Mean Consumer Surplus",
        x = NULL
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
        axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
        axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10))
      )
    
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
    plot_diff_prices_ratio_histogram(firm_data, binwidth = 5)
    
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


#######################################
# Welfare Plot Function
#######################################
make_welfare_plot <- function(df, num_of_bins, model_name, 
                              cs_color = "#00B9E3", profits_color = "#93AA00", welfare_color = "red", 
                              alpha = 0.8, width = 8, height = 5, SP = FALSE){
  # csの変化
  p <- ggplot(df, aes(x = diff_cs_ratio)) +
    geom_histogram(bins = num_of_bins, fill = cs_color, color = "black", alpha = alpha) +
    labs(title = paste0("% Change in <span style='color:", cs_color, ";'>Consumer Surplus</span> (", model_name , ")"),
         x = "Change in Consumer Surplus (%)",
         y = "Frequency") +
    theme_economist_white() +
    theme(plot.title = element_markdown())  # 指定された色にするためにelement_markdownを使用
  print(p)
  
  if(SP){
    ggsave(here::here("01_analysis", "00_figuretable", paste0(model_name, "_cs_AP.pdf")), p, device = "pdf", width = width, height = height)
  } else {
    ggsave(here::here("01_analysis", "00_figuretable", paste0(model_name, "_cs.pdf")), p, device = "pdf", width = width, height = height)  
  }
  
  
  
  # profitsの変化
  p <- ggplot(df, aes(x = diff_total_profits_ratio)) +
    geom_histogram(bins = num_of_bins, fill = profits_color, color = "black", alpha = alpha) +
    labs(title = paste0("% Change in <span style='color:", profits_color, ";'>Total Profits</span> (", model_name , ")"),
         x = "Change in Total Profits (%)",
         y = "Frequency") +
    theme_economist_white() +
    theme(plot.title = element_markdown())  # 指定された色にするためにelement_markdownを使用
  print(p)
  
  if(SP){
    ggsave(here::here("01_analysis", "00_figuretable", paste0(model_name, "_profits_AP.pdf")), p, device = "pdf", width = width, height = height)
  } else {
    ggsave(here::here("01_analysis", "00_figuretable", paste0(model_name, "_profits.pdf")), p, device = "pdf", width = width, height = height)
  }
  
  # social welfareの変化
  p <- ggplot(df, aes(x = diff_total_profits + diff_cs_ratio)) +
    geom_histogram(bins = num_of_bins, fill = welfare_color, color = "black", alpha = alpha) +
    labs(title = paste0("% Change in <span style='color:", welfare_color, ";'>Social Welfare</span> (", model_name , ")"),
         x = "Change in Social Welfare (%)",
         y = "Frequency") +
    theme_economist_white() +
    theme(plot.title = element_markdown())  # 指定された色にするためにelement_markdownを使用
  print(p)
  
  if(SP){
    ggsave(here::here("01_analysis", "00_figuretable", paste0(model_name, "_social_welfare_AP.pdf")), p, device = "pdf", width = width, height = height)
  } else {
    ggsave(here::here("01_analysis", "00_figuretable", paste0(model_name, "_social_welfare.pdf")), p, device = "pdf", width = width, height = height)
  }
}




##############################
# Descriptive Statistics for Welfare
##############################
make_descriptive_stats <- function(df, model_name){
  # 列名の修正とmodel列の追加
  df <- df |> 
    select(
      cs, changed_cs, diff_cs_ratio,
      total_profits, changed_total_profits, diff_total_profits_ratio,
      social_welfare, changed_social_welfare, diff_social_welfare_ratio, airbnb_n, booking_n, n,
    ) |> 
    rename(
      "Consumer Surplus (Senario 1)" = cs,
      "Consumer Surplus (Senario 2)" = changed_cs,
      "% Change in Consumer Surplus" = diff_cs_ratio,
      "Total Profits (Senario 1)" = total_profits,
      "Total Profits (Senario 2)" = changed_total_profits,
      "% Change in Total Profits" = diff_total_profits_ratio,
      "Social Welfare (Senario 1)" = social_welfare,
      "Social Welfare (Senario 2)" = changed_social_welfare,
      "% Change in Social Welfare" = diff_social_welfare_ratio
    ) |> 
    mutate(model = model_name) # model_name列を追加
  
  return(df) # データフレームを返す
}


#############################################################
# Welfare Effectをプロットする関数
#############################################################
plot_welfare_changes <- function(df, output_file, 
                                 cs_color = "#00B9E3", 
                                 profits_color = "#93AA00", 
                                 welfare_color = "red",
                                 binwidth = 100, # ヒストグラムの間の間隔
                                 alpha = 0.8,
                                 dodge_width = 0.9, # 棒と棒の間隔
                                 x_break_step = 10, # x軸のメモリの間隔（10%刻み）
                                 legend_text_size = 8, # 凡例のテキストサイズ
                                 legend_key_size = 0.5, # 凡例のシンボルサイズ
                                 lower = -30, # デフォルトの下限
                                 upper = 0 # デフォルトの上限
) {
  
  # ヒストグラム用のデータフレームを作成
  df_long <- df |>
    select(`% Change in Consumer Surplus`, `% Change in Total Profits`, `% Change in Social Welfare`, model) |>
    pivot_longer(cols = c(`% Change in Consumer Surplus`, `% Change in Total Profits`, `% Change in Social Welfare`),
                 names_to = "measure",
                 values_to = "change")
  
  # カスタムラベルの設定
  label_names <- c(
    `% Change in Consumer Surplus` = "Consumer surplus",
    `% Change in Total Profits` = "Producer surplus",
    `% Change in Social Welfare` = "Social welfare"
  )
  
  # measure列を順序付き因子に変換
  df_long <- df_long |>
    mutate(measure = factor(measure, 
                            levels = c("% Change in Consumer Surplus", 
                                       "% Change in Total Profits", 
                                       "% Change in Social Welfare")))  # 順番を指定
  
  # 色を指定
  colors <- c(cs_color, profits_color, welfare_color)
  
  # プロット
  p <- ggplot(df_long, aes(x = change, fill = model)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = binwidth, 
                   position = position_dodge(preserve = "single", width = dodge_width), 
                   color = "black", alpha = alpha) +
    facet_wrap(~measure, scales = "fixed", ncol = 1, 
               labeller = as_labeller(label_names)) +  # カスタムラベルを設定
    scale_fill_manual(values = colors) +
    scale_x_continuous(breaks = seq(floor(min(df_long$change)/10) * 10, ceiling(max(df_long$change)/10) * 10, by = x_break_step),
                       limits = c(lower, upper)) +  # x軸のメモリを10%刻みに設定し、表示範囲を指定
    labs(
      x = "% change",
      y = "Density") +  # 縦軸のタイトルをDensityに変更
    theme_economist_white() + # theme_economist_whiteに変更
    theme(legend.title = element_blank(),
          legend.position = "bottom", # 凡例をプロットの下に表示
          legend.text = element_text(size = legend_text_size),  # 凡例のテキストサイズを指定
          legend.key.size = unit(legend_key_size, "cm"),  # 凡例のシンボルサイズを指定
          strip.text = element_text(size = 12, face = "bold", color = "black"),  # ファセットラベルを太字で黒に設定
          axis.title.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 10)),  # x軸ラベルを太字で黒に設定し、もう少し下に配置
          axis.title.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 10)),  # y軸ラベルをメモリから離す
          plot.title = element_text(hjust = 0.5, size = 16))
  
  # プロットの表示
  print(p)
  
  # PDFに保存
  output_path <- here::here("01_analysis", "00_figuretable", output_file)
  ggsave(output_path, p, width = 7, height = 7.5)
}




#######################################
# 引数で指定された列のヒストグラムをプロットする関数
#######################################
hist_plot <- function(df, column_name, model_name, binwidth = 5, fill_color = NULL, 
                      width = 7, height = 7.5, x_lab, x_axis_limits = NULL) {
  
  # モデル名に応じて色を設定
  fill_color <- switch(model_name,
                       "Plain Logit" = "#00B9E3",
                       "Nested Logit" = "#93AA00",
                       "Random Coefficient Logit" = "red",
                       fill_color) # デフォルトは引数で指定されたfill_color
  
  # プロット
  p <- ggplot(df, aes_string(x = column_name)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = binwidth, 
                   color = "black", fill = fill_color, alpha = 0.8) +
    labs(
      title = model_name,
      x = x_lab,
      y = "Density"
    ) +
    theme_economist_white() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom", # 凡例をプロットの下に表示
      legend.text = element_text(size = 8),  # 凡例のテキストサイズを指定
      legend.key.size = unit(0.5, "cm"),  # 凡例のシンボルサイズを指定
      strip.text = element_text(size = 12, face = "bold", color = "black"),  # ファセットラベルを太字で黒に設定
      axis.title.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 10)),  # x軸ラベルを太字で黒に設定し、もう少し下に配置
      axis.title.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 10)),  # y軸ラベルをメモリから離す
      plot.title = element_text(hjust = 0.5, size = 16)  # タイトルの中央揃えとサイズ指定
    )
  
  # x軸の表示範囲を指定（NULLでない場合にのみ適用）
  if (!is.null(x_axis_limits)) {
    p <- p + scale_x_continuous(limits = x_axis_limits)
  }
  
  # プロットの表示
  print(p)
  
  # PDFに保存
  output_path <- here::here("01_analysis", "00_figuretable", paste0("06_", model_name, "_", column_name, "_mc.pdf"))
  ggsave(output_path, p, width = width, height = height)
  
  return(p)
}









plot_city_cs <- function(df, highlight_city = "shinjuku") {
  
  # 各 city_ids のデータにフラグを追加
  df <- df %>%
    mutate(
      highlight = if_else(city_ids == highlight_city, TRUE, FALSE)
    )
  
  # プロット
  p <- ggplot(df, aes(x = date)) +
    # 他の city_ids の点と線を灰色で表示（形状はデフォルトのまま）
    geom_point(data = df %>% filter(!highlight), aes(y = cs), color = "grey70", size = 3) +
    geom_line(data = df %>% filter(!highlight), aes(y = cs), color = "grey70", linetype = "solid") +
    geom_point(data = df %>% filter(!highlight), aes(y = changed_cs), color = "grey70", size = 3) +
    geom_line(data = df %>% filter(!highlight), aes(y = changed_cs), color = "grey70", linetype = "solid") +
    
    # 強調した city_ids の点と線をカラーで表示、白で塗りつぶし
    geom_point(data = df %>% filter(highlight), aes(y = cs, shape = "cs", color = "cs"), fill = "white", size = 3) +
    geom_line(data = df %>% filter(highlight), aes(y = cs, color = "cs"), linetype = "solid") +
    geom_point(data = df %>% filter(highlight), aes(y = changed_cs, shape = "changed_cs", color = "changed_cs"), fill = "white", size = 3) +
    geom_line(data = df %>% filter(highlight), aes(y = changed_cs, color = "changed_cs"), linetype = "solid") +
    
    # カスタムの色と形（22: 菱形、24: 上向き三角形）
    scale_shape_manual(values = c("cs" = 22, "changed_cs" = 24)) +
    scale_color_manual(values = c("cs" = "blue", "changed_cs" = "red")) +
    
    labs(
      title = paste("Consumer Surplus over Time for", highlight_city),
      x = "Date",
      y = "Consumer Surplus",
      color = "Legend",
      shape = "Legend"
    ) +
    theme_economist_white() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 16)
    )
  
  # プロットの表示
  print(p)
}

#######################################
# city_idsごとに、指定された列の平均値をプロットする関数
#######################################
plot_city_avg <- function(df, x_col, column_name, model_name, width = 7, height = 7.5) {
  
  # ヒストグラムの色をモデル名に応じて設定
  fill_color <- if (model_name == "Plain Logit") {
    "#00B9E3"
  } else if (model_name == "Nested Logit") {
    "#93AA00"
  } else if (model_name == "Random Coefficient Logit") {
    "red"
  } else {
    "gray"  # デフォルトの色
  }
  
  # x_colごとの平均値を計算
  avg_data <- df %>%
    group_by(.data[[x_col]]) %>%
    summarize(mean_value = mean(.data[[column_name]], na.rm = TRUE))
  
  # プロット
  p <- ggplot(avg_data, aes_string(x = x_col, y = "mean_value")) +
    geom_bar(stat = "identity", fill = fill_color, color = "black", alpha = 0.8) +
    labs(
      title = paste(model_name, "- Average", column_name, "by", x_col),
      x = NULL,  # x軸ラベルを非表示
      y = paste("Average", column_name)
    ) +
    theme_economist_white() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      strip.text = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_blank(), # x軸タイトルを非表示
      axis.title.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 16)
    )
  
  # x_col が "city_ids" の場合、x軸のラベルを90度回転
  if (x_col == "city_ids") {
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  # プロットの表示
  print(p)
  
  # PDFに保存
  output_path <- here::here("01_analysis", "00_figuretable", paste0("07_", model_name, "_", column_name, "_", x_col, "_avg.pdf"))
  ggsave(output_path, p, width = width, height = height)
}
