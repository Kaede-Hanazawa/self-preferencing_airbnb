##########################
# RCNL
##########################
product_data <- readr::read_csv(
  here::here(
    "01_analysis", 
    "05_output_all", 
    "AP", 
    "03_random_coefficient_nested_logit", 
    "product_data_random_coefficient_nested_logit.csv"
  )
)

product_data <- product_data |> 
  dplyr::mutate(
    price_diff_ratio = (changed_prices - prices) / prices * 100,
    cluster = as.factor(cluster),
    with_AP = dplyr::if_else(room_type=="Airbnb", paste0(room_type, dplyr::if_else(AP, " with AP", " without AP")), "Booking.com"),
    cs = as.numeric(gsub("\\[|\\]", "", cs))*100,
    # cs = as.numeric(gsub("\\[|\\]", "", cs)),
    changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs))*100,
    # changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs)),
    date = as.Date(stringr::str_extract(market_ids, "^\\d{4}-\\d{2}-\\d{2}")),
    cs_diff_ratio = (changed_cs - cs)/ cs * 100,
    profits = profits*100,
    changed_profits = changed_profits*100,
    # 正しいコード
    profits_diff_ratio = (changed_profits - profits) / profits * 100,
    # 間違っていたコード
    # profits_diff_ratio = (profits - changed_profits) / changed_profits * 100,
    social_welfare = cs + profits,
    changed_social_welfare = changed_cs + changed_profits,
    social_welfare_diff_ratio = (changed_social_welfare - social_welfare) / social_welfare * 100,
    model = "NL",
    cluster = as.factor(as.character(cluster))
  )

########################
# NL
########################
product_data_nl <- readr::read_csv(
  here::here(
    "01_analysis", 
    "05_output_all", 
    "AP", 
    "01_nested_logit", 
    "product_data_nested_logit.csv"
  )
)

changed_profits_nl <- readr::read_csv(here::here("01_analysis", "05_output_all", "AP", "01_nested_logit", "changed_profit.csv"))


product_data_nl <- product_data_nl |> 
  cbind(changed_profits_nl) |> 
  dplyr::mutate(
    price_diff_ratio = (changed_prices - prices) / prices * 100,
    cluster = as.factor(as.character(cluster)),
    with_AP = dplyr::if_else(room_type=="Airbnb", paste0(room_type, dplyr::if_else(AP, " with AP", " without AP")), "Booking.com"),
    cs = as.numeric(gsub("\\[|\\]", "", cs))*100,
    # cs = as.numeric(gsub("\\[|\\]", "", cs)),
    changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs))*100,
    # changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs)),
    date = as.Date(stringr::str_extract(market_ids, "^\\d{4}-\\d{2}-\\d{2}")),
    cs_diff_ratio = (changed_cs - cs)/ cs * 100,
    profits = profits*100,
    changed_profits = changed_profits*100,
    tmp = profits,
    profits = changed_profits,
    changed_profits = tmp,
    profits_diff_ratio = (changed_profits - profits) / profits * 100,
    social_welfare = cs + profits,
    changed_social_welfare = changed_cs + changed_profits,
    social_welfare_diff_ratio = (changed_social_welfare - social_welfare) / social_welfare * 100,
    model = "RCNL"
  )


#####################
# Welfare Effects Summary
#####################


# merge data
product_data_merged <- dplyr::bind_rows(product_data_nl, product_data) |> 
  dplyr::rename(
    `CS (baseline)` = cs,
    `CS (self-preferencing)` = changed_cs,
    `% change in CS` = cs_diff_ratio,
    `PS (baseline)` = profits,
    `PS (self-preferencing)` = changed_profits,
    `% change in PS` = profits_diff_ratio,
    `SW (baseline)` = social_welfare,
    `SW (self-preferencing)` = changed_social_welfare,
    `% change in SW` = social_welfare_diff_ratio
  )

modelsummary::datasummary(
  data = product_data_merged,
  formula = `PS (baseline)` + `PS (self-preferencing)` + `% change in PS` ~  (model) * (Mean + SD),
  #   `CS (baseline)` + `CS (self-preferencing)` + `% change in CS` + 
  #   `PS (baseline)` + `PS (self-preferencing)` + `% change in PS` + 
  #   `SW (baseline)` + `SW (self-preferencing)` + `% change in SW`
  # # ~  (model) * (Mean + SD + Min + Max),
  # ~  (model) * (Mean + SD),
  fmt = 3,
  # output = "latex"
)

product_data <- product_data |> 
  dplyr::rename(
    `CS (baseline)` = cs,
    `CS (self-preferencing)` = changed_cs,
    `% change in CS` = cs_diff_ratio,
    `PS (baseline)` = profits,
    `PS (self-preferencing)` = changed_profits,
    `% change in PS` = profits_diff_ratio,
    `SW (baseline)` = social_welfare,
    `SW (self-preferencing)` = changed_social_welfare,
    `% change in SW` = social_welfare_diff_ratio
  )
modelsummary::datasummary(
  data = product_data,
  formula = 
    `PS (baseline)` + `PS (self-preferencing)` + `% change in PS` 
  # ~  (room_type) * (Mean + SD + Min + Max),
  ~   (Mean + SD + Min + Max),
  fmt = 3,
  # output = "latex"
)


product_data |> 
  ggplot2::ggplot(aes(x = `% change in PS`, linetype=room_type))+
  ggplot2::geom_density(alpha = 0.8) +
  ggplot2::scale_x_continuous(limits = c(-10, 10))+
  ggplot2::labs(
    y = "Density",
  ) +
  ggthemes::theme_economist_white()+
  ggplot2::labs(
    #x = "Marginal costs",
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
  
ggsave(here::here("01_analysis", "00_figuretable", "25_profits_diff_ratio_RCNL.pdf"), width = 7, height = 5)

