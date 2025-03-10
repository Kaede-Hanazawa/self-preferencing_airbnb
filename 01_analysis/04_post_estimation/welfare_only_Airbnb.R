##########################
# RCNL
##########################
product_data <- read_csv(
  here::here(
    "01_analysis", 
    "06_output_airbnb_only", 
    "RCNL", 
    "product_data_random_coefficient_nested_logit.csv"
  )
)
changed_profits <- read_csv(here::here("01_analysis", "06_output_airbnb_only", "RCNL", "changed_profit.csv"))

product_data <- product_data |> 
  cbind(changed_profits) |>
  dplyr::mutate(
    price_diff_ratio = (changed_prices - prices) / prices * 100,
    cluster = as.factor(cluster),
    with_AP = if_else(room_type=="Airbnb", paste0(room_type, if_else(AP, " with AP", " without AP")), "Booking.com"),
    cs = as.numeric(gsub("\\[|\\]", "", cs))*100,
    changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs))*100,
    date = as.Date(stringr::str_extract(market_ids, "^\\d{4}-\\d{2}-\\d{2}")),
    cs_diff_ratio = (changed_cs - cs)/ cs * 100,
    profits = profits*100,
    changed_profits = changed_profits*100,
    profits_diff_ratio = (profits - changed_profits) / changed_profits * 100,
    social_welfare = cs + profits,
    changed_social_welfare = changed_cs + changed_profits,
    social_welfare_diff_ratio = (changed_social_welfare - social_welfare) / social_welfare * 100,
    model = "RCNL",
    cluster = as.factor(as.character(cluster)),
    clustering_ids = as.character(clustering_ids),
    nesting_ids = as.character(nesting_ids)
  )

########################
# NL
########################
product_data_nl <- read_csv(
  here::here(
    "01_analysis", 
    "06_output_airbnb_only", 
    "Nested_Logit", 
    "product_data_nested_logit.csv"
  )
)

changed_profits_nl <- read_csv(
  here::here("01_analysis", "06_output_airbnb_only", "Nested_Logit", "changed_profit.csv"))


product_data_nl <- product_data_nl |> 
  cbind(changed_profits_nl) |> 
  dplyr::mutate(
    price_diff_ratio = (changed_prices - prices) / prices * 100,
    cluster = as.factor(as.character(cluster)),
    with_AP = if_else(room_type=="Airbnb", paste0(room_type, if_else(AP, " with AP", " without AP")), "Booking.com"),
    cs = as.numeric(gsub("\\[|\\]", "", cs))*10,
    changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs))*10,
    date = as.Date(stringr::str_extract(market_ids, "^\\d{4}-\\d{2}-\\d{2}")),
    cs_diff_ratio = (changed_cs - cs)/ cs * 100,
    profits = profits*10,
    changed_profits = changed_profits*10,
    profits_diff_ratio = (profits - changed_profits) / changed_profits * 100,
    social_welfare = cs + profits,
    changed_social_welfare = changed_cs + changed_profits,
    social_welfare_diff_ratio = (changed_social_welfare - social_welfare) / social_welfare * 100,
    model = "NL"
  )


#####################
# Welfare Effects Summary
#####################

# merge data
product_data_merged <- dplyr::bind_rows(product_data, product_data_nl) |> 
  dplyr::rename(
    `CS (baseline)` = cs,
    `CS (self-preferencing)` = changed_cs,
    `% change in CS` = cs_diff_ratio,
    `PS (baseline)` = changed_profits,
    `PS (self-preferencing)` = profits,
    `% change in PS` = profits_diff_ratio,
    `SW (baseline)` = social_welfare,
    `SW (self-preferencing)` = changed_social_welfare,
    `% change in SW` = social_welfare_diff_ratio
  )

modelsummary::datasummary(
  data = product_data_merged,
  formula = 
    `CS (baseline)` + `CS (self-preferencing)` + `% change in CS` + 
    `PS (baseline)` + `PS (self-preferencing)` + `% change in PS` + 
    `SW (baseline)` + `SW (self-preferencing)` + `% change in SW`
  #~  (model) * (Mean + SD + Min + Max),
   ~  (model) * (Mean + SD),
  fmt = 3,
  output = "latex"
)
