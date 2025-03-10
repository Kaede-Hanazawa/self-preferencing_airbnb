# only_Airbnbの値によって選択する列を設定
if (only_Airbnb) {
  selected_columns <- c("market_ids", "city_ids", "firm_ids", "product_ids", "num_of_reviews", 
                        "room_score", "cleanness_score", "staff_communication_score", "day_of_week", 
                        "month", "location", "room_type", "prices", "cluster", "num_of_vacant", "num_of_beds",
                        "AP"#, "hosting_years", "host_num_of_reviews"
                        )
} else {
  selected_columns <- c("market_ids", "city_ids", "firm_ids", "product_ids", "num_of_reviews", 
                        "room_score", "cleanness_score", "staff_communication_score", "day_of_week", 
                        "month", "location", "room_type", "prices", "cluster", "num_of_vacant", "AP", 
                        "num_of_beds"
                        #, "hosting_years"
                        #"host_num_of_reviews"
                        )
}

# メイン処理
df_final <- df |> 
  rename(
    city_ids = wards
  ) |> 
  filter(!is.na(city_ids)) |> 
  mutate(
    # market_idsを作る
    market_ids = case_when(
      cluster_mkt_ids ~ paste0(チェックイン, ":", city_ids, ":", cluster),
      TRUE ~ paste0(チェックイン, ":", city_ids)
    ),
    # firm_idsはAirbnbの場合、ホスト名。Booking.comの場合、物件名
    firm_ids = case_when(
      room_type == "Airbnb" ~ ホスト名,
      room_type == "Booking.com" ~ 物件名
    ),
    # product_idsを作る。Airbnbの場合、物件名1。Booking.comの場合、paste0(物件名1, ":", アパートメントタイプ)
    product_ids = case_when(
      room_type == "Airbnb" ~ 物件名1,
      room_type == "Booking.com" ~ paste0(物件名1, ":", アパートメントタイプ)
    ),
    # num_of_reviewsについて、Airbnbの場合は、ホストのレビュー件数を採用する
    # num_of_reviews = case_when(
    #   room_type == "Airbnb" ~ host_num_of_reviews,
    #   room_type == "Booking.com" ~ num_of_reviews
    # ),
    # hosting_years = case_when(
    #   room_type == "Airbnb" ~ hosting_years,
    #   room_type == "Booking.com" ~ -1
    # ),
  ) |> 
  select(all_of(selected_columns)) |> 
  mutate(
    num_of_vacant = ifelse(is.na(num_of_vacant), 1, num_of_vacant),
    AP = ifelse(is.na(AP), FALSE, AP),
    # merger_ids (PyBLPの予約語)を作る。
    merger_ids = case_when(
      # (room_type == "Airbnb" & cluster >= n_clusters_AP) ~ "Airbnb",
      AP ~ "Airbnb",
      TRUE ~ firm_ids
    ),
    prices = prices / price_divide
    # ---------------------------------------
    # num_of_reviewを100で割って、各スコアにかける
    # ---------------------------------------
    # num_of_reviews = num_of_reviews / 1000,
    # room_score = room_score * num_of_reviews,
    # cleanness_score = cleanness_score * num_of_reviews,
    # staff_communication_score = staff_communication_score * num_of_reviews,
    # location = location * num_of_reviews,
  ) |> 
  as_tibble() |>
  na.omit() # remove NA




