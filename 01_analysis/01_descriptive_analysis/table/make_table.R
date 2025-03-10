###########################
# データの記述分析: 全変数テーブルの作成
###########################

# 使用するデータ: df_final (given)

colnames(df_final)

d <- df_final

###########################
# 全データ
###########################
modelsummary::datasummary_skim(
  d,
  note = paste0("全ての市場をまとめたデータ. サンプルサイズ: ", nrow(d))
)

###########################
# Airbnbのみ
###########################
a <- d |> 
  filter(room_type == "Airbnb")
modelsummary::datasummary_skim(
  a,
  note = paste0("全ての市場をまとめたデータ (only Airbnb). サンプルサイズ: ", nrow(a))
)


###########################
# Booking.comのみ
###########################
a <- d |> 
  filter(room_type == "Booking.com")
modelsummary::datasummary_skim(
  a,
  note = paste0("全ての市場をまとめたデータ (only Booking.com). サンプルサイズ: ", nrow(a))
)



###########################
# 23区ごとデータ
###########################

d |> 
  group_by(city_ids, room_type) |>
  summarise(
    n = n(),
    across(
      c(num_of_reviews, room_score, cleanness_score, staff_communication_score, prices, AP, num_of_vacant),
      list(
        mean = ~ round(mean(.x, na.rm = TRUE), 2),
        sd = ~ round(sd(.x, na.rm = TRUE), 2),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  ) |> 
  View()





# 価格について、airbnbとbooking.comを比較
a <- d |> 
  select(
    prices, room_type
  )

modelsummary::datasummary_balance(
  ~room_type,
  data = a,
  stars = TRUE
)



a <- d |> 
  select(
    prices, room_type, day_of_week
  )
modelsummary::datasummary_balance(
  ~day_of_week,
  data = a,
  stars = TRUE
)

