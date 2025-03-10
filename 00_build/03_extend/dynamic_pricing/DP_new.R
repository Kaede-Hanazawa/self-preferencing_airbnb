# データの読み込み
path <- here::here("99_data", "Airbnb.com", "airbnb-date-price")

df_DP_raw <- read_data(path = path)

df_DP <- create_23ku_dummy(clean_airbnb_data_for_DP(df_DP_raw), "物件名2") |> 
  dplyr::select(物件名1, ホスト名, データ取得日, 日付, prices)

# 各物件ごとに価格変動を計算し、Dynamic Pricing (DP)を判定
df_DP_new <- df_DP |> 
  arrange(物件名1, 日付) |> 
  group_by(物件名1) |> 
  mutate(diff_prices = prices - lag(prices),
         DP = any(!is.na(diff_prices) & diff_prices != 0, na.rm = TRUE)) |> 
  ungroup()

# DPを利用している物件数と割合を計算
count <- df_DP_new |> 
  filter(DP) |> 
  distinct(物件名1) |> 
  nrow()

DP_ratio <- round(count / length(unique(df_DP$物件名1)) * 100, 2)

print(glue::glue("DPを利用する物件の数: {count}"))
print(glue::glue("DPを利用する物件の割合: {DP_ratio}%"))

# 元のデータ(df_merged)にDP情報を結合
df_DP_summary <- df_DP_new |> 
  select(物件名1, DP) |> 
  distinct(物件名1, .keep_all = TRUE)

df <- df_merged |> 
  left_join(df_DP_summary, by = "物件名1")




# a <- df |> filter(room_type == "Airbnb")
# unique(a$num_of_vacant)
# [1] NA