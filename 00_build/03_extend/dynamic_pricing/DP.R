##################
# 目的:
# Airbnbホストが、Dynamic Pricingをしているかどうかを判定する。

# 方法:
# df_DP$データ取得日ごとに、df_DP$日付の価格を比較し、価格が変動しているかどうかを判定する。
##################

# データの読み込み
path <- here::here("99_data", "Airbnb.com", "airbnb-date-price")

df_DP_raw <- read_data(path = path)

df_DP <- create_23ku_dummy(clean_airbnb_data_for_DP(df_DP_raw), "物件名2") |> 
  dplyr::select(物件名1, ホスト名, データ取得日, 日付, prices)

# df_DP$データ取得日に対して、異なるdf_DP$日付における価格を比較する。
# 価格が変動している場合、Dynamic Pricingをしていると判定する。

df_DP_new <- tibble()
count <- 0 # DPをしている物件数

total_loops <- length(unique(df_DP$物件名1))
progress_intervals <- seq(0, total_loops, by = total_loops * 0.1)
current_loop <- 0

for (i in unique(df_DP$物件名1)){
  current_loop <- current_loop + 1
  
  # 進捗を表示
  if  (current_loop %% 100 == 0) {
    percent_complete <- (current_loop / total_loops) * 100
    print(paste0("ループの", round(percent_complete, 1), "%が完了"))
  }
  
  flag <- FALSE
  d <- df_DP |> 
    filter(物件名1 == i)
  
  for (j in unique(df_DP$日付)){
    d2 <- d |> 
      filter(日付 == j) |> 
      mutate(
        diff_prices = prices - lag(prices),
        DP = any(!is.na(diff_prices) & diff_prices != 0)
      )
    if (isTRUE(d2$DP[1])){
      flag <- TRUE
    }
    df_DP_new <- rbind(df_DP_new, d2)
  }
  
  if(flag){
    df_DP_new <- df_DP_new |>
      mutate(DP = ifelse(物件名1 == i, TRUE, DP))
    count <- count + 1
  }
}


print(glue::glue("DPを利用する物件の数: {count}"))
DP_ratio <- round(count/length(unique(df_DP_new$物件名1))*100, 2)

print(glue::glue("DPを利用する物件の割合: {DP_ratio}%"))

# DP列がTRUEになった物件名1について、DPをしていると判定する。元のデータ(df_merged)にDP情報を結合する。
df_DP_new <- df_DP_new |> 
  select(物件名1, DP) |> 
  distinct(物件名1, .keep_all = TRUE)

# dfとdf_DP_newをleft_joinする
df <- df_merged |> 
  left_join(df_DP_new, by = "物件名1")







