### 処理の流れ（使用するデータはtidy_for_PyBLP.Rの後なので、df_final）

# 1. Airbnbのデータだけ取り出す
# 2. market_ids, clusterごとにgroup_by
# 3. 平均価格、平均Xを計算、SDも。
# 4. 計算された平均価格および平均Xから、正規分布より土日データを生成
  # 1. このとき、クラスター4と5については、そのXX%を、DPとする（ランダムでいいかな）
  # 2. 新しく生成するのは、事前に指定したcity_idsのみ（条例の有無を調べる）
  # 3. 曜日としては、土日に増やす。
  # 4. 増やす量は？平均Nとかで良さそう。
  # 5. 土日がいっぱいあると思うのですが！これは全ての土日に対して生成すればok
df_sim <- df_final |>
  dplyr::mutate(date = as.Date(stringr::str_extract(market_ids, "^\\d{4}-\\d{2}-\\d{2}")))


info <- readxl::read_excel(here::here("99_data", "住宅宿泊事業の届出_東京都", "23区_住宅宿泊事業_届出情報_URL.xlsx")) |> 
  select(city_ids, content)
# View(info)
# colnames(df_final)
# unique(df_final$AP)
df_sim <- df_sim |> 
  left_join(info, by = "city_ids") 
  
# View(df_sim)


# 渋谷については全期間を対象に増加させる
# 数は20%ずつくらい？
n_percent <- 0.2
# market_idsごとの、APの割合を計算する
# 生成したデータのうち、APの割合だけ、AP=TRUEとする。
a <- df_sim |>
  group_by(market_ids) |> 
  summarise(AP_ratio = sum(AP) / n())

df_sim <- df_sim |> 
  left_join(a, by = "market_ids") 

a <- df_sim |> 
  group_by(market_ids, cluster) |> 
  summarise(
    mean_price = mean(prices),
    mean_num_of_reviews = mean(num_of_reviews),
    mean_room_score = mean(room_score),
    mean_cleanness_score = mean(cleanness_score),
    mean_staff_communication_score = mean(staff_communication_score),
    mean_location = mean(location),
    sd_price = sd(prices),
    sd_num_of_reviews = sd(num_of_reviews),
    sd_room_score = sd(room_score),
    sd_cleanness_score = sd(cleanness_score),
    sd_staff_communication_score = sd(staff_communication_score),
    sd_location = sd(location),
    market_cluster_n = n()
  )

df_sim <- df_sim |>
  left_join(a, by = c("market_ids", "cluster"))


for(i in unique(df_sim$city_ids)){
  if(i == "shibuya"){
    # 各クラスターとmarket_idsごとにデータを生成して追加
    new_rows <- process_data(df_sim, i)
  } else if (i == "minato") {
    ## TODO: 港区については、7月10日より前のデータが存在しないので、ここについてデータを生成する、という処理を行う必要がある。
    a <- df_sim |> filter(date <= as.Date("2024-07-10") & city_ids == i)
    new_rows <- process_data(a, i)
  }
  
  # 生成された新しいデータを元のデータに追加
  df_sim <- dplyr::bind_rows(df_sim, new_rows)
}




generate_new_data <- function(mean_vals, sd_vals, n_rows, market_id, cluster_id) {
  tibble::tibble(
    market_ids = rep(market_id, n_rows),
    cluster = rep(cluster_id, n_rows),
    prices = pmax(0, rnorm(n_rows, mean_vals["mean_price"], sd_vals["sd_price"])),
    num_of_reviews = pmax(0, rnorm(n_rows, mean_vals["mean_num_of_reviews"], sd_vals["sd_num_of_reviews"])),
    room_score = pmax(0, rnorm(n_rows, mean_vals["mean_room_score"], sd_vals["sd_room_score"])),
    cleanness_score = pmax(0, rnorm(n_rows, mean_vals["mean_cleanness_score"], sd_vals["sd_cleanness_score"])),
    staff_communication_score = pmax(0, rnorm(n_rows, mean_vals["mean_staff_communication_score"], sd_vals["sd_staff_communication_score"])),
    location = pmax(0, rnorm(n_rows, mean_vals["mean_location"], sd_vals["sd_location"]))
  )
}

process_data <- function(df, city_id) {
  df |> 
    dplyr::filter(city_ids == city_id) |>
    dplyr::distinct(market_ids, cluster, market_cluster_n, mean_price, mean_num_of_reviews, mean_room_score, mean_cleanness_score, mean_staff_communication_score, mean_location, sd_price, sd_num_of_reviews, sd_room_score, sd_cleanness_score, sd_staff_communication_score, sd_location) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      new_data = list(generate_new_data(
        mean_vals = c(
          mean_price = mean_price,
          mean_num_of_reviews = mean_num_of_reviews,
          mean_room_score = mean_room_score,
          mean_cleanness_score = mean_cleanness_score,
          mean_staff_communication_score = mean_staff_communication_score,
          mean_location = mean_location
        ),
        sd_vals = c(
          sd_price = sd_price,
          sd_num_of_reviews = sd_num_of_reviews,
          sd_room_score = sd_room_score,
          sd_cleanness_score = sd_cleanness_score,
          sd_staff_communication_score = sd_staff_communication_score,
          sd_location = sd_location
        ),
        n_rows = round(market_cluster_n * n_percent),
        market_id = market_ids,
        cluster_id = cluster
      ))
    ) |>
    dplyr::select(new_data) |>
    tidyr::unnest(cols = c(new_data))
}


View(new_rows)




