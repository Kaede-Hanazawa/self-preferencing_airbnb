pacman::p_load(tidyverse, dplyr, readxl, modelsummary, openxlsx, kableExtra)

# 23区のダミー変数を作成
create_23ku_dummy <- function(df, column_name){
  df |> 
    dplyr::mutate(
      shibuya = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "渋谷|shibuya|Shibuya"), 1, 0),
      shinjuku = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "新宿|shinjuku|Shinjuku"), 1, 0),
      chiyoda = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "千代田|chiyoda|Chiyoda"), 1, 0),
      chuo = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "中央|chuo|Chuo"), 1, 0),
      minato = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "港|minato|Minato"), 1, 0),
      shinagawa = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "品川|shinagawa|Shinagawa"), 1, 0),
      bunkyo = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "文京|bunkyo|Bunkyo"), 1, 0),
      taito = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "台東|taito|Taito"), 1, 0),
      sumida = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "墨田|sumida|Sumida"), 1, 0),
      koto = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "江東|koto|Koto"), 1, 0),
      meguro = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "目黒|meguro|Meguro"), 1, 0),
      ota = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "大田|ota|Ota"), 1, 0),
      setagaya = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "世田谷|setagaya|Setagaya"), 1, 0),
      nakano = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "中野|nakano|Nakano"), 1, 0),
      suginami = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "杉並|suginami|Suginami"), 1, 0),
      toshima = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "豊島|toshima|Toshima"), 1, 0),
      kita = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "北|kita|Kita"), 1, 0),
      arakawa = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "荒川|arakawa|Arakawa"), 1, 0),
      itabashi = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "板橋|itabashi|Itabashi"), 1, 0),
      nerima = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "練馬|nerima|Nerima"), 1, 0),
      adachi = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "足立|adachi|Adachi"), 1, 0),
      katsushika = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "葛飾|katsushika|Katsushika"), 1, 0),
      edogawa = dplyr::if_else(stringr::str_detect(!!dplyr::sym(column_name), "江戸川|edogawa|Edogawa"), 1, 0),
      wards = dplyr::case_when(
        shibuya == 1 ~ "shibuya",
        shinjuku == 1 ~ "shinjuku",
        chiyoda == 1 ~ "chiyoda",
        chuo == 1 ~ "chuo",
        minato == 1 ~ "minato",
        shinagawa == 1 ~ "shinagawa",
        bunkyo == 1 ~ "bunkyo",
        taito == 1 ~ "taito",
        sumida == 1 ~ "sumida",
        koto == 1 ~ "koto",
        meguro == 1 ~ "meguro",
        ota == 1 ~ "ota",
        setagaya == 1 ~ "setagaya",
        nakano == 1 ~ "nakano",
        suginami == 1 ~ "suginami",
        toshima == 1 ~ "toshima",
        kita == 1 ~ "kita",
        arakawa == 1 ~ "arakawa",
        itabashi == 1 ~ "itabashi",
        nerima == 1 ~ "nerima",
        adachi == 1 ~ "adachi",
        katsushika == 1 ~ "katsushika",
        edogawa == 1 ~ "edogawa",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(-shibuya, -shinjuku, -chiyoda, -chuo, -minato, -shinagawa, -bunkyo, -taito, -sumida, 
                  -koto, -meguro, -ota, -setagaya, -nakano, -suginami, -toshima, -kita, -arakawa, 
                  -itabashi, -nerima, -adachi, -katsushika, -edogawa)
}


# Airbnbのデータを加工する
clean_airbnb_data <- function(df) {
  
  if(exclude_guests_3over){
    df <- df |> 
      filter(!(grepl("ゲスト[1-9][0-9]*人", 物件情報) & !grepl("ゲスト2人", 物件情報)))
  }
  
  df <- df |>
    # 重複を削除
    # dplyr::distinct(物件名1, 物件名2, ホスト名, チェックイン, データ取得日, 価格, 本人確認, 通常価格, 清掃料金, .keep_all = TRUE) |>
    # 'Unnamed: 0' 列を削除
    dplyr::select(-dplyr::any_of("Unnamed: 0")) |>
    # 通常価格がNAの行は削除
    dplyr::filter(!is.na(通常価格)) |>
    # NAを0に置換、合計価格の列を追加、日付処理
    dplyr::mutate(
      `Airbnbサービス料` = tidyr::replace_na(`Airbnbサービス料`, 0),
      清掃料金 = tidyr::replace_na(清掃料金, 0),
      prices = `Airbnbサービス料` + 清掃料金 + 通常価格,
      チェックイン_日付 = lubridate::ymd(チェックイン),
      day_of_week = lubridate::wday(チェックイン_日付),
      チェックイン_曜日 = paste0(c('日', '月', '火', '水', '木', '金', '土')[day_of_week], '曜日'),
      # ホストのホスティング歴、を変数として使用する
      hosting_years = as.numeric(ホストのホスティング歴),
      IS_AIRBNB = TRUE
    ) |>
    dplyr::select(
        -...1, -file_name, -物件情報,-通常価格, -清掃料金, -`Airbnbサービス料`, -搭載情報の正確さ, -価格, -宿泊人数, -部屋タイプ, -建物タイプ
    ) |>
    # 列名の修正
    dplyr::rename(
        "num_of_reviews" = レビュー件数,
        "location" = ロケーション,
        "room_score" = 平均スコア,
        "cleanness_score" = 清潔さ,
        "staff_communication_score" = コミュニケーション,
        "host_num_of_reviews" = ホストのレビュー件数,
        "host_hosting_years" = ホストのホスティング歴
    )

  if(!only_Airbnb & adjust_X == "hotel"){
    df <- df |>
      dplyr::mutate(
        # Airbnbの各評価値をホテルに合わせてスケーリングするバージョン
        room_score = scales::rescale(room_score, to = c(0, 10)),
        staff_communication_score = scales::rescale(staff_communication_score, to = c(0, 10)),
        cleanness_score = scales::rescale(cleanness_score, to = c(0, 10)),
        location = scales::rescale(location, to = c(0, 10))
      )
  }

  return(df)
}


# Booking.comのデータをクリーニングする関数
data_cleaning_for_booking <- function(df){

  df <- df |>
  # 元の料金がNAの行を削除
  dplyr::filter(!is.na(元の料金)) |>
  mutate(
    # NAは0に変換
    期間限定セール = dplyr::if_else(is.na(期間限定セール), 0, 期間限定セール),
    清掃料金 = dplyr::if_else(is.na(清掃料金), 0, 清掃料金),
  ) |>
  # 外れ値の除去
  filter(清掃料金 < 1000000 & 元の料金 < 800000) |>
  filter(トータルスコア <= 10.0) |>
  mutate(
    prices = 元の料金 - 期間限定セール + 清掃料金,
    チェックイン_日付 = as.Date(チェックイン),
    day_of_week = lubridate::wday(チェックイン_日付),
    チェックイン_曜日 = paste0(c('日', '月', '火', '水', '木', '金', '土')[day_of_week], '曜日'),
    IS_AIRBNB = FALSE
  ) |> 
  select(
    # 不要な列を削除
    -...1,
    -部屋ID,
    -count
  ) |>
  # 列名の修正
  dplyr::rename(
      "hotel_rank" = ホテルランク,
      "room_score" = トータルスコア,
      "staff_communication_score" = スタッフ,
      "cleanness_score" = 清潔さ,
      "location" = ロケーション,
      "num_of_reviews" = 口コミ件数,
      # "物件名1" = 物件名, # これはAirbnbに統一する必要なし。
      "num_of_vacant" = 部屋の空室状況
  )
  
  if(adjust_X == "airbnb"){
    df <- df |>
       dplyr::mutate(
        # ホテルの各評価値をAirbnbに合わせてスケーリングする
        room_score = scales::rescale(room_score, to = c(1, 5)),
        staff_communication_score = scales::rescale(staff_communication_score, to = c(1, 5)),
        cleanness_score = scales::rescale(cleanness_score, to = c(1, 5)),
        location = scales::rescale(location, to = c(1, 5))
      )
  }
  
  return(df)
 
}



# Airbnbのデータを加工する（for dynamic pricing）
clean_airbnb_data_for_DP <- function(df) {
  
  df |>
    # NAを0に置換、合計価格の列を追加、日付処理
    dplyr::mutate(
      `Airbnbサービス料` = tidyr::replace_na(`Airbnbサービス料`, 0),
      清掃料金 = tidyr::replace_na(清掃料金, 0),
      # prices = `Airbnbサービス料` + 清掃料金 + 通常価格,
    ) |>
    dplyr::select(
      -...1, -file_name, -物件情報, -清掃料金, -`Airbnbサービス料`, -搭載情報の正確さ, -価格, -宿泊人数, -部屋タイプ, -建物タイプ
    ) |>
    # 列名の修正
    dplyr::rename(
      "prices" = 通常価格,
      "num_of_reviews" = レビュー件数,
      "location" = ロケーション,
      "room_score" = 平均スコア,
      "cleanness_score" = 清潔さ,
      "staff_communication_score" = コミュニケーション,
      "host_num_of_reviews" = ホストのレビュー件数,
      "host_hosting_years" = ホストのホスティング歴
    )
}
