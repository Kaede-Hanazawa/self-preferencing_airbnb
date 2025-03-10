# データを読み込む
read_data <- function(pattern = "^(新宿|上野).*\\.xlsx$", path) {
  # 指定されたパターンに一致するファイルを検索
  excel_files <- base::list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE
  )
  
  # ファイルが見つからない場合はメッセージを表示して NULL を返す
  if (base::length(excel_files) == 0) {
    base::message("指定されたパターンに一致するファイルが見つかりませんでした。")
    return(NULL)
  }
  
  # ファイルを読み込んでリストに格納
  df_list <- purrr::map(excel_files, ~ readxl::read_excel(.x))
  
  # 各データフレームの "チェックイン" 列を character 型に変換
  df_list <- purrr::map(df_list, ~ mutate(.x, チェックイン = as.character(チェックイン)))

  # リストを1つのデータフレームに結合
  combined_df <- dplyr::bind_rows(df_list, .id = "file_name")
  
  # 結合したデータフレームを返す
  return(combined_df)
}


df_airbnb_raw <- read_data(path = here::here("99_data", "Airbnb.com", "tokyo")) |>
  dplyr::mutate(room_type = "Airbnb")

df_booking_raw <- read_data(pattern = "^東京.*\\.xlsx$", path = here::here("99_data", "booking.com", "tokyo")) |>
  dplyr::mutate(
    room_type = "Booking.com", 
    チェックイン = as.character(チェックイン)
    )


# AirbnbのデータをCSVファイルとして保存
write.csv(df_airbnb_raw, file = here::here("00_build", "01_load", "df_airbnb_raw.csv"), row.names = FALSE)

# Booking.comのデータをCSVファイルとして保存
write.csv(df_booking_raw, file = here::here("00_build", "01_load", "df_booking_raw.csv"), row.names = FALSE)

