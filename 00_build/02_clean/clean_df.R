source(here::here("00_build", "02_clean", "clean_functions.R"))

# # tibbleデータフレームとしてdf_airbnb_rawを読み込む
# df_airbnb_raw <- readr::read_csv(here::here("00_build", "01_load", "df_airbnb_raw.csv"))

# # tibbleデータフレームとしてdf_booking_rawを読み込む
# df_booking_raw <- readr::read_csv(here::here("00_build", "01_load", "df_booking_raw.csv"))


df_airbnb <- create_23ku_dummy(clean_airbnb_data(df_airbnb_raw), "物件名2")
df_booking <- create_23ku_dummy(data_cleaning_for_booking(df_booking_raw), "住所")

df_merged <- dplyr::bind_rows(df_airbnb, df_booking) |>
    dplyr::mutate(
        hotel_rank = as.factor(hotel_rank),
        day_of_week = case_when(
            day_of_week == 1 ~ "Sun",
            day_of_week == 2 ~ "Mon",
            day_of_week == 3 ~ "Tue",
            day_of_week == 4 ~ "Wed",
            day_of_week == 5 ~ "Thu",
            day_of_week == 6 ~ "Fri",
            day_of_week == 7 ~ "Sat"
          ),
        day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
        # 月ダミー列を作る
        month = month(ymd(チェックイン)),
        チェックイン_日付 = as.character(チェックイン_日付),
    ) |> 
  filter(!is.na(prices))



# Airbnbのみを残す場合
if(only_Airbnb){
  df_merged <- df_merged |>
    filter(room_type == "Airbnb")
} else {
  # チェックイン_日付、がAirbnbとHotelで同じである行のみ残す
  
  # IS_AIRBNBがTRUEのユニークなチェックイン_日付を取得
  dates_true <- df_merged |>
    filter(IS_AIRBNB == TRUE) |>
    pull(チェックイン_日付) |>
    unique()
  # IS_AIRBNBがFALSEのユニークなチェックイン_日付を取得
  dates_false <- df_merged |>
    filter(IS_AIRBNB == FALSE) |>
    pull(チェックイン_日付) |>
    unique()
  
  #  dates_false と dates_true を Date 型に変換
  dates_false <- as.character(dates_false)
  dates_true <- as.character(dates_true)
  
  # 重複している日付を抽出して新しいベクトルに格納
  common_dates <- intersect(dates_false, dates_true)
  
  df_merged <- df_merged |>
    filter(チェックイン_日付 %in% common_dates)
}
