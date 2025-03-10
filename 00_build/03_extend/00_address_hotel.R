df_hotel <- df_booking |> 
  dplyr::mutate(
    郵便番号 = stringr::str_extract(住所, "\\d{3}-\\d{4}"),
    住所 = stringr::str_remove(住所, "〒\\d{3}-\\d{4}東京都,?"),
    住所 = stringr::str_replace(住所, "^東京都,", "東京都")
  )

df_hotel |> 
  select(
    住所, 郵便番号
  )

df_booking |> 
  select(
    住所
  )
unique(df_hotel$住所)
