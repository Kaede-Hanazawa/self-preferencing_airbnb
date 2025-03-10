df_booking_raw <- df_booking_raw |> 
  dplyr::mutate(
    num_of_beds = stringr::str_extract_all(ベッド情報, "(シングルベッド|ダブルベッド|クインベッド|キングベッド|段ベッドベッド|ソファベッド|布団|singlebed|doublebed|largedoublebed|extralargedoublebed|bunkbed|sofabed|futon)\\d+") |> 
      purrr::map_dbl(~ sum(
        stringr::str_extract(.x, "\\d+") |> 
          as.numeric() *
          dplyr::case_when(
            stringr::str_detect(.x, "シングルベッド|singlebed") ~ 1,
            stringr::str_detect(.x, "ダブルベッド|doublebed") ~ 1.56,
            stringr::str_detect(.x, "クインベッド|largedoublebed") ~ 1.78,
            stringr::str_detect(.x, "キングベッド|extralargedoublebed") ~ 2,
            stringr::str_detect(.x, "段ベッドベッド|bunkbed") ~ 2,
            stringr::str_detect(.x, "ソファベッド|sofabed") ~ 1,
            stringr::str_detect(.x, "布団|futon") ~ 1,
            TRUE ~ 0
          )
      ))
  )


df_airbnb_raw <- df_airbnb_raw |> 
  dplyr::mutate(
    num_of_beds = stringr::str_extract(
      物件情報,
      "(シ[⁠　]*ン[⁠　]*グ[⁠　]*ル[⁠　]*ベ[⁠　]*ッ[⁠　]*ド|ダ[⁠　]*ブ[⁠　]*ル[⁠　]*ベ[⁠　]*ッ[⁠　]*ド|セ[⁠　]*ミ[⁠　]*ダ[⁠　]*ブ[⁠　]*ル[⁠　]*ベ[⁠　]*ッ[⁠　]*ド|ク[⁠　]*イ[⁠　]*ン[⁠　]*ベ[⁠　]*ッ[⁠　]*ド|キ[⁠　]*ン[⁠　]*グ[⁠　]*ベ[⁠　]*ッ[⁠　]*ド|二[⁠　]*段[⁠　]*ベ[⁠　]*ッ[⁠　]*ド|ソ[⁠　]*フ[⁠　]*ァ[⁠　]*ベ[⁠　]*ッ[⁠　]*ド|ベ[⁠　]*ッ[⁠　]*ド)\\d+"
    ) |> 
      (\(x) {
        bed_count <- stringr::str_extract(x, "\\d+") |> as.numeric()
        bed_count * dplyr::case_when(
          stringr::str_detect(x, "シ[⁠　]*ン[⁠　]*グ[⁠　]*ル[⁠　]*ベ[⁠　]*ッ[⁠　]*ド") ~ 1,
          stringr::str_detect(x, "ダ[⁠　]*ブ[⁠　]*ル[⁠　]*ベ[⁠　]*ッ[⁠　]*ド") ~ 1.56,
          stringr::str_detect(x, "ク[⁠　]*イ[⁠　]*ン[⁠　]*ベ[⁠　]*ッ[⁠　]*ド") ~ 1.78,
          stringr::str_detect(x, "キ[⁠　]*ン[⁠　]*グ[⁠　]*ベ[⁠　]*ッ[⁠　]*ド") ~ 2,
          stringr::str_detect(x, "二[⁠　]*段[⁠　]*ベ[⁠　]*ッ[⁠　]*ド") ~ 2,
          stringr::str_detect(x, "ソ[⁠　]*フ[⁠　]*ァ[⁠　]*ベ[⁠　]*ッ[⁠　]*ド") ~ 1,
          stringr::str_detect(x, "セ[⁠　]*ミ[⁠　]*ダ[⁠　]*ブ[⁠　]*ル[⁠　]*ベ[⁠　]*ッ[⁠　]*ド") ~ 1.33,
          stringr::str_detect(x, "ベ[⁠　]*ッ[⁠　]*ド") ~ 1,
          TRUE ~ 0
        )
      })(),
    num_of_beds = ifelse(
      is.na(num_of_beds),
      stringr::str_extract(物件情報, "ゲスト\\d+人") |> 
        stringr::str_extract("\\d+") |> 
        as.numeric() |> 
        (\(x) round(x / 2))(),
      num_of_beds
    )
  ) 




