df_output <- df_final |>
  group_by(room_type) |>
  group_modify(~ {
    if (.y$room_type == "Airbnb") {
      # Airbnb の場合は指定の処理を適用
      .x |>
        # group_by(market_ids, firm_ids, prices) |>
        group_by(market_ids, firm_ids) |>
        mutate(num_of_vacant = n()) |>
        distinct(product_ids, firm_ids, prices, .keep_all = TRUE) |>
        ungroup()
    } else {
      # Booking.comの場合はそのまま返す
      .x
    }
  }) |>
  ungroup()

# unique(df_output$num_of_vacant)
# # unique(df_final$num_of_vacant)
# a <- df_output |> filter(month %in% c(5, 6, 7))
# unique(a$num_of_vacant)
# table(df_output$num_of_vacant)
# unique(df_final$month)
# 
# a <- df_output |> filter(room_type == "Airbnb" & month %in% c(5, 6, 7))
# unique(a$num_of_vacant)
# [1]  1  2  4  7  3  5  6 11  8 10 21 12 16  9 14 13 33 32 17 31 15 29 28 30
# 
# 
# df_final |> 
#   # filter(month %in% c(5, 6, 7)) |>
#   nrow()
#   group_by(market_ids, firm_ids, prices) |> 
#   View()
