##################
# 目的:
# Airbnbホストが、APを利用するかどうかを判定する。

# 方法:
# (DP==TRUE and hotel_rank ≤ 3)の条件を満たすホストをAP==TRUEとする！

# 使用するデータフレーム: df
##################

df <- df |> 
  mutate(
    # AP = TRUE
    AP = if_else((DP==TRUE & cluster >= 4 & room_type == "Airbnb"), TRUE, FALSE)
  )
  

total_airbnb <- nrow(df |> filter(room_type == "Airbnb"))
AP_ratio <- round(nrow(df |> filter(AP == TRUE)) / total_airbnb * 100, 1)
print(glue::glue("AP ratio: {AP_ratio}%"))

# df |>
#   filter(month %in% c(3, 4)) |>
#   filter()