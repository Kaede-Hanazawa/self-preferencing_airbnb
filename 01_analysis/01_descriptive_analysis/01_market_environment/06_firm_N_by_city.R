df_new <- df_output |> 
  mutate(date = as.Date(sub(":.*", "", market_ids)))

# 1. market_idsおよびcity_idsごとに、room_typeごとのカウントを計算
df_counts <- df_new |>
  filter(room_type %in% c("Airbnb", "Booking.com")) |>  # 必要なroom_typeのみを抽出
  group_by(date, market_ids, city_ids, room_type) |>
  summarize(count = n(), .groups = "drop")

# 2. ggplot2でプロット
ggplot(df_counts, aes(x = date, y = count, color = room_type, linetype = room_type)) +
  geom_line(size = 1) +
  labs(x = "Date", y = "Count", title = "Count of Room Types by Date and City") +
  scale_color_manual(values = c("Airbnb" = "#fc2fbf", "Booking.com" = "#00B9E3")) +
  scale_linetype_manual(values = c("Airbnb" = "solid", "Booking.com" = "solid")) +  # Airbnbは実線、Booking.comは破線
  facet_wrap(~ city_ids) +
  theme_minimal()




# 1. room_typeが"Airbnb"または"Booking.com"のみを抽出し、日付ごとにカウント
df_counts <- df_new |>
  filter(room_type %in% c("Airbnb", "Booking.com")) |>  # 必要なroom_typeのみを抽出
  group_by(date, market_ids, city_ids, room_type, day_of_week) |>
  summarize(count = n(), .groups = "drop")


View(df_counts)

# 2. day_of_weekとcity_idsごとにcountの平均を計算
df_avg_counts <- df_counts |>
  group_by(day_of_week, city_ids, room_type) |>
  summarize(avg_count = mean(count), .groups = "drop")

# 3. ggplot2でプロット
ggplot(df_counts, aes(x = day_of_week, y = count, color = room_type, linetype = room_type, group = room_type)) +
  geom_line(size = 1) +
  labs(x = NULL, y = "Total N", color = NULL, linetype = NULL) +  # 凡例のタイトルを削除
  scale_color_manual(values = c("Airbnb" = "#fc2fbf", "Booking.com" = "#00B9E3")) +
  scale_linetype_manual(values = c("Airbnb" = "solid", "Booking.com" = "solid")) +
  facet_wrap(~ city_ids, scales = "free_x") +  # 各プロットにx軸を表示するための設定
  theme_economist_white() +
  theme(
    strip.placement = "outside",  # facetラベルを外側に配置
    panel.spacing = unit(1, "lines"),  # パネル間のスペースを調整
    axis.title.y = element_text(margin = margin(r = 10), face = "bold"),  # y軸タイトルを太字にし、メモリから離す
    legend.position = "bottom"  # 凡例を下に配置
  )

ggsave(here::here("01_analysis", "00_figuretable", "09_Firm_N_by_city.pdf"), width = 15, height = 10)









# 
df_summary <- df_counts |>
  group_by(city_ids, room_type) |>
  summarize(
    mean_n = mean(count),
    total_n = sum(count),
    .groups = "drop"
  )


ggplot2::ggplot(df_summary, ggplot2::aes(x = city_ids, y = total_n, fill = room_type)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::labs(
    title = NULL,
    x = NULL,
    y = "Total Count of Listings",
    fill = NULL
  ) +
  ggthemes::theme_economist_white() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5), # x軸ラベルを90度回転
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)), # y軸タイトルを太字にし、メモリと間隔を確保
    legend.position = "bottom",
    legend.text = element_text(size = 12) # 凡例テキストのサイズを小さくする
  )

ggsave(here::here("01_analysis", "00_figuretable", "18_Firm_N_count_by_city.pdf"), width = 7, height = 5)

ggplot2::ggplot(df_summary, ggplot2::aes(x = city_ids, y = total_n, fill = room_type)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::labs(
    title = "Stacked Total Count by city_ids and room_type",
    x = "City IDs",
    y = "Total Count"
  ) +
  ggthemes::theme_economist_white()


