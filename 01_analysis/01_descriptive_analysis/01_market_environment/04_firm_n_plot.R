df_new <- df_output |> 
  mutate(date = as.Date(sub(":.*", "", market_ids)))



# room_type ごとのカウントを計算
plot_data <- df_new %>%
  filter(room_type %in% c("Airbnb", "Booking.com")) %>%
  group_by(date, room_type) %>%
  summarize(count = n(), .groups = "drop")

# プロット
ggplot(plot_data, aes(x = date, y = count, color = room_type, shape = room_type)) +
  geom_point(size = 4, fill = "white") +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("Airbnb" = "#fc2fbf", "Booking.com" = "#00B9E3")) +
  scale_shape_manual(values = c("Airbnb" = 21, "Booking.com" = 23)) + # 丸(21)と菱形(23)に変更
  scale_x_date(
    date_breaks = "3 days",    # メモリの間隔を3日ごとに設定
    date_labels = "%m/%d"      # 日付形式を月/日の形式に設定
  ) +
  labs(
    x = NULL,
    y = "Number of obs.",
    color = "Room Type",
    shape = "Room Type"
  ) +
  theme_economist_white() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1)  # x軸のラベルを90度回転
  )

ggsave(here::here("01_analysis", "00_figuretable", "03_Firm_N.pdf"), width = 13, height = 6)
