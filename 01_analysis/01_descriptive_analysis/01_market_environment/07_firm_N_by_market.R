# market_idsごとにroom_typeごとの数を集計
df_counts <- df_output |> 
  # filter(shares >= 0.03) |> 
  group_by(market_ids, room_type, firm_ids) |> 
  summarise(count = n(), .groups = "drop") 

# ヒストグラムの作成
df_counts |> 
  ggplot(aes(x = total, y = count, fill = room_type)) +
  geom_bar(stat = "identity") + # room_typeごとの数を積み上げて表示
  labs(x = "N", y = "Frequency", fill = NULL) +
  theme_economist_white() +
  # scale_fill_manual(values = c("Airbnb" = "#fc2fbf", "Booking.com" = "#00B9E3")) + # 色を指定
  theme(
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)), # y軸ラベルを太字にし、メモリから離す
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)), # x軸ラベルを太字にし、メモリから離す
    legend.position = "bottom", # 凡例を下に配置
    legend.text = element_text(size = 8) # 凡例テキストのサイズを小さくする
  )

ggsave(here::here("01_analysis", "00_figuretable", "12_Firm_N_by_room_type_hist.pdf"), width = 7, height = 5)


ggplot2::ggplot(df_counts, ggplot2::aes(x = count, fill = room_type)) +
  ggplot2::geom_histogram(binwidth = 1, position = "stack") + # ヒストグラムを縦にスタック
  ggplot2::labs(
    title = NULL,
    x = "Firm N",
    y = "Count"
  ) +
  ggthemes::theme_economist_white() +
  theme(
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)), # y軸ラベルを太字にし、メモリから離す
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)), # x軸ラベルを太字にし、メモリから離す
    legend.position = "bottom", # 凡例を下に配置
    legend.text = element_text(size = 12) # 凡例テキストのサイズを小さくする
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 20), # x軸の範囲を 0 から 50 に設定
    breaks = seq(0, 20, by = 1) # 10単位で目盛りを設定
  ) 

ggsave(here::here("01_analysis", "00_figuretable", "19_Firm_N_count.pdf"), width = 7, height = 5)

# 使用されている色のコード："#F8766D" "#00BFC4"
