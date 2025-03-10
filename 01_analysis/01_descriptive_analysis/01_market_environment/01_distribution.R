# ### 概要
# `create_histogram` 関数は、指定されたデータフレームからx軸と塗り分けに基づいてヒストグラムを作成し、指定のファイル名で保存します。
# ### 引数
# - `data`: ヒストグラムを作成するためのデータフレーム。
# - `x_column`: x軸にプロットする列の名前。
# - `colors`: 各カテゴリの塗り色を指定するベクトル（デフォルトは`c("#fc2fbf", "#00B9E3")`）。
# - `upper`: x軸の最大値を指定（デフォルトは2）。
# - `lower`: x軸の最小値を指定（デフォルトは0）。
# - `bin_width`: ヒストグラムのビンの幅を指定（デフォルトはNULLで自動設定）。
# - `x_label`: x軸のラベル（指定しない場合は`x_column`がラベルになる）。
# - `y_label`: y軸のラベル（デフォルトは"Count"）。
# - `file_name`: 保存するファイルの名前（デフォルトは"hist"）。
# - `plot_width`: 保存するプロットの幅（デフォルトは8）。
# - `plot_height`: 保存するプロットの高さ（デフォルトは6）。
create_histogram <- function(data, 
                             x_column, 
                             colors = c("#fc2fbf", "#00B9E3"),
                             upper = 2, 
                             lower = 0, 
                             bin_width = NULL, 
                             x_label = NULL, 
                             y_label = "Count",
                             file_name = "hist",
                             plot_width = 8,
                             plot_height = 6
) {
  plot <- ggplot(data, aes_string(x = x_column, fill = "room_type")) +
    geom_histogram(binwidth = bin_width, alpha = 0.8, position = "identity", color = "black") +  # 境界線を黒に指定
    facet_wrap(~ room_type, scales = "fixed") +
    labs(
      x = ifelse(is.null(x_label), x_column, x_label),
      y = ifelse(is.null(y_label), "Count", y_label)
    ) +
    coord_cartesian(xlim = c(lower, upper)) + 
    scale_fill_manual(values = colors) +  # fillの色を指定
    theme_economist_white(base_size = 11) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 10), face = "bold"),  # x軸のラベルを太文字
      axis.title.y = element_text(face = "bold"),                           # y軸のラベルを太文字
      strip.text = element_text(face = "bold")                              # facet_wrapのラベルを太文字
    )
  
  output_path <- paste0(here::here("01_analysis", "00_figuretable"), "/", file_name)
  
  ggsave(output_path, plot, width = plot_width, height = plot_height)
}

create_histogram(df_output, "prices", bin_width = 0.1, file_name = "01_Price_Distribution.pdf", x_label = "Prices")
# create_histogram(df_output, "hosting_years", lower = 0, upper = 15)


# city_idsごとに価格の平均を計算
df_avg_prices <- df_output %>%
  group_by(city_ids, room_type) %>%  # city_idsとroom_typeごとにグループ化
  summarise(mean_price = mean(prices, na.rm = TRUE))  # 価格の平均を計算

ggplot(df_avg_prices, aes(x = city_ids, y = mean_price, fill = room_type)) +
  # geom_bar(stat = "identity", color = "black") +
  geom_col(color = "black", width = 1, alpha = 1) +
  facet_wrap(~ room_type, scales = "fixed") +
  labs(
    # title = "Average Prices by City IDs and Room Type",
    x = NULL,
    y = "Average Prices"
  ) +
  theme_economist_white() +
  # scale_fill_manual(values = c("#fc2fbf", "#00B9E3")) +  # fillの色を指定
  # scale_color_manual(values = c("#fc2fbf", "#00B9E3")) + # colorの色を指定
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # x軸のラベルを90度回転
    legend.position = "none",  # 凡例を非表示
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),  # x軸のラベルを太文字
    axis.title.y = element_text(margin = margin(b = 20), face = "bold"),  # y軸のラベルを太文字
    strip.text = element_text(face = "bold")  # facet_wrapのラベルを太文字
  )

ggsave(here::here("01_analysis", "00_figuretable", "02_Ave_Price_By_City.pdf"), width = 8, height = 6)



# monthごとに価格の平均を計算
df_avg_prices_by_month <- df_output %>%
  group_by(month, room_type) %>%  # monthとroom_typeごとにグループ化
  summarise(mean_price = mean(prices, na.rm = TRUE))  # 価格の平均を計算

# 散布図と線グラフを作成
plot_by_month <- ggplot(df_avg_prices_by_month, aes(x = factor(month), y = mean_price, color = room_type, group = room_type)) +
  geom_point(aes(shape = room_type), size = 3, stroke = 1.5, fill = "white", alpha = 0.8) +  # 中を白抜きにするためにfill="white"を指定
  geom_line(size = 1.2) +  # 棒線を追加
  labs(
    title = "Panel (A)",
    x = "Month",
    y = "Average Prices"
  ) +
  scale_shape_manual(values = c(21, 23)) +  # Airbnbは丸 (21), Booking.comはひし形 (23)
  # scale_color_manual(values = c("#fc2fbf", "#00B9E3")) +  # Airbnbは青、Booking.comはオレンジ
  theme_economist_white() +
  theme(
    legend.title = element_blank(),  # 凡例のタイトルを非表示
    legend.position = "bottom",  # 凡例を下側に表示
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),  # x軸のラベルを太文字
    axis.title.y = element_text(margin = margin(b = 20), face = "bold"),   # y軸のラベルを太文字
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(here::here("01_analysis", "00_figuretable", "03_Ave_Price_By_Month.pdf"), width = 8, height = 6)

# day_of_weekごとに価格の平均を計算
df_avg_prices_by_day <- df_output %>%
  group_by(day_of_week, room_type) %>%  # day_of_weekとroom_typeごとにグループ化
  summarise(mean_price = mean(prices, na.rm = TRUE))  # 価格の平均を計算

# 曜日ごとの散布図を作成
plot_by_day_of_week <- ggplot(df_avg_prices_by_day, aes(x = factor(day_of_week), y = mean_price, shape = room_type, color = room_type, group = room_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, stroke = 1.5, fill = "white", alpha = 0.8) +  # 中を白抜きにするためにfill="white"を指定
  labs(
    x = "Day of the Week",
    y = "Average Prices",
    title = "Panel (B)"
  ) +
  scale_shape_manual(values = c(21, 23)) +  # Airbnbは丸 (21), Booking.comはひし形 (23)
  # scale_color_manual(values = c("#fc2fbf", "#00B9E3")) +  # Airbnbは青、Booking.comはオレンジ
  theme_economist_white() +
  theme(
    legend.title = element_blank(),  # 凡例のタイトルを非表示
    legend.position = "bottom",  # 凡例を下側に表示
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),  # x軸のラベルを太文字
    axis.title.y = element_text(margin = margin(b = 20), face = "bold"),   # y軸のラベルを太文字
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(here::here("01_analysis", "00_figuretable", "04_Ave_Price_By_Day_of_Week.pdf"), width = 8, height = 6)

p <- gridExtra::grid.arrange(plot_by_month, plot_by_day_of_week, ncol = 2)
ggsave(
  plot = p,
  here::here("01_analysis", "00_figuretable", "20_Ave_Price_By_Month_and_Day_of_week_merged.pdf"), width = 10, height = 6
  )




df_output |> 
  ggplot(aes(x=shares)) +
  geom_histogram(binwidth = 0.005) +
  facet_wrap(~room_type, scales = "free") 




#######################
# sharesと価格の散布図を出す
#######################
create_scatter_plot <- function(data, x_column, y_column, alpha = 0.8, x_label = NULL, y_label = NULL) {
  ggplot(data, aes_string(x = x_column, y = y_column, color = "room_type")) +
    geom_point(alpha = alpha) +  # 散布図の点をプロット
    facet_wrap(~ room_type, scales = "fixed") +
    geom_smooth(method = "lm", se = FALSE, color = "black") +  # 線形回帰直線を追加
    labs(
      x = ifelse(is.null(x_label), x_column, x_label),
      y = ifelse(is.null(y_label), y_column, y_label)
    ) +
    coord_cartesian(xlim = c(0, 2)) + 
    # scale_color_manual(values = c("#fc2fbf", "#00B9E3")) +  # 色を指定
    theme_economist_white(base_size = 11) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 10), face = "bold"),  # x軸のラベルを太文字
      axis.title.y = element_text(face = "bold"),                           # y軸のラベルを太文字
      strip.text = element_text(face = "bold")                              # facet_wrapのラベルを太文字
    )
}
create_scatter_plot(df_output, "prices", "shares", alpha = 0.5)
