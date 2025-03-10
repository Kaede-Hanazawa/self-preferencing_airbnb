colnames(product_data)
product_data |> 
  group_by(market_ids) |> 
  summarise(
    n = sum(merger_ids == "Airbnb"),
    # merger_ids=="Airbnb"の行について、price_diff_ratioの平均を計算する
    # price_change_mean = mean(price_diff_ratio[merger_ids == "Airbnb"], na.rm = TRUE),
    price_change_mean = mean(price_diff_ratio[room_type == "Booking.com"], na.rm = TRUE),
    # price_change_mean = mean(price_diff_ratio, na.rm = TRUE),
  ) |> 
  ggplot(aes(x = n, y = price_change_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Number of host listings with SP",
    y = "Average % change in price (Only Hotels)",
  ) +
  ggthemes::theme_economist_white() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 12) # 凡例テキストのサイズを小さくする
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, 13, by = 1))



# ggsave(here::here("01_analysis", "00_figuretable", "29_price_and_SP_N.pdf"), width = 7, height = 5)
# ggsave(here::here("01_analysis", "00_figuretable", "30_price_and_SP_N_only_Airbnb.pdf"), width = 7, height = 5)
ggsave(here::here("01_analysis", "00_figuretable", "31_price_and_SP_N_only_Hotels.pdf"), width = 7, height = 5)

a <- product_data |> 
     group_by(market_ids) |> 
     summarise(
         n=sum(merger_ids=="Airbnb")
       ) 

modelsummary::datasummary(
  data = a,
  formula = n ~ Mean + SD + Min + Max + Median + N,
  fmt = 3
)






product_data |> 
  group_by(market_ids) |> 
  summarise(
    n = sum(merger_ids == "Airbnb"),
    # merger_ids=="Airbnb"の行について、price_diff_ratioの平均を計算する
    # price_change_mean = mean(price_diff_ratio[merger_ids == "Airbnb"], na.rm = TRUE),
    # price_change_mean = mean(price_diff_ratio[room_type == "Booking.com"], na.rm = TRUE),
    # price_change_mean = mean(price_diff_ratio, na.rm = TRUE),
    mean_cs_diff_ratio = cs_diff_ratio,
  ) |> 
  ggplot(aes(x = n, y = mean_cs_diff_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Number of host listings with SP",
    y = "Average % change in Consuemr Surplus",
  ) +
  ggthemes::theme_economist_white() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 12) # 凡例テキストのサイズを小さくする
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, 13, by = 1))













