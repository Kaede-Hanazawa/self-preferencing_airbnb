df |> 
  ggplot(aes(x = prices, fill = factor(cluster))) +
  geom_histogram(alpha = 1, binwidth = 0.005) + # 色分けと透明度の設定
  labs(x = "Prices", y = "Count", fill = "Price Cluster") +
  theme_economist_white() +
  scale_x_continuous(limits = c(0, 2)) + # x軸の表示範囲を0から2に指定
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)), # x軸ラベルを太字にし、メモリから離す
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)), # y軸ラベルを太字にし、メモリから離す
    legend.position = "bottom" # 凡例を右に配置
  )

###########################
# clusterごとに集計
# 有効数字3桁でフォーマット
###########################
df_summary <- df_output |>
  group_by(cluster) |>
  summarise(
    N = n(),                             # clusterごとの観測数
    `# Airbnb` = sum(room_type == "Airbnb"),   # Airbnbの観測数
    `# Booking.com` = sum(room_type == "Booking.com"), # Booking.comの観測数
    Mean = mean(prices, na.rm = TRUE),           # pricesの平均
    `Std. dev.` = sd(prices, na.rm = TRUE),      # pricesの標準偏差
    Min = min(prices, na.rm = TRUE),             # pricesの最小値
    Max = max(prices, na.rm = TRUE)              # pricesの最大値
  ) |>
  mutate(
    `Airbnb (%)` = (`# Airbnb` / `N`) * 100,        # Airbnbの割合
    `Hotels (%)` = (`# Booking.com` / `N`) * 100, # Booking.comの割合
    cluster = as.character(cluster), # cluster列をcharacterに変換
    across(c(Mean, `Std. dev.`, Min, Max), ~ number(., accuracy = 0.001)), # 数値列を3桁フォーマット
    across(c(`Airbnb (%)`, `Hotels (%)`), ~ number(., accuracy = 0.001)) # 割合列を小数点以下2桁フォーマット
  ) |>
  as_tibble() |> 
  select(
    -`# Airbnb`, -`# Booking.com`, # 不要な列を削除
  ) |> 
  select(
    cluster, N, `Airbnb (%)`, `Hotels (%)`, Mean, `Std. dev.`, Min, Max # 列の順番を整理
  )

# 出力
df_summary
df_summary |>
  kable(format = "latex", booktabs = TRUE, caption = "価格によるクラスタリング結果") |>
  print()



###########################
# clusterごとにAPがどれだけ使われているか
###########################
df_output |> 
  filter(room_type == "Airbnb") |> 
  group_by(cluster) |> 
  summarise(
    `# AP` = sum(AP == 1), # APが使われている数
    `# Non-AP` = sum(AP == 0), # APが使われていない数
    `AP (%)` = (`# AP` / n()) * 100, # APの割合
    `Non-AP (%)` = (`# Non-AP` / n()) * 100, # Non-APの割合
    # 有効数字3桁でフォーマット
    across(c(`AP (%)`, `Non-AP (%)`), ~ number(., accuracy = 0.001))
  ) |> 
  kable(format = "latex", booktabs = TRUE, caption = "SP ratio") |>
  print()
