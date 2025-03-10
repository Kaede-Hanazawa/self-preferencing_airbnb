# 1. market_idsごとにfirm_idsとmerger_idsのユニークな値の数をカウントし、diff_nを計算
df_summary <- df |>
  group_by(market_ids, city_ids) |>  # city_idsもgroup_byに追加
  summarize(
    unique_firm_ids = n_distinct(firm_ids),
    unique_merger_ids = n_distinct(merger_ids),
    n = n()
  ) |>
  ungroup() |>
  mutate(diff_n = unique_merger_ids - unique_firm_ids)


# 2. city_idsごとにdiff_nの平均を計算
df_city_summary <- df_summary |>
  group_by(city_ids) |>
  summarize(avg_diff_n = mean(diff_n, na.rm = TRUE))

# 3. diff_nのcity_idsごとの平均値のヒストグラムを作成し、先端にポイントを追加
ggplot(df_city_summary, aes(x = city_ids, y = avg_diff_n)) +
  geom_col(fill = "skyblue", color = "black") +
  geom_point(aes(x = city_ids, y = avg_diff_n), color = "black", size = 2, shape = 21, fill = "red") +
  labs(x = NULL, y = "Average differences in N") +
  theme_economist_white() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)) # y軸ラベルを太字にし、メモリから離す
  )

ggsave(here::here("01_analysis", "00_figuretable", "08_Firm_N_diff.pdf"), width = 13, height = 6)



df_summary |> 
  ggplot(aes(x = diff_n)) +
  geom_histogram(fill = "skyblue", color = "black", binwidth = 1) +
  geom_vline(xintercept = mean(df_summary$diff_n), color = "red", linetype = "solid", size = 2) +
  labs(x = NULL, y = "Frequency") +
  theme_economist_white() +
  scale_x_continuous(breaks = seq(min(df_summary$diff_n), max(df_summary$diff_n), by = 1)) +# x軸のラベルを1刻みに設定
  theme(
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)) # y軸ラベルを太字にし、メモリから離す
  )

ggsave(here::here("01_analysis", "00_figuretable", "09_Firm_N_diff_hist.pdf"), width = 6, height = 3.5)
  


df_summary |> 
  ggplot(aes(x = n)) +
  geom_histogram(fill = "skyblue", color = "black", binwidth = 1) +
  geom_vline(xintercept = mean(df_summary$n), color = "red", linetype = "dashed", size = 1) +
  labs(x = NULL, y = "Frequency") +
  theme_economist_white() +
  #scale_x_continuous(breaks = seq(min(df_summary$diff_n), max(df_summary$diff_n), by = 1)) +# x軸のラベルを1刻みに設定
  theme(
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)) # y軸ラベルを太字にし、メモリから離す
  )

ggsave(here::here("01_analysis", "00_figuretable", "11_Firm_N_by_market_hist.pdf"), width = 6, height = 3.5)

# 結果を表示
View(combined_counts)


df_filtered <- df |>
  filter(firm_ids != merger_ids) |>
  select(firm_ids, merger_ids, room_type, market_ids, cluster, prices)
View(df_filtered)
