source("stat_binscatter.R")


# 棒グラフの作成
df |>
  ggplot(aes(x = city_ids, y = mean_diff_cs_ratio)) +
  geom_bar(stat = "identity", fill = "#00B9E3") +
  labs(
    x = NULL,
    y = "Mean of Diff CS Ratio",
    title = "Average Diff CS Ratio by City"
  ) +
  theme_economist_white() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )




df |>
  ggplot(aes(x = city_ids, y = mean_diff_cs_ratio)) +
  geom_bar(stat = "identity", fill = "#00B9E3") +
  geom_text(
    aes(label = mean_SP_N_ratio),
    vjust = 1,  # 棒グラフの上に表示するために位置を調整
    color = "red",
    fontface = "bold"
  ) +
  labs(
    x = NULL,
    y = "Mean of Diff CS Ratio",
    title = "Average Diff CS Ratio by City"
  ) +
  theme_economist_white() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )


# firm_dataが必要
# firm_dataは、03_00_RCL_only.Rで作成されている
new_df <- firm_data |> 
  left_join(summary_df, by = "market_ids") |> 
  mutate(
    diff_cs_ratio = case_when(
      diff_prices_ratio == 0 ~ 0,
      TRUE ~ diff_cs_ratio
    )
  )

make_SP_N_ratio_and_Changed_in_CS_scatter_plot <- function(df){
  p <- df |> 
    ggplot(aes(x = SP_N_ratio, y = diff_cs_ratio)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "SP N Ratio (%)",
      y = "Change in CS (%)"
    ) +
    # stat_binscatter(color = "red", geom = "pointrange") +
    # stat_binscatter(bins = 10, geom = "line") +
    theme_economist_white() +
    theme(
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 14)
    )
  ggsave(filename = here::here("01_analysis", "00_figuretable", "16_SP_N_and_Diff_CS_ratio_scatter_plot.pdf"), width = 7, height = 5, plot = p)
  
  print(p)
}
make_SP_N_ratio_and_Changed_in_CS_scatter_plot(new_df)



make_SP_N_ratio_and_Changed_in_Prices_scatter_plot <- function(df) {
  # Panel A
  p1 <- df |>
    ggplot(aes(x = SP_N_ratio, y = diff_prices_ratio)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Panel A",
      x = "SP N ratio (%)",
      y = "Change in Prices (%)"
    ) +
    theme_economist_white() +
    theme(
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  
  # Panel B
  p2 <- df |>
    ggplot(aes(x = SP_N_ratio, y = diff_prices_ratio)) +
    stat_binscatter(color = "red", geom = "point") +
    stat_binscatter(bins = 20, geom = "line") +
    labs(
      title = "Panel B",
      x = "SP N ratio (%)",
      y = "Change in Prices (%)"
    ) +
    theme_economist_white() +
    theme(
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  
  # 両方のプロットを並べて表示
  p <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  # PDF ファイルに保存
  ggsave(
    filename = here::here("01_analysis", "00_figuretable", "17_Change_in_Prices_and_SP_N_ratio_scatter_plot.pdf"),
    width = 8, height = 5, plot = p
  )
  
  # プロットの表示
  print(p)
}


make_SP_N_ratio_and_Changed_in_Prices_scatter_plot(new_df)




new_df |> 
  ggplot(aes(x = diff_prices_ratio, y = diff_cs_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Change in Prices (%)",
    y = "Change in CS (%)"
  ) +
  theme_economist_white() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 14)
  )

new_df |> 
  select(market_ids, firm_ids, merger_ids, diff_cs_ratio, diff_prices_ratio) |> 
  View()


new_df |> 
  ggplot2::ggplot(ggplot2::aes(x = diff_prices_ratio)) +
  geom_histogram(fill = "skyblue", color = "black", binwidth = 50)+
  theme_economist_white() 


new_df |> 
  filter(SP_N_ratio == 0 & diff_cs_ratio < 0) |> 
  select(
    market_ids, firm_ids, merger_ids, shares, changed_shares, SP_N_ratio, diff_cs_ratio,
    prices, changed_prices
  ) |> 
  View()


new_df |> 
  filter(SP_N_ratio > 0 & diff_cs_ratio == 0) |> 
  select(
    market_ids, firm_ids, merger_ids, shares, changed_shares, SP_N_ratio, diff_cs_ratio,
    prices, changed_prices, cs, changed_cs
  ) |> 
  View()























