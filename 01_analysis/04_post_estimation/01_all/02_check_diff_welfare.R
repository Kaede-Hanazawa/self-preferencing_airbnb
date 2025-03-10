source(here::here("01_analysis", "04_post_estimation", "01_all", "00_check_welfare_functions.R"))

main <- function(SP = FALSE){
  result <- make_welfare_result_df_and_plot(SP=SP)
  # 結果を表示
  final_summary <- result[[1]]
  final_descriptive_stats <- result[[2]]
  
  # View(final_summary)
  # View(final_descriptive_stats)
  datasummary(
    All(final_descriptive_stats) ~ model * (Mean + SD),
    data = final_descriptive_stats,
    fmt = 3,
    # output = "latex"
  )
  
  plot_scatter_by_model(final_descriptive_stats, alpha =0.8)
  
  # プロットの幅を指定
  width <- 2
  # 実行
  file_name = if_else(SP, "05_welfare_changes_histograms_SP.pdf", "05_welfare_changes_histograms.pdf")
  plot_welfare_changes(
    final_descriptive_stats, output_file = file_name, 
    binwidth = width, dodge_width = width, x_break_step = 5, alpha = 1,
    legend_text_size = 12,  # 凡例のテキストサイズを指定
    legend_key_size = 0.5,  # 凡例のシンボルサイズを指定
    lower = -25,
    upper = 15
  )
}

cnt <- 1
mc_plot_list <- list()
# 実行
main(SP = TRUE)


## 各プロットのタイトルラベルを薄く、かつ大きさを調整
plot1 <- mc_plot_list[[1]] + theme(axis.title.x = element_blank(), plot.title = element_text(face = "bold", size = 14))
plot2 <- mc_plot_list[[2]] + theme(axis.title.x = element_blank(), plot.title = element_text(face = "bold", size = 14))
plot3 <- mc_plot_list[[3]] + theme(plot.title = element_text(face = "bold", size = 14)) # 下段のプロットはx軸タイトルを残す


# 縦に並べて表示
combined_plot <- plot1 / plot2 / plot3

# プロットの表示
print(combined_plot)
ggsave(here::here("01_analysis", "00_figuretable", "13_MC_hist.pdf"), width = 6, height = 8.5)
