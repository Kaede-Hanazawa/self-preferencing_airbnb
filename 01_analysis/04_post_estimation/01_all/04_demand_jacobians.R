demand_jacobians <- read_csv(here::here("01_analysis", "04_post_estimation", "01_all", "demand_jacobians.csv"))

result <- tibble::as_tibble(demand_jacobians) |>
  tidyr::pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  dplyr::select(value)

result |> 
  modelsummary::datasummary_skim(fmt = 3)

result |> 
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.00001) +
  xlim(-0.003, 0.003) 


# positive と negative のカウントをまとめて集計
count_summary <- result |>
  dplyr::summarise(
    positive_count = sum(value > 0, na.rm = TRUE),
    negative_count = sum(value < 0, na.rm = TRUE),
  )

# 結果を表示
print(count_summary)
