nrow(df_output)
df_output |> 
  filter(room_type == "Airbnb") |> 
  group_by(
    city_ids, day_of_week
  ) |> 
  summarise(
    n = n()
  ) |>
  View()
  

# day_of_weekごとの平均を計算
city_avg <- df_output |>
  filter(room_type == "Airbnb") |>
  group_by(city_ids, day_of_week) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(city_ids) |>
  summarise(avg_n = mean(n), .groups = "drop")

# city_ids を平均が高い順に並べ、5個ずつに分割
sorted_cities <- city_avg |>
  arrange(desc(avg_n)) |>
  pull(city_ids)

# city_ids を5個ずつのリストに分割
city_groups <- split(sorted_cities, ceiling(seq_along(sorted_cities) / 5))

# プロットを作成する関数
create_plot <- function(city_ids_group) {
  df_output |>
    filter(room_type == "Airbnb", city_ids %in% city_ids_group) |>
    group_by(city_ids, day_of_week) |>
    summarise(n = n(), .groups = "drop") |>
    ggplot(aes(x = day_of_week, y = n, color = city_ids, group = city_ids)) +
    geom_line() +
    geom_point() +
    theme_economist() +
    labs(
      title = paste("Airbnb Listings for City IDs:", paste(city_ids_group, collapse = ", ")),
      x = "Day of Week",
      y = "Number of Listings",
      color = "City IDs"
    )
}

# 各グループに対してプロットを作成
plots <- lapply(city_groups, create_plot)

# プロットを grid.arrange で並べて表示
do.call(grid.arrange, c(plots, ncol = 2))

