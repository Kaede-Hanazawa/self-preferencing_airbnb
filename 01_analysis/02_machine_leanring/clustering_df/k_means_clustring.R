library(tidyverse)

colnames(df_airbnb)
colnames(df_booking)

# pricesと、num_of_reviewでk-meansクラスタリングをする
df_airbnb <- df_airbnb |> 
  #rename(num_of_review = ホストのレビュー件数) |> 
  select(prices, num_of_review) |> 
  mutate(
    is_airbnb = TRUE,
    num_of_review = if_else(is.na(num_of_review), 0, num_of_review)
  )

df_booking <- df_booking |>
  rename(num_of_review = 口コミ件数) |> 
  select(prices, num_of_review) |> 
  mutate(
    is_airbnb = FALSE,
    num_of_review = if_else(is.na(num_of_review), 0, num_of_review)
  )

df <- rbind(df_airbnb, df_booking)
df <- df |> 
  filter(prices < 600000 & prices != 0)
modelsummary::datasummary_skim(df)

# dfのpricesのヒストグラム
df |> ggplot(aes(x = prices, fill = is_airbnb)) +
  geom_histogram(bins = 500, alpha = 0.5) +
  theme_minimal()



# pricesとnum_of_reviewを基準に、k-means
set.seed(123)
kmeans_res <- kmeans(df |> select(prices, num_of_review), centers = 5)
df <- df |> 
  mutate(cluster = kmeans_res$cluster)

# dfのclusterのヒストグラム
df |> ggplot(aes(x = cluster, fill = is_airbnb)) +
  geom_histogram(bins = 500, alpha = 0.5) +
  theme_minimal()

# cluster == 3のデータにおいて、is_airbnbの割合を確認
df |> 
  filter(cluster == 3) |> 
  group_by(is_airbnb) |> 
  summarise(n = n()) |> 
  mutate(ratio = n / sum(n))


