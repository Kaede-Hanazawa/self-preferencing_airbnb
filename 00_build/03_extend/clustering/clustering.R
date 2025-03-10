# k-means法により、airbnbとhotelの物件を5つにクラスタリングする
df <- df_merged

# クラスタリングに使用する列を選択
df_cluster <- df_merged[, c("prices")]

# k-meansクラスタリングの実行（5つのクラスタに分ける）
set.seed(1) # 再現性のために乱数シードを設定

kmeans_result <- kmeans(df_cluster, centers = n_clusters)

# クラスタリング結果を元のデータフレームに追加
df$cluster <- kmeans_result$cluster


# 各クラスターの平均値を計算
cluster_means <- aggregate(prices ~ cluster, data = df, FUN = mean)

# 平均値を降順にソートし、クラスター番号を再割り当て
sorted_clusters <- cluster_means[order(cluster_means$prices, decreasing = TRUE), ]
sorted_clusters$cluster_new <- seq_len(nrow(sorted_clusters))

# 元のデータフレームに新しいクラスターラベルを追加
df <- merge(df, sorted_clusters[, c("cluster", "cluster_new")], by = "cluster", all.x = TRUE)

# クラスターラベルを更新
df$cluster <- df$cluster_new

# 不要な列を削除
df$cluster_new <- NULL
cluster_means <- aggregate(prices ~ cluster, data = df, FUN = mean)
# クラスターごとの平均値を確認
print(cluster_means)

a <- df |> 
  select(prices, cluster, room_type) |> 
  mutate(
    prices = prices / 10000,)

table_cluster_prices <- modelsummary::datasummary_balance(
  ~ cluster,
  data = a,
  notes = "1が最も平均価格の大きいグループ。単位：$1/10000$円",
  dinm = FALSE,
  #output = "latex",
  fmt = 2
)

print(table_cluster_prices)

df_merged <- df


# a <- df_merged |> 
#   filter(room_type == "Airbnb") 
# unique(a$num_of_vacant)  
# [1] NA

