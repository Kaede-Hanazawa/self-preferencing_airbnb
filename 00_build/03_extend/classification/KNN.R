# 特徴量とターゲット変数を分離
features <- df_for_classification[, c("prices", "staff_communication_score", "cleanness_score", "location", "num_of_reviews")]
target <- df_for_classification$hotel_rank

# データの正規化
preprocess <- preProcess(features, method = c("center", "scale"))
features_normalized <- predict(preprocess, features)


# 特徴量とターゲット変数を分離
train_features <- train_data[, c("prices", "staff_communication_score", "cleanness_score", "location", "num_of_reviews")]
train_target <- train_data$hotel_rank

test_features <- test_data[, c("prices", "staff_communication_score", "cleanness_score", "location", "num_of_reviews")]
test_target <- test_data$hotel_rank


# データの正規化
preprocess <- preProcess(train_features, method = c("center", "scale"))
train_features_normalized <- predict(preprocess, train_features)
test_features_normalized <- predict(preprocess, test_features)

# kの候補値を設定
k_values <- 1:10  # 1から10までのkの値を試す

# クロスバリデーションの設定
control <- trainControl(method = "cv", number = 5)  # 5-fold クロスバリデーション

# グリッドサーチでkの最適値を見つける
knn_grid <- expand.grid(k = k_values)


knn_model <- train(x = train_features_normalized, 
                   y = train_target,
                   method = "knn",
                   tuneGrid = knn_grid,
                   trControl = control,
                   metric = "Accuracy")

# 最適なkの値とそのAccuracyを表示
print(knn_model$bestTune)
print(max(knn_model$results$Accuracy))

# 最適なkを使用してモデルを再学習
best_k <- knn_model$bestTune$k

# テストデータでの予測
final_predictions <- knn(train = train_features_normalized,
                         test = test_features_normalized,
                         cl = train_target,
                         k = best_k)

# Accuracyの計算
final_accuracy <- sum(final_predictions == test_target) / length(test_target)
print(paste("Final Accuracy on test data:", final_accuracy))

####################################
# Airbnbデータに対する予測
####################################

# df_airbnb_classificationの必要な特徴量を抽出
airbnb_features <- df_airbnb_classification[, c("prices", "staff_communication_score", "cleanness_score", "location", "num_of_reviews")]

# df_airbnb_classificationのデータを正規化
airbnb_features_normalized <- predict(preprocess, airbnb_features)

# 最適なkを使用してdf_airbnb_classificationに対する予測を行う
airbnb_predictions <- knn(train = train_features_normalized,
                          test = airbnb_features_normalized,
                          cl = train_target,
                          k = best_k)

# 予測結果をdf_airbnb_classificationに追加
df_airbnb_classification$predicted_hotel_rank <- airbnb_predictions

# 結果の確認
print(head(df_airbnb_classification[, c("predicted_hotel_rank")]))

