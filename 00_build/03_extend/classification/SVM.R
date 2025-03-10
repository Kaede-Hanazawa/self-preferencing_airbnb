library(e1071)
library(caret)

# 特徴量とターゲット変数を分離
train_features <- train_data[, c("prices", "staff_communication_score", "cleanness_score", "location", "num_of_reviews")]
train_target <- train_data$hotel_rank

test_features <- test_data[, c("prices", "staff_communication_score", "cleanness_score", "location", "num_of_reviews")]
test_target <- test_data$hotel_rank

# データの正規化
preprocess <- preProcess(train_features, method = c("center", "scale"))
train_features_normalized <- predict(preprocess, train_features)
test_features_normalized <- predict(preprocess, test_features)

# クロスバリデーションの設定
ctrl <- trainControl(method = "cv", number = 5)

# SVMのハイパーパラメータのグリッド
svm_grid <- expand.grid(
  C = c(0.1, 1, 10, 100),
  sigma = c(0.01, 0.1, 1, 10)
)

# クロスバリデーションを使用してSVMモデルを学習
svm_model <- train(
  x = train_features_normalized,
  y = train_target,
  method = "svmRadial",
  trControl = ctrl,
  tuneGrid = svm_grid,
  metric = "Accuracy"
)

# 最適なハイパーパラメータとAccuracyを表示
print(svm_model$bestTune)
print(max(svm_model$results$Accuracy))

# テストデータでの予測
test_pred <- predict(svm_model, newdata = test_features_normalized)

# テストデータでのAccuracyを計算
test_accuracy <- mean(test_pred == test_target)
print(paste("Test Accuracy:", test_accuracy))

# df_airbnb_classificationの特徴量を抽出と正規化
airbnb_features <- df_airbnb_classification[, c("prices", "staff_communication_score", "cleanness_score", "location", "num_of_reviews")]
airbnb_features_normalized <- predict(preprocess, airbnb_features)

# df_airbnb_classificationに対する予測
# 予測結果をdf_airbnb_classificationに追加
df_airbnb_classification$hotel_rank <- predict(svm_model, newdata = airbnb_features_normalized)

# 結果の確認
print(head(df_airbnb_classification[, c("hotel_rank")]))