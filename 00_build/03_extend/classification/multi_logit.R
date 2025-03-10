# 多項ロジスティック回帰モデルの学習
multinom_model <- multinom(model_formula, data = train_data)

summary(multinom_model)

# モデルの予測を行う
predictions <- predict(multinom_model, newdata = test_data, type = "class")

# # 実際の値と予測値を比較してRMSEを計算
# # 実際の値と予測値を数値に変換する必要がある場合がある
# actual_values <- as.numeric(test_data$hotel_rank)
# predicted_values <- as.numeric(predictions) # これをAirbnbのhotel_rankとして採用する(?)
# 
# # RMSEを計算
# rmse_value <- rmse(actual_values, predicted_values)
# 
# # 結果を表示
# print(paste("RMSE:", rmse_value))

# 実際の値と予測値を比較してAccuracyを計算
actual_values <- test_data$hotel_rank
accuracy <- sum(predictions == actual_values) / length(actual_values)

# 結果を表示
print(paste("Accuracy:", accuracy))

# 予測値をAirbnbのhotel_rankとして採用
df_airbnb_classification$hotel_rank <- as.numeric(predict(multinom_model, newdata = df_airbnb_classification, type = "class"))


# モデルの予測を行う
predictions <- predict(multinom_model, newdata = test_data, type = "class")



# 予測値をAirbnbのhotel_rankとして採用
df_airbnb_classification$hotel_rank <- as.factor(predict(multinom_model, newdata = df_airbnb_classification, type = "class"))
