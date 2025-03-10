##################
# 目的:
# Hotel dataから、Hotel Rankを教師データとして、分類アルゴリズムを学習。
# Airbnbに適用する。
##################

##################
# アルゴリズムの学習に使用する変数
    # prices
    # room_score = トータルスコア,
    # staff_communication_score = スタッフ,
    # cleanliness_score = 清潔さ,
    # location_score = ロケーション,
    # num_of_reviews = 口コミ件数
##################

# 使用するデータ: df_merged

##################
# 使用する分類アルゴリズム
    # ニューラルネットワーク
    # 多項ロジスティック回帰
    # ランダムフォレスト
##################

# Hotelデータを取得
df_for_classification <- df_merged |>
    dplyr::select(
        prices,
        room_score,
        staff_communication_score,
        cleanness_score,
        location,
        num_of_reviews,
        hotel_rank,
        room_type # Airbnb or Booking.com
    ) |>
    dplyr::filter(room_type == "Booking.com") |>
    na.omit()

# Airbnbデータを取得
df_airbnb_classification <- df_merged |>
    dplyr::select(
        物件名1,
        prices,
        room_score,
        staff_communication_score,
        cleanness_score,
        location,
        num_of_reviews,
        #hotel_rank,
        room_type # Airbnb or Booking.com
    ) |>
    dplyr::filter(room_type == "Airbnb") |>
    na.omit()

set.seed(1)  # 再現性のためにシードを設定


model_formula <- as.formula(hotel_rank ~ prices + staff_communication_score + cleanness_score + location + num_of_reviews)

# Hotel Dataを学習データとテストデータに分割
split <- caTools::sample.split(df_for_classification$hotel_rank, SplitRatio = 0.8)

# トレーニングデータとテストデータを分割
train_data <- subset(df_for_classification, split == TRUE)
test_data <- subset(df_for_classification, split == FALSE)



# 多項ロジットモデルによる予測
source(here::here("00_build", "03_extend", "classification", "multi_logit.R"))

# k近傍法による予測
# source(here::here("00_build", "03_extend", "classification", "KNN.R"))

# df_mergedにhotel_rankをくっつける。
df_airbnb_classification <- df_airbnb_classification |> 
  select(物件名1, hotel_rank) |> 
  distinct(物件名1, .keep_all = TRUE)

df_merged <- df_merged |> 
  left_join(df_airbnb_classification, by = "物件名1")




