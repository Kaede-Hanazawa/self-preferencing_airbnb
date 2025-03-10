# ファイルパスのリスト
file_paths <- list(
  here::here("99_data", "Inside_Airbnb", "2024_06_29", "listings.csv"),
  here::here("99_data", "Inside_Airbnb", "2024_03_30", "listings.csv"),
  here::here("99_data", "Inside_Airbnb", "2023_12_27", "listings.csv"),
  here::here("99_data", "Inside_Airbnb", "2023_09_24", "listings.csv"),
  here::here("99_data", "Inside_Airbnb", "2023_06_29", "listings.csv")
)

# データ読み込みと前処理を関数化
process_data <- function(path) {
  readr::read_csv(path) |>
    dplyr::filter(stringr::str_ends(neighbourhood_cleansed, "Ku")) |>
    dplyr::mutate(
      neighbourhood_cleansed = stringr::str_replace(neighbourhood_cleansed, "\\sKu$", ""),
      neighbourhood_cleansed = stringr::str_to_lower(neighbourhood_cleansed),
      price = stringr::str_remove_all(price, "[$,]") |> as.numeric(),
      price = price / 100000
    ) |>
    dplyr::rename(
      wards = neighbourhood_cleansed,
      prices_inside = price
    ) |>
    select(
      name, wards, last_scraped, prices_inside, host_response_time, host_response_rate,
      host_name, host_is_superhost, host_listings_count, host_total_listings_count, 
      host_identity_verified, latitude, longitude, property_type, accommodates, bathrooms,
      bedrooms, beds, number_of_reviews, review_scores_rating, review_scores_accuracy,
      review_scores_cleanliness, review_scores_checkin, review_scores_communication,
      review_scores_location, review_scores_value
    )
}

# 各ファイルに対して処理を適用し、データを結合
combined_df <- lapply(file_paths, process_data) |> dplyr::bind_rows()
df_AIRBNB <- combined_df |> 
  dplyr::distinct() |> 
  select(
    host_name, wards, latitude, longitude
  ) |> 
  rename(
    firm_ids = host_name,
    city_ids = wards
  )


df_merged <- df |> 
  filter(room_type == "Airbnb") |> 
  left_join(df_AIRBNB, by=c("firm_ids", "city_ids")) |> 
  na.omit()

df_Hotel <- df |> 
  filter(room_type == "Booking.com")

df_merged_final <- df_merged |> 
  bind_rows(df_Hotel) 
nrow(df_merged_final)  

df_unique <- 
write_csv(df_unique, here::here("99_data", "df_unique.csv"))


modelsummary::datasummary_skim(
  df_unique,
  fmt = 3)

df |> 
  group_by(wards) |>
  summarise(
    mean_price = mean(prices_inside, na.rm = TRUE),
  ) |> 
  ggplot2::ggplot(aes(x = wards, y = mean_price)) +
  geom_col() 

df |> 
  ggplot(aes(x = last_scraped, y = prices_inside)) +
  geom_point() 

nrow(df)
lm_formula <- as.formula("prices_inside ~ number_of_reviews + review_scores_rating + review_scores_accuracy + review_scores_cleanliness + review_scores_checkin + review_scores_communication + review_scores_location + review_scores_value + host_is_superhost + bathrooms + beds")
lm_model <- list(
  "OLS" = lm(lm_formula, data = df),
  "Fixed Effects" = fixest::feols(
    prices_inside ~ number_of_reviews + review_scores_rating + review_scores_accuracy +
      review_scores_cleanliness + review_scores_checkin + review_scores_communication +
      review_scores_location + review_scores_value + host_is_superhost + bathrooms + beds |
      last_scraped + property_type + accommodates + wards + host_name,
    data = df
  )
)


modelsummary::modelsummary(lm_model, stars = TRUE)


