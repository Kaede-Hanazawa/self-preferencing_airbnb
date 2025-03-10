df_AIRBNB <- df |> 
  filter(room_type == "Airbnb") |> 
  filter(clustering_ids == 5)

lm_model <- list(
  "lm" = lm(prices ~ hosting_years + num_of_reviews + num_of_vacant + location + cleanness_score + room_score + staff_communication_score, data = df_AIRBNB),
  "Hosting_years on X" = lm(hosting_years ~ num_of_reviews + num_of_vacant + location + cleanness_score + room_score + staff_communication_score, data = df_AIRBNB)
)

modelsummary::modelsummary(
  lm_model,
  stars = TRUE,
)

# hosting_yearsのヒストグラム
df_AIRBNB |> 
  ggplot(aes(x = hosting_years)) +
  geom_histogram(fill = "skyblue", binwidth = 0.5) +
  theme_economist_white() +
  labs(
    title = "hosting_years",
    x = "hosting_years",
    y = "Frequency"
  )

# pricesとhosting_yearsの散布図にlm直線を引く
df_AIRBNB |> 
  ggplot(aes(x = hosting_years, y = num_of_vacant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_economist_white() +
  labs(
    title = "hosting_years vs prices",
    x = "hosting_years",
    y = "prices"
  )


df |> 
  group_by(market_ids) |> 
  summarise(n = n()) |> 
  View()
