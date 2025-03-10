df_airbnb <- df |>
  filter(room_type == "Airbnb")

df_hotel <- df |>
  filter(room_type == "Booking.com")

modelsummary::datasummary_skim(
    data = df_airbnb,
    fmt = 3,
    title = "Summary of Airbnb Data"
    )


modelsummary::datasummary_skim(
    data = df_hotel,
    fmt = 4
    )


modelsummary::datasummary_skim(
    data = df_output,
    fmt = 4
    )

df|>
  modelsummary::datasummary_balance(
    ~room_type,
    data = _,
    fmt = 3,
    stars = TRUE)


# df_output |>
df |>
  # filter(cluster == 5) |> 
  mutate(location_score = location) |>
  select(
    prices, num_of_reviews, location_score, staff_communication_score,
    cleanness_score, room_score, num_of_vacant, room_type, num_of_beds,# shares
    ) |>
  rename(
    Prices = prices,
    `# reviews` = num_of_reviews,
    `Rating of Location` = location_score,
    `Rating of Staff Communication` = staff_communication_score,
    `Rating of Cleanness` = cleanness_score,
    `Rating of Room` = room_score,
    `# vacancies` = num_of_vacant,
    `# beds`= num_of_beds,
    # Shares = shares
    ) |>
  modelsummary::datasummary_balance(
    ~room_type,
    data = _,
    fmt = 3,
    stars = c("*" = .1, "**" = .05, "***" = .01), 
    output = "latex",
    # notes = "注釈：*p<0.1; **p<0.05; ***p<0.01",
    #title = "Balance Test Results"
    )

df_output |>colnames()
