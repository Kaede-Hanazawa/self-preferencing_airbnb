product_data <- readr::read_csv(here::here("01_analysis", "05_output_all", "product_data.csv"))

View(product_data)

product_data <- product_data |>
  mutate(
    diff_price = changed_prices - prices,
    diff_price_ratio = (changed_prices - prices) / prices,
    cs = as.numeric(gsub("\\[|\\]", "", cs)),
    changed_cs = as.numeric(gsub("\\[|\\]", "", changed_cs)),
    diff_cs = changed_cs - cs,
    diff_cs_ratio = (changed_cs - cs) / cs
  )


product_data |> 
  ggplot(aes(x = diff_price)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  theme_economist() +
  labs(
    title = "Distribution of Price Differences",
    x = "Price Difference",
    y = "Frequency"
  )

datasummary_skim(
  product_data,
  fmt = 3
  )