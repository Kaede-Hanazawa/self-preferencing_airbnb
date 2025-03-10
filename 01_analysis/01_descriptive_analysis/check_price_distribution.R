
library(ggplot2)
library(gridExtra)

# Airbnbのヒストグラム
p1 <- ggplot(df_final[df_final$room_type == "Airbnb", ], aes(x = prices)) +
  geom_histogram(bins = 25, fill = "blue", color = "black") +
  labs(title = "Airbnb", x = "Price", y = "Count") +
  theme_minimal() +
  xlim(0, 100000) +
  ylim(0, 20000)

# Booking.comのヒストグラム
p2 <- ggplot(df_final[df_final$room_type == "Booking.com", ], aes(x = prices)) +
  geom_histogram(bins = 25, fill = "red", color = "black") +
  labs(title = "Booking.com", x = "Price", y = "Count") +
  theme_minimal() +
  xlim(0, 100000) +
  ylim(0, 20000)

# 並べて表示
grid.arrange(p1, p2, ncol = 2)


# AirbnbとBooking.comの密度推定
p3 <- ggplot() +
  geom_density(data = df_final[df_final$room_type == "Airbnb", ], aes(x = prices, color = "Airbnb"), fill = "blue", alpha = 0.3) +
  geom_density(data = df_final[df_final$room_type == "Booking.com", ], aes(x = prices, color = "Hotel"), fill = "red", alpha = 0.3) +
  labs(title = "Price Density Estimates", x = "Price", y = "Density") +
  theme_minimal() +
  xlim(0, 100000) +
  scale_color_manual(values = c("Airbnb" = "blue", "Hotel" = "red"))

# グラフ表示
print(p3)








