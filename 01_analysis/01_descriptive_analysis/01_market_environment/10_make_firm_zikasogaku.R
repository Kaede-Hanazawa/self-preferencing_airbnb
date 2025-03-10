# Data frame creation with 'platform' column
data <- tibble(
  Firm = c("Apple", "NVIDIA", "Microsoft", "Amazon", "Meta Platforms", "Alphabet A", "Alphabet C", "Tesla", "Broadcom", "Eli Lilly"),
  Market_Cap_Million_Dollars = c(3389154.1787, 3321362.0000, 3051052.0240, 2077387.5528, 1239090.7834, 1000847.4700, 955445.1000, 799240.6539, 788953.7119, 777423.1013),
  platform = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # TRUE for specified firms
)

# Bar chart with color based on 'platform' column
ggplot(data, aes(x = reorder(Firm, -Market_Cap_Million_Dollars), y = Market_Cap_Million_Dollars, fill = platform)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray")) +
  labs(title = NULL, x = NULL, y = "Aggregate market value (Million Dollars)") +
  theme_economist_white() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold")
  )

ggsave(here::here("01_analysis", "00_figuretable", "100_firm_zikasogaku.pdf"), width = 7, height = 4)
