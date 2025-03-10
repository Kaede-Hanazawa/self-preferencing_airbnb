source(here::here("preamble.R"))

# Whether to divide market_ids by cluster or not
cluster_mkt_ids <- FALSE

# Whether to exclude Airbnb listings for guests of 3 or more
exclude_guests_3over <- FALSE

# If TRUE, judge whether it's AP only based on whether it's DP or not
only_DP <- FALSE

# Whether to use AP
use_AP <- TRUE

# -----------------------------------------
# -----------------------------------------
# Analyze Airbnb only (exclude Booking.com)
only_Airbnb <- FALSE

# Whether to use the nested logit model
use_nested_logit <- TRUE
# -----------------------------------------
# -----------------------------------------

# Number of clusters
n_clusters <- 5

# Minimum number of clusters to keep in the data
min_cluster <- 1

# If the number of Airbnb clusters is greater than or equal to this, treat as using AP
n_clusters_AP <- 4

# The value to divide prices by
price_divide <- 100000

# Which side to adjust the values to: "hotel" or "airbnb"
adjust_X <- "airbnb"

# Whether to calculate share using the maximum potential market size by market × weekday
use_max_potential_market_size <- FALSE

# Run
main()
