# 残したい変数名
keep_vars <- c("df_airbnb_raw", "df_booking_raw")

# すべての変数を取得し、残したい変数以外を削除
rm(list = ls()[!ls() %in% keep_vars])

#rm(list = ls())
main <- function(){
  # Load packages
  pacman::p_load(
    tidyverse, dplyr, scales, caTools, nnet, Metrics, knitr, kableExtra, caret, class, e1071, kernlab,
    purrr, readr, ggthemes, stringdist, ggforce, patchwork, lubridate, ggtext, sf
  )
  
  # Load the data
  source(here::here("00_build", "01_load", "load_df.R"))
  
  # create the variable "num_of_beds"
  source(here::here("00_build", "03_extend", "mutate_num_of_beds.R"))
  
  # Clean the data
  source(here::here("00_build", "02_clean", "clean_df.R"))
  
  # Make "Hotel Rank" for Airbnb
  # source(here::here("00_build", "03_extend", "classification", "classification.R"))
  
  # Clustering
  source(here::here("00_build", "03_extend", "clustering", "clustering.R"))
  
  # Check whether hosts use dynamic pricing strategy.
  source(here::here("00_build", "03_extend", "dynamic_pricing", "DP_new.R"))
  
  # Determine whether hosts use AP.
  source(here::here("00_build", "03_extend", "determine_whether_AP.R"))
  
  # Make IVs
  # source(here::here("00_build", "03_extend", "IVs", "blp_iv.R"))
  
  # Tidy the data for PyBLP
  source(here::here("00_build", "03_extend", "tidy_for_PyBLP.R"))
  

  # Modify the number of vacant rooms for Airbnb
  source(here::here("00_build", "03_extend", "modify_airbnb_n_vacant.R"))
  
  # Make market share
  source(here::here("00_build", "03_extend", "market_share.R"))
  
  # Adjust the data
  source(here::here("00_build", "03_extend", "final_adjust_df.R"))
  
  # Make Test Data for PyBLP
  # source(here::here("00_build", "03_extend", "make_test_df.R"))
}
