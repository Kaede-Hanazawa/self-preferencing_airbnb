# -----------------------------------
# df_outputにshares列を追加する
# ----------------------------------

if (!use_max_potential_market_size){
  potential_market_size_base <- 5
  if(cluster_mkt_ids){ # market_idsを、クラスターごとに分ける場合
    MAX_N <- df_output |> 
      group_by(market_ids, cluster) |> 
      summarise(
        # potential_market_size = n() * potential_market_size_base
        potential_market_size = sum(num_of_vacant, na.rm = TRUE) * potential_market_size_base
      )

    df_output <- df_output |>
      left_join(MAX_N, by = c("market_ids", "cluster")) |>
      mutate(
        shares = num_of_vacant / potential_market_size
      )
  } else { # market_idsを、クラスターごとに分けない場合
    MAX_N <- df_output |> 
      group_by(market_ids) |> 
      summarise(
        # potential_market_size = n() * potential_market_size_base
        potential_market_size = sum(num_of_vacant, na.rm = TRUE) * potential_market_size_base
      )
    
    df_output <- df_output |>
      left_join(MAX_N, by = c("market_ids")) |>
      mutate(
        shares = num_of_vacant / potential_market_size
      )
  }
} else { # use_max_potential_market_size = TRUEの場合
  potential_market_size_base <- 5
  if(cluster_mkt_ids){ # market_idsを、クラスターごとに分ける場合
    MAX_N <- df_output |> 
      group_by(city_ids, cluster, day_of_week) |> 
      summarise(
        potential_market_size = sum(num_of_vacant, na.rm = TRUE) * potential_market_size_base
      ) |> 
      mutate(
        max_potential_market_size = max(potential_market_size)
        ) |> 
      filter(potential_market_size == max_potential_market_size) |> 
      ungroup() |>
      select(-potential_market_size)
    
    df_output <- df_output |>
      left_join(MAX_N, by = c("city_ids", "cluster", "day_of_week")) |>
      mutate(
        shares = num_of_vacant / max_potential_market_size
      )

  } else { # market_idsを、クラスターごとに分けない場合
    MAX_N <- df_output |> 
      group_by(city_ids, day_of_week) |> 
      summarise(
        # potential_market_size = n() * potential_market_size_base
        potential_market_size = sum(num_of_vacant, na.rm = TRUE) * potential_market_size_base
      ) |>
      mutate(
        max_potential_market_size = max(potential_market_size)
        ) |> 
      filter(potential_market_size == max_potential_market_size) |> 
      ungroup() |>
      select(-potential_market_size)
    
    df_output <- df_output |>
      left_join(MAX_N, by = c("city_ids", "day_of_week")) |>
      mutate(
        shares = num_of_vacant / max_potential_market_size
      )
  }
}
