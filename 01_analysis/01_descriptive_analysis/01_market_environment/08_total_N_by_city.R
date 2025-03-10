df_total_N <- df |> 
  group_by(city_ids, room_type) |> 
  summarise(count = n(), .groups = "drop")
  

print(df_total_N, n = 46)
