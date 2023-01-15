
# data_grid_city2015 ------------------------------------------------------

data_grid_city2015 <- grid_city2015 |>
  mutate(city_name_ja = str_c(pref_name_ja, city_name_ja,
                              sep = " "))

data_grid_city2015_city <- data_grid_city2015 |>
  as_tibble() |>
  distinct(pref_code, city_code, pref_name, pref_name_ja, city_name_ja)

data_grid_city2015_pref <- data_grid_city2015 |>
  as_tibble() |>
  distinct(pref_code, pref_name, pref_name_ja)
