
# utils-data --------------------------------------------------------------

WGS84 <- 4326
grid_size <- c("80km", "10km", "1km", "500m", "250m", "125m", "100m")

# data_grid_city
data_grid_city <- grid_city |>
  mutate(city_name_ja = str_c(pref_name_ja, city_name_ja,
                              sep = " "))

data_city <- data_grid_city |>
  distinct(pref_code, city_code, pref_name, pref_name_ja, city_name_ja)

data_pref <- data_grid_city |>
  distinct(pref_code, pref_name, pref_name_ja)
