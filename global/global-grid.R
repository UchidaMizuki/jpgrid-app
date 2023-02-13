
# global_grid -------------------------------------------------------------

# data
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

# functions
text_size_grid <- function(grid, zoom) {
  round(2 ^ zoom * jpgrid:::grid_size(grid) * 1.25e-6)
}

str_trunc_grid <- function(grid, text_size) {
  str_trunc(as.character(grid),
            width = pmax(4, text_size,
                         na.rm = TRUE),
            side = "left")
}

color_factor_brewer <- function(domain,
                                pal = "Set2") {
  domain <- vec_unique(domain)
  size <- vec_size(domain)

  palette <- quietly(RColorBrewer::brewer.pal)(size, pal)$result
  palette <- vec_rep(palette, ceiling(size / vec_size(palette))) |>
    vec_slice(seq_len(size))

  colorFactor(palette = palette,
              domain = domain)
}
