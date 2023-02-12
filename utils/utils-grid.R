
# utils-grid --------------------------------------------------------------

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
