library(shiny)
library(shinydashboard)
library(leaflet)

library(dplyr)
library(fs)
library(purrr)
library(readr)
library(rlang)
library(sf)
library(stringr)
library(tibble)
library(vctrs)

library(stickyr)
library(jpgrid)

# utils -------------------------------------------------------------------

leafletOptions_grid <- partial(leafletOptions,
                               zoomControl = FALSE)

addProviderTiles_grid <- partial(addProviderTiles,
                                 provider = "CartoDB.Positron")

addPolygons_grid <- partial(addPolygons,
                            weight = 2,
                            opacity = 1)

addLegend_grid <- partial(addLegend,
                          position = "bottomright")

# addLabelOnlyMarkers_grid <- partial(addLabelOnlyMarkers,
#                                     lng = ~X,
#                                     lat = ~Y)

labelOptions_grid <- partial(labelOptions,
                             noHide = TRUE,
                             textOnly = TRUE,
                             direction = "center")

# addMarkers_grid <- partial(addMarkers,
#                            lng = ~X,
#                            lat = ~Y)

grid_size_to_km <- function(grid_size) {
  switch (
    grid_size,
    `80km` = 80,
    `10km` = 10,
    `1km` = 1,
    `500m` = 0.5,
    `250m` = 0.25,
    `125m` = 0.125,
    `100m` = 0.1
  )
}

text_size_grid <- function(grid_size, zoom) {
  km <- grid_size_to_km(grid_size)
  round(2 ^ zoom * km * 1.25e-3)
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

zoom_grid <- function(data) {
  bbox <- st_bbox(data)
  slippymath::bbox_to_tile_grid(bbox,
                                max_tiles = 12)$zoom
}

center_grid <- function(data) {
  bbox <- st_bbox(data)
  list(lng = (bbox[["xmin"]] + bbox[["xmax"]]) / 2,
       lat = (bbox[["ymin"]] + bbox[["ymax"]]) / 2)
}
