
# utils-leaflet -----------------------------------------------------------

leafletOptions_grid <- partial(leafletOptions,
                               zoomControl = FALSE)

addProviderTiles_grid <- partial(addProviderTiles,
                                 provider = "CartoDB.Positron")

addPolygons_grid <- partial(addPolygons,
                            weight = 2,
                            opacity = 1)

addLegend_grid <- partial(addLegend,
                          position = "bottomright")

labelOptions_grid <- partial(labelOptions,
                             noHide = TRUE,
                             textOnly = TRUE,
                             direction = "center")

addLabelOnlyMarkers_grid <- function(map, grid, zoom, ...) {
  text_size <- text_size_grid(grid, zoom)
  map |>
    addLabelOnlyMarkers(lng = ~X,
                        lat = ~Y,
                        label = ~str_trunc_grid(grid,
                                                text_size = text_size),
                        labelOptions = labelOptions_grid(textsize = str_c(text_size, "px"),
                                                         style = list(color = "dimgray")))
}
