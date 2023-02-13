
# global-shiny ------------------------------------------------------------

href_jpgrid <- "https://uchidamizuki.github.io/jpgrid/"

icon_tab_about <- icon("circle-info")
icon_tab_grid_city <- icon("tree-city")
icon_tab_parse_grid <- icon("hashtag")
icon_tab_coords_to_grid <- icon("location-pin")

text_tab_about <- "このアプリについて"
text_tab_grid_city <- "市区町村別のメッシュ生成"
text_tab_parse_grid <- "文字列からメッシュ生成"
text_tab_coords_to_grid <- "緯度経度からメッシュ生成"

style_button <- "simple"

# html
a_blank <- partial(a,
                   target="_blank",
                   rel="noopener noreferrer")

# leaflet
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
