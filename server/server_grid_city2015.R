
# server_grid_city2015 ----------------------------------------------------

server_grid_city2015 <- function(input, output, session) {
  output$select_grid_city2015_city_code <- renderUI({
    pref_code <- input$select_grid_city2015_pref_code

    data_grid_city2015_city <- vec_slice(data_grid_city2015_city,
                                         data_grid_city2015_city$pref_code %in% pref_code)

    city_code <- data_grid_city2015_city$city_code %||% character()
    shinyWidgets::pickerInput("select_grid_city2015_city_code",
                              "市区町村",
                              choices = city_code |>
                                set_names(data_grid_city2015_city$city_name_ja),
                              multiple = TRUE)
  }) |>
    bindCache(input$select_grid_city2015_pref_code) |>
    bindEvent(input$select_grid_city2015_pref_code)

  reactive_data_grid_city2015 <- reactive({
    city_code <- input$select_grid_city2015_city_code
    grid_size <- input$select_grid_city2015_grid_size

    if (is.null(city_code)) {
      NULL
    } else {
      vec_slice(data_grid_city2015,
                data_grid_city2015$city_code %in% city_code) |>
        as_tbl_grid(size = grid_size) |>
        distinct(pref_code, city_code, pref_name, pref_name_ja, city_name_ja, grid) |>
        st_as_sf(crs = WGS84) |>
        mutate(grid |>
                 grid_to_XY())
    }
  }) |>
    bindCache(input$select_grid_city2015_city_code, input$select_grid_city2015_grid_size) |>
    bindEvent(input$display_grid_city2015)

  output$leaflet_grid_city2015 <- renderLeaflet({
    city_code <- input$select_grid_city2015_city_code
    data_grid_city2015 <- reactive_data_grid_city2015()

    if (!is.null(city_code)) {
      pal <- color_factor_brewer(data_grid_city2015$city_name_ja)
      leaflet(data_grid_city2015,
              options = leafletOptions_grid()) |>
        addProviderTiles_grid() |>
        addPolygons_grid(color = ~pal(city_name_ja)) |>
        addLegend_grid(pal = pal,
                       title = "市区町村名",
                       values = ~city_name_ja)
    }
  }) |>
    bindCache(input$select_grid_city2015_city_code, input$select_grid_city2015_grid_size) |>
    bindEvent(input$display_grid_city2015)

  observe({
    zoom <- input$leaflet_grid_city2015_zoom

    if (!is.null(zoom)) {
      data_grid_city2015 <- reactive_data_grid_city2015()
      grid_size <- input$select_grid_city2015_grid_size

      # FIXME
      # pal <- color_factor_brewer(data_grid_city2015$city_name_ja)

      text_size <- text_size_grid(grid_size, zoom)
      leafletProxy("leaflet_grid_city2015",
                   session = session,
                   data = data_grid_city2015) |>
        clearMarkers() |>
        addLabelOnlyMarkers(lng = ~X,
                            lat = ~Y,
                            label = ~str_trunc_grid(grid,
                                                    text_size = text_size),
                            labelOptions = labelOptions_grid(textsize = str_c(text_size, "px"),

                                                             # FIXME: Bug where style cannot be set?
                                                             # style = list(color = pal(data_grid_city2015$city_name_ja))
                                                             style = list(color = "dimgray")))
    }
  }) |>
    bindEvent(input$display_grid_city2015, input$leaflet_grid_city2015_zoom)

  filename_grid_city2015 <- function(input, ext) {
    city_code <- input$select_grid_city2015_city_code
    grid_size <- input$select_grid_city2015_grid_size

    city_code <- str_c(city_code,
                       collapse = "_")
    path(str_c("grid_city2015", grid_size, city_code,
               sep = "-"),
         ext = ext)
  }

  output$download_grid_city2015 <- downloadHandler(
    filename = \() {
      ext <- input$select_grid_city2015_ext

      filename_grid_city2015(input, ext)
    },
    content = \(file) {
      data_grid_city2015 <- reactive_data_grid_city2015()
      city_code <- input$select_grid_city2015_city_code
      ext <- input$select_grid_city2015_ext

      if (!is.null(city_code)) {
        data_grid_city2015 <- data_grid_city2015 |>
          mutate(grid = as.character(grid)) |>
          select(pref_code, city_code, pref_name, pref_name_ja, city_name_ja, grid)

        if (ext == "csv") {
          data_grid_city2015 <- as_tibble(data_grid_city2015)
          write_excel_csv(data_grid_city2015, file)
        } else if (ext == "gpkg") {
          layer <- filename_grid_city2015(input, "")

          write_sf(data_grid_city2015, file,
                   layer = layer)
        }
      }
    }
  )
}
