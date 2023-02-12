
# server-grid_city ----------------------------------------------------

server_grid_city <- function(input, output, session) {
  output$select_grid_city_city_code <- renderUI({
    pref_code <- input$select_grid_city_pref_code

    data_city <- vec_slice(data_city,
                           data_city$pref_code %in% pref_code)

    city_code <- data_city$city_code %||% character()
    shinyWidgets::pickerInput("select_grid_city_city_code",
                              "市区町村",
                              choices = city_code |>
                                set_names(data_city$city_name_ja),
                              multiple = TRUE)
  }) |>
    bindCache(input$select_grid_city_pref_code) |>
    bindEvent(input$select_grid_city_pref_code)

  reactive_data_grid_city <- reactive({
    city_code <- input$select_grid_city_city_code
    grid_size <- input$select_grid_city_grid_size

    if (is.null(city_code)) {
      NULL
    } else {
      vec_slice(data_grid_city,
                data_grid_city$city_code %in% city_code) |>
        mutate(grid = grid |>
                 grid_convert(grid_size = grid_size)) |>
        distinct(pref_code, city_code, pref_name, pref_name_ja, city_name_ja, grid) |>
        mutate(grid_to_coords(grid)) |>
        grid_as_sf(crs = WGS84)
    }
  }) |>
    bindCache(input$select_grid_city_city_code, input$select_grid_city_grid_size) |>
    bindEvent(input$display_grid_city)

  output$leaflet_grid_city <- renderLeaflet({
    data_grid_city <- reactive_data_grid_city()

    if (!is.null(data_grid_city)) {
      pal <- color_factor_brewer(data_grid_city$city_name_ja)
      leaflet(data_grid_city,
              options = leafletOptions_grid()) |>
        addProviderTiles_grid() |>
        addPolygons_grid(color = ~pal(city_name_ja)) |>
        addLegend_grid(pal = pal,
                       title = "市区町村名",
                       values = ~city_name_ja)
    }
  }) |>
    bindCache(input$select_grid_city_city_code, input$select_grid_city_grid_size) |>
    bindEvent(input$display_grid_city)

  observe({
    zoom <- input$leaflet_grid_city_zoom

    if (!is.null(zoom)) {
      data_grid_city <- reactive_data_grid_city()

      leafletProxy("leaflet_grid_city",
                   session = session,
                   data = data_grid_city) |>
        clearMarkers() |>
        addLabelOnlyMarkers_grid(grid = data_grid_city$grid,
                                 zoom = zoom)
    }
  }) |>
    bindEvent(input$display_grid_city, input$leaflet_grid_city_zoom)

  file_name_grid_city <- function(input, grid,
                                  ext = "") {
    city_code <- input$select_grid_city_city_code
    grid_size <- class(grid)[[1L]] |>
      str_remove("^grid_")

    city_code <- str_c(city_code,
                       collapse = "_")
    path(str_c("grid_city", city_code, grid_size,
               sep = "-"),
         ext = ext)
  }

  output$download_grid_city <- downloadHandler(
    filename = \() {
      ext <- input$select_grid_city_ext
      if (ext == "shp") {
        ext <- "zip"
      }
      data_grid_city <- reactive_data_grid_city()

      file_name_grid_city(input = input,
                          grid = data_grid_city$grid,
                          ext = ext)
    },
    content = \(file) {
      data_grid_city <- reactive_data_grid_city()
      ext <- input$select_grid_city_ext

      if (!is.null(data_grid_city)) {
        data_grid_city <- data_grid_city |>
          select(pref_code, city_code, pref_name, pref_name_ja, city_name_ja, grid)

        if (ext == "gpkg") {
          layer <- file_name_grid_city(input = input,
                                       grid = data_grid_city$grid)
          data_grid_city <- data_grid_city |>
            mutate(grid = as.character(grid))

          write_sf(data_grid_city, file,
                   layer = layer)
        } else if (ext == "csv") {
          data_grid_city <- data_grid_city |>
            mutate(grid = as.character(grid)) |>
            st_drop_geometry()
          write_excel_csv(data_grid_city, file)
        }
      }
    }
  )
}
