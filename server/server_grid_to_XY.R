
# server_grid_to_XY -------------------------------------------------------

server_grid_to_XY <- function(input, output, session) {
  reactive_data_grid_to_XY_raw <- reactive({
    file_grid_to_XY <- input$file_grid_to_XY$datapath
    ext <- path_ext(file_grid_to_XY)

    if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(file_grid_to_XY,
                         col_types = "text")
    } else {
      read_csv(file_grid_to_XY,
               col_types = cols(.default = "c"))
    }
  }) |>
    bindEvent(input$file_grid_to_XY)

  output$select_grid_to_XY_grid <- renderUI({
    data_grid_to_XY_raw <- reactive_data_grid_to_XY_raw()

    shinyWidgets::pickerInput("select_grid_to_XY_grid",
                              "メッシュの列名",
                              choices = names(data_grid_to_XY_raw))
  }) |>
    bindEvent(input$file_grid_to_XY)

  reactive_data_grid_to_XY <- reactive({
    data_grid_to_XY_raw <- reactive_data_grid_to_XY_raw()

    if (!is.null(data_grid_to_XY_raw)) {
      grid <- input$select_grid_to_XY_grid
      grid_size <- input$select_grid_to_XY_grid_size
      # FIXME: A bug of jpgrid
      grid_size <- grid_size_to_km(grid_size) * 1e3

      tibble(grid = data_grid_to_XY_raw[[grid]]) |>
        as_tbl_grid(grid,
                    size = grid_size) |>
        mutate(grid_to_XY(grid)) |>
        st_as_sf(crs = WGS84)
    }
  }) |>
    bindEvent(input$display_grid_to_XY)

  output$leaflet_grid_to_XY <- renderLeaflet({
    data_grid_to_XY <- reactive_data_grid_to_XY()

    if (!is.null(data_grid_to_XY)) {
      leaflet(data_grid_to_XY,
              options = leafletOptions_grid()) |>
        addProviderTiles_grid() |>
        addPolygons_grid(color = "steelblue",
                         fillColor = "steelblue") |>
        addMarkers(lng = ~X,
                   lat = ~Y)
    }
  }) |>
    bindEvent(input$display_grid_to_XY)

  observe({
    zoom <- input$leaflet_grid_to_XY_zoom

    if (!is.null(zoom)) {
      data_grid_to_XY <- reactive_data_grid_to_XY()
      grid_size <- input$select_grid_to_XY_grid_size

      text_size <- text_size_grid(grid_size, zoom)
      leafletProxy("leaflet_grid_to_XY",
                   session = session,
                   data = data_grid_to_XY) |>
        removeMarker(as.character(data_grid_to_XY$grid)) |>
        addLabelOnlyMarkers(lng = ~X,
                            lat = ~Y,
                            layerId = ~as.character(grid),
                            label = ~str_trunc_grid(grid,
                                                    text_size = text_size),
                            labelOptions = labelOptions_grid(textsize = str_c(text_size, "px"),
                                                             style = list(color = "steelblue")))
    }
  }) |>
    bindEvent(input$display_grid_to_XY, input$leaflet_grid_to_XY_zoom)

  filename_grid_to_XY <- function(input, ext) {
    grid_size <- input$select_grid_to_XY_grid_size

    path(str_c("grid_to_XY", grid_size,
               sep = "-"),
         ext = ext)
  }

  output$download_grid_to_XY <- downloadHandler(
    filename = \() {
      ext <- input$select_grid_to_XY_ext

      filename_grid_to_XY(input, ext)
    },
    content = \(file) {
      data_grid_to_XY <- reactive_data_grid_to_XY()
      grid <- input$select_grid_to_XY_grid
      ext <- input$select_grid_to_XY_ext

      if (!is.null(data_grid_to_XY)) {
        data_grid_to_XY <- data_grid_to_XY |>
          mutate(grid = as.character(grid)) |>
          select(grid, X, Y) |>
          rename_with(~ grid,
                      grid)

        if (ext == "csv") {
          data_grid_to_XY <- as_tibble(data_grid_to_XY)
          write_excel_csv(data_grid_to_XY, file)
        } else if (ext == "gpkg") {
          layer <- filename_grid_to_XY(input, "")

          write_sf(data_grid_to_XY, file,
                   layer = layer)
        }
      }
    }
  )
}
