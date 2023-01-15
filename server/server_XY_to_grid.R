
# server_XY_to_grid -------------------------------------------------------

server_XY_to_grid <- function(input, output, session) {
  reactive_data_XY_to_grid_raw <- reactive({
    file_XY_to_grid <- input$file_XY_to_grid$datapath
    ext <- path_ext(file_XY_to_grid)

    if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(file_XY_to_grid,
                         col_types = "text")
    } else {
      read_csv(file_XY_to_grid,
               col_types = cols(.default = "c"))
    }
  }) |>
    bindEvent(input$file_XY_to_grid)

  output$select_XY_to_grid_X <- renderUI({
    data_XY_to_grid_raw <- reactive_data_XY_to_grid_raw()

    shinyWidgets::pickerInput("select_XY_to_grid_X",
                              "経度の列名（X方向）",
                              choices = names(data_XY_to_grid_raw))
  }) |>
    bindEvent(input$file_XY_to_grid)

  output$select_XY_to_grid_Y <- renderUI({
    data_XY_to_grid_raw <- reactive_data_XY_to_grid_raw()

    shinyWidgets::pickerInput("select_XY_to_grid_Y",
                              "緯度の列名（Y方向）",
                              choices = names(data_XY_to_grid_raw))
  }) |>
    bindEvent(input$file_XY_to_grid)

  output$select_XY_to_grid_name <- renderUI({
    data_XY_to_grid_raw <- reactive_data_XY_to_grid_raw()

    shinyWidgets::pickerInput("select_XY_to_grid_name",
                              "地点名の列名（任意）",
                              choices = c("", names(data_XY_to_grid_raw)))
  }) |>
    bindEvent(input$file_XY_to_grid)

  reactive_data_XY_to_grid <- reactive({
    data_XY_to_grid_raw <- reactive_data_XY_to_grid_raw()

    if (!is.null(data_XY_to_grid_raw)) {
      X <- input$select_XY_to_grid_X
      Y <- input$select_XY_to_grid_Y
      name <- input$select_XY_to_grid_name
      grid_size <- input$select_XY_to_grid_grid_size

      data_XY_to_grid <- tibble(X = data_XY_to_grid_raw[[X]],
                                Y = data_XY_to_grid_raw[[Y]])
      if (!is.null(name)) {
        data_XY_to_grid <- data_XY_to_grid |>
          add_column(name = data_XY_to_grid_raw[[name]])
      }

      data_XY_to_grid |>
        mutate(across(c(X, Y),
                      parse_number),
               grid = XY_to_grid(X, Y,
                                 size = grid_size),
               grid_to_XY(grid) |>
                 rename_with(~ .x |>
                               str_c("_grid"))) |>
        as_tbl_grid() |>
        st_as_sf(crs = WGS84)
    }
  }) |>
    bindEvent(input$display_XY_to_grid)

  output$leaflet_XY_to_grid <- renderLeaflet({
    data_XY_to_grid <- reactive_data_XY_to_grid()

    if (!is.null(data_XY_to_grid)) {
      leaflet_XY_to_grid <- leaflet(data_XY_to_grid,
              options = leafletOptions_grid()) |>
        addProviderTiles_grid() |>
        addPolygons_grid(color = "steelblue",
                         fillColor = "steelblue")

      if ("name" %in% names(data_XY_to_grid)) {
        leaflet_XY_to_grid <- leaflet_XY_to_grid |>
          addMarkers(lng = ~X,
                     lat = ~Y,
                     label = ~name)
      } else {
        leaflet_XY_to_grid <- leaflet_XY_to_grid |>
          addMarkers(lng = ~X,
                     lat = ~Y)
      }

      leaflet_XY_to_grid
    }
  }) |>
    bindEvent(input$display_XY_to_grid)

  observe({
    zoom <- input$leaflet_XY_to_grid_zoom

    if (!is.null(zoom)) {
      data_XY_to_grid <- reactive_data_XY_to_grid()
      grid_size <- input$select_XY_to_grid_grid_size

      text_size <- text_size_grid(grid_size, zoom)
      leafletProxy("leaflet_XY_to_grid",
                   session = session,
                   data = data_XY_to_grid) |>
        removeMarker(as.character(data_XY_to_grid$grid)) |>
        addLabelOnlyMarkers(lng = ~X_grid,
                            lat = ~Y_grid,
                            layerId = ~as.character(grid),
                            label = ~str_trunc_grid(grid,
                                                    text_size = text_size),
                            labelOptions = labelOptions_grid(textsize = str_c(text_size, "px"),
                                                             style = list(color = "steelblue")))
    }
  }) |>
    bindEvent(input$display_XY_to_grid, input$leaflet_XY_to_grid_zoom)

  filename_XY_to_grid <- function(input, ext) {
    grid_size <- input$select_XY_to_grid_grid_size

    path(str_c("XY_to_grid", grid_size,
               sep = "-"),
         ext = ext)
  }

  output$download_XY_to_grid <- downloadHandler(
    filename = \() {
      ext <- input$select_XY_to_grid_ext

      filename_XY_to_grid(input, ext)
    },
    content = \(file) {
      data_XY_to_grid <- reactive_data_XY_to_grid()
      X <- input$select_XY_to_grid_X
      Y <- input$select_XY_to_grid_Y
      name <- input$select_XY_to_grid_name
      ext <- input$select_XY_to_grid_ext

      if (!is.null(data_XY_to_grid)) {
        data_XY_to_grid <- data_XY_to_grid |>
          mutate(grid = as.character(grid))

        if (is.null(name)) {
          data_XY_to_grid <- data_XY_to_grid |>
            select(X, Y, grid) |>
            rename_with(~ c(X, Y),
                        c(X, Y))
        } else {
          data_XY_to_grid <- data_XY_to_grid |>
            select(name, X, Y, grid) |>
            rename_with(~ c(name, X, Y),
                        c(name, X, Y))
        }

        if (ext == "csv") {
          data_XY_to_grid <- as_tibble(data_XY_to_grid)
          write_excel_csv(data_XY_to_grid, file)
        } else if (ext == "gpkg") {
          layer <- filename_XY_to_grid(input, "")

          write_sf(data_XY_to_grid, file,
                   layer = layer)
        }
      }
    }
  )
}
