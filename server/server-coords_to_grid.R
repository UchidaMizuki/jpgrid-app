
# server-coords_to_grid ---------------------------------------------------

server_coords_to_grid <- function(input, output, session) {
  reactive_data_coords_to_grid_raw <- reactive({
    file_coords_to_grid <- input$file_coords_to_grid$datapath
    ext <- path_ext(file_coords_to_grid)

    if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(file_coords_to_grid,
                         col_types = "text")
    } else {
      read_csv(file_coords_to_grid,
               col_types = cols(.default = "c"))
    }
  }) |>
    bindEvent(input$file_coords_to_grid)

  output$select_coords_to_grid_col_X <- renderUI({
    data_coords_to_grid_raw <- reactive_data_coords_to_grid_raw()

    shinyWidgets::pickerInput("select_coords_to_grid_col_X",
                              "経度の列名（X方向）",
                              choices = names(data_coords_to_grid_raw))
  }) |>
    bindEvent(input$file_coords_to_grid)

  output$select_coords_to_grid_col_Y <- renderUI({
    data_coords_to_grid_raw <- reactive_data_coords_to_grid_raw()

    shinyWidgets::pickerInput("select_coords_to_grid_col_Y",
                              "緯度の列名（Y方向）",
                              choices = names(data_coords_to_grid_raw))
  }) |>
    bindEvent(input$file_coords_to_grid)

  output$select_coords_to_grid_col_id <- renderUI({
    data_coords_to_grid_raw <- reactive_data_coords_to_grid_raw()

    shinyWidgets::pickerInput("select_coords_to_grid_col_id",
                              "地点IDの列名（任意）",
                              choices = c("", names(data_coords_to_grid_raw)))
  }) |>
    bindEvent(input$file_coords_to_grid)

  reactive_data_coords_to_grid <- reactive({
    data_coords_to_grid_raw <- reactive_data_coords_to_grid_raw()

    if (!is.null(data_coords_to_grid_raw)) {
      col_X <- input$select_coords_to_grid_col_X
      col_Y <- input$select_coords_to_grid_col_Y
      col_id <- input$select_coords_to_grid_col_id
      grid_size <- input$select_coords_to_grid_grid_size

      if (col_id == "") {
        id <- NA_character_
      } else {
        id <- data_coords_to_grid_raw[[col_id]]
      }

      tibble(id = id,
             X_coords = parse_number(data_coords_to_grid_raw[[col_X]]),
             Y_coords = parse_number(data_coords_to_grid_raw[[col_Y]])) |>
        mutate(grid = coords_to_grid(X = X_coords,
                                     Y = Y_coords,
                                     grid_size = grid_size),
               grid_to_coords(grid)) |>
        grid_as_sf(crs = WGS84)
    }
  }) |>
    bindEvent(input$display_coords_to_grid)

  output$leaflet_coords_to_grid <- renderLeaflet({
    data_coords_to_grid <- reactive_data_coords_to_grid()

    if (!is.null(data_coords_to_grid)) {
      leaflet(data_coords_to_grid,
              options = leafletOptions_grid()) |>
        addProviderTiles_grid() |>
        addPolygons_grid()
    }
  }) |>
    bindEvent(input$display_coords_to_grid)

  observe({
    zoom <- input$leaflet_coords_to_grid_zoom
    col_id <- input$select_coords_to_grid_col_id

    if (!is.null(zoom)) {
      data_coords_to_grid <- reactive_data_coords_to_grid()

      out <- leafletProxy("leaflet_coords_to_grid",
                          session = session,
                          data = data_coords_to_grid) |>
        clearMarkers() |>
        addLabelOnlyMarkers_grid(grid = data_coords_to_grid$grid,
                                 zoom = zoom)

      if (col_id == "") {
        out <- out |>
          addMarkers(lng = ~X_coords,
                     lat = ~Y_coords)
      } else {
        out <- out |>
          addMarkers(lng = ~X_coords,
                     lat = ~Y_coords,
                     label = ~id)
      }
      out
    }
  }) |>
    bindEvent(input$display_coords_to_grid, input$leaflet_coords_to_grid_zoom)

  file_name_coords_to_grid <- function(grid,
                                       ext = "") {
    grid_size <- class(grid)[[1L]] |>
      str_remove("^grid_")

    path(str_c("coords_to_grid", grid_size,
               sep = "-"),
         ext = ext)
  }

  rename_data_coords_to_grid <- function(data_coords_to_grid, col_X, col_Y, col_id) {
    if (col_id == "") {
      data_coords_to_grid <- data_coords_to_grid[c("X", "Y")]
      names(data_coords_to_grid) <- c(col_X, col_Y, "geometry")
    } else {
      data_coords_to_grid <- data_coords_to_grid[c("id", "X", "Y")]
      names(data_coords_to_grid) <- c(col_id, col_X, col_Y, "geometry")
    }
    data_coords_to_grid
  }

  output$download_coords_to_grid <- downloadHandler(
    filename = \() {
      ext <- input$select_coords_to_grid_ext
      if (ext == "shp") {
        ext <- "zip"
      }
      data_coords_to_grid <- reactive_data_coords_to_grid()

      file_name_coords_to_grid(grid = data_coords_to_grid$grid,
                               ext = ext)
    },
    content = \(file) {
      data_coords_to_grid <- reactive_data_coords_to_grid()
      col_X <- input$select_coords_to_grid_col_X
      col_Y <- input$select_coords_to_grid_col_Y
      col_id <- input$select_coords_to_grid_col_id
      ext <- input$select_coords_to_grid_ext

      if (!is.null(data_coords_to_grid)) {
        if (ext == "gpkg") {
          layer <- file_name_coords_to_grid(grid = data_coords_to_grid$grid)
          data_coords_to_grid <- data_coords_to_grid |>
            mutate(grid = as.character(grid)) |>
            rename_data_coords_to_grid(col_X = col_X,
                                       col_Y = col_Y,
                                       col_id = col_id)

          write_sf(data_coords_to_grid, file,
                   layer = layer)
        } else if (ext == "csv") {
          data_coords_to_grid <- data_coords_to_grid |>
            mutate(grid = as.character(grid)) |>
            rename_data_coords_to_grid(col_X = col_X,
                                       col_Y = col_Y,
                                       col_id = col_id) |>
            st_drop_geometry()
          write_excel_csv(data_coords_to_grid, file)
        }
      }
    }
  )
}
