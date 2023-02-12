
# server_parse_grid -------------------------------------------------------

server_parse_grid <- function(input, output, session) {
  reactive_data_parse_grid_raw <- reactive({
    file_parse_grid <- input$file_parse_grid$datapath
    ext <- path_ext(file_parse_grid)

    if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(file_parse_grid,
                         col_types = "text")
    } else {
      read_csv(file_parse_grid,
               col_types = cols(.default = "c"))
    }
  }) |>
    bindEvent(input$file_parse_grid)

  output$select_parse_grid_col_grid <- renderUI({
    data_parse_grid_raw <- reactive_data_parse_grid_raw()

    shinyWidgets::pickerInput("select_parse_grid_col_grid",
                              "メッシュ文字列の列名",
                              choices = names(data_parse_grid_raw))
  }) |>
    bindEvent(input$file_parse_grid)

  output$select_parse_grid_col_id <- renderUI({
    data_parse_grid_raw <- reactive_data_parse_grid_raw()

    shinyWidgets::pickerInput("select_parse_grid_col_id",
                              "地点IDの列名（任意）",
                              choices = c("", names(data_parse_grid_raw)))
  }) |>
    bindEvent(input$file_parse_grid)

  output$select_parse_grid_grid_size <- renderUI({
    grid_size_auto <- input$select_parse_grid_grid_size_auto

    if (grid_size_auto) {
      return(NULL)
    } else {
      shinyWidgets::radioGroupButtons("select_parse_grid_grid_size",
                                      "メッシュサイズ",
                                      choices = grid_size,
                                      justified = TRUE)
    }
  }) |>
    bindEvent(input$select_parse_grid_grid_size_auto)

  reactive_data_parse_grid <- reactive({
    data_parse_grid_raw <- reactive_data_parse_grid_raw()

    if (!is.null(data_parse_grid_raw)) {
      col_grid <- input$select_parse_grid_col_grid
      col_id <- input$select_parse_grid_col_id
      grid_size <- input$select_parse_grid_grid_size

      if (col_id == "") {
        id <- NA_character_
      } else {
        id <- data_parse_grid_raw[[col_id]]
      }

      tibble(id = id,
             grid = data_parse_grid_raw[[col_grid]]) |>
        mutate(grid = parse_grid(grid,
                                 grid_size = grid_size,
                                 strict = FALSE),
               grid_to_coords(grid)) |>
        grid_as_sf(crs = WGS84)
    }
  }) |>
    bindEvent(input$display_parse_grid)

  output$leaflet_parse_grid <- renderLeaflet({
    data_parse_grid <- reactive_data_parse_grid()

    if (!is.null(data_parse_grid)) {
      leaflet(data_parse_grid,
              options = leafletOptions_grid()) |>
        addProviderTiles_grid() |>
        addPolygons_grid()
    }
  }) |>
    bindEvent(input$display_parse_grid)

  observe({
    zoom <- input$leaflet_parse_grid_zoom

    if (!is.null(zoom)) {
      data_parse_grid <- reactive_data_parse_grid()

      leafletProxy("leaflet_parse_grid",
                   session = session,
                   data = data_parse_grid) |>
        clearMarkers() |>
        addLabelOnlyMarkers_grid(grid = data_parse_grid$grid,
                                 zoom = zoom)
    }
  }) |>
    bindEvent(input$display_parse_grid, input$leaflet_parse_grid_zoom)

  file_name_parse_grid <- function(grid,
                                   ext = "") {
    grid_size <- class(grid)[[1L]] |>
      str_remove("^grid_")

    path(str_c("parse_grid", grid_size,
               sep = "-"),
         ext = ext)
  }

  rename_data_parse_grid <- function(data_parse_grid, col_grid, col_id) {
    if (col_id == "") {
      data_parse_grid <- data_parse_grid["grid"]
      names(data_parse_grid) <- c(col_grid, "geometry")
    } else {
      data_parse_grid <- data_parse_grid[c("id", "grid")]
      names(data_parse_grid) <- c(col_id, col_grid, "geometry")
    }
    data_parse_grid
  }

  output$download_parse_grid <- downloadHandler(
    filename = \() {
      ext <- input$select_parse_grid_ext
      if (ext == "shp") {
        ext <- "zip"
      }
      data_parse_grid <- reactive_data_parse_grid()

      file_name_parse_grid(grid = data_parse_grid$grid,
                           ext = ext)
    },
    content = \(file) {
      data_parse_grid <- reactive_data_parse_grid()
      col_grid <- input$select_parse_grid_col_grid
      col_id <- input$select_parse_grid_col_id
      ext <- input$select_parse_grid_ext

      if (!is.null(data_parse_grid)) {
        if (ext == "gpkg") {
          layer <- file_name_parse_grid(grid = data_parse_grid$grid)
          data_parse_grid <- data_parse_grid |>
            mutate(grid = as.character(grid)) |>
            rename_data_parse_grid(col_grid = col_grid,
                                   col_id = col_id)

          write_sf(data_parse_grid, file,
                   layer = layer)
        } else if (ext == "csv") {
          data_parse_grid <- data_parse_grid |>
            mutate(grid = as.character(grid)) |>
            rename_data_parse_grid(col_grid = col_grid,
                                   col_id = col_id) |>
            st_drop_geometry()
          write_excel_csv(data_parse_grid, file)
        }
      }
    }
  )
}
