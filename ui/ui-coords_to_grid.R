
# ui-coords_to_grid -------------------------------------------------------

tabItem_coords_to_grid <- tabItem(
  tabName = "tab_coords_to_grid",
  fluidRow(
    column(
      width = 4,
      box(
        title = "データ選択",
        width = 12,
        fileInput("file_coords_to_grid",
                  "データの選択（CSVまたはExcel）"),
        uiOutput("select_coords_to_grid_col_X"),
        uiOutput("select_coords_to_grid_col_Y"),
        uiOutput("select_coords_to_grid_col_id"),
        shinyWidgets::radioGroupButtons("select_coords_to_grid_grid_size",
                                        "メッシュサイズ",
                                        choices = grid_size,
                                        justified = TRUE),
        shinyWidgets::actionBttn("display_coords_to_grid",
                                 "メッシュ表示",
                                 style = style_button,
                                 icon = icon("map"))
      ),
      box(
        title = "データダウンロード",
        width = 12,
        shinyWidgets::radioGroupButtons("select_coords_to_grid_ext",
                                        "データ形式",
                                        choices = c("gpkg", "csv"),
                                        justified = TRUE),
        shinyWidgets::downloadBttn("download_coords_to_grid",
                                   "ダウンロード",
                                   style = style_button)
      )
    ),
    box(
      width = 8,
      leafletOutput("leaflet_coords_to_grid",
                    height = 800)
    )
  )
)
