
# ui-parse_grid -----------------------------------------------------------

tabItem_parse_grid <- tabItem(
  tabName = "tab_parse_grid",
  fluidRow(
    column(
      width = 4,
      box(
        title = "データ選択",
        width = 12,
        fileInput("file_parse_grid",
                  "データの選択（CSVまたはExcel）"),
        uiOutput("select_parse_grid_col_grid"),
        uiOutput("select_parse_grid_col_id"),
        shinyWidgets::materialSwitch("select_parse_grid_grid_size_auto",
                                     "メッシュサイズの自動判定",
                                     value = TRUE),
        uiOutput("select_parse_grid_grid_size"),
        shinyWidgets::actionBttn("display_parse_grid",
                                 "メッシュ表示",
                                 style = style_button,
                                 icon = icon("map"))
      ),
      box(
        title = "データダウンロード",
        width = 12,
        shinyWidgets::radioGroupButtons("select_parse_grid_ext",
                                        "データ形式",
                                        choices = c("gpkg", "csv"),
                                        justified = TRUE),
        shinyWidgets::downloadBttn("download_parse_grid",
                                   "ダウンロード",
                                   style = style_button)
      )
    ),
    box(
      width = 8,
      leafletOutput("leaflet_parse_grid",
                    height = 800)
    )
  )
)
