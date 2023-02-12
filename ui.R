source("utils.R")

# UI ----------------------------------------------------------------------

header <- dashboardHeader(title = "jpgrid App")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("市区町村別のメッシュ生成",
             tabName = "tab_grid_city",
             icon = icon("tree-city")),
    menuItem("文字列からメッシュ生成",
             tabName = "tab_parse_grid",
             icon = icon("hashtag")),
    menuItem("緯度経度からメッシュ生成",
             tabName = "tab_coords_to_grid",
             icon = icon("location-pin"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "tab_grid_city",
      fluidRow(
        column(
          width = 4,
          box(
            title = "データ選択",
            width = 12,
            shinyWidgets::pickerInput("select_grid_city_pref_code",
                                      "都道府県",
                                      choices = data_pref$pref_code |>
                                        set_names(data_pref$pref_name_ja),
                                      multiple = TRUE),
            uiOutput("select_grid_city_city_code"),
            uiOutput("select_grid_city_grid_size"),
            shinyWidgets::radioGroupButtons("select_grid_city_grid_size",
                                            "メッシュサイズ",
                                            choices = c("1km", "10km", "80km"),
                                            justified = TRUE),
            shinyWidgets::actionBttn("display_grid_city",
                                     "メッシュ表示",
                                     style = "material-flat",
                                     icon = icon("map"))
          ),
          box(
            title = "データダウンロード",
            width = 12,
            shinyWidgets::radioGroupButtons("select_grid_city_ext",
                                            "データ形式",
                                            choices = c("gpkg", "csv"),
                                            justified = TRUE),
            shinyWidgets::downloadBttn("download_grid_city",
                                       "ダウンロード",
                                       style = "material-flat")
          )
        ),
        box(
          width = 8,
          leafletOutput("leaflet_grid_city",
                        height = 800)
        )
      )
    ),
    tabItem(
      tabName = "tab_parse_grid",
      fluidRow(
        column(
          width = 4,
          box(
            title = "データ選択",
            width = 12,
            fileInput("file_parse_grid",
                      "メッシュデータの選択"),
            uiOutput("select_parse_grid_col_grid"),
            uiOutput("select_parse_grid_col_id"),
            shinyWidgets::materialSwitch("select_parse_grid_grid_size_auto",
                                         "メッシュサイズの自動判定",
                                         value = TRUE),
            uiOutput("select_parse_grid_grid_size"),
            shinyWidgets::actionBttn("display_parse_grid",
                                     "メッシュ表示",
                                     style = "material-flat",
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
                                       style = "material-flat")
          )
        ),
        box(
          width = 8,
          leafletOutput("leaflet_parse_grid",
                        height = 800)
        )
      )
    ),
    tabItem(
      tabName = "tab_coords_to_grid",
      fluidRow(
        column(
          width = 4,
          box(
            title = "データ選択",
            width = 12,
            fileInput("file_coords_to_grid",
                      "メッシュデータの選択"),
            uiOutput("select_coords_to_grid_col_X"),
            uiOutput("select_coords_to_grid_col_Y"),
            uiOutput("select_coords_to_grid_col_id"),
            shinyWidgets::radioGroupButtons("select_coords_to_grid_grid_size",
                                            "メッシュサイズ",
                                            choices = grid_size,
                                            justified = TRUE),
            shinyWidgets::actionBttn("display_coords_to_grid",
                                     "メッシュ表示",
                                     style = "material-flat",
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
                                       style = "material-flat")
          )
        ),
        box(
          width = 8,
          leafletOutput("leaflet_coords_to_grid",
                        height = 800)
        )
      )
    )
  )
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "midnight"
)
