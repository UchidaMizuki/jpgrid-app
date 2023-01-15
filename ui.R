# https://stackoverflow.com/questions/41440016/shinydasboard-not-loading-r
source("utils.R")
source("data.R")

# UI ----------------------------------------------------------------------

header <- dashboardHeader(title = "jpgrid App")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("市区町村別メッシュ取得",
             tabName = "tab_grid_city2015",
             icon = icon("tree-city")),
    menuItem("緯度経度からメッシュに変換",
             tabName = "tab_XY_to_grid",
             icon = icon("location-pin")),
    menuItem("メッシュから中心点に変換",
             tabName = "tab_grid_to_XY",
             icon = icon("border-all"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "tab_grid_city2015",
      fluidRow(
        column(
          width = 4,
          box(
            width = 12,
            shinyWidgets::pickerInput("select_grid_city2015_pref_code",
                                      "都道府県",
                                      choices = data_grid_city2015_pref$pref_code |>
                                        set_names(data_grid_city2015_pref$pref_name_ja),
                                      multiple = TRUE),
            uiOutput("select_grid_city2015_city_code"),
            uiOutput("select_grid_city2015_grid_size"),
            shinyWidgets::radioGroupButtons("select_grid_city2015_grid_size",
                                            "メッシュサイズ",
                                            choices = c("1km", "10km", "80km"),
                                            justified = TRUE),
            shinyWidgets::actionBttn("display_grid_city2015",
                                     "メッシュ表示",
                                     style = "material-flat",
                                     icon = icon("map"))
          ),
          box(
            width = 12,
            shinyWidgets::radioGroupButtons("select_grid_city2015_ext",
                                            "データ形式",
                                            choices = c("csv", "gpkg"),
                                            justified = TRUE),
            shinyWidgets::downloadBttn("download_grid_city2015",
                                       "ダウンロード",
                                       style = "material-flat")
          )
        ),
        box(
          width = 8,
          leafletOutput("leaflet_grid_city2015",
                        height = 800)
        )
      )
    ),
    tabItem(
      tabName = "tab_XY_to_grid",
      fluidRow(
        column(
          width = 4,
          box(
            width = 12,
            fileInput("file_XY_to_grid",
                      "緯度経度データの選択"),
            uiOutput("select_XY_to_grid_X"),
            uiOutput("select_XY_to_grid_Y"),
            uiOutput("select_XY_to_grid_name"),
            shinyWidgets::radioGroupButtons("select_XY_to_grid_grid_size",
                                            "メッシュサイズ",
                                            choices = rev(grid_size),
                                            justified = TRUE),
            shinyWidgets::actionBttn("display_XY_to_grid",
                                     "データ変換",
                                     style = "material-flat",
                                     icon = icon("map"))
          ),
          box(
            width = 12,
            shinyWidgets::radioGroupButtons("select_XY_to_grid_ext",
                                            "データ形式",
                                            choices = c("csv", "gpkg"),
                                            justified = TRUE),
            shinyWidgets::downloadBttn("download_XY_to_grid",
                                       "ダウンロード",
                                       style = "material-flat")
          )
        ),
        box(
          width = 8,
          leafletOutput("leaflet_XY_to_grid",
                        height = 800)
        )
      ),
    ),
    tabItem(
      tabName = "tab_grid_to_XY",
      fluidRow(
        column(
          width = 4,
          box(
            width = 12,
            fileInput("file_grid_to_XY",
                      "メッシュデータの選択"),
            uiOutput("select_grid_to_XY_grid"),
            shinyWidgets::radioGroupButtons("select_grid_to_XY_grid_size",
                                            "メッシュサイズ",
                                            choices = rev(grid_size),
                                            justified = TRUE),
            shinyWidgets::actionBttn("display_grid_to_XY",
                                     "データ変換",
                                     style = "material-flat",
                                     icon = icon("map"))
          ),
          box(
            width = 12,
            shinyWidgets::radioGroupButtons("select_grid_to_XY_ext",
                                            "データ形式",
                                            choices = c("csv", "gpkg"),
                                            justified = TRUE),
            shinyWidgets::downloadBttn("download_grid_to_XY",
                                       "ダウンロード",
                                       style = "material-flat")
          )
        ),
        box(
          width = 8,
          leafletOutput("leaflet_grid_to_XY",
                        height = 800)
        )
      ),
    )
  )
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
)
