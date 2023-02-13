source("utils.R")

# UI ----------------------------------------------------------------------

href_jpgrid <- "https://uchidamizuki.github.io/jpgrid/"

icon_tab_about <- icon("circle-info")
icon_tab_grid_city <- icon("tree-city")
icon_tab_parse_grid <- icon("hashtag")
icon_tab_coords_to_grid <- icon("location-pin")

text_tab_about <- "このアプリについて"
text_tab_grid_city <- "市区町村別のメッシュ生成"
text_tab_parse_grid <- "文字列からメッシュ生成"
text_tab_coords_to_grid <- "緯度経度からメッシュ生成"

style_button <- "simple"

header <- dashboardHeader(title = "jpgrid App")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      text_tab_about,
      tabName = "tab_about",
      icon = icon_tab_about
    ),
    menuItem(
      text_tab_grid_city,
      tabName = "tab_grid_city",
      icon = icon_tab_grid_city
    ),
    menuItem(
      text_tab_parse_grid,
      tabName = "tab_parse_grid",
      icon = icon_tab_parse_grid
    ),
    menuItem(
      text_tab_coords_to_grid,
      tabName = "tab_coords_to_grid",
      icon = icon_tab_coords_to_grid
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "tab_about",
      fluidRow(
        box(
          title = "このアプリについて",
          width = 12,
          "地域メッシュデータを利用するためのRパッケージである",
          a_blank(
            href = href_jpgrid,
            "jpgrid"
          ),
          "パッケージの機能の一部を提供しています．",
          br(),
          a_blank(
            href = href_jpgrid,
            img(
              src = "https://raw.githubusercontent.com/UchidaMizuki/jpgrid/main/man/figures/logo.png",
              align = "right",
              height = 139
            )
          ),
          "このアプリは以下の機能を提供しています．",
          br(),
          br(),
          shinyWidgets::actionBttn("link_tab_grid_city",
                                   text_tab_grid_city,
                                   style = style_button,
                                   icon = icon_tab_grid_city),
          br(),
          br(),
          shinyWidgets::actionBttn("link_tab_parse_grid",
                                   text_tab_parse_grid,
                                   style = style_button,
                                   icon = icon_tab_parse_grid),
          br(),
          br(),
          shinyWidgets::actionBttn("link_tab_coords_to_grid",
                                   text_tab_coords_to_grid,
                                   style = style_button,
                                   icon = icon_tab_coords_to_grid)
        )
      )
    ),
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
                                     style = style_button,
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
                                       style = style_button)
          ),
          box(
            title = "データについて",
            width = 12,
            "総務省統計局の",
            a_blank(
              href = "https://www.stat.go.jp/data/mesh/m_itiran.html",
              "市区町村別メッシュ・コード一覧"
            ),
            "を",
            br(),
            "加工して作成"
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
  )
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "midnight"
)
