
# ui-grid_city ------------------------------------------------------------

tabItem_grid_city <- tabItem(
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
)
