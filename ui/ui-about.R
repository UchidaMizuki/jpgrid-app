
# ui-about ----------------------------------------------------------------

tabItem_about <- tabItem(
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
)
