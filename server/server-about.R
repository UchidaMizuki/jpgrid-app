
# server-about ------------------------------------------------------------

server_about <- function(input, output, session) {
  observe({
    updateTabItems(session, "tabs", "tab_grid_city")
  }) |>
    bindEvent(input$link_tab_grid_city)

  observe({
    updateTabItems(session, "tabs", "tab_parse_grid")
  }) |>
    bindEvent(input$link_tab_parse_grid)

  observe({
    updateTabItems(session, "tabs", "tab_coords_to_grid")
  }) |>
    bindEvent(input$link_tab_coords_to_grid)
}
