source("global.R")

# UI ----------------------------------------------------------------------

source("ui/ui-about.R")
source("ui/ui-grid_city.R")
source("ui/ui-parse_grid.R")
source("ui/ui-coords_to_grid.R")

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
    tabItem_about,
    tabItem_grid_city,
    tabItem_parse_grid,
    tabItem_coords_to_grid
  )
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "midnight"
)
