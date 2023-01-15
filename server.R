
# server ------------------------------------------------------------------

source("server/server_XY_to_grid.R")
source("server/server_grid_to_XY.R")
source("server/server_grid_city2015.R")

server <- function(input, output, session) {
  server_grid_city2015(input, output, session)
  server_XY_to_grid(input, output, session)
  server_grid_to_XY(input, output, session)
}
