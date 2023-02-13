source("global.R")

# server ------------------------------------------------------------------

source("server/server-about.R")
source("server/server-grid_city.R")
source("server/server-parse_grid.R")
source("server/server-coords_to_grid.R")

server <- function(input, output, session) {
  server_about(input, output, session)
  server_grid_city(input, output, session)
  server_parse_grid(input, output, session)
  server_coords_to_grid(input, output, session)
}
