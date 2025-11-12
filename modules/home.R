# modules/home.R
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$p("Home")
  )
}

mod_home_server <- function(id, on_bound=function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("home", "INFO", "Server bound / session started"); on_bound()
  })
}
