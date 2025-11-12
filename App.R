# App.R
library(shiny)
source("modules/common/helpers.R", local = TRUE)

# loaded modules
MODULES <- c("home","pdf_length","pdf_split","pdf_subset","pdf_combine",
             "pdf_compress","pdf_rotate_pages","pdf_overlay_stamp")

# auto loaded modules
for (m in MODULES) {
  path <- sprintf("modules/%s.R", m)
  if (file.exists(path)) source(path, local = TRUE)
}

home_tab <- tabPanel("Home",
  fluidPage(
    tags$h2("Home"),
    if ("home" %in% MODULES) mod_home_ui("home") else tags$p("（Home module unload）"),
    tags$hr(),
    tags$h4("Binding status"),
    uiOutput("bind_status"),
    tags$hr(),
    tags$h4("TOday logs（last 50 rows）"),
    selectInput("log_module", "select module", MODULES, selected = MODULES[1]),
    verbatimTextOutput("log_tail", placeholder = TRUE)
  )
)

other_tabs <- list()
for (m in MODULES) {
  if (m == "home") next
  ui_fun <- get0(sprintf("mod_%s_ui", m), mode = "function")
  if (!is.null(ui_fun)) {
    other_tabs[[length(other_tabs)+1]] <- tabPanel(m, fluidPage(ui_fun(m)))
  }
}

ui <- do.call(navbarPage, c(list(title = "qpdf Tools (Simple Modules)", id = "nav"),
                            list(home_tab),
                            other_tabs))

server <- function(input, output, session) {
  bound <- reactiveValues()
  for (m in MODULES) bound[[m]] <- FALSE

  for (m in MODULES) {
    srv_fun <- get0(sprintf("mod_%s_server", m), mode = "function")
    if (!is.null(srv_fun)) {
      srv_fun(m, on_bound = (function(mm){function(){ bound[[mm]] <<- TRUE } })(m))
    }
  }

  output$bind_status <- renderUI({
    tags$div(lapply(MODULES, function(m) {
      val <- isolate(bound[[m]])
      lab <- if (isTRUE(val)) "✅ Bound" else "❌ Not bound"
      tags$p(sprintf("%s: %s", m, lab))
    }))
  })

  output$log_tail <- renderText({
    module <- req(input$log_module)
    path <- log_path_for(module)
    if (!file.exists(path)) return("No logs yet.")
    txt <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
    if (length(txt) == 0) "No logs yet." else paste(tail(txt, 50), collapse = "\n")
  })
}

shinyApp(ui, server)