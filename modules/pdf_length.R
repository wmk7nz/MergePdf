# modules/pdf_length.R
mod_pdf_length_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload a PDF", accept = ".pdf"),
    actionButton(ns("go"), "Get length"),
    tags$hr(),
    verbatimTextOutput(ns("out"), placeholder = TRUE)
  )
}

mod_pdf_length_server <- function(id, on_bound=function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("pdf_length", "INFO", "Server bound / session started"); on_bound()
    output$out <- renderText("")
    observeEvent(input$go, {
      req(input$file)
      if (!requireNamespace("qpdf", quietly = TRUE)) {
        log_line("pdf_length", "ERROR", "qpdf not installed"); return(NULL)
      }
      n <- tryCatch(qpdf::pdf_length(input$file$datapath), error=function(e){ log_line("pdf_length","ERROR",conditionMessage(e)); NA })
      output$out <- renderText(if (is.na(n)) "Error" else paste("Pages:", n))
      log_line("pdf_length", "INFO", paste("Pages:", n))
    })
  })
}
