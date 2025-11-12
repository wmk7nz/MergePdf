# modules/pdf_overlay_stamp.R
mod_pdf_overlay_stamp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("input"), "Upload input PDF", accept = ".pdf"),
    fileInput(ns("stamp"), "Upload stamp PDF (first page used)", accept = ".pdf"),
    textInput(ns("name"), "Output file name", "stamped.pdf"),
    actionButton(ns("go"), "Overlay"),
    tags$hr(),
    downloadButton(ns("dl"), "Download stamped.pdf")
  )
}

mod_pdf_overlay_stamp_server <- function(id, on_bound=function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("pdf_overlay_stamp", "INFO", "Server bound / session started"); on_bound()
    rv <- reactiveValues(path=NULL)
    observeEvent(input$go, {
      req(input$input, input$stamp)
      if (!requireNamespace("qpdf", quietly = TRUE)) {
        log_line("pdf_overlay_stamp", "ERROR", "qpdf not installed"); return(NULL)
      }
      work <- tempfile("stamp_"); dir.create(work)
      out <- file.path(work, if (nzchar(input$name)) input$name else "stamped.pdf")
      ok <- TRUE
      tryCatch(qpdf::pdf_overlay_stamp(input = input$input$datapath, stamp = input$stamp$datapath, output = out),
               error=function(e){ ok<<-FALSE; log_line("pdf_overlay_stamp","ERROR",conditionMessage(e)) })
      if (ok) rv$path <- out
    })
    output$dl <- downloadHandler(
      filename=function(){ if (is.null(rv$path)) "stamped.pdf" else basename(rv$path) },
      content=function(file){ req(rv$path); file.copy(rv$path, file, overwrite = TRUE) }
    )
  })
}
