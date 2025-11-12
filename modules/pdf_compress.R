# modules/pdf_compress.R
mod_pdf_compress_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload a PDF", accept = ".pdf"),
    checkboxInput(ns("linear"), "Linearize (fast web view)", value = FALSE),
    textInput(ns("name"), "Output file name", "compressed.pdf"),
    actionButton(ns("go"), "Compress"),
    tags$hr(),
    downloadButton(ns("dl"), "Download compressed.pdf")
  )
}

mod_pdf_compress_server <- function(id, on_bound=function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("pdf_compress", "INFO", "Server bound / session started"); on_bound()
    rv <- reactiveValues(path=NULL)
    observeEvent(input$go, {
      req(input$file)
      if (!requireNamespace("qpdf", quietly = TRUE)) {
        log_line("pdf_compress", "ERROR", "qpdf not installed"); return(NULL)
      }
      work <- tempfile("compress_"); dir.create(work)
      out <- file.path(work, if (nzchar(input$name)) input$name else "compressed.pdf")
      ok <- TRUE
      tryCatch(qpdf::pdf_compress(input$file$datapath, output = out, linearize = isTRUE(input$linear)),
               error=function(e){ ok<<-FALSE; log_line("pdf_compress","ERROR",conditionMessage(e)) })
      if (ok) rv$path <- out
    })
    output$dl <- downloadHandler(
      filename=function(){ if (is.null(rv$path)) "compressed.pdf" else basename(rv$path) },
      content=function(file){ req(rv$path); file.copy(rv$path, file, overwrite = TRUE) }
    )
  })
}
