# modules/pdf_subset.R
mod_pdf_subset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload a PDF", accept = ".pdf"),
    textInput(ns("pages"), "Pages (e.g., 1:3 or 1,3,5)", "1:3"),
    textInput(ns("name"), "Output file name", "subset.pdf"),
    actionButton(ns("go"), "Create"),
    tags$hr(),
    downloadButton(ns("dl"), "Download subset.pdf")
  )
}

mod_pdf_subset_server <- function(id, on_bound=function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("pdf_subset", "INFO", "Server bound / session started"); on_bound()
    rv <- reactiveValues(path=NULL)
    observeEvent(input$go, {
      req(input$file)
      if (!requireNamespace("qpdf", quietly = TRUE)) {
        log_line("pdf_subset", "ERROR", "qpdf not installed"); return(NULL)
      }
      pages <- tryCatch(eval(parse(text = input$pages)), error=function(e){ log_line("pdf_subset","ERROR","Invalid pages"); NULL })
      req(!is.null(pages))
      work <- tempfile("subset_"); dir.create(work)
      out <- file.path(work, if (nzchar(input$name)) input$name else "subset.pdf")
      ok <- TRUE
      tryCatch(qpdf::pdf_subset(input$file$datapath, pages = pages, output = out),
               error=function(e){ ok <<- FALSE; log_line("pdf_subset","ERROR",conditionMessage(e)) })
      if (ok) rv$path <- out
    })
    output$dl <- downloadHandler(
      filename=function(){ if (is.null(rv$path)) "subset.pdf" else basename(rv$path) },
      content=function(file){ req(rv$path); file.copy(rv$path, file, overwrite = TRUE) }
    )
  })
}
