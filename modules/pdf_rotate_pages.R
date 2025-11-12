# modules/pdf_rotate_pages.R
mod_pdf_rotate_pages_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload a PDF", accept = ".pdf"),
    textInput(ns("pages"), "Pages (e.g., 1:3 or 2,4,6)", "1"),
    numericInput(ns("angle"), "Angle (degrees, +clockwise)", 90, step = 90),
    checkboxInput(ns("relative"), "Relative rotation", TRUE),
    textInput(ns("name"), "Output file name", "rotated.pdf"),
    actionButton(ns("go"), "Rotate"),
    tags$hr(),
    downloadButton(ns("dl"), "Download rotated.pdf")
  )
}

mod_pdf_rotate_pages_server <- function(id, on_bound=function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("pdf_rotate_pages", "INFO", "Server bound / session started"); on_bound()
    rv <- reactiveValues(path=NULL)
    observeEvent(input$go, {
      req(input$file)
      if (!requireNamespace("qpdf", quietly = TRUE)) {
        log_line("pdf_rotate_pages", "ERROR", "qpdf not installed"); return(NULL)
      }
      pages <- tryCatch(eval(parse(text = input$pages)), error=function(e){ log_line("pdf_rotate_pages","ERROR","Invalid pages"); NULL })
      req(!is.null(pages))
      work <- tempfile("rotate_"); dir.create(work)
      out <- file.path(work, if (nzchar(input$name)) input$name else "rotated.pdf")
      ok <- TRUE
      tryCatch(qpdf::pdf_rotate_pages(input$file$datapath, pages = pages, angle = input$angle,
                                      relative = isTRUE(input$relative), output = out),
               error=function(e){ ok<<-FALSE; log_line("pdf_rotate_pages","ERROR",conditionMessage(e)) })
      if (ok) rv$path <- out
    })
    output$dl <- downloadHandler(
      filename=function(){ if (is.null(rv$path)) "rotated.pdf" else basename(rv$path) },
      content=function(file){ req(rv$path); file.copy(rv$path, file, overwrite = TRUE) }
    )
  })
}
