# modules/pdf_combine.R
mod_pdf_combine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("files"), "Upload multiple PDFs", multiple = TRUE, accept = ".pdf"),
    textInput(ns("name"), "Output file name", "combined.pdf"),
    tags$h4("file list(index：file name)"),
    verbatimTextOutput(ns("list"), placeholder = TRUE),
    textInput(ns("order"), "merge index（comma sep）", value = ""),
    actionButton(ns("apply"), "apply index"),
    tags$hr(),
    # downloadButton(ns("dl_code"), "下载可复现代码（在合并前）"),
    actionButton(ns("go"), "Combine"),
    tags$hr(),
    downloadButton(ns("dl"), "Download combined.pdf"),
    tags$br(), tags$br(),
    # tags$h4("local logs（last 50 rows）"),
    # verbatimTextOutput(ns("tail"), placeholder = TRUE)
  )
}

mod_pdf_combine_server <- function(id, on_bound=function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("pdf_combine", "INFO", "Server bound / session started"); on_bound()
    rv <- reactiveValues(order = integer(0), out = NULL)

    output$list <- renderText({
      req(input$files)
      idx <- seq_len(nrow(input$files))
      paste(paste0(idx, ": ", input$files$name), collapse = "\n")
    })

    observeEvent(input$apply, {
      req(input$files)
      if (!nzchar(input$order)) {
        showNotification("input index，e.g.: 2,1,3", type="warning"); return(NULL)
      }
      ord <- gsub("\\s+", "", input$order)
      ord <- strsplit(ord, ",", fixed = TRUE)[[1]]
      ord <- suppressWarnings(as.integer(ord))
      if (any(is.na(ord)) || length(ord) != nrow(input$files) || any(sort(ord) != seq_len(nrow(input$files)))) {
        showNotification("index error：must be  1..N ", type="error", duration=6)
        log_line("pdf_combine","ERROR","Invalid order input")
        return(NULL)
      }
      rv$order <- ord
      log_line("pdf_combine", "INFO", paste("Applied order:", paste(ord, collapse=",")))
    })

    output$dl_code <- downloadHandler(
      filename = function() "combine_repro.R",
      content = function(file) {
        req(input$files)
        ord <- if (length(rv$order)) rv$order else seq_len(nrow(input$files))
        files <- input$files$name[ord]
        out_name <- if (nzchar(input$name)) input$name else "combined.pdf"
        code <- c(
          "# Reproducible script for combining PDFs using {qpdf}",
          "if (!requireNamespace('qpdf', quietly = TRUE)) install.packages('qpdf')",
          sprintf("files <- c(%s)", paste(sprintf('\"%s\"', files), collapse = ", ")),
          "stopifnot(all(file.exists(files)))",
          sprintf('qpdf::pdf_combine(files, output = \"%s\")', out_name),
          sprintf('message(\"Done: %s\")', out_name)
        )
        writeLines(code, file, useBytes = TRUE)
        log_line("pdf_combine", "INFO", "User downloaded reproducible code")
      }
    )

    observeEvent(input$go, {
      log_line("pdf_combine", "INFO", "Clicked Combine button")
      req(input$files)
      if (!requireNamespace("qpdf", quietly = TRUE)) {
        log_line("pdf_combine", "ERROR", "qpdf not installed")
        showNotification("Package qpdf not installed", type="error", duration=5); return(NULL)
      }
      ord <- if (length(rv$order)) rv$order else seq_len(nrow(input$files))
      work <- tempfile("combine_"); dir.create(work)
      inpaths <- character(0)
      for (i in ord) {
        src <- input$files$datapath[i]
        dst <- file.path(work, input$files$name[i])
        if (file.copy(src, dst, overwrite = TRUE)) inpaths <- c(inpaths, dst)
      }
      log_line("pdf_combine", "INFO", paste("Order:", paste(ord, collapse=",")))
      log_line("pdf_combine", "INFO", paste("Files:", paste(basename(inpaths), collapse=", ")))
      if (length(inpaths) < 2) {
        showNotification("at least two PDF", type="error"); return(NULL)
      }
      out <- file.path(work, if (nzchar(input$name)) input$name else "combined.pdf")
      ok <- TRUE
      tryCatch(qpdf::pdf_combine(inpaths, output = out),
               error=function(e){ ok<<-FALSE; log_line("pdf_combine","ERROR",conditionMessage(e)); showNotification(conditionMessage(e), type="error", duration=8) })
      if (ok && file.exists(out)) {
        rv$out <- out
        log_line("pdf_combine", "INFO", paste("Combined into", basename(out)))
        showNotification("Combine finished", type="message", duration=3)
      }
    })

    output$dl <- downloadHandler(
      filename = function(){ if (is.null(rv$out)) "combined.pdf" else basename(rv$out) },
      content = function(file){ req(rv$out); file.copy(rv$out, file, overwrite = TRUE) }
    )

    output$tail <- renderText({
      path <- log_path_for("pdf_combine")
      if (!file.exists(path)) return("No logs yet.")
      txt <- tryCatch(readLines(path, warn = FALSE), error=function(e) character(0))
      if (length(txt) == 0) "No logs yet." else paste(tail(txt, 50), collapse = "\n")
    })
  })
}
