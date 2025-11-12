# modules/pdf_split.R
mod_pdf_split_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload a PDF", accept = ".pdf"),
    textInput(ns("prefix"), "Output base name", "page"),
    actionButton(ns("go"), "Split"),
    tags$hr(),
    uiOutput(ns("downloads"), inline = FALSE),
    tags$hr(),
    tags$h4("Debug (for troubleshooting)"),
    verbatimTextOutput(ns("dbg"), placeholder = TRUE)
  )
}

mod_pdf_split_server <- function(id, on_bound = function(){}) {
  moduleServer(id, function(input, output, session) {
    log_line("pdf_split", "INFO", "Server bound / session started")
    on_bound()
    
    rv <- reactiveValues(
      files = character(0), 
      zip   = NULL,         
      zip_label = "Download split_parts.zip",
      msg   = NULL
    )
    
    output$downloads <- renderUI({
      ns <- session$ns
      if (!is.null(rv$msg)) {
        return(div(style="color:#dc2626;font-weight:600;", rv$msg))
      }
      if (!is.null(rv$zip) && file.exists(rv$zip)) {
        return(downloadButton(ns("dl_zip"), label = rv$zip_label))
      }
      div(style = "color:#6b7280;", "No output yet — upload a PDF and click Split.")
    })
    
    # Debug 
    output$dbg <- renderText({
      has_upload <- !is.null(input$file)
      n_files <- length(rv$files)
      has_zip <- !is.null(rv$zip) && file.exists(rv$zip)
      paste(
        sprintf("has_upload: %s", has_upload),
        sprintf("rv$files length: %d", n_files),
        sprintf("rv$zip exists: %s", has_zip),
        sprintf("rv$msg: %s", if (is.null(rv$msg)) "NULL" else rv$msg),
        sep = "\n"
      )
    })
    
    output$dl_zip <- downloadHandler(
      filename = function() {
        if (is.null(rv$zip)) "split_parts.zip" else basename(rv$zip)
      },
      content = function(file) {
        req(rv$zip, file.exists(rv$zip))
        file.copy(rv$zip, file, overwrite = TRUE)
      }
    )
    
    observeEvent(input$go, {
      rv$msg   <- NULL
      rv$files <- character(0)
      rv$zip   <- NULL
      
      if (!requireNamespace("qpdf", quietly = TRUE)) {
        rv$msg <- "Package 'qpdf' not installed."
        log_line("pdf_split", "ERROR", rv$msg)
        showNotification(rv$msg, type = "error", duration = 6)
        return(NULL)
      }
      if (!requireNamespace("zip", quietly = TRUE)) {
        rv$msg <- "Package 'zip' not installed. Please install.packages('zip') to download a ZIP."
        log_line("pdf_split", "ERROR", rv$msg)
        showNotification(rv$msg, type = "error", duration = 6)
        return(NULL)
      }
      
      req(input$file)
      
      withProgress(message = "Splitting PDF...", value = 0, {
        incProgress(0.1, detail = "Preparing workspace")
        work <- tempfile("split_")
        dir.create(work, recursive = TRUE, showWarnings = FALSE)
        
        inpath <- file.path(work, "input.pdf")
        if (!file.copy(input$file$datapath, inpath, overwrite = TRUE)) {
          rv$msg <- "Failed to stage uploaded file."
          log_line("pdf_split", "ERROR", rv$msg)
          showNotification(rv$msg, type = "error", duration = 6)
          return(NULL)
        }
        
        pages <- NA_integer_
        try(pages <- qpdf::pdf_length(inpath), silent = TRUE)
        if (!is.na(pages) && pages <= 0) {
          rv$msg <- "Input PDF has 0 pages."
          log_line("pdf_split", "ERROR", rv$msg)
          showNotification(rv$msg, type = "error", duration = 6)
          return(NULL)
        }
        
        prefix <- if (nzchar(input$prefix)) input$prefix else "page"
        outbase <- file.path(work, prefix)
        
        incProgress(0.45, detail = "Running qpdf::pdf_split")
        ok <- TRUE
        files_vec <- character(0)
        tryCatch({
          files_vec <<- qpdf::pdf_split(inpath, output = outbase)
        }, error = function(e) {
          ok <<- FALSE
          rv$msg <- paste("Split error:", conditionMessage(e))
          log_line("pdf_split", "ERROR", rv$msg)
          showNotification(rv$msg, type = "error", duration = 8)
        })
        
        if (ok && length(files_vec) == 0) {
          cand <- list.files(work, pattern = paste0("^", basename(outbase), ".*\\.pdf$"),
                             full.names = TRUE)
          files_vec <- sort(cand)
        }
        
        if (!ok || length(files_vec) == 0) {
          if (is.null(rv$msg)) {
            rv$msg <- "Split produced no output files. Check if the PDF is valid or encrypted."
            log_line("pdf_split", "ERROR", rv$msg)
            showNotification(rv$msg, type = "error", duration = 8)
          }
          return(NULL)
        }
        
        rv$files <- files_vec
        log_line("pdf_split", "INFO", paste("Generated", length(files_vec), "files"))
        
        incProgress(0.8, detail = "Creating ZIP")
        zip_base <- paste0(basename(outbase), "_parts.zip")
        zip_path <- file.path(work, zip_base)
        tryCatch({
          zip::zipr(zipfile = zip_path, files = basename(files_vec), root = work)
        }, error = function(e) {
          rv$msg <- paste("ZIP error:", conditionMessage(e))
          log_line("pdf_split", "ERROR", rv$msg)
        })
        
        if (!is.null(rv$msg) || !file.exists(zip_path)) {
          if (is.null(rv$msg)) {
            rv$msg <- "ZIP creation failed. Please ensure package 'zip' works in your environment."
          }
          showNotification(rv$msg, type = "error", duration = 8)
          return(NULL)
        }
        
        rv$zip <- zip_path
        rv$zip_label <- paste0("Download ", basename(zip_path))
        log_line("pdf_split", "INFO", paste("ZIP created:", basename(zip_path)))
        incProgress(1.0, detail = "Done")
        showNotification("Split finished", type = "message", duration = 3)
      })
    })
  })
}
