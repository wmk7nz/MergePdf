# modules/common/helpers.R
if (!dir.exists("logs")) dir.create("logs", recursive = TRUE, showWarnings = FALSE)

log_path_for <- function(module) {
  file.path("logs", sprintf("%s-%s.log", module, format(Sys.Date(), "%Y-%m-%d")))
}

log_line <- function(module, level = "INFO", msg = "") {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("%s [%s] %s", ts, level, msg)
  cat(line, sep = "\n")
  # cat(line, file = log_path_for(module), sep = "\n", append = TRUE)
}
