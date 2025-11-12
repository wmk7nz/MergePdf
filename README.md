# shinylive-qpdf-app

Modular Shiny app for **shinylive** wrapping key **qpdf** functions.
- Per-function submodules with prefixed filenames (`<module>/<module>_ui.R`, `<module>/<module>_server.R`).
- **shinylogs** records actions to `logs/` using `store_rds()`.
- Multi-output modules attempt to ZIP via `{zip}`; otherwise provide per-file downloads.

## Run (server Shiny)
```r
install.packages(c("shiny","shinylogs","qpdf"))
shiny::runApp("shinylive-qpdf-app")
```

## Export (shinylive)
```r
install.packages("shinylive")
shinylive::export("shinylive-qpdf-app","qpdf-app-site")
```
