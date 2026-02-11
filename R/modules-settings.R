# R/modules-settings.R
settings_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6,
           h4("About"),
           tags$p("Prototype for modular auto-classification with LLM-based semantic enrichment (TF-IDF baseline)."),
           tags$p("LLM prompts are cached (Parquet) to reduce cost & latency. Hierarchical schemas are aggregated so parent nodes include descendant semantics."),
           tags$p(class="small-note","Note: because we don't train on your data, a train/test split isn't required. If you tune thresholds/hyperparameters on labeled data, report metrics via cross-validation to avoid optimistic bias.")
    ),
    column(6,
           h4("Cache"),
           actionButton(ns("btn_save_snapshot"), "Save snapshot (scores & cache)"),
           verbatimTextOutput(ns("cache_info"))
    )
  )
}

settings_server <- function(id, docs_enriched, cats_enriched, scores, ckpt_dir) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$btn_save_snapshot, {
      if (!is.null(cats_enriched())) parquet_write(cats_enriched(), ckpt_path(ckpt_dir(), "cats_enriched.parquet"))
      if (!is.null(docs_enriched())) parquet_write(docs_enriched(), ckpt_path(ckpt_dir(), "docs_enriched.parquet"))
      if (!is.null(scores())) parquet_write(scores(), ckpt_path(ckpt_dir(), "scores.parquet"))
      showNotification("Snapshot saved to checkpoint directory.")
    })
    output$cache_info <- renderText({
      dir <- ckpt_dir()
      if (!dir.exists(dir)) return("Directory not found.")
      paste(c(dir, list.files(dir, full.names = TRUE)), collapse = "\n")
    })
  })
}
