# R/modules-scoring.R
scoring_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 4,
                 selectInput(ns("model_key"),"Model", choices = c("tfidf_llm_enrich")),
                 sliderInput(ns("top_k"),"Top-K categories to keep", min=1, max=20, value=5),
                 sliderInput(ns("threshold"),"Score threshold", min=0, max=1, value=0, step=0.01),
                 actionButton(ns("btn_score"),"Compute scores"),
                 hr(),
                 uiOutput(ns("category_filter_ui")),
                 awesomeCheckbox(ns("show_only_filtered"),"Apply category filter to table", FALSE),
                 hr(),
                 h4("Auto-threshold (if labels available)"),
                 actionButton(ns("btn_auto_threshold"),"Find threshold maximizing macro-F1")
    ),
    mainPanel(
      h4("Scores (per document)"),
      DTOutput(ns("scores_table")),
      hr(),
      h4("Metrics (if labels provided)"),
      DTOutput(ns("metrics_summary")),
      h5("Per-class"),
      DTOutput(ns("metrics_per_class"))
    )
  )
}

scoring_server <- function(id, docs_enriched, cats_enriched, labels, language, ckpt_dir) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(scores = NULL)
    
    output$category_filter_ui <- renderUI({
      cats <- cats_enriched(); req(cats)
      choices <- cats$name
      pickerInput(session$ns("category_filter"), "Limit table to categories:", choices = choices, multiple = TRUE,
                  options = list(`actions-box`=TRUE, `live-search`=TRUE))
    })
    
    observeEvent(input$btn_score, {
      docs_e <- docs_enriched(); cats_e <- cats_enriched(); req(docs_e, cats_e)
      lang <- language()
      model <- get_model(input$model_key)
      fitted <- model$fit(
        docs_dt   = docs_e[, .(id, enriched_text)],
        schema_dt = cats_e[, .(id, name, description, enriched_text, agg_enriched_text)],
        language  = lang
      )
      scores <- model$predict(fitted, top_k = input$top_k, threshold = input$threshold)
      parquet_write(scores, ckpt_path(ckpt_dir(), "scores.parquet"))
      rv$scores <- scores
    })
    
    observeEvent(input$btn_auto_threshold, {
      s <- rv$scores; lbl <- labels(); req(s, lbl)
      cand <- sort(unique(round(s$score, 3)))
      if (!length(cand)) return()
      best <- list(t=0, f1=-Inf)
      for (t in cand) {
        st <- s[score >= t]
        st <- st[ , .SD[which.min(rank)], by = id]
        m <- compute_metrics(st, lbl)
        f1 <- m$summary[metric=="macro_f1", value]
        if (length(f1) && !is.na(f1) && f1 > best$f1) best <- list(t=t, f1=f1)
      }
      updateSliderInput(session, "threshold", value = best$t)
      showNotification(paste("Auto-threshold set to", best$t, "with macro-F1 =", round(best$f1, 3)))
    })
    
    output$scores_table <- renderDT({
      s <- rv$scores; req(s)
      cats <- cats_enriched(); req(cats)
      d <- merge(s, cats[, .(category_id=id, category_name=name)], by="category_id", all.x=TRUE)
      if (isTRUE(input$show_only_filtered) && length(input$category_filter)) {
        d <- d[category_name %in% input$category_filter]
      }
      docs <- docs_enriched(); d <- merge(d, docs[, .(id, text = substr(enriched_text,1,300))], by="id", all.x=TRUE)
      lbl <- labels(); if (!is.null(lbl)) d <- merge(d, lbl, by="id", all.x=TRUE)
      setorder(d, id, rank)
      datatable(d, options = list(scrollX=TRUE, pageLength=25))
    })
    
    output$metrics_summary <- renderDT({
      s <- rv$scores; lbl <- labels(); req(s, lbl)
      s_top1 <- s[rank==1L]
      m <- compute_metrics(s_top1, lbl)
      datatable(m$summary, options = list(dom='t'))
    })
    output$metrics_per_class <- renderDT({
      s <- rv$scores; lbl <- labels(); req(s, lbl)
      s_top1 <- s[rank==1L]
      m <- compute_metrics(s_top1, lbl)
      datatable(m$per_class, options = list(pageLength=10))
    })
    
    list(scores = reactive(rv$scores))
  })
}
