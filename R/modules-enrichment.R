# R/modules-enrichment.R
enrichment_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("OpenAI"),
                 passwordInput(ns("openai_key"), "OpenAI API key (or set OPENAI_API_KEY env var)", value = ""),
                 textInput(ns("openai_model"), "Model", value = "gpt-4o-mini"),
                 sliderInput(ns("llm_temp"), "Temperature", min=0, max=1, value=0.2, step=0.05),
                 hr(),
                 h4("Category enrichment"),
                 # Few-shot controls
                 awesomeCheckbox(ns("use_fewshot"), "Use few-shot seeds from labeled data", TRUE),
                 numericInput(ns("seeds_top"), "Seeds per category (derived)", value = 15, min=3, max=50, step=1),
                 numericInput(ns("seeds_min_df"), "Min doc freq for seed terms", value = 5, min = 1, step = 1),
                 sliderInput(ns("seeds_ngram_max"), "Max n-gram for seed extraction", min=1, max=3, value=2, step=1),
                 numericInput(ns("seeds_cap_prompt"), "Max seeds to include in prompt", value = 12, min = 3, max = 40, step = 1),
                 sliderInput(ns("seed_weight"), "Seed weight in category text", min=0, max=4, value=1, step=1),
                 actionButton(ns("btn_compute_seeds"), "Compute seeds from labels"),
                 DTOutput(ns("seeds_preview")),
                 hr(),
                 numericInput(ns("cat_k"), "LLM keywords per category", value = 15, min=3, max=100, step=1),
                 actionButton(ns("btn_enrich_categories"), "Enrich categories with LLM"),
                 hr(),
                 h4("Document enrichment"),
                 numericInput(ns("doc_k"), "LLM keywords per document", value = 12, min=3, max=100, step=1),
                 sliderInput(ns("doc_weight"), "Weight of original text (repeats)", min=1, max=8, value=3, step=1),
                 actionButton(ns("btn_enrich_docs"), "Enrich documents with LLM")
    ),
    mainPanel(
      h4("Category enrichment (sample)"),
      DTOutput(ns("cats_enriched_preview")),
      hr(),
      h4("Document enrichment (sample)"),
      DTOutput(ns("docs_enriched_preview"))
    )
  )
}

enrichment_server <- function(id, docs, schema, labels, language, ckpt_dir) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(docs_enriched = NULL, cats_enriched = NULL, seeds_dt = NULL)
    
    get_api_key <- reactive({
      key <- input$openai_key
      if (!nzchar(key)) key <- Sys.getenv("OPENAI_API_KEY")
      key
    })
    
    # --------- Seed derivation from labeled data ---------
    observeEvent(input$btn_compute_seeds, {
      d <- docs(); l <- labels()
      if (is.null(d) || is.null(l) || !nrow(d) || !nrow(l)) {
        showNotification("No labels found. Provide a label column (or demo 'class') to derive seeds.", type="warning")
        rv$seeds_dt <- NULL; return(NULL)
      }
      lang <- language()
      seeds <- derive_seed_terms(
        docs_dt   = d, labels_dt = l,
        text_col  = "text", label_col = "label",
        language  = lang, top_m = input$seeds_top,
        min_df    = input$seeds_min_df,
        ngram     = c(1L, as.integer(input$seeds_ngram_max)),
        max_global_df  = 0.40,     # drop very common terms
        extra_stop     = TRUE,     # add news-specific stopwords
        drop_shared_ge = 2L,       # remove seeds seen in >=2 classes
        min_token_len  = 3L
      )
      parquet_write(seeds, ckpt_path(ckpt_dir(), "cats_seed_terms.parquet"))
      rv$seeds_dt <- seeds
    })
    
    output$seeds_preview <- renderDT({
      req(rv$seeds_dt)
      datatable(head(rv$seeds_dt[, .(category_id, seeds = vapply(seed_terms, function(x) paste(x, collapse=", "), ""))], 20),
                options = list(scrollX=TRUE, pageLength=10))
    })
    
    # --------- Category enrichment (with optional few-shot seeds) ---------
    observeEvent(input$btn_enrich_categories, {
      cats <- schema(); req(cats)
      key <- get_api_key()
      if (!nzchar(key)) { showNotification("Provide OpenAI API key in Enrichment tab.", type="error"); return(NULL) }
      lang <- language()
      cats2 <- cats[, .(id=as.character(category_id), name=as.character(name), description=as.character(description), parent_id=as.character(parent_id))]
      
      seed_map <- NULL
      if (isTRUE(input$use_fewshot) && !is.null(rv$seeds_dt)) {
        # Map by category_id
        seed_map <- setNames(rv$seeds_dt$seed_terms, rv$seeds_dt$category_id)
      }
      
      cats_en <- enrich_categories_llm(
        cats2, api_key = key, model = input$openai_model,
        target_lang     = lang,
        k               = input$cat_k,
        temperature     = input$llm_temp,
        ckpt_dir        = ckpt_dir(),
        seed_terms_map  = seed_map,
        use_fewshot     = isTRUE(input$use_fewshot),
        seed_weight     = as.integer(input$seed_weight),
        fewshot_seed_cap= as.integer(input$seeds_cap_prompt)
      )
      cats_en <- compute_hierarchy_aggregation(cats_en)
      parquet_write(cats_en, ckpt_path(ckpt_dir(), "cats_enriched.parquet"))
      rv$cats_enriched <- cats_en
    })
    
    # --------- Document enrichment ---------
    observeEvent(input$btn_enrich_docs, {
      d <- docs(); req(d)
      key <- get_api_key()
      if (!nzchar(key)) { showNotification("Provide OpenAI API key in Enrichment tab.", type="error"); return(NULL) }
      lang <- language()
      docs_dt <- copy(d); docs_dt[, text := prep_text(text)]
      docs_en <- enrich_texts_llm(docs_dt, text_col="text", api_key = key, model = input$openai_model,
                                  target_lang = lang, k = input$doc_k, weight_text = input$doc_weight,
                                  temperature = input$llm_temp, ckpt_dir = ckpt_dir())
      parquet_write(docs_en, ckpt_path(ckpt_dir(), "docs_enriched.parquet"))
      rv$docs_enriched <- docs_en
    })
    
    output$cats_enriched_preview <- renderDT({
      req(rv$cats_enriched)
      datatable(head(rv$cats_enriched[, .(id, name, description,
                                          seeds = vapply(seed_terms, function(x) paste(x, collapse=", "), ""),
                                          keywords = vapply(keywords, function(x) paste(x, collapse=", "), ""),
                                          enriched_text, agg_enriched_text)], 20),
                options = list(scrollX=TRUE, pageLength=10))
    })
    output$docs_enriched_preview <- renderDT({
      req(rv$docs_enriched)
      datatable(head(rv$docs_enriched[, .(id, keywords = vapply(keywords, function(x) paste(x, collapse=", "), ""),
                                          enriched_text)], 20),
                options = list(scrollX=TRUE, pageLength=10))
    })
    
    list(
      docs_enriched = reactive(rv$docs_enriched),
      cats_enriched = reactive(rv$cats_enriched)
    )
  })
}
