# R/modules-data.R
data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Input data"),
                 radioButtons(ns("data_source"), "Choose a data source:", inline=FALSE,
                              choices = c("Upload file"="upload", "Demo: AG's News (textdata)"="agnews"),
                              selected = "agnews"),
                 conditionalPanel(sprintf("input['%s'] == 'upload'", ns("data_source")),
                                  fileInput(ns("file_upload"), "Upload CSV or Parquet",
                                            accept=c(".csv",".parquet",".parq",".pq"), multiple = FALSE),
                                  awesomeCheckbox(ns("has_header"),"CSV has header", TRUE),
                                  textInput(ns("csv_sep"),"CSV separator (ignored for Parquet)", ",")
                 ),
                 conditionalPanel(sprintf("input['%s'] == 'agnews'", ns("data_source")),
                                  helpText("Loads AG's News dataset via {textdata} with columns: class, title, description."),
                                  awesomeCheckbox(ns("auto_download_demo"), "Auto-download demo if missing", TRUE)
                 ),
                 hr(),
                 h4("Schema (categories)"),
                 fileInput(ns("schema_upload"), "Upload category schema (CSV/Parquet).",
                           accept=c(".csv",".parquet",".parq",".pq"), multiple = FALSE),
                 helpText("Expected columns: category_id (or id), name, description, optional parent_id for hierarchy."),
                 hr(),
                 h4("Feature selection & labels"),
                 uiOutput(ns("text_columns_ui")),
                 textInput(ns("id_column"), "ID column name (if present)", value = ""),
                 textInput(ns("label_column"), "Human label column (optional)", value = ""),
                 hr(),
                 h4("Language"),
                 textInput(ns("target_lang"), "Target language for enrichment (e.g., 'en', 'nl')", value = "en"),
                 awesomeCheckbox(ns("auto_detect_lang"), "Auto-detect language of input text (best-effort)", FALSE),
                 hr(),
                 h4("Storage"),
                 textInput(ns("ckpt_dir"), "Checkpoint directory (Parquet cache)", value = default_ckpt_dir),
                 hr(),
                 h4("Sampling"),
                 awesomeCheckbox(ns("enable_sample"), "Use a sample of the data", TRUE),
                 radioButtons(ns("sample_mode"), "Sample by", choices = c("N rows"="n","Fraction"="frac", "Per category (N per class)"="perclass"), inline=TRUE),
                 numericInput(ns("sample_n"),    "Sample size (N)", value = 5000, min = 100, step = 100),
                 numericInput(ns("sample_n_perclass"), "N per class", value = 1000, min = 10, step = 10),
                 sliderInput(ns("sample_frac"),  "Sample fraction", min=0.01, max=1, value=0.10, step=0.01),
                 awesomeCheckbox(ns("sample_stratify"), "Stratify by label (if available)", TRUE),
                 numericInput(ns("sample_seed"), "Random seed", value = 42, min = 1, step = 1),
                 actionButton(ns("btn_apply_sample"), "Apply sample")
    ),
    mainPanel(
      h4("Preview"),
      DTOutput(ns("data_preview")),
      div(class="small-note", uiOutput(ns("sample_info"))),
      hr(),
      h4("Schema Preview"),
      DTOutput(ns("schema_preview"))
    )
  )
}

data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      data_raw_full = NULL, # full dataset as loaded (pre-sample)
      data_raw      = NULL, # active dataset (post-sample)
      schema_raw    = NULL,
      docs          = NULL,
      labels        = NULL,
      language      = "en"
    )
    
    # --------------------- helpers: sampling ---------------------
    sample_dt <- function(dt, n, label_col = NULL, seed = 42) {
      set.seed(seed)
      N <- nrow(dt)
      n <- max(1L, min(as.integer(n), N))
      if (!is.null(label_col) && nzchar(label_col) && (label_col %in% names(dt))) {
        tab <- dt[, .(cnt = .N), by = .(grp = get(label_col))]
        setnames(tab, "grp", label_col)
        tab[, prop := cnt / sum(cnt)]
        tab[, q := prop * n]
        tab[, take := pmax(0L, floor(q))]
        total <- sum(tab$take)
        if (total < n) {
          tab[, leftover := q - take]
          ord <- order(tab$leftover, decreasing = TRUE)
          add <- n - total
          if (add > 0) tab$take[ord[seq_len(min(add, nrow(tab)))]] <- tab$take[ord[seq_len(min(add, nrow(tab)))]] + 1L
        } else if (total > n) {
          tab[, leftover := q - take]
          ord <- order(tab$leftover, decreasing = FALSE)
          rem <- total - n
          idx <- ord[tab$take[ord] > 0][seq_len(min(rem, sum(tab$take > 0)))]
          tab$take[idx] <- tab$take[idx] - 1L
        }
        parts <- vector("list", nrow(tab))
        for (i in seq_len(nrow(tab))) {
          k <- tab$take[i]; if (k <= 0) next
          rows <- dt[get(label_col) == tab[[label_col]][i]]
          parts[[i]] <- if (k >= nrow(rows)) rows else rows[sample(.N, k)]
        }
        out <- rbindlist(parts, use.names = TRUE, fill = TRUE)
        if (!is.null(out) && nrow(out)) out else dt[sample(.N, n)]
      } else {
        if (n >= N) return(copy(dt))
        dt[sample(.N, n)]
      }
    }
    
    # --- helper: sample exactly up to n_per per class (no oversampling) ---
    sample_per_class <- function(dt, label_col, n_per, seed = 42L) {
      stopifnot(is.character(label_col), length(label_col) == 1L, nzchar(label_col))
      if (!label_col %in% names(dt)) stop("Label column '", label_col, "' not found in data.")
      set.seed(seed)
      # IMPORTANT: pass character column name directly to by=  (portable across data.table versions)
      dt[, .SD[sample(.N, min(.N, as.integer(n_per)))], by = c(label_col)]
    }
    
    # --- main sampling dispatcher (call this from your observeEvent) ---
    apply_sampling <- function() {
      full <- rv$data_raw_full
      if (is.null(full)) return()
      
      # Disabled? Use full data
      if (!isTRUE(input$enable_sample)) { rv$data_raw <- copy(full); return() }
      
      # Determine label column (user-provided or demo fallback)
      label_col <- input$label_column
      if (!nzchar(label_col) || !(label_col %in% names(full))) {
        label_col <- if ("class" %in% names(full)) "class" else NULL
      }
      
      # --- Mode: Per category (N per class) ---
      if (identical(input$sample_mode, "perclass")) {
        if (is.null(label_col)) {
          showNotification("Per-category sampling requires a label column (or 'class' in the demo). Falling back to simple N sample.", type="warning")
          # fall back to simple N sampling
          N <- nrow(full)
          n_target <- as.integer(input$sample_n)
          rv$data_raw <- if (n_target >= N) copy(full) else full[sample(.N, n_target)]
          return()
        }
        n_per <- as.integer(input$sample_n_perclass)
        sampled <- sample_per_class(full, label_col = label_col, n_per = n_per, seed = input$sample_seed)
        # data.table returns the grouping column as the first column if it wasn't already there; keep as-is
        rv$data_raw <- as.data.table(sampled)
        return()
      }
      
      # --- Mode: Fraction or N (optionally stratified) ---
      N <- nrow(full)
      if (identical(input$sample_mode, "frac")) {
        n_target <- max(1L, floor(N * as.numeric(input$sample_frac)))
      } else {
        n_target <- as.integer(input$sample_n)
      }
      
      # Existing stratified sampler (uses proportional allocation without oversampling)
      if (isTRUE(input$sample_stratify) && !is.null(label_col)) {
        rv$data_raw <- sample_dt(full, n = n_target, label_col = label_col, seed = input$sample_seed)
      } else {
        rv$data_raw <- if (n_target >= N) copy(full) else full[sample(.N, n_target)]
      }
    }
    
    observeEvent(input$btn_apply_sample,  ignoreInit = TRUE, { apply_sampling() })
    
    # --------------------- reset on source change ---------------------
    observeEvent(input$data_source, ignoreInit = TRUE, {
      rv$data_raw_full <- NULL
      rv$data_raw      <- NULL
      rv$docs          <- NULL
      rv$labels        <- NULL
      rv$schema_raw    <- NULL
    })
    
    # --------------------- Demo: AG's News (auto-download) ---------------------
    observeEvent(input$data_source, {
      if (input$data_source == "agnews") {
        ok <- requireNamespace("textdata", quietly = TRUE)
        if (!ok) {
          showNotification("Please install the 'textdata' package for the AG's News demo.", type="error")
          return(NULL)
        }
        # Try to load; if missing, optionally auto-download
        dt <- tryCatch({
          textdata::dataset_ag_news(split = "train")
        }, error = function(e) {
          if (isTRUE(input$auto_download_demo)) {
            showNotification("AG's News not found in cache. Attempting download to your {textdata} cache...", type = "message")
            op <- options(textdata.ask = FALSE) # avoid interactive prompt if used by textdata
            on.exit(options(op), add = TRUE)
            # second attempt should trigger download
            textdata::dataset_ag_news(split = "train")
          } else {
            stop(e)
          }
        })
        # Ensure plain character columns
        for (col in intersect(c("class","title","description"), names(dt))) {
          dt[[col]] <- as.character(dt[[col]])
        }
        dt <- as.data.table(dt)
        dt[, id := sprintf("ag_%06d", .I)]
        setcolorder(dt, c("id", setdiff(names(dt), "id")))
        rv$data_raw_full <- dt
        # Minimal schema from classes if none uploaded
        if ("class" %in% names(dt)) {
          sch <- unique(dt[, .(category_id = as.character(class))])
          sch[, `:=`(name=category_id, description=category_id, parent_id=NA_character_)]
          rv$schema_raw <- sch
        }
        # Auto-apply sampling on load
        apply_sampling()
      }
    })
    
    # --------------------- Upload data ---------------------
    observeEvent(input$file_upload, {
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$name)
      dt <- switch(tolower(ext),
                   "csv"     = fread(input$file_upload$datapath, header = isTRUE(input$has_header), sep = input$csv_sep, quote="\"", encoding="UTF-8"),
                   "parquet" = as.data.table(read_parquet(input$file_upload$datapath)),
                   "parq"    = as.data.table(read_parquet(input$file_upload$datapath)),
                   "pq"      = as.data.table(read_parquet(input$file_upload$datapath)),
                   stop("Unsupported file type.")
      )
      if (!nzchar(input$id_column) || !input$id_column %in% names(dt)) {
        dt[, id := sprintf("doc_%06d", .I)]
      } else setnames(dt, input$id_column, "id")
      rv$data_raw_full <- as.data.table(dt)
      apply_sampling()
    })
    
    # --------------------- Upload schema ---------------------
    observeEvent(input$schema_upload, {
      req(input$schema_upload)
      ext <- tools::file_ext(input$schema_upload$name)
      dt <- switch(tolower(ext),
                   "csv"     = fread(input$schema_upload$datapath),
                   "parquet" = as.data.table(read_parquet(input$schema_upload$datapath)),
                   "parq"    = as.data.table(read_parquet(input$schema_upload$datapath)),
                   "pq"      = as.data.table(read_parquet(input$schema_upload$datapath)),
                   stop("Unsupported schema file type.")
      )
      nm <- names(dt)
      if ("id" %in% nm && !"category_id" %in% nm) setnames(dt, "id", "category_id")
      req_cols <- c("category_id","name","description")
      missing <- setdiff(req_cols, names(dt))
      if (length(missing)) {
        showNotification(paste("Schema missing columns:", paste(missing, collapse=", ")), type="error")
        return(NULL)
      }
      if (!"parent_id" %in% names(dt)) dt[, parent_id := NA_character_]
      rv$schema_raw <- as.data.table(dt)
    })
    
    # --------------------- Feature selection UI ---------------------
    output$text_columns_ui <- renderUI({
      req(rv$data_raw)
      cols <- setdiff(names(rv$data_raw), "id")
      # Pick robust defaults: prefer title+description, else text-like columns, else first column.
      defaults <- intersect(cols, c("title","description","text","content","message","body"))
      sel <- if (length(defaults)) defaults else head(cols, 1)
      selectizeInput(ns("text_columns"), "Text columns to classify (concatenated):",
                     choices = cols, selected = sel,
                     multiple = TRUE, options = list(plugins=list("remove_button")))
    })
    
    # --------------------- Compose docs & labels (robust against 0/1 columns) ---------------------
    observe({
      req(rv$data_raw)
      # Do not proceed until user has a non-empty selection
      if (is.null(input$text_columns) || !length(input$text_columns)) return()
      
      dt <- copy(rv$data_raw)
      
      # Labels (optional); for demo fall back to 'class' if user didn't set label_column
      if (nzchar(input$label_column) && input$label_column %in% names(dt)) {
        rv$labels <- dt[, .(id, label = .SD[[input$label_column]])]
      } else if ("class" %in% names(dt)) {
        rv$labels <- dt[, .(id, label = class)]
      } else {
        rv$labels <- NULL
      }
      
      cols <- intersect(input$text_columns, names(dt))
      if (!length(cols)) {
        showNotification("Please select at least one valid text column.", type="error")
        return()
      }
      
      # Safe row-wise concatenation with NA removal
      if (length(cols) == 1L) {
        txt <- as.character(dt[[cols]])
      } else {
        subdf <- as.data.frame(dt[, ..cols], stringsAsFactors = FALSE, check.names = FALSE)
        # apply() needs a matrix/data frame with positive dims
        if (nrow(subdf) == 0L) {
          txt <- character()
        } else {
          txt <- apply(subdf, 1, function(row) paste(na.omit(row), collapse=" | "))
        }
      }
      
      rv$docs <- data.table(id = dt$id, text = txt)
    })
    
    # --------------------- Language (manual + autodetect) ---------------------
    language <- reactive({
      lang <- norm_lang(input$target_lang)
      if (isTRUE(input$auto_detect_lang) && !is.null(rv$docs) && nrow(rv$docs)) {
        sample_txt <- rv$docs$text[seq_len(min(100, nrow(rv$docs)))]
        det <- detect_language(sample_txt)
        if (length(det) && is.character(det)) {
          det_tbl <- sort(table(det), decreasing=TRUE)
          lang <- norm_lang(names(det_tbl)[1])
        }
      }
      rv$language <- lang
      lang
    })
    
    # --------------------- Previews & sampling info ---------------------
    # Data preview (show ALL rows with virtual scrolling)
    output$data_preview <- renderDT({
      req(rv$data_raw)
      datatable(
        rv$data_raw,
        options = list(
          deferRender = TRUE,
          scrollX = TRUE,
          scrollY = "60vh",
          scroller = TRUE,
          dom = "tip",          # table + info + pagination (minimal UI)
          pageLength = 50
        ),
        filter = "top",
        rownames = FALSE,
        extensions = "Scroller"
      )
    }, server = TRUE)
    
    # Schema preview (usually small; still enable horizontal scroll)
    output$schema_preview <- renderDT({
      req(rv$schema_raw)
      datatable(
        rv$schema_raw,
        options = list(scrollX = TRUE, pageLength = 50),
        rownames = FALSE
      )
    }, server = TRUE)
    output$sample_info <- renderUI({
      full_n <- if (!is.null(rv$data_raw_full)) nrow(rv$data_raw_full) else NA_integer_
      cur_n  <- if (!is.null(rv$data_raw)) nrow(rv$data_raw) else NA_integer_
      mode_txt <- if (isTRUE(input$enable_sample)) {
        if (identical(input$sample_mode,"frac")) {
          paste0("sampling fraction = ", input$sample_frac, ", seed = ", input$sample_seed)
        } else if (identical(input$sample_mode,"perclass")) {
          paste0("N per class = ", input$sample_n_perclass, ", seed = ", input$sample_seed)
        } else {
          paste0("sampling N = ", input$sample_n, ", seed = ", input$sample_seed)
        }
      } else "sampling disabled"
      HTML(sprintf("<em>Active rows:</em> <b>%s</b> &nbsp; | &nbsp; <em>Full rows:</em> %s &nbsp; | &nbsp; <em>Mode:</em> %s",
                   format(cur_n, big.mark=","), format(full_n, big.mark=","), mode_txt))
    })
    
    # --------------------- Expose to other modules ---------------------
    list(
      docs     = reactive(rv$docs),
      labels   = reactive(rv$labels),
      schema   = reactive(rv$schema_raw),
      language = language,
      ckpt_dir = reactive(input$ckpt_dir)
    )
  })
}
