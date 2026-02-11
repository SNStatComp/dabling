# app.R
source("R/packages.R")
source("R/utils_storage.R")
source("R/utils_text.R")
source("R/utils_openai.R")
source("R/utils_hierarchy.R")
source("R/utils_vectorize.R")
source("R/utils_metrics.R")
source("R/utils_models.R")
source("R/utils_seeding.R")
source("R/modules-data.R")
source("R/modules-enrichment.R")
source("R/modules-scoring.R")
source("R/modules-settings.R")

# ---- Config laden ----
config <- NULL
config_path <- file.path("config", "config.yml")

if (file.exists(config_path)) {
  config <- yaml::read_yaml(config_path)
} else {
  warning("config.yml niet gevonden in ./config – OpenAI-config wordt overgeslagen.")
}

# OpenAI API key uit config → environment variable
if (!is.null(config$openai$api_key) && nzchar(config$openai$api_key)) {
  Sys.setenv(OPENAI_API_KEY = config$openai$api_key)
}

# Standaard model voor zoekverrijking
if (!is.null(config$openai$model) && nzchar(config$openai$model)) {
  options(dabling_search_llm = config$openai$model)
}


options(AUTOCLASSIFY_LLM_DEBUG = TRUE)

ui <- navbarPage(
  title = "Auto-Classification Prototype",
  id = "mainnav",
  tabPanel("Data",      data_ui("data")),
  tabPanel("Enrichment",enrichment_ui("enrich")),
  tabPanel("Scoring",   scoring_ui("score")),
  tabPanel("Settings",  settings_ui("settings"))
)

server <- function(input, output, session) {
  data_res <- data_server("data")
  enrich_res <- enrichment_server("enrich",
                                  docs     = data_res$docs,
                                  schema   = data_res$schema,
                                  labels   = data_res$labels,  
                                  language = data_res$language,
                                  ckpt_dir = data_res$ckpt_dir)
  scoring_res <- scoring_server("score",
                                docs_enriched = enrich_res$docs_enriched,
                                cats_enriched = enrich_res$cats_enriched,
                                labels        = data_res$labels,
                                language      = data_res$language,
                                ckpt_dir      = data_res$ckpt_dir)
  settings_server("settings",
                  docs_enriched = enrich_res$docs_enriched,
                  cats_enriched = enrich_res$cats_enriched,
                  scores        = scoring_res$scores,
                  ckpt_dir      = data_res$ckpt_dir)
}

shinyApp(ui, server)
