# R/utils_models.R
model_registry <- new.env(parent = emptyenv())

register_model <- function(key, fit_fn, predict_fn, label = key) {
  assign(key, list(fit=fit_fn, predict=predict_fn, label=label), envir = model_registry)
}
get_model <- function(key) {
  m <- mget(key, envir = model_registry, ifnotfound = list(NULL))[[1]]
  if (is.null(m)) stop("Unknown model key: ", key)
  m
}
available_models <- function() vapply(ls(model_registry), function(k) get_model(k)$label, character(1))

# TF-IDF + LLM enrichment model
fit_tfidf_llm <- function(docs_dt, schema_dt, language="en", ...) {
  list(language=language,
       docs_dt=docs_dt[, .(id, enriched_text)],
       schema_dt=schema_dt[, .(category_id=id, agg_enriched_text)])
}
predict_tfidf_llm <- function(model, top_k=5L, threshold=0) {
  sim <- tfidf_similarity(model$docs_dt$enriched_text,
                          model$schema_dt$agg_enriched_text,
                          language = model$language)
  score_dt <- scores_long_dt(sim, model$docs_dt$id, model$schema_dt$category_id, top_k = top_k, threshold = threshold)
  score_dt
}
register_model("tfidf_llm_enrich", fit_tfidf_llm, predict_tfidf_llm, "TF-IDF + LLM Enrichment")
