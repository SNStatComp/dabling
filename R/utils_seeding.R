# R/utils_seeding.R
# Improved seed derivation from labeled data:
# - strict cleaning (prep_text_strict)
# - extra news stopwords
# - binary features, DF filters, global DF cap
# - log-odds with informative prior
# - sharedness filtering across classes

news_extra_stopwords <- function(language = "en") {
  days    <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
  months  <- tolower(month.name)
  wires   <- c("reuters","ap","afp","upi","associated","press","news","report","reports","reported")
  speech  <- c("said","says","say","told","according")
  company <- c("inc","corp","co","ltd","plc","company","group")
  misc    <- c("u","s","us","u.s","u_s","lt","gt","quot","amp","new","year","month","today","yesterday","tomorrow")
  unique(c(days, months, wires, speech, company, misc, stopwords::stopwords(language)))
}

derive_seed_terms <- function(
    docs_dt, labels_dt, text_col = "text", label_col = "label",
    language = "en", top_m = 15L, min_df = 5L, max_global_df = 0.40,
    ngram = c(1L,2L), min_token_len = 3L, extra_stop = TRUE, drop_shared_ge = 2L
) {
  stopifnot(all(c("id", text_col) %in% names(docs_dt)), all(c("id", label_col) %in% names(labels_dt)))
  dt <- merge(
    docs_dt[, .(id, text = get(text_col))],
    labels_dt[, .(id, label = as.character(get(label_col)))],
    by = "id", all.x = FALSE
  )
  if (!nrow(dt)) return(data.table(category_id=character(), seed_terms=I(list())))
  
  dt[, text := prep_text_strict(text)]
  tok_fun <- text2vec::word_tokenizer
  it0 <- text2vec::itoken(dt$text, tokenizer = tok_fun, progressbar = FALSE)
  stopw <- if (isTRUE(extra_stop)) news_extra_stopwords(language) else stopwords::stopwords(language)
  
  # small helper to build DTM with given pruning
  build_dtm <- function(doc_prop_max, min_df_keep) {
    vocab <- text2vec::create_vocabulary(it0, stopwords = stopw, ngram = ngram)
    if (nrow(vocab)) {
      keep <- nchar(vocab$term) >= min_token_len & !grepl("\\d", vocab$term)
      whitelist <- c("us","uk","eu","ai")
      keep <- keep | vocab$term %in% whitelist
      vocab <- vocab[keep, ]
    }
    if (!nrow(vocab)) return(NULL)
    vocab <- text2vec::prune_vocabulary(
      vocab,
      term_count_min = 1, doc_proportion_min = 0,
      doc_proportion_max = doc_prop_max, vocab_term_max = 100000L
    )
    if (!nrow(vocab)) return(NULL)
    vect <- text2vec::vocab_vectorizer(vocab)
    dtm  <- text2vec::create_dtm(text2vec::itoken(dt$text, tokenizer = tok_fun, progressbar = FALSE), vect)
    if (nrow(dtm) == 0 || ncol(dtm) == 0) return(NULL)
    # binary presence
    if (length(dtm@x)) dtm@x[] <- 1
    # min df filter
    df <- Matrix::colSums(dtm)
    keep <- df >= as.integer(min_df_keep)
    if (!any(keep)) return(NULL)
    dtm[, keep, drop = FALSE]
  }
  
  # Try strict
  dtm_bin <- build_dtm(doc_prop_max = max_global_df, min_df_keep = min_df)
  
  # Fallback 1: relax thresholds
  if (is.null(dtm_bin)) dtm_bin <- build_dtm(doc_prop_max = 1, min_df_keep = 1)
  
  # Fallback 2: still NULL -> naive top tokens per class from raw tokens
  if (is.null(dtm_bin)) {
    lbls <- unique(dt$label)
    out <- lapply(lbls, function(cl) {
      tx <- dt[label == cl, text]
      if (!length(tx)) return(character())
      toks <- unlist(tok_fun(tx), use.names = FALSE)
      toks <- toks[!(toks %in% stopw)]
      toks <- toks[nchar(toks) >= min_token_len & !grepl("\\d", toks)]
      head(names(sort(table(toks), decreasing = TRUE)), top_m)
    })
    seeds <- data.table(category_id = lbls, seed_terms = I(out))
    # drop shared terms if requested
    if (is.finite(drop_shared_ge) && drop_shared_ge > 1L) {
      freq <- table(unlist(seeds$seed_terms, use.names = FALSE))
      blacklist <- names(freq[freq >= drop_shared_ge])
      for (i in seq_len(nrow(seeds))) {
        seeds$seed_terms[[i]] <- setdiff(seeds$seed_terms[[i]], blacklist)
      }
    }
    return(seeds)
  }
  
  lbl <- dt$label
  cats <- sort(unique(lbl))
  n_docs <- nrow(dtm_bin)
  df_all <- Matrix::colSums(dtm_bin)
  p <- as.numeric(df_all / n_docs)
  alpha0 <- 1.0
  prior_success <- alpha0 * p
  prior_fail    <- alpha0 * (1 - p)
  
  terms <- colnames(dtm_bin)
  out <- vector("list", length(cats)); names(out) <- cats
  for (i in seq_along(cats)) {
    c <- cats[i]
    idx_c <- which(lbl == c)
    idx_r <- which(lbl != c)
    if (length(idx_c) < 2L || length(idx_r) < 2L) {
      y1 <- Matrix::colSums(dtm_bin[idx_c, , drop=FALSE])
      ord <- order(y1, decreasing = TRUE)
      out[[c]] <- terms[head(ord, top_m)]
      next
    }
    y1 <- Matrix::colSums(dtm_bin[idx_c, , drop=FALSE])
    y2 <- Matrix::colSums(dtm_bin[idx_r, , drop=FALSE])
    y1a <- y1 + prior_success
    y2a <- y2 + prior_success
    f1a <- (length(idx_c) - y1) + prior_fail
    f2a <- (length(idx_r) - y2) + prior_fail
    logodds1 <- log(y1a / f1a)
    logodds2 <- log(y2a / f2a)
    delta <- as.numeric(logodds1 - logodds2)
    var   <- 1 / y1a + 1 / y2a
    z     <- delta / sqrt(var)
    z[!is.finite(z)] <- -Inf
    ord <- order(z, decreasing = TRUE)
    out[[c]] <- terms[head(ord, top_m * 2L)]
    out[[c]] <- unique(out[[c]])[seq_len(min(top_m, length(out[[c]])))]
  }
  
  seeds <- data.table(category_id = names(out), seed_terms = I(unname(out)))
  if (is.finite(drop_shared_ge) && drop_shared_ge > 1L) {
    freq <- table(unlist(seeds$seed_terms, use.names = FALSE))
    blacklist <- names(freq[freq >= drop_shared_ge])
    for (i in seq_len(nrow(seeds))) {
      seeds$seed_terms[[i]] <- setdiff(seeds$seed_terms[[i]], blacklist)
      if (length(seeds$seed_terms[[i]]) > top_m) {
        seeds$seed_terms[[i]] <- seeds$seed_terms[[i]][seq_len(top_m)]
      }
    }
  }
  seeds
}