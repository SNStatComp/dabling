# R/utils_vectorize.R
tfidf_similarity <- function(docs_text, cats_text, language="en", vocab_prune_max_terms = 50000L) {
  stopw <- tryCatch(stopwords::stopwords(language), error=function(e) character())
  tok_fun <- text2vec::word_tokenizer
  it_docs <- itoken(docs_text, tokenizer = tok_fun, progressbar = FALSE)
  it_cats <- itoken(cats_text, tokenizer = tok_fun, progressbar = FALSE)
  it_all  <- itoken(c(docs_text, cats_text), tokenizer = tok_fun, progressbar = FALSE)
  vocab <- create_vocabulary(it_all, stopwords = stopw)
  vocab <- prune_vocabulary(vocab, term_count_min = 1, doc_proportion_min = 0, doc_proportion_max = 1,
                            vocab_term_max = vocab_prune_max_terms)
  vectorizer <- vocab_vectorizer(vocab)
  dtm_docs <- create_dtm(it_docs, vectorizer)
  dtm_cats <- create_dtm(it_cats, vectorizer)
  dtm_all <- rbind(dtm_docs, dtm_cats)
  tfidf <- TfIdf$new()
  dtm_all_tfidf <- tfidf$fit_transform(dtm_all)
  nr_docs <- nrow(dtm_docs)
  dtm_docs_tfidf <- dtm_all_tfidf[seq_len(nr_docs), ]
  dtm_cats_tfidf <- dtm_all_tfidf[(nr_docs+1):nrow(dtm_all_tfidf), ]
  l2 <- function(m) {
    rs <- sqrt(Matrix::rowSums(m^2))
    rs[rs == 0] <- 1
    Diagonal(x = 1/rs) %*% m
  }
  A <- l2(dtm_docs_tfidf); B <- l2(dtm_cats_tfidf)
  A %*% Matrix::t(B)
}

scores_long_dt <- function(sim_mat, doc_ids, cat_ids, top_k = 5L, threshold = 0) {
  stopifnot(length(doc_ids) == nrow(sim_mat), length(cat_ids) == ncol(sim_mat))
  res_list <- vector("list", nrow(sim_mat))
  for (i in seq_len(nrow(sim_mat))) {
    row <- sim_mat[i, ]
    idx <- which(row > threshold)
    vals <- as.numeric(row[idx])
    ord <- order(vals, decreasing = TRUE)
    if (length(ord) > top_k) ord <- ord[seq_len(top_k)]
    if (length(ord)) {
      res_list[[i]] <- data.table(
        id = doc_ids[i],
        category_id = cat_ids[idx[ord]],
        score = vals[ord],
        rank = seq_along(ord)
      )
    } else {
      res_list[[i]] <- data.table(id = doc_ids[i], category_id = NA_character_, score = NA_real_, rank = NA_integer_)
    }
  }
  rbindlist(res_list, use.names = TRUE, fill = TRUE)
}
