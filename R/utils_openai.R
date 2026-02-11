# Harvest ALL character leaves anywhere in the response object.
.collect_character_leaves <- function(x, max_n = 2000) {
  out <- character()
  walk <- function(z) {
    if (is.character(z)) {
      out <<- c(out, z)
      if (length(out) >= max_n) return(invisible())  # simple safety cap
    } else if (is.list(z)) {
      for (w in z) walk(w)
    }
    invisible()
  }
  walk(x)
  # Keep unique, non-empty; prefer strings that look like JSON
  out <- unique(out[nzchar(out)])
  jsonish <- grep("\\{|\\[", out, value = TRUE)
  if (length(jsonish)) jsonish else out
}


# --- helpers (place near the top of utils_openai.R) ---
.test_openai_json <- function() {
  pr <- "You produce 5 keywords. Return {\"keywords\":[\"...\"]} only."
  ans <- openai_call_json(
    prompt = pr,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    model = dabling_search_llm,
    wrap_array_key = "keywords",
    debug = TRUE,
    debug_log_dir = getwd(),
    debug_tag = "smoketest",
    use_responses_api = FALSE
  )
  print(ans)
}

.extract_text_candidates <- function(res) {
  cands <- character()
  add <- function(x) {
    if (is.null(x)) return(invisible())
    if (is.character(x) && length(x)) {
      x <- x[nzchar(x)]
      if (length(x)) cands <<- c(cands, x)
    }
    invisible()
  }
  add_json <- function(obj) {
    if (!is.null(obj)) {
      txt <- tryCatch(jsonlite::toJSON(obj, auto_unbox = TRUE), error = function(e) NULL)
      add(txt)
    }
  }
  
  # Responses API
  add(tryCatch(res$output_text, error = function(e) NULL))
  out <- tryCatch(res$output, error = function(e) NULL)
  if (!is.null(out) && length(out)) {
    for (blk in out) {
      cont <- tryCatch(blk$content, error = function(e) NULL)
      if (!is.null(cont) && length(cont)) {
        for (item in cont) {
          add(tryCatch(item$text,       error = function(e) NULL))
          add(tryCatch(item$input_text, error = function(e) NULL))
          add_json(tryCatch(item$json,  error = function(e) NULL))
        }
      }
    }
  }
  add(tryCatch(res$response, error = function(e) NULL))
  
  # Chat completions
  add(tryCatch(res$choices[[1]]$message$content, error = function(e) NULL))
  add_json(tryCatch(res$choices[[1]]$message$parsed, error = function(e) NULL))
  
  # Chat tools + legacy function_call
  tc <- tryCatch(res$choices[[1]]$message$tool_calls, error = function(e) NULL)
  if (!is.null(tc) && length(tc)) {
    for (tci in tc) {
      args <- tryCatch(tci[["function"]][["arguments"]], error = function(e) NULL)
      add(args)
    }
  }
  fc <- tryCatch(res$choices[[1]]$message$`function_call`, error = function(e) NULL)
  if (!is.null(fc)) add(tryCatch(fc$arguments, error = function(e) NULL))
  
  unique(cands)
}

.best_effort_parse_json <- function(txt, wrap_array_key = NULL) {
  if (!is.character(txt) || !nzchar(txt)) return(NULL)
  s <- gsub("^\\s*```(json)?\\s*|\\s*```\\s*$", "", txt)  # strip code fences
  s <- trimws(s)
  # If model returned a bare JSON array, wrap it
  if (!is.null(wrap_array_key) && grepl("^\\s*\\[", s)) {
    s <- sprintf("{\"%s\":%s}", wrap_array_key, s)
  }
  # direct parse
  ok <- tryCatch(jsonlite::validate(s), error=function(e) FALSE)
  if (isTRUE(ok)) return(jsonlite::fromJSON(s, simplifyVector = TRUE))
  # try to extract largest {...} block
  m <- regexpr("\\{[\\s\\S]*\\}", s, perl = TRUE)
  if (m[1] != -1) {
    sub <- substr(s, m[1], m[1] + attr(m, "match.length") - 1)
    ok2 <- tryCatch(jsonlite::validate(sub), error=function(e) FALSE)
    if (isTRUE(ok2)) return(jsonlite::fromJSON(sub, simplifyVector = TRUE))
  }
  NULL
}

# --- robust OpenAI call that enforces JSON and has multiple fallbacks ---
openai_call_json <- function(
    prompt,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    model = dabling_search_llm,
    temperature = 0.0,
    max_tokens = 300,
    use_responses_api = FALSE,   # Chat first; Responses if asked
    request_timeout = 120,
    wrap_array_key = NULL,
    debug = FALSE,
    debug_log_dir = NULL,
    debug_tag = NULL
) {
  if (!nzchar(api_key)) stop("OpenAI API key not set. Provide in Enrichment tab or set OPENAI_API_KEY.")
  if (!debug) debug <- isTRUE(getOption("AUTOCLASSIFY_LLM_DEBUG", FALSE))
  
  kw_schema <- list(
    type="object",
    properties=list(keywords=list(type="array", items=list(type="string"))),
    required=list("keywords"),
    additionalProperties=FALSE
  )
  
  .dump <- function(obj, suffix) {
    if (!debug || is.null(debug_log_dir)) return(invisible())
    dir.create(debug_log_dir, FALSE, TRUE)
    tag <- if (is.null(debug_tag)) format(Sys.time(), "%Y%m%d_%H%M%S") else debug_tag
    p <- file.path(debug_log_dir, sprintf("openai_%s_%s.json", tag, suffix))
    txt <- tryCatch(jsonlite::toJSON(obj, auto_unbox=TRUE, pretty=TRUE), error=function(e) NULL)
    if (!is.null(txt)) try(suppressWarnings(writeLines(txt, p)), silent=TRUE)
  }
  
  .best_effort_parse_json <- function(txt, wrap_array_key = NULL) {
    if (!is.character(txt) || !nzchar(txt)) return(NULL)
    s <- gsub("^\\s*```(json)?\\s*|\\s*```\\s*$", "", txt)
    s <- trimws(s)
    if (!is.null(wrap_array_key) && grepl("^\\s*\\[", s)) s <- sprintf("{\"%s\":%s}", wrap_array_key, s)
    ok <- tryCatch(jsonlite::validate(s), error=function(e) FALSE)
    if (isTRUE(ok)) return(jsonlite::fromJSON(s, simplifyVector = TRUE))
    m <- regexpr("\\{[\\s\\S]*\\}", s, perl = TRUE)
    if (m[1] != -1) {
      sub <- substr(s, m[1], m[1] + attr(m, "match.length") - 1)
      ok2 <- tryCatch(jsonlite::validate(sub), error=function(e) FALSE)
      if (isTRUE(ok2)) return(jsonlite::fromJSON(sub, simplifyVector = TRUE))
    }
    NULL
  }
  
  .parse_first <- function(cands, res_for_dump = NULL) {
    if (debug) {
      message(sprintf("[openai_call_json] %d candidate blocks", length(cands)))
      for (i in seq_len(min(3, length(cands)))) {
        snip <- substr(gsub("[\r\n]", " ", cands[[i]]), 1, 600)
        message(sprintf("candidate[%d] >>> %s", i, snip))
      }
      if (!is.null(res_for_dump)) .dump(list(candidates=cands), "candidates")
    }
    if (!length(cands)) return(NULL)
    for (cand in cands) {
      parsed <- .best_effort_parse_json(cand, wrap_array_key = wrap_array_key)
      if (!is.null(parsed)) return(parsed)
    }
    NULL
  }
  
  req_headers <- list(Authorization=paste("Bearer", api_key), "Content-Type"="application/json")
  
  try_chat_tools <- function() {
    req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(!!!req_headers) |>
      httr2::req_body_json(list(
        model=model, temperature=temperature, max_tokens=max_tokens,
        tools=list(list(type="function", `function`=list(name="return_keywords", parameters=kw_schema))),
        tool_choice=list(type="function", `function`=list(name="return_keywords")),
        messages=list(
          list(role="system", content="Use the provided function to return JSON that matches the schema."),
          list(role="user",   content=prompt)
        )
      )) |>
      httr2::req_timeout(request_timeout)
    resp <- httr2::req_perform(req)
    out  <- httr2::resp_body_json(resp, simplifyVector=TRUE)
    if (debug) { .dump(out, "chat_tools_raw"); message("[openai_call_json] Chat(tools) OK") }
    cands <- .extract_text_candidates(out)
    if (!length(cands)) {
      # Deep harvest fallback
      cands <- .collect_character_leaves(out)
      if (debug && length(cands)) message("[openai_call_json] harvested candidates from deep leaves (tools)")
    }
    .parse_first(cands, out)
  }
  
  try_chat_schema <- function() {
    req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(!!!req_headers) |>
      httr2::req_body_json(list(
        model=model, temperature=temperature, max_tokens=max_tokens,
        response_format=list(type="json_schema", json_schema=list(name="keywords_schema", strict=TRUE, schema=kw_schema)),
        messages=list(
          list(role="system", content="Return strictly valid minified JSON that conforms to the schema."),
          list(role="user",   content=prompt)
        )
      )) |>
      httr2::req_timeout(request_timeout)
    resp <- httr2::req_perform(req)
    out  <- httr2::resp_body_json(resp, simplifyVector=TRUE)
    if (debug) { .dump(out, "chat_schema_raw"); message("[openai_call_json] Chat(schema) OK") }
    cands <- .extract_text_candidates(out)
    if (!length(cands)) {
      cands <- .collect_character_leaves(out)
      if (debug && length(cands)) message("[openai_call_json] harvested candidates from deep leaves (schema)")
    }
    .parse_first(cands, out)
  }
  
  try_chat_json <- function() {
    req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(!!!req_headers) |>
      httr2::req_body_json(list(
        model=model, temperature=temperature, max_tokens=max_tokens,
        response_format=list(type="json_object"),
        messages=list(
          list(role="system", content="Return strictly valid minified JSON."),
          list(role="user",   content=prompt)
        )
      )) |>
      httr2::req_timeout(request_timeout)
    resp <- httr2::req_perform(req)
    out  <- httr2::resp_body_json(resp, simplifyVector=TRUE)
    if (debug) { .dump(out, "chat_json_raw"); message("[openai_call_json] Chat(json_object) OK") }
    cands <- .extract_text_candidates(out)
    if (!length(cands)) {
      cands <- .collect_character_leaves(out)
      if (debug && length(cands)) message("[openai_call_json] harvested candidates from deep leaves (json_object)")
    }
    .parse_first(cands, out)
  }
  
  try_responses <- function() {
    req <- httr2::request("https://api.openai.com/v1/responses") |>
      httr2::req_headers(!!!req_headers) |>
      httr2::req_body_json(list(
        model=model, input=prompt, temperature=temperature, max_output_tokens=max_tokens,
        response_format=list(type="json_schema",
                             json_schema=list(name="keywords_schema", strict=TRUE, schema=kw_schema))
      )) |>
      httr2::req_timeout(request_timeout)
    resp <- httr2::req_perform(req)
    out  <- httr2::resp_body_json(resp, simplifyVector=TRUE)
    if (debug) { .dump(out, "responses_raw"); message("[openai_call_json] Responses API OK") }
    cands <- .extract_text_candidates(out)
    if (!length(cands)) {
      cands <- .collect_character_leaves(out)
      if (debug && length(cands)) message("[openai_call_json] harvested candidates from deep leaves (responses)")
    }
    .parse_first(cands, out)
  }
  
  # Try sequence:
  for (fn in list(try_chat_tools, try_chat_schema, try_chat_json,
                  if (isTRUE(use_responses_api)) try_responses else NULL)) {
    if (is.null(fn)) next
    ans <- tryCatch(fn(), error=function(e){ if (debug) message("[openai_call_json] route error: ", e$message); NULL })
    if (!is.null(ans)) return(ans)
  }
  
  stop("OpenAI API call succeeded but no text content was returned.")
}

prompt_keywords_for_category <- function(cat_name, cat_desc, target_lang="en", k=15) {
  sprintf(paste(
    "You produce domain-relevant keywords/short phrases in %s for a classification category.",
    "Category name: %s",
    "Category description: %s",
    "Return a JSON object with a single key 'keywords' mapped to a unique array of %d strings.",
    "Constraints: no explanations, no numbering, no duplicates, keep phrases concise.",
    sep="\n"),
    target_lang, cat_name, cat_desc, k
  )
}
prompt_keywords_for_text <- function(text, target_lang="en", k=12) {
  sprintf(paste(
    "You generate semantic keywords/short phrases in %s for the following text.",
    "Text: %s",
    "Return a JSON object with key 'keywords' mapped to an array of up to %d strings.",
    "Constraints: Only JSON, concise domain-relevant terms, no stopwords, no duplicates.",
    sep="\n"),
    target_lang, text, k
  )
}

cache_get <- function(h, scope, ckpt_dir) {
  p  <- ensure_llm_cache(ckpt_dir)
  dt <- parquet_read(p)
  if (!nrow(dt)) return(NULL)
  dt[, `:=`(hash = as.character(hash),
            scope = as.character(scope),
            payload = as.character(payload))]
  out <- dt[hash == h & scope == scope, payload]
  if (!length(out)) return(NULL)
  jsonlite::fromJSON(out[[1]], simplifyVector = TRUE)
}

cache_put <- function(h, scope, obj, ckpt_dir) {
  p  <- ensure_llm_cache(ckpt_dir)
  dt <- parquet_read(p)
  # Coerce existing payload to plain character (robustification)
  if ("payload" %in% names(dt)) dt[, payload := as.character(payload)]
  payload_chr <- tryCatch(as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)),
                          error = function(e) NA_character_)
  row <- data.table(hash = as.character(h),
                    scope = as.character(scope),
                    payload = payload_chr)
  # rbind with ignore.attr=TRUE to be extra safe
  out <- rbindlist(list(dt, row), use.names = TRUE, fill = TRUE, ignore.attr = TRUE)
  write_parquet(out, p)
}


enrich_categories_llm <- function(
    cats_dt, api_key, model,
    target_lang="en", k=15, temperature=0.2, ckpt_dir=default_ckpt_dir,
    seed_terms_map = NULL,        # named list: names = category ids, values = character vector seeds
    use_fewshot = TRUE,
    seed_weight = 1L,             # how many times to repeat seeds in enriched text (0 = don't inject)
    fewshot_seed_cap = 12L        # cap seeds included in prompt to save tokens
) {
  cats_dt <- copy(cats_dt)
  cats_dt[is.na(description) | !nzchar(description), description := name]
  res <- vector("list", nrow(cats_dt))
  withProgress(message = "LLM enriching categories...", value = 0, {
    for (i in seq_len(nrow(cats_dt))) {
      print(i)
      setProgress(i/nrow(cats_dt))
      nm <- cats_dt$name[i]; desc <- cats_dt$description[i]; cid <- cats_dt$id[i]
      print(nm)
      seeds <- NULL
      if (!is.null(seed_terms_map) && length(seed_terms_map) && !is.null(seed_terms_map[[cid]])) {
        seeds <- unique(head(seed_terms_map[[cid]], fewshot_seed_cap))
      }
      key <- digest(paste("cat", nm, desc, target_lang, k, model, paste(seeds, collapse="|"), use_fewshot, sep="|"), algo="xxhash64")
      cached <- cache_get(key, "category", ckpt_dir)
      if (!is.null(cached)) { res[[i]] <- cached; next }
      
      prompt <- if (use_fewshot && length(seeds)) {
        prompt_keywords_for_category_fewshot(nm, desc, target_lang, k, seed_terms = seeds)
      } else {
        prompt_keywords_for_category(nm, desc, target_lang, k)
      }
      print(prompt)
      ans <- openai_call_json(
        prompt,
        api_key = api_key,
        model = model,
        temperature = temperature,
        wrap_array_key = "keywords",
        debug = isTRUE(getOption("AUTOCLASSIFY_LLM_DEBUG", FALSE)),
        debug_log_dir = ckpt_dir,
        debug_tag = paste0("cats_", cid),
        use_responses_api = FALSE   # <— force Chat first
      )
      print(ans)
      kw <- unique(trimws(as.character(ans$keywords %||% character())))
      res[[i]] <- list(keywords = kw, seed_terms = seeds)
      cache_put(key, "category", list(keywords=kw, seed_terms=seeds), ckpt_dir)
    }
  })
  
  cats_dt[, `:=`(
    keywords   = lapply(res, function(z) z$keywords %||% character()),
    seed_terms = lapply(res, function(z) z$seed_terms %||% character())
  )]
  
  # Construct enriched_text with optional seed weighting (lightly)
  cats_dt[, enriched_text := mapply(function(nm, desc, kw, sd) {
    parts <- c(nm, desc)
    if (length(kw)) parts <- c(parts, paste(kw, collapse=" "))
    if (is.numeric(seed_weight) && seed_weight > 0 && length(sd)) {
      parts <- c(parts, rep(paste(sd, collapse=" "), seed_weight))
    }
    paste(parts, collapse=" | ")
  }, name, description, keywords, seed_terms, SIMPLIFY = TRUE)]
  
  cats_dt[]
}

enrich_texts_llm <- function(text_dt, text_col, api_key, model, target_lang="en", k=12, weight_text=3, temperature=0.2, ckpt_dir=default_ckpt_dir) {
  dt <- copy(text_dt)
  stopifnot("id" %in% names(dt))
  dt[[text_col]] <- prep_text(dt[[text_col]])
  res <- vector("list", nrow(dt))
  withProgress(message = "LLM enriching documents...", value = 0, {
    for (i in seq_len(nrow(dt))) {
      setProgress(i/nrow(dt))
      tx <- dt[[text_col]][i]
      key <- digest(paste("doc", tx, target_lang, k, model, sep="|"), algo="xxhash64")
      cached <- cache_get(key, "document", ckpt_dir)
      if (!is.null(cached)) { res[[i]] <- cached; next }
      prompt <- prompt_keywords_for_text(tx, target_lang, k)
      ans <- openai_call_json(
        prompt,
        api_key=api_key,
        model=model,
        temperature=temperature,
        wrap_array_key="keywords",
        debug = isTRUE(getOption("AUTOCLASSIFY_LLM_DEBUG", FALSE)),
        debug_log_dir = ckpt_dir,
        debug_tag = paste0("doc_", dt$id[i]),   # <-- fix
        use_responses_api = FALSE
      )
      
      kw <- unique(trimws(as.character(ans$keywords %||% character())))
      res[[i]] <- list(keywords = kw)
      cache_put(key, "document", list(keywords=kw), ckpt_dir)
    }
  })
  dt[, keywords := lapply(res, function(z) z$keywords %||% character())]
  dt[, enriched_text := mapply(function(tx, kw) paste(c(rep(tx, weight_text), if (length(kw)) paste(kw, collapse=" ")), collapse=" "), dt[[text_col]], keywords)]
  dt[]
}


prompt_keywords_for_category_fewshot <- function(cat_name, cat_desc, target_lang="en", k=15, seed_terms=NULL) {
  seeds_txt <- if (!is.null(seed_terms) && length(seed_terms)) {
    paste0("Seed terms from labeled data (use as guidance, but expand beyond them; avoid verbatim repeats): ",
           paste(head(unique(seed_terms), k*2), collapse=", "))
  } else {
    "No seeds provided."
  }
  sprintf(paste(
    "You produce domain-relevant keywords/short phrases in %s for a classification category.",
    "Category name: %s",
    "Category description: %s",
    "%s",
    "Return a JSON object with a single key 'keywords' mapped to a unique array of %d strings.",
    "Constraints: output ONLY JSON; no explanations; avoid repeating seed terms verbatim; include synonyms, hyponyms, and multi-word phrases common in this domain; no duplicates.",
    sep="\n"),
    target_lang, cat_name, cat_desc, seeds_txt, k
  )
}
