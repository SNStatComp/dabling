# R/utils_text.R
norm_lang <- function(x) {
  if (is.null(x) || x == "" ) return("en")
  x <- tolower(x)
  if (x %in% stopwords_getlanguages("snowball")) return(x)
  m <- c("english"="en","en-us"="en","en-gb"="en","dutch"="nl","nederlands"="nl",
         "german"="de","french"="fr","spanish"="es","italian"="it","portuguese"="pt")
  if (x %in% names(m)) return(m[[x]])
  substr(x,1,2)
}
detect_language <- function(txt) {
  if (!exists("has_textcat") || !isTRUE(has_textcat)) return(NA_character_)
  pred <- textcat::textcat(txt)
  sub("\\..*$","", tolower(pred))
}
prep_text <- function(x) {
  x <- tolower(x)
  x <- gsub("[^\\p{L}\\p{N}\\s'-]+", " ", x, perl=TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

html_unescape_simple <- function(x) {
  x <- gsub("&lt;", " ", x, fixed = TRUE)
  x <- gsub("&gt;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", " ", x, fixed = TRUE)
  x <- gsub("&quot;", " ", x, fixed = TRUE)
  x <- gsub("&#39;|&apos;|’", " ", x)
  x
}

prep_text_strict <- function(x) {
  x <- html_unescape_simple(x)
  x <- gsub("_", " ", x)
  x <- tolower(x)
  x <- gsub("<[^>]+>", " ", x)                       # strip any tags
  x <- gsub("\\b(lt|gt|quot|amp|nbsp|lt_b|b_gt)\\b", " ", x)
  x <- gsub("\\b(39|39_s)\\b", " ", x)               # odd apostrophe artefacts
  x <- gsub("[^\\p{L}\\s-]", " ", x, perl = TRUE)    # keep letters/spaces/hyphen
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
