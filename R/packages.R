required_pkgs <- c(
  "shiny","shinyWidgets","DT","data.table",
  "arrow","text2vec","Matrix",
  "jsonlite","httr2","digest",
  "stopwords","yaml",
  "textdata"
)

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Missing required packages: ", paste(missing, collapse = ", "),
    "\n\nRun:\n  install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))\n"
  )
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(DT)
  library(data.table)
  library(arrow)
  library(text2vec)
  library(Matrix)
  library(jsonlite)
  library(httr2)
  library(digest)
  library(stopwords)
  library(yaml)
  library(textdata)
})

has_textcat <- requireNamespace("textcat", quietly = TRUE)
`%||%` <- function(a, b) if (!is.null(a)) a else b