# R/packages.R

# install.packages(c(
#   "shiny","shinyWidgets","DT","data.table","arrow","text2vec",
#   "Matrix","jsonlite","httr2","digest","stopwords"
# ))
# # Optional (language detection)
# install.packages("textcat")
# # Demo dataset
# install.packages("textdata")

pkgs <- c("shiny","shinyWidgets","DT","data.table","arrow","text2vec",
          "Matrix","jsonlite","httr2","digest","stopwords")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) stop("Please install missing packages: ", paste(missing, collapse=", "))
suppressPackageStartupMessages({
  lapply(pkgs, require, character.only = TRUE)
})
has_textcat <- requireNamespace("textcat", quietly = TRUE) # optional
has_openai  <- requireNamespace("openai",  quietly = TRUE) # optional
`%||%` <- function(a, b) if (!is.null(a)) a else b
