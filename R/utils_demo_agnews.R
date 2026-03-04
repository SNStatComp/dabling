agnews_textdata_urls <- list(
  train = "https://raw.githubusercontent.com/mhjabreel/CharCnn_Keras/master/data/ag_news_csv/train.csv",
  test  = "https://raw.githubusercontent.com/mhjabreel/CharCnn_Keras/master/data/ag_news_csv/test.csv"
)

agnews_default_textdata_dir <- function() file.path(tempdir(), "textdata_cache")

.download_to <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
}

ensure_agnews_in_cache <- function(dir = NULL, clean = TRUE, auto_download = TRUE) {
  dir <- dir %||% agnews_default_textdata_dir()
  folder_path <- textdata::dataset_ag_news(dir = dir, return_path = TRUE)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  
  train_rds <- file.path(folder_path, "ag_news_train.rds")
  test_rds  <- file.path(folder_path, "ag_news_test.rds")
  if (file.exists(train_rds) && file.exists(test_rds)) return(invisible())
  
  if (!isTRUE(auto_download)) stop("AG News not cached; enable auto-download.")
  
  train_csv <- file.path(folder_path, "ag_news_train.csv")
  test_csv  <- file.path(folder_path, "ag_news_test.csv")
  if (!file.exists(train_csv)) .download_to(agnews_textdata_urls$train, train_csv)
  if (!file.exists(test_csv))  .download_to(agnews_textdata_urls$test,  test_csv)
  
  if (!file.exists(train_csv) || !file.exists(test_csv)) {
    stop(
      "AG News download did not complete successfully. Expected files not found in: ",
      folder_path
    )
  }
  
  textdata::dataset_ag_news(
    dir = dir,
    split = "train",
    manual_download = TRUE,
    clean = isTRUE(clean)
  )
}

load_agnews_demo <- function(split = c("train","test"), dir = NULL, clean = TRUE, auto_download = TRUE) {
  split <- match.arg(split)
  dir <- dir %||% agnews_default_textdata_dir()
  
  folder_path <- textdata::dataset_ag_news(dir = dir, return_path = TRUE)
  rds_path <- file.path(folder_path, sprintf("ag_news_%s.rds", split))
  if (!file.exists(rds_path)) ensure_agnews_in_cache(dir, clean, auto_download)
  
  textdata::dataset_ag_news(dir = dir, split = split)
}