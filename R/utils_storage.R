# R/utils_storage.R
default_ckpt_dir <- file.path(tempdir(), "autoclassify_cache")
if (!dir.exists(default_ckpt_dir)) dir.create(default_ckpt_dir, recursive = TRUE, showWarnings = FALSE)

ckpt_path <- function(ckpt_dir, name) {
  dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(ckpt_dir, name)
}
parquet_write <- function(dt, path) write_parquet(as.data.table(dt), path)
parquet_read  <- function(path) as.data.table(read_parquet(path))

ensure_llm_cache <- function(ckpt_dir) {
  p <- ckpt_path(ckpt_dir, "llm_keywords_cache.parquet")
  if (!file.exists(p)) write_parquet(data.table(hash=character(), scope=character(), payload=character()), p)
  p
}
