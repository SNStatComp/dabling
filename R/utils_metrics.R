# R/utils_metrics.R
compute_metrics <- function(scores_dt, truth_dt, truth_col = "label") {
  pred1 <- scores_dt[rank == 1L, .(id, pred = category_id)]
  dt <- merge(pred1, truth_dt[, .(id, truth = .SD[[truth_col]])], by="id", all.x=TRUE)
  dt <- dt[!is.na(truth)]
  if (!nrow(dt)) return(list(summary = data.table(metric=character(), value=numeric()),
                             per_class = data.table(class=character(), precision=numeric(), recall=numeric(), f1=numeric())))
  acc <- mean(dt$pred == dt$truth)
  classes <- unique(c(dt$pred, dt$truth))
  per <- lapply(classes, function(cl) {
    tp <- sum(dt$pred == cl & dt$truth == cl)
    fp <- sum(dt$pred == cl & dt$truth != cl)
    fn <- sum(dt$pred != cl & dt$truth == cl)
    precision <- ifelse(tp+fp>0, tp/(tp+fp), NA_real_)
    recall    <- ifelse(tp+fn>0, tp/(tp+fn), NA_real_)
    f1        <- ifelse(!is.na(precision) & !is.na(recall) & (precision+recall)>0, 2*precision*recall/(precision+recall), NA_real_)
    data.table(class=cl, precision=precision, recall=recall, f1=f1)
  })
  per <- rbindlist(per)
  macro <- colMeans(per[, .(precision, recall, f1)], na.rm = TRUE)
  summary <- data.table(metric=c("accuracy","macro_precision","macro_recall","macro_f1"),
                        value=c(acc, macro["precision"], macro["recall"], macro["f1"]))
  list(summary=summary, per_class=per)
}
