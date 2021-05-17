#' Oversample a dataset by SMOTE.
#'
#' @param data Dataset to be oversampled.
#' @param cls Class to be oversampled.
#' @param cls_col Column containing class information.
#' @param m Desired number of samples in the oversampled data.
#'
#' @return The oversampled dataset.
#' @export
#'
#' @importFrom smotefamily SMOTE
#'
#' @examples
#' table(iris$Species)
#' smoted <- oversample_smote(iris, "setosa", "Species", 100)
#' nrow(smoted)
oversample_smote <- function(data, cls, cls_col, m) {
  col_ind <- which(names(data) == cls_col)
  orig_cols <- names(data)
  dup_size <- ceiling(m / sum(data[[cls_col]] == cls))
  # set the class to whether it is equal to the minority class
  data[[cls_col]] <- as.factor(data[[cls_col]] == cls)
  # smotefamily::SMOTE breaks for one-dim datasets. This adds a dummy column
  # so SMOTE can execute in that case. This does not affect how data is synthesized
  if (ncol(data) == 2) {
    data$dummy__col__ <- 0
  }
  # perform SMOTE
  smoteret <- SMOTE(data[, -col_ind],
    data[, col_ind],
    dup_size = dup_size
  )
  # rbind the original observations and sufficient samples of the synthetic ones
  orig <- smoteret$orig_P
  target_samp <- m - nrow(orig)
  synt <- smoteret$syn_data[sample.int(nrow(smoteret$syn_data),
    size = target_samp,
    replace = target_samp > nrow(smoteret$syn_data)
  ), ]
  d_prime <- rbind(orig, synt)
  colnames(d_prime)[ncol(d_prime)] <- cls_col
  d_prime[[cls_col]] <- cls
  # remove the dummy column if necessary
  d_prime <- d_prime[, names(d_prime) != "dummy__col__"]
  # reorder the columns to be the same as the original data
  return(d_prime[, orig_cols])
}
