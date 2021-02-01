#' Oversample a dataset by SMOTE.
#'
#' @param data Dataset to be oversampled.
#' @param cls Class to be oversampled.
#' @param cls.col Column containing class information.
#' @param m Desired number of samples in the oversampled data.
#'
#' @return The oversampled dataset.
#' @export
#'
#' @importFrom smotefamily SMOTE
#'
#' @examples
#' table(iris$Species)
#' smoted <- oversample.smote(iris, "setosa", "Species", 100)
#' nrow(smoted)
oversample.smote <- function(data, cls, cls.col, m){
    col.ind <- which(names(data) == cls.col)
    orig.cols <- names(data)
    dup.size = ceiling(m / sum(data[[cls.col]] == cls))
    # set the class to whether it is equal to the minority class
    data[[cls.col]] <- as.factor(data[[cls.col]] == cls)
    # smotefamily::SMOTE breaks for one-dim datasets. This adds a dummy column
    # so SMOTE can execute in that case. This does not affect how data is synthesized
    if (ncol(data) == 2) {data$dummy__col__ <- 0}
    # perform SMOTE
    smoteret <- SMOTE(data[, -col.ind],
                      data[, col.ind],
                      dup_size=dup.size)
    # rbind the original observations and sufficient samples of the synthetic ones
    orig <- smoteret$orig_P
    target.samp <- m - nrow(orig)
    synt <- smoteret$syn_data[sample.int(nrow(smoteret$syn_data),
                                         size=target.samp,
                                         replace=target.samp > nrow(smoteret$syn_data)), ]
    d_prime <- rbind(orig, synt)
    colnames(d_prime)[ncol(d_prime)] <- cls.col
    d_prime[[cls.col]] <- cls
    # remove the dummy column if necessary
    d_prime <- d_prime[, names(d_prime) != "dummy__col__"]
    # reorder the columns to be the same as the original data
    return(d_prime[, orig.cols])
}
