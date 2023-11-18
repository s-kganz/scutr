# scutr 0.2.0

 * Fixed an issue where inappropriate values of `k` were passed to `smotefamily::SMOTE` (#1)
 * `undersample_*` functions now accept additional keyword arguments and pass these on to underlying functions, e.g. `stats::dist`.

# scutr 0.1.0

This is the initial version of the package. The following is available.

 * `SCUT` and `SCUT_parallel` driver functions.
 * Undersampling functions: `undersample_mclust`, `undersample_kmeans`, `undersample_mindist`, `undersample_hclust`, and `undersample_tomek`.
 * Oversampling functions: `oversample_smote`.
 * `resample_random` for both oversampling and undersampling.
 * Three imbalanced toy datasets: `wine`, `bullseye`, and `imbalance`.
