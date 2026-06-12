#' Aggregate pacing curve across runners
#'
#' For every runner, builds a relative-pace curve normalised so the
#' reference split equals 1, then averages across runners with a quantile
#' band for the spread. The negative-split runners are averaged
#' separately and overlaid, so the typical field and the target strategy
#' sit on the same plot.
#'
#' @param data A tidy results data frame with `Distance`, `Time` and a
#'   `Bib`/`Name` column.
#' @param relative Reference split distance in km. Default `30`.
#' @param agg_fun Aggregation across runners, `"mean"` or `"median"`.
#' @param ci_probs Lower/upper quantile probabilities for the band.
#' @param return_plot Attach a ggplot2 object? Default `TRUE`.
#'
#' @return A [pacemaker] object.
#' @export
pacemaker <- function(data, relative = 30,
                      agg_fun = c("mean", "median"),
                      ci_probs = c(0.05, 0.95),
                      return_plot = TRUE) {

  agg_fun <- match.arg(agg_fun)
  agg_f   <- match.fun(agg_fun)

  id_col <- if ("Bib" %in% names(data)) "Bib"
  else if ("Name" %in% names(data)) "Name"
  else stop("Data must contain 'Bib' or 'Name'.")

  marathon_dist <- max(data$Distance, na.rm = TRUE)
  ids <- unique(data[[id_col]])

  # One split table per runner; keep only those reaching the reference split.
  split_list <- lapply(ids, function(id) compute_split_table(data, id_col, id))
  split_list <- split_list[vapply(split_list,
                                  function(df) any(df$Distance == relative),
                                  logical(1))]
  if (length(split_list) == 0)
    stop("No runners with the chosen reference split found.")

  dist_grid <- sort(unique(split_list[[1]]$Distance))
  idx_ref   <- which(dist_grid == relative)

  # Relative curve for one runner, normalised so the reference split = 1.
  # NA if the runner has a missing/bad split. Shared by the aggregate and
  # the negative-split summary.
  rel_curve <- function(df) {
    pace <- df$Pace_sec_per_km[match(dist_grid, df$Distance)]
    if (any(!is.finite(pace)) || any(pace <= 0))
      return(rep(NA_real_, length(dist_grid)))
    rel <- pace[idx_ref] / pace
    rel / rel[idx_ref]
  }

  # One column per runner, one row per split distance.
  rel_mat <- vapply(split_list, rel_curve, numeric(length(dist_grid)))

  # Negative-split runners: second half quicker than first.
  is_neg <- vapply(split_list, function(df) {
    half   <- df[which.min(abs(df$Distance - marathon_dist / 2)), ]
    finish <- df[which.max(df$Distance), ]
    compute_positive_split(half$Time_sec, finish$Time_sec) < 0
  }, logical(1))

  rel_neg <- if (any(is_neg)) {
    rowMeans(rel_mat[, is_neg, drop = FALSE], na.rm = TRUE)
  } else {
    rep(NA_real_, length(dist_grid))
  }

  curve_tbl <- tibble::tibble(
    Distance      = dist_grid,
    rel_agg       = apply(rel_mat, 1, agg_f, na.rm = TRUE),
    rel_ci_low    = apply(rel_mat, 1, quantile, ci_probs[1], na.rm = TRUE),
    rel_ci_high   = apply(rel_mat, 1, quantile, ci_probs[2], na.rm = TRUE),
    rel_neg_split = rel_neg
  )

  plot_obj <- NULL
  if (return_plot) {
    plot_obj <- ggplot2::ggplot(curve_tbl, ggplot2::aes(x = Distance)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = rel_ci_low, ymax = rel_ci_high),
        fill = "grey80", alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = rel_agg),
                         colour = "steelblue", linewidth = 1.2) +
      ggplot2::geom_line(ggplot2::aes(y = rel_neg_split),
                         colour = "purple", linetype = "dotdash") +
      ggplot2::labs(
        x = "Distance (km)",
        y = paste0("Relative pace (ref = ", relative, " km)"),
        title = "PacemakeR Relative Pacing Curve",
        subtitle = paste("Aggregated using", agg_fun)) +
      ggplot2::theme_minimal(base_size = 14)
  }

  new_pacemaker(curve_tbl, plot_obj,
                meta = list(relative = relative,
                            n_runners = ncol(rel_mat), agg_fun = agg_fun))
}
