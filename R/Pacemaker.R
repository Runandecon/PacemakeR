#' Build a pacing analysis from marathon split data
#'
#' @description
#' Runs the whole pacing analysis in one call and returns a single object
#' you can plot and print from. For every runner it builds a relative-pace
#' curve normalised so the reference split equals 1, averages those curves
#' across the field with a quantile band, and works out the negative-split
#' subgroup and the cumulative advantage they gain over the course.
#'
#' The result is a `pacemaker` object. From it you can:
#' \itemize{
#'   \item `plot(x)` -- the aggregate pacing curve (default),
#'   \item `plot(x, "gain")` -- the cumulative negative-split advantage,
#'   \item `print(x)` -- the summary table.
#' }
#'
#' @param data A tidy results data frame with `Distance`, `Time` and a
#'   `Bib`/`Name` column.
#' @param relative Reference split distance in km. Default `30`.
#' @param agg_fun Aggregation across runners, `"mean"` or `"median"`.
#' @param ci_probs Lower/upper quantile probabilities for the band.
#'
#' @return A [pacemaker] object.
#' @examples
#' \dontrun{
#'   opt <- pacemaker(London_Marathon_2026)
#'   opt                                  # summary table
#'   plot(opt)                            # pacing curve
#'   plot(opt, "gain")                    # advantage in metres
#'   plot(opt, "gain", unit = "time", pace_sec = 300)
#' }
#' @export
pacemaker <- function(data, relative = 30,
                      agg_fun = c("mean", "median"),
                      ci_probs = c(0.05, 0.95)) {

  agg_fun <- match.arg(agg_fun)
  agg_f   <- match.fun(agg_fun)

  id_col <- if ("Bib" %in% names(data)) "Bib"
  else if ("Name" %in% names(data)) "Name"
  else stop("Data must contain 'Bib' or 'Name'.")

  marathon_dist <- max(data$Distance, na.rm = TRUE)
  ids <- unique(data[[id_col]])

  # One split table per runner; keep only those who reach the reference split.
  split_list <- lapply(ids, function(id) compute_split_table(data, id_col, id))
  split_list <- split_list[vapply(split_list,
                                  function(df) any(df$Distance == relative),
                                  logical(1))]
  if (length(split_list) == 0)
    stop("No runners with the chosen reference split found.")

  dist_grid <- sort(unique(split_list[[1]]$Distance))
  idx_ref   <- which(dist_grid == relative)

  # Relative curve for one runner, normalised so the reference split = 1.
  # NA if the runner has a missing or non-positive split. Used for both the
  # field aggregate and the negative-split subgroup.
  rel_curve <- function(df) {
    pace <- df$Pace_sec_per_km[match(dist_grid, df$Distance)]
    if (any(!is.finite(pace)) || any(pace <= 0))
      return(rep(NA_real_, length(dist_grid)))
    rel <- pace[idx_ref] / pace
    rel / rel[idx_ref]
  }

  # One column per runner, one row per split.
  rel_mat <- vapply(split_list, rel_curve, numeric(length(dist_grid)))

  # Flag negative-split runners: second half quicker than first.
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

  # The pacing curve: field aggregate, band, and negative-split overlay.
  curve <- tibble::tibble(
    Distance      = dist_grid,
    rel_agg       = apply(rel_mat, 1, agg_f, na.rm = TRUE),
    rel_ci_low    = apply(rel_mat, 1, stats::quantile, ci_probs[1], na.rm = TRUE),
    rel_ci_high   = apply(rel_mat, 1, stats::quantile, ci_probs[2], na.rm = TRUE),
    rel_neg_split = rel_neg
  )

  # Cumulative negative-split advantage, in km. The plot scales this to
  # metres or seconds on demand, so the expensive part is done once here.
  seg_len    <- c(dist_grid[1], diff(dist_grid))
  seg_diff   <- (1 / curve$rel_agg - 1 / curve$rel_neg_split) * seg_len
  cumdiff_km <- if (any(is_neg)) cumsum(seg_diff) else rep(NA_real_, length(dist_grid))

  # A short table for print().
  summary_tbl <- tibble::tibble(
    metric = c("min_rel", "max_rel", "mean_rel", "fade_at_finish"),
    value  = c(min(curve$rel_agg),  max(curve$rel_agg),
               mean(curve$rel_agg), curve$rel_agg[length(curve$rel_agg)])
  )

  new_pacemaker(
    curve      = curve,
    cumdiff_km = cumdiff_km,
    summary    = summary_tbl,
    meta = list(relative = relative, n_runners = ncol(rel_mat),
                agg_fun = agg_fun, has_negative = any(is_neg))
  )
}
