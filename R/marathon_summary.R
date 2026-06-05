#' Summarize marathon times or paces at finish or any split
#'
#' @description
#' Computes summary statistics (mean, median, min, max, sd) for finishing times
#' or for any split distance (e.g., 5K, 10K, 30K). Can summarize either time
#' (default) or pace (mm:ss per km).
#'
#' @param data A tidy data frame with columns `Distance` and `Time`.
#' @param distance Either "finish" (default) or a numeric distance (e.g., 30).
#' @param pace Logical. If TRUE, summarize pace instead of time.
#' @param plot Logical. If TRUE, plots a distribution of the selected metric.
#'
#' @return A tibble with summary statistics in seconds and formatted output.
#' @export
marathon_summary <- function(data, distance = "finish", pace = FALSE, plot = FALSE) {

  if (!all(c("Distance", "Time") %in% names(data))) {
    stop("Data must contain 'Distance' and 'Time' columns.")
  }

  # --- Select rows based on distance ---
  if (identical(distance, "finish")) {
    rows <- get_finish_rows(data)
    title_label <- "Finishing Times"
  } else if (is.numeric(distance)) {
    rows <- get_split_rows(data, distance)
    title_label <- paste0(unique(rows$Distance)[1], " km Split Times")
  } else {
    stop("`distance` must be 'finish' or numeric.")
  }

  # --- Metric values (time or pace) ---
  if (pace) {
    metric_values <- compute_pace_metric_values(rows)
    y_label <- "Pace (mm:ss per km)"
    formatter <- function(x) format_pace_mmss(round(x))
    title_label <- sub("Times", "Paces", title_label)
  } else {
    metric_values <- compute_time_metric_values(rows)
    y_label <- "Time (hh:mm:ss)"
    formatter <- function(x) format_time_hms(round(x))
  }

  # SECOND FILTER — robust against any remaining invalids
  metric_values <- metric_values[is.finite(metric_values) & metric_values > 0]

  # --- Summary ---
  summary_tbl <- tibble::tibble(
    metric = c("mean", "median", "min", "max", "sd"),
    seconds = c(
      mean(metric_values, na.rm = TRUE),
      median(metric_values, na.rm = TRUE),
      min(metric_values, na.rm = TRUE),
      max(metric_values, na.rm = TRUE),
      sd(metric_values, na.rm = TRUE)
    )
  )

  summary_tbl$formatted <- formatter(summary_tbl$seconds)

  # --- Plot ---
  if (plot) {
    p <- ggplot2::ggplot(
      tibble::tibble(values = metric_values),
      ggplot2::aes(x = values)
    ) +
      ggplot2::geom_histogram(
        bins = 40,
        fill = "steelblue",
        alpha = 0.7,
        color = "white"
      ) +
      ggplot2::scale_x_continuous(labels = formatter) +
      ggplot2::labs(
        title = paste("Distribution of", title_label),
        x = y_label,
        y = "Count"
      ) +
      ggplot2::theme_minimal(base_size = 14)

    print(p)
  }

  summary_tbl
}
