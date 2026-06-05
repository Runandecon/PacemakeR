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

  # --- Validate columns ---
  if (!all(c("Distance", "Time") %in% names(data))) {
    stop("Data must contain 'Distance' and 'Time' columns.")
  }

  # --- Select rows based on distance ---
  if (identical(distance, "finish")) {

    rows <- data |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c("Bib", "Name")))) |>
      dplyr::slice_max(Distance, with_ties = FALSE) |>
      dplyr::ungroup()

    title_label <- "Finishing Times"

  } else if (is.numeric(distance)) {

    available <- unique(data$Distance)

    if (!(distance %in% available)) {

    # Find nearest split
    nearest <- closest_distance(distance, available)

    warning(
      sprintf(
        "Distance %s km not found. Using closest available distance: %s km.",
        distance, nearest
      )
    )

    distance <- nearest
  }

  rows <- data[data$Distance == distance, ]
  title_label <- paste0(distance, " km Split Times")


  } else {
    stop("`distance` must be 'finish' or numeric.")
  }

  # --- Clean data using utils ---
  rows <- drop_zero_distance(rows)
  rows <- clean_invalid_times(rows)

  # Convert to seconds safely
  time_sec <- safe_hms_to_sec(rows$Time)

  # Remove NA times
  valid <- is.finite(time_sec)
  rows <- rows[valid, ]
  time_sec <- time_sec[valid]

  dist_val <- rows$Distance

  # --- Pace or time ---
  if (pace) {

    pace_sec <- time_sec / dist_val

    # Keep only valid paces
    valid_pace <- is.finite(pace_sec) & pace_sec > 0
    pace_sec <- pace_sec[valid_pace]

    metric_values <- pace_sec

    # SECOND FILTER — this is the missing fix
    metric_values <- metric_values[is.finite(metric_values) & metric_values > 0]

    y_label <- "Pace (mm:ss per km)"

    # Round before formatting to avoid sprintf errors
    formatter <- function(x) format_pace_mmss(round(x))

    title_label <- sub("Times", "Paces", title_label)
  }


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

  return(summary_tbl)
}
