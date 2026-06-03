#' Plot cumulative pacing for one or all runners
#'
#' @description
#' Plots cumulative time (hh:mm:ss) against distance. Requires columns:
#' Distance (numeric), Time (hh:mm:ss), and either Bib or Name.
#'
#' @param data A tidy data frame.
#' @param id A Bib or Name. Use "ALL" to plot all runners.
#'
#' @return A ggplot object.
#' @export
plot_pacing <- function(data, id = NULL) {

  # --- 1. Error if id missing ---
  if (is.null(id)) {
    stop('You must specify a id = Bib/Name or "ALL".')
  }

  # --- 2. Detect identifier column ---
  if ("Bib" %in% names(data)) {
    id_col <- "Bib"
  } else if ("Name" %in% names(data)) {
    id_col <- "Name"
  } else {
    stop("Data must contain a 'Bib' or 'Name' column.")
  }

  # --- 3. Filter if not ALL ---
  if (id != "ALL") {
    data <- data[data[[id_col]] == id, ]
    if (nrow(data) == 0) stop("No matching runner found.")
  }

  # --- 4. Convert hh:mm:ss → seconds ---
  data$Time_sec <- as.numeric(hms::as_hms(data$Time))

  # --- 5. Add starting point ---
  start_row <- data[1, ]
  start_row$Distance <- 0
  start_row$Time <- "00:00:00"
  start_row$Time_sec <- 0

  data <- rbind(start_row, data)
  data <- data[order(data[[id_col]], data$Distance), ]

  # --- 6. Plot ---
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = Distance,
      y = Time_sec,
      colour = .data[[id_col]],
      group = .data[[id_col]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(
      labels = function(sec) hms::as_hms(sec)
    ) +
    ggplot2::labs(
      x = "Distance (km)",
      y = "Cumulative Time",
      colour = id_col,
      title = if (id == "ALL") "Pacing Curves for All Runners"
      else {
        runner_name <- unique(data[[id_col]])[1]
        runner_bib  <- if ("Bib" %in% names(data)) unique(data$Bib)[1] else NA
        paste0("Pacing Curve of ", runner_name, " (", runner_bib, ")")
      }
    ) +
    ggplot2::theme_minimal(base_size = 14)
}
