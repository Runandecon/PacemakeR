#' Plot split pace for one or all runners
#'
#' @description
#' Plots split pace (mm:ss per km) between splits. Requires columns:
#' Distance (numeric), Time (hh:mm:ss), and either Bib or Name.
#'
#' @param data A tidy data frame.
#' @param id A Bib or Name. Use "ALL" to plot all runners.
#' @param distance TRUE for distance to average pace, or numeric distance
#'   to show pace distance to that split.
#' @param reference "decimal" (default), "pct"/"percentage", or "pace"
#'
#' @return A ggplot object.
#' @export
plot_pace_splits <- function(data, id = NULL, distance = NULL,
                             reference = "decimal") {

  if (is.null(id)) {
    stop('You must specify a Bib/Name or "ALL".')
  }

  # Identify ID column
  if ("Bib" %in% names(data)) {
    id_col <- "Bib"
  } else if ("Name" %in% names(data)) {
    id_col <- "Name"
  } else {
    stop("Data must contain a 'Bib' or 'Name' column.")
  }

  # Core split table
  data <- compute_split_table(data, id_col, id)

  # Relative pace handling
  rel_info <- compute_relative_pace(data, distance)
  rel_values <- rel_info$rel_values
  ref_pace   <- rel_info$ref_pace
  avg_pace   <- rel_info$avg_pace

  dashed_line <- NULL
  y_label <- "Split Pace (mm:ss per km)"
  y_format <- format_pace_mmss

  if (!is.null(rel_values)) {

    dashed_line <- ggplot2::geom_hline(
      yintercept = if (!is.na(avg_pace)) 1 else 1,
      linetype = "dashed",
      color = "red"
    )

    if (reference %in% c("pct", "percentage")) {

      data$PlotValue <- rel_values
      y_label <- "distance Pace (%)"
      y_format <- format_percent_rel

    } else if (reference == "pace") {

      if (!is.na(avg_pace) && is.logical(distance) && distance) {
        data$PlotValue <- avg_pace / rel_values
        ref_line <- avg_pace
      } else {
        data$PlotValue <- ref_pace / rel_values
        ref_line <- ref_pace
      }

      y_label <- "distance Pace (mm:ss)"
      y_format <- format_pace_mmss

      dashed_line <- ggplot2::geom_hline(
        yintercept = ref_line,
        linetype = "dashed",
        color = "red"
      )

    } else {

      data$PlotValue <- rel_values
      y_label <- "distance Pace (ratio)"
      y_format <- scales::number_format(accuracy = 0.01)
    }

  } else {

    data$PlotValue <- data$Pace_sec_per_km
    y_label <- "Split Pace (mm:ss per km)"
    y_format <- format_pace_mmss
  }

  # Extract Name + Bib for title
  runner_name <- if ("Name" %in% names(data)) unique(data$Name)[1] else NA
  runner_bib  <- if ("Bib"  %in% names(data)) unique(data$Bib)[1]  else NA

  rel_label <- if (is.logical(distance) && distance) {
    "distance Pace (vs average)"
  } else if (is.numeric(distance)) {
    paste0("distance Pace (ref = ", distance, " km)")
  } else {
    "Split Pace"
  }

  plot_title <- if (id == "ALL") {
    if (is.null(rel_values)) "Split Pace for All Runners"
    else paste0(rel_label, " for All Runners")
  } else {
    if (is.null(rel_values)) {
      paste0("Pacing Curve of ", runner_name, " (", runner_bib, ")")
    } else {
      paste0(rel_label, " of ", runner_name, " (", runner_bib, ")")
    }
  }

  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = Distance,
      y = PlotValue,
      colour = .data[[id_col]],
      group = .data[[id_col]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    dashed_line +
    ggplot2::scale_y_continuous(labels = y_format) +
    ggplot2::labs(
      x = "Distance (km)",
      y = y_label,
      colour = id_col,
      title = plot_title
    ) +
    ggplot2::theme_minimal(base_size = 14)
}
