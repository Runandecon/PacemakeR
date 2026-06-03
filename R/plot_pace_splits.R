format_pace_mmss <- function(sec) {
  m <- sec %/% 60
  s <- sec %% 60
  sprintf("%02d:%02d", m, s)
}

format_percent_rel <- function(x) {
  pct <- (x - 1) * 100
  sprintf("%+d%%", round(pct))
}


#' Plot split pace for one or all runners
#'
#' @description
#' Plots split pace (mm:ss per km) between splits. Requires columns:
#' Distance (numeric), Time (hh:mm:ss), and either Bib or Name.
#'
#' @param data A tidy data frame.
#' @param id A Bib or Name. Use "ALL" to plot all runners.
#' @param relative TRUE for relative to average pace, or numeric distance
#'   to show pace relative to that split.
#' @param reference "decimal" (default), "pct"/"percentage", or "pace"
#'
#' @return A ggplot object.
#' @export
plot_pace_splits <- function(data, id = NULL, relative = NULL,
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

  # Filter if needed
  if (id != "ALL") {
    data <- data[data[[id_col]] == id, ]
    if (nrow(data) == 0) stop("No matching runner found.")
  }

  # Convert cumulative time
  data$Time_sec <- as.numeric(hms::as_hms(data$Time))

  # --- ADD START ROW TO PRESERVE 5K SPLIT ---
  start_row <- data[1, ]
  start_row$Distance <- 0
  start_row$Time <- "00:00:00"
  start_row$Time_sec <- 0

  data <- rbind(start_row, data)
  data <- data[order(data[[id_col]], data$Distance), ]

  # Compute split time + split distance
  data$SplitTime <- c(NA, diff(data$Time_sec))
  data$SplitDist <- c(NA, diff(data$Distance))

  # Pace per km
  data$Pace_sec_per_km <- data$SplitTime / data$SplitDist

  # Remove first NA row
  data <- data[!is.na(data$Pace_sec_per_km), ]

  # ---------------------------
  # RELATIVE OPTION
  # ---------------------------

  dashed_line <- NULL   # default: no line

  if (is.logical(relative) && relative == TRUE) {

    finish_time <- max(data$Time_sec)
    avg_pace <- finish_time / 42.195

    # Higher = faster
    rel_values <- avg_pace / data$Pace_sec_per_km

    rel_label <- "Relative Pace (vs average)"

  } else if (is.numeric(relative)) {

    if (!(relative %in% data$Distance)) {
      stop(paste0("Reference distance ", relative, " km not found in data."))
    }

    ref_pace <- data$Pace_sec_per_km[data$Distance == relative][1]

    rel_values <- ref_pace / data$Pace_sec_per_km

    rel_label <- paste0("Relative Pace (ref = ", relative, " km)")

  } else {

    # No relative mode → normal split pace
    rel_values <- NULL
  }

  # ---------------------------
  # REFERENCE FORMAT HANDLING
  # ---------------------------

  if (!is.null(rel_values)) {

    # Always show dashed line at 1 (baseline)
    dashed_line <- ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      color = "red"
    )

    if (reference %in% c("pct", "percentage")) {

      data$PlotValue <- rel_values
      y_label <- "Relative Pace (%)"
      y_format <- format_percent_rel

    } else if (reference == "pace") {

      # Convert relative pace back into mm:ss pace
      # pace_rel = ref_pace / pace → pace = ref_pace / pace_rel
      if (is.logical(relative) && relative == TRUE) {
        finish_time <- max(data$Time_sec)
        avg_pace <- finish_time / 42.195
        data$PlotValue <- avg_pace / rel_values
      } else {
        data$PlotValue <- ref_pace / rel_values
      }

      y_label <- "Relative Pace (mm:ss)"
      y_format <- format_pace_mmss

      # dashed line at reference pace
      dashed_line <- ggplot2::geom_hline(
        yintercept = if (is.logical(relative) && relative == TRUE) avg_pace else ref_pace,
        linetype = "dashed",
        color = "red"
      )

    } else {

      # Default: decimal
      data$PlotValue <- rel_values
      y_label <- "Relative Pace (ratio)"
      y_format <- scales::number_format(accuracy = 0.01)
    }

  } else {

    # Normal split pace mode
    data$PlotValue <- data$Pace_sec_per_km
    y_label <- "Split Pace (mm:ss per km)"
    y_format <- format_pace_mmss
  }

  # Extract Name + Bib for title
  runner_name <- if ("Name" %in% names(data)) unique(data$Name)[1] else NA
  runner_bib  <- if ("Bib"  %in% names(data)) unique(data$Bib)[1]  else NA

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

  # ---------------------------
  # PLOT
  # ---------------------------

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

