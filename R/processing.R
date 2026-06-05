# -------------------------------------------------------------------
# Core processing logic for PacemakeR
# Split selection, time/pace computation, relative pace
# -------------------------------------------------------------------

#' @keywords internal
#' @noRd
get_finish_rows <- function(data) {
  data |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("Bib", "Name")))) |>
    dplyr::slice_max(.data$Distance, with_ties = FALSE) |>
    dplyr::ungroup()
}

#' @keywords internal
#' @noRd
get_split_rows <- function(data, distance) {
  available <- unique(data$Distance)

  if (!(distance %in% available)) {
    nearest <- closest_distance(distance, available)
    warning(
      sprintf(
        "Distance %s km not found. Using closest available distance: %s km.",
        distance, nearest
      )
    )
    distance <- nearest
  }

  data[data$Distance == distance, ]
}

#' @keywords internal
#' @noRd
compute_time_metric_values <- function(rows) {
  rows <- drop_zero_distance(rows)
  rows <- clean_invalid_times(rows)

  time_sec <- safe_hms_to_sec(rows$Time)
  valid <- is.finite(time_sec)
  time_sec[valid]
}

#' @keywords internal
#' @noRd
compute_pace_metric_values <- function(rows) {
  rows <- drop_zero_distance(rows)
  rows <- clean_invalid_times(rows)

  time_sec <- safe_hms_to_sec(rows$Time)
  valid <- is.finite(time_sec)
  rows <- rows[valid, ]
  time_sec <- time_sec[valid]

  dist_val <- rows$Distance
  pace_sec <- time_sec / dist_val

  pace_sec[is.finite(pace_sec) & pace_sec > 0]
}

#' @keywords internal
#' @noRd
compute_split_table <- function(data, id_col, id) {
  if (id != "ALL") {
    data <- data[data[[id_col]] == id, ]
    if (nrow(data) == 0) stop("No matching runner found.")
  }

  data$Time_sec <- as.numeric(hms::as_hms(data$Time))

  start_row <- data[1, ]
  start_row$Distance <- 0
  start_row$Time <- "00:00:00"
  start_row$Time_sec <- 0

  data <- rbind(start_row, data)
  data <- data[order(data[[id_col]], data$Distance), ]

  data$SplitTime <- c(NA, diff(data$Time_sec))
  data$SplitDist <- c(NA, diff(data$Distance))

  data$Pace_sec_per_km <- data$SplitTime / data$SplitDist
  data[!is.na(data$Pace_sec_per_km), ]
}

#' @keywords internal
#' @noRd
compute_relative_pace <- function(data, distance) {
  # returns list(rel_values, ref_pace, avg_pace, mode)
  if (is.logical(distance) && distance) {
    finish_time <- max(data$Time_sec)
    avg_pace <- finish_time / 42.195
    rel_values <- avg_pace / data$Pace_sec_per_km
    list(rel_values = rel_values, ref_pace = avg_pace, avg_pace = avg_pace, mode = "avg")
  } else if (is.numeric(distance)) {
    if (!(distance %in% data$Distance)) {
      stop(paste0("Reference distance ", distance, " km not found in data."))
    }
    ref_pace <- data$Pace_sec_per_km[data$Distance == distance][1]
    rel_values <- ref_pace / data$Pace_sec_per_km
    list(rel_values = rel_values, ref_pace = ref_pace, avg_pace = NA_real_, mode = "ref")
  } else {
    list(rel_values = NULL, ref_pace = NA_real_, avg_pace = NA_real_, mode = "none")
  }
}
