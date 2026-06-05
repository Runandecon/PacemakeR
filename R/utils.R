# -------------------------------------------------------------------
# Utility functions for PacemakeR
# Internal helpers: formatting, cleaning, conversions
# -------------------------------------------------------------------

#' @keywords internal
#' @noRd
#' @import dplyr ggplot2 hms scales tibble
#' @importFrom stats median sd
NULL

#' @keywords internal
#' @noRd
# Format seconds into mm:ss pace string
format_pace_mmss <- function(sec) {
  m <- sec %/% 60
  s <- sec %% 60
  sprintf("%02d:%02d", m, s)
}

#' @keywords internal
#' @noRd
# Format seconds into hh:mm:ss time string
format_time_hms <- function(sec) {
  h <- sec %/% 3600
  m <- (sec %% 3600) %/% 60
  s <- sec %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}

#' @keywords internal
#' @noRd
# Convert a relative ratio (e.g., 1.05, 0.98) into a signed percentage string
# such as "+5%" or "-2%".
format_percent_rel <- function(x) {
  pct <- (x - 1) * 100
  sprintf("%+d%%", round(pct))
}

#' @keywords internal
#' @noRd
# Safely convert hh:mm:ss to seconds, returning NA for invalid entries.
safe_hms_to_sec <- function(x) {
  out <- suppressWarnings(as.numeric(hms::as_hms(x)))
  ifelse(is.finite(out), out, NA_real_)
}

#' @keywords internal
#' @noRd
# Remove synthetic 0 km rows (added for split-pace plotting)
drop_zero_distance <- function(df) {
  df[df$Distance > 0, ]
}

#' @keywords internal
#' @noRd
# Remove rows with invalid or missing time values (NA, "", DNF, DNS)
clean_invalid_times <- function(df) {
  bad <- is.na(df$Time) | df$Time %in% c("", "DNF", "DNS")
  df[!bad, ]
}

#' @keywords internal
#' @noRd
# Closest available distance to requested
closest_distance <- function(requested, available) {
  diffs <- abs(available - requested)
  available[which.min(diffs)]
}

#' @keywords internal
#' @noRd
parse_marker <- function(x) {
  parts <- strsplit(x, ":")[[1]]

  # mm:ss → pace
  if (length(parts) == 2) {
    m <- as.numeric(parts[1])
    s <- as.numeric(parts[2])
    return(list(type = "pace", value = m * 60 + s))
  }

  # hh:mm:ss → finish time
  if (length(parts) == 3) {
    h <- as.numeric(parts[1])
    m <- as.numeric(parts[2])
    s <- as.numeric(parts[3])
    total <- h * 3600 + m * 60 + s
    return(list(type = "time", value = total))
  }

  stop("Marker must be mm:ss or hh:mm:ss")
}




#' @importFrom stats median sd
NULL

utils::globalVariables(c(".data", "Distance", "PlotValue", "values"))

