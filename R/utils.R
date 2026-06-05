# -------------------------------------------------------------------
# Utility functions for PacemakeR
# These helpers are internal and not exported.
# They provide formatting and small computational tools used
# across plotting and summary functions.
# -------------------------------------------------------------------

#' @keywords internal
#' @noRd
# Format seconds into mm:ss pace string
# Used for pace visualisation and summary output.
format_pace_mmss <- function(sec) {
  m <- sec %/% 60
  s <- sec %% 60
  sprintf("%02d:%02d", m, s)
}

#' @keywords internal
#' @noRd
# Convert a relative ratio (e.g., 1.05, 0.98) into a signed percentage string
# such as "+5%" or "-2%". Used in relative pace plots.
format_percent_rel <- function(x) {
  pct <- (x - 1) * 100
  sprintf("%+d%%", round(pct))
}

#' @keywords internal
#' @noRd
# Safely convert hh:mm:ss to seconds, returning NA for invalid entries.
# Prevents crashes when malformed times appear in the dataset.
safe_hms_to_sec <- function(x) {
  out <- suppressWarnings(as.numeric(hms::as_hms(x)))
  ifelse(is.finite(out), out, NA_real_)
}

#' @keywords internal
#' @noRd
# Remove synthetic 0 km rows (added for split-pace plotting)
# Ensures summary functions only use real split distances.
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

closest_distance <- function(requested, available) {
  diffs <- abs(available - requested)
  available[which.min(diffs)]
}

formatter <- function(x) format_pace_mmss(round(x))
