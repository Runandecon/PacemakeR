# -------------------------------------------------------------------
# Utility functions for PacemakeR
# Internal helpers: formatting, time parsing, row cleaning, small bits
# of math shared across the package. None of this is exported.
# -------------------------------------------------------------------

#' @keywords internal
#' @noRd
#' @import dplyr ggplot2 hms scales tibble
#' @importFrom stats median sd quantile
#' @importFrom utils globalVariables
NULL

# Column names referenced with bare symbols inside ggplot/dplyr, declared
# here so R CMD check doesn't flag them as undefined globals.
utils::globalVariables(c(
  ".data", "Distance", "PlotValue", "values",
  "rel_agg", "rel_ci_low", "rel_ci_high", "rel_neg_split", "cumdiff"
))


# ---- Formatting -----------------------------------------------------

#' @keywords internal
#' @noRd
# Seconds -> "mm:ss".
format_pace_mmss <- function(sec) {
  sprintf("%02d:%02d", as.integer(sec %/% 60), as.integer(sec %% 60))
}

#' @keywords internal
#' @noRd
# Seconds -> "hh:mm:ss".
format_time_hms <- function(sec) {
  sprintf("%02d:%02d:%02d",
          as.integer(sec %/% 3600),
          as.integer((sec %% 3600) %/% 60),
          as.integer(sec %% 60))
}

#' @keywords internal
#' @noRd
# Ratio (1 = on pace) -> signed percentage, e.g. 1.05 -> "+5%".
format_percent_rel <- function(x) {
  sprintf("%+d%%", round((x - 1) * 100))
}


# ---- Time parsing and cleaning --------------------------------------

#' @keywords internal
#' @noRd
# "hh:mm:ss" -> seconds. Anything that won't parse comes back as NA
# rather than throwing, so one bad row can't kill a whole call.
safe_hms_to_sec <- function(x) {
  vapply(x, function(v) {
    out <- tryCatch(suppressWarnings(as.numeric(hms::as_hms(v))),
                    error = function(e) NA_real_)
    if (is.finite(out)) out else NA_real_
  }, numeric(1), USE.NAMES = FALSE)
}

#' @keywords internal
#' @noRd
# Drop the synthetic 0 km row added for split-pace plotting.
drop_zero_distance <- function(df) {
  df[df$Distance > 0, ]
}

#' @keywords internal
#' @noRd
# Drop rows with missing / blank / DNF / DNS times.
clean_invalid_times <- function(df) {
  bad <- is.na(df$Time) | df$Time %in% c("", "DNF", "DNS")
  df[!bad, ]
}

#' @keywords internal
#' @noRd
# Closest available distance to a requested one.
closest_distance <- function(requested, available) {
  available[which.min(abs(available - requested))]
}


# ---- Markers and splits ---------------------------------------------

#' @keywords internal
#' @noRd
# Parse a marker string: "mm:ss" is a pace, "hh:mm:ss" is a finish time.
parse_marker <- function(x) {
  parts <- as.numeric(strsplit(x, ":")[[1]])

  if (length(parts) == 2) {                       # mm:ss -> pace
    return(list(type = "pace", value = parts[1] * 60 + parts[2]))
  }
  if (length(parts) == 3) {                       # hh:mm:ss -> finish time
    return(list(type = "time",
                value = parts[1] * 3600 + parts[2] * 60 + parts[3]))
  }
  stop("Marker must be mm:ss or hh:mm:ss")
}

#' @keywords internal
#' @noRd
# Positive split in seconds: finish - 2 * half. A negative result means
# the second half was quicker, i.e. a negative split.
compute_positive_split <- function(half_sec, finish_sec) {
  finish_sec - 2 * half_sec
}
