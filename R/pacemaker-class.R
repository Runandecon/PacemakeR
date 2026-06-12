# -------------------------------------------------------------------
# S3 class 'pacemaker'
# Holds the whole analysis from pacemaker() so it prints as a table and
# plots its different views, instead of being a bare list.
# -------------------------------------------------------------------

# Small NULL fallback used by the print method.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Constructor. Internal; users get one back from pacemaker().
new_pacemaker <- function(curve, cumdiff_km, summary, meta = list()) {
  stopifnot(tibble::is_tibble(curve))
  structure(list(curve = curve, cumdiff_km = cumdiff_km,
                 summary = summary, meta = meta),
            class = "pacemaker")
}

#' Methods for `pacemaker` objects
#'
#' `print`, `plot` and `is_pacemaker` for the object returned by
#' [pacemaker()]. `print()` shows the summary table; `plot()` draws either
#' the pacing curve or the cumulative negative-split advantage.
#'
#' @param x A `pacemaker` object.
#' @param which Which view to plot: `"curve"` (default) or `"gain"`.
#' @param unit For the gain view, `"meter"` (default) or `"time"`.
#' @param pace_sec Reference pace in sec/km; required when `unit = "time"`.
#' @param ... Unused; present for method consistency.
#' @return `is_pacemaker()` a logical; `print()`/`plot()` their input,
#'   invisibly.
#' @name pacemaker-class
NULL

#' @rdname pacemaker-class
#' @export
is_pacemaker <- function(x) inherits(x, "pacemaker")

#' @rdname pacemaker-class
#' @export
print.pacemaker <- function(x, ...) {
  cat("<pacemaker> pacing analysis\n")
  cat(sprintf("  Reference split : %s km\n", x$meta$relative %||% NA))
  cat(sprintf("  Runners used    : %s\n",    x$meta$n_runners %||% NA))
  cat(sprintf("  Aggregated by   : %s\n",    x$meta$agg_fun %||% NA))
  cat("\n")
  print(x$summary)
  invisible(x)
}

#' @rdname pacemaker-class
#' @export
plot.pacemaker <- function(x, which = c("curve", "gain"),
                           unit = c("meter", "time"), pace_sec = NULL, ...) {

  which <- match.arg(which)
  unit  <- match.arg(unit)

  if (which == "curve") {
    p <- ggplot2::ggplot(x$curve, ggplot2::aes(x = Distance)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = rel_ci_low, ymax = rel_ci_high),
        fill = "grey80", alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = rel_agg),
                         colour = "steelblue", linewidth = 1.2) +
      ggplot2::geom_line(ggplot2::aes(y = rel_neg_split),
                         colour = "purple", linetype = "dotdash") +
      ggplot2::labs(
        x = "Distance (km)",
        y = paste0("Relative pace (ref = ", x$meta$relative, " km)"),
        title = "PacemakeR Relative Pacing Curve",
        subtitle = paste("Aggregated using", x$meta$agg_fun)) +
      ggplot2::theme_minimal(base_size = 14)

  } else {  # which == "gain"
    if (!isTRUE(x$meta$has_negative)) {
      stop("No negative-split subgroup available for a gain plot.",
           call. = FALSE)
    }
    if (unit == "time" && is.null(pace_sec)) {
      stop("pace_sec must be provided when unit = 'time'.", call. = FALSE)
    }

    # Scale the stored cumulative km difference to the requested unit.
    cumdiff <- if (unit == "meter") x$cumdiff_km * 1000
    else x$cumdiff_km * pace_sec
    ylab  <- if (unit == "time") "Cumulative difference (seconds)"
    else "Cumulative difference (meters)"
    title <- if (unit == "time")
      "Cumulative Time Advantage of Negative-Split Pacing"
    else
      "Cumulative Distance Advantage of Negative-Split Pacing"

    # Start the line at the origin.
    tbl <- dplyr::bind_rows(
      tibble::tibble(Distance = 0, cumdiff = 0),
      tibble::tibble(Distance = x$curve$Distance, cumdiff = cumdiff)
    )

    p <- ggplot2::ggplot(tbl, ggplot2::aes(x = Distance, y = cumdiff)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::geom_line(color = "firebrick", linewidth = 1.2) +
      ggplot2::labs(x = "Distance (km)", y = ylab, title = title) +
      ggplot2::theme_minimal(base_size = 14)
  }

  print(p)
  invisible(x)
}
