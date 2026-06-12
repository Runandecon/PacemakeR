# -------------------------------------------------------------------
# S3 class 'pacemaker'
# Wraps a pacing-curve result so it prints, plots and summarises
# nicely instead of being a bare list.
# -------------------------------------------------------------------

# Constructor. Internal; users get one back from pacemaker().
new_pacemaker <- function(curve, plot = NULL, meta = list()) {
  stopifnot(tibble::is_tibble(curve))
  structure(list(curve = curve, plot = plot, meta = meta),
            class = "pacemaker")
}

# Small NULL fallback helper used by the print method.
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Methods for `pacemaker` objects
#'
#' `print`, `plot`, `summary` and `is_pacemaker` for the object returned
#' by [pacemaker()].
#'
#' @param x,object A `pacemaker` object.
#' @param ... Unused; present for method consistency.
#' @return `is_pacemaker()` a logical; `summary()` a tibble;
#'   `print()`/`plot()` their input, invisibly.
#' @name pacemaker-class
NULL

#' @rdname pacemaker-class
#' @export
is_pacemaker <- function(x) inherits(x, "pacemaker")

#' @rdname pacemaker-class
#' @export
print.pacemaker <- function(x, ...) {
  cat("<pacemaker> relative pacing forecast\n")
  cat(sprintf("  Reference split : %s km\n", x$meta$relative %||% NA))
  cat(sprintf("  Runners used    : %s\n",    x$meta$n_runners %||% NA))
  cat(sprintf("  Aggregated by   : %s\n", x$meta$agg_fun %||% NA))
  cat(sprintf("  Distance grid   : %d points (%.1f-%.1f km)\n",
              nrow(x$curve), min(x$curve$Distance), max(x$curve$Distance)))
  cat(sprintf("  Plot attached   : %s\n",    !is.null(x$plot)))
  invisible(x)
}

#' @rdname pacemaker-class
#' @export
plot.pacemaker <- function(x, ...) {
  if (is.null(x$plot)) {
    stop("No plot stored. Re-run with return_plot = TRUE.", call. = FALSE)
  }
  print(x$plot)
  invisible(x)
}

#' @rdname pacemaker-class
#' @export
summary.pacemaker <- function(object, ...) {
  curve <- object$curve
  tibble::tibble(
    metric = c("min_rel", "max_rel", "mean_rel", "fade_at_finish"),
    value  = c(min(curve$rel_agg),  max(curve$rel_agg),
               mean(curve$rel_agg), curve$rel_agg[length(curve$rel_agg)])
  )
}
