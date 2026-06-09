pacemaker <- function(data,
                      relative    = 30,
                      n_boot      = 6000,
                      cores       = max(1, parallel::detectCores() - 1),
                      return_plot = TRUE) {

  # --- Identify ID column ---------------------------------------------------
  if ("Bib" %in% names(data)) id_col <- "Bib"
  else if ("Name" %in% names(data)) id_col <- "Name"
  else stop("Data must contain 'Bib' or 'Name'.")

  ids <- unique(data[[id_col]])
  marathon_dist <- max(data$Distance, na.rm = TRUE)

  # --- Build per-runner split tables ---------------------------------------
  split_list <- lapply(ids, function(id) compute_split_table(data, id_col, id))
  names(split_list) <- ids

  # --- Keep only runners with the reference split ---------------------------
  has_ref <- sapply(split_list, function(df) any(df$Distance == relative))
  split_list <- split_list[has_ref]
  ids        <- ids[has_ref]

  if (length(split_list) == 0)
    stop("No runners with the chosen reference split found.")

  # Common distance grid
  dist_grid <- sort(unique(split_list[[1]]$Distance))
  if (!relative %in% dist_grid)
    stop("Reference split not in common split grid.")

  idx_ref <- which(dist_grid == relative)

  # --- Single-runner bootstrap function ------------------------------------
  boot_fun <- function(b) {

    # sample ONE runner
    i <- sample(seq_along(split_list), size = 1, replace = TRUE)
    df <- split_list[[i]]

    pace_vec <- df$Pace_sec_per_km[match(dist_grid, df$Distance)]

    if (any(!is.finite(pace_vec)) || any(pace_vec <= 0))
      return(rep(NA_real_, length(dist_grid)))

    # raw relative pace
    rel_curve <- pace_vec[idx_ref] / pace_vec

    # renormalize so rel(reference) = 1
    rel_curve / rel_curve[idx_ref]
  }

  # --- Windows-safe parallel backend ---------------------------------------
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterExport(
    cl,
    varlist = c("split_list", "dist_grid", "idx_ref"),
    envir = environment()
  )

  boot_res <- parallel::parLapply(cl, seq_len(n_boot), boot_fun)

  boot_mat <- do.call(rbind, boot_res)
  boot_mat <- boot_mat[rowSums(is.finite(boot_mat)) > 0, , drop = FALSE]

  # --- Summary curves -------------------------------------------------------
  rel_mean  <- apply(boot_mat, 2, mean, na.rm = TRUE)
  rel_low   <- apply(boot_mat, 2, quantile, 0.05, na.rm = TRUE)
  rel_high  <- apply(boot_mat, 2, quantile, 0.95, na.rm = TRUE)
  rel_best  <- apply(boot_mat, 2, max, na.rm = TRUE)
  rel_worst <- apply(boot_mat, 2, min, na.rm = TRUE)

  # --- Historical negative-split curve -------------------------------------
  neg_ids <- sapply(split_list, function(df) {
    half_dist  <- marathon_dist / 2
    half_row   <- df[which.min(abs(df$Distance - half_dist)), ]
    finish_row <- df[which.max(df$Distance), ]
    compute_positive_split(half_row$Time_sec, finish_row$Time_sec) < 0
  })

  if (any(neg_ids)) {
    neg_list <- split_list[neg_ids]
    neg_mat <- sapply(neg_list, function(df) {
      pace_vec <- df$Pace_sec_per_km[match(dist_grid, df$Distance)]
      if (any(!is.finite(pace_vec)) || any(pace_vec <= 0))
        return(rep(NA_real_, length(dist_grid)))

      rel_curve <- pace_vec[idx_ref] / pace_vec
      rel_curve / rel_curve[idx_ref]
    })
    rel_neg <- rowMeans(neg_mat, na.rm = TRUE)
  } else {
    rel_neg <- rep(NA_real_, length(dist_grid))
  }

  # --- Output tibble --------------------------------------------------------
  curve_tbl <- tibble::tibble(
    Distance      = dist_grid,
    rel_mean      = rel_mean,
    rel_ci_low    = rel_low,
    rel_ci_high   = rel_high,
    rel_best      = rel_best,
    rel_worst     = rel_worst,
    rel_neg_split = rel_neg
  )

  # --- Optional plot --------------------------------------------------------
  plot_obj <- NULL
  if (return_plot) {
    plot_obj <- ggplot2::ggplot(curve_tbl, ggplot2::aes(x = Distance)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = rel_ci_low, ymax = rel_ci_high),
        fill = "grey80", alpha = 0.6
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = rel_mean),
        colour = "steelblue", linewidth = 1.2
      ) +
      # ggplot2::geom_line(
      #   ggplot2::aes(y = rel_best),
      #   colour = "darkgreen", linetype = "dashed"
      # ) +
      # ggplot2::geom_line(
      #   ggplot2::aes(y = rel_worst),
      #   colour = "red", linetype = "dashed"
      # ) +
      ggplot2::geom_line(
        ggplot2::aes(y = rel_neg_split),
        colour = "purple", linetype = "dotdash"
      ) +
      ggplot2::labs(
        x = "Distance (km)",
        y = paste0("Relative pace (ref = ", relative, " km)"),
        title = "PacemakeR Relative Pacing Forecast",
        subtitle = "Single-runner bootstrap: realistic CI, best/worst, negative-split"
      ) +
      ggplot2::theme_minimal(base_size = 14)
  }

  list(curve = curve_tbl, plot = plot_obj)
}
