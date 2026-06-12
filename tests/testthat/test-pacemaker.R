# tests/testthat/test-pacemaker.R

# ---- Shared fixture ------------------------------------------------------
# Two runners, splits at 10/21.0975/30/42.195 km. Bib 1 negative-splits,
# Bib 2 positive-splits. Built so finish = max(Distance).

make_test_data <- function() {
  tibble::tibble(
    Name     = c(rep("Alpha", 4), rep("Beta", 4)),
    Bib      = c(rep(1, 4), rep(2, 4)),
    Distance = rep(c(10, 21.0975, 30, 42.195), 2),
    Time     = c(
      # Alpha — second half faster (negative split)
      "00:35:00", "01:14:00", "01:44:00", "02:25:00",
      # Beta — second half slower (positive split)
      "00:32:00", "01:08:00", "01:42:00", "02:30:00"
    )
  )
}

# ==========================================================================
# Formatting helpers
# ==========================================================================
test_that("format_pace_mmss formats seconds as mm:ss", {
  expect_equal(format_pace_mmss(0),   "00:00")
  expect_equal(format_pace_mmss(65),  "01:05")
  expect_equal(format_pace_mmss(300), "05:00")
})

test_that("format_time_hms formats seconds as hh:mm:ss", {
  expect_equal(format_time_hms(0),     "00:00:00")
  expect_equal(format_time_hms(3661),  "01:01:01")
  expect_equal(format_time_hms(7325),  "02:02:05")
})

test_that("format_percent_rel produces signed percentages", {
  expect_equal(format_percent_rel(1.05), "+5%")
  expect_equal(format_percent_rel(0.90), "-10%")
  expect_equal(format_percent_rel(1.00), "+0%")
})

# ==========================================================================
# Time parsing / cleaning
# ==========================================================================
test_that("safe_hms_to_sec parses valid times and NAs out junk", {
  out <- safe_hms_to_sec(c("00:01:00", "01:00:00", "garbage", NA))
  expect_equal(out[1], 60)
  expect_equal(out[2], 3600)
  expect_true(is.na(out[3]))
  expect_true(is.na(out[4]))
})

test_that("clean_invalid_times drops blanks, DNF, DNS and NA", {
  df <- tibble::tibble(Time = c("01:00:00", "", "DNF", "DNS", NA))
  out <- clean_invalid_times(df)
  expect_equal(nrow(out), 1)
  expect_equal(out$Time, "01:00:00")
})

test_that("drop_zero_distance removes the synthetic 0 km row", {
  df <- tibble::tibble(Distance = c(0, 5, 10))
  expect_equal(drop_zero_distance(df)$Distance, c(5, 10))
})

test_that("closest_distance finds the nearest available split", {
  expect_equal(closest_distance(31, c(10, 30, 35)), 30)
  expect_equal(closest_distance(34, c(10, 30, 35)), 35)
})

# ==========================================================================
# Markers
# ==========================================================================
test_that("parse_marker distinguishes pace from finish time", {
  p <- parse_marker("05:00")
  expect_equal(p$type, "pace")
  expect_equal(p$value, 300)

  t <- parse_marker("02:30:00")
  expect_equal(t$type, "time")
  expect_equal(t$value, 9000)
})

test_that("parse_marker rejects malformed markers", {
  expect_error(parse_marker("5"), "mm:ss or hh:mm:ss")
  expect_error(parse_marker("1:2:3:4"), "mm:ss or hh:mm:ss")
})

test_that("compute_positive_split sign reflects split direction", {
  expect_lt(compute_positive_split(3600, 7000), 0)  # 2nd half faster
  expect_gt(compute_positive_split(3600, 7400), 0)  # 2nd half slower
})

# ==========================================================================
# Split / metric computation
# ==========================================================================
test_that("compute_split_table inserts a zero start and computes pace", {
  d   <- make_test_data()
  tbl <- compute_split_table(d, "Bib", 1)

  expect_true(all(c("SplitTime", "SplitDist", "Pace_sec_per_km") %in% names(tbl)))
  expect_true(all(is.finite(tbl$Pace_sec_per_km)))
  expect_true(all(tbl$Pace_sec_per_km > 0))
})

test_that("compute_split_table errors on unknown runner", {
  expect_error(compute_split_table(make_test_data(), "Bib", 999),
               "No matching runner")
})

test_that("compute_time_metric_values returns finite finish times", {
  rows <- get_finish_rows(make_test_data())
  vals <- compute_time_metric_values(rows)
  expect_length(vals, 2)
  expect_true(all(is.finite(vals)))
})

test_that("compute_pace_metric_values returns positive paces", {
  rows <- get_finish_rows(make_test_data())
  vals <- compute_pace_metric_values(rows)
  expect_true(all(vals > 0))
  expect_true(all(is.finite(vals)))
})

test_that("compute_relative_pace handles avg, ref and none modes", {
  tbl <- compute_split_table(make_test_data(), "Bib", 1)

  avg <- compute_relative_pace(tbl, TRUE)
  expect_equal(avg$mode, "avg")
  expect_false(is.null(avg$rel_values))

  ref <- compute_relative_pace(tbl, 30)
  expect_equal(ref$mode, "ref")

  none <- compute_relative_pace(tbl, NULL)
  expect_equal(none$mode, "none")
  expect_null(none$rel_values)
})

test_that("compute_relative_pace errors on a missing reference split", {
  tbl <- compute_split_table(make_test_data(), "Bib", 1)
  expect_error(compute_relative_pace(tbl, 99), "not found")
})

# ==========================================================================
# Row selection
# ==========================================================================
test_that("get_finish_rows returns one row per runner at max distance", {
  rows <- get_finish_rows(make_test_data())
  expect_equal(nrow(rows), 2)
  expect_true(all(rows$Distance == 42.195))
})

test_that("get_split_rows warns and snaps to the nearest split", {
  expect_warning(out <- get_split_rows(make_test_data(), 31),
                 "closest available")
  expect_true(all(out$Distance == 30))
})

test_that("get_split_rows returns the exact split without warning", {
  expect_silent(out <- get_split_rows(make_test_data(), 30))
  expect_true(all(out$Distance == 30))
})

# ==========================================================================
# marathon_summary
# ==========================================================================
test_that("marathon_summary requires Distance and Time columns", {
  expect_error(marathon_summary(tibble::tibble(x = 1)),
               "'Distance' and 'Time'")
})

test_that("marathon_summary returns the five summary metrics (time)", {
  out <- marathon_summary(make_test_data())
  expect_s3_class(out, "tbl_df")
  expect_equal(out$metric, c("mean", "median", "min", "max", "sd"))
  expect_true(all(c("seconds", "formatted") %in% names(out)))
  expect_true(all(is.finite(out$seconds[1:4])))
})

test_that("marathon_summary works in pace mode", {
  out <- marathon_summary(make_test_data(), pace = TRUE)
  expect_equal(out$metric, c("mean", "median", "min", "max", "sd"))
  # paces are in sec/km, far smaller than finish times in sec
  expect_lt(out$seconds[out$metric == "mean"], 1000)
})

test_that("marathon_summary accepts a numeric split distance", {
  out <- marathon_summary(make_test_data(), distance = 30)
  expect_equal(nrow(out), 5)
})

test_that("marathon_summary rejects a non-numeric, non-'finish' distance", {
  expect_error(marathon_summary(make_test_data(), distance = "halfway"),
               "'finish' or numeric")
})

# ==========================================================================
# pacemaker() + S3 class
# ==========================================================================
test_that("pacemaker returns a valid pacemaker object", {
  obj <- pacemaker(make_test_data(), relative = 30)
  expect_s3_class(obj, "pacemaker")
  expect_true(is_pacemaker(obj))
  expect_false(is_pacemaker(list()))

  expect_true(tibble::is_tibble(obj$curve))
  expect_true(all(c("rel_agg", "rel_ci_low", "rel_ci_high",
                    "rel_neg_split") %in% names(obj$curve)))
  # reference split normalises to 1
  expect_equal(obj$curve$rel_agg[obj$curve$Distance == 30], 1,
               tolerance = 1e-8)
})

test_that("pacemaker records meta information", {
  obj <- pacemaker(make_test_data(), relative = 30, agg_fun = "median")
  expect_equal(obj$meta$relative, 30)
  expect_equal(obj$meta$agg_fun, "median")
  expect_equal(obj$meta$n_runners, 2)
})

test_that("pacemaker errors when no runner reaches the reference split", {
  expect_error(pacemaker(make_test_data(), relative = 99),
               "reference split")
})

test_that("pacemaker requires a Bib or Name column", {
  d <- make_test_data()
  d$Bib <- NULL; d$Name <- NULL
  expect_error(pacemaker(d), "'Bib' or 'Name'")
})

test_that("print.pacemaker returns its input invisibly", {
  obj <- pacemaker(make_test_data(), relative = 30)
  expect_output(print(obj), "pacemaker")
  expect_invisible(print(obj))
})

test_that("plot.pacemaker draws curve and gain views invisibly", {
  obj <- pacemaker(make_test_data(), relative = 30)

  expect_invisible(p <- plot(obj))                 # curve (default)
  expect_invisible(plot(obj, which = "gain"))      # gain in metres
})

test_that("plot.pacemaker gain in time requires pace_sec", {
  obj <- pacemaker(make_test_data(), relative = 30)
  skip_if_not(isTRUE(obj$meta$has_negative))
  expect_error(plot(obj, which = "gain", unit = "time"),
               "pace_sec must be provided")
})

# ==========================================================================
# plot_pace_splits
# ==========================================================================
test_that("plot_pace_splits requires an id", {
  expect_error(plot_pace_splits(make_test_data()),
               'Bib/Name or "ALL"')
})

test_that("plot_pace_splits returns a ggplot for one runner", {
  p <- plot_pace_splits(make_test_data(), id = 1)
  expect_s3_class(p, "ggplot")
})

test_that("plot_pace_splits handles ALL and reference modes", {
  expect_s3_class(plot_pace_splits(make_test_data(), id = "ALL"), "ggplot")
  expect_s3_class(
    plot_pace_splits(make_test_data(), id = 1, distance = 30, reference = "pct"),
    "ggplot"
  )
  expect_s3_class(
    plot_pace_splits(make_test_data(), id = 1, distance = TRUE, reference = "pace"),
    "ggplot"
  )
})
