#!/usr/bin/env Rscript

script_args <- commandArgs(trailingOnly = TRUE)
full_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", full_args, value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else NA_character_
script_dir <- if (!is.na(script_path)) dirname(normalizePath(script_path)) else getwd()

source(file.path(script_dir, "analysis_utils.R"))
source(file.path(script_dir, "analysis_metrics.R"))
source(file.path(script_dir, "analysis_plots.R"))

main <- function() {
  opts <- parse_args(script_args)

  csv_files <- list.files(opts$root, pattern = "^analysis_.*\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    stop(sprintf("No analysis CSV files found in %s", opts$root))
  }

  metas <- lapply(csv_files, parse_file_metadata)
  keep <- vapply(metas, function(m) !is.null(m), logical(1))
  csv_files <- csv_files[keep]
  metas <- metas[keep]

  if (length(csv_files) == 0) {
    stop("No files matched expected naming pattern analysis_<protocol>_<bitrate>kbps_<mtu>mtu.csv")
  }

  proto_keep <- vapply(metas, function(m) m$protocol %in% opts$protocols, logical(1))
  csv_files <- csv_files[proto_keep]
  metas <- metas[proto_keep]

  if (length(csv_files) == 0) {
    stop(sprintf(
      "No files remained after protocol filter: %s",
      paste(opts$protocols, collapse = ",")
    ))
  }

  run_rows <- list()
  throughput_rows <- list()
  stutter_rows <- list()
  streak_rows <- list()

  for (i in seq_along(csv_files)) {
    path <- csv_files[[i]]
    meta <- metas[[i]]
    message(sprintf("Processing %s", basename(path)))

    df <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE),
      error = function(e) stop(sprintf("Failed reading %s: %s", path, e$message))
    )

    result <- compute_run_metrics(df, meta, opts$stall_ms, opts$window_ms)
    run_rows[[length(run_rows) + 1]] <- result$run_metrics
    throughput_rows[[length(throughput_rows) + 1]] <- result$throughput_ts
    stutter_rows[[length(stutter_rows) + 1]] <- result$stutter_events
    streak_rows[[length(streak_rows) + 1]] <- result$misorder_streak_events
  }

  per_run <- do.call(rbind, run_rows)
  per_run <- per_run[order(per_run$protocol, per_run$bitrate_kbps, per_run$mtu), ]

  throughput_ts <- if (length(throughput_rows) > 0) do.call(rbind, throughput_rows) else data.frame()
  stutter_events <- if (length(stutter_rows) > 0) do.call(rbind, stutter_rows) else data.frame()
  misorder_streak_events <- if (length(streak_rows) > 0) do.call(rbind, streak_rows) else data.frame()

  plot_outputs(per_run, throughput_ts, misorder_streak_events, file.path(opts$root, "analysis"))
  export_summary_tables(
    per_run,
    aggregate_group_metrics(per_run),
    stutter_events,
    misorder_streak_events,
    file.path(opts$root, "analysis", "out")
  )

  proto_groups <- split(per_run, per_run$protocol)
  for (proto in names(proto_groups)) {
    g <- proto_groups[[proto]]
    avg_reorder_rate <- mean(g$reorder_rate_pct, na.rm = TRUE)
    avg_reorder_events <- mean(g$reorder_events, na.rm = TRUE)
    avg_misordered_amount <- mean(g$misordered_packet_count, na.rm = TRUE)
    avg_magnitude <- mean(g$reorder_depth_mean_misordered, na.rm = TRUE)
    avg_reorder_depth_max <- mean(g$reorder_depth_max, na.rm = TRUE)
    message(sprintf(
      "Misorder summary [%s] -> avg rate: %.3f%%, avg events: %.2f, avg amount: %.2f, avg magnitude: %.2f, avg max depth: %.2f",
      proto,
      avg_reorder_rate,
      avg_reorder_events,
      avg_misordered_amount,
      avg_magnitude,
      avg_reorder_depth_max
    ))
  }

  message("Analysis complete.")
  message(sprintf("Runs processed: %d", nrow(per_run)))
  message(sprintf("Plots: %s", file.path(opts$root, "analysis", "plots")))
  message(sprintf("Summaries: %s", file.path(opts$root, "analysis", "out")))
}

main()
