#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args_vec) {
  opts <- list(
    root = ".",
    protocols = c("stream", "gossipsub"),
    stall_ms = 120,
    window_ms = 1000
  )

  i <- 1
  while (i <= length(args_vec)) {
    arg <- args_vec[[i]]

    if (arg == "--root" && i < length(args_vec)) {
      opts$root <- args_vec[[i + 1]]
      i <- i + 2
    } else if (arg == "--protocols" && i < length(args_vec)) {
      opts$protocols <- strsplit(args_vec[[i + 1]], ",")[[1]]
      opts$protocols <- trimws(opts$protocols)
      opts$protocols <- opts$protocols[opts$protocols != ""]
      i <- i + 2
    } else if (arg == "--stall-ms" && i < length(args_vec)) {
      opts$stall_ms <- as.numeric(args_vec[[i + 1]])
      i <- i + 2
    } else if (arg == "--window-ms" && i < length(args_vec)) {
      opts$window_ms <- as.numeric(args_vec[[i + 1]])
      i <- i + 2
    } else {
      stop(sprintf("Unknown or incomplete argument: %s", arg))
    }
  }

  if (length(opts$protocols) == 0) {
    stop("No protocols provided after --protocols")
  }

  if (is.na(opts$stall_ms) || opts$stall_ms <= 0) {
    stop("--stall-ms must be a positive number")
  }

  if (is.na(opts$window_ms) || opts$window_ms <= 0) {
    stop("--window-ms must be a positive number")
  }

  opts
}

parse_file_metadata <- function(path) {
  base <- basename(path)
  match <- regexec("^analysis_([a-zA-Z0-9-]+)_([0-9]+)kbps_([0-9]+)mtu\\.csv$", base)
  pieces <- regmatches(base, match)[[1]]

  if (length(pieces) != 4) {
    return(NULL)
  }

  list(
    file = base,
    protocol = pieces[[2]],
    bitrate_kbps = as.numeric(pieces[[3]]),
    mtu = as.numeric(pieces[[4]])
  )
}

safe_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out
}

compute_run_metrics <- function(df, meta, stall_ms, window_ms) {
  required_cols <- c("creation_time", "arrival_time", "index", "size")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required columns in %s: %s",
      meta$file,
      paste(missing_cols, collapse = ", ")
    ))
  }

  df$creation_time <- safe_num(df$creation_time)
  df$arrival_time <- safe_num(df$arrival_time)
  df$index <- safe_num(df$index)
  df$size <- safe_num(df$size)

  df <- df[complete.cases(df[, required_cols]), ]
  if (nrow(df) == 0) {
    stop(sprintf("No valid rows in %s", meta$file))
  }

  # Keep observed arrival order for inter-arrival and stutter analysis.
  df <- df[order(df$arrival_time, df$index), ]

  delay_ms <- df$arrival_time - df$creation_time
  inter_arrival_ms <- c(NA, diff(df$arrival_time))
  inter_creation_ms <- c(NA, diff(df$creation_time))
  abs_delay_delta_ms <- c(NA, abs(diff(delay_ms)))

  run_duration_ms <- max(df$arrival_time) - min(df$arrival_time)
  run_duration_s <- ifelse(run_duration_ms > 0, run_duration_ms / 1000, NA_real_)

  total_bytes <- sum(df$size, na.rm = TRUE)
  goodput_mbps <- ifelse(is.na(run_duration_s) || run_duration_s <= 0, NA_real_, (total_bytes * 8) / run_duration_s / 1e6)

  unique_indices <- sort(unique(df$index))
  idx_min <- min(unique_indices)
  idx_max <- max(unique_indices)
  expected_count <- as.numeric(idx_max - idx_min + 1)
  observed_unique_count <- length(unique_indices)
  duplicate_count <- nrow(df) - observed_unique_count

  missing_count <- if (expected_count <= 0) 0 else expected_count - observed_unique_count
  missing_count <- max(missing_count, 0)
  inferred_loss_pct <- ifelse(expected_count <= 0, NA_real_, (missing_count / expected_count) * 100)

  idx_diff <- diff(df$index)
  reorder_events <- sum(idx_diff < 0, na.rm = TRUE)
  reorder_rate_pct <- ifelse(length(idx_diff) == 0, 0, (reorder_events / length(idx_diff)) * 100)

  running_max_idx <- cummax(df$index)
  reorder_depth <- pmax(running_max_idx - df$index, 0)
  misordered_packet_mask <- reorder_depth > 0
  misordered_packet_count <- sum(misordered_packet_mask, na.rm = TRUE)
  misordered_packet_rate_pct <- ifelse(nrow(df) == 0, 0, (misordered_packet_count / nrow(df)) * 100)
  misordered_depth_values <- reorder_depth[misordered_packet_mask]
  reorder_depth_max <- max(reorder_depth, na.rm = TRUE)
  reorder_depth_mean_misordered <- ifelse(
    misordered_packet_count > 0,
    mean(misordered_depth_values, na.rm = TRUE),
    0
  )
  reorder_depth_p95_misordered <- ifelse(
    misordered_packet_count > 0,
    suppressWarnings(as.numeric(quantile(misordered_depth_values, probs = 0.95, na.rm = TRUE))),
    0
  )
  reorder_depth_total <- sum(misordered_depth_values, na.rm = TRUE)

  stutter_mask <- !is.na(inter_arrival_ms) & inter_arrival_ms > stall_ms
  stutter_count <- sum(stutter_mask)
  stutter_total_ms <- sum(inter_arrival_ms[stutter_mask], na.rm = TRUE)
  stutter_longest_ms <- ifelse(stutter_count > 0, max(inter_arrival_ms[stutter_mask], na.rm = TRUE), 0)

  delay_mean <- mean(delay_ms, na.rm = TRUE)
  delay_sd <- sd(delay_ms, na.rm = TRUE)
  delay_var <- var(delay_ms, na.rm = TRUE)
  delay_cv <- ifelse(is.na(delay_mean) || delay_mean == 0, NA_real_, delay_sd / delay_mean)

  inter_arrival_mean <- mean(inter_arrival_ms, na.rm = TRUE)
  inter_arrival_sd <- sd(inter_arrival_ms, na.rm = TRUE)
  inter_arrival_cv <- ifelse(is.na(inter_arrival_mean) || inter_arrival_mean == 0, NA_real_, inter_arrival_sd / inter_arrival_mean)

  jitter_mean <- mean(abs_delay_delta_ms, na.rm = TRUE)
  jitter_p95 <- suppressWarnings(as.numeric(quantile(abs_delay_delta_ms, probs = 0.95, na.rm = TRUE)))

  target_interarrival_ms <- ifelse(meta$bitrate_kbps > 0, (mean(df$size, na.rm = TRUE) * 8) / as.numeric(meta$bitrate_kbps), NA_real_)
  stability_score <- 100 - (pmin(100, delay_cv * 100) * 0.5 + pmin(100, inter_arrival_cv * 100) * 0.5)
  stability_score <- pmax(stability_score, 0)

  # Throughput time series by fixed windows from first arrival.
  t0 <- min(df$arrival_time)
  bucket <- floor((df$arrival_time - t0) / window_ms)
  bucket_bytes <- tapply(df$size, bucket, sum)
  throughput_ts <- data.frame(
    file = meta$file,
    protocol = meta$protocol,
    bitrate_kbps = meta$bitrate_kbps,
    mtu = meta$mtu,
    bucket = as.numeric(names(bucket_bytes)),
    bytes = as.numeric(bucket_bytes)
  )
  throughput_ts$window_start_ms <- throughput_ts$bucket * window_ms
  throughput_ts$throughput_mbps <- (throughput_ts$bytes * 8) / (window_ms / 1000) / 1e6

  stutter_events <- data.frame(
    file = meta$file,
    protocol = meta$protocol,
    bitrate_kbps = meta$bitrate_kbps,
    mtu = meta$mtu,
    index = df$index,
    arrival_time = df$arrival_time,
    inter_arrival_ms = inter_arrival_ms,
    is_stutter = stutter_mask
  )
  stutter_events <- stutter_events[stutter_events$is_stutter, c(
    "file", "protocol", "bitrate_kbps", "mtu", "index", "arrival_time", "inter_arrival_ms"
  )]

  run_metrics <- data.frame(
    file = meta$file,
    protocol = meta$protocol,
    bitrate_kbps = meta$bitrate_kbps,
    mtu = meta$mtu,
    row_count = nrow(df),
    unique_index_count = observed_unique_count,
    expected_index_count = expected_count,
    inferred_missing_count = missing_count,
    inferred_loss_pct = inferred_loss_pct,
    duplicate_count = duplicate_count,
    duplicate_rate_pct = ifelse(nrow(df) == 0, NA_real_, duplicate_count / nrow(df) * 100),
    reorder_events = reorder_events,
    reorder_rate_pct = reorder_rate_pct,
    misordered_packet_count = misordered_packet_count,
    misordered_packet_rate_pct = misordered_packet_rate_pct,
    reorder_depth_max = reorder_depth_max,
    reorder_depth_mean_misordered = reorder_depth_mean_misordered,
    reorder_depth_p95_misordered = reorder_depth_p95_misordered,
    reorder_depth_total = reorder_depth_total,
    run_duration_ms = run_duration_ms,
    total_bytes = total_bytes,
    goodput_mbps = goodput_mbps,
    delay_mean_ms = delay_mean,
    delay_sd_ms = delay_sd,
    delay_variance_ms2 = delay_var,
    delay_cv = delay_cv,
    delay_p50_ms = suppressWarnings(as.numeric(quantile(delay_ms, probs = 0.50, na.rm = TRUE))),
    delay_p95_ms = suppressWarnings(as.numeric(quantile(delay_ms, probs = 0.95, na.rm = TRUE))),
    delay_p99_ms = suppressWarnings(as.numeric(quantile(delay_ms, probs = 0.99, na.rm = TRUE))),
    delay_max_ms = max(delay_ms, na.rm = TRUE),
    jitter_mean_ms = jitter_mean,
    jitter_p95_ms = jitter_p95,
    inter_arrival_mean_ms = inter_arrival_mean,
    inter_arrival_sd_ms = inter_arrival_sd,
    inter_arrival_cv = inter_arrival_cv,
    inter_creation_mean_ms = mean(inter_creation_ms, na.rm = TRUE),
    stutter_threshold_ms = stall_ms,
    stutter_count = stutter_count,
    stutter_total_ms = stutter_total_ms,
    stutter_longest_ms = stutter_longest_ms,
    target_interarrival_ms = target_interarrival_ms,
    stability_score_0_100 = stability_score
  )

  list(run_metrics = run_metrics, stutter_events = stutter_events, throughput_ts = throughput_ts)
}

aggregate_group_metrics <- function(per_run) {
  numeric_cols <- names(per_run)[sapply(per_run, is.numeric)]
  group_keys <- c("protocol", "bitrate_kbps", "mtu")

  groups <- split(per_run, interaction(per_run$protocol, per_run$bitrate_kbps, per_run$mtu, drop = TRUE))
  out <- lapply(groups, function(g) {
    row <- g[1, group_keys, drop = FALSE]
    for (col in numeric_cols) {
      row[[paste0(col, "_mean")]] <- mean(g[[col]], na.rm = TRUE)
      row[[paste0(col, "_sd")]] <- sd(g[[col]], na.rm = TRUE)
    }
    row$run_count <- nrow(g)
    row
  })

  do.call(rbind, out)
}

plot_outputs <- function(per_run, throughput_ts, out_dir) {
  plot_dir <- file.path(out_dir, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  preferred_protocol_order <- c("stream", "gossipsub")
  protocol_levels <- unique(as.character(per_run$protocol))
  protocol_levels <- c(
    preferred_protocol_order[preferred_protocol_order %in% protocol_levels],
    sort(setdiff(protocol_levels, preferred_protocol_order))
  )

  protocol_palette <- setNames(rep("gray50", length(protocol_levels)), protocol_levels)
  if ("stream" %in% protocol_levels) {
    protocol_palette[["stream"]] <- "#1f77b4"
  }
  if ("gossipsub" %in% protocol_levels) {
    protocol_palette[["gossipsub"]] <- "#d62728"
  }

  other_protocols <- setdiff(protocol_levels, c("stream", "gossipsub"))
  if (length(other_protocols) > 0) {
    protocol_palette[other_protocols] <- setNames(rainbow(length(other_protocols)), other_protocols)
  }

  # Delay boxplot by protocol.
  png(file.path(plot_dir, "delay_p95_by_protocol.png"), width = 1200, height = 700)
  box_data <- per_run
  box_data$protocol <- factor(as.character(box_data$protocol), levels = protocol_levels)
  boxplot(delay_p95_ms ~ protocol, data = box_data,
          main = "Delay P95 by Protocol",
          xlab = "Protocol",
          ylab = "Delay P95 (ms)",
          col = unname(protocol_palette[protocol_levels]))
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Inferred loss by run.
  png(file.path(plot_dir, "inferred_loss_by_run.png"), width = 1400, height = 800)
  ord <- order(per_run$protocol, per_run$bitrate_kbps, per_run$mtu)
  ordered_protocols <- as.character(per_run$protocol[ord])
  bar_cols <- unname(protocol_palette[ordered_protocols])
  barplot(per_run$inferred_loss_pct[ord],
          names.arg = per_run$file[ord],
          las = 2,
          cex.names = 0.7,
          col = bar_cols,
          main = "Inferred Packet Loss (Missing Index %) by Run",
          ylab = "Inferred Loss (%)")
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Goodput by run.
  png(file.path(plot_dir, "goodput_by_run.png"), width = 1400, height = 800)
  barplot(per_run$goodput_mbps[ord],
          names.arg = per_run$file[ord],
          las = 2,
          cex.names = 0.7,
          col = bar_cols,
          main = "Goodput by Run",
          ylab = "Goodput (Mbps)")
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Misordered packets (reorder events) by run.
  png(file.path(plot_dir, "misordered_events_by_run.png"), width = 1400, height = 800)
  barplot(per_run$reorder_events[ord],
      names.arg = per_run$file[ord],
      las = 2,
      cex.names = 0.7,
      col = bar_cols,
      main = "Misordered Packets by Run (Reorder Events)",
      ylab = "Reorder Events (count)")
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Misordered packet rate (%) by run.
  png(file.path(plot_dir, "misordered_rate_by_run.png"), width = 1400, height = 800)
  barplot(per_run$reorder_rate_pct[ord],
      names.arg = per_run$file[ord],
      las = 2,
      cex.names = 0.7,
      col = bar_cols,
      main = "Misordered Packet Rate by Run",
      ylab = "Reorder Rate (%)")
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Misordered amount by run (count of packets that arrived out of order).
  png(file.path(plot_dir, "misordered_amount_by_run.png"), width = 1400, height = 800)
  barplot(per_run$misordered_packet_count[ord],
          names.arg = per_run$file[ord],
          las = 2,
          cex.names = 0.7,
          col = bar_cols,
          main = "Misordered Packet Amount by Run",
          ylab = "Misordered Packets (count)")
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Maximum reorder depth by run.
  png(file.path(plot_dir, "misordered_depth_by_run.png"), width = 1400, height = 800)
  barplot(per_run$reorder_depth_max[ord],
      names.arg = per_run$file[ord],
      las = 2,
      cex.names = 0.7,
      col = bar_cols,
      main = "Maximum Reorder Depth by Run",
      ylab = "Max Reorder Depth (indices)")
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Misordered magnitude by run (mean depth among misordered packets).
  png(file.path(plot_dir, "misordered_magnitude_by_run.png"), width = 1400, height = 800)
  barplot(per_run$reorder_depth_mean_misordered[ord],
          names.arg = per_run$file[ord],
          las = 2,
          cex.names = 0.7,
          col = bar_cols,
          main = "Misordered Magnitude by Run",
          ylab = "Mean Reorder Depth (misordered packets only)")
  legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
  dev.off()

  # Throughput timelines.
  png(file.path(plot_dir, "throughput_timeline.png"), width = 1400, height = 800)
  if (nrow(throughput_ts) > 0) {
    throughput_ts <- throughput_ts[order(throughput_ts$protocol, throughput_ts$bitrate_kbps, throughput_ts$mtu, throughput_ts$file, throughput_ts$bucket), ]
    ylim <- range(throughput_ts$throughput_mbps, na.rm = TRUE)
    xlim <- range(throughput_ts$window_start_ms, na.rm = TRUE)
    plot(NA,
         xlim = xlim,
         ylim = ylim,
         xlab = "Window Start (ms)",
         ylab = "Throughput (Mbps)",
         main = "Throughput Timelines")

    run_levels <- unique(throughput_ts$file)
    mtu_levels <- sort(unique(throughput_ts$mtu))
    lty_choices <- c(1, 2, 3, 4, 5, 6)
    mtu_lty <- setNames(lty_choices[((seq_along(mtu_levels) - 1) %% length(lty_choices)) + 1], mtu_levels)

    for (i in seq_along(run_levels)) {
      f <- run_levels[[i]]
      d <- throughput_ts[throughput_ts$file == f, ]
      proto <- as.character(d$protocol[[1]])
      mtu_value <- as.character(d$mtu[[1]])
      line_col <- protocol_palette[[proto]]
      line_lty <- mtu_lty[[mtu_value]]
      lines(d$window_start_ms, d$throughput_mbps, col = line_col, lwd = 2, lty = line_lty)
    }

    legend("topright",
           legend = protocol_levels,
           col = unname(protocol_palette[protocol_levels]),
           lty = 1,
           lwd = 3,
           title = "Protocol",
           bty = "n")

    legend("topleft",
           legend = paste0("MTU ", names(mtu_lty)),
           col = "gray30",
           lty = unname(mtu_lty),
           lwd = 2,
           title = "Line type",
           bty = "n")
  } else {
    plot.new()
    text(0.5, 0.5, "No throughput data")
  }
  dev.off()

  # Protocol comparison across bitrates (aggregated across runs/MTUs).
  plot_bitrate_comparison <- function(metric_col, file_name, title_text, ylab_text) {
    bitrates <- sort(unique(per_run$bitrate_kbps))
    value_matrix <- matrix(
      NA_real_,
      nrow = length(protocol_levels),
      ncol = length(bitrates),
      dimnames = list(protocol_levels, as.character(bitrates))
    )

    for (pi in seq_along(protocol_levels)) {
      proto <- protocol_levels[[pi]]
      for (bi in seq_along(bitrates)) {
        br <- bitrates[[bi]]
        v <- per_run[per_run$protocol == proto & per_run$bitrate_kbps == br, metric_col]
        value_matrix[pi, bi] <- ifelse(length(v) == 0, NA_real_, mean(v, na.rm = TRUE))
      }
    }

    png(file.path(plot_dir, file_name), width = 1400, height = 800)
    barplot(
      value_matrix,
      beside = TRUE,
      col = unname(protocol_palette[protocol_levels]),
      names.arg = paste0(bitrates, " kbps"),
      las = 1,
      main = title_text,
      xlab = "Bitrate",
      ylab = ylab_text,
      ylim = c(0, max(value_matrix, na.rm = TRUE) * 1.1)
    )
    legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
    dev.off()
  }

  plot_bitrate_comparison(
    metric_col = "goodput_mbps",
    file_name = "compare_goodput_by_bitrate.png",
    title_text = "Stream vs Gossipsub: Goodput by Bitrate",
    ylab_text = "Goodput (Mbps)"
  )

  plot_bitrate_comparison(
    metric_col = "delay_p95_ms",
    file_name = "compare_delay_p95_by_bitrate.png",
    title_text = "Stream vs Gossipsub: Delay P95 by Bitrate",
    ylab_text = "Delay P95 (ms)"
  )

  plot_bitrate_comparison(
    metric_col = "inferred_loss_pct",
    file_name = "compare_inferred_loss_by_bitrate.png",
    title_text = "Stream vs Gossipsub: Inferred Loss by Bitrate",
    ylab_text = "Inferred Loss (%)"
  )

  plot_bitrate_comparison(
    metric_col = "misordered_packet_rate_pct",
    file_name = "compare_misorder_rate_by_bitrate.png",
    title_text = "Stream vs Gossipsub: Misorder Rate by Bitrate",
    ylab_text = "Misordered Packet Rate (%)"
  )
}

main <- function() {
  opts <- parse_args(args)

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

  for (i in seq_along(csv_files)) {
    path <- csv_files[[i]]
    meta <- metas[[i]]
    message(sprintf("Processing %s", basename(path)))

    df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                   error = function(e) stop(sprintf("Failed reading %s: %s", path, e$message)))

    result <- compute_run_metrics(df, meta, opts$stall_ms, opts$window_ms)
    run_rows[[length(run_rows) + 1]] <- result$run_metrics
    throughput_rows[[length(throughput_rows) + 1]] <- result$throughput_ts
  }

  per_run <- do.call(rbind, run_rows)
  per_run <- per_run[order(per_run$protocol, per_run$bitrate_kbps, per_run$mtu), ]

  throughput_ts <- if (length(throughput_rows) > 0) do.call(rbind, throughput_rows) else data.frame()

  plot_outputs(per_run, throughput_ts, file.path(opts$root, "analysis"))

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
}

main()
