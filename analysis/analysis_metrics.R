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
  misordered_bytes <- sum(df$size[misordered_packet_mask], na.rm = TRUE)
  misordered_bytes_rate_pct <- ifelse(total_bytes > 0, (misordered_bytes / total_bytes) * 100, NA_real_)
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

  misorder_streak_lengths <- if (any(misordered_packet_mask)) {
    r <- rle(misordered_packet_mask)
    r$lengths[r$values]
  } else {
    numeric(0)
  }
  misorder_streak_count <- length(misorder_streak_lengths)
  misorder_streak_max <- if (misorder_streak_count > 0) max(misorder_streak_lengths, na.rm = TRUE) else 0
  misorder_streak_mean <- if (misorder_streak_count > 0) mean(misorder_streak_lengths, na.rm = TRUE) else 0
  misorder_streak_p95 <- if (misorder_streak_count > 0) suppressWarnings(as.numeric(quantile(misorder_streak_lengths, probs = 0.95, na.rm = TRUE))) else 0
  misorder_streak_events <- if (misorder_streak_count > 0) {
    data.frame(
      file = rep(meta$file, misorder_streak_count),
      run_id = rep(meta$run_id, misorder_streak_count),
      protocol = rep(meta$protocol, misorder_streak_count),
      bitrate_kbps = rep(meta$bitrate_kbps, misorder_streak_count),
      mtu = rep(meta$mtu, misorder_streak_count),
      streak_id = seq_len(misorder_streak_count),
      streak_length = as.numeric(misorder_streak_lengths)
    )
  } else {
    data.frame(
      file = character(),
      run_id = numeric(),
      protocol = character(),
      bitrate_kbps = numeric(),
      mtu = numeric(),
      streak_id = integer(),
      streak_length = numeric()
    )
  }

  stutter_mask <- !is.na(inter_arrival_ms) & inter_arrival_ms > stall_ms
  stutter_count <- sum(stutter_mask)
  stutter_total_ms <- sum(inter_arrival_ms[stutter_mask], na.rm = TRUE)
  stutter_longest_ms <- ifelse(stutter_count > 0, max(inter_arrival_ms[stutter_mask], na.rm = TRUE), 0)

  delay_mean <- mean(delay_ms, na.rm = TRUE)
  delay_sd <- sd(delay_ms, na.rm = TRUE)
  delay_var <- var(delay_ms, na.rm = TRUE)
  delay_cv <- ifelse(is.na(delay_mean) || delay_mean == 0, NA_real_, delay_sd / delay_mean)
  delay_spike_threshold_ms <- suppressWarnings(as.numeric(quantile(delay_ms, probs = 0.95, na.rm = TRUE)))
  delay_spike_mask <- !is.na(delay_ms) & delay_ms > delay_spike_threshold_ms
  delay_spike_values <- delay_ms[delay_spike_mask]
  delay_spike_count <- sum(delay_spike_mask, na.rm = TRUE)
  delay_spike_max_ms <- if (delay_spike_count > 0) max(delay_spike_values, na.rm = TRUE) else 0
  delay_spike_mean_ms <- if (delay_spike_count > 0) mean(delay_spike_values, na.rm = TRUE) else 0
  delay_spike_excess_ms <- if (delay_spike_count > 0) delay_spike_values - delay_spike_threshold_ms else numeric(0)
  delay_spike_excess_mean_ms <- if (delay_spike_count > 0) mean(delay_spike_excess_ms, na.rm = TRUE) else 0
  delay_spike_excess_max_ms <- if (delay_spike_count > 0) max(delay_spike_excess_ms, na.rm = TRUE) else 0

  inter_arrival_mean <- mean(inter_arrival_ms, na.rm = TRUE)
  inter_arrival_sd <- sd(inter_arrival_ms, na.rm = TRUE)
  inter_arrival_cv <- ifelse(is.na(inter_arrival_mean) || inter_arrival_mean == 0, NA_real_, inter_arrival_sd / inter_arrival_mean)

  jitter_mean <- mean(abs_delay_delta_ms, na.rm = TRUE)
  jitter_p95 <- suppressWarnings(as.numeric(quantile(abs_delay_delta_ms, probs = 0.95, na.rm = TRUE)))

  target_interarrival_ms <- ifelse(meta$bitrate_kbps > 0, (mean(df$size, na.rm = TRUE) * 8) / as.numeric(meta$bitrate_kbps), NA_real_)
  stability_score <- 100 - (pmin(100, delay_cv * 100) * 0.5 + pmin(100, inter_arrival_cv * 100) * 0.5)
  stability_score <- pmax(stability_score, 0)

  t0 <- min(df$arrival_time)
  bucket <- floor((df$arrival_time - t0) / window_ms)
  bucket_bytes <- tapply(df$size, bucket, sum)
  throughput_ts <- data.frame(
    file = meta$file,
    run_id = meta$run_id,
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
    run_id = meta$run_id,
    protocol = meta$protocol,
    bitrate_kbps = meta$bitrate_kbps,
    mtu = meta$mtu,
    index = df$index,
    arrival_time = df$arrival_time,
    inter_arrival_ms = inter_arrival_ms,
    is_stutter = stutter_mask
  )
  stutter_events <- stutter_events[stutter_events$is_stutter, c(
    "file", "run_id", "protocol", "bitrate_kbps", "mtu", "index", "arrival_time", "inter_arrival_ms"
  )]

  run_metrics <- data.frame(
    file = meta$file,
    run_id = meta$run_id,
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
    misordered_bytes = misordered_bytes,
    misordered_bytes_rate_pct = misordered_bytes_rate_pct,
    reorder_depth_max = reorder_depth_max,
    reorder_depth_mean_misordered = reorder_depth_mean_misordered,
    reorder_depth_p95_misordered = reorder_depth_p95_misordered,
    reorder_depth_total = reorder_depth_total,
    misorder_streak_count = misorder_streak_count,
    misorder_streak_max = misorder_streak_max,
    misorder_streak_mean = misorder_streak_mean,
    misorder_streak_p95 = misorder_streak_p95,
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
    delay_spike_threshold_ms = delay_spike_threshold_ms,
    delay_spike_count = delay_spike_count,
    delay_spike_max_ms = delay_spike_max_ms,
    delay_spike_mean_ms = delay_spike_mean_ms,
    delay_spike_excess_mean_ms = delay_spike_excess_mean_ms,
    delay_spike_excess_max_ms = delay_spike_excess_max_ms,
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

  list(
    run_metrics = run_metrics,
    stutter_events = stutter_events,
    throughput_ts = throughput_ts,
    misorder_streak_events = misorder_streak_events
  )
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

export_summary_tables <- function(per_run, grouped, stutter_events, misorder_streak_events, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(per_run, file.path(out_dir, "per_run_metrics.csv"), row.names = FALSE)
  write.csv(grouped, file.path(out_dir, "grouped_metrics.csv"), row.names = FALSE)
  if (!is.null(stutter_events) && nrow(stutter_events) > 0) {
    write.csv(stutter_events, file.path(out_dir, "stutter_events.csv"), row.names = FALSE)
  }
  if (!is.null(misorder_streak_events) && nrow(misorder_streak_events) > 0) {
    write.csv(misorder_streak_events, file.path(out_dir, "misorder_streak_events.csv"), row.names = FALSE)
  }
}