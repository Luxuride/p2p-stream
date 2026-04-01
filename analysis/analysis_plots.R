plot_outputs <- function(per_run, throughput_ts, misorder_streak_events, out_dir) {
  plot_dir <- file.path(out_dir, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  root_pngs <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE, recursive = TRUE)
  if (length(root_pngs) > 0) {
    unlink(root_pngs)
  }

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

  mtu_levels <- sort(unique(per_run$mtu))
  mtu_group_dir <- function(base_dir, mtu_value) {
    file.path(base_dir, sprintf("mtu_%s", mtu_value))
  }

  condition_run_label <- function(df, group_cols = c("protocol", "bitrate_kbps", "mtu")) {
    if (nrow(df) == 0) {
      return("n=0 runs/condition")
    }

    keys <- intersect(group_cols, names(df))
    if (length(keys) == 0) {
      return("n=? runs/condition")
    }

    if ("run_id" %in% names(df)) {
      counts <- aggregate(df$run_id, by = df[keys], FUN = function(x) length(unique(x)))
      vals <- counts$x
    } else if ("file" %in% names(df)) {
      counts <- aggregate(df$file, by = df[keys], FUN = function(x) length(unique(x)))
      vals <- counts$x
    } else {
      return("n=? runs/condition")
    }

    if (length(vals) == 0 || all(is.na(vals))) {
      return("n=? runs/condition")
    }

    min_n <- min(vals, na.rm = TRUE)
    max_n <- max(vals, na.rm = TRUE)
    if (min_n == max_n) {
      sprintf("n=%d runs/condition", min_n)
    } else {
      sprintf("n=%d-%d runs/condition", min_n, max_n)
    }
  }

  title_with_runs <- function(df, title_text, direction_text = NULL, group_cols = c("protocol", "bitrate_kbps", "mtu")) {
    base_title <- ifelse(is.null(direction_text), title_text, sprintf("%s (%s)", title_text, direction_text))
    sprintf("%s [%s]", base_title, condition_run_label(df, group_cols = group_cols))
  }

  safe_positive <- function(x) {
    x[is.na(x)] <- NA_real_
    x[!is.na(x) & x <= 0] <- 1e-9
    x
  }

  maybe_log_variant <- function(path) {
    sub("\\.png$", "_log.png", path)
  }

  plot_run_bar <- function(df, metric_col, file_path, title_text, ylab_text, direction_text = NULL, log_variant = FALSE) {
    ord <- order(df$protocol, df$bitrate_kbps, df$mtu)
    ordered_protocols <- as.character(df$protocol[ord])
    bar_cols <- unname(protocol_palette[ordered_protocols])
    png(file_path, width = 1800, height = 900)
    par(mar = c(4, 18, 4, 2) + 0.1, xpd = NA)
    values <- df[[metric_col]][ord]
    if (log_variant) {
      values <- safe_positive(values)
    }
    xlim_values <- values[!is.na(values)]
    xlim_lower <- if (length(xlim_values) > 0) min(xlim_values) * 0.8 else 1e-9
    xlim_upper <- if (length(xlim_values) > 0) max(xlim_values) * 1.1 else 1
    barplot(
      values,
      names.arg = df$file[ord],
      horiz = TRUE,
      las = 1,
      cex.names = 1.0,
      col = bar_cols,
      main = title_with_runs(df, title_text, direction_text),
      xlab = ylab_text,
      xlim = if (log_variant) c(xlim_lower, xlim_upper) else c(0, xlim_upper),
      log = if (log_variant) "x" else ""
    )
    legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
    dev.off()
  }

  plot_protocol_comparison <- function(df, compare_col, metric_col, file_path, title_text, ylab_text, group_label, direction_text = NULL, log_variant = FALSE) {
    groups <- sort(unique(df[[compare_col]]))
    value_matrix <- matrix(
      NA_real_,
      nrow = length(protocol_levels),
      ncol = length(groups),
      dimnames = list(protocol_levels, as.character(groups))
    )

    for (pi in seq_along(protocol_levels)) {
      proto <- protocol_levels[[pi]]
      for (gi in seq_along(groups)) {
        g <- groups[[gi]]
        v <- df[df$protocol == proto & df[[compare_col]] == g, metric_col]
        value_matrix[pi, gi] <- ifelse(length(v) == 0, NA_real_, mean(v, na.rm = TRUE))
      }
    }

    if (log_variant) {
      value_matrix <- ifelse(is.na(value_matrix) | value_matrix <= 0, NA_real_, value_matrix)
      value_matrix <- ifelse(is.na(value_matrix), NA_real_, value_matrix)
      value_matrix[!is.na(value_matrix)] <- pmax(value_matrix[!is.na(value_matrix)], 1e-9)
    }
    yvals <- value_matrix[!is.na(value_matrix)]
    y_lower <- if (length(yvals) > 0) min(yvals) * 0.8 else 1e-9
    y_upper <- if (length(yvals) > 0) max(yvals) * 1.1 else 1

    png(file_path, width = 1400, height = 800)
    barplot(
      value_matrix,
      beside = TRUE,
      col = unname(protocol_palette[protocol_levels]),
      names.arg = paste0(groups, " ", group_label),
      las = 1,
      main = title_with_runs(df, title_text, direction_text),
      xlab = group_label,
      ylab = ylab_text,
      ylim = if (log_variant) c(y_lower, y_upper) else c(0, y_upper),
      log = if (log_variant) "y" else ""
    )
    legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
    dev.off()

    invisible(value_matrix)
  }

  plot_protocol_mtu_comparison <- function(df, metric_col, bitrate_value, file_path, title_text, ylab_text, direction_text = NULL, log_variant = FALSE) {
    d <- df[df$bitrate_kbps == bitrate_value, ]
    mtu_levels <- sort(unique(d$mtu))
    value_matrix <- matrix(
      NA_real_,
      nrow = length(protocol_levels),
      ncol = length(mtu_levels),
      dimnames = list(protocol_levels, as.character(mtu_levels))
    )

    for (pi in seq_along(protocol_levels)) {
      proto <- protocol_levels[[pi]]
      for (mi in seq_along(mtu_levels)) {
        mtu_value <- mtu_levels[[mi]]
        v <- d[d$protocol == proto & d$mtu == mtu_value, metric_col]
        value_matrix[pi, mi] <- ifelse(length(v) == 0, NA_real_, mean(v, na.rm = TRUE))
      }
    }

    if (log_variant) {
      value_matrix <- ifelse(is.na(value_matrix) | value_matrix <= 0, NA_real_, value_matrix)
      value_matrix <- ifelse(is.na(value_matrix), NA_real_, value_matrix)
      value_matrix[!is.na(value_matrix)] <- pmax(value_matrix[!is.na(value_matrix)], 1e-9)
    }
    yvals <- value_matrix[!is.na(value_matrix)]
    y_lower <- if (length(yvals) > 0) min(yvals) * 0.8 else 1e-9
    y_upper <- if (length(yvals) > 0) max(yvals) * 1.1 else 1

    png(file_path, width = 1400, height = 800)
    barplot(
      value_matrix,
      beside = TRUE,
      col = unname(protocol_palette[protocol_levels]),
      names.arg = paste0(mtu_levels, " mtu"),
      las = 1,
      main = title_with_runs(d, title_text, direction_text),
      xlab = "MTU",
      ylab = ylab_text,
      ylim = if (log_variant) c(y_lower, y_upper) else c(0, y_upper),
      log = if (log_variant) "y" else ""
    )
    legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
    dev.off()
  }

  plot_protocol_box_by_mtu <- function(df, metric_col, base_dir, file_stub, title_text, ylab_text, direction_text, log_variant = FALSE) {
    for (mtu_value in mtu_levels) {
      d <- df[df$mtu == mtu_value, , drop = FALSE]
      if (nrow(d) == 0) {
        next
      }
      mtu_dir <- mtu_group_dir(base_dir, mtu_value)
      dir.create(mtu_dir, recursive = TRUE, showWarnings = FALSE)
      png(file.path(mtu_dir, if (log_variant) maybe_log_variant(file_stub) else file_stub), width = 1200, height = 700)
      box_data <- d
      box_data$protocol <- factor(as.character(box_data$protocol), levels = protocol_levels)
      box_data$.metric <- box_data[[metric_col]]
      if (log_variant) {
        box_data$.metric <- ifelse(is.na(box_data$.metric), NA_real_, pmax(box_data$.metric, 1e-9))
        yvals <- box_data$.metric
        y_upper <- max(yvals, na.rm = TRUE) * 1.1
        boxplot(
          .metric ~ protocol,
          data = box_data,
          log = "y",
          ylim = c(1e-9, y_upper),
          main = sprintf("%s [%s]", sprintf("%s at MTU %s (log scale; %s)", title_text, mtu_value, direction_text), condition_run_label(d)),
          xlab = "Protocol",
          ylab = ylab_text,
          col = unname(protocol_palette[protocol_levels])
        )
      } else {
        yvals <- box_data$.metric
        y_upper <- max(yvals, na.rm = TRUE) * 1.1
        boxplot(
          .metric ~ protocol,
          data = box_data,
          ylim = c(0, y_upper),
          main = sprintf("%s [%s]", sprintf("%s at MTU %s (%s)", title_text, mtu_value, direction_text), condition_run_label(d)),
          xlab = "Protocol",
          ylab = ylab_text,
          col = unname(protocol_palette[protocol_levels])
        )
      }
      legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
      dev.off()
    }
  }

  plot_metric_box_per_protocol_by_mtu <- function(df, metric_col, base_dir, file_stub, title_text, ylab_text, direction_text) {
    for (mtu_value in mtu_levels) {
      mtu_data <- df[df$mtu == mtu_value, , drop = FALSE]
      if (nrow(mtu_data) == 0) {
        next
      }

      for (proto in protocol_levels) {
        proto_data <- mtu_data[mtu_data$protocol == proto, , drop = FALSE]
        if (nrow(proto_data) == 0) {
          next
        }

        mtu_dir <- mtu_group_dir(base_dir, mtu_value)
        dir.create(mtu_dir, recursive = TRUE, showWarnings = FALSE)
        out_file <- file.path(mtu_dir, sprintf("%s_%s.png", sub("\\.png$", "", file_stub), proto))

        png(out_file, width = 1200, height = 700)
        box_data <- proto_data
        box_data$bitrate_kbps <- factor(as.character(box_data$bitrate_kbps), levels = as.character(sort(unique(box_data$bitrate_kbps))))
        yvals <- box_data[[metric_col]]
        y_upper <- max(yvals, na.rm = TRUE) * 1.1
        boxplot(
          box_data[[metric_col]] ~ bitrate_kbps,
          data = box_data,
          ylim = c(0, y_upper),
          main = sprintf("%s [%s]", sprintf("%s [%s] at MTU %s (%s)", title_text, proto, mtu_value, direction_text), condition_run_label(proto_data)),
          xlab = "Bitrate (kbps)",
          ylab = ylab_text,
          col = protocol_palette[[proto]]
        )
        dev.off()
      }
    }
  }

  plot_metric_by_bitrate_and_protocol <- function(metric_col, file_name, title_text, ylab_text, log_ok = TRUE) {
    direction_text <- if (metric_col %in% c("goodput_mbps", "stability_score_0_100")) {
      "higher is better"
    } else {
      "lower is better"
    }
    bitrate_dir <- file.path(plot_dir, "bitrate")
    for (mtu_value in mtu_levels) {
      d <- per_run[per_run$mtu == mtu_value, , drop = FALSE]
      if (nrow(d) == 0) {
        next
      }
      mtu_dir <- mtu_group_dir(bitrate_dir, mtu_value)
      dir.create(mtu_dir, recursive = TRUE, showWarnings = FALSE)
      linear_path <- file.path(mtu_dir, file_name)
      plot_protocol_comparison(
        d,
        compare_col = "bitrate_kbps",
        metric_col = metric_col,
        file_path = linear_path,
        title_text = sprintf("%s at MTU %s", title_text, mtu_value),
        ylab_text = ylab_text,
        group_label = "kbps",
        direction_text = direction_text
      )
      if (log_ok) {
        plot_protocol_comparison(
          d,
          compare_col = "bitrate_kbps",
          metric_col = metric_col,
          file_path = maybe_log_variant(linear_path),
          title_text = sprintf("%s at MTU %s (log scale)", title_text, mtu_value),
          ylab_text = ylab_text,
          group_label = "kbps",
          direction_text = direction_text,
          log_variant = TRUE
        )
      }
    }
  }

  plot_metric_by_mtu_and_protocol <- function(metric_col, file_stub, title_text, ylab_text) {
    direction_text <- if (metric_col %in% c("goodput_mbps", "stability_score_0_100")) {
      "higher is better"
    } else {
      "lower is better"
    }
    mtu_dir <- file.path(plot_dir, "mtu")
    dir.create(mtu_dir, recursive = TRUE, showWarnings = FALSE)
    bitrates <- sort(unique(per_run$bitrate_kbps))
    for (br in bitrates) {
      file_name <- sprintf("%s_%skbps.png", file_stub, br)
      plot_protocol_mtu_comparison(
        per_run,
        metric_col = metric_col,
        bitrate_value = br,
        file_path = file.path(mtu_dir, file_name),
        title_text = sprintf("%s at %s kbps", title_text, br),
        ylab_text = ylab_text,
        direction_text = direction_text
      )
      plot_protocol_mtu_comparison(
        per_run,
        metric_col = metric_col,
        bitrate_value = br,
        file_path = file.path(mtu_dir, maybe_log_variant(file_name)),
        title_text = sprintf("%s at %s kbps (log scale)", title_text, br),
        ylab_text = ylab_text,
        direction_text = direction_text,
        log_variant = TRUE
      )
    }
  }

  plot_run_metric_by_mtu <- function(metric_col, base_dir, file_name, title_text, ylab_text, direction_text) {
    for (mtu_value in mtu_levels) {
      d <- per_run[per_run$mtu == mtu_value, , drop = FALSE]
      if (nrow(d) == 0) {
        next
      }
      mtu_dir <- mtu_group_dir(base_dir, mtu_value)
      dir.create(mtu_dir, recursive = TRUE, showWarnings = FALSE)
      plot_run_bar(
        d,
        metric_col,
        file.path(mtu_dir, file_name),
        sprintf("%s at MTU %s", title_text, mtu_value),
        ylab_text,
        direction_text
      )
      plot_run_bar(
        d,
        metric_col,
        file.path(mtu_dir, maybe_log_variant(file_name)),
        sprintf("%s at MTU %s (log scale)", title_text, mtu_value),
        ylab_text,
        direction_text,
        log_variant = TRUE
      )
    }
  }

  # Delay plots.
  delay_dir <- file.path(plot_dir, "delay")
  dir.create(delay_dir, recursive = TRUE, showWarnings = FALSE)
  plot_run_metric_by_mtu("delay_p95_ms", delay_dir, "delay_p95_by_run.png", "Delay P95 by Run", "Delay P95 (ms)", "lower is better")
  plot_run_metric_by_mtu("delay_mean_ms", delay_dir, "delay_mean_by_run.png", "Delay Mean by Run", "Delay Mean (ms)", "lower is better")
  plot_run_metric_by_mtu("delay_p99_ms", delay_dir, "delay_p99_by_run.png", "Delay P99 by Run", "Delay P99 (ms)", "lower is better")
  plot_run_metric_by_mtu("delay_cv", delay_dir, "delay_cv_by_run.png", "Delay CV by Run", "Delay CV", "lower is better")
  plot_run_metric_by_mtu("jitter_p95_ms", delay_dir, "delay_jitter_p95_by_run.png", "Delay Jitter P95 by Run", "Jitter P95 (ms)", "lower is better")

  for (mtu_value in mtu_levels) {
    mtu_data <- per_run[per_run$mtu == mtu_value, , drop = FALSE]
    if (nrow(mtu_data) == 0) {
      next
    }

    png(file.path(delay_dir, sprintf("delay_p95_by_protocol_mtu_%s.png", mtu_value)), width = 1200, height = 700)
    box_data <- mtu_data
    box_data$protocol <- factor(as.character(box_data$protocol), levels = protocol_levels)
    boxplot(
      delay_p95_ms ~ protocol,
      data = box_data,
      main = sprintf("Delay P95 by Protocol at MTU %s (lower is better) [%s]", mtu_value, condition_run_label(mtu_data)),
      xlab = "Protocol",
      ylab = "Delay P95 (ms)",
      ylim = c(0, max(box_data$delay_p95_ms, na.rm = TRUE) * 1.1),
      col = unname(protocol_palette[protocol_levels])
    )
    legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
    dev.off()

    png(file.path(delay_dir, sprintf("delay_p95_by_protocol_mtu_%s_log.png", mtu_value)), width = 1200, height = 700)
    box_data <- mtu_data
    box_data$protocol <- factor(as.character(box_data$protocol), levels = protocol_levels)
    boxplot(
      delay_p95_ms ~ protocol,
      data = box_data,
      log = "y",
      main = sprintf("Delay P95 by Protocol at MTU %s (log scale; lower is better) [%s]", mtu_value, condition_run_label(mtu_data)),
      xlab = "Protocol",
      ylab = "Delay P95 (ms)",
      col = unname(protocol_palette[protocol_levels])
    )
    legend("topright", legend = protocol_levels, fill = unname(protocol_palette[protocol_levels]), bty = "n")
    dev.off()
  }

  # Packet loss plots.
  packet_loss_dir <- file.path(plot_dir, "packet_loss")
  dir.create(packet_loss_dir, recursive = TRUE, showWarnings = FALSE)
  plot_run_metric_by_mtu("inferred_loss_pct", packet_loss_dir, "inferred_loss_by_run.png", "Inferred Packet Loss by Run", "Inferred Loss (%)", "lower is better")
  plot_run_metric_by_mtu("duplicate_rate_pct", packet_loss_dir, "duplicate_rate_by_run.png", "Duplicate Rate by Run", "Duplicate Rate (%)", "lower is better")
  plot_metric_by_bitrate_and_protocol(
    metric_col = "inferred_loss_pct",
    file_name = "compare_inferred_loss_by_bitrate.png",
    title_text = "Protocol Comparison: Inferred Loss by Bitrate",
    ylab_text = "Inferred Loss (%)"
  )
  for (mtu_value in mtu_levels) {
    d <- per_run[per_run$mtu == mtu_value, , drop = FALSE]
    if (nrow(d) == 0) {
      next
    }
    mtu_dir <- mtu_group_dir(packet_loss_dir, mtu_value)
    dir.create(mtu_dir, recursive = TRUE, showWarnings = FALSE)
    png(file.path(mtu_dir, "loss_vs_goodput.png"), width = 1400, height = 800)
    plot(d$inferred_loss_pct, d$goodput_mbps,
      col = unname(protocol_palette[as.character(d$protocol)]),
      pch = 19,
      xlab = "Inferred Loss (%)",
      ylab = "Goodput (Mbps)",
      xlim = c(0, max(d$inferred_loss_pct, na.rm = TRUE) * 1.1),
      ylim = c(0, max(d$goodput_mbps, na.rm = TRUE) * 1.1),
      main = sprintf("Inferred Loss vs Goodput at MTU %s (lower loss, higher goodput is better) [%s]", mtu_value, condition_run_label(d))
    )
    legend("topright", legend = protocol_levels, col = unname(protocol_palette[protocol_levels]), pch = 19, bty = "n")
    dev.off()

    png(file.path(mtu_dir, "loss_vs_goodput_log.png"), width = 1400, height = 800)
    plot(d$inferred_loss_pct, d$goodput_mbps,
      col = unname(protocol_palette[as.character(d$protocol)]),
      pch = 19,
      xlab = "Inferred Loss (%)",
      ylab = "Goodput (Mbps)",
      log = "y",
      main = sprintf("Inferred Loss vs Goodput at MTU %s (log scale) [%s]", mtu_value, condition_run_label(d))
    )
    legend("topright", legend = protocol_levels, col = unname(protocol_palette[protocol_levels]), pch = 19, bty = "n")
    dev.off()
  }

  # Misordered packet plots.
  misordered_dir <- file.path(plot_dir, "misordered")
  dir.create(misordered_dir, recursive = TRUE, showWarnings = FALSE)
  plot_run_metric_by_mtu("reorder_events", misordered_dir, "misordered_events_by_run.png", "Misordered Packets by Run (Reorder Events)", "Reorder Events (count)", "lower is better")
  plot_run_metric_by_mtu("reorder_rate_pct", misordered_dir, "misordered_rate_by_run.png", "Misordered Packet Rate by Run", "Reorder Rate (%)", "lower is better")
  plot_run_metric_by_mtu("misordered_packet_count", misordered_dir, "misordered_amount_by_run.png", "Misordered Packet Amount by Run", "Misordered Packets (count)", "lower is better")
  plot_run_metric_by_mtu("misordered_bytes", misordered_dir, "misordered_bytes_by_run.png", "Misordered Bytes by Run", "Misordered Bytes", "lower is better")
  plot_run_metric_by_mtu("misordered_bytes_rate_pct", misordered_dir, "misordered_bytes_rate_by_run.png", "Misordered Bytes Rate by Run", "Misordered Bytes Rate (%)", "lower is better")
  plot_run_metric_by_mtu("reorder_depth_max", misordered_dir, "misordered_depth_by_run.png", "Maximum Reorder Depth by Run", "Max Reorder Depth (indices)", "lower is better")
  plot_run_metric_by_mtu("reorder_depth_mean_misordered", misordered_dir, "misordered_magnitude_by_run.png", "Misordered Magnitude by Run", "Mean Reorder Depth (misordered packets only)", "lower is better")
  plot_run_metric_by_mtu("reorder_depth_p95_misordered", misordered_dir, "misordered_depth_p95_by_run.png", "Misordered Depth P95 by Run", "Reorder Depth P95", "lower is better")
  plot_protocol_box_by_mtu(
    misorder_streak_events,
    metric_col = "streak_length",
    base_dir = misordered_dir,
    file_stub = "misorder_streak_boxplot.png",
    title_text = "Misorder Streak Length Boxplot",
    ylab_text = "Misorder Streak Length (packets)",
    direction_text = "lower is better"
  )
  plot_metric_box_per_protocol_by_mtu(
    misorder_streak_events,
    metric_col = "streak_length",
    base_dir = misordered_dir,
    file_stub = "misorder_streak_boxplot_by_bitrate.png",
    title_text = "Misorder Streak Length Boxplot by Bitrate",
    ylab_text = "Misorder Streak Length (packets)",
    direction_text = "lower is better"
  )

  plot_protocol_box_by_mtu(
    per_run,
    metric_col = "delay_spike_excess_max_ms",
    base_dir = delay_dir,
    file_stub = "delay_spike_boxplot.png",
    title_text = "Delay Spike Excess Boxplot",
    ylab_text = "Delay Spike Excess (ms)",
    direction_text = "lower is better"
  )

  # Bitrate comparison plots.
  plot_metric_by_bitrate_and_protocol(
    metric_col = "goodput_mbps",
    file_name = "compare_goodput_by_bitrate.png",
    title_text = "Protocol Comparison: Goodput by Bitrate",
    ylab_text = "Goodput (Mbps)"
  )
  plot_metric_by_bitrate_and_protocol(
    metric_col = "delay_p95_ms",
    file_name = "compare_delay_p95_by_bitrate.png",
    title_text = "Protocol Comparison: Delay P95 by Bitrate",
    ylab_text = "Delay P95 (ms)"
  )
  plot_metric_by_bitrate_and_protocol(
    metric_col = "misordered_packet_rate_pct",
    file_name = "compare_misorder_rate_by_bitrate.png",
    title_text = "Protocol Comparison: Misorder Rate by Bitrate",
    ylab_text = "Misordered Packet Rate (%)"
  )
  plot_metric_by_bitrate_and_protocol(
    metric_col = "stability_score_0_100",
    file_name = "compare_stability_score_by_bitrate.png",
    title_text = "Protocol Comparison: Stability Score by Bitrate",
    ylab_text = "Stability Score (0-100)"
  )

  # MTU comparison plots, one file per bitrate.
  plot_metric_by_mtu_and_protocol(
    metric_col = "goodput_mbps",
    file_stub = "compare_goodput_by_mtu",
    title_text = "Protocol Comparison: Goodput by MTU",
    ylab_text = "Goodput (Mbps)"
  )
  plot_metric_by_mtu_and_protocol(
    metric_col = "delay_p95_ms",
    file_stub = "compare_delay_p95_by_mtu",
    title_text = "Protocol Comparison: Delay P95 by MTU",
    ylab_text = "Delay P95 (ms)"
  )
  plot_metric_by_mtu_and_protocol(
    metric_col = "inferred_loss_pct",
    file_stub = "compare_inferred_loss_by_mtu",
    title_text = "Protocol Comparison: Inferred Loss by MTU",
    ylab_text = "Inferred Loss (%)"
  )
  plot_metric_by_mtu_and_protocol(
    metric_col = "misordered_bytes_rate_pct",
    file_stub = "compare_misordered_bytes_rate_by_mtu",
    title_text = "Protocol Comparison: Misordered Bytes Rate by MTU",
    ylab_text = "Misordered Bytes Rate (%)"
  )
  plot_metric_by_mtu_and_protocol(
    metric_col = "misordered_bytes",
    file_stub = "compare_misordered_bytes_by_mtu",
    title_text = "Protocol Comparison: Misordered Bytes by MTU",
    ylab_text = "Misordered Bytes"
  )
  plot_metric_by_mtu_and_protocol(
    metric_col = "stability_score_0_100",
    file_stub = "compare_stability_score_by_mtu",
    title_text = "Protocol Comparison: Stability Score by MTU",
    ylab_text = "Stability Score (0-100)"
  )

  # Throughput timelines.
  for (mtu_value in mtu_levels) {
    d <- throughput_ts[throughput_ts$mtu == mtu_value, ]
    mtu_dir <- mtu_group_dir(delay_dir, mtu_value)
    dir.create(mtu_dir, recursive = TRUE, showWarnings = FALSE)
    png(file.path(mtu_dir, "throughput_timeline.png"), width = 1400, height = 800)
    if (nrow(d) > 0) {
      d <- d[order(d$protocol, d$bitrate_kbps, d$file, d$bucket), ]
      ylim <- range(d$throughput_mbps, na.rm = TRUE)
      xlim <- range(d$window_start_ms, na.rm = TRUE)
      plot(NA,
          xlim = c(0, max(xlim, na.rm = TRUE)),
          ylim = c(0, max(ylim, na.rm = TRUE)),
        xlab = "Window Start (ms)",
        ylab = "Throughput (Mbps)",
        main = sprintf("Throughput Timelines at MTU %s (higher is better) [%s]", mtu_value, condition_run_label(d))
      )

      run_levels <- unique(d$file)
      lty_choices <- c(1, 2, 3, 4, 5, 6)
      bitrate_levels <- sort(unique(d$bitrate_kbps))
      bitrate_lty <- setNames(lty_choices[((seq_along(bitrate_levels) - 1) %% length(lty_choices)) + 1], bitrate_levels)

      for (i in seq_along(run_levels)) {
        f <- run_levels[[i]]
        dd <- d[d$file == f, ]
        proto <- as.character(dd$protocol[[1]])
        bitrate_value <- as.character(dd$bitrate_kbps[[1]])
        line_col <- protocol_palette[[proto]]
        line_lty <- bitrate_lty[[bitrate_value]]
        lines(dd$window_start_ms, dd$throughput_mbps, col = line_col, lwd = 2, lty = line_lty)
      }

      legend(
        "topright",
        legend = protocol_levels,
        col = unname(protocol_palette[protocol_levels]),
        lty = 1,
        lwd = 3,
        title = "Protocol",
        bty = "n"
      )

      legend(
        "topleft",
        legend = paste0(names(bitrate_lty), " kbps"),
        col = "gray30",
        lty = unname(bitrate_lty),
        lwd = 2,
        title = "Line type",
        bty = "n"
      )
    } else {
      plot.new()
      text(0.5, 0.5, sprintf("No throughput data for MTU %s", mtu_value))
    }
    dev.off()

    png(file.path(mtu_dir, "throughput_timeline_log.png"), width = 1400, height = 800)
    if (nrow(d) > 0) {
      ylim <- range(d$throughput_mbps, na.rm = TRUE)
      xlim <- range(d$window_start_ms, na.rm = TRUE)
      plot(NA,
        xlim = c(0, max(xlim, na.rm = TRUE)),
        ylim = c(1e-9, max(ylim, na.rm = TRUE)),
        xlab = "Window Start (ms)",
        ylab = "Throughput (Mbps)",
        log = "y",
        main = sprintf("Throughput Timelines at MTU %s (log scale; higher is better) [%s]", mtu_value, condition_run_label(d))
      )
      for (i in seq_along(run_levels)) {
        f <- run_levels[[i]]
        dd <- d[d$file == f, ]
        proto <- as.character(dd$protocol[[1]])
        bitrate_value <- as.character(dd$bitrate_kbps[[1]])
        line_col <- protocol_palette[[proto]]
        line_lty <- bitrate_lty[[bitrate_value]]
        lines(dd$window_start_ms, dd$throughput_mbps + 1e-9, col = line_col, lwd = 2, lty = line_lty)
      }
      legend(
        "topright",
        legend = protocol_levels,
        col = unname(protocol_palette[protocol_levels]),
        lty = 1,
        lwd = 3,
        title = "Protocol",
        bty = "n"
      )
      legend(
        "topleft",
        legend = paste0(names(bitrate_lty), " kbps"),
        col = "gray30",
        lty = unname(bitrate_lty),
        lwd = 2,
        title = "Line type",
        bty = "n"
      )
    } else {
      plot.new()
      text(0.5, 0.5, sprintf("No throughput data for MTU %s", mtu_value))
    }
    dev.off()
  }
}