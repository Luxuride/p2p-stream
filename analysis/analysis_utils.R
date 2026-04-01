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
  suppressWarnings(as.numeric(x))
}