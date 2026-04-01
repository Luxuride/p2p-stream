#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

VIDEO_PATH="${1:-${VIDEO_SOURCE:-}}"
if [ -z "$VIDEO_PATH" ]; then
  echo "Usage: stream_test/run_matrix.sh <video_path>"
  echo "Or set VIDEO_SOURCE in environment."
  exit 1
fi

PROTOCOLS=(stream gossipsub)
BITRATES=(250 500 1000 2500 5000 10000)
CHUNK_SIZES=(1200 4096)

for protocol in "${PROTOCOLS[@]}"; do
  for bitrate in "${BITRATES[@]}"; do
    for chunk_size in "${CHUNK_SIZES[@]}"; do
      output="analysis_${protocol}_${bitrate}kbps_${chunk_size}mtu.csv"
      echo "Running protocol=${protocol} bitrate=${bitrate} chunk_size=${chunk_size} -> ${output}"
      cargo run -p stream_test -- \
        --role manager \
        --protocol "$protocol" \
        --bitrate "$bitrate" \
        --chunk-size "$chunk_size" \
        --video-path "$VIDEO_PATH" \
        --output "$output"
    done
  done
done

echo "Matrix run completed. Generated analysis files in: $ROOT_DIR"
