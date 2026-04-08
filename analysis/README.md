# Analysis

This folder contains an R-based analysis pipeline for generated run data.

The main entrypoint is [analysis/analyze.R](analysis/analyze.R). It sources helper files in the same folder and generates plots plus CSV summaries.

- Input files (repo root): `analysis_<protocol>_<bitrate>kbps_<mtu>mtu.csv`
- Supported default protocols: `stream`, `gossipsub`
- Required CSV columns: `creation_time,arrival_time,index,size`

## What it computes

Per run:

- Delay: mean, sd, variance, p50/p95/p99, max
- Stability: delay coefficient of variation (CV), inter-arrival CV, combined `stability_score_0_100`
- Jitter: mean and p95 of absolute delay deltas
- Stuttering: count/total/longest inter-arrival gaps above threshold
- Throughput: run goodput (Mbps) + windowed throughput timeline
- Sequence integrity: inferred missing index count/% (packet loss proxy), duplicates, reorder rate, reorder depth

Grouped summary:

- Mean/sd for all numeric metrics grouped by `(protocol, bitrate_kbps, mtu)`

Exported summaries:

- `analysis/out/per_run_metrics.csv`
- `analysis/out/grouped_metrics.csv`
- `analysis/out/stutter_events.csv` when stutter events exist

## Usage

From repository root:

```bash
Rscript analysis/analyze.R
```

Optional arguments:

```bash
Rscript analysis/analyze.R \
  --root . \
  --protocols stream,gossipsub \
  --stall-ms 120 \
  --window-ms 1000
```

## Outputs

Generated under `analysis/`:

- `analysis/plots/delay/*.png`
- `analysis/plots/packet_loss/*.png`
- `analysis/plots/misordered/*.png`
- `analysis/plots/bitrate/*.png`
- `analysis/plots/mtu/*.png`
- `analysis/out/*.csv`

Non-MTU comparison plots are split further by MTU under subfolders such as:

- `analysis/plots/delay/mtu_<value>/*.png`
- `analysis/plots/packet_loss/mtu_<value>/*.png`
- `analysis/plots/misordered/mtu_<value>/*.png`
- `analysis/plots/bitrate/mtu_<value>/*.png`

Each of those folders now also contains logarithmic companion plots with a `_log.png` suffix.

Notable plot groups:

- Delay plots: delay mean, p95, p99, jitter p95, and delay CV by condition, plus protocol boxplots.
- Delay plots: delay p95 boxplots split by bitrate, both protocol-comparison and per-protocol views.
- Delay plots: delay mean, p95, p99, jitter p95, and delay CV by condition, plus protocol boxplots split by MTU.
- Delay plots: delay mean, p95, p99, jitter p95, and delay CV by condition, plus delay spike boxplots split by MTU.
- Packet loss plots: inferred packet loss and duplicate packet rate by condition, loss vs goodput, plus protocol comparison by bitrate.
- Reordering plots: reorder events/rate, out-of-order packet count, max depth, mean depth, and p95 depth by condition.
- Reordering plots: reorder events/rate, out-of-order bytes/rate, max depth, mean depth, p95 depth, and misorder streak boxplots by MTU.
- Bitrate comparison plots: goodput, delay p95, inferred packet loss, out-of-order packet rate, and stability score.
- MTU comparison plots: one file per bitrate comparing protocols across MTUs for goodput, delay p95, inferred packet loss, out-of-order bytes/rate, and stability score, using condition-level averages across runs.

## Notes

- Packet loss is **inferred** from missing indices in observed index range; it is a proxy, not guaranteed true network-layer loss.
- Delay uses `arrival_time - creation_time` and assumes clocks are meaningful for this difference in your test setup.
- MTU comparison plots are generated separately for each bitrate so you can compare protocols at a fixed bitrate.
- All other plots are grouped by MTU to make the comparisons easier to read at a fixed packet size.
