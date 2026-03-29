# Analysis

This folder contains an R-based analysis pipeline for generated run data:

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

- `analysis/plots/*.png`
- Includes dedicated misorder plots:
  - `analysis/plots/misordered_events_by_run.png`
  - `analysis/plots/misordered_rate_by_run.png`
  - `analysis/plots/misordered_amount_by_run.png`
  - `analysis/plots/misordered_depth_by_run.png`
  - `analysis/plots/misordered_magnitude_by_run.png`
- Includes protocol comparison by bitrate plots:
  - `analysis/plots/compare_goodput_by_bitrate.png`
  - `analysis/plots/compare_delay_p95_by_bitrate.png`
  - `analysis/plots/compare_inferred_loss_by_bitrate.png`
  - `analysis/plots/compare_misorder_rate_by_bitrate.png`

## Notes

- Packet loss is **inferred** from missing indices in observed index range; it is a proxy, not guaranteed true network-layer loss.
- Delay uses `arrival_time - creation_time` and assumes clocks are meaningful for this difference in your test setup.
