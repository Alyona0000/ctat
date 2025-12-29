# Organization of Plots by Report ✅

To keep generated figures tidy, plots are saved in per-report subfolders under `plots/`:

- `plots/report1/` — figures for `1_звіт.md` (simple regression examples)
- `plots/report2/` — Variant 1 (AAPL, LLY) plots (created by `scripts/analysis_variant1.R`)
- `plots/report3/` — Correlation analysis (created by `3_звіт.R`)
- `plots/report4/` — Variant 4 (proportions — `4_звіт.R`)
- `plots/report5/` — Reserve simulations (Variant 5 — `5_звіт.R`)

Notes:
- Some plots are not yet generated because corresponding scripts were not run in this environment (R not available here). Run the R scripts locally or add the missing data files (`data/ChGendUS2023.csv`, `data/Canada0.txt`) and run the scripts to generate plots.
- Report markdowns in `outputs/` reference images using relative paths (e.g. `../plots/report2/aapl_hist_closing_counts.png`).