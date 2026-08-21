# Shiny Dashboard Template

This repository is a starter template for Shiny dashboards.

## This dashboard

`app.R` is wired up as a concrete dashboard for the `pharm` repo's iteration-0 outputs (het
Amsterdam health & technology institute, ahti): psychofarmacagebruik (ATC), GGZ-zorggebruik en
-kosten, en de prevalentie van psychische diagnosegroepen, elk uitsplitsbaar naar SES-WOA,
inkomensklasse, opleidingsniveau en leeftijdsgroep. Source data lives in `data/*.xlsx` (already
aggregated, non-traceable outputs of the pharm pipeline — see the pharm repo's own top-level
`README.md`); `utils/pharm_data.R` reads and reshapes them, and
`data/metadata/dictionary_seed.R` seeds the Dictionary tab with Dutch labels for every raw code.
The template's own generic scaffold description below still applies to `utils/`/`templates/`/
`state/` — only `app.R`'s own UI/server logic is dashboard-specific.

Use the template as follows:

1. Put your input files in `data/`.
2. Keep shared metadata and branding helpers in `data/metadata/`.
3. Add reusable functions to `utils/`.
4. Replace the scaffold in `app.R` with your dashboard UI and server logic.

## Included Helpers

- [data/metadata/brand_colors.R](data/metadata/brand_colors.R) contains the branding palette.
- [utils/format_thinkcell_download.R](utils/format_thinkcell_download.R) reshapes plot data into think-cell Excel layouts.
- [utils/slide_download.R](utils/slide_download.R) turns that same data into a PowerPoint slide (+ table + log) ZIP, using a matching template from `templates/`.
- [utils/chart_downloads.R](utils/chart_downloads.R) provides Shiny UI/server helpers for raw, think-cell, and slide downloads (plus the "save as favorite" star).
- [utils/favorites.R](utils/favorites.R) is the shared (per-dashboard, not per-user) favorites list and combined "download all favorites" deck.
- [utils/export_history.R](utils/export_history.R) automatically logs every slide export so it can be redownloaded exactly, any time later, from the **Export history** tab.
- [utils/template_admin.R](utils/template_admin.R) lets someone upload a new `.pptx` template (and an optional preview screenshot) through the app, no git commit required.

## Think-cell export workflow

For each chart, `chart_data_downloads_ui()`/`chart_data_downloads_server()` wire up to three buttons:

- **Download data (raw)** — exact plot data as used in the ggplot.
- **Download data (think-cell)** — formatted matrix for linking in PowerPoint (only for supported chart types).
- **Download slide (PowerPoint)** — shown whenever a `templates/*.pptx` template matches the chart type; returns a ZIP with the slide (or, if think-cell isn't installed on whatever machine renders it, the template + a ready-to-render `.ppttc` data file + instructions), the underlying table, and a log of exactly which options produced it.

Supported chart types: `line`, `bar`, `stacked_bar`, `grouped_bar`, `waterfall`. Templates exist for a wider set of chart shapes than the raw/think-cell export supports — see `TC_TEMPLATE_BY_CHART_TYPE` in `utils/slide_download.R`.

PM workflow (no think-cell installed): download the think-cell `.xlsx`, open in Excel, link the range to a think-cell chart in PowerPoint. PM workflow (think-cell installed wherever the app runs): download the slide ZIP directly.

### Favorites, export history, and templates

Next to the slide button, "☆ Save as favorite" snapshots that chart's current export into a shared (per-dashboard, not per-user) list on the **Favorites** tab, where "Download all favorites" bundles every starred chart into one combined ZIP: `favorites_thinkcell_tables.xlsx` and `favorites_raw_tables.xlsx` (one sheet per favorite each, matching names), one self-contained `charts_overview.html` bundling every favorite's Plotly snapshot (captured client-side at star-time — see `plot_output_id` on `chart_data_downloads_ui()`) as one ordered page instead of separate image files (see `tc_build_charts_overview_html()`), and either a rendered multi-slide deck or the same graceful template+`.ppttc` fallback the single-chart download uses. A favorite is labeled with the chart's own title (falling back to the sub-tab name if the chart has none) and shows when it was saved; "Remove all" (behind a confirmation) clears the list at once. A favorite is a snapshot taken at star-time, not a live query — re-run it (star it again) after the underlying data changes, or use Export History's regenerate control below to get one against current data without re-starring.

`favorites_panel_ui()`/`favorites_panel_server()` can be mounted more than once: pass `tab_label_filter` (matching a favorite's own `tab_label`) to show, download, and "Remove all" only the favorites starred from one top-level tab — mount one filtered instance per tab (each its own module id) alongside one unfiltered instance for the main "Favorites" tab, which always shows the cumulative, all-tabs list. All instances read/write the same shared `favorites.json`, so nothing needs to be kept in sync manually.

Every "Download slide" click is *also* logged automatically to the **Export history** tab (no star needed) — a durable, searchable, timestamped record. Each entry gets a short **download id**, and every chart from the same "Download all favorites" click additionally shares one **favorite_download_id** — both embedded in the exported `.ppttc`/datasheet (and, if a template designer binds a `DownloadID`/`FavoriteDownloadID` automation field, visibly on the rendered slide itself), so a chart spotted in a real PowerPoint deck can be traced back to its history entry, and a whole bulk download can be found as one group (shown as an expandable row when it has more than one member).

Two ways to get a chart back from history, both available instantly per row and, via a checkbox on each row plus a bottom summary banner, across an arbitrary multi-row selection:
- **Redownload** — exact-snapshot replay, byte-for-byte the same ZIP; selecting several rows combines them into one deck instead of one zip per click (`export_history_download_many()`).
- **Regenerate** — rebuilds against **today's live dashboard data** if that chart is still active in the current session (via a per-session chart registry — see `tc_chart_registry_register()`/`tc_chart_registry_get()` in `utils/slide_download.R`), falling back to the last-known snapshot otherwise; selecting several rows mints one shared `favorite_download_id` across the whole regenerated batch, same as a bulk "Download all favorites" click (`export_history_regenerate_many()`). Either way it mints brand-new ids and logs a fresh entry — it never overwrites or reuses the entry it started from. Checking a bulk-download row's checkbox acts on that whole group as one unit, same as its own per-row Regenerate button.

Every exported chart also carries a provenance log *inside the chart element itself*: `tc_build_ppttc_slide_block()` writes it as a single string into the datasheet's corner cell (row 1, column 1) — the one cell think-cell's own JSON automation manual documents as unused by the figure (not a category, not a series, not a data point, and not rendered on the slide). Because it lives in the chart's embedded datasheet rather than a linked Excel file or a separate log file, it travels with the chart wherever it's copied — to a new slide, a new deck, anywhere — without any extra wiring per chart, and there is no `log.txt` in the ZIP; this corner cell is the one provenance record. It's provenance, not security: anyone who opens the chart's datasheet in PowerPoint (double-click the chart) can see or edit that cell. "Download data (think-cell)" — the plain `.xlsx` a PM opens to link a range into their own think-cell chart — carries the same log the same way, stamped onto its corner *header* cell instead (`tc_stamp_tc_matrix_corner()` in `utils/format_thinkcell_download.R`), since that export never goes through a ppttc chart datasheet at all.

The log always includes a generation timestamp and the download id(s) above, plus, when a dashboard's chart data is assembled from named external outputs (a pipeline output id, a workbook sheet name, etc.), optional `source_output`/`source_sheet` identifiers that ride along in the same log line (`output=...`/`sheet=...`) — see `tc_build_datasheet_log()` in `utils/slide_download.R`. Pass them (via `chart_data_downloads_server()`/`favorites_capture()`) whenever your dashboard has that concept, so a chart found later can be traced all the way back to the exact source file/sheet that produced it, not just the dashboard tab. The log only lists *that chart's own* selected options, not every input in the app — see `dl_option_prefixes` on `tc_register_app_context()` in the Conventions below.

The **Manage templates** tab lets anyone upload a new `.pptx` template (plus an optional preview screenshot, shown as a thumbnail) without a developer committing it to git — uploads land in `state/template_uploads/`, which (like `state/favorites.json` and `state/export_history/`) sits outside every folder the deploy workflow syncs, so all three survive a redeploy automatically. (Uploads write to `state/` rather than `templates/` because the deploy-owned `templates/` directory is often read-only for the app process on a server.) Every chart's own "Slide template" picker (right next to its download buttons) shows those same previews *inline in the dropdown* — a thumbnail next to each template name, so a PM can scroll through and see every option before picking one, rather than choosing blind by name and checking Manage Templates separately; the currently-picked template's thumbnail stays visible once the dropdown closes too.

All of a chart's downloads/preview/favorite controls (the three download buttons, the template + category-order pickers, and "☆ Save as favorite") render inside one visually contained panel via `chart_data_downloads_ui()`, rather than as a loose sequence of separate widgets.

## Dependencies

Install these R packages before running the app or tests:

```r
install.packages(c("shiny", "shinymanager", "dplyr", "ggplot2", "tidyr", "tibble", "readxl", "writexl", "jsonlite", "testthat"))
```

## Run Locally

From an R session in the project folder:

```r
shiny::runApp("app.R")
```

If needed, set the working directory first:

```r
setwd("c:/Users/MarcoGriepAHTI/Git Repos/shiny_dashboard_template")
shiny::runApp("app.R")
```

## Run Tests

```r
testthat::test_dir("tests")
```

## Notes

- The template app sources the shared helper files from `app.R`.
- Deployment settings live in `deploy.env`; set `APP_FOLDER` to the project name used under `/apps/`.
- The deploy workflow ships `app.R`, `data/`, `utils/`, and `templates/` (the built-in templates only —
  `state/` is runtime state, never synced, so uploaded templates and favorites survive a redeploy).
