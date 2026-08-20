---
name: thinkcell-export
description: Standardize think-cell data exports from Shiny dashboard charts. Use when adding or editing a chart (in app.R, utils/**/*.R, or any modules/**/*.R) that has, or should have, download buttons.
---

# Think-cell chart data exports

When adding or editing charts with download buttons, follow this pattern.

## Download buttons

Every chart that supports export offers, via `chart_data_downloads_ui()` /
`chart_data_downloads_server()` in `utils/chart_downloads.R`:

1. **Download data (raw)** — the same data passed through `format_tc_data()` as the think-cell
   download below (same matrix shape, so the two are directly comparable side by side), just
   without the corner-cell provenance stamp. Falls back to the exact, unreshaped plotting data
   frame only for a chart type think-cell doesn't support yet.
2. **Download data (think-cell)** — the same data passed through `format_tc_data()`, written as `.xlsx`.
3. **Download slide (PowerPoint)** — shown automatically whenever a `templates/*.pptx` template
   matches the chart type (see `utils/slide_download.R`); returns a ZIP with the slide (or a
   graceful template+`.ppttc`+instructions fallback when no think-cell renderer is available)
   and the underlying table (no `log.txt` — see "Provenance log" below). Every click is also
   logged automatically to the shared **Export history** tab (`utils/export_history.R`) with a
   short `download_id`, so it can be redownloaded exactly later, or *regenerated* against
   today's live data — no wiring needed per chart, this happens inside
   `chart_data_downloads_server()` itself. Next to the slide button, a "☆ Save as favorite"
   button snapshots that export into the shared **Favorites** tab (`utils/favorites.R`) for
   later bulk download; every chart in one "Download all favorites" click shares one
   `favorite_download_id`, so the whole batch can be found and regenerated as a group.

All of this (the three buttons, the "Slide template"/"Category order" pickers, the favorite
button) renders inside one visually contained panel, not a loose sequence of widgets. The "Slide
template" picker shows every template's preview thumbnail *inline in the dropdown* (see
`tc_template_choice_items()`/`TC_TEMPLATE_PICKER_RENDER_JS` in `utils/chart_downloads.R`), so a
PM can scroll through and see them before picking one — no separate preview box, and no trip to
Manage Templates required. "Download all favorites" bundles every favorite's captured chart
image into one `charts_overview.html` (in favorite order), not separate `.png` files — see
`tc_build_charts_overview_html()` in `utils/favorites.R`.

The think-cell button is shown only when `chart_type` is in `TC_SUPPORTED_CHART_TYPES`:

- `line`
- `bar`
- `stacked_bar`
- `grouped_bar`
- `waterfall`

If a chart does not match one of these types, wire only the raw download (or omit think-cell export until the chart type is added to `format_thinkcell_download.R`). The slide button has its own, wider chart-type-to-template mapping — see `TC_TEMPLATE_BY_CHART_TYPE` in `utils/slide_download.R` — and is independent of think-cell-export support.

## ggplot to export column mapping

| ggplot aesthetic | `format_tc_data()` argument |
|------------------|----------------------------|
| `aes(x = ...)`   | `category_col`               |
| `aes(fill = ...)` or `aes(color = ...)` or `aes(group = ...)` | `series_col` |
| `aes(y = ...)`   | `value_col`                  |
| `facet_wrap(~ ...)` / `facet_grid(...)` | `facet_col` (optional; one Excel sheet per facet level) |

Set `chart_type` explicitly to match the think-cell chart the PM will build in PowerPoint.

- `line` → column/line orientation (series in rows, categories in columns)
- `bar`, `stacked_bar`, `grouped_bar` → bar orientation (transposed matrix)
- `stacked_bar` vs `grouped_bar` → same Excel layout; stacking is configured in think-cell

## Preserving the plotted bar/category order

The exported Excel matrix must show categories (and series) in the **same order the
chart plots them**, not alphabetically. `format_tc_data()` decides the order like this:

1. Explicit `category_order` / `series_order` argument (a character vector) — always wins.
2. Otherwise, if the column is a **factor**, its `levels()` are used — and because ggplot
   also draws bars in factor-level order, this makes the export match the figure for free.
3. Otherwise: numeric columns sort ascending; plain character columns fall back to
   alphabetical (the case to avoid).

So whenever a chart orders its axis deliberately (by value, by a custom sequence, reverse
chronological, …), **give the download the same ordering you give ggplot**:

- Preferred: pass the *already factor-ordered* data to both the plot and
  `chart_data_downloads_server()` (e.g. `mutate(quarter = factor(quarter, levels = my_order))`
  upstream, so plot and export share one source of truth), **or**
- pass `category_order = my_order` / `series_order = my_order` explicitly to the download server.

Never rely on the default alphabetical order for a chart whose bars are intentionally ordered.

## Rules

- Never hand-roll `pivot_wider()` for think-cell exports in `app.R` or modules.
- Pass the same reactive/filtered data to both the plot and the download handlers.
- Prefer `agg_fun = NULL` when the plot data is already aggregated.
- Keep the plotted order: pass factor-ordered data or `category_order`/`series_order` (see
  "Preserving the plotted bar/category order" above).
- Add new chart types in `utils/format_thinkcell_download.R` with tests before exposing the think-cell button.
- New `.pptx` slide templates go in `templates/` (built-in) or can be added at runtime via the
  **Manage templates** panel (`utils/template_admin.R`), which writes to the runtime uploads dir
  (`state/template_uploads/`) — a git commit is never required just to add a template.
- Pass `plot_output_id` (the chart's plain, un-namespaced `plotlyOutput()` id) to
  `chart_data_downloads_ui()` whenever the chart is Plotly-based, so starring it also captures a
  PNG snapshot for the favorites deck ZIP. Omit it for `renderPlot()`-based charts — the favorite
  still saves, just without an image.
- If this dashboard's chart data is assembled from named external outputs (a pipeline output id,
  a workbook sheet name, etc.), pass `source_output`/`source_sheet` to
  `chart_data_downloads_server()` (and `favorites_capture()`, if the chart supports favoriting) so
  every export's embedded log (see "Provenance log" below) can be traced back to the exact source,
  not just the dashboard tab. Skip both when there's no such concept.
- Set `dl_option_prefixes` on the app's one `tc_register_app_context()` call for every chart
  wired up: a named character vector mapping `chart_data_downloads_server(id = ...)` to a regex
  matching *that chart's own* input ids (e.g. `c("my_chart_downloads" = "^my_chart_")`). Skipping
  a chart here falls back to logging every non-plumbing input in the whole app to its provenance
  log — noisy, and rarely what you want.
- Every chart's plot should have a dedicated, real title (`ggtitle()`/`labs(title=)`/plotly
  `layout(title=)`) — compute it once in a reactive and pass the *same* reactive as both the
  plot's title and `figure_title` (or `slide_title`) here, so Favorites can label a starred
  chart with its actual title instead of falling back to the sub-tab name. The sub-tab fallback
  is only for a chart that genuinely has no title.

## Provenance log

Every export this module offers embeds the same provenance log — a generation timestamp,
`download_id` (+ `favorite_download_id` when part of a bulk download), dashboard/tab/sub-tab,
chart type, selected options, and (when supplied) `source_output`/`source_sheet` — built by
`tc_build_datasheet_log()` in `utils/slide_download.R`. There is no `log.txt`; this is the one
provenance record every export carries:

- **Slide (+ favorites deck) downloads**: written into the chart's own think-cell *datasheet*,
  corner cell (row 1, column 1) — the cell think-cell's JSON automation manual documents as
  unused by the figure, so it rides along invisibly inside the chart element itself and survives
  the chart being copied to a new slide or deck. The same ids are also written as their own
  `DownloadID`/`FavoriteDownloadID` automation data blocks, so a template designer can bind a
  small text box to make them visible on the rendered slide too.
- **"Download data (think-cell)"**: the same log, stamped onto the *header* of that `.xlsx`'s
  first (unused, by convention `""`) column instead, via `tc_stamp_tc_matrix_corner()` in
  `utils/format_thinkcell_download.R` — this export never goes through a chart datasheet.

Both are wired automatically inside `chart_data_downloads_server()` — no per-chart work needed
beyond optionally passing `source_output`/`source_sheet`.

## Regenerating against live data

Export History's "Regenerate" control rebuilds a chart (or, given a `favorite_download_id`, a
whole bulk download as one group) against *today's* dashboard data instead of replaying a frozen
snapshot — but only while that chart's module is still mounted in the *current* Shiny session,
via a session-scoped registry (`tc_chart_registry_register()`/`tc_chart_registry_get()` in
`utils/slide_download.R`) that `chart_data_downloads_server()` populates automatically. No wiring
needed per chart. If the registry has nothing for a chart (e.g. the app restarted since it was
last downloaded), it falls back to an exact-snapshot rebuild instead of failing.

## Example wiring

```r
plot_data <- reactive({ filtered_chart_data() })

output$my_chart <- renderPlot({
  plot_data() %>%
    ggplot(aes(x = quarter, y = revenue, fill = product)) +
    geom_col(position = "stack")
})

chart_data_downloads_server(
  id = "my_chart_downloads",
  data = plot_data,
  chart_type = "stacked_bar",
  category_col = "quarter",
  series_col = "product",
  value_col = "revenue",
  filename_prefix = "revenue_chart",
  agg_fun = NULL
)
```
