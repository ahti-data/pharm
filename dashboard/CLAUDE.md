# Shiny Dashboard Template

This repo is a starter template for Shiny dashboards, not a finished app. `app.R` currently
contains a scaffold/example (revenue-by-quarter chart) meant to be replaced with real dashboard
logic — don't treat its contents as fixed requirements.

## Sibling deployments

Real dashboards are built by copying this template's `utils/` into their own repo and wiring up
their own `app.R` against real chart data (e.g. `RVS_laatste_1000_dagen`, checked out as a sibling
under the same `Git Repos` parent directory — see its own `CLAUDE.md` for that project's specifics).
A fix or feature added to `utils/` here is almost always relevant there too: after changing
anything under `utils/`, check whether the sibling repo(s) need the same change, port it with
`git apply`/manual edit (paths differ slightly, e.g. `utils/` here vs `dashboard/utils/` there),
and run that repo's own test suite before considering the work done — see `tests/testthat/` below
for how to run the suite in this repo; adapt the path for a sibling.

## Structure

- `data/` — project input files.
- `data/metadata/` — shared metadata and branding helpers (e.g. `brand_colors.R`).
- `utils/` — reusable functions shared across the app.
- `templates/` — built-in think-cell `.pptx` slide templates for the "Download slide" export
  (committed to git, shipped by the deploy workflow).
  - `templates/previews/<name>.png` — optional, curated-by-hand screenshot for a built-in
    template (same stem as the `.pptx`, e.g. `template_v_bar.png`), shown as a thumbnail on the
    "Manage templates" tab. There's no server-side pptx rendering pipeline -- a missing preview
    just means no thumbnail, never a broken template. Can also be added at runtime through the
    same tab (see `utils/template_admin.R`).
- `state/` — runtime state, never committed and never synced by the deploy workflow:
  - `dictionary.json` — the shared raw-name -> pretty-label lookup for chart bar/category
    labels (see `utils/dictionary.R`), edited from the **Dictionary** tab
    (`utils/dictionary_admin.R`). Prefilled on first use from `dictionary_seed_entries()` — the
    template's own default is empty; a real dashboard overrides that function (defined *after*
    sourcing `dictionary.R`, e.g. from its own `data/metadata/dictionary_seed.R`) with entries
    derived from its actual raw data-source names. A user edit always wins, since edits upsert
    into the same file the seed was written into. Override the location with the
    `SHINY_DICTIONARY_PATH` env var. Deliberately only covers bar/category *values*, never axis
    titles.
  - `favorites.json` — the shared per-dashboard favorites list (see `utils/favorites.R`).
    A favorite has no PNG storage of its own -- a client-captured screenshot taken when a favorite
    is actually downloaded (via `TC_CHART_CAPTURE_JS` in `utils/slide_download.R`) is written into
    `export_history_assets/` below, keyed by a freshly-minted `download_id`, the same as any other
    export's captured image -- so it isn't deleted when the favorite itself is removed.
  - `template_uploads/` — templates uploaded at runtime through the "Manage templates" tab (see
    `utils/template_admin.R`). Kept here (not under `templates/`) because the app process must be
    able to *write* uploads, whereas the deploy-owned `templates/` is often read-only on a server;
    an uploaded template overrides a built-in of the same name. Override the location with the
    `SHINY_TEMPLATE_UPLOADS_DIR` env var — set it to the *same* absolute path across several
    dashboard deployments on the same host to share one runtime-uploaded template set between
    them, instead of each dashboard keeping its own; uploads already write under a file lock
    (`tc_with_file_lock()` in `utils/slide_download.R`), so concurrent uploads from different
    dashboard processes pointed at the shared path are safe. This is a server/deployment
    configuration change (set the env var in each dashboard's own process environment) — no code
    change needed here.
    - `template_uploads/previews/<name>.png` — same preview convention, for uploaded templates,
      and follows the same `SHINY_TEMPLATE_UPLOADS_DIR` override automatically.
  - `export_history/<id>.json` — one file per "Download slide" click, logged automatically (see
    `utils/export_history.R`), each holding a frozen snapshot so it can be redownloaded exactly
    later from the **Export history** tab, however long ago it was created. Distinct from
    `favorites.json`: history is automatic and complete, favorites are manually curated. Override
    with `SHINY_EXPORT_HISTORY_DIR`. Every entry also gets a short `download_id`; every entry from
    the same "Download all favorites" click additionally shares one `favorite_download_id`, both
    embedded in the export's own datasheet corner cell (see `tc_build_datasheet_log()`) so a
    bulk download can be found and regenerated as one group later. "Regenerate" (as opposed to
    the always-instant, exact-snapshot "Redownload") rebuilds a chart against *today's* live
    dashboard data via a per-session chart registry (`tc_chart_registry_register()`/
    `tc_chart_registry_get()` in `utils/slide_download.R`) — it only works for a chart still
    mounted in the current session; otherwise it falls back to the last-known snapshot. There is
    no `log.txt` in any export ZIP — the datasheet corner cell is the one provenance record.
  - `export_history_assets/<download_id>.png` — the client-captured screenshot for each export
    history entry (see `TC_CHART_CAPTURE_JS` in `utils/slide_download.R` and
    `export_history_asset_path()` in `utils/export_history.R`), embedded into that export's ZIP as
    part of `charts_overview.html`. Sibling directory to `export_history/`, same override rules.
- `app.R` — the dashboard UI and server logic; sources the helpers above.
- `tests/testthat/` — testthat tests.
- `deploy.env` — deployment settings (`APP_FOLDER` sets the project name under `/apps/`).

## Conventions

- Add reusable logic to `utils/`, not inline in `app.R`.
- The deploy workflow ships only `app.R`, `data/`, `utils/`, and `templates/` (the built-in
  templates). `state/` (favorites + uploaded templates) is runtime state, never part of the
  synced deploy folder, so it survives a redeploy — keep runtime dependencies inside those
  directories.
- New chart types for think-cell export go in `utils/format_thinkcell_download.R` with tests
  before being exposed in the UI (see `utils/chart_downloads.R` for the wiring pattern, and
  `utils/slide_download.R` for the PowerPoint slide export).
- If a dashboard's chart data is assembled from named external outputs (a pipeline output id,
  a workbook sheet name, etc.), pass `source_output`/`source_sheet` when wiring
  `chart_data_downloads_server()` (and `favorites_capture()`, if the chart supports favoriting).
  They ride along in every export's embedded provenance log (see `tc_build_datasheet_log()` in
  `utils/slide_download.R`) so a chart found later can be traced back to the exact source
  file/sheet, not just the dashboard tab. Skip both for a dashboard with no such concept.
- Each chart's provenance log only lists *that chart's own* selected options, not every input in
  the app — set `dl_option_prefixes` on `tc_register_app_context()` (a named character vector,
  `chart_data_downloads_server(id=...)` -> a regex matching that chart's own input ids) for every
  chart wired up. Skipping a chart here falls back to logging every non-plumbing input in the
  whole app, which is noisy and rarely what you want.
- Every chart's plot should have a dedicated, real title (`ggtitle()`/`labs(title=)`/plotly
  `layout(title=)`) — compute it once in a reactive shared by both the plot and
  `chart_data_downloads_server(figure_title = ...)`/`favorites_capture(figure_title = ...)`, so
  Favorites can label a starred chart with its actual title instead of falling back to the
  sub-tab name (the fallback is only for a chart that genuinely can't have one).
- A chart's bar/category *values* (never axis titles) are dictionary-formatted in two, separate
  places on purpose, not one shared reactive: the plot's own render call applies
  `dictionary_relabel()` (`utils/dictionary.R`) directly (e.g. a `plot_data_pretty <-
  reactive({ plot_data() %>% mutate(!!category_col := dictionary_relabel(.data[[category_col]],
  scope = category_col)) })` feeding only `renderPlot()`/`renderPlotly()`), while
  `chart_data_downloads_server(data = plot_data, ...)` gets the *raw*, undictionaried reactive
  and applies its own `dictionary_relabel()` internally, gated by its own per-chart "Format from
  dictionary" checkbox (default on) -- see `utils/chart_downloads.R`. This split exists so
  unchecking that checkbox can produce a genuinely raw/source-traceable download without also
  changing what the chart shows on screen. Pass `category_scope`/`series_scope` to
  `chart_data_downloads_server()` whenever `category_col`/`series_col` aren't themselves
  meaningful dictionary scopes (e.g. several charts sharing a generic column name like
  `"category"` for different kinds of raw values) -- otherwise downloads silently fall back to
  `dictionary_default_prettify()` instead of the intended lookup. Skip this whole split for a
  chart whose category/series values are already composites built from *several* independently
  dictionary-formatted pieces (e.g. `paste(a, b, sep = " | ")`) -- there's no single raw value or
  scope left to relabel at that point, so wiring the checkbox for such a chart would need to
  start further upstream, at the pieces themselves, not at `chart_data_downloads_server()`.
- **Overriding a `utils/`-defined hook (like `dictionary_seed_entries()`) from `app.R` needs
  either a separately-`source()`d file, or `<<-` if defined inline — never a plain top-level `<-`
  directly in `app.R`.** `shiny::runApp()` sources `app.R` itself into its own private child
  environment; every `utils/*.R` file, by contrast, is sourced with `source(path, local = FALSE)`
  (see `source_util()`-equivalent `source()` calls at the top of `app.R`), which always lands in
  R's *global* environment regardless of the caller's own environment. A function defined in
  `utils/dictionary.R` (e.g. `dictionary_list()`) resolves free variables via *its own* lexical
  scope — the global environment — not wherever it happens to be called from. So: a real
  dashboard's `dictionary_seed_entries()` override defined in its own file and `source()`d the
  same `local = FALSE` way (as the bullet above already implies) lands in the same global
  environment and works correctly. But a dashboard that instead writes `dictionary_seed_entries
  <- function() {...}` directly inside `app.R` creates a binding local to `app.R`'s own private
  environment — invisible to `dictionary_list()`, which keeps silently calling the original,
  unoverridden default. No error, just a permanently-unchanged result. If you ever do need to
  override a `utils/`-defined function or default *inline* in `app.R`, use `dictionary_seed_entries
  <<- function() {...}` instead — superassignment walks up to (and updates in place) the existing
  global binding rather than shadowing it locally. This exact mistake broke a real dashboard's
  production deploy once (crashed on every fresh start with no pre-existing `state/` file) before
  being caught — treat it as a hard rule, not a suggestion.
- Never call `Position()`, `filter()`, `intersect()`, or `union()` unqualified once `ggplot2`/
  `dplyr` are loaded (as `app.R` always does) — `ggplot2` attaches its own `Position` ggproto
  object ahead of `base::Position` on the search path, and `dplyr` masks `filter()`/`intersect()`/
  `union()` from `base`/`stats`. A bare call silently resolves to the wrong thing instead of
  erroring. Use a plain `for` loop (or `which(vapply(...))[1]`) instead of `Position()`, and an
  explicit `base::`/`dplyr::` prefix for the others.
- **Adding a new chart, step by step:** (1) build its data-prep reactive; (2) pick or register a
  `chart_type` in `utils/format_thinkcell_download.R` (with tests, before exposing it in the UI —
  see `utils/chart_downloads.R` for the wiring pattern); (3) compute a real title once in a
  reactive shared by the plot and `chart_data_downloads_server(figure_title=...)`; (4) if this
  dashboard's data comes from named external outputs, pass `source_output`/`source_sheet` (and
  `source_mtime`); (5) register this chart's `dl_option_prefixes` on `tc_register_app_context()` —
  **the template's own scaffold in `app.R` never actually calls `tc_register_app_context()`**, so
  there's no worked example to copy here; without it, this chart's provenance log falls back to
  logging every non-plumbing input in the whole app; (6) decide whether the dictionary split
  applies (the bullet above) and pass `category_scope`/`series_scope` if `category_col`/
  `series_col` aren't themselves meaningful scopes. `chart_data_downloads_server()`'s own roxygen
  block (`utils/chart_downloads.R`, above its definition) is the authoritative reference for its
  full, still-growing parameter list — this recipe is the order to work through them in, not a
  substitute for reading it.
- `utils/template_admin.R`'s helpers are deliberately prefixed `tmpl_*`, not `template_admin_*`
  like every sibling admin module's functions (`favorites_*`, `dictionary_*`, `export_history_*`)
  — a known, harmless naming inconsistency from before that convention solidified. Don't "fix" it
  as a drive-by change; a real rename is low-value on its own and not worth the diff noise.

## Dependencies

```r
install.packages(c("shiny", "dplyr", "ggplot2", "tidyr", "tibble", "writexl", "jsonlite", "testthat", "filelock", "rlang", "htmltools"))
```

## Running

```r
shiny::runApp("app.R")
```

## Tests

```r
testthat::test_dir("tests")
```

Run the test suite after changing anything in `utils/`. `tests/testthat.R` sources every
`utils/*.R` file before running — when adding a new one, add its `source()` line there too, or its
tests will fail with "function not found" instead of a clean signal.

Conventions used across `tests/testthat/`:
- Each state-file-backed module's tests sandbox themselves with a local `with_<thing>_path()`
  helper (e.g. `with_favorites_path()`, `with_dictionary_path()`) that points the relevant
  `SHINY_*_PATH`/`SHINY_*_DIR` env var at a `tempfile()`/`tempdir()` for the duration of one
  `test_that()` block, restoring the previous value via `on.exit()`. Each test file defines its
  own copy rather than sharing one — follow that pattern for a new module rather than
  centralizing it.
- `tests/testthat/helper-session.R`'s `fake_session()` (a bare `list(userData = new.env())`) is
  the stand-in wherever a test needs `session$userData` without a real Shiny session (e.g. the
  per-session chart registry in `utils/slide_download.R`).
- `shiny::testServer()` (for exercising a `moduleServer()`'s reactives/observers directly, e.g.
  `chart_data_downloads_server()`, `dictionary_admin_server()`) is used in `test-chart_downloads.R`
  and `test-dictionary_admin.R` — after changing an input that should trigger observers registered
  *inside another reactive* (e.g. dynamically-created per-row buttons), call
  `session$flushReact()` before simulating a click on one; merely reading the reactive that
  creates them isn't enough to flush the rest of the graph.
