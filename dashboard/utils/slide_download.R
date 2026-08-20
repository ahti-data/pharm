# =============================================================================
# think-cell slide (PowerPoint) ZIP download helpers
# =============================================================================
# Turns the exact data that already backs the dashboard's think-cell TABLE
# download into a downloadable ZIP containing:
#
#   1. <prefix>_table.xlsx  - the same think-cell matrix as the table download
#                             (so the underlying data always matches the slide).
#   2. slide.pptx           - a think-cell slide rendered from the template that
#                             matches the displayed chart type. Its own datasheet
#                             corner cell carries the provenance log (dashboard /
#                             tab / sub-tab / selections / download id) -- see
#                             tc_build_datasheet_log() -- there is no log.txt.
#
# If think-cell (ppttc.exe) is not installed on the host, the ZIP instead
# ships the (valid, non-corrupt) template plus the ready-to-render `.ppttc`
# data file and a short README, so the user can finish the slide in one click.
#
# Design goals (see CLAUDE.md):
#   * small, pure, individually unit-testable functions
#   * a single source of truth for "which template matches which chart"
#   * reuses the rendering approach proven in R/thinkcell_shiny_app.R
# =============================================================================

# Internal null-coalescing helper. Kept local (tc_or) so this file does not
# depend on, nor collide with, the app-level `%||%`.
tc_or <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Current timestamp string in the dashboard's local timezone, regardless of
#' the server process's own system timezone (commonly UTC on a deployed
#' server, which would otherwise stamp every log/history/favorite timestamp
#' a couple hours off from Dutch wall-clock time). Every stamped or logged
#' timestamp anywhere in the app should go through this rather than a bare
#' `Sys.time()`/`format(Sys.time())`.
#' @param fmt strftime format string.
tc_now <- function(fmt = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), fmt, tz = "Europe/Amsterdam")
}

#' A fresh, sortable-by-creation-time id: `<prefix><timestamp>_<6 random
#' alnum chars>`. Shared by every module that mints its own entry ids
#' (`favorites_new_id()`/`favorites_download_new_id()` in `utils/favorites.R`,
#' `export_history_new_id()` in `utils/export_history.R`) so the shape only
#' needs to be right in one place.
#' @param prefix Short string identifying what kind of id this is (e.g. `"fav_"`).
tc_new_id <- function(prefix) {
  paste0(
    prefix, tc_now("%Y%m%d%H%M%S"), "_",
    paste(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = "")
  )
}

#' Run `fn` while holding an exclusive lock on a `.lock` sidecar next to
#' `path`, so concurrent read-modify-write cycles against the same shared
#' state file (`favorites.json`, a template upload) don't race each other.
#' Locks a sidecar rather than `path` itself so readers that don't go through
#' this helper are never blocked.
#'
#' Degrades to running `fn()` unlocked (with a `warning()`, never an error)
#' if the `filelock` package isn't installed or a lock can't be acquired in
#' time -- this repo deploys via a plain file copy with no package-install
#' step (see `.github/workflows/deploy.yml`), so a server that never had
#' `filelock` installed must not have every favorite star/unstar crash the
#' user's session; losing lock protection on rare contention is a far better
#' failure mode than that.
#' @param path The shared file (or directory) being protected; the actual
#'   lock file is `paste0(path, ".lock")`.
#' @param fn Zero-arg function to run while holding the lock.
#' @param timeout Milliseconds to wait for the lock before giving up.
tc_with_file_lock <- function(path, fn, timeout = 10000) {
  if (!requireNamespace("filelock", quietly = TRUE)) {
    warning(
      "Package 'filelock' is not installed -- running '", path, "' update ",
      "without a lock. Install it (install.packages(\"filelock\")) to ",
      "prevent lost updates under concurrent use.",
      call. = FALSE
    )
    return(fn())
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock <- tryCatch(
    filelock::lock(paste0(path, ".lock"), timeout = timeout),
    error = function(e) NULL
  )
  if (is.null(lock)) {
    warning(
      "Could not acquire a lock on '", path, "' within ", timeout,
      "ms -- running the update without one.",
      call. = FALSE
    )
    return(fn())
  }
  on.exit(filelock::unlock(lock))
  fn()
}

#' Read a shared "flat JSON array of entries" state file (the shape
#' `favorites.json`/`dictionary.json` both use), or an empty list if it
#' doesn't exist yet or fails to parse. Shared by `utils/favorites.R` and
#' `utils/dictionary.R` so this read behavior only needs to be right once.
#' @param path Path to the JSON file.
tc_json_list_read <- function(path) {
  if (!file.exists(path)) return(list())
  entries <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(entries)) return(list())
  entries
}

#' Write a shared "flat JSON array of entries" state file, creating its
#' directory if needed. Counterpart to [tc_json_list_read()].
#' @param entries List of entries.
#' @param path Destination path.
tc_json_list_write <- function(entries, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(entries, path, auto_unbox = TRUE, null = "null", na = "null")
  invisible(path)
}

#' Format a source data file's last-modified time for the `source_updated=`
#' field in [tc_build_datasheet_log()] -- same format/timezone as [tc_now()],
#' so the two are directly comparable on a datasheet's corner cell.
#' @param path Path to the source file (e.g. one resolved via
#'   `resolve_existing_path()`/`resolve_iteration3_subfile()` in `app.R`).
#' @return Formatted string, or `""` if `path` is `NA`/empty/doesn't exist.
tc_format_source_mtime <- function(path, fmt = "%Y-%m-%d %H:%M:%S") {
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) return("")
  format(file.info(path)$mtime, fmt, tz = "Europe/Amsterdam")
}

# ---------------------------------------------------------------------------
# Session-scoped chart registry: lets Export History "regenerate" a chart
# against *today's* live data, not just replay a frozen snapshot.
#
# `chart_data_downloads_server()` (utils/chart_downloads.R) registers a
# zero-argument-except-zip-path build function per chart id, so a later
# regenerate (utils/export_history.R) can call it again on demand. Backed by
# `session$userData`, which Shiny shares across every module in one user
# session -- so this only ever sees charts mounted in the *current* session;
# a chart from a different/older session simply isn't in it (the caller
# falls back to an exact-snapshot rebuild in that case).
# ---------------------------------------------------------------------------
tc_chart_registry <- function(session) {
  if (is.null(session$userData$tc_chart_registry)) {
    session$userData$tc_chart_registry <- new.env(parent = emptyenv())
  }
  session$userData$tc_chart_registry
}

#' Register a chart's "build this export right now" function.
#' @param session The Shiny session (module or top-level -- `userData` is shared).
#' @param id The chart's `chart_data_downloads_server(id = ...)`.
#' @param build_fn `function(zip_path, favorite_download_id = NULL)` that
#'   rebuilds this chart's slide ZIP from *current* reactive data.
tc_chart_registry_register <- function(session, id, build_fn) {
  assign(id, build_fn, envir = tc_chart_registry(session))
  invisible(NULL)
}

#' Look up a registered chart's build function, or `NULL` if this session
#' never mounted (or no longer has) that chart id.
tc_chart_registry_get <- function(session, id) {
  if (is.null(id) || !nzchar(id)) return(NULL)
  reg <- tc_chart_registry(session)
  if (!exists(id, envir = reg, inherits = FALSE)) return(NULL)
  get(id, envir = reg, inherits = FALSE)
}

# ---------------------------------------------------------------------------
# App context: registered once by the main server so the download module can
# label the log with the dashboard name, the active tab / sub-tab, and a
# snapshot of every option the user has selected -- without editing each of
# the (many) per-chart download wirings.
# ---------------------------------------------------------------------------
.TC_CTX <- new.env(parent = emptyenv())

# Inputs that are UI plumbing rather than figure-defining options; excluded
# from the "selected options" log section.
TC_INTERNAL_INPUT_PATTERN <- paste0(
  "(_cell_edit$|_cell_clicked$|_rows_|_columns_|_state$|_search$|_search_",
  "|plotly_|_hover$|hoverData|clickData|relayout|brush|_open$|active_tab$",
  "|^\\.|_dl-|-thinkcell$|-raw$|-slide$|-dictionary_format$)"
)

#' Register the running app's context for slide-export logging.
#'
#' @param input The top-level Shiny `input`.
#' @param dashboard_title Human-readable dashboard name.
#' @param nav_id Input id of the top-level navbar (e.g. "main_nav").
#' @param subtab_by_tab Named character vector mapping each top-level tab value
#'   to the input id of its tabset (e.g. c("Iteratie 1" = "iter1_tabs")).
#' @param dl_option_prefixes Named character vector mapping each download module
#'   id to a regex matching the option inputs that define that chart's figure
#'   (e.g. c("iter1_totaal_dl" = "^tot_")). When a module id is found here, only
#'   the matching inputs are logged; otherwise all non-plumbing inputs are.
#' @param internal_pattern Optional regex overriding [TC_INTERNAL_INPUT_PATTERN].
tc_register_app_context <- function(input,
                                    dashboard_title = "",
                                    nav_id = NULL,
                                    subtab_by_tab = NULL,
                                    dl_option_prefixes = NULL,
                                    internal_pattern = NULL) {
  .TC_CTX$input              <- input
  .TC_CTX$dashboard_title    <- dashboard_title
  .TC_CTX$nav_id             <- nav_id
  .TC_CTX$subtab_by_tab      <- subtab_by_tab
  .TC_CTX$dl_option_prefixes <- dl_option_prefixes
  .TC_CTX$internal_pattern   <- tc_or(internal_pattern, TC_INTERNAL_INPUT_PATTERN)
  invisible(TRUE)
}

tc_ctx_dashboard_title <- function() tc_or(.TC_CTX$dashboard_title, "")

tc_ctx_active_tab <- function() {
  inp <- .TC_CTX$input; nav <- .TC_CTX$nav_id
  if (is.null(inp) || is.null(nav)) return("")
  val <- tryCatch(shiny::isolate(inp[[nav]]), error = function(e) NULL)
  tc_or(val, "")
}

tc_ctx_active_subtab <- function() {
  inp <- .TC_CTX$input; map <- .TC_CTX$subtab_by_tab
  if (is.null(inp) || is.null(map)) return("")
  tab <- tc_ctx_active_tab()
  if (!nzchar(tab) || !tab %in% names(map)) return("")
  sub_id <- map[[tab]]
  val <- tryCatch(shiny::isolate(inp[[sub_id]]), error = function(e) NULL)
  tc_or(val, "")
}

#' Snapshot of the user's current figure-defining option selections.
#'
#' When `module_id` maps to a registered option prefix, only that chart's own
#' inputs are returned; otherwise all non-plumbing inputs are (fallback).
#' @param module_id Optional download module id used to scope the options.
#' @return Named list (sorted).
tc_ctx_selections <- function(module_id = NULL) {
  inp <- .TC_CTX$input
  if (is.null(inp)) return(list())
  all <- tryCatch(shiny::isolate(shiny::reactiveValuesToList(inp)),
                  error = function(e) list())
  if (length(all) == 0) return(list())

  prefix <- NULL
  if (!is.null(module_id) && !is.null(.TC_CTX$dl_option_prefixes) &&
      module_id %in% names(.TC_CTX$dl_option_prefixes)) {
    prefix <- .TC_CTX$dl_option_prefixes[[module_id]]
  }

  if (!is.null(prefix) && nzchar(prefix)) {
    # Scoped: only inputs belonging to this chart's sidebar.
    keep <- names(all)[grepl(prefix, names(all), perl = TRUE)]
  } else {
    # Fallback: everything except UI plumbing and the nav/subtab selectors.
    nav_ids <- c(.TC_CTX$nav_id, unname(.TC_CTX$subtab_by_tab))
    keep <- names(all)[!grepl(.TC_CTX$internal_pattern, names(all))]
    keep <- setdiff(keep, nav_ids)
  }

  # keep only scalar/short atomic option values (drop data frames, long blobs)
  keep <- keep[vapply(keep, function(k) {
    v <- all[[k]]
    is.null(v) || (is.atomic(v) && length(v) <= 50)
  }, logical(1))]
  all[sort(keep)]
}

# ---------------------------------------------------------------------------
# Chart type  ->  template mapping (the "internal function that determines the
# type of plot displayed and picks the right template").
#
# Only chart types with a genuinely matching template are listed. Anything not
# here (stacked_bar, waterfall, scatter, boxplot, ...) has NO suitable template
# and the caller must tell the user so.
# ---------------------------------------------------------------------------
TC_TEMPLATE_BY_CHART_TYPE <- c(
  line            = "template_line.pptx",
  bar             = "template_v_bar.pptx",         # single series, all bars one colour
  v_bar           = "template_v_bar.pptx",
  h_bar           = "template_h_bar.pptx",
  grouped_bar     = "template_v_bar_group.pptx",   # bars coloured per group
  v_bar_group     = "template_v_bar_group.pptx",
  stacked_bar     = "template_v_bar_stacked.pptx", # absolute stacked columns
  v_bar_stacked   = "template_v_bar_stacked.pptx",
  stacked_bar_100 = "template_v_bar_stacked_100.pptx", # 100% stacked columns
  v_bar_stacked_100 = "template_v_bar_stacked_100.pptx",
  pie             = "template_pie.pptx"
)

#' Does a chart type have a matching think-cell template?
#'
#' Type-level check (mapping membership only). Use [tc_template_available()]
#' when you also need the template file to actually exist on disk.
#' @param chart_type Character chart type (legacy aliases are normalised).
#' @return Logical scalar.
tc_chart_type_has_template <- function(chart_type) {
  ct <- normalize_tc_chart_type(chart_type)
  ct %in% names(TC_TEMPLATE_BY_CHART_TYPE)
}

#' Is a usable template file available for this chart type?
#'
#' Stricter than [tc_chart_type_has_template()]: the mapped `.pptx` must also
#' exist on disk. Used to gate the download button so it only appears when a
#' slide can really be produced.
#' @param chart_type Character chart type.
#' @param templates_dir Optional templates directory override.
#' @return Logical scalar.
tc_template_available <- function(chart_type, templates_dir = NULL) {
  !is.na(tc_template_for_chart_type(chart_type, templates_dir))
}

#' Locate the templates directory relative to the app root / working dir.
#' @param start Optional extra root to search first.
#' @return Normalised directory path, or NA_character_ if not found.
tc_find_templates_dir <- function(start = NULL) {
  roots <- c(start,
             if (exists("APP_ROOT")) get("APP_ROOT") else NULL,
             getwd())
  roots <- unique(Filter(function(r) !is.null(r) && !is.na(r) && nzchar(r), roots))
  candidates <- character(0)
  for (r in roots) {
    candidates <- c(
      candidates,
      file.path(r, "templates"),
      file.path(dirname(r), "templates"),
      file.path(r, "..", "templates")
    )
  }
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) return(NA_character_)
  normalizePath(candidates[[1]], winslash = "/", mustWork = FALSE)
}

#' Root under which the app keeps runtime state it must be able to *write*
#' (favorites, uploaded templates). Anchored to `APP_ROOT` when the app defines
#' it, else the working directory — the same base `favorites.json` is written
#' to. Deliberately NOT the deploy-owned `templates/` dir, which on a server is
#' typically read-only for the Shiny process.
tc_runtime_state_root <- function() {
  root <- if (exists("APP_ROOT")) get("APP_ROOT") else NULL
  if (is.null(root) || (length(root) == 1 && is.na(root)) || !nzchar(root)) root <- getwd()
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

#' Runtime-writable directory for templates uploaded through the app (rather
#' than committed to git).
#'
#' In production (no explicit `templates_dir`) this lives beside the app's other
#' runtime state, at `state/template_uploads/` — a location the Shiny process
#' can always write (the same reason `state/favorites.json` works), even when
#' the deploy-owned `templates/` dir is read-only for the app user. Like
#' `state/`, it is never synced by the deploy, so uploads survive a redeploy.
#'
#' Tests (and bespoke deployments) may pass `templates_dir`, in which case the
#' historical `templates/custom/` layout under that dir is used instead; the
#' `SHINY_TEMPLATE_UPLOADS_DIR` environment variable overrides everything.
#' @param templates_dir Optional base templates directory override.
#' @return Path (may not yet exist).
tc_custom_templates_dir <- function(templates_dir = NULL) {
  if (!is.null(templates_dir) && !is.na(templates_dir) && nzchar(templates_dir)) {
    return(file.path(templates_dir, "custom"))
  }
  override <- Sys.getenv("SHINY_TEMPLATE_UPLOADS_DIR", "")
  if (nzchar(override)) return(override)
  file.path(tc_runtime_state_root(), "state", "template_uploads")
}

#' Resolve a template (filename or full path) to an existing file path.
#'
#' A bare filename is looked up in the runtime uploads dir
#' ([tc_custom_templates_dir()]) first, then the built-in `templates/`, so an
#' uploaded override takes precedence over a built-in template of the same name.
#' @return Normalised path, or NA_character_ if it cannot be found.
tc_resolve_template_path <- function(template, templates_dir = NULL) {
  if (is.null(template) || is.na(template) || !nzchar(template)) return(NA_character_)
  if (file.exists(template)) {
    return(normalizePath(template, winslash = "/", mustWork = FALSE))
  }
  custom_dir <- tc_custom_templates_dir(templates_dir)
  if (!is.null(custom_dir) && !is.na(custom_dir) && nzchar(custom_dir)) {
    custom_candidate <- file.path(custom_dir, template)
    if (file.exists(custom_candidate)) {
      return(normalizePath(custom_candidate, winslash = "/", mustWork = FALSE))
    }
  }
  dir <- tc_or(templates_dir, tc_find_templates_dir())
  if (is.null(dir) || is.na(dir)) return(NA_character_)
  candidate <- file.path(dir, template)
  if (file.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

#' Template that visually matches the displayed chart.
#' @param chart_type Character chart type (legacy aliases normalised).
#' @param templates_dir Optional templates directory override.
#' @param override Optional explicit template filename/path to use instead.
#' @return Existing template path, or NA_character_ when none is suitable.
tc_template_for_chart_type <- function(chart_type, templates_dir = NULL, override = NULL) {
  if (!is.null(override) && !is.na(override) && nzchar(override)) {
    return(tc_resolve_template_path(override, templates_dir))
  }
  ct <- normalize_tc_chart_type(chart_type)
  if (!ct %in% names(TC_TEMPLATE_BY_CHART_TYPE)) return(NA_character_)
  fname <- unname(TC_TEMPLATE_BY_CHART_TYPE[[ct]])
  tc_resolve_template_path(fname, templates_dir)
}

# ---------------------------------------------------------------------------
# .ppttc JSON builder (same wire format as R/thinkcell_shiny_app.R, hardened
# for NA / non-numeric cells so it never emits invalid JSON).
# ---------------------------------------------------------------------------
tc_json_escape <- function(s) {
  s <- as.character(s)
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub('"', '\\\\"', s)
  s <- gsub("\r", "\\\\r", s)
  s <- gsub("\n", "\\\\n", s)
  s <- gsub("\t", "\\\\t", s)
  s
}

tc_cell_label <- function(x) sprintf('{"string":"%s"}', tc_json_escape(x))

tc_cell_value <- function(x) {
  if (length(x) == 0 || is.na(x)) return("null")
  num <- suppressWarnings(as.numeric(x))
  if (is.na(num)) return(tc_cell_label(x))  # non-numeric -> string cell
  sprintf('{"number":%s}', formatC(num, format = "f", drop0trailing = TRUE))
}

tc_json_row <- function(cells) sprintf("[%s]", paste(cells, collapse = ","))

#' Transpose a think-cell matrix (swap rows <-> columns).
#'
#' Input/output keep the think-cell convention: first column holds row labels
#' and the first header cell is empty (""). Used to put slide data into the
#' orientation the templates expect (categories across the header, series down
#' the first column) regardless of how [format_tc_data()] laid it out.
tc_transpose_matrix <- function(m) {
  m <- as.data.frame(m, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(m) < 2 || nrow(m) < 1) return(m)
  row_labels    <- as.character(m[[1]])   # become the new header
  series_labels <- names(m)[-1]           # become the new first column
  vals <- t(as.matrix(m[, -1, drop = FALSE]))
  out <- data.frame(series_labels, vals, check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- c("", row_labels)
  rownames(out) <- NULL
  out
}

#' Put a think-cell matrix into the orientation the slide templates expect.
#'
#' The reference templates (and R/thinkcell_shiny_app.R) read categories from
#' the header row and series from the first column. [format_tc_data()] already
#' does this for line charts but transposes bar/stacked/grouped charts, so we
#' transpose those back for the slide. (The exported table keeps its own layout.)
tc_slide_orientation <- function(m, chart_type) {
  if (normalize_tc_chart_type(chart_type) %in% tc_chart_types_transposed()) {
    return(tc_transpose_matrix(m))
  }
  m
}

#' Chart types that render as vertical/horizontal bars (may be single- or
#' multi-series). Used to decide when a chart collapses to a plain bar.
TC_BAR_FAMILY <- c(
  "bar", "v_bar", "h_bar",
  "grouped_bar", "v_bar_group",
  "stacked_bar", "v_bar_stacked",
  "stacked_bar_100", "v_bar_stacked_100"
)

#' List the template `.pptx` files available to the app.
#'
#' Merges the built-in `templates/` set with any uploaded overrides in the
#' runtime uploads dir ([tc_custom_templates_dir()]), deduplicated by file name;
#' `tc_resolve_template_path()` prefers the uploaded copy when both exist.
#' @return Sorted character vector of file names (may be empty).
tc_list_templates <- function(templates_dir = NULL) {
  dir <- tc_or(templates_dir, tc_find_templates_dir())
  base_files <- if (!is.null(dir) && !is.na(dir) && dir.exists(dir)) {
    list.files(dir, pattern = "\\.pptx$", ignore.case = TRUE)
  } else {
    character(0)
  }
  custom_dir <- tc_custom_templates_dir(templates_dir)
  custom_files <- if (!is.null(custom_dir) && !is.na(custom_dir) && dir.exists(custom_dir)) {
    list.files(custom_dir, pattern = "\\.pptx$", ignore.case = TRUE)
  } else {
    character(0)
  }
  sort(unique(c(base_files, custom_files)))
}

#' Named choices for a template-override selectInput.
#' First entry is the automatic (detected) option with value "".
tc_template_choices <- function(templates_dir = NULL) {
  files  <- tc_list_templates(templates_dir)
  labels <- c("Automatisch (gedetecteerd)", files)
  values <- c("", files)
  stats::setNames(values, labels)
}

#' Same choices as [tc_template_choices()], but each carrying its preview
#' image (if any) too -- for a `selectizeInput` picker that shows a
#' thumbnail per option (see `chart_data_downloads_ui()` in
#' `utils/chart_downloads.R`), rather than plain text a PM has to pick
#' blind. A template with no curated/uploaded preview just has `preview =
#' NA` -- the picker falls back to a blank swatch for it, never an error.
#' @return List of `list(value, label, preview)`.
tc_template_choice_items <- function(templates_dir = NULL) {
  choices <- tc_template_choices(templates_dir)
  labels  <- names(choices)
  values  <- unname(choices)
  lapply(seq_along(values), function(i) {
    preview <- if (nzchar(values[[i]])) tc_preview_data_uri(values[[i]], templates_dir) else NA_character_
    list(value = values[[i]], label = labels[[i]], preview = if (is.na(preview)) "" else preview)
  })
}

# ---------------------------------------------------------------------------
# Template preview thumbnails (Manage Templates tab).
#
# A preview is a plain screenshot PNG, curated by hand (or uploaded through
# the app) rather than auto-generated -- there is no server-side pptx
# rendering pipeline. It's purely cosmetic: a missing preview never blocks a
# template from being used, only from showing a thumbnail.
# ---------------------------------------------------------------------------

#' Preview file name for a template: same stem, `.png` extension.
#' @param template_name A `.pptx` file name (e.g. from [tc_list_templates()]).
tc_preview_filename <- function(template_name) {
  paste0(tools::file_path_sans_ext(basename(template_name)), ".png")
}

#' Directory for built-in template previews, curated alongside `templates/`.
#' @param templates_dir Optional base templates directory override.
tc_builtin_previews_dir <- function(templates_dir = NULL) {
  dir <- tc_or(templates_dir, tc_find_templates_dir())
  if (is.null(dir) || is.na(dir)) return(NA_character_)
  file.path(dir, "previews")
}

#' Directory for previews of runtime-uploaded templates. Lives alongside the
#' uploads themselves ([tc_custom_templates_dir()]) so it follows the same
#' `SHINY_TEMPLATE_UPLOADS_DIR` override and survives a redeploy the same way.
#' @param templates_dir Optional base templates directory override.
tc_custom_previews_dir <- function(templates_dir = NULL) {
  custom_dir <- tc_custom_templates_dir(templates_dir)
  if (is.null(custom_dir) || is.na(custom_dir)) return(NA_character_)
  file.path(custom_dir, "previews")
}

#' Resolve a template's preview PNG, if one exists.
#'
#' Checks the custom (uploaded) previews dir first, then the built-in one --
#' same precedence as [tc_resolve_template_path()] -- so a preview added
#' through the app for a built-in template's name still overrides nothing
#' that matters (there's no built-in preview to lose) and just fills the gap.
#' @param template_name A `.pptx` file name.
#' @param templates_dir Optional base templates directory override.
#' @return Existing PNG path, or `NA_character_` if none is available.
tc_resolve_preview_path <- function(template_name, templates_dir = NULL) {
  if (is.null(template_name) || is.na(template_name) || !nzchar(template_name)) {
    return(NA_character_)
  }
  preview_name <- tc_preview_filename(template_name)

  custom_dir <- tc_custom_previews_dir(templates_dir)
  if (!is.null(custom_dir) && !is.na(custom_dir)) {
    custom_candidate <- file.path(custom_dir, preview_name)
    if (file.exists(custom_candidate)) {
      return(normalizePath(custom_candidate, winslash = "/", mustWork = FALSE))
    }
  }

  builtin_dir <- tc_builtin_previews_dir(templates_dir)
  if (!is.null(builtin_dir) && !is.na(builtin_dir)) {
    builtin_candidate <- file.path(builtin_dir, preview_name)
    if (file.exists(builtin_candidate)) {
      return(normalizePath(builtin_candidate, winslash = "/", mustWork = FALSE))
    }
  }

  NA_character_
}

#' A template's preview as an inline `data:image/png;base64,...` URI, ready to
#' drop straight into an `<img src=...>` -- avoids needing a `www/`-style
#' static resource route for files that live under `templates/` or `state/`.
#' @param template_name A `.pptx` file name.
#' @param templates_dir Optional base templates directory override.
#' @return Data URI string, or `NA_character_` if no preview is available.
tc_preview_data_uri <- function(template_name, templates_dir = NULL) {
  path <- tc_resolve_preview_path(template_name, templates_dir)
  if (is.na(path)) return(NA_character_)
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size), error = function(e) NULL)
  if (is.null(bytes) || length(bytes) == 0) return(NA_character_)
  paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
}

#' Detect the template chart type for the *displayed* figure from its data.
#'
#' Same rule used by [tc_prepare_slide()], exposed separately so the UI can show
#' the chosen template reactively without building the whole matrix.
tc_detect_slide_type <- function(df, chart_type, category_col, series_col) {
  ct <- normalize_tc_chart_type(chart_type)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c(category_col, series_col) %in% names(df))) return(ct)
  n_cat <- dplyr::n_distinct(df[[category_col]])
  n_ser <- dplyr::n_distinct(df[[series_col]])
  n_series_eff <- if (n_cat <= 1 && n_ser > 1) n_cat else n_ser
  if (ct %in% TC_BAR_FAMILY && n_series_eff <= 1) "bar" else ct
}

#' Parse a vector to numbers, or return NULL if any element isn't numeric.
#' Handles both "." and "," decimal separators.
tc_numeric_or_na <- function(x) {
  x <- as.character(x)
  n <- suppressWarnings(as.numeric(x))
  if (!anyNA(n)) return(n)
  n2 <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  if (!anyNA(n2)) return(n2)
  NULL
}

#' Order the category (column) axis of a slide matrix.
#'
#' The slide matrix keeps categories in the header row and series in the first
#' column, so ordering only ever reorders the category columns - there is no
#' ambiguity about which axis is affected.
#'
#' Modes:
#'   * "auto"     - numeric-ascending when *every* category label is a number
#'                  (matches how the dashboard renders numeric axes); otherwise
#'                  left as displayed. Safe default.
#'   * "as_is"    - keep the order exactly as provided.
#'   * "cat_asc"  / "cat_desc" - sort by category label (numeric-aware).
#'   * "val_asc"  / "val_desc" - sort by the category's total across all series.
#' Coerce one think-cell matrix cell to numeric for total/ordering purposes,
#' stripping a waterfall marker prefix first (`format_tc_waterfall()` encodes
#' subtotal/end cells as `"t|123"`/`"e|456"`, not plain numbers) -- without
#' this, those cells silently coerce to `NA` and drop out of the total. A
#' no-op for any other matrix, since the regex simply doesn't match a plain
#' numeric string.
tc_numeric_cell_value <- function(x) {
  suppressWarnings(as.numeric(sub("^[a-z]\\|", "", as.character(x))))
}

tc_order_slide_matrix <- function(m, mode = "auto") {
  m <- as.data.frame(m, stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(mode) || !nzchar(mode) || identical(mode, "as_is")) return(m)
  if (ncol(m) < 3) return(m)  # 0 or 1 category: nothing to reorder

  cats <- names(m)[-1]
  cat_num <- tc_numeric_or_na(cats)

  col_totals <- function() {
    vapply(cats, function(cn) {
      sum(tc_numeric_cell_value(m[[cn]]), na.rm = TRUE)
    }, numeric(1))
  }

  ord <- switch(
    mode,
    auto     = if (!is.null(cat_num)) order(cat_num) else seq_along(cats),
    cat_asc  = if (!is.null(cat_num)) order(cat_num) else order(cats),
    cat_desc = rev(if (!is.null(cat_num)) order(cat_num) else order(cats)),
    val_asc  = order(col_totals()),
    val_desc = rev(order(col_totals())),
    seq_along(cats)
  )

  m[, c(1, 1 + ord), drop = FALSE]
}

#' Reorder a think-cell matrix's *category* axis -- its rows or its columns,
#' depending on chart type (see [tc_chart_types_transposed()]) -- to match a
#' given left-to-right order, so the plain reference `_table.xlsx` a PM
#' opens reads in the same category order as the slide's own chart, without
#' the caller needing to know or care which axis a given chart type's own
#' table puts categories on (`slide_matrix`, in contrast, always keeps
#' categories as columns, by construction -- see [tc_slide_orientation()]).
#' A faceted workbook (named list of data frames, one per facet) is
#' reordered facet-by-facet; a facet whose categories don't cleanly match
#' `ordered_categories` 1:1 is left untouched rather than guessed at.
#' @param m A think-cell matrix, or a named list of them (faceted).
#' @param ordered_categories Character vector: the desired left-to-right
#'   category order (typically `names(slide_matrix)[-1]` after
#'   [tc_order_slide_matrix()] has already been applied to that matrix).
#' @return `m`, reordered where possible; unchanged otherwise.
tc_reorder_by_categories <- function(m, ordered_categories) {
  if (is.null(ordered_categories) || length(ordered_categories) == 0) return(m)

  reorder_one <- function(mat) {
    mat <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(mat) < 2) return(mat)

    cols <- names(mat)[-1]
    if (length(ordered_categories) == length(cols) && all(ordered_categories %in% cols)) {
      return(mat[, c(1, 1 + match(ordered_categories, cols)), drop = FALSE])
    }

    row_labels <- as.character(mat[[1]])
    if (length(ordered_categories) == length(row_labels) && all(ordered_categories %in% row_labels)) {
      return(mat[match(ordered_categories, row_labels), , drop = FALSE])
    }

    mat
  }

  if (is_tc_workbook_list(m)) lapply(m, reorder_one) else reorder_one(m)
}

#' Resolve the ordered slide/table category axis for a chart: the caller's
#' pre-oriented `slide_matrix` when supplied (already reflecting the
#' displayed plot type), otherwise one derived from `tc_data`'s own
#' orientation for `chart_type` (see [tc_slide_orientation()]) -- either way,
#' with `slide_order` applied via [tc_order_slide_matrix()]. Shared by
#' [tc_build_slide_zip()] (for both its pptx chart and companion table.xlsx)
#' and any lighter-weight path that only needs the table (e.g. an
#' Excel-only regenerate), so both agree on category order without
#' duplicating the orientation+ordering pipeline. Non-faceted only -- a
#' faceted `tc_data` is a named list of matrices, not one matrix; callers
#' with a faceted chart pass a single facet's matrix in, same as
#' [tc_build_slide_zip()]'s own faceted handling.
#' @return The ordered slide matrix (data frame).
tc_resolve_slide_matrix <- function(tc_data, chart_type, slide_matrix, slide_order) {
  m <- if (!is.null(slide_matrix)) {
    as.data.frame(slide_matrix, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    tc_slide_orientation(tc_data, chart_type)
  }
  tc_order_slide_matrix(m, slide_order)
}

#' Windows-safe path for embedding in the .ppttc `template` field.
#'
#' think-cell's `ppttc` can fail to load templates whose path contains spaces or
#' parentheses (e.g. "Downloads (2)"). On Windows we hand it the short 8.3 path,
#' which has neither; elsewhere we just normalise. Returns a forward-slashed path.
tc_short_path <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(path)
  p <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    short <- tryCatch(utils::shortPathName(p), error = function(e) p)
    if (length(short) == 1 && nzchar(short)) p <- short
  }
  gsub("\\\\", "/", p)
}

#' Determine the template chart type and slide matrix that match the *displayed*
#' figure, based on the data behind it.
#'
#' The dashboard sometimes declares a grouped/stacked bar even when the current
#' selection collapses the figure to a single series (one category, or one
#' series). think-cell should then show a plain vertical bar. This inspects the
#' data and:
#'   * puts the dimension that actually varies on the x-axis (categories),
#'   * downgrades grouped/stacked bars to a simple `bar` when only one series
#'     remains,
#'   * returns the matrix already in the templates' expected orientation
#'     (categories across the header, series down the first column).
#'
#' @return list(chart_type = <template chart type>, matrix = <data frame>).
tc_prepare_slide <- function(df, chart_type, category_col, series_col, value_col,
                             agg_fun = NULL, category_order = NULL, series_order = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  n_cat <- dplyr::n_distinct(df[[category_col]])
  n_ser <- dplyr::n_distinct(df[[series_col]])

  cat_col <- category_col
  ser_col <- series_col
  ord_cat <- category_order
  ord_ser <- series_order
  # One category but several series -> put the series on the x-axis so the
  # figure reads as a simple bar chart rather than a cluster at one position.
  if (n_cat <= 1 && n_ser > 1) {
    cat_col <- series_col
    ser_col <- category_col
    ord_cat <- series_order
    ord_ser <- category_order
  }

  # "line" layout == reference orientation (series rows, categories columns).
  m <- format_tc_data(
    df, chart_type = "line",
    category_col = cat_col, series_col = ser_col, value_col = value_col,
    agg_fun = agg_fun, category_order = ord_cat, series_order = ord_ser
  )

  slide_type <- tc_detect_slide_type(df, chart_type, category_col, series_col)

  list(chart_type = slide_type, matrix = m)
}

#' Build a single think-cell slide `{template,data}` JSON object (no array
#' wrapper). Exposed separately from [tc_build_ppttc_json()] so multiple
#' slides can be concatenated into one `.ppttc` array for a multi-slide deck
#' (see `favorites_build_deck_zip()` in `utils/favorites.R`), since one
#' `ppttc.exe` call over an array of these blocks renders one deck.
#'
#' @param df think-cell matrix: column 1 = row labels, remaining columns are
#'   categories. This is exactly what [format_tc_data()] returns.
#' @param template Template path written into the JSON (forward-slashed).
#' @param slide_title Optional slide title (bound to SlideTitle). Skipped if "".
#' @param figure_title Optional figure caption (bound to FigureTitle). Skipped if "".
#' @param chart_id Optional download id (bound to a `DownloadID` automation
#'   field). Lets a template designer bind a small, discreet text box to
#'   `DownloadID` so a PM can find this exact export again later in the
#'   **Export history** tab (`utils/export_history.R`) straight from a real
#'   slide, months after the fact -- no export-history-specific work is
#'   required here beyond adding one more named data block, harmless (ignored
#'   by think-cell) for any template that hasn't bound the field yet.
#' @param favorite_download_id Optional id shared by every chart in the same
#'   "Download all favorites" click (bound to a `FavoriteDownloadID`
#'   automation field), so a bulk download can be found and regenerated as
#'   one unit later. `NULL`/empty for a solo "Download slide" click.
#' @param datasheet_log Optional selection-log string written into the
#'   *chart's own datasheet*, corner cell (row 1, column 1) -- see
#'   [tc_build_datasheet_log()]. That cell precedes the category labels, so
#'   think-cell never renders it or reads it as data; it just rides along
#'   inside the chart element itself (not a linked Excel range), so it
#'   survives copy-pasting the chart to a new slide or deck. Purely
#'   provenance, not tamper-proof -- anyone opening the datasheet can see or
#'   edit it.
#' @return A single JSON object string (not wrapped in `[...]`).
tc_build_ppttc_slide_block <- function(df, template, slide_title = "", figure_title = "",
                                        chart_id = NULL, datasheet_log = NULL,
                                        favorite_download_id = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(df) < 2) {
    stop("think-cell matrix needs at least a label column and one value column.", call. = FALSE)
  }
  cats <- names(df)[-1]
  corner <- if (nzchar(trimws(tc_or(datasheet_log, "")))) tc_cell_label(datasheet_log) else "null"
  header <- tc_json_row(c(corner, vapply(cats, tc_cell_label, character(1))))
  series <- vapply(seq_len(nrow(df)), function(i) {
    label  <- tc_cell_label(df[[1]][i])
    values <- vapply(df[i, -1, drop = FALSE], tc_cell_value, character(1))
    tc_json_row(c(label, values))
  }, character(1))

  chart_table <- sprintf("[%s]", paste(c(header, series), collapse = ","))

  blocks <- character(0)
  if (nzchar(trimws(tc_or(slide_title, "")))) {
    blocks <- c(blocks, sprintf('{"name":"SlideTitle","table":[[%s]]}',
                                tc_cell_label(slide_title)))
  }
  blocks <- c(blocks, sprintf('{"name":"Chart1","table":%s}', chart_table))
  if (nzchar(trimws(tc_or(figure_title, "")))) {
    blocks <- c(blocks, sprintf('{"name":"FigureTitle","table":[[%s]]}',
                                tc_cell_label(figure_title)))
  }
  if (nzchar(trimws(tc_or(chart_id, "")))) {
    blocks <- c(blocks, sprintf('{"name":"DownloadID","table":[[%s]]}',
                                tc_cell_label(chart_id)))
  }
  if (nzchar(trimws(tc_or(favorite_download_id, "")))) {
    blocks <- c(blocks, sprintf('{"name":"FavoriteDownloadID","table":[[%s]]}',
                                tc_cell_label(favorite_download_id)))
  }

  data_block <- paste(blocks, collapse = ",")
  sprintf('{"template":"%s","data":[%s]}',
          gsub("\\\\", "/", template), data_block)
}

#' Build the think-cell `.ppttc` JSON for one slide.
#' @inheritParams tc_build_ppttc_slide_block
#' @return A single JSON string (a one-element array).
tc_build_ppttc_json <- function(df, template, slide_title = "", figure_title = "",
                                chart_id = NULL, datasheet_log = NULL, favorite_download_id = NULL) {
  sprintf("[%s]", tc_build_ppttc_slide_block(df, template, slide_title, figure_title, chart_id, datasheet_log, favorite_download_id))
}

# ---------------------------------------------------------------------------
# think-cell executable discovery + rendering (mirrors the reference app).
# ---------------------------------------------------------------------------
#' Find the think-cell `ppttc` executable, or NA_character_ if unavailable.
tc_find_ppttc_exe <- function() {
  candidates <- c(
    getOption("tc.ppttc_exe", default = NA_character_),
    Sys.getenv("TC_PPTTC_EXE", unset = NA_character_),
    "C:/Program Files (x86)/think-cell/ppttc.exe",
    "C:/Program Files/think-cell/ppttc.exe"
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) return(normalizePath(hit[[1]], winslash = "/", mustWork = FALSE))
  on_path <- unname(Sys.which(c("ppttc.exe", "ppttc")))
  on_path <- on_path[nzchar(on_path)]
  if (length(on_path) > 0) return(on_path[[1]])
  NA_character_
}

#' Render a .pptx from ppttc JSON via the think-cell executable.
#' @return list(ok, status, log).
tc_render_pptx_ppttc <- function(json, out_pptx, exe) {
  ppttc_path <- tempfile(fileext = ".ppttc")
  writeLines(json, ppttc_path, useBytes = TRUE)
  on.exit(unlink(ppttc_path), add = TRUE)
  out <- suppressWarnings(system2(
    exe,
    args   = c(shQuote(ppttc_path), "-o", shQuote(out_pptx)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(
    ok     = (status == 0L && file.exists(out_pptx)),
    status = status,
    log    = paste(out, collapse = "\n")
  )
}

# ---------------------------------------------------------------------------
# Datasheet corner-cell log -- the one provenance record every export carries
# (there is no separate log.txt file: the corner cell/xlsx header is it).
# ---------------------------------------------------------------------------
#' Derive `name = value` pairs from a selections list -- shared by every
#' place that needs to render a selections snapshot (currently just
#' [tc_build_datasheet_log()], and [favorites_selections_inline()] in
#' `utils/favorites.R` for its own, differently-formatted display).
#' @return A list of `list(name, value)`, or `list()` if there's nothing.
tc_selection_kv_pairs <- function(selections) {
  if (is.null(selections) || length(selections) == 0) return(list())
  nm <- names(selections)
  if (is.null(nm)) nm <- paste0("option_", seq_along(selections))
  lapply(seq_along(selections), function(i) {
    v <- selections[[i]]
    v <- if (is.null(v) || length(v) == 0) "" else paste(as.character(v), collapse = ", ")
    list(name = nm[[i]], value = v)
  })
}

#' Build the single-line selection log embedded in the chart datasheet's
#' corner cell (see [tc_build_ppttc_slide_block()]), and, via
#' [tc_stamp_tc_matrix_corner()], the corner header of a plain think-cell
#' `.xlsx` export. This is the *only* provenance record an export carries --
#' there is no separate log.txt. Always stamps its own generation timestamp,
#' so a chart found later in a deck can be dated without cross-referencing
#' anything else.
#' @param chart_id Optional download id, included as its own `download_id=`
#'   field when supplied. Omitted for exports that aren't logged to Export
#'   History (e.g. the plain "Download data (think-cell)" button).
#' @param favorite_download_id Optional id shared by every chart in the same
#'   "Download all favorites" click, included as its own
#'   `favorite_download_id=` field when supplied.
#' @param source_output,source_sheet Optional identifiers naming the
#'   underlying data source this chart's numbers came from -- e.g. a
#'   pipeline output id ("3a") and, within it, a sheet name -- for a
#'   dashboard whose data is assembled from named external outputs. Included
#'   as `output=`/`sheet=` fields when supplied; omit both for dashboards
#'   with no such concept (the default, and this template's own scaffold).
#' @param source_mtime Optional last-modified date of `source_output`'s
#'   underlying file, already formatted (see [tc_format_source_mtime()]) --
#'   included as its own `source_updated=` field, right after `sheet=`, when
#'   supplied. Distinct from `timestamp=` (when this *export* was made): this
#'   is when the *source data itself* was last edited.
#' @return A single-line character string, e.g.
#'   `"LOG | timestamp=...; download_id=...; output=...; sheet=...; source_updated=...; dashboard=...; tab=...; sub-tab=...; chart_type=...; opt=val"`.
#' @param dictionary_crosswalk Optional named character vector -- names are
#'   raw values, values their dictionary-relabeled pretty counterparts --
#'   appended as its own `dictionary=raw1->pretty1|raw2->pretty2` segment
#'   when non-empty. Built by [chart_data_downloads_server()]
#'   (`utils/chart_downloads.R`) from whichever distinct category/series
#'   values a real dictionary hit actually changed for that specific
#'   download -- a value with no dictionary entry (or whose "Format from
#'   dictionary" checkbox is off) never contributes a pair, keeping this
#'   compact rather than listing every value in the chart.
#' @param dictionary_format Optional logical -- whether the "Format from
#'   dictionary" checkbox was on for this download. When `TRUE`, an explicit
#'   `dictionary_format=on` segment is emitted, so "on but nothing was
#'   actually relabeled" (empty `dictionary_crosswalk`) is still
#'   distinguishable from "off". `NULL`/`FALSE` emits nothing.
tc_build_datasheet_log <- function(dashboard_title, tab_label, subtab_label,
                                   chart_type, selections, chart_id = NULL,
                                   favorite_download_id = NULL,
                                   source_output = NULL, source_sheet = NULL,
                                   source_mtime = NULL, dictionary_crosswalk = NULL,
                                   dictionary_format = NULL) {
  parts <- c(sprintf("timestamp=%s", tc_now()))
  if (!is.null(chart_id) && nzchar(trimws(tc_or(chart_id, "")))) {
    parts <- c(parts, sprintf("download_id=%s", chart_id))
  }
  if (!is.null(favorite_download_id) && nzchar(trimws(tc_or(favorite_download_id, "")))) {
    parts <- c(parts, sprintf("favorite_download_id=%s", favorite_download_id))
  }
  if (!is.null(source_output) && nzchar(trimws(tc_or(source_output, "")))) {
    parts <- c(parts, sprintf("output=%s", source_output))
  }
  if (!is.null(source_sheet) && nzchar(trimws(tc_or(source_sheet, "")))) {
    parts <- c(parts, sprintf("sheet=%s", source_sheet))
  }
  if (!is.null(source_mtime) && nzchar(trimws(tc_or(source_mtime, "")))) {
    parts <- c(parts, sprintf("source_updated=%s", source_mtime))
  }
  parts <- c(
    parts,
    sprintf("dashboard=%s", tc_or(dashboard_title, "")),
    sprintf("tab=%s", tc_or(tab_label, "")),
    sprintf("sub-tab=%s", tc_or(subtab_label, "")),
    sprintf("chart_type=%s", tc_or(chart_type, ""))
  )
  pairs <- tc_selection_kv_pairs(selections)
  if (length(pairs) > 0) {
    parts <- c(parts, vapply(pairs, function(p) sprintf("%s=%s", p$name, p$value), character(1)))
  }
  if (isTRUE(dictionary_format)) {
    parts <- c(parts, "dictionary_format=on")
  }
  if (!is.null(dictionary_crosswalk) && length(dictionary_crosswalk) > 0) {
    crosswalk <- paste(
      sprintf("%s->%s", names(dictionary_crosswalk), unname(dictionary_crosswalk)),
      collapse = "|"
    )
    parts <- c(parts, sprintf("dictionary=%s", crosswalk))
  }
  paste0("LOG | ", paste(parts, collapse = "; "))
}

# ---------------------------------------------------------------------------
# Orchestrator: build the download ZIP.
# ---------------------------------------------------------------------------
#' Build the think-cell slide download ZIP at `zip_path`.
#'
#' @param zip_path Output .zip path (the `file` handed in by downloadHandler).
#' @param tc_data think-cell matrix from [format_tc_data()] (a data frame, or a
#'   named list of data frames for faceted charts).
#' @param raw_data Optional raw (pre-think-cell-reshape) data frame -- the
#'   same data the chart's own "Download data (raw)" button writes. When
#'   supplied, written into the ZIP as `<prefix>_raw.xlsx` alongside the
#'   think-cell `<prefix>_table.xlsx`. `NULL` (the default) omits it --
#'   e.g. for callers replaying an older entry that never captured one.
#' @param chart_type Resolved chart type of the displayed figure.
#' @param slide_title,figure_title Optional titles bound in the template.
#' @param dashboard_title,tab_label,subtab_label Log metadata.
#' @param selections Named list of the user's option selections (for the log).
#' @param filename_prefix Prefix for the table file name.
#' @param templates_dir,template_override,ppttc_exe Optional overrides.
#' @param write_table_fun Function(data, path) writing the underlying table.
#' @param chart_id Optional download id (see [tc_build_ppttc_slide_block()]);
#'   passed straight through, so callers not using `utils/export_history.R`
#'   can just omit it (default `NULL`, identical to today's behavior).
#' @param favorite_download_id Optional id shared by every chart in the same
#'   "Download all favorites" click (see [tc_build_ppttc_slide_block()] and
#'   [tc_build_datasheet_log()]) -- `NULL` for a solo "Download slide" click.
#' @param source_output,source_sheet,source_mtime Optional data-source
#'   identifiers (see [tc_build_datasheet_log()]) -- passed straight through
#'   to the chart datasheet's corner-cell log, nowhere else.
#' @param asset_path,asset_label Optional path to a captured PNG snapshot of
#'   this chart (see `TC_CHART_CAPTURE_JS`/`export_history_asset_path()` in
#'   `utils/chart_downloads.R`/`utils/export_history.R`) and its display
#'   label. When `asset_path` exists on disk, a `charts_overview.html` (see
#'   [tc_build_charts_overview_html()] in `utils/favorites.R`) is written
#'   into the ZIP alongside the table/slide -- the same self-contained
#'   viewer a bulk favorites download already gets, now for a solo export
#'   too. Silently omitted (not an error) when `asset_path` is `NULL` or the
#'   file doesn't exist -- e.g. no `plot_output_id` was wired, the client
#'   capture failed/timed out, or this chart has no Plotly widget at all.
#' @return list(zip_path, rendered, template, note) invisibly.
tc_build_slide_zip <- function(zip_path,
                               tc_data,
                               chart_type,
                               raw_data         = NULL,
                               slide_title      = "",
                               figure_title     = "",
                               dashboard_title  = "",
                               tab_label        = "",
                               subtab_label     = "",
                               selections       = NULL,
                               filename_prefix  = "chart",
                               templates_dir    = NULL,
                               template_override = NULL,
                               ppttc_exe        = NULL,
                               slide_matrix     = NULL,
                               slide_order      = "auto",
                               write_table_fun  = write_tc_xlsx,
                               chart_id         = NULL,
                               favorite_download_id = NULL,
                               source_output    = NULL,
                               source_sheet     = NULL,
                               source_mtime     = NULL,
                               dictionary_crosswalk = NULL,
                               dictionary_format = NULL,
                               asset_path       = NULL,
                               asset_label      = NULL) {

  resolved_type <- normalize_tc_chart_type(chart_type)
  template_path <- tc_template_for_chart_type(resolved_type, templates_dir, template_override)
  has_template  <- !is.na(template_path)

  work <- tempfile("tc_slide_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # think-cell renders a single matrix. The caller may pass a pre-oriented
  # `slide_matrix` (already reflecting the displayed plot type); otherwise derive
  # it from the table data and orient it for the templates.
  is_faceted <- is_tc_workbook_list(tc_data)
  slide_matrix <- tc_resolve_slide_matrix(
    if (is_faceted) tc_data[[1]] else tc_data, chart_type, slide_matrix, slide_order
  )
  note <- if (is_faceted) {
    sprintf("data has %d facets; slide shows first facet '%s', table contains all facets",
            length(tc_data), names(tc_data)[[1]])
  } else NULL

  # The one provenance record this export carries -- see
  # tc_build_ppttc_slide_block() for where it's written into the slide's own
  # datasheet (there is no log.txt). Computed before (2) below so the loose
  # _table.xlsx companion can carry the same corner-cell stamp, not just the
  # rendered slide.
  datasheet_log <- tc_build_datasheet_log(
    dashboard_title = dashboard_title,
    tab_label       = tab_label,
    subtab_label    = subtab_label,
    chart_type      = chart_type,
    selections      = selections,
    chart_id        = chart_id,
    favorite_download_id = favorite_download_id,
    source_output   = source_output,
    source_sheet    = source_sheet,
    source_mtime    = source_mtime,
    dictionary_crosswalk = dictionary_crosswalk,
    dictionary_format = dictionary_format
  )

  # ---- (2) underlying table -- the SAME matrix embedded in the slide's own
  # think-cell chart (categories across the header), so a PM can open this
  # workbook and paste it straight into the chart datasheet without
  # re-pivoting. For a non-faceted chart that's exactly `slide_matrix`; a
  # faceted chart keeps all its facets (the slide embeds only the first),
  # reordered to the first facet's category order. ----
  table_matrix <- if (is_faceted) {
    tc_reorder_by_categories(tc_data, names(slide_matrix)[-1])
  } else {
    slide_matrix
  }
  table_path <- file.path(work, paste0(filename_prefix, "_table.xlsx"))
  write_table_fun(
    tc_stamp_tc_matrix_corner(table_matrix, datasheet_log),
    table_path
  )

  # ---- (2b) raw data (the exact data behind the plot, before think-cell
  # reshaping -- same as the chart's own "Download data (raw)" button) ----
  # optional: an older history entry replayed via export_history_redownload()
  # may not have one captured. Not stamped: raw_data's column 1 is a real
  # data column, not the blank placeholder think-cell leaves.
  if (!is.null(raw_data)) {
    raw_path <- file.path(work, paste0(filename_prefix, "_raw.xlsx"))
    write_table_fun(raw_data, raw_path)
  }

  rendered <- FALSE

  if (!has_template) {
    # No suitable template: still give the user the data + a clear explanation.
    note <- paste(c(
      sprintf("No think-cell template matches chart type '%s'; slide was not created.", chart_type),
      note
    ), collapse = " | ")
    writeLines(paste0(
      "No suitable think-cell template is available for the chart currently ",
      "displayed in the dashboard (chart type: ", chart_type, ").\n\n",
      "The underlying data table is still included so it can be built manually.\n",
      "Templates exist for: ", paste(sort(unique(names(TC_TEMPLATE_BY_CHART_TYPE))), collapse = ", "), ".\n"
    ), file.path(work, "NO_TEMPLATE.txt"), useBytes = TRUE)
  } else {
    # ---- (3) slide ----------------------------------------------------------
    if (!nzchar(trimws(tc_or(slide_title, "")))) slide_title <- tc_or(subtab_label, "")
    json <- tc_build_ppttc_json(slide_matrix, tc_short_path(template_path), slide_title, figure_title, chart_id, datasheet_log, favorite_download_id)
    exe  <- tc_or(ppttc_exe, tc_find_ppttc_exe())

    if (!is.null(exe) && !is.na(exe) && nzchar(exe)) {
      out_pptx <- file.path(work, "slide.pptx")
      res <- tc_render_pptx_ppttc(json, out_pptx, exe)
      if (isTRUE(res$ok)) {
        rendered <- TRUE
      } else {
        note <- paste(c(note, paste("ppttc render failed:", res$log)), collapse = " | ")
      }
    } else {
      note <- paste(c(note,
        paste0("think-cell (ppttc) not found on this machine, so the slide was ",
               "not rendered. Set options(tc.ppttc_exe = \"<path to ppttc.exe>\") ",
               "or the TC_PPTTC_EXE environment variable, then download again.")),
        collapse = " | ")
    }

    if (!rendered) {
      # Graceful, never-corrupt fallback: ship the valid template + ppttc data.
      # The shipped .ppttc must reference the template by the bare file name it's
      # copied under here, NOT the absolute path resolved above -- that path is
      # only valid on *this* machine (typically the Linux server, which is why
      # rendering fell back in the first place). A PM opening this bundle on
      # their own PC has no such path; think-cell needs "slide_template.pptx"
      # sitting right next to chart_data.ppttc, not a server path it can't reach.
      file.copy(template_path, file.path(work, "slide_template.pptx"), overwrite = TRUE)
      portable_json <- tc_build_ppttc_json(slide_matrix, "slide_template.pptx", slide_title, figure_title, chart_id, datasheet_log, favorite_download_id)
      writeLines(portable_json, file.path(work, "chart_data.ppttc"), useBytes = TRUE)
      writeLines(paste0(
        "think-cell was not available to render the slide automatically on this machine.\n",
        "To finish the slide on a PC with PowerPoint + think-cell:\n\n",
        "  Option A (command line):\n",
        "    ppttc chart_data.ppttc -o slide.pptx\n\n",
        "  Option B (in PowerPoint):\n",
        "    1. Open slide_template.pptx.\n",
        "    2. think-cell ribbon > update the chart from chart_data.ppttc.\n\n",
        "The underlying data (", basename(table_path), ") matches this chart exactly.\n"
      ), file.path(work, "README_render_slide.txt"), useBytes = TRUE)
    }
  }

  # One page with the captured chart image, if any -- same viewer a bulk
  # favorites download already gets (tc_build_charts_overview_html()), now
  # for a solo export too. A no-op when asset_path is NULL/doesn't exist.
  overview_html <- tc_build_charts_overview_html(list(list(
    label = tc_or(asset_label, tc_or(figure_title, tc_or(slide_title, filename_prefix))),
    asset_path = asset_path
  )))
  if (!is.na(overview_html)) {
    writeLines(overview_html, file.path(work, "charts_overview.html"), useBytes = TRUE)
  }

  # ---- zip (flat) -----------------------------------------------------------
  # No log.txt: the datasheet_log embedded above (and the underlying table)
  # is this export's one provenance record.
  files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = files, flags = "-q -X")

  invisible(list(
    zip_path = zip_path_abs,
    rendered = rendered,
    template = if (has_template) template_path else NA_character_,
    note     = note
  ))
}
