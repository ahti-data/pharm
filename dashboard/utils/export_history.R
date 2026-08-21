#' Export history: an automatic, durable log of every "Download slide" click.
#'
#' Distinct from `utils/favorites.R` (a manually curated, shared shortlist):
#' every slide download is logged here automatically, with a frozen snapshot
#' of exactly what was exported, so a PM can find and redownload a chart from
#' a long time ago -- byte-for-byte the same ZIP -- without needing to have
#' kept the original around. Each entry gets a short download id that also
#' gets embedded in the exported `.ppttc`/datasheet (see
#' `tc_build_ppttc_slide_block()` in `utils/slide_download.R`), so a chart
#' spotted in a real PowerPoint deck can be traced back here.
#'
#' Three actions, all driven by the checkboxes + bottom banner across an
#' arbitrary multi-chart selection (including picking out specific charts
#' within a bulk download rather than only the whole group, since every
#' member has its own checkbox alongside the group's own "select every
#' member" one) -- there's no per-row button, only this one action surface:
#'   * "Redownload" -- always an exact-snapshot replay, instant, no lookup
#'     needed. Selecting several rows combines them into one deck instead of
#'     one zip per click.
#'   * "Regenerate" -- rebuilds against *today's* live dashboard data when a
#'     chart's module is still registered in this session (see
#'     `tc_chart_registry_get()` in `utils/slide_download.R`), falling back
#'     to the last-known snapshot otherwise. Every regenerate mints
#'     brand-new ids and logs a fresh entry -- it never overwrites or reuses
#'     the one it started from. Selecting several rows mints one shared
#'     `favorite_download_id` across the whole regenerated batch, same as a
#'     bulk "Download all favorites" click.
#'   * "Regenerate Excel only" -- same live-or-fallback logic as "Regenerate",
#'     but skips the slide/pptx entirely and writes just the stamped
#'     think-cell table (see `export_history_regenerate_excel_many()`). One
#'     row writes a bare `.xlsx`; several zip one per chart.

#' One JSON file per entry (`state/export_history/<id>.json`) rather than one
#' growing array, so appending never rewrites the whole log -- same reasoning
#' as `state/template_uploads/`. Override with `SHINY_EXPORT_HISTORY_DIR`.
export_history_dir <- function() {
  Sys.getenv("SHINY_EXPORT_HISTORY_DIR", file.path("state", "export_history"))
}

export_history_new_id <- function() {
  tc_new_id("exp_")
}

#' Directory holding every export's captured PNG snapshot -- sibling to
#' `export_history_dir()`, same "state/ is runtime-only, never deployed"
#' treatment as everything else under `state/`.
export_history_assets_dir <- function() {
  file.path(dirname(export_history_dir()), "export_history_assets")
}

#' Path an export's PNG snapshot would live at, whether or not it exists yet.
#' Used to embed a `charts_overview.html` (see [tc_build_charts_overview_html()]
#' in `utils/favorites.R`) into every export's ZIP -- including a live
#' favorites bulk download's, which mints its own fresh history entry (and
#' thus its own asset path) per chart, same as any other export.
#' @param id A history entry's own id.
export_history_asset_path <- function(id) {
  file.path(export_history_assets_dir(), paste0(id, ".png"))
}

#' Decode a `data:image/png;base64,...` URI (as sent by `TC_CHART_CAPTURE_JS`)
#' and write it to `path`. A no-op -- not an error -- when `image` is
#' `NULL`/empty or fails to decode, since a missing snapshot only means that
#' chart's overview page is absent from the ZIP, never a broken export.
#' @param image The data-URI string, or `NULL`.
#' @param path Destination `.png` path (see [export_history_asset_path()]).
tc_write_captured_asset <- function(image, path) {
  if (is.null(image) || !nzchar(image)) return(invisible(FALSE))
  b64   <- sub("^data:image/[^;]+;base64,", "", image)
  bytes <- tryCatch(jsonlite::base64_dec(b64), error = function(e) NULL)
  if (is.null(bytes) || length(bytes) == 0) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(bytes, path)
  invisible(TRUE)
}

#' The inverse of [tc_write_captured_asset()]: read an already-stored PNG
#' back out as a `data:image/png;base64,...` URI, so it can be handed to
#' [tc_write_captured_asset()] again under a *new* id -- used when a
#' regenerate has no fresh capture to work with (module not registered this
#' session, or the client-side capture round found nothing) but an older
#' snapshot exists, so the regenerated entry's ZIP still gets an overview
#' page rather than none at all.
#' @param path A `.png` path (e.g. from [export_history_asset_path()]).
#' @return The data-URI string, or `NULL` if `path` doesn't exist/is unreadable.
tc_read_asset_as_data_uri <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  bytes <- tryCatch(readBin(path, "raw", n = file.info(path)$size), error = function(e) NULL)
  if (is.null(bytes) || length(bytes) == 0) return(NULL)
  paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
}

#' Save a new history entry.
#'
#' @param entry List, typically the output of [tc_history_capture()]. If
#'   `entry$id` is already set (so the same id can be embedded in the export
#'   itself -- see `utils/chart_downloads.R`), it's kept as-is; otherwise one
#'   is generated. Same for `entry$created_at` -- kept as-is when already
#'   set, so a batch of entries from one bulk download/regenerate can share
#'   one exact timestamp instead of each independently stamping "now".
#' @return The entry's id (invisibly).
export_history_add <- function(entry) {
  if (is.null(entry$id) || !nzchar(entry$id)) {
    entry$id <- export_history_new_id()
  }
  if (is.null(entry$created_at) || !nzchar(entry$created_at)) {
    entry$created_at <- tc_now()
  }
  dir <- export_history_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    entry, file.path(dir, paste0(entry$id, ".json")),
    auto_unbox = TRUE, null = "null", na = "null"
  )
  invisible(entry$id)
}

#' Read a single history entry by id.
#' @return The entry (a list), or `NULL` if missing/corrupt.
export_history_get <- function(id) {
  path <- file.path(export_history_dir(), paste0(id, ".json"))
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

#' List every history entry, most recently created first.
#'
#' Reads every `*.json` file in [export_history_dir()]; unreadable/corrupt
#' files are skipped rather than failing the whole list (the same tolerance
#' [favorites_list()] applies to its single file).
#' @return List of entries (possibly empty).
export_history_list <- function() {
  dir <- export_history_dir()
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0) return(list())
  entries <- lapply(files, function(f) {
    tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  })
  entries <- Filter(Negate(is.null), entries)
  created <- vapply(entries, function(e) tc_or(e$created_at, ""), character(1))
  entries[order(created, decreasing = TRUE)]
}

#' Remove a history entry by id.
export_history_remove <- function(id) {
  path <- file.path(export_history_dir(), paste0(id, ".json"))
  if (file.exists(path)) unlink(path)
  invisible(TRUE)
}

#' Package an already-resolved slide export into a storable history entry.
#'
#' Takes the *same resolved values* the "Download slide" handler in
#' `utils/chart_downloads.R` already computed for its own [tc_build_slide_zip()]
#' call -- not a second, independent re-derivation -- so a history entry
#' always matches exactly what was actually downloaded. Faceted charts
#' (`tc_data` is a per-facet named list, not one matrix) are the caller's
#' responsibility to skip; this function assumes a single matrix.
#'
#' @param tc_data The resolved think-cell matrix (from [format_tc_data()]) --
#'   what the zip's own `<prefix>_table.xlsx` is built from.
#' @param chart_type The *resolved* slide chart type (e.g. from
#'   [tc_prepare_slide()]), not necessarily the chart's declared type.
#' @param slide_matrix Optional pre-oriented matrix for the slide/`.ppttc`
#'   itself, when it differs from `tc_data` (see [tc_build_slide_zip()]).
#' @param raw_data Optional raw (pre-think-cell-reshape) data frame -- the
#'   same data the chart's own "Download data (raw)" button writes. Stored
#'   so a later redownload/regenerate can also ship a `<prefix>_raw.xlsx`
#'   alongside the think-cell one (see [tc_build_slide_zip()]). `NULL` when
#'   unavailable (e.g. legacy entries, or the caller never had it).
#' @param slide_title,figure_title Resolved slide text.
#' @param template_override The resolved template choice (manual or
#'   auto-detected name/path) actually used, so a later redownload reproduces
#'   the same template rather than re-running auto-detection against
#'   whatever exists at that time.
#' @param slide_order Resolved category order mode.
#' @param dashboard_title,tab_label,subtab_label,selections Export log metadata.
#' @param source_output,source_sheet,source_mtime Optional data-source
#'   identifiers (see `tc_build_datasheet_log()` in `utils/slide_download.R`),
#'   stored on the entry so a redownload stamps the same values into the
#'   datasheet corner cell as the original export did.
#' @param favorite_download_id Optional id shared by every chart from the
#'   same "Download all favorites" click or bulk regenerate -- `NULL` for a
#'   solo download.
#' @param module_id The chart's `chart_data_downloads_server(id = ...)`, so a
#'   later regenerate can look this chart back up in the session's live
#'   chart registry (see `tc_chart_registry_get()` in
#'   `utils/slide_download.R`) instead of only ever replaying this snapshot.
#' @param filename_prefix Prefix used for this chart's downloads.
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @return A list ready for [export_history_add()].
tc_history_capture <- function(
    tc_data, chart_type, slide_matrix = NULL, raw_data = NULL,
    slide_title = "", figure_title = "", template_override = "", slide_order = "auto",
    dashboard_title = "", tab_label = "", subtab_label = "",
    selections = NULL, source_output = NULL, source_sheet = NULL, source_mtime = NULL,
    favorite_download_id = NULL, module_id = NULL,
    filename_prefix = "chart", templates_dir = NULL,
    dictionary_format = NULL, dictionary_crosswalk = NULL
) {
  override <- if (nzchar(tc_or(template_override, ""))) template_override else NULL
  template_path <- tc_template_for_chart_type(chart_type, templates_dir = templates_dir, override = override)

  resolved_label <- Find(
    function(x) !is.null(x) && nzchar(x),
    list(figure_title, slide_title, subtab_label, filename_prefix)
  )

  list(
    label             = tc_or(resolved_label, "chart"),
    filename_prefix   = filename_prefix,
    dashboard_title   = dashboard_title,
    tab_label         = tab_label,
    subtab_label      = subtab_label,
    chart_type        = chart_type,
    template_name     = if (!is.na(template_path)) basename(template_path) else NA_character_,
    template_override = tc_or(template_override, ""),
    selections        = selections,
    source_output     = source_output,
    source_sheet      = source_sheet,
    source_mtime      = source_mtime,
    favorite_download_id = favorite_download_id,
    module_id         = module_id,
    slide_order       = slide_order,
    slide_title       = slide_title,
    figure_title      = figure_title,
    dictionary_format = isTRUE(dictionary_format),
    # Stored so a later exact-snapshot redownload reproduces the same
    # corner-cell crosswalk (a live regenerate recomputes it fresh instead).
    dictionary_crosswalk = if (length(dictionary_crosswalk) > 0) as.list(dictionary_crosswalk) else NULL,
    tc_data_table     = favorites_table_to_storage(tc_data),
    slide_matrix_table = if (!is.null(slide_matrix)) favorites_table_to_storage(slide_matrix) else NULL,
    raw_data_table    = if (!is.null(raw_data)) favorites_table_to_storage(raw_data) else NULL
  )
}

#' Rebuild the exact original ZIP for a history entry.
#' @param entry A history entry (as returned by [export_history_get()]).
#' @param zip_path Output `.zip` path (the `file` handed in by downloadHandler).
#' @param templates_dir,ppttc_exe Optional overrides (mainly for tests).
export_history_redownload <- function(entry, zip_path, templates_dir = NULL, ppttc_exe = NULL) {
  tc_build_slide_zip(
    zip_path          = zip_path,
    tc_data           = favorites_table_as_df(entry$tc_data_table),
    chart_type        = entry$chart_type,
    slide_matrix      = if (!is.null(entry$slide_matrix_table)) {
      favorites_table_as_df(entry$slide_matrix_table)
    } else {
      NULL
    },
    raw_data          = if (!is.null(entry$raw_data_table)) {
      favorites_table_as_df(entry$raw_data_table)
    } else {
      NULL
    },
    slide_title       = tc_or(entry$slide_title, ""),
    figure_title      = tc_or(entry$figure_title, ""),
    dashboard_title   = tc_or(entry$dashboard_title, ""),
    tab_label         = tc_or(entry$tab_label, ""),
    subtab_label      = tc_or(entry$subtab_label, ""),
    selections        = entry$selections,
    source_output     = tc_or(entry$source_output, ""),
    source_sheet      = tc_or(entry$source_sheet, ""),
    source_mtime      = tc_or(entry$source_mtime, ""),
    filename_prefix   = tc_or(entry$filename_prefix, "chart"),
    templates_dir     = templates_dir,
    template_override = tc_or(entry$template_override, ""),
    ppttc_exe         = ppttc_exe,
    slide_order       = tc_or(entry$slide_order, "auto"),
    chart_id          = entry$id,
    favorite_download_id = entry$favorite_download_id,
    dictionary_format = isTRUE(entry$dictionary_format),
    dictionary_crosswalk = if (length(entry$dictionary_crosswalk) > 0) unlist(entry$dictionary_crosswalk) else NULL,
    asset_path        = export_history_asset_path(entry$id),
    asset_label       = tc_or(entry$label, "chart")
  )
}

# ---------------------------------------------------------------------------
# Regenerate: rebuild a chart (or a whole bulk-download group) against
# *today's* live dashboard data, via the session's chart registry
# (utils/slide_download.R), falling back to the last-known snapshot when a
# chart's module isn't registered in the current session. Every regenerate
# mints brand-new ids and logs a fresh entry -- it never touches the id that
# was pasted in.
# ---------------------------------------------------------------------------

#' The option set a regenerate should rebuild this entry against: the entry's
#' own stored `selections`, so "regenerate" means *today's data, this entry's
#' figure* -- not today's data with whatever options the chart's inputs happen
#' to show in the regenerating session. `NULL` (meaning "use live inputs") only
#' for an entry saved without any, e.g. logged before this was recorded.
#'
#' A chart that can't honor a replay (no `data_for()` -- see
#' `chart_data_downloads_server()`) falls back to live inputs and reports the
#' live selections, so a regenerated entry never claims options it didn't use.
#' @param entry A history entry (as returned by [export_history_list()]).
#' @return Named list of option values, or `NULL`.
export_history_replay_selections <- function(entry) {
  if (length(entry$selections) == 0) return(NULL)
  entry$selections
}

#' Copy one entry's stored PNG snapshot to a new entry's asset path, if it
#' exists. Used when a regenerate mints a fresh id but has no *new* capture
#' to use for it (module not live this session, or the capture round found
#' nothing) -- the regenerated entry's ZIP still gets an overview page,
#' carried over from the entry it was regenerated from, rather than none.
tc_copy_asset <- function(from_id, to_id) {
  from <- export_history_asset_path(from_id)
  if (!file.exists(from)) return(invisible(FALSE))
  to <- export_history_asset_path(to_id)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, to, overwrite = TRUE)
}

#' Regenerate one history entry as a standalone ZIP (used for a solo
#' regenerate) -- live, via the session's chart registry, when possible;
#' otherwise an exact-snapshot rebuild, same as [export_history_redownload()]
#' but under a brand-new id (a regenerate never reuses the pasted id).
#' @param captured_image Optional data-URI from this session's bulk-capture
#'   round (see `TC_CHART_CAPTURE_JS`'s `.tc-regenerate-go-btn` handler and
#'   `export_history_regenerate_many()`) for this entry's own module, if one
#'   was found/captured -- `NULL` when no fresh capture is available, in
#'   which case the entry's last stored snapshot is reused instead (still
#'   better than no image at all).
#' @return `list(live = TRUE/FALSE)`, invisibly.
export_history_regenerate_entry <- function(entry, zip_path, session,
                                            templates_dir = NULL, ppttc_exe = NULL,
                                            captured_image = NULL) {
  reg <- tc_chart_registry_get(session, tc_or(entry$module_id, ""))
  if (!is.null(reg)) {
    image_to_use <- if (!is.null(captured_image)) {
      captured_image
    } else {
      tc_read_asset_as_data_uri(export_history_asset_path(entry$id))
    }
    # Regenerate = today's data, but THIS entry's own stored options -- not
    # whatever the chart's inputs currently show (see
    # favorites_live_spec_or_null() for the same reasoning).
    sel <- export_history_replay_selections(entry)
    ok <- tryCatch({
      if (is.null(sel) || !tc_accepts_selections(reg$build_zip)) {
        reg$build_zip(zip_path, captured_image = image_to_use)
      } else {
        reg$build_zip(zip_path, captured_image = image_to_use, selections = sel)
      }
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(invisible(list(live = TRUE)))
  }

  new_entry <- entry
  new_entry$id <- export_history_new_id()
  new_entry$favorite_download_id <- NULL
  new_entry$created_at <- NULL
  export_history_add(new_entry)
  tc_copy_asset(entry$id, new_entry$id)
  export_history_redownload(new_entry, zip_path, templates_dir = templates_dir, ppttc_exe = ppttc_exe)
  invisible(list(live = FALSE))
}

#' Prepare one history entry as a [tc_build_deck_from_specs()] spec (used for
#' a bulk regenerate, where every member folds into one combined deck) --
#' live, via the session's chart registry, when possible; otherwise from the
#' entry's own frozen snapshot. Either way, mints a fresh `download_id`,
#' tags it with `favorite_download_id`/`created_at` (the bulk regenerate's
#' shared batch values), and logs a brand-new history entry.
#' @param captured_image Optional data-URI from this session's bulk-capture
#'   round for this entry's own module (see `export_history_regenerate_many()`);
#'   `NULL` falls back to a copy of the entry's last stored snapshot.
#' @return `list(live = TRUE/FALSE, spec = list(...))`.
export_history_prepare_regenerate_spec <- function(entry, session, favorite_download_id = NULL,
                                                    created_at = NULL, templates_dir = NULL,
                                                    captured_image = NULL) {
  reg <- tc_chart_registry_get(session, tc_or(entry$module_id, ""))
  live_spec <- NULL
  if (!is.null(reg)) {
    live_spec <- tryCatch(tc_registry_spec(reg, export_history_replay_selections(entry)),
                          error = function(e) NULL)
    if (!is.null(live_spec) && isTRUE(live_spec$is_faceted)) live_spec <- NULL
  }

  if (!is.null(live_spec)) {
    history_entry <- tc_history_capture(
      tc_data           = live_spec$tc_data,
      chart_type        = live_spec$chart_type,
      slide_matrix      = live_spec$slide_matrix,
      raw_data          = live_spec$raw_data,
      slide_title       = live_spec$slide_title,
      figure_title      = live_spec$figure_title,
      template_override = live_spec$template_override,
      slide_order       = live_spec$slide_order,
      dashboard_title   = live_spec$dashboard_title,
      tab_label         = live_spec$tab_label,
      subtab_label      = live_spec$subtab_label,
      selections        = live_spec$selections,
      source_output     = live_spec$source_output,
      source_sheet      = live_spec$source_sheet,
      source_mtime      = live_spec$source_mtime,
      favorite_download_id = favorite_download_id,
      module_id         = tc_or(entry$module_id, ""),
      filename_prefix   = live_spec$filename_prefix,
      templates_dir     = templates_dir,
      dictionary_format = live_spec$dictionary_format,
      dictionary_crosswalk = live_spec$dictionary_crosswalk
    )
    history_entry$id <- export_history_new_id()
    if (!is.null(created_at)) history_entry$created_at <- created_at
    download_id <- export_history_add(history_entry)

    tpl_path <- tc_template_for_chart_type(
      live_spec$chart_type, templates_dir = templates_dir,
      override = tc_or(live_spec$template_override, "")
    )
    datasheet_log <- tc_build_datasheet_log(
      dashboard_title = live_spec$dashboard_title, tab_label = live_spec$tab_label,
      subtab_label = live_spec$subtab_label, chart_type = live_spec$chart_type,
      selections = live_spec$selections, chart_id = download_id,
      favorite_download_id = favorite_download_id,
      source_output = live_spec$source_output, source_sheet = live_spec$source_sheet,
      source_mtime = live_spec$source_mtime,
      dictionary_format = live_spec$dictionary_format,
      dictionary_crosswalk = live_spec$dictionary_crosswalk
    )
    label <- tc_or(
      Find(function(x) !is.null(x) && nzchar(x),
           list(live_spec$figure_title, live_spec$slide_title, live_spec$subtab_label, live_spec$filename_prefix)),
      "chart"
    )
    slide_matrix <- tc_or(live_spec$slide_matrix, live_spec$tc_data)

    image_to_use <- if (!is.null(captured_image)) {
      captured_image
    } else {
      tc_read_asset_as_data_uri(export_history_asset_path(entry$id))
    }
    tc_write_captured_asset(image_to_use, export_history_asset_path(download_id))

    return(list(live = TRUE, spec = list(
      label = label,
      tc_table = as.data.frame(slide_matrix, stringsAsFactors = FALSE, check.names = FALSE),
      raw_table = if (!is.null(live_spec$raw_data)) {
        as.data.frame(live_spec$raw_data, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        NULL
      },
      chart_type = live_spec$chart_type,
      template_path = tpl_path,
      slide_title = live_spec$slide_title,
      figure_title = live_spec$figure_title,
      download_id = download_id,
      favorite_download_id = favorite_download_id,
      datasheet_log = datasheet_log,
      asset_path = export_history_asset_path(download_id)
    )))
  }

  # Fallback: this chart's module isn't registered in the current session
  # (e.g. the app restarted since it was last downloaded) -- rebuild from
  # its own frozen snapshot instead, still as a brand-new history entry.
  new_entry <- entry
  new_entry$id <- export_history_new_id()
  new_entry$favorite_download_id <- favorite_download_id
  new_entry$created_at <- created_at
  export_history_add(new_entry)
  tc_copy_asset(entry$id, new_entry$id)

  tpl_path <- tc_template_for_chart_type(
    new_entry$chart_type, templates_dir = templates_dir,
    override = tc_or(new_entry$template_override, "")
  )
  datasheet_log <- tc_build_datasheet_log(
    dashboard_title = tc_or(new_entry$dashboard_title, ""), tab_label = tc_or(new_entry$tab_label, ""),
    subtab_label = tc_or(new_entry$subtab_label, ""), chart_type = new_entry$chart_type,
    selections = new_entry$selections, chart_id = new_entry$id,
    favorite_download_id = favorite_download_id,
    source_output = tc_or(new_entry$source_output, ""), source_sheet = tc_or(new_entry$source_sheet, ""),
    source_mtime = tc_or(new_entry$source_mtime, ""),
    dictionary_format = isTRUE(new_entry$dictionary_format),
    dictionary_crosswalk = if (length(new_entry$dictionary_crosswalk) > 0) unlist(new_entry$dictionary_crosswalk) else NULL
  )
  slide_matrix <- if (!is.null(new_entry$slide_matrix_table)) {
    favorites_table_as_df(new_entry$slide_matrix_table)
  } else {
    favorites_table_as_df(new_entry$tc_data_table)
  }

  list(live = FALSE, spec = list(
    label = tc_or(new_entry$label, "chart"),
    tc_table = slide_matrix,
    raw_table = if (!is.null(new_entry$raw_data_table)) {
      favorites_table_as_df(new_entry$raw_data_table)
    } else {
      NULL
    },
    chart_type = new_entry$chart_type,
    template_path = tpl_path,
    slide_title = tc_or(new_entry$slide_title, ""),
    figure_title = tc_or(new_entry$figure_title, ""),
    download_id = new_entry$id,
    favorite_download_id = favorite_download_id,
    datasheet_log = datasheet_log,
    asset_path = export_history_asset_path(new_entry$id)
  ))
}

#' Build one [tc_build_deck_from_specs()]-shaped spec straight from a stored
#' history entry's frozen snapshot -- no session/registry involved, and no
#' new id minted (this is an exact redownload, not a regenerate: the spec
#' keeps the entry's own `id`/`favorite_download_id` as-is). This is the same
#' "pick slide_matrix_table over tc_data_table, rebuild the corner-cell log"
#' logic [export_history_prepare_regenerate_spec()]'s fallback branch already
#' does, extracted so both share it instead of a third near-duplicate.
#' @param entry A history entry (as returned by [export_history_get()]).
#' @param templates_dir Optional templates directory override (mainly for tests).
export_history_snapshot_spec <- function(entry, templates_dir = NULL) {
  tpl_path <- tc_template_for_chart_type(
    entry$chart_type, templates_dir = templates_dir,
    override = tc_or(entry$template_override, "")
  )
  datasheet_log <- tc_build_datasheet_log(
    dashboard_title = tc_or(entry$dashboard_title, ""), tab_label = tc_or(entry$tab_label, ""),
    subtab_label = tc_or(entry$subtab_label, ""), chart_type = entry$chart_type,
    selections = entry$selections, chart_id = entry$id,
    favorite_download_id = entry$favorite_download_id,
    source_output = tc_or(entry$source_output, ""), source_sheet = tc_or(entry$source_sheet, ""),
    source_mtime = tc_or(entry$source_mtime, ""),
    dictionary_format = isTRUE(entry$dictionary_format),
    dictionary_crosswalk = if (length(entry$dictionary_crosswalk) > 0) unlist(entry$dictionary_crosswalk) else NULL
  )
  slide_matrix <- if (!is.null(entry$slide_matrix_table)) {
    favorites_table_as_df(entry$slide_matrix_table)
  } else {
    favorites_table_as_df(entry$tc_data_table)
  }

  list(
    label             = tc_or(entry$label, "chart"),
    tc_table          = slide_matrix,
    raw_table         = if (!is.null(entry$raw_data_table)) {
      favorites_table_as_df(entry$raw_data_table)
    } else {
      NULL
    },
    chart_type        = entry$chart_type,
    template_path     = tpl_path,
    slide_title       = tc_or(entry$slide_title, ""),
    figure_title      = tc_or(entry$figure_title, ""),
    download_id       = entry$id,
    favorite_download_id = entry$favorite_download_id,
    datasheet_log     = datasheet_log,
    asset_path        = export_history_asset_path(entry$id)
  )
}

#' Exact-snapshot redownload of an arbitrary list of history entries (the
#' Export History tab's checkbox-driven "Redownload selected"). One entry
#' replays [export_history_redownload()] unchanged; two or more combine into
#' a single deck -- same shape "Download all favorites" produces -- via
#' [export_history_snapshot_spec()] + [tc_build_deck_from_specs()]. No new
#' history entries are logged; a redownload isn't a new export.
#' @param entries List of history entries.
#' @return `zip_path`, invisibly.
export_history_download_many <- function(entries, zip_path, ppttc_exe = NULL, templates_dir = NULL) {
  if (length(entries) == 1) {
    return(export_history_redownload(entries[[1]], zip_path, templates_dir = templates_dir, ppttc_exe = ppttc_exe))
  }
  specs <- lapply(entries, export_history_snapshot_spec, templates_dir = templates_dir)
  tc_build_deck_from_specs(specs, zip_path, ppttc_exe)
}

#' Regenerate an arbitrary list of history entries against today's live
#' dashboard data where possible (the Export History tab's checkbox-driven
#' "Regenerate selected"). One entry
#' delegates to [export_history_regenerate_entry()] unchanged, writing a
#' standalone ZIP; two or more mint one shared `favorite_download_id` + one
#' `batch_created_at` (same as a bulk "Download all favorites" click), call
#' [export_history_prepare_regenerate_spec()] per entry, then combine into
#' one deck via [tc_build_deck_from_specs()]. Every regenerate mints
#' brand-new ids -- it never touches the entries passed in.
#' @param entries List of history entries.
#' @param captures Named list of data-URIs from this session's bulk-capture
#'   round (see `TC_CHART_CAPTURE_JS`'s `.tc-regenerate-go-btn` handler),
#'   keyed by `module_id` -- an entry whose `module_id` isn't a name in this
#'   list simply has no fresh capture (falls back to its last stored
#'   snapshot, same as a chart that isn't live this session at all).
#' @return `list(live_count, total)`, invisibly.
export_history_regenerate_many <- function(entries, zip_path, session, templates_dir = NULL,
                                           ppttc_exe = NULL, captures = list()) {
  if (length(entries) == 1) {
    res <- export_history_regenerate_entry(
      entries[[1]], zip_path, session, templates_dir = templates_dir, ppttc_exe = ppttc_exe,
      captured_image = captures[[tc_or(entries[[1]]$module_id, "")]]
    )
    return(invisible(list(live_count = if (isTRUE(res$live)) 1 else 0, total = 1)))
  }

  new_favorite_download_id <- favorites_download_new_id()
  batch_created_at <- tc_now()
  prepared <- lapply(entries, function(e) {
    export_history_prepare_regenerate_spec(
      e, session, favorite_download_id = new_favorite_download_id,
      created_at = batch_created_at, templates_dir = templates_dir,
      captured_image = captures[[tc_or(e$module_id, "")]]
    )
  })
  specs <- lapply(prepared, function(p) p$spec)
  live_count <- sum(vapply(prepared, function(p) isTRUE(p$live), logical(1)))
  tc_build_deck_from_specs(specs, zip_path, ppttc_exe)
  invisible(list(live_count = live_count, total = length(specs)))
}

#' Regenerate just the think-cell Excel table for one history entry -- no
#' slide/pptx, no `tc_build_slide_zip()` -- live, via the session's chart
#' registry, when possible; otherwise from the entry's own frozen snapshot.
#' Same category ordering the slide/table export uses
#' ([tc_resolve_slide_matrix()] + [tc_reorder_by_categories()], both in
#' `utils/slide_download.R`), and the same corner-cell provenance stamp the
#' plain "Download data (think-cell)" button uses
#' ([tc_stamp_tc_matrix_corner()]) -- but unlike that button, this one *is*
#' logged to Export History (mints a fresh id), since every other regenerate
#' action is.
#' @param entry A history entry.
#' @param session The Shiny session (for the live chart registry lookup).
#' @param templates_dir Optional templates directory override (mainly for tests).
#' @return `list(live = TRUE/FALSE, data = <stamped think-cell matrix>,
#'   filename_prefix = ...)`.
export_history_regenerate_excel_one <- function(entry, session, templates_dir = NULL) {
  reg <- tc_chart_registry_get(session, tc_or(entry$module_id, ""))
  live_spec <- NULL
  if (!is.null(reg)) {
    live_spec <- tryCatch(tc_registry_spec(reg, export_history_replay_selections(entry)),
                          error = function(e) NULL)
    if (!is.null(live_spec) && isTRUE(live_spec$is_faceted)) live_spec <- NULL
  }

  if (!is.null(live_spec)) {
    history_entry <- tc_history_capture(
      tc_data           = live_spec$tc_data,
      chart_type        = live_spec$chart_type,
      slide_matrix      = live_spec$slide_matrix,
      raw_data          = live_spec$raw_data,
      slide_title       = live_spec$slide_title,
      figure_title      = live_spec$figure_title,
      template_override = live_spec$template_override,
      slide_order       = live_spec$slide_order,
      dashboard_title   = live_spec$dashboard_title,
      tab_label         = live_spec$tab_label,
      subtab_label      = live_spec$subtab_label,
      selections        = live_spec$selections,
      source_output     = live_spec$source_output,
      source_sheet      = live_spec$source_sheet,
      source_mtime      = live_spec$source_mtime,
      module_id         = tc_or(entry$module_id, ""),
      filename_prefix   = live_spec$filename_prefix,
      templates_dir     = templates_dir,
      dictionary_format = live_spec$dictionary_format,
      dictionary_crosswalk = live_spec$dictionary_crosswalk
    )
    history_entry$id <- export_history_new_id()
    download_id <- export_history_add(history_entry)

    # The exact matrix embedded in the slide chart (see the note in
    # chart_downloads.R's output$thinkcell) -- so the regenerated Excel has
    # the same orientation as every other think-cell table download.
    ordered_matrix <- tc_resolve_slide_matrix(
      live_spec$tc_data, live_spec$chart_type, live_spec$slide_matrix, live_spec$slide_order
    )

    log_line <- tc_build_datasheet_log(
      dashboard_title = live_spec$dashboard_title, tab_label = live_spec$tab_label,
      subtab_label = live_spec$subtab_label, chart_type = live_spec$chart_type,
      selections = live_spec$selections, chart_id = download_id,
      source_output = live_spec$source_output, source_sheet = live_spec$source_sheet,
      source_mtime = live_spec$source_mtime,
      dictionary_format = live_spec$dictionary_format,
      dictionary_crosswalk = live_spec$dictionary_crosswalk
    )
    return(list(
      live = TRUE,
      data = tc_stamp_tc_matrix_corner(ordered_matrix, log_line),
      filename_prefix = tc_or(live_spec$filename_prefix, "chart")
    ))
  }

  # Fallback: this chart's module isn't registered in the current session --
  # rebuild from its own frozen snapshot instead, still as a brand-new
  # history entry (every regenerate mints one, live or not).
  new_entry <- entry
  new_entry$id <- export_history_new_id()
  new_entry$favorite_download_id <- NULL
  new_entry$created_at <- NULL
  export_history_add(new_entry)

  tc_data <- favorites_table_as_df(new_entry$tc_data_table)
  slide_matrix <- if (!is.null(new_entry$slide_matrix_table)) {
    favorites_table_as_df(new_entry$slide_matrix_table)
  } else {
    NULL
  }
  ordered_matrix <- tc_resolve_slide_matrix(tc_data, new_entry$chart_type, slide_matrix, tc_or(new_entry$slide_order, "auto"))

  log_line <- tc_build_datasheet_log(
    dashboard_title = tc_or(new_entry$dashboard_title, ""), tab_label = tc_or(new_entry$tab_label, ""),
    subtab_label = tc_or(new_entry$subtab_label, ""), chart_type = new_entry$chart_type,
    selections = new_entry$selections, chart_id = new_entry$id,
    source_output = tc_or(new_entry$source_output, ""), source_sheet = tc_or(new_entry$source_sheet, ""),
    source_mtime = tc_or(new_entry$source_mtime, ""),
    dictionary_format = isTRUE(new_entry$dictionary_format),
    dictionary_crosswalk = if (length(new_entry$dictionary_crosswalk) > 0) unlist(new_entry$dictionary_crosswalk) else NULL
  )
  list(
    live = FALSE,
    data = tc_stamp_tc_matrix_corner(ordered_matrix, log_line),
    filename_prefix = tc_or(new_entry$filename_prefix, "chart")
  )
}

#' Regenerate just the think-cell Excel table for an arbitrary list of
#' history entries (the Export History tab's checkbox-driven "Regenerate
#' Excel only"). Always writes one bare `.xlsx` straight to `file` -- one
#' entry's data as the workbook's only sheet, or (for two or more) one sheet
#' per entry in a single combined workbook -- never a `.zip`, same combined-
#' workbook shape [tc_build_deck_from_specs()] already uses for a bulk
#' slide download's own tables (`utils/favorites.R`). Every entry mints a
#' fresh id and logs a new history entry, same as the other regenerate
#' actions.
#' @param entries List of history entries.
#' @param file Output `.xlsx` path (the `file` handed in by downloadHandler).
#' @param session The Shiny session (for the live chart registry lookup).
#' @return `list(live_count, total)`, invisibly.
export_history_regenerate_excel_many <- function(entries, file, session, templates_dir = NULL) {
  results <- lapply(entries, export_history_regenerate_excel_one, session = session, templates_dir = templates_dir)
  live_count <- sum(vapply(results, function(r) isTRUE(r$live), logical(1)))

  if (length(results) == 1) {
    write_tc_xlsx(results[[1]]$data, file)
    return(invisible(list(live_count = live_count, total = 1)))
  }

  labels <- sanitize_excel_sheet_names(
    vapply(results, function(r) tc_or(r$filename_prefix, "chart"), character(1))
  )
  sheets <- stats::setNames(lapply(results, function(r) r$data), labels)
  write_tc_xlsx(sheets, file)
  invisible(list(live_count = live_count, total = length(results)))
}

#' Compact breadcrumb + chart-type/template line for one history row.
tc_history_entry_subtitle <- function(e) {
  breadcrumb <- paste(
    Filter(nzchar, c(tc_or(e$dashboard_title, ""), tc_or(e$tab_label, ""), tc_or(e$subtab_label, ""))),
    collapse = " / "
  )
  breadcrumb
}

#' Every checkbox in this panel is a selection toggle with an empty label
#' (`checkboxInput(..., NULL, ...)`), but two Bootstrap/Shiny defaults still
#' reserve real horizontal space for a label that's never there: Bootstrap
#' 3's `.checkbox label` keeps 20px of padding-left for text that would
#' normally follow the checkbox, and Shiny's own `.shiny-input-container`
#' defaults every input's wrapper to `width: 300px` regardless of how small
#' the actual control is -- together these read as a large, unwanted left
#' indent before each row's real content. Scoped to `.tc-row-checkbox` (a
#' plain wrapper div around each `checkboxInput()` call, see
#' `entry_row_ui()`/`group_row_ui()`) rather than every
#' `.shiny-input-container` in the panel, so the search box up top -- which
#' *does* want its default width -- is untouched.
TC_EXPORT_HISTORY_CSS <- r"(
.tc-export-history .tc-row-checkbox .shiny-input-container { width: auto; min-width: 0; margin-bottom: 0; }
.tc-export-history .checkbox { margin: 0; }
.tc-export-history .checkbox label { padding-left: 0; min-height: 0; }
.tc-export-history .checkbox label input[type="checkbox"] { position: static; margin: 0; }
)"

#' UI for the shared "Export history" tab.
#' @param id Module id.
export_history_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "tc-export-history",
    shiny::h3("Export history"),
    shiny::p(
      class = "text-muted",
      "Every “Download slide” click is logged here automatically, so a chart from a ",
      "while back can be redownloaded exactly as it was — no need to keep the original ZIP ",
      "around. Find a chart's id on its rendered slide or in its datasheet's corner cell, ",
      "or search for it below. Check one or more rows and use the actions that appear ",
      "at the bottom of the screen."
    ),
    shiny::textInput(
      ns("search"), NULL,
      placeholder = "Search by download id, dashboard, tab, sub-tab, or chart type..."
    ),
    shiny::tags$hr(),
    shiny::uiOutput(ns("list")),
    shiny::uiOutput(ns("selection_banner")),
    shiny::tags$script(shiny::HTML(TC_CHART_CAPTURE_JS)),
    shiny::tags$style(shiny::HTML(TC_EXPORT_HISTORY_CSS))
  )
}

#' Group history entries sharing one `favorite_download_id` into a single
#' row (kind `"group"`); every other entry stays its own row (kind
#' `"solo"`). Sorted by recency, a group's timestamp being its most-recent
#' member's, so bulk and solo rows interleave correctly in time order.
#' @param entries List of history entries (e.g. from `filtered_entries()`).
#' @return List of `list(kind = "solo", entry, created_at)` or
#'   `list(kind = "group", favorite_download_id, members, created_at)`,
#'   most-recent first.
export_history_group_rows <- function(entries) {
  has_group <- vapply(entries, function(e) nzchar(tc_or(e$favorite_download_id, "")), logical(1))

  solo_rows <- lapply(entries[!has_group], function(e) {
    list(kind = "solo", entry = e, created_at = tc_or(e$created_at, ""))
  })

  bulk_ids <- unique(vapply(entries[has_group], function(e) e$favorite_download_id, character(1)))
  group_rows <- lapply(bulk_ids, function(gid) {
    members <- Filter(function(e) identical(tc_or(e$favorite_download_id, ""), gid), entries)
    created_ats <- vapply(members, function(e) tc_or(e$created_at, ""), character(1))
    list(
      kind = "group",
      favorite_download_id = gid,
      members = members,
      # "%Y-%m-%d %H:%M:%S" strings sort correctly lexicographically, so
      # plain max() gives the most recent one -- which.max() would silently
      # coerce to numeric (NA, with a warning) and error on the resulting
      # empty index.
      created_at = max(created_ats)
    )
  })

  rows <- c(solo_rows, group_rows)
  rows[order(vapply(rows, function(r) r$created_at, character(1)), decreasing = TRUE)]
}

#' Server logic for the shared "Export history" tab.
#'
#' Uses `reactivePoll()` over the history directory (max modification time +
#' file count) so the list picks up new entries logged from any chart's
#' module server, the same pattern `favorites_panel_server()` uses.
#' @param id Module id.
#' @param poll_interval_ms How often to check the history directory for changes.
#' @param display_limit Cap on entries shown when there's no search filter
#'   (most-recent-first); a search always shows every match, unfiltered.
export_history_panel_server <- function(id, poll_interval_ms = 2000, display_limit = 50) {
  shiny::moduleServer(id, function(input, output, session) {
    entries_reactive <- shiny::reactivePoll(
      poll_interval_ms, session,
      checkFunc = function() {
        dir <- export_history_dir()
        if (!dir.exists(dir)) return("0|")
        files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
        if (length(files) == 0) return("0|")
        paste(length(files), max(file.info(files)$mtime), sep = "|")
      },
      valueFunc = export_history_list
    )

    filtered_entries <- shiny::reactive({
      entries <- entries_reactive()
      query <- trimws(tc_or(input$search, ""))
      if (nzchar(query)) {
        keep <- vapply(entries, function(e) {
          haystack <- paste(
            tc_or(e$id, ""), tc_or(e$favorite_download_id, ""), tc_or(e$label, ""),
            tc_or(e$dashboard_title, ""), tc_or(e$tab_label, ""), tc_or(e$subtab_label, ""),
            tc_or(e$chart_type, ""),
            sep = " | "
          )
          grepl(query, haystack, ignore.case = TRUE, fixed = TRUE)
        }, logical(1))
        entries[keep]
      } else if (length(entries) > display_limit) {
        entries[seq_len(display_limit)]
      } else {
        entries
      }
    })

    # Groups entries sharing one favorite_download_id into a single
    # expand/collapse row; solo entries (no favorite_download_id) render as
    # before. See export_history_group_rows() for the (pure, unit-tested)
    # grouping/ordering itself.
    display_rows <- shiny::reactive(export_history_group_rows(filtered_entries()))

    # Selection state for the checkbox-driven bottom banner. Every individual
    # entry (solo or a bulk group's member) has its own checkbox, keyed by its
    # own `id` -- a bulk group's checkbox is a separate "select all members in
    # this group" toggle (see group_row_ui()), not a selection unit of its
    # own, so a specific chart within a bulk download can be picked out on its
    # own or the whole group can be selected in one click. Stored in
    # `selected` (reactiveValues) rather than read directly from
    # `input[[...]]`, because a freshly-rendered checkboxInput (renderUI can
    # re-run on any poll tick, e.g. someone else logs a new export while rows
    # are checked) resets to its `value=` argument -- each row is rendered
    # with `value = isTRUE(selected[[e$id]])` so it survives that.
    selected <- shiny::reactiveValues()
    registered_checkboxes <- new.env()

    entry_row_ui <- function(e, indent = FALSE) {
      shiny::tags$div(
        style = paste(
          "display:flex; justify-content:space-between; align-items:flex-start;",
          "gap:12px; padding:8px 0; border-bottom:1px solid #eee;",
          if (indent) "margin-left:20px; padding-left:10px; border-left:2px solid #E5E7EB;" else ""
        ),
        shiny::tags$div(
          style = "display:flex; gap:8px; align-items:flex-start;",
          shiny::tags$div(
            class = "tc-row-checkbox",
            shiny::checkboxInput(session$ns(paste0("sel_", e$id)), NULL, value = isTRUE(selected[[e$id]]))
          ),
          shiny::tags$div(
            shiny::tags$strong(tc_or(e$label, "(untitled)")),
            shiny::tags$code(style = "font-size:11px; margin-left:8px; color:#6B7280;", tc_or(e$id, "")),
            tc_dictionary_badge_ui(e$dictionary_format),
            shiny::tags$div(
              style = "font-size:12px; color:#6B7280;",
              tc_history_entry_subtitle(e)
            ),
            shiny::tags$div(
              style = "font-size:11px; color:#9CA3AF;",
              tc_or(e$chart_type, "")
            ),
            if (nzchar(tc_or(e$created_at, ""))) shiny::tags$div(
              style = "font-size:11px; color:#9CA3AF;",
              paste0("Downloaded: ", e$created_at)
            ),
            if (nzchar(tc_or(e$template_name, ""))) shiny::tags$div(
              style = "font-size:11px; color:#6B7280; margin-top:2px;",
              shiny::tags$span(style = "color:#9CA3AF;", "Template: "),
              e$template_name
            ),
            tc_selections_details_ui(e$selections, crosswalk = e$dictionary_crosswalk)
          )
        )
      )
    }

    group_row_ui <- function(g) {
      is_open <- isTRUE(expanded[[g$favorite_download_id]])
      # This checkbox isn't its own selection -- it's a "select every member
      # of this group" convenience, reflecting (and driving) the members' own
      # checkboxes below rather than a unit of its own, so a specific chart
      # within a bulk download can still be picked out on its own.
      all_selected <- length(g$members) > 0 &&
        all(vapply(g$members, function(m) isTRUE(selected[[m$id]]), logical(1)))
      shiny::tags$div(
        style = "padding:8px 0; border-bottom:1px solid #eee;",
        shiny::tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; gap:12px;",
          shiny::tags$div(
            style = "display:flex; gap:8px; align-items:center;",
            shiny::tags$div(
              class = "tc-row-checkbox",
              shiny::checkboxInput(session$ns(paste0("sel_group_", g$favorite_download_id)), NULL, value = all_selected)
            ),
            shiny::actionLink(
              session$ns(paste0("toggle_", g$favorite_download_id)),
              label = sprintf("%s \U0001F4E6 Bulk download — %d charts",
                              if (is_open) "▾" else "▸", length(g$members))
            ),
            shiny::tags$code(style = "font-size:11px; margin-left:8px; color:#6B7280;", g$favorite_download_id),
            if (nzchar(g$created_at)) shiny::tags$div(
              style = "font-size:11px; color:#9CA3AF;",
              paste0("Downloaded: ", g$created_at)
            )
          )
        ),
        if (is_open) shiny::tagList(lapply(g$members, function(m) entry_row_ui(m, indent = TRUE)))
      )
    }

    output$list <- shiny::renderUI({
      rows  <- display_rows()
      total <- length(entries_reactive())
      if (total == 0) {
        return(shiny::tags$p(class = "text-muted", "No exports logged yet."))
      }
      if (length(rows) == 0) {
        return(shiny::tags$p(class = "text-muted", "No exports match that search."))
      }

      cap_note <- if (!nzchar(trimws(tc_or(input$search, ""))) && total > display_limit) {
        shiny::tags$p(
          class = "text-muted", style = "font-size:12px;",
          sprintf("Showing the %d most recent of %d exports. Search to reach further back.",
                  display_limit, total)
        )
      } else {
        NULL
      }

      row_uis <- lapply(rows, function(r) {
        if (identical(r$kind, "group")) group_row_ui(r) else entry_row_ui(r$entry)
      })
      do.call(shiny::tagList, c(list(cap_note), row_uis))
    })

    # Every individual entry (solo or a bulk group's member) is its own
    # selection unit, so the whole selection is just whichever currently-
    # displayed entries have their own checkbox checked.
    selected_entries <- shiny::reactive({
      Filter(function(e) isTRUE(selected[[e$id]]), filtered_entries())
    })

    # Expand/collapse state for bulk groups, and a lazily-registered toggle +
    # "select all members" observer per group id -- registered exactly once
    # per id ever seen (favorite_download_id is stable/permanent), unlike the
    # redownload handlers above: an observeEvent (unlike a downloadHandler
    # assignment) isn't idempotent, so re-registering on every poll tick
    # would stack duplicate handlers and make toggling/selecting flip-flop
    # incorrectly over time.
    expanded <- shiny::reactiveValues()
    registered_toggles <- new.env()
    shiny::observe({
      entries <- entries_reactive()
      bulk_ids <- unique(Filter(nzchar, vapply(entries, function(e) tc_or(e$favorite_download_id, ""), character(1))))
      new_ids <- Filter(function(gid) !exists(gid, envir = registered_toggles, inherits = FALSE), bulk_ids)
      lapply(new_ids, function(gid) {
        assign(gid, TRUE, envir = registered_toggles)
        btn_id <- paste0("toggle_", gid)
        shiny::observeEvent(input[[btn_id]], {
          expanded[[gid]] <- !isTRUE(expanded[[gid]])
        }, ignoreInit = TRUE)

        group_id <- gid
        group_input_id <- paste0("sel_group_", group_id)
        shiny::observeEvent(input[[group_input_id]], {
          new_val <- isTRUE(input[[group_input_id]])
          members <- Filter(function(e) identical(tc_or(e$favorite_download_id, ""), group_id), entries_reactive())
          for (m in members) {
            selected[[m$id]] <- new_val
            shiny::updateCheckboxInput(session, paste0("sel_", m$id), value = new_val)
          }
        }, ignoreInit = TRUE, ignoreNULL = FALSE)
      })
    })

    # Same lazy-registration pattern as the toggles above, but for each
    # entry's own checkbox -- an observeEvent isn't idempotent, so this must
    # only ever register once per entry id.
    shiny::observe({
      ids <- vapply(entries_reactive(), function(e) e$id, character(1))
      new_ids <- Filter(function(rid) !exists(rid, envir = registered_checkboxes, inherits = FALSE), ids)
      lapply(new_ids, function(rid) {
        assign(rid, TRUE, envir = registered_checkboxes)
        input_id <- paste0("sel_", rid)
        shiny::observeEvent(input[[input_id]], {
          selected[[rid]] <- isTRUE(input[[input_id]])
        }, ignoreInit = TRUE, ignoreNULL = FALSE)
      })
    })

    shiny::observeEvent(input$clear_selection, {
      for (rid in ls(registered_checkboxes)) {
        selected[[rid]] <- FALSE
        shiny::updateCheckboxInput(session, paste0("sel_", rid), value = FALSE)
      }
      for (gid in ls(registered_toggles)) {
        shiny::updateCheckboxInput(session, paste0("sel_group_", gid), value = FALSE)
      }
    })

    output$selection_banner <- shiny::renderUI({
      entries <- selected_entries()
      if (length(entries) == 0) return(NULL)
      shiny::tags$div(
        style = paste(
          "position:fixed; left:0; right:0; bottom:0; z-index:1000;",
          "background:#111827; color:#fff; padding:10px 20px;",
          "display:flex; align-items:center; justify-content:center; gap:16px; flex-wrap:wrap;",
          "box-shadow:0 -2px 8px rgba(0,0,0,0.15);"
        ),
        sprintf("%d chart(s) selected", length(entries)),
        shiny::actionLink(session$ns("clear_selection"), "Clear selection", style = "color:#93C5FD;"),
        shiny::downloadButton(
          session$ns("download_selected"), "Redownload selected",
          class = "btn-default btn-sm"
        ),
        shiny::actionButton(
          session$ns("regenerate_selected_go"), "Regenerate selected against today's data",
          class = "btn-primary btn-sm tc-regenerate-go-btn",
          `data-module-ids` = jsonlite::toJSON(unique(Filter(
            nzchar, vapply(entries, function(e) tc_or(e$module_id, ""), character(1))
          ))),
          `data-capture-input-id` = session$ns("regenerate_capture")
        ),
        shiny::tags$span(
          style = "display:none;",
          shiny::downloadButton(session$ns("regenerate_selected"), "")
        ),
        shiny::downloadButton(
          session$ns("regenerate_excel_selected"), "Regenerate Excel only (think-cell format)",
          class = "btn-default btn-sm"
        )
      )
    })

    output$download_selected <- shiny::downloadHandler(
      filename = function() {
        entries <- selected_entries()
        # A single selected entry's own id, or (for a multi-select that
        # happens to be one whole bulk group) that group's shared
        # favorite_download_id -- both are already known from the entries
        # themselves, no extra minting needed. An arbitrary mixed selection
        # has no single id to show, so it's omitted rather than guessed at.
        id_part <- if (length(entries) == 1) {
          tc_or(entries[[1]]$id, "")
        } else {
          group_ids <- unique(Filter(nzchar, vapply(entries, function(e) tc_or(e$favorite_download_id, ""), character(1))))
          if (length(entries) > 0 && length(group_ids) == 1) group_ids else ""
        }
        paste0("export_history_selected_", if (nzchar(id_part)) paste0(id_part, "_") else "", Sys.Date(), ".zip")
      },
      content = function(file) {
        entries <- selected_entries()
        shiny::req(length(entries) > 0)
        export_history_download_many(entries, file)
      }
    )

    # Set by TC_CHART_CAPTURE_JS's ".tc-regenerate-go-btn" click handler --
    # a named list of data-URIs keyed by module_id, one per currently-mounted
    # chart it managed to screenshot before giving up (see
    # export_history_regenerate_many()'s `captures` param). Triggers the
    # real (hidden) download once the client is done, whether or not it
    # found anything to capture.
    pending_regenerate_captures <- shiny::reactiveVal(list())
    shiny::observeEvent(input$regenerate_capture, {
      pending_regenerate_captures(tc_or(input$regenerate_capture$captures, list()))
      session$sendCustomMessage("tc_trigger_download", list(download_id = session$ns("regenerate_selected")))
    }, ignoreInit = TRUE)

    output$regenerate_selected <- shiny::downloadHandler(
      filename = function() paste0("export_history_selected_regenerated_", Sys.Date(), ".zip"),
      content = function(file) {
        entries <- selected_entries()
        shiny::req(length(entries) > 0)
        export_history_regenerate_many(entries, file, session, captures = pending_regenerate_captures())
      }
    )
    # This download link lives inside a `display:none` wrapper (see
    # output$selection_banner above) -- see the matching note in
    # utils/chart_downloads.R's own output$slide for why this is required.
    shiny::outputOptions(output, "regenerate_selected", suspendWhenHidden = FALSE)

    output$regenerate_excel_selected <- shiny::downloadHandler(
      filename = function() {
        entries <- selected_entries()
        if (length(entries) == 1) {
          # The *source* entry's own id -- the regenerated copy mints its
          # own fresh id inside export_history_regenerate_excel_many(), only
          # known once content() actually runs, so this traces the download
          # back to what it was regenerated from instead.
          id_part <- tc_or(entries[[1]]$id, "")
          paste0(
            tc_or(entries[[1]]$filename_prefix, "chart"), "_thinkcell_regenerated_",
            if (nzchar(id_part)) paste0(id_part, "_") else "", Sys.Date(), ".xlsx"
          )
        } else {
          paste0("export_history_selected_thinkcell_regenerated_", Sys.Date(), ".xlsx")
        }
      },
      content = function(file) {
        entries <- selected_entries()
        shiny::req(length(entries) > 0)
        export_history_regenerate_excel_many(entries, file, session)
      }
    )
  })
}
