library(testthat)

templates_dir <- file.path("..", "..", "templates")
have_templates <- dir.exists(templates_dir)

with_favorites_path <- function(code) {
  path <- tempfile("favorites_", fileext = ".json")
  old <- Sys.getenv("SHINY_FAVORITES_PATH", unset = NA)
  Sys.setenv(SHINY_FAVORITES_PATH = path)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_FAVORITES_PATH") else Sys.setenv(SHINY_FAVORITES_PATH = old)
    unlink(path)
  }, add = TRUE)
  force(code)
}

with_history_dir <- function(code) {
  dir <- tempfile("export_history_")
  old <- Sys.getenv("SHINY_EXPORT_HISTORY_DIR", unset = NA)
  Sys.setenv(SHINY_EXPORT_HISTORY_DIR = dir)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_EXPORT_HISTORY_DIR") else Sys.setenv(SHINY_EXPORT_HISTORY_DIR = old)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)
  force(code)
}

sample_df <- function() {
  data.frame(
    quarter = rep(c("Q1", "Q2"), each = 2),
    product = rep(c("A", "B"), 2),
    revenue = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

# fake_session() lives in helper-session.R (auto-sourced by testthat), shared
# with test-export_history.R.

#' Register a chart's "current exportable state" into a fake session's chart
#' registry, the same shape build_export_spec() in utils/chart_downloads.R
#' returns -- everything favorites_live_spec_or_null() (and everything built
#' on top of it) needs to rebuild a favorite live, without a real Shiny
#' session or a real chart module.
register_live_chart <- function(session, module_id, overrides = list()) {
  base_spec <- list(
    tc_data = format_tc_data(
      sample_df(), chart_type = "stacked_bar",
      category_col = "quarter", series_col = "product", value_col = "revenue",
      agg_fun = NULL
    ),
    raw_data = sample_df(),
    chart_type = "stacked_bar",
    slide_matrix = NULL,
    is_faceted = FALSE,
    slide_title = "",
    figure_title = "",
    template_override = "",
    slide_order = "auto",
    dashboard_title = "D",
    tab_label = "T",
    subtab_label = "Revenue",
    selections = list(),
    source_output = "",
    source_sheet = "",
    source_mtime = "",
    filename_prefix = "revenue_chart"
  )
  spec <- utils::modifyList(base_spec, overrides)
  tc_chart_registry_register(session, module_id, list(
    build_zip = function(...) stop("not used by favorites tests"),
    get_spec  = function() spec
  ))
  invisible(spec)
}

test_that("favorites_path honors SHINY_FAVORITES_PATH and defaults to state/favorites.json", {
  with_favorites_path({
    expect_equal(favorites_path(), Sys.getenv("SHINY_FAVORITES_PATH"))
  })
  old <- Sys.getenv("SHINY_FAVORITES_PATH", unset = NA)
  Sys.unsetenv("SHINY_FAVORITES_PATH")
  on.exit(if (!is.na(old)) Sys.setenv(SHINY_FAVORITES_PATH = old), add = TRUE)
  expect_equal(favorites_path(), file.path("state", "favorites.json"))
})

test_that("favorites_list is empty before anything is saved", {
  with_favorites_path({
    expect_equal(favorites_list(), list())
  })
})

test_that("add/list/remove round-trip through the JSON file", {
  with_favorites_path({
    id1 <- favorites_add(list(label = "Revenue Q1-Q4", chart_type = "stacked_bar"))
    id2 <- favorites_add(list(label = "Another chart", chart_type = "line"))

    entries <- favorites_list()
    expect_length(entries, 2)
    expect_equal(entries[[1]]$label, "Revenue Q1-Q4")
    expect_true(nzchar(entries[[1]]$id))
    expect_true(nzchar(entries[[1]]$created_at))

    favorites_remove(id1)
    remaining <- favorites_list()
    expect_length(remaining, 1)
    expect_equal(remaining[[1]]$label, "Another chart")

    favorites_remove(id2)
    expect_equal(favorites_list(), list())
  })
})

test_that("repeated sequential favorites_add/favorites_remove calls still succeed under the new file lock", {
  # favorites_add()/favorites_remove() now run their read-modify-write cycle
  # under tc_with_file_lock() (see utils/slide_download.R); if the lock
  # weren't released after each call, a later call would hang/time out
  # rather than fail an assertion.
  with_favorites_path({
    ids <- vapply(1:5, function(i) favorites_add(list(label = paste0("Fav ", i))), character(1))
    expect_length(unique(ids), 5)
    expect_length(favorites_list(), 5)

    favorites_remove(ids[[1]])
    expect_length(favorites_list(), 4)
  })
})

test_that("favorites_remove_all empties the list", {
  with_favorites_path({
    favorites_add(list(label = "One"))
    favorites_add(list(label = "Two"))

    favorites_remove_all()

    expect_equal(favorites_list(), list())
  })
})

test_that("favorites_remove_ids removes only the given ids, leaving the rest untouched", {
  with_favorites_path({
    id1 <- favorites_add(list(label = "One"))
    id2 <- favorites_add(list(label = "Two"))
    id3 <- favorites_add(list(label = "Three"))

    favorites_remove_ids(c(id1, id3))

    remaining <- favorites_list()
    expect_length(remaining, 1)
    expect_equal(remaining[[1]]$id, id2)
  })
})

test_that("favorites persist across a fresh read (simulating an app restart)", {
  with_favorites_path({
    favorites_add(list(label = "Survives restart"))
    # A brand-new call to favorites_list() re-reads from disk, not memory.
    expect_equal(favorites_list()[[1]]$label, "Survives restart")
  })
})

test_that("favorites_capture stores only bookmark/display metadata, not a data snapshot", {
  entry <- favorites_capture(
    chart_type = "stacked_bar", dashboard_title = "D", tab_label = "T",
    subtab_label = "Revenue", module_id = "revenue_chart_dl",
    filename_prefix = "revenue_chart"
  )
  expect_equal(entry$chart_type, "stacked_bar")
  expect_equal(entry$module_id, "revenue_chart_dl")
  expect_null(entry$tc_table)
  expect_null(entry$raw_table)
  expect_null(entry$slide_block)
})

test_that("favorites_capture labels a favorite with the chart's own title, not the sub-tab, when one is supplied", {
  entry <- favorites_capture(
    chart_type = "stacked_bar",
    figure_title = "Revenue by Product", subtab_label = "Revenue",
    filename_prefix = "revenue_chart"
  )
  expect_equal(entry$label, "Revenue by Product")
})

test_that("favorites_capture falls back to the sub-tab label when the chart has no title", {
  entry <- favorites_capture(
    chart_type = "stacked_bar", subtab_label = "Revenue",
    filename_prefix = "revenue_chart"
  )
  expect_equal(entry$label, "Revenue")
})

test_that("favorites_capture stores module_id when supplied", {
  entry <- favorites_capture(
    chart_type = "stacked_bar", module_id = "revenue_chart_dl",
    filename_prefix = "revenue_chart"
  )
  expect_equal(entry$module_id, "revenue_chart_dl")
})

test_that("favorites_capture records the dictionary_format flag (defaulting to FALSE)", {
  on  <- favorites_capture(chart_type = "stacked_bar", dictionary_format = TRUE)
  off <- favorites_capture(chart_type = "stacked_bar", dictionary_format = FALSE)
  default <- favorites_capture(chart_type = "stacked_bar")
  expect_true(on$dictionary_format)
  expect_false(off$dictionary_format)
  expect_false(default$dictionary_format)
})

test_that("favorites_selections_inline renders a compact, truncated one-liner", {
  expect_equal(favorites_selections_inline(NULL), "")
  expect_equal(favorites_selections_inline(list()), "")
  # empty values are dropped
  expect_equal(
    favorites_selections_inline(list(jaar = "2023", leeg = "", pop = character(0))),
    "jaar: 2023"
  )
  # multiple options joined; multi-value collapsed
  expect_equal(
    favorites_selections_inline(list(jaar = "2023", groep = c("A", "B"))),
    paste0("jaar: 2023 ", intToUtf8(0x00B7), " groep: A, B")
  )
  # long strings are truncated with an ellipsis
  long <- favorites_selections_inline(list(x = paste(rep("y", 300), collapse = "")), max_chars = 20)
  expect_true(nchar(long) <= 20)
  expect_equal(substr(long, nchar(long), nchar(long)), intToUtf8(0x2026))
})

test_that("favorites_table_as_df passes a live data.frame through unchanged", {
  df <- sample_df()
  expect_identical(favorites_table_as_df(df), df)
})

test_that("table storage/restore round-trips columns and values, including an empty header", {
  m <- data.frame(lab = c("A", "B"), `2023` = c(42, 30.5), check.names = FALSE)
  names(m)[1] <- ""

  stored <- favorites_table_to_storage(m)
  expect_equal(stored$columns, c("", "2023"))

  restored <- favorites_table_as_df(stored)
  expect_equal(names(restored), c("", "2023"))
  expect_equal(restored[[1]], c("A", "B"))
  expect_equal(restored[["2023"]], c(42, 30.5))
})

test_that("the empty first-column header survives a real JSON round-trip", {
  # This is the exact bug this shape avoids: jsonlite renames an empty
  # data.frame column name to its positional index ("1") when a data.frame is
  # serialized directly, which favorites_table_to_storage() sidesteps.
  m <- data.frame(lab = "A", `2023` = 42, check.names = FALSE)
  names(m)[1] <- ""
  stored <- favorites_table_to_storage(m)

  path <- tempfile(fileext = ".json")
  jsonlite::write_json(stored, path, auto_unbox = TRUE)
  reread <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  expect_equal(reread$columns[[1]], "")
  restored <- favorites_table_as_df(reread)
  expect_equal(names(restored)[1], "")
})

test_that("tc_build_charts_overview_html returns NA when no spec has a usable asset_path", {
  specs <- list(
    list(label = "A", asset_path = NULL),
    list(label = "B", asset_path = "/does/not/exist.png")
  )
  expect_true(is.na(tc_build_charts_overview_html(specs)))
})

test_that("tc_build_charts_overview_html embeds every image, in spec order, as a data URI", {
  png1 <- tempfile(fileext = ".png")
  png2 <- tempfile(fileext = ".png")
  writeBin(as.raw(1:4), png1)
  writeBin(as.raw(5:8), png2)
  on.exit(unlink(c(png1, png2)))

  specs <- list(
    list(label = "First Chart", asset_path = png1),
    list(label = "Second Chart", asset_path = png2)
  )
  html <- tc_build_charts_overview_html(specs)
  expect_false(is.na(html))
  expect_true(grepl("<!doctype html>", html, fixed = TRUE))
  expect_true(grepl("data:image/png;base64,", html, fixed = TRUE))
  # "First Chart" must appear before "Second Chart" -- same order as specs.
  expect_lt(regexpr("First Chart", html, fixed = TRUE), regexpr("Second Chart", html, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Live rebuild: favorites_live_spec_or_null() / favorites_prepare_live_table()
# / favorites_prepare_live_spec() and the four bulk-download consumers built
# on them. There is no snapshot fallback -- a favorite whose chart isn't
# live this session is skipped, not replayed.
# ---------------------------------------------------------------------------

test_that("favorites_live_spec_or_null returns NULL when the chart isn't registered this session", {
  session <- fake_session()
  expect_null(favorites_live_spec_or_null(list(module_id = "missing_dl"), session))
})

test_that("favorites_live_spec_or_null returns NULL for a faceted chart", {
  session <- fake_session()
  register_live_chart(session, "faceted_dl", list(is_faceted = TRUE))
  expect_null(favorites_live_spec_or_null(list(module_id = "faceted_dl"), session))
})

test_that("favorites_live_spec_or_null returns the live spec when the chart is registered", {
  session <- fake_session()
  register_live_chart(session, "revenue_dl")
  spec <- favorites_live_spec_or_null(list(module_id = "revenue_dl"), session)
  expect_equal(spec$chart_type, "stacked_bar")
  expect_equal(spec$subtab_label, "Revenue")
})

test_that("favorites_prepare_live_table returns today's tables for a live favorite, NULL for a stale one", {
  session <- fake_session()
  register_live_chart(session, "revenue_dl")

  live <- favorites_prepare_live_table(list(label = "Revenue", module_id = "revenue_dl"), session)
  expect_equal(live$label, "Revenue")
  expect_equal(nrow(live$raw_table), nrow(sample_df()))
  expect_equal(names(live$tc_table)[1], "")

  expect_null(favorites_prepare_live_table(list(label = "Stale", module_id = "gone_dl"), session))
})

test_that("favorites_build_raw_xlsx/thinkcell_xlsx include only live favorites and report the rest as skipped", {
  skip_if_not_installed("readxl")
  session <- fake_session()
  register_live_chart(session, "revenue_dl", list(subtab_label = "Revenue"))

  entries <- list(
    list(label = "Revenue", module_id = "revenue_dl"),
    list(label = "Stale Chart", module_id = "gone_dl")
  )

  raw_path <- tempfile(fileext = ".xlsx")
  skipped_raw <- favorites_build_raw_xlsx(raw_path, entries = entries, session = session)
  expect_equal(readxl::excel_sheets(raw_path), "Revenue")
  expect_equal(skipped_raw, "Stale Chart")

  tc_path <- tempfile(fileext = ".xlsx")
  skipped_tc <- favorites_build_thinkcell_xlsx(tc_path, entries = entries, session = session)
  expect_equal(readxl::excel_sheets(tc_path), "Revenue")
  expect_equal(skipped_tc, "Stale Chart")
})

test_that("favorites_build_raw_xlsx is never logged to Export History", {
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    favorites_build_raw_xlsx(tempfile(fileext = ".xlsx"), entries = entries, session = session)
    expect_length(export_history_list(), 0)
  })
})

test_that("favorites_build_thinkcell_xlsx stamps the A1 corner-cell log and logs to Export History (identical to the slides-zip table)", {
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    tc_path <- tempfile(fileext = ".xlsx")
    favorites_build_thinkcell_xlsx(tc_path, entries = entries, session = session, templates_dir = templates_dir)

    # The A1 corner-cell provenance log is now present (the first batch's
    # lightweight path left it bare), and the sheet name matches the ZIP.
    expect_equal(readxl::excel_sheets(tc_path), "Revenue")
    header <- names(readxl::read_excel(tc_path, sheet = "Revenue", n_max = 0))
    expect_true(grepl("^LOG \\|", header[[1]]))
    # And a think-cell Excel download is now recorded in Export History,
    # just like a slide download.
    expect_length(export_history_list(), 1)
  })
})

test_that("favorites_build_thinkcell_xlsx embeds a pre-minted favorite_download_id in the corner-cell log", {
  # The bulk think-cell download's filename() pre-mints the id and puts it in
  # the file name, then passes it here as favorite_download_id_override so the
  # workbook's own A1 log carries the SAME id -- file name and log agree.
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    tc_path <- tempfile(fileext = ".xlsx")
    favorites_build_thinkcell_xlsx(
      tc_path, entries = entries, session = session, templates_dir = templates_dir,
      favorite_download_id_override = "fav_dl_test_777"
    )
    header <- names(readxl::read_excel(tc_path, sheet = "Revenue", n_max = 0))
    expect_true(grepl("favorite_download_id=fav_dl_test_777", header[[1]], fixed = TRUE))
  })
})

test_that("favorites_capture persists the chosen slide template (manual pick and explicit auto)", {
  e_manual <- favorites_capture(chart_type = "stacked_bar", template_override = "template_h_bar.pptx")
  expect_equal(e_manual$template_override, "template_h_bar.pptx")
  # No manual pick -> "" (explicit auto), still a present field so a live
  # rebuild treats it as "the user wanted auto", distinct from an older
  # favorite that predates the field entirely (no key at all).
  e_auto <- favorites_capture(chart_type = "stacked_bar")
  expect_equal(e_auto$template_override, "")
})

test_that("favorites_prepare_live_spec applies the favorite's stored template, not the live picker's default", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    # Live chart reports auto (template_override = ""), which for stacked_bar
    # would resolve to template_v_bar_stacked.pptx.
    register_live_chart(session, "revenue_dl")
    entry <- list(label = "Revenue", module_id = "revenue_dl",
                  template_override = "template_h_bar.pptx")

    spec <- favorites_prepare_live_spec(entry, session, templates_dir = templates_dir)
    expect_equal(basename(spec$template_path), "template_h_bar.pptx")
  })
})

test_that("favorites_prepare_live_spec falls back to the live template for a favorite predating the stored field", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl", overrides = list(template_override = "template_h_bar.pptx"))
    entry <- list(label = "Revenue", module_id = "revenue_dl")  # no template_override key

    spec <- favorites_prepare_live_spec(entry, session, templates_dir = templates_dir)
    expect_equal(basename(spec$template_path), "template_h_bar.pptx")
  })
})

test_that("favorites_build_raw_xlsx with no favorites still writes a valid workbook", {
  path <- tempfile(fileext = ".xlsx")
  favorites_build_raw_xlsx(path, entries = list(), session = fake_session())
  expect_true(file.exists(path))
})

test_that("favorites_build_thinkcell_xlsx with no favorites still writes a valid workbook", {
  path <- tempfile(fileext = ".xlsx")
  favorites_build_thinkcell_xlsx(path, entries = list(), session = fake_session())
  expect_true(file.exists(path))
})

test_that("favorites_build_specs_with_history builds a spec for a live favorite and skips a stale one", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(
      list(label = "Revenue", module_id = "revenue_dl"),
      list(label = "Stale Chart", module_id = "gone_dl")
    )

    result <- favorites_build_specs_with_history(entries, session, templates_dir = templates_dir)
    expect_length(result$specs, 1)
    expect_equal(result$specs[[1]]$label, "Revenue")
    expect_equal(result$skipped, "Stale Chart")
  })
})

test_that("deck ZIP with no favorites still produces a valid, explanatory ZIP", {
  z <- tempfile(fileext = ".zip")
  favorites_build_deck_zip(z, entries = list(), session = fake_session())
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("README.txt" %in% files)
})

test_that("deck ZIP includes the think-cell table even when the live spec has no raw data", {
  session <- fake_session()
  register_live_chart(session, "no_raw_dl", list(
    chart_type = "waterfall", raw_data = NULL, subtab_label = "S"
  ))
  entries <- list(list(label = "No template chart", module_id = "no_raw_dl"))

  z <- tempfile(fileext = ".zip")
  skipped <- favorites_build_deck_zip(z, entries = entries, session = session)
  expect_length(skipped, 0)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("favorites_thinkcell_tables.xlsx" %in% files)
  expect_false("log.txt" %in% files)
  # "waterfall" has no built-in template in templates/, so no deck is built.
  expect_true("NO_TEMPLATE.txt" %in% files)
  expect_false(any(grepl("deck", files)))
  # No raw_data on the live spec -> the raw workbook is skipped, not an error.
  expect_false("favorites_raw_tables.xlsx" %in% files)
})

test_that("deck ZIP includes a raw-tables workbook alongside the think-cell one", {
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)
    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("favorites_thinkcell_tables.xlsx" %in% files)
    expect_true("favorites_raw_tables.xlsx" %in% files)

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    raw <- as.data.frame(readxl::read_excel(
      file.path(extract_dir, "favorites_raw_tables.xlsx"), sheet = "Revenue"
    ))
    # The raw workbook holds the original long-format plot data, not the
    # think-cell pivoted matrix.
    expect_true(all(c("quarter", "product", "revenue") %in% names(raw)))
    expect_equal(nrow(raw), nrow(sample_df()))
    # favorites_build_deck_zip() already has the *combined* workbooks above --
    # it must not also duplicate each chart's own table/raw.xlsx (that's
    # favorites_build_slides_zip()'s job, see tc_write_deck_files()'s
    # include_tables).
    expect_false("Revenue_table.xlsx" %in% files)
    expect_false("Revenue_raw.xlsx" %in% files)
  })
})

test_that("favorites_build_slides_zip includes the deck plus one combined table/raw workbook, not one loose xlsx per chart", {
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_slides_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)
    files <- utils::unzip(z, list = TRUE)$Name
    expect_true(any(grepl("^favorites_deck", files)))
    expect_true("favorites_thinkcell_tables.xlsx" %in% files)
    expect_true("favorites_raw_tables.xlsx" %in% files)
    expect_false("Revenue_table.xlsx" %in% files)
    expect_false("Revenue_raw.xlsx" %in% files)

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    expect_equal(
      readxl::excel_sheets(file.path(extract_dir, "favorites_thinkcell_tables.xlsx")), "Revenue"
    )

    # Still logged to Export History, same as favorites_build_deck_zip().
    expect_length(export_history_list(), 1)
  })
})

test_that("deck ZIP's combined favorites_thinkcell_tables.xlsx carries each sheet's own provenance log in its corner cell", {
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    header <- names(readxl::read_excel(
      file.path(extract_dir, "favorites_thinkcell_tables.xlsx"), sheet = "Revenue", n_max = 0
    ))
    expect_true(grepl("^LOG \\|", header[[1]]))
  })
})

test_that("favorites_build_slides_zip's combined table workbook carries the same provenance log the single-chart download does", {
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_slides_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    header <- names(readxl::read_excel(
      file.path(extract_dir, "favorites_thinkcell_tables.xlsx"), sheet = "Revenue", n_max = 0
    ))
    expect_true(grepl("^LOG \\|", header[[1]]))
  })
})

test_that("favorites_build_slides_zip's combined workbook survives a '|'-containing chart label", {
  skip_if_not(have_templates, "templates directory not available")
  skip_if_not_installed("readxl")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    # Several chart titles in real dashboards join parts with " | " (e.g.
    # "sheet | outcome | maat") -- Excel sheet names tolerate '|' (unlike a
    # real file name, which is why this used to matter for the old
    # per-chart-file layout); the combined workbook writes it straight
    # through as a sheet name with no special handling needed.
    entries <- list(list(label = "MSZ activiteit diagnostiek | Aantal gebruikers", module_id = "revenue_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_slides_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)
    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    expect_equal(
      readxl::excel_sheets(file.path(extract_dir, "favorites_thinkcell_tables.xlsx")),
      "MSZ activiteit diagnostiek | Aantal gebruikers"
    )
  })
})

test_that("favorites_build_slides_zip with no favorites still produces a valid, explanatory ZIP", {
  z <- tempfile(fileext = ".zip")
  favorites_build_slides_zip(z, entries = list(), session = fake_session())
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("README.txt" %in% files)
})

test_that("deck ZIP includes one charts_overview.html bundling only the favorites with a captured image", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "with_snap_dl", list(subtab_label = "Has Snapshot"))
    register_live_chart(session, "no_snap_dl", list(subtab_label = "No Snapshot"))

    entries <- list(
      list(label = "Has Snapshot", module_id = "with_snap_dl"),
      list(label = "No Snapshot", module_id = "no_snap_dl")
    )
    fake_png <- paste0("data:image/png;base64,", jsonlite::base64_enc(as.raw(c(0x89, 0x50, 0x4E, 0x47))))

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(
      z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir,
      captures = list(with_snap_dl = fake_png)
    )
    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("charts_overview.html" %in% files)

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    html <- paste(readLines(file.path(extract_dir, "charts_overview.html")), collapse = "\n")
    expect_true(grepl("Has Snapshot", html, fixed = TRUE))
    expect_false(grepl("No Snapshot", html, fixed = TRUE))
    expect_true(grepl("data:image/png;base64,", html, fixed = TRUE))
  })
})

test_that("deck ZIP falls back to template + combined .ppttc when no renderer is available", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)
    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("favorites_deck.ppttc" %in% files)
    expect_true(any(grepl("README_render_deck", files)))
    expect_true("favorites_thinkcell_tables.xlsx" %in% files)
    expect_false("log.txt" %in% files)
  })
})

test_that("the fallback deck .ppttc references co-located templates, not the live spec's absolute path", {
  # Regression test for the same bug class as tc_build_slide_zip(): a
  # resolved template_path is an *absolute* path, valid only on the machine
  # that resolved it. The shipped favorites_deck.ppttc must instead reference
  # each template by the bare file name copied alongside it.
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")

    spec <- favorites_prepare_live_spec(list(label = "Revenue", module_id = "revenue_dl"), session,
                                         templates_dir = templates_dir)
    # Sanity check on the "before" state: the resolved path is a full
    # (short-)path, not just the bare template file name.
    expect_gt(nchar(spec$template_path), nchar(basename(spec$template_path)))

    z <- tempfile(fileext = ".zip")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)
    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")

    expect_true(grepl(sprintf('"template":"%s"', basename(spec$template_path)), ppttc, fixed = TRUE))
    expect_false(grepl(spec$template_path, ppttc, fixed = TRUE))
  })
})

test_that("each favorite in a deck download gets its own datasheet log in the corner cell", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl", list(selections = list(view = "quarterly")))
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)
    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")

    expect_true(grepl('"string":"LOG \\| ', ppttc))
    expect_true(grepl("dashboard=D; tab=T; sub-tab=Revenue", ppttc, fixed = TRUE))
    expect_true(grepl("view=quarterly", ppttc, fixed = TRUE))
  })
})

test_that("every favorite in one 'Download all favorites' click shares a single favorite_download_id", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl", list(subtab_label = "Revenue"))
    register_live_chart(session, "cost_dl", list(subtab_label = "Cost", filename_prefix = "cost_chart"))
    entries <- list(
      list(label = "Revenue", module_id = "revenue_dl"),
      list(label = "Cost", module_id = "cost_dl")
    )

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)

    history <- export_history_list()
    expect_length(history, 2)
    fdl_ids <- unique(vapply(history, function(e) tc_or(e$favorite_download_id, ""), character(1)))
    expect_length(fdl_ids, 1)
    expect_true(nzchar(fdl_ids))
    expect_true(startsWith(fdl_ids, "favdl_"))
    # ...and every download_id among them is distinct.
    expect_length(unique(vapply(history, function(e) e$id, character(1))), 2)

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")
    expect_equal(lengths(regmatches(ppttc, gregexpr("FavoriteDownloadID", ppttc)))[[1]], 2)
  })
})

test_that("downloading a favorites deck auto-logs each renderable favorite to Export History", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    expect_equal(export_history_list(), list())

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)

    history <- export_history_list()
    expect_length(history, 1)
    expect_equal(history[[1]]$subtab_label, "Revenue")
    expect_equal(history[[1]]$chart_type, "stacked_bar")
    # So a later redownload of this entry can also ship a companion
    # _raw.xlsx (see export_history_redownload()), not just the think-cell one.
    expect_false(is.null(history[[1]]$raw_data_table))

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "favorites_deck.ppttc")), collapse = "\n")
    expect_true(grepl(paste0("download_id=", history[[1]]$id), ppttc, fixed = TRUE))
    expect_true(grepl(sprintf('"string":"%s"', history[[1]]$id), ppttc, fixed = TRUE))
  })
})

test_that("each favorites-deck download mints a fresh history entry, not a reused one", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    entries <- list(list(label = "Revenue", module_id = "revenue_dl"))

    z1 <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z1, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)
    z2 <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z2, entries = entries, session = session, ppttc_exe = NA, templates_dir = templates_dir)

    history <- export_history_list()
    expect_length(history, 2)
    expect_false(identical(history[[1]]$id, history[[2]]$id))
  })
})

test_that("a favorite with no matching template is not logged to Export History", {
  with_history_dir({
    session <- fake_session()
    register_live_chart(session, "no_tpl_dl", list(chart_type = "waterfall"))
    entries <- list(list(label = "No template chart", module_id = "no_tpl_dl"))

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session)
    expect_equal(export_history_list(), list())

    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("NO_TEMPLATE.txt" %in% files)
    expect_false(any(grepl("deck", files)))
  })
})

test_that("a working ppttc executable renders one combined deck for multiple favorites", {
  skip_if_not(have_templates, "templates directory not available")
  skip_on_os("windows")  # stub is a POSIX shell script
  with_history_dir({
    exe <- tempfile(fileext = ".sh")
    writeLines(c("#!/bin/sh", 'out="$3"; printf "PK\\003\\004" > "$out"'), exe)
    Sys.chmod(exe, "0755")

    session <- fake_session()
    register_live_chart(session, "revenue_dl")
    # Same chart counted twice, to simulate two favorites folding into one
    # combined deck.
    entries <- list(
      list(label = "Revenue", module_id = "revenue_dl"),
      list(label = "Revenue", module_id = "revenue_dl")
    )

    z <- tempfile(fileext = ".zip")
    favorites_build_deck_zip(z, entries = entries, session = session, ppttc_exe = exe, templates_dir = templates_dir)
    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("favorites_deck.pptx" %in% files)
    expect_false(any(grepl("README_render_deck", files)))
  })
})

test_that("checking a favorite's row checkbox is reflected in selected_entries()", {
  with_favorites_path({
    favorites_add(list(label = "One"))
    id2 <- favorites_add(list(label = "Two"))

    shiny::testServer(favorites_panel_server, args = list(id = "test_fav"), {
      # Force the list (and its dynamic per-row checkbox observers,
      # registered by a separate shiny::observe() block) to materialize
      # before simulating a click on one of those checkboxes -- same
      # reasoning as test-dictionary_admin.R's own dynamic-id tests.
      entries <- entries_reactive()
      expect_length(entries, 2)
      session$flushReact()

      expect_length(selected_entries(), 0)

      do.call(session$setInputs, setNames(list(TRUE), paste0("sel_", id2)))
      selected <- selected_entries()
      expect_length(selected, 1)
      expect_equal(selected[[1]]$id, id2)
    })
  })
})

test_that("clear_selection unchecks every selected favorite", {
  with_favorites_path({
    id1 <- favorites_add(list(label = "One"))
    id2 <- favorites_add(list(label = "Two"))

    shiny::testServer(favorites_panel_server, args = list(id = "test_fav"), {
      entries_reactive()
      session$flushReact()
      do.call(session$setInputs, setNames(list(TRUE), paste0("sel_", id1)))
      do.call(session$setInputs, setNames(list(TRUE), paste0("sel_", id2)))
      expect_length(selected_entries(), 2)

      session$setInputs(clear_selection = 1)
      expect_length(selected_entries(), 0)
    })
  })
})

test_that("select_all checks every displayed favorite", {
  with_favorites_path({
    favorites_add(list(label = "One"))
    favorites_add(list(label = "Two"))
    favorites_add(list(label = "Three"))

    shiny::testServer(favorites_panel_server, args = list(id = "test_fav"), {
      entries_reactive()
      session$flushReact()
      expect_length(selected_entries(), 0)

      session$setInputs(select_all = 1)
      expect_length(selected_entries(), 3)
    })
  })
})

test_that("selected_entries() resolves to only the checked favorites, feeding the selected-only download handlers the right subset", {
  # download_selected_raw/download_selected_thinkcell (utils/favorites.R)
  # both just call favorites_build_raw_xlsx()/favorites_build_thinkcell_xlsx()
  # with entries = selected_entries() -- both build functions' own
  # entries-filtering is already covered by
  # "favorites_build_raw_xlsx/thinkcell_xlsx include only live favorites..."
  # above, so what actually needs covering here is that selected_entries()
  # itself resolves to the right subset once a checkbox is checked.
  skip_if_not_installed("readxl")
  with_favorites_path({
    favorites_add(list(label = "Revenue", module_id = "revenue_dl"))
    id2 <- favorites_add(list(label = "Other", module_id = "other_dl"))

    shiny::testServer(favorites_panel_server, args = list(id = "test_fav"), {
      # Registered against testServer's own internal session (bound to
      # `session` inside this block), not a separate fake_session() --
      # favorites_build_raw_xlsx() below is called with that same session,
      # and tc_chart_registry_get() only finds a chart under the session
      # that registered it.
      register_live_chart(session, "revenue_dl")
      register_live_chart(session, "other_dl")

      entries_reactive()
      session$flushReact()
      do.call(session$setInputs, setNames(list(TRUE), paste0("sel_", id2)))

      selected <- selected_entries()
      expect_length(selected, 1)
      expect_equal(selected[[1]]$module_id, "other_dl")

      path <- tempfile(fileext = ".xlsx")
      favorites_build_raw_xlsx(path, entries = selected, session = session)
      expect_equal(readxl::excel_sheets(path), "Other")
    })
  })
})

test_that("remove_selected_confirm removes only the checked favorites", {
  with_favorites_path({
    id1 <- favorites_add(list(label = "One"))
    id2 <- favorites_add(list(label = "Two"))

    shiny::testServer(favorites_panel_server, args = list(id = "test_fav"), {
      entries_reactive()
      session$flushReact()
      do.call(session$setInputs, setNames(list(TRUE), paste0("sel_", id1)))

      session$setInputs(remove_selected = 1)
      session$setInputs(remove_selected_confirm = 1)

      remaining <- favorites_list()
      expect_length(remaining, 1)
      expect_equal(remaining[[1]]$id, id2)
    })
  })
})
