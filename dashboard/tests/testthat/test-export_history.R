library(testthat)

templates_dir <- file.path("..", "..", "templates")
have_templates <- dir.exists(templates_dir)

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

sample_matrix <- function() {
  m <- data.frame(lab = c("A", "B"), `2023` = c(42, 30.5), `2024` = c(NA, 22),
                  check.names = FALSE, stringsAsFactors = FALSE)
  names(m) <- c("", "2023", "2024")
  m
}

test_that("export_history_dir honors SHINY_EXPORT_HISTORY_DIR and defaults to state/export_history", {
  with_history_dir({
    expect_equal(export_history_dir(), Sys.getenv("SHINY_EXPORT_HISTORY_DIR"))
  })
  old <- Sys.getenv("SHINY_EXPORT_HISTORY_DIR", unset = NA)
  Sys.unsetenv("SHINY_EXPORT_HISTORY_DIR")
  on.exit(if (!is.na(old)) Sys.setenv(SHINY_EXPORT_HISTORY_DIR = old), add = TRUE)
  expect_equal(export_history_dir(), file.path("state", "export_history"))
})

test_that("export_history_list is empty before anything is logged", {
  with_history_dir({
    expect_equal(export_history_list(), list())
  })
})

test_that("add/list/get/remove round-trip through one-file-per-entry storage", {
  with_history_dir({
    id1 <- export_history_add(list(label = "Chart One", chart_type = "line"))
    Sys.sleep(1.1)  # ensure a distinct created_at for ordering
    id2 <- export_history_add(list(label = "Chart Two", chart_type = "bar"))

    expect_true(nzchar(id1))
    expect_true(nzchar(id2))
    expect_true(file.exists(file.path(export_history_dir(), paste0(id1, ".json"))))

    entries <- export_history_list()
    expect_length(entries, 2)
    # Most recently created first.
    expect_equal(entries[[1]]$label, "Chart Two")
    expect_equal(entries[[2]]$label, "Chart One")

    got <- export_history_get(id1)
    expect_equal(got$label, "Chart One")
    expect_null(export_history_get("does_not_exist"))

    export_history_remove(id1)
    remaining <- export_history_list()
    expect_length(remaining, 1)
    expect_equal(remaining[[1]]$label, "Chart Two")
  })
})

test_that("export_history_add respects a pre-set id instead of generating a new one", {
  with_history_dir({
    id <- export_history_add(list(id = "exp_fixed_id", label = "Pinned"))
    expect_equal(id, "exp_fixed_id")
    expect_true(file.exists(file.path(export_history_dir(), "exp_fixed_id.json")))
    expect_equal(export_history_get("exp_fixed_id")$label, "Pinned")
  })
})

test_that("a corrupt entry file is skipped, not fatal, when listing", {
  with_history_dir({
    export_history_add(list(label = "Good entry"))
    dir.create(export_history_dir(), recursive = TRUE, showWarnings = FALSE)
    writeLines("not valid json {{{", file.path(export_history_dir(), "exp_broken.json"))

    entries <- export_history_list()
    expect_length(entries, 1)
    expect_equal(entries[[1]]$label, "Good entry")
  })
})

test_that("tc_history_capture resolves the template and a sensible label", {
  skip_if_not(have_templates, "templates directory not available")
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  expect_equal(entry$label, "Revenue")
  expect_false(is.na(entry$template_name))
  expect_equal(entry$tc_data_table$columns[1], "")
  expect_null(entry$slide_matrix_table)
  expect_null(entry$raw_data_table)
})

test_that("tc_history_capture stores slide_matrix separately when supplied", {
  skip_if_not(have_templates, "templates directory not available")
  slide_m <- data.frame(lab = "X", `2023` = 1, check.names = FALSE)
  names(slide_m)[1] <- ""
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line", slide_matrix = slide_m,
    filename_prefix = "chart", templates_dir = templates_dir
  )
  expect_false(is.null(entry$slide_matrix_table))
  restored <- favorites_table_as_df(entry$slide_matrix_table)
  expect_equal(restored[[1]], "X")
})

test_that("tc_history_capture stores raw_data separately when supplied", {
  raw_df <- data.frame(category = "X", value = 1, stringsAsFactors = FALSE)
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line", raw_data = raw_df,
    filename_prefix = "chart"
  )
  expect_false(is.null(entry$raw_data_table))
  restored <- favorites_table_as_df(entry$raw_data_table)
  expect_equal(restored$category, "X")
})

test_that("tc_history_capture stores favorite_download_id and module_id", {
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    filename_prefix = "my_chart", favorite_download_id = "favdl_xyz", module_id = "my_chart_dl"
  )
  expect_equal(entry$favorite_download_id, "favdl_xyz")
  expect_equal(entry$module_id, "my_chart_dl")
})

test_that("tc_history_capture records the dictionary_format flag and crosswalk", {
  on <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line", filename_prefix = "my_chart",
    dictionary_format = TRUE, dictionary_crosswalk = c(bedragwlzzin = "WLZ kosten")
  )
  off <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line", filename_prefix = "my_chart"
  )
  expect_true(on$dictionary_format)
  expect_equal(on$dictionary_crosswalk, list(bedragwlzzin = "WLZ kosten"))
  expect_false(off$dictionary_format)
  expect_null(off$dictionary_crosswalk)
})

test_that("tc_history_capture falls back to filename_prefix when no subtab_label is set", {
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "waterfall",
    filename_prefix = "my_chart"
  )
  expect_equal(entry$label, "my_chart")
  expect_true(is.na(entry$template_name))
})

test_that("export_history_redownload rebuilds a zip carrying the entry's chart_id", {
  skip_if_not(have_templates, "templates directory not available")
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  entry$id <- "exp_redownload_test"

  z <- tempfile(fileext = ".zip")
  export_history_redownload(entry, z, templates_dir = templates_dir, ppttc_exe = NA)

  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
  expect_true(grepl("download_id=exp_redownload_test", ppttc, fixed = TRUE))
  expect_true(grepl('"string":"exp_redownload_test"', ppttc, fixed = TRUE))
})

test_that("export_history_redownload includes a companion _raw.xlsx when the entry has raw_data_table", {
  skip_if_not(have_templates, "templates directory not available")
  raw_df <- data.frame(category = "X", value = 1, stringsAsFactors = FALSE)
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line", raw_data = raw_df,
    dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir
  )
  entry$id <- "exp_redownload_raw_test"

  z <- tempfile(fileext = ".zip")
  export_history_redownload(entry, z, templates_dir = templates_dir, ppttc_exe = NA)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("revenue_chart_table.xlsx" %in% files)
  expect_true("revenue_chart_raw.xlsx" %in% files)
})

test_that("export_history_redownload re-embeds a stored favorite_download_id", {
  skip_if_not(have_templates, "templates directory not available")
  entry <- tc_history_capture(
    tc_data = sample_matrix(), chart_type = "line",
    dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
    filename_prefix = "revenue_chart", templates_dir = templates_dir,
    favorite_download_id = "favdl_stored"
  )
  entry$id <- "exp_redownload_test2"

  z <- tempfile(fileext = ".zip")
  export_history_redownload(entry, z, templates_dir = templates_dir, ppttc_exe = NA)

  extract_dir <- tempfile("extract_")
  utils::unzip(z, exdir = extract_dir)
  ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
  expect_true(grepl('"FavoriteDownloadID"', ppttc))
  expect_true(grepl('"string":"favdl_stored"', ppttc, fixed = TRUE))
})

test_that("tc_history_entry_subtitle joins non-empty breadcrumb parts", {
  expect_equal(
    tc_history_entry_subtitle(list(dashboard_title = "D", tab_label = "T", subtab_label = "S")),
    "D / T / S"
  )
  expect_equal(
    tc_history_entry_subtitle(list(dashboard_title = "", tab_label = "T", subtab_label = "")),
    "T"
  )
  expect_equal(tc_history_entry_subtitle(list()), "")
})

test_that("export_history_group_rows groups entries sharing a favorite_download_id into one row", {
  entries <- list(
    list(id = "exp_1", created_at = "2026-08-01 09:00:00", favorite_download_id = "favdl_a"),
    list(id = "exp_2", created_at = "2026-08-01 09:00:01", favorite_download_id = "favdl_a"),
    list(id = "exp_3", created_at = "2026-08-02 10:00:00", favorite_download_id = NULL),
    list(id = "exp_4", created_at = "2026-08-03 11:00:00", favorite_download_id = "favdl_b")
  )
  rows <- export_history_group_rows(entries)

  expect_length(rows, 3)  # one group of 2 ("favdl_a"), one solo, one group of 1 ("favdl_b")
  kinds <- vapply(rows, function(r) r$kind, character(1))
  expect_equal(sum(kinds == "group"), 2)
  expect_equal(sum(kinds == "solo"), 1)

  group_a <- Find(function(r) identical(r$kind, "group") && identical(r$favorite_download_id, "favdl_a"), rows)
  expect_length(group_a$members, 2)
  # This is the exact regression this test guards: a naive which.max() on a
  # character vector of timestamps silently coerces to NA and errors on the
  # resulting empty index (this crashed the real Export History tab in
  # production once it had a genuine multi-member group).
  expect_equal(group_a$created_at, "2026-08-01 09:00:01")

  # Most-recent-first ordering, group and solo rows interleaved correctly.
  expect_equal(rows[[1]]$favorite_download_id, "favdl_b")
})

test_that("export_history_group_rows handles an empty entry list", {
  expect_equal(export_history_group_rows(list()), list())
})

# ---- Regenerate -------------------------------------------------------------
# fake_session() lives in helper-session.R (auto-sourced by testthat), shared
# with test-favorites.R's live-rebuild tests.

test_that("export_history_regenerate_entry falls back to a snapshot rebuild when the chart isn't registered", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir,
      module_id = "not_a_registered_module"
    )
    entry$id <- export_history_new_id()

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_entry(entry, z, fake_session(), templates_dir = templates_dir, ppttc_exe = NA)
    expect_false(res$live)

    history <- export_history_list()
    expect_length(history, 1)
    expect_false(identical(history[[1]]$id, entry$id))

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
    expect_true(grepl(paste0("download_id=", history[[1]]$id), ppttc, fixed = TRUE))
  })
})

test_that("export_history_regenerate_entry uses the live build function when the chart is registered", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir,
      module_id = "live_chart_dl"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    session <- fake_session()
    build_calls <- 0
    tc_chart_registry_register(session, "live_chart_dl", list(
      build_zip = function(zip_path, favorite_download_id = NULL, captured_image = NULL) {
        build_calls <<- build_calls + 1
        writeLines("live content", zip_path)
      },
      get_spec = function() stop("not used in this test")
    ))

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_entry(entry, z, session, templates_dir = templates_dir, ppttc_exe = NA)
    expect_true(res$live)
    expect_equal(build_calls, 1)
    expect_equal(readLines(z), "live content")
  })
})

test_that("export_history_regenerate_many with one entry delegates to export_history_regenerate_entry", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_many(list(entry), z, fake_session(), templates_dir = templates_dir, ppttc_exe = NA)
    expect_equal(res$total, 1)

    history <- export_history_list()
    expect_length(history, 2)
    ids <- vapply(history, function(e) e$id, character(1))
    expect_true(entry$id %in% ids)
    expect_true(any(ids != entry$id))
  })
})

test_that("export_history_regenerate_many with 2+ entries mints one fresh favorite_download_id shared by every member", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    e1 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart",
      templates_dir = templates_dir, favorite_download_id = "favdl_original"
    )
    e1$id <- export_history_new_id()
    export_history_add(e1)
    e2 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Cost", filename_prefix = "cost_chart",
      templates_dir = templates_dir, favorite_download_id = "favdl_original"
    )
    e2$id <- export_history_new_id()
    export_history_add(e2)

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_many(list(e1, e2), z, fake_session(), templates_dir = templates_dir, ppttc_exe = NA)
    expect_equal(res$total, 2)

    history <- export_history_list()
    expect_length(history, 4)
    # every regenerated entry shares one *new* favorite_download_id, distinct from the original
    regenerated <- Filter(function(e) {
      !is.null(e$favorite_download_id) && nzchar(e$favorite_download_id) &&
        !identical(e$favorite_download_id, "favdl_original")
    }, history)
    expect_length(regenerated, 2)
    expect_equal(length(unique(vapply(regenerated, function(e) e$favorite_download_id, character(1)))), 1)

    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("favorites_thinkcell_tables.xlsx" %in% files)
  })
})

test_that("export_history_snapshot_spec builds a spec from an entry's frozen snapshot without minting a new id", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", figure_title = "Revenue chart",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir,
      favorite_download_id = "favdl_x"
    )
    entry$id <- export_history_new_id()

    spec <- export_history_snapshot_spec(entry, templates_dir = templates_dir)
    expect_equal(spec$download_id, entry$id)
    expect_equal(spec$favorite_download_id, "favdl_x")
    expect_equal(spec$figure_title, "Revenue chart")
    expect_true(grepl(paste0("download_id=", entry$id), spec$datasheet_log, fixed = TRUE))
  })
})

test_that("export_history_snapshot_spec carries the entry's stored raw_data_table through as raw_table", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    raw_df <- data.frame(category = "X", value = 1, stringsAsFactors = FALSE)
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", raw_data = raw_df,
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir
    )
    entry$id <- export_history_new_id()

    spec <- export_history_snapshot_spec(entry, templates_dir = templates_dir)
    expect_false(is.null(spec$raw_table))
    expect_equal(spec$raw_table$category, "X")
  })
})

test_that("export_history_prepare_regenerate_spec pulls raw_table from the live chart's current raw_data", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir,
      module_id = "live_chart_raw_dl"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    session <- fake_session()
    live_raw <- data.frame(category = "Y", value = 2, stringsAsFactors = FALSE)
    tc_chart_registry_register(session, "live_chart_raw_dl", list(
      build_zip = function(...) stop("not used in this test"),
      get_spec = function() list(
        tc_data = sample_matrix(), raw_data = live_raw, chart_type = "line",
        slide_matrix = NULL, is_faceted = FALSE, slide_title = "", figure_title = "",
        template_override = "", slide_order = "auto", dashboard_title = "D",
        tab_label = "T", subtab_label = "Revenue", selections = list(),
        source_output = "", source_sheet = "", source_mtime = "",
        filename_prefix = "revenue_chart"
      )
    ))

    res <- export_history_prepare_regenerate_spec(entry, session, templates_dir = templates_dir)
    expect_true(res$live)
    expect_false(is.null(res$spec$raw_table))
    expect_equal(res$spec$raw_table$category, "Y")
  })
})

test_that("export_history_download_many with one entry is identical to export_history_redownload", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", templates_dir = templates_dir
    )
    entry$id <- export_history_new_id()

    z <- tempfile(fileext = ".zip")
    export_history_download_many(list(entry), z, templates_dir = templates_dir, ppttc_exe = NA)

    # No new history entries are logged by a redownload.
    expect_length(export_history_list(), 0)

    extract_dir <- tempfile("extract_")
    utils::unzip(z, exdir = extract_dir)
    ppttc <- paste(readLines(file.path(extract_dir, "chart_data.ppttc")), collapse = "\n")
    expect_true(grepl(paste0("download_id=", entry$id), ppttc, fixed = TRUE))
  })
})

test_that("export_history_download_many with 2+ entries combines them into one deck and logs nothing new", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    e1 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart",
      templates_dir = templates_dir
    )
    e1$id <- export_history_new_id()
    e2 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Cost", filename_prefix = "cost_chart",
      templates_dir = templates_dir
    )
    e2$id <- export_history_new_id()

    z <- tempfile(fileext = ".zip")
    export_history_download_many(list(e1, e2), z, templates_dir = templates_dir, ppttc_exe = NA)

    expect_length(export_history_list(), 0)
    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("favorites_thinkcell_tables.xlsx" %in% files)
  })
})

test_that("export_history_regenerate_excel_one falls back to a snapshot rebuild when the chart isn't registered", {
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", module_id = "not_a_registered_module"
    )
    entry$id <- export_history_new_id()

    res <- export_history_regenerate_excel_one(entry, fake_session())
    expect_false(res$live)
    expect_equal(res$filename_prefix, "revenue_chart")
    expect_true(grepl("^LOG \\|", names(res$data)[1]))

    history <- export_history_list()
    expect_length(history, 1)
    expect_false(identical(history[[1]]$id, entry$id))
  })
})

test_that("export_history_regenerate_excel_one uses the live get_spec callback when the chart is registered", {
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart", module_id = "live_chart_dl"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    session <- fake_session()
    live_data <- data.frame(lab = c("X", "Y"), `2023` = c(1, 2),
                            check.names = FALSE, stringsAsFactors = FALSE)
    names(live_data) <- c("", "2023")
    spec_calls <- 0
    tc_chart_registry_register(session, "live_chart_dl", list(
      build_zip = function(zip_path) stop("not used in this test"),
      get_spec = function() {
        spec_calls <<- spec_calls + 1
        list(
          tc_data = live_data, chart_type = "line", slide_matrix = NULL,
          is_faceted = FALSE, slide_title = "", figure_title = "Live Title",
          template_override = "", slide_order = "auto",
          dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
          selections = NULL, source_output = "", source_sheet = "",
          filename_prefix = "revenue_chart"
        )
      }
    ))

    res <- export_history_regenerate_excel_one(entry, session)
    expect_true(res$live)
    expect_equal(spec_calls, 1)
    expect_equal(as.character(res$data[[1]]), c("X", "Y"))

    history <- export_history_list()
    expect_length(history, 2)
    expect_true(any(vapply(history, function(e) identical(e$figure_title, "Live Title"), logical(1))))
  })
})

test_that("export_history_regenerate_excel_many with one entry writes a bare xlsx, no zip wrapper", {
  skip_if_not_installed("readxl")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line",
      dashboard_title = "D", tab_label = "T", subtab_label = "Revenue",
      filename_prefix = "revenue_chart"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    out <- tempfile(fileext = ".xlsx")
    res <- export_history_regenerate_excel_many(list(entry), out, fake_session())
    expect_equal(res$total, 1)
    expect_false(res$live_count == 1)  # not registered -> snapshot fallback

    df <- as.data.frame(readxl::read_excel(out, col_names = FALSE))
    expect_true(grepl("^LOG \\|", df[1, 1]))

    expect_length(export_history_list(), 2)
  })
})

test_that("export_history_regenerate_excel_many with 2+ entries writes one combined workbook, one sheet per entry, never a zip", {
  skip_if_not_installed("readxl")
  with_history_dir({
    e1 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart"
    )
    e1$id <- export_history_new_id()
    export_history_add(e1)
    e2 <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Cost", filename_prefix = "cost_chart"
    )
    e2$id <- export_history_new_id()
    export_history_add(e2)

    out <- tempfile(fileext = ".xlsx")
    res <- export_history_regenerate_excel_many(list(e1, e2), out, fake_session())
    expect_equal(res$total, 2)

    sheets <- readxl::excel_sheets(out)
    expect_equal(sheets, c("revenue_chart", "cost_chart"))

    expect_length(export_history_list(), 4)
  })
})

# ---- Captured-PNG asset helpers (§4: single-chart charts_overview.html) ----

test_that("export_history_asset_path is a sibling of export_history_dir", {
  with_history_dir({
    expect_equal(
      normalizePath(dirname(export_history_asset_path("abc123")), winslash = "/", mustWork = FALSE),
      normalizePath(export_history_assets_dir(), winslash = "/", mustWork = FALSE)
    )
    expect_equal(basename(export_history_asset_path("abc123")), "abc123.png")
  })
})

test_that("tc_write_captured_asset / tc_read_asset_as_data_uri round-trip a PNG", {
  with_history_dir({
    path <- export_history_asset_path("exp_test1")
    original <- as.raw(c(1, 2, 3, 255, 0))
    uri_in <- paste0("data:image/png;base64,", jsonlite::base64_enc(original))

    expect_true(tc_write_captured_asset(uri_in, path))
    expect_true(file.exists(path))

    uri_out <- tc_read_asset_as_data_uri(path)
    expect_true(grepl("^data:image/png;base64,", uri_out))
    roundtripped <- jsonlite::base64_dec(sub("^data:image/[^;]+;base64,", "", uri_out))
    expect_equal(roundtripped, original)
  })
})

test_that("tc_write_captured_asset is a no-op for NULL/empty/undecodable input", {
  with_history_dir({
    p1 <- export_history_asset_path("exp_null")
    expect_false(tc_write_captured_asset(NULL, p1))
    expect_false(file.exists(p1))

    p2 <- export_history_asset_path("exp_empty")
    expect_false(tc_write_captured_asset("", p2))
    expect_false(file.exists(p2))

    p3 <- export_history_asset_path("exp_garbage")
    expect_false(tc_write_captured_asset("data:image/png;base64,not-valid-base64!!!", p3))
  })
})

test_that("tc_read_asset_as_data_uri returns NULL for a missing file", {
  with_history_dir({
    expect_null(tc_read_asset_as_data_uri(export_history_asset_path("never_written")))
    expect_null(tc_read_asset_as_data_uri(NULL))
  })
})

test_that("tc_copy_asset copies an existing snapshot to a new id, no-ops if the source is missing", {
  with_history_dir({
    tc_write_captured_asset(
      paste0("data:image/png;base64,", jsonlite::base64_enc(as.raw(1:3))),
      export_history_asset_path("exp_from")
    )
    expect_true(tc_copy_asset("exp_from", "exp_to"))
    expect_true(file.exists(export_history_asset_path("exp_to")))

    expect_false(tc_copy_asset("exp_never_existed", "exp_to2"))
    expect_false(file.exists(export_history_asset_path("exp_to2")))
  })
})

# ---- Regenerate/redownload threading a captured/stored asset through -----

test_that("export_history_redownload includes the entry's own stored snapshot in the overview", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart",
      templates_dir = templates_dir
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)
    tc_write_captured_asset(
      paste0("data:image/png;base64,", jsonlite::base64_enc(as.raw(1:3))),
      export_history_asset_path(entry$id)
    )

    z <- tempfile(fileext = ".zip")
    export_history_redownload(entry, z, templates_dir = templates_dir, ppttc_exe = NA)
    files <- utils::unzip(z, list = TRUE)$Name
    expect_true("charts_overview.html" %in% files)
  })
})

test_that("export_history_regenerate_entry writes a fresh captured_image under the new chart_id", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart",
      templates_dir = templates_dir, module_id = "live_chart_dl2"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    session <- fake_session()
    tc_chart_registry_register(session, "live_chart_dl2", list(
      build_zip = function(zip_path, favorite_download_id = NULL, captured_image = NULL) {
        expect_true(grepl("^data:image/png;base64,", captured_image))
        writeLines("live content", zip_path)
      },
      get_spec = function() stop("not used in this test")
    ))

    fresh_uri <- paste0("data:image/png;base64,", jsonlite::base64_enc(as.raw(9:11)))
    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_entry(
      entry, z, session, templates_dir = templates_dir, ppttc_exe = NA, captured_image = fresh_uri
    )
    expect_true(res$live)
  })
})

test_that("export_history_regenerate_entry falls back to the old stored snapshot when no fresh capture is available", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart",
      templates_dir = templates_dir, module_id = "not_registered_this_time"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)
    tc_write_captured_asset(
      paste0("data:image/png;base64,", jsonlite::base64_enc(as.raw(1:3))),
      export_history_asset_path(entry$id)
    )

    z <- tempfile(fileext = ".zip")
    res <- export_history_regenerate_entry(entry, z, fake_session(), templates_dir = templates_dir, ppttc_exe = NA)
    expect_false(res$live)

    history <- export_history_list()
    new_id <- Filter(function(e) !identical(e$id, entry$id), history)[[1]]$id
    expect_true(file.exists(export_history_asset_path(new_id)))
  })
})

test_that("export_history_regenerate_many threads captures by module_id through to a solo regenerate", {
  skip_if_not(have_templates, "templates directory not available")
  with_history_dir({
    entry <- tc_history_capture(
      tc_data = sample_matrix(), chart_type = "line", dashboard_title = "D",
      tab_label = "T", subtab_label = "Revenue", filename_prefix = "revenue_chart",
      templates_dir = templates_dir, module_id = "live_chart_dl3"
    )
    entry$id <- export_history_new_id()
    export_history_add(entry)

    session <- fake_session()
    seen_image <- NULL
    tc_chart_registry_register(session, "live_chart_dl3", list(
      build_zip = function(zip_path, favorite_download_id = NULL, captured_image = NULL) {
        seen_image <<- captured_image
        writeLines("live content", zip_path)
      },
      get_spec = function() stop("not used in this test")
    ))

    fresh_uri <- paste0("data:image/png;base64,", jsonlite::base64_enc(as.raw(1:3)))
    z <- tempfile(fileext = ".zip")
    export_history_regenerate_many(
      list(entry), z, session, templates_dir = templates_dir, ppttc_exe = NA,
      captures = setNames(list(fresh_uri), "live_chart_dl3")
    )
    expect_equal(seen_image, fresh_uri)
  })
})
