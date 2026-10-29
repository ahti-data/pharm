library(testthat)

# Covers the ported cardio4cities feature: chart_data_downloads_ui()'s
# "Include N"/"Include Aantal"/"Opslaan met N=" checkboxes
# (offer_n/offer_count on the UI, n_col/count_col on the server), and how a
# favorite replays them later via tc_registry_spec_with_n() and
# favorites_with_extra_rows() -- see utils/chart_downloads.R,
# utils/slide_download.R and utils/favorites.R.

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

templates_dir_rel <- file.path("..", "..", "templates")
have_templates <- dir.exists(templates_dir_rel)

# chart_data_downloads_server()'s favorite button/checkboxes and
# build_export_spec() only exist inside its register_slide gate, which needs
# a real template to resolve via tc_find_templates_dir() (APP_ROOT/getwd()) --
# neither is the repo root while tests run from tests/testthat/. See the
# identical helper + comment in test-chart_downloads.R.
with_app_root_local <- function(code) {
  had_root <- exists("APP_ROOT", envir = globalenv())
  old_root <- if (had_root) get("APP_ROOT", envir = globalenv()) else NULL
  assign("APP_ROOT", normalizePath(file.path("..", "..")), envir = globalenv())
  on.exit({
    if (had_root) assign("APP_ROOT", old_root, envir = globalenv()) else rm("APP_ROOT", envir = globalenv())
  }, add = TRUE)
  force(code)
}

sample_data_n <- function() {
  data.frame(
    subgroep = rep(c("18-34", "35-49"), each = 2),
    categorie = rep(c("bg", "sg"), 2),
    waarde = c(5, 7, 9, 11),
    n_pop = c(100, 100, 200, 200),
    aantal = c(5, 7, 9, 11),
    stringsAsFactors = FALSE
  )
}

test_that("build_export_spec only builds n_table/count_table when asked, and leaves tc_data alone", {
  skip_if_not(have_templates, "templates directory not available")
  with_app_root_local({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_n",
      data = shiny::reactive(sample_data_n()),
      chart_type = "grouped_bar",
      category_col = "subgroep",
      series_col = "categorie",
      value_col = "waarde",
      agg_fun = NULL,
      n_col = "n_pop",
      count_col = "aantal"
    ), {
      plain <- build_export_spec()
      expect_null(plain$n_table)
      expect_null(plain$count_table)

      with_n <- build_export_spec(NULL, include_n = TRUE)
      expect_false(is.null(with_n$n_table))
      expect_true(all(c("bg", "sg") %in% names(with_n$n_table)))
      expect_identical(names(with_n$n_table), names(with_n$tc_data))
      expect_null(with_n$count_table)

      with_both <- build_export_spec(NULL, include_n = TRUE, include_count = TRUE)
      expect_false(is.null(with_both$count_table))
      expect_identical(names(with_both$count_table), names(with_both$tc_data))
    })
  })
})

test_that("'Opslaan met N=' formats every value cell as literal 'N=<number>' text", {
  skip_if_not(have_templates, "templates directory not available")
  with_app_root_local({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_n_equals",
      data = shiny::reactive(sample_data_n()),
      chart_type = "grouped_bar",
      category_col = "subgroep",
      series_col = "categorie",
      value_col = "waarde",
      agg_fun = NULL,
      n_col = "n_pop"
    ), {
      spec <- build_export_spec(NULL, include_n = TRUE, n_equals = TRUE)
      value_cols <- setdiff(names(spec$n_table), names(spec$n_table)[1])
      vals <- unlist(spec$n_table[value_cols])
      expect_true(all(grepl("^N=[0-9]+$", vals)))
    })
  })
})

test_that("include_n/include_count are no-ops when n_col/count_col aren't wired", {
  skip_if_not(have_templates, "templates directory not available")
  with_app_root_local({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_no_n",
      data = shiny::reactive(sample_data_n()),
      chart_type = "grouped_bar",
      category_col = "subgroep",
      series_col = "categorie",
      value_col = "waarde",
      agg_fun = NULL
    ), {
      spec <- build_export_spec(NULL, include_n = TRUE, include_count = TRUE)
      expect_null(spec$n_table)
      expect_null(spec$count_table)
    })
  })
})

test_that("checking 'Include N'/'Include Aantal'/'Opslaan met N=' and starring persists them on the favorite", {
  skip_if_not(have_templates, "templates directory not available")
  with_app_root_local({
  with_favorites_path({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_star",
      data = shiny::reactive(sample_data_n()),
      chart_type = "grouped_bar",
      category_col = "subgroep",
      series_col = "categorie",
      value_col = "waarde",
      agg_fun = NULL,
      n_col = "n_pop",
      count_col = "aantal"
    ), {
      session$setInputs(
        favorite_include_n = TRUE,
        favorite_include_count = TRUE,
        favorite_n_equals = TRUE
      )
      session$setInputs(favorite = 1)
    })

    entries <- favorites_list()
    expect_length(entries, 1)
    expect_true(entries[[1]]$include_n)
    expect_true(entries[[1]]$include_count)
    expect_true(entries[[1]]$n_equals)
  })
  })
})

test_that("a favorite starred with no N/Aantal checkbox persists them as FALSE", {
  skip_if_not(have_templates, "templates directory not available")
  with_app_root_local({
  with_favorites_path({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_star_plain",
      data = shiny::reactive(sample_data_n()),
      chart_type = "grouped_bar",
      category_col = "subgroep",
      series_col = "categorie",
      value_col = "waarde",
      agg_fun = NULL,
      n_col = "n_pop"
    ), {
      session$setInputs(favorite = 1)
    })

    entries <- favorites_list()
    expect_length(entries, 1)
    expect_false(isTRUE(entries[[1]]$include_n))
    expect_false(isTRUE(entries[[1]]$include_count))
    expect_false(isTRUE(entries[[1]]$n_equals))
  })
  })
})

test_that("tc_accepts_n_flags detects include_n/include_count/n_equals formals", {
  expect_true(tc_accepts_n_flags(function(selections = NULL, include_n = FALSE, include_count = FALSE, n_equals = FALSE) NULL))
  expect_true(tc_accepts_n_flags(function(...) NULL))
  expect_false(tc_accepts_n_flags(function(selections = NULL) NULL))
  expect_false(tc_accepts_n_flags("not a function"))
})

test_that("tc_registry_spec_with_n passes the flags through when get_spec supports them, else falls back", {
  seen <- NULL
  reg_full <- list(get_spec = function(selections = NULL, include_n = FALSE, include_count = FALSE, n_equals = FALSE) {
    seen <<- list(selections = selections, include_n = include_n, include_count = include_count, n_equals = n_equals)
    list(ok = TRUE)
  })
  out <- tc_registry_spec_with_n(reg_full, list(dim = "x"), include_n = TRUE, include_count = FALSE, n_equals = TRUE)
  expect_true(out$ok)
  expect_equal(seen$selections, list(dim = "x"))
  expect_true(seen$include_n)
  expect_true(seen$n_equals)

  # A narrower get_spec (no include_n/include_count/n_equals formals) --
  # falls back to the plain selections-only call, same as tc_registry_spec().
  reg_narrow <- list(get_spec = function(selections = NULL) list(got = selections))
  out2 <- tc_registry_spec_with_n(reg_narrow, list(dim = "y"), include_n = TRUE)
  expect_equal(out2$got, list(dim = "y"))
})

test_that("favorites_with_extra_rows appends Aantal then N rows, prefixed, aligned by column", {
  table <- data.frame(category = "serie", A = 1, B = 2, check.names = FALSE)
  spec <- list(
    n_table = data.frame(category = "serie", A = 100, B = 200, check.names = FALSE),
    count_table = data.frame(category = "serie", A = 10, B = 20, check.names = FALSE)
  )
  out <- favorites_with_extra_rows(table, spec)
  expect_equal(nrow(out), 3)
  expect_equal(out[[1]], c("serie", "Aantal: serie", "N: serie"))
})

test_that("favorites_with_extra_rows leaves the table unchanged when there's nothing to add, or columns don't align", {
  table <- data.frame(category = "serie", A = 1, B = 2, check.names = FALSE)
  expect_identical(favorites_with_extra_rows(table, list()), table)

  misaligned <- list(n_table = data.frame(category = "serie", A = 100, check.names = FALSE))
  expect_identical(favorites_with_extra_rows(table, misaligned), table)
})

test_that("a favorite starred with Include N replays the N row on every later download, never into the slide's own table", {
  with_favorites_path({
    session <- fake_session()
    # A realistic think-cell-pivoted shape: one row per series, categories as
    # columns -- the same shape tc_data/n_table share in a real spec (see
    # build_export_spec()'s build_aux_table()), so the "N: " row appended
    # below is exactly one extra row, same as a real favorite's workbook.
    spec <- list(
      tc_data = data.frame(category = "Totaal", `18-34` = 5, `35-49` = 9, check.names = FALSE),
      raw_data = sample_data_n(),
      chart_type = "bar",
      slide_matrix = NULL,
      is_faceted = FALSE,
      slide_title = "", figure_title = "Test chart",
      template_override = "", slide_order = "auto",
      dashboard_title = "D", tab_label = "T", subtab_label = "S",
      selections = list(),
      source_output = "", source_sheet = "", source_mtime = "",
      filename_prefix = "test",
      n_table = data.frame(category = "Totaal", `18-34` = 100, `35-49` = 200, check.names = FALSE),
      count_table = NULL
    )
    get_spec <- function(selections = NULL, include_n = FALSE, include_count = FALSE, n_equals = FALSE) {
      if (!isTRUE(include_n)) {
        out <- spec
        out$n_table <- NULL
        return(out)
      }
      spec
    }
    tc_chart_registry_register(session, "n_dl", list(
      build_zip = function(...) stop("unused"), get_spec = get_spec
    ))

    entry <- favorites_capture(
      chart_type = "bar", module_id = "n_dl", filename_prefix = "test",
      include_n = TRUE
    )
    favorites_add(entry)

    # templates_dir points nowhere so no template resolves -- keeps this test
    # focused on tc_sheet/tc_table, with no Export History side effects to
    # sandbox (see test-selection-replay.R for the templated case).
    out <- favorites_prepare_live_spec(favorites_list()[[1]], session, templates_dir = tempfile())
    expect_equal(nrow(out$tc_sheet), 2)
    expect_true(any(grepl("^N: ", out$tc_sheet[[1]])))
    # The slide's own embedded table never gets the extra row.
    expect_equal(nrow(out$tc_table), 1)
    expect_false(any(grepl("^N: ", out$tc_table[[1]])))
  })
})
