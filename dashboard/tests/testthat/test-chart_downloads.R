library(testthat)

with_dictionary_path <- function(code) {
  path <- tempfile("dictionary_", fileext = ".json")
  old <- Sys.getenv("SHINY_DICTIONARY_PATH", unset = NA)
  Sys.setenv(SHINY_DICTIONARY_PATH = path)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_DICTIONARY_PATH") else Sys.setenv(SHINY_DICTIONARY_PATH = old)
    unlink(path)
  }, add = TRUE)
  force(code)
}

sample_data <- function() {
  data.frame(
    quarter = rep(c("Q1", "Q2"), each = 2),
    product = rep(c("Product A", "Product B"), 2),
    revenue = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

test_that("the 'Format from dictionary' checkbox toggles category/series relabeling for downloads", {
  with_dictionary_path({
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")
    dictionary_set_entry("Product A", "product", "Product Alpha")

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      df_on <- data()
      expect_setequal(unique(df_on$quarter), c("Kwartaal 1", "Q2"))
      expect_setequal(unique(df_on$product), c("Product Alpha", "Product B"))

      session$setInputs(dictionary_format = FALSE)
      df_off <- data()
      expect_setequal(unique(df_off$quarter), c("Q1", "Q2"))
      expect_setequal(unique(df_off$product), c("Product A", "Product B"))
    })
  })
})

test_that("category_order/series_order are relabeled to match the checkbox, so factor levels still line up", {
  with_dictionary_path({
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")
    dictionary_set_entry("Q2", "quarter", "Kwartaal 2")

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL,
      category_order = c("Q2", "Q1")
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_equal(resolved_category_order(), c("Kwartaal 2", "Kwartaal 1"))

      session$setInputs(dictionary_format = FALSE)
      expect_equal(resolved_category_order(), c("Q2", "Q1"))
    })
  })
})

test_that("category_scope/series_scope default to the column name, and an explicit scope overrides it", {
  with_dictionary_path({
    # Entry stored under scope "quarter" -- matches the column name used below,
    # so the default (no explicit category_scope) finds it.
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_equal(scope_or_col(NULL, "quarter"), "quarter")
      expect_true("Kwartaal 1" %in% data()$quarter)
    })

    # Same raw values, but the chart's column is generically named "category"
    # -- omitting category_scope would default to scope "category" and miss
    # every entry stored under "quarter"; passing category_scope = "quarter"
    # explicitly recovers the intended lookup. This is exactly the pitfall
    # CLAUDE.md's chart_data_downloads_server() convention warns about.
    generic_data <- function() {
      df <- sample_data()
      names(df)[names(df) == "quarter"] <- "category"
      df
    }

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart_generic",
      data = shiny::reactive(generic_data()),
      chart_type = "stacked_bar",
      category_col = "category",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_false("Kwartaal 1" %in% data()$category)
    })

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart_generic_scoped",
      data = shiny::reactive(generic_data()),
      chart_type = "stacked_bar",
      category_col = "category",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL,
      category_scope = "quarter"
    ), {
      session$setInputs(dictionary_format = TRUE)
      expect_true("Kwartaal 1" %in% data()$category)
    })
  })
})

test_that("a value with no matching dictionary entry passes through unchanged, not through the generic fallback prettifier", {
  with_dictionary_path({
    # No dictionary_set_entry() calls at all -- every value below is a miss.
    already_pretty <- function() {
      data.frame(
        quarter = c("18-29 jaar", "30-39 jaar"),
        product = c("Product A", "Product B"),
        revenue = c(10, 20),
        stringsAsFactors = FALSE
      )
    }

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(already_pretty()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL,
      category_order = c("18-29 jaar", "30-39 jaar")
    ), {
      session$setInputs(dictionary_format = TRUE)
      # dictionary_default_prettify() would have turned this into
      # "18 29 Jaar" (hyphen -> space, "jaar" wrongly capitalized) -- the
      # identity fallback must leave it exactly as given instead.
      expect_setequal(unique(data()$quarter), c("18-29 jaar", "30-39 jaar"))
      expect_equal(resolved_category_order(), c("18-29 jaar", "30-39 jaar"))
    })
  })
})

test_that("relabeling an ordered factor column relabels its levels too, instead of silently losing the order", {
  with_dictionary_path({
    dictionary_set_entry("Q2", "quarter", "Kwartaal 2")
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")

    # An ordered factor, exactly like `stats::reorder(name, value)` produces
    # for a chart whose export needs to match the plot's own bar order --
    # deliberately factor levels in the OPPOSITE order from alphabetical,
    # so a silent fall-back to character/alphabetical order would be caught.
    ordered_data <- function() {
      data.frame(
        quarter = factor(c("Q2", "Q1"), levels = c("Q2", "Q1")),
        product = c("Product A", "Product B"),
        revenue = c(10, 20),
        stringsAsFactors = FALSE
      )
    }

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(ordered_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      relabeled <- data()$quarter
      expect_true(is.factor(relabeled))
      expect_equal(levels(relabeled), c("Kwartaal 2", "Kwartaal 1"))
      expect_equal(as.character(relabeled), c("Kwartaal 2", "Kwartaal 1"))
    })
  })
})

with_history_dir_local <- function(code) {
  dir <- tempfile("export_history_")
  old <- Sys.getenv("SHINY_EXPORT_HISTORY_DIR", unset = NA)
  Sys.setenv(SHINY_EXPORT_HISTORY_DIR = dir)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_EXPORT_HISTORY_DIR") else Sys.setenv(SHINY_EXPORT_HISTORY_DIR = old)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)
  force(code)
}

# chart_data_downloads_server() has no templates_dir override -- its
# register_slide gate (and everything inside it, including output$slide)
# only exists when tc_template_available() can resolve a real template via
# tc_find_templates_dir(), which searches APP_ROOT/getwd() -- neither of
# which is the repo root while tests run from tests/testthat/. Defining
# APP_ROOT here (as a real deployed app.R does) is the only way to reach
# that branch in a test.
with_app_root_local <- function(code) {
  had_root <- exists("APP_ROOT", envir = globalenv())
  old_root <- if (had_root) get("APP_ROOT", envir = globalenv()) else NULL
  assign("APP_ROOT", normalizePath(file.path("..", "..")), envir = globalenv())
  on.exit({
    if (had_root) assign("APP_ROOT", old_root, envir = globalenv()) else rm("APP_ROOT", envir = globalenv())
  }, add = TRUE)
  force(code)
}

test_that("build_export_now() reuses a pre-minted chart_id_override instead of generating a fresh one", {
  # This is the plumbing output$slide's filename() and content() both rely
  # on: filename() (resolved by Shiny before content() runs) reads
  # pending_slide_id() to embed the same id build_export_now() below ends
  # up logging to Export History -- see the observeEvent(input$slide_capture)
  # that mints it in utils/chart_downloads.R.
  skip_if_not(dir.exists(file.path("..", "..", "templates")), "templates directory not available")
  with_app_root_local({
  with_history_dir_local({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      pending_slide_id("preminted_id_123")

      # build_export_now() writes the history entry (and mints/reuses
      # chart_id) before it ever touches utils::zip() -- so this still
      # verifies the id override regardless of whether zip is installed on
      # the machine running the test.
      zip_path <- tempfile(fileext = ".zip")
      suppressWarnings(build_export_now(zip_path, chart_id_override = pending_slide_id()))

      entries <- export_history_list()
      expect_length(entries, 1)
      expect_equal(entries[[1]]$id, "preminted_id_123")
    })
  })
  })
})

test_that("the raw Excel download is the exact unpivoted plotting data frame, never the pivoted think-cell matrix", {
  skip_if_not_installed("readxl")
  shiny::testServer(chart_data_downloads_server, args = list(
    id = "test_chart",
    data = shiny::reactive(sample_data()),
    chart_type = "stacked_bar",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL
  ), {
    raw_df <- as.data.frame(readxl::read_excel(output$raw))
    # Long shape: the original columns, one row per plotted point -- NOT a
    # pivoted matrix, and no corner-cell log (raw is just the data).
    expect_equal(names(raw_df), c("quarter", "product", "revenue"))
    expect_equal(nrow(raw_df), nrow(sample_data()))
    expect_false(grepl("^LOG \\|", names(raw_df)[[1]]))
  })
})

test_that("the raw Excel download stays unpivoted even for a chart type think-cell doesn't support", {
  skip_if_not_installed("readxl")
  shiny::testServer(chart_data_downloads_server, args = list(
    id = "test_chart",
    data = shiny::reactive(sample_data()),
    chart_type = "scatter",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL
  ), {
    raw_df <- as.data.frame(readxl::read_excel(output$raw))
    expect_equal(names(raw_df), c("quarter", "product", "revenue"))
    expect_equal(nrow(raw_df), nrow(sample_data()))
  })
})

test_that("the think-cell download uses the slide-embedded orientation (categories across the header), the transpose of format_tc_data's tc_data", {
  skip_if_not_installed("readxl")
  with_dictionary_path({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      tc_df <- as.data.frame(readxl::read_excel(output$thinkcell), check.names = FALSE)
      hdr <- names(tc_df)
      # Corner cell carries the provenance log.
      expect_true(grepl("^LOG \\|", hdr[[1]]))
      # Bar-family charts embed the matrix with the CATEGORIES across the
      # header (slide_matrix orientation) -- so the download pastes straight
      # into the slide datasheet. The series ("Product A/B") are row labels,
      # NOT column headers (which would be format_tc_data's tc_data
      # orientation this download used to write).
      expect_setequal(hdr[-1], c("Q1", "Q2"))
      expect_false(any(c("Product A", "Product B") %in% hdr))
    })
  })
})

test_that("the think-cell download's corner cell includes the dictionary crosswalk when the checkbox is on, and omits it when off", {
  skip_if_not_installed("readxl")
  with_dictionary_path({
    dictionary_set_entry("Q1", "quarter", "Kwartaal 1")

    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      session$setInputs(dictionary_format = TRUE)
      on_path <- output$thinkcell
      header_on <- names(readxl::read_excel(on_path, n_max = 0))
      expect_true(grepl("dictionary=Q1->Kwartaal 1", header_on[[1]], fixed = TRUE))

      session$setInputs(dictionary_format = FALSE)
      off_path <- output$thinkcell
      header_off <- names(readxl::read_excel(off_path, n_max = 0))
      expect_false(grepl("dictionary=", header_off[[1]], fixed = TRUE))
    })
  })
})

test_that("the single-chart think-cell download's file-name id matches the download_id in its corner-cell log", {
  skip_if_not_installed("readxl")
  with_dictionary_path({
    shiny::testServer(chart_data_downloads_server, args = list(
      id = "test_chart",
      data = shiny::reactive(sample_data()),
      chart_type = "stacked_bar",
      category_col = "quarter",
      series_col = "product",
      value_col = "revenue",
      agg_fun = NULL
    ), {
      # filename() (run here, before content()) mints the id and stashes it in
      # pending_thinkcell_id(); content() then stamps that SAME id into the
      # workbook's A1 log -- so the id in the file name and the id in the log
      # always agree.
      p <- output$thinkcell
      id <- pending_thinkcell_id()
      expect_true(nzchar(id))
      header <- names(readxl::read_excel(p, n_max = 0))
      expect_true(grepl(paste0("download_id=", id), header[[1]], fixed = TRUE))
    })
  })
})

test_that("the download panel shows a 'Source data updated' line from source_mtime, and nothing when it is unset", {
  shiny::testServer(chart_data_downloads_server, args = list(
    id = "test_chart",
    data = shiny::reactive(sample_data()),
    chart_type = "stacked_bar",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL,
    source_mtime = "2026-08-14 09:30:00"
  ), {
    html <- as.character(output$source_updated_info)
    expect_true(any(grepl("Source data updated", html)))
    expect_true(any(grepl("2026-08-14 09:30:00", html, fixed = TRUE)))
  })

  # No source_mtime wired (e.g. a chart from inline/synthetic data): the line
  # renders nothing at all.
  shiny::testServer(chart_data_downloads_server, args = list(
    id = "test_chart",
    data = shiny::reactive(sample_data()),
    chart_type = "stacked_bar",
    category_col = "quarter",
    series_col = "product",
    value_col = "revenue",
    agg_fun = NULL
  ), {
    html <- as.character(output$source_updated_info)
    expect_false(any(grepl("Source data updated", html)))
  })
})
