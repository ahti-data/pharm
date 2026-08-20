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

test_that("dictionary_scope_label uses a curated label when one exists, else a generic prettified fallback", {
  expect_equal(dictionary_scope_label(""), "Overig (geen scope)")
  expect_equal(dictionary_scope_label("some_new_scope"), "Some New Scope")
})

test_that("dictionary_scope_order puts curated scopes first, in their own order, then unknown ones alphabetically", {
  old <- DICTIONARY_SCOPE_LABELS
  DICTIONARY_SCOPE_LABELS <<- c(stats::setNames("Overig", ""), sheet = "Sheets", age_cat = "Leeftijd")
  on.exit(DICTIONARY_SCOPE_LABELS <<- old, add = TRUE)

  ordered <- dictionary_scope_order(c("zeta", "age_cat", "alpha", "sheet"))
  expect_equal(ordered, c("sheet", "age_cat", "alpha", "zeta"))
})

test_that("adding a new entry through the module persists it to the dictionary", {
  with_dictionary_path({
    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      session$setInputs(add = 1)
      session$setInputs(edit_raw_key = "wlz", edit_scope = "sheet", edit_pretty_label = "WLZ")
      session$setInputs(save = 1)

      expect_equal(dictionary_lookup("wlz", "sheet"), "WLZ")
      expect_true(isTRUE(status_rv$ok))
      expect_match(status_rv$message, "Saved")
    })
  })
})

test_that("saving with a missing raw name or pretty label is rejected without writing anything", {
  with_dictionary_path({
    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      session$setInputs(add = 1)
      session$setInputs(edit_raw_key = "", edit_scope = "", edit_pretty_label = "Something")
      session$setInputs(save = 1)

      expect_false(isTRUE(status_rv$ok))
      expect_equal(dictionary_list(), list())
    })
  })
})

test_that("editing a row's inline pretty-label field and clicking its Save button updates the entry in place, with no modal involved", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      # Force the entries list (and its dynamic per-row Save/Delete
      # observers, registered by a separate shiny::observe() block) to
      # materialize before simulating a click on one of those buttons --
      # session$flushReact() runs the whole reactive graph, not just the one
      # reactive referenced directly below.
      entries <- filtered_entries()
      expect_length(entries, 1)
      session$flushReact()
      row_id <- dict_entry_ui_id("wlz", "sheet")

      do.call(session$setInputs, setNames(list("WLZ (bijgewerkt)"), paste0("pretty_", row_id)))
      do.call(session$setInputs, setNames(list(1), paste0("save_", row_id)))

      expect_equal(dictionary_lookup("wlz", "sheet"), "WLZ (bijgewerkt)")
      expect_true(isTRUE(status_rv$ok))
      expect_match(status_rv$message, "Saved")
    })
  })
})

test_that("saving a row's inline field with an empty pretty label is rejected without writing anything", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      filtered_entries()
      session$flushReact()
      row_id <- dict_entry_ui_id("wlz", "sheet")

      do.call(session$setInputs, setNames(list(""), paste0("pretty_", row_id)))
      do.call(session$setInputs, setNames(list(1), paste0("save_", row_id)))

      expect_equal(dictionary_lookup("wlz", "sheet"), "WLZ")
      expect_false(isTRUE(status_rv$ok))
    })
  })
})

test_that("clicking a row's inline Delete button removes it, with no modal involved", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      filtered_entries()
      session$flushReact()
      row_id <- dict_entry_ui_id("wlz", "sheet")

      do.call(session$setInputs, setNames(list(1), paste0("delete_", row_id)))

      expect_null(dictionary_lookup("wlz", "sheet"))
      expect_true(isTRUE(status_rv$ok))
      expect_match(status_rv$message, "Removed")
    })
  })
})

test_that("opening a category with several entries makes every one of them editable at once, no per-entry click-to-reveal step", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")
    dictionary_set_entry("zvw", "sheet", "ZVW")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      entries <- filtered_entries()
      expect_length(entries, 2)
      session$flushReact()

      # Both rows' Save buttons must already be registered/usable
      # immediately -- neither requires an "Edit" click first.
      wlz_row <- dict_entry_ui_id("wlz", "sheet")
      zvw_row <- dict_entry_ui_id("zvw", "sheet")
      do.call(session$setInputs, setNames(list("WLZ 2"), paste0("pretty_", wlz_row)))
      do.call(session$setInputs, setNames(list(1), paste0("save_", wlz_row)))
      do.call(session$setInputs, setNames(list("ZVW 2"), paste0("pretty_", zvw_row)))
      do.call(session$setInputs, setNames(list(1), paste0("save_", zvw_row)))

      expect_equal(dictionary_lookup("wlz", "sheet"), "WLZ 2")
      expect_equal(dictionary_lookup("zvw", "sheet"), "ZVW 2")
    })
  })
})

test_that("the search box filters the entries list by raw name, scope, or label", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")
    dictionary_set_entry("zvw", "sheet", "ZVW")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      expect_length(filtered_entries(), 2)

      session$setInputs(search = "zvw")
      expect_length(filtered_entries(), 1)
      expect_equal(filtered_entries()[[1]]$raw_key, "zvw")

      session$setInputs(search = "")
      expect_length(filtered_entries(), 2)
    })
  })
})
