# Mounts the REAL dashboard server and checks every chart it registers can
# rebuild from a favorite/history entry's stored options.
#
# This is the regression guard for the whole class of bug: a chart added or
# edited without `data_for` wired still looks completely fine in the UI (its
# stored options display correctly in Favorites) while silently downloading
# whatever the live inputs hold. Only a mechanical check catches that, so this
# test deliberately exercises the actual app rather than a fixture.

app_dir <- normalizePath(file.path("..", ".."), mustWork = FALSE)

# Mounting the real server touches every piece of shared runtime state the app
# owns (dictionary, favorites, export history, template uploads), so point all
# of it at throwaway dirs for the duration -- these test files share one R
# session, and otherwise this one seeds state the others then assert against.
#
# It also has a side effect no env var covers: app.R sources
# data/metadata/dictionary_seed.R, which *redefines* dictionary_seed_entries()
# in the global environment (utils/ and app.R both load into globalenv -- see
# CLAUDE.md). That redefinition outlives the test and makes every later
# dictionary_read() seed pharm's real entries, so snapshot and restore it too.
with_isolated_app_state <- function(code) {
  d <- tempfile(); dir.create(d)
  vars <- c(SHINY_DICTIONARY_PATH = file.path(d, "dictionary.json"),
            SHINY_FAVORITES_PATH = file.path(d, "favorites.json"),
            SHINY_EXPORT_HISTORY_DIR = file.path(d, "export_history"),
            SHINY_TEMPLATE_UPLOADS_DIR = file.path(d, "template_uploads"))
  old <- Sys.getenv(names(vars), unset = NA)
  do.call(Sys.setenv, as.list(vars))

  had_seed <- exists("dictionary_seed_entries", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get("dictionary_seed_entries", envir = globalenv()) else NULL

  on.exit({
    for (k in names(old)) {
      if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, stats::setNames(list(old[[k]]), k))
    }
    if (had_seed) {
      assign("dictionary_seed_entries", old_seed, envir = globalenv())
    } else if (exists("dictionary_seed_entries", envir = globalenv(), inherits = FALSE)) {
      rm("dictionary_seed_entries", envir = globalenv())
    }
  }, add = TRUE)
  force(code)
}

test_that("every chart in the real app can rebuild from stored options", {
  skip_if_not(file.exists(file.path(app_dir, "app.R")), "app.R not found")
  skip_if_not(dir.exists(file.path(app_dir, "data")), "data/ not available")

  # testServer() evaluates its expression inside the server function's own
  # environment, so the charts register into this session exactly as they do in
  # production -- no fixture can go stale relative to app.R.
  with_isolated_app_state({
    shiny::testServer(app_dir, {
      session$flushReact()
      ids <- tc_chart_registry_ids(session)
      expect_true(length(ids) > 0)
      # Fails with the offending chart id(s) named if a data_for is ever dropped.
      expect_silent(tc_assert_replayable(session))
    })
  })
})

test_that("a real chart rebuilds different data for different stored options", {
  # End-to-end proof against real data: same chart module, two stored option
  # sets, one live input state -> two genuinely different exports.
  skip_if_not(file.exists(file.path(app_dir, "app.R")), "app.R not found")
  skip_if_not(dir.exists(file.path(app_dir, "data")), "data/ not available")

  with_isolated_app_state({
    shiny::testServer(app_dir, {
      session$setInputs(ggz_dim = "leeftijd", ggz_metric = "totale_kosten")
      session$flushReact()

      reg <- tc_chart_registry_get(session, "ggz_kosten_dl")
      skip_if(is.null(reg), "ggz_kosten_dl not registered")

      live  <- reg$get_spec()
      other <- reg$get_spec(list(ggz_dim = "leeftijd", ggz_metric = "gebruikers"))

      # A different stored metric must yield a different figure, even though the
      # live inputs never changed.
      expect_false(identical(live$tc_data, other$tc_data))
      expect_false(identical(live$figure_title, other$figure_title))
      # And the spec reports the options it actually used.
      expect_equal(other$selections$ggz_metric, "gebruikers")
      expect_equal(live$selections$ggz_metric, "totale_kosten")

      for (m in c("totale_kosten", "gebruikers", "gem_kosten_per_gebr", "prevalentie")) {
        expect_equal(reg$get_spec(list(ggz_dim = "leeftijd", ggz_metric = m))$selections$ggz_metric, m)
      }
    })
  })
})

test_that("a replayed spec is identical no matter what the live inputs hold", {
  # THE invariant, and the only assertion that reliably catches a live-input
  # read hiding inside an option-specific branch: rebuilding the *same* stored
  # option set must give the *same* result regardless of the session's current
  # inputs. Checking merely that different stored options give different data
  # is not enough -- a leaked `input$ggz_metric` still yields distinct (just
  # wrong) values and sails through, which is exactly what happened when an
  # upstream change to this chart was merged into the replay refactor.
  skip_if_not(file.exists(file.path(app_dir, "app.R")), "app.R not found")
  skip_if_not(dir.exists(file.path(app_dir, "data")), "data/ not available")

  with_isolated_app_state({
    shiny::testServer(app_dir, {
      reg <- tc_chart_registry_get(session, "ggz_kosten_dl")
      skip_if(is.null(reg), "ggz_kosten_dl not registered")

      metrics <- c("totale_kosten", "gebruikers", "gem_kosten_per_gebr", "prevalentie")

      # For each stored metric, rebuild it twice: once while the live input
      # happens to MATCH it, once while it doesn't. A branch that tests the
      # live input instead of the replayed value flips between those two runs,
      # so the two results diverge -- whereas correct code is identical. (Two
      # live states that both differ from the stored one would agree even with
      # the bug, which is why one of them must be the stored value itself.)
      for (m in metrics) {
        stored <- list(ggz_dim = "leeftijd", ggz_metric = m)
        other  <- setdiff(metrics, m)[[1]]

        session$setInputs(ggz_dim = "leeftijd", ggz_metric = m)
        session$flushReact()
        matching <- reg$get_spec(stored)

        session$setInputs(ggz_dim = "totaal", ggz_metric = other)
        session$flushReact()
        differing <- reg$get_spec(stored)

        expect_equal(matching$tc_data, differing$tc_data, info = paste("metric:", m))
        expect_equal(matching$figure_title, differing$figure_title, info = paste("metric:", m))
        expect_equal(matching$selections, differing$selections, info = paste("metric:", m))
      }
    })
  })
})
