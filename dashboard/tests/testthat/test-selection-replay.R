# Guards the bug where two favorites (or history entries) of the SAME chart,
# saved with DIFFERENT options, both downloaded identical data -- because the
# rebuild read the session's live inputs instead of each entry's own stored
# `selections`. See `data_for` on chart_data_downloads_server().

# favorites_prepare_live_spec() only logs a history entry when a real template
# resolves for the chart type, so the history-logging tests below need the
# repo's templates/ dir (same convention as test-favorites.R).
replay_templates_dir <- file.path("..", "..", "templates")
replay_have_templates <- dir.exists(replay_templates_dir)

with_replay_dirs <- function(code) {
  d <- tempfile(); dir.create(d)
  old <- Sys.getenv(c("SHINY_FAVORITES_PATH", "SHINY_EXPORT_HISTORY_DIR"), unset = NA)
  Sys.setenv(SHINY_FAVORITES_PATH = file.path(d, "fav.json"),
             SHINY_EXPORT_HISTORY_DIR = file.path(d, "hist"))
  on.exit({
    for (k in names(old)) {
      if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, stats::setNames(list(old[[k]]), k))
    }
  }, add = TRUE)
  force(code)
}

# A chart whose data genuinely depends on one option ("dim"), registered the
# way chart_data_downloads_server() registers a real one. `live_dim` stands in
# for the session's current input value.
register_replayable_chart <- function(session, module_id, live_dim = "leeftijd",
                                      can_replay = TRUE) {
  data_for <- function(sel) {
    dim <- if (!is.null(sel) && !is.null(sel[["dim"]])) sel[["dim"]] else live_dim
    data.frame(
      subgroep = paste0(dim, c("_A", "_B")),
      klasse = c("N05", "N05"),
      waarde = if (identical(dim, "leeftijd")) c(10, 20) else c(999, 888),
      stringsAsFactors = FALSE
    )
  }
  get_spec <- function(selections = NULL) {
    # Mirrors build_export_spec()'s contract: an unhonorable replay degrades to
    # the live inputs AND reports the live selections, never the stored ones.
    sel <- selections
    if (!is.null(sel) && !can_replay) sel <- NULL
    df <- data_for(sel)
    dim_used <- if (!is.null(sel) && !is.null(sel[["dim"]])) sel[["dim"]] else live_dim
    list(
      tc_data = df, raw_data = df, chart_type = "grouped_bar", slide_matrix = NULL,
      is_faceted = FALSE, slide_title = "", figure_title = paste("Chart for", dim_used),
      template_override = "", slide_order = "auto", dashboard_title = "Pharm",
      tab_label = "ATC", subtab_label = "Prevalentie",
      selections = if (is.null(sel)) list(dim = live_dim) else sel,
      source_output = "", source_sheet = "", source_mtime = "",
      filename_prefix = "chart", dictionary_format = FALSE, dictionary_crosswalk = NULL
    )
  }
  tc_chart_registry_register(session, module_id, list(
    build_zip = function(...) stop("not used by these tests"),
    get_spec = get_spec, can_replay = can_replay
  ))
}

test_that("two favorites of the same chart with different options rebuild to different data", {
  with_replay_dirs({
    session <- fake_session()
    # Live input is "leeftijd" -- the value BOTH favorites would have collapsed
    # onto before the fix.
    register_replayable_chart(session, "atc_dl", live_dim = "leeftijd")

    fav_a <- list(label = "A", module_id = "atc_dl", selections = list(dim = "leeftijd"),
                  template_override = "")
    fav_b <- list(label = "B", module_id = "atc_dl", selections = list(dim = "inkomen"),
                  template_override = "")

    a <- favorites_prepare_live_spec(fav_a, session)
    b <- favorites_prepare_live_spec(fav_b, session)

    expect_false(identical(a$tc_table, b$tc_table))
    expect_equal(a$figure_title, "Chart for leeftijd")
    expect_equal(b$figure_title, "Chart for inkomen")
    # The favorite starred on a non-live option must NOT come back as the live one.
    expect_true(any(grepl("inkomen", as.character(unlist(b$tc_table)))))
    expect_false(any(grepl("inkomen", as.character(unlist(a$tc_table)))))
  })
})

test_that("each favorite's history entry logs its own options, not the live ones", {
  skip_if_not(replay_have_templates, "templates directory not available")
  with_replay_dirs({
    session <- fake_session()
    register_replayable_chart(session, "atc_dl", live_dim = "leeftijd")

    favorites_prepare_live_spec(
      list(label = "A", module_id = "atc_dl", selections = list(dim = "leeftijd"),
           template_override = ""), session, templates_dir = replay_templates_dir)
    favorites_prepare_live_spec(
      list(label = "B", module_id = "atc_dl", selections = list(dim = "inkomen"),
           template_override = ""), session, templates_dir = replay_templates_dir)

    logged <- sort(vapply(export_history_list(),
                          function(e) as.character(unlist(e$selections)[["dim"]]), character(1)))
    expect_equal(logged, c("inkomen", "leeftijd"))
  })
})

test_that("favorites_live_spec_or_null passes the stored selections through to get_spec", {
  session <- fake_session()
  seen <- NULL
  tc_chart_registry_register(session, "spy_dl", list(
    build_zip = function(...) stop("unused"),
    get_spec = function(selections = NULL) {
      seen <<- selections
      list(tc_data = data.frame(a = 1), raw_data = data.frame(a = 1), chart_type = "bar",
           is_faceted = FALSE, selections = list())
    },
    can_replay = TRUE
  ))

  favorites_live_spec_or_null(
    list(module_id = "spy_dl", selections = list(dim = "inkomen", jaar = 2024)), session)
  expect_equal(seen, list(dim = "inkomen", jaar = 2024))

  # A favorite with no stored selections still means "use live inputs" (NULL),
  # so older favorites keep working exactly as before.
  seen <- "untouched"
  favorites_live_spec_or_null(list(module_id = "spy_dl", selections = list()), session)
  expect_null(seen)
})

test_that("a chart that cannot replay logs the live options it actually used, not the stored ones", {
  # The provenance log must never claim options the data didn't come from.
  skip_if_not(replay_have_templates, "templates directory not available")
  with_replay_dirs({
    session <- fake_session()
    register_replayable_chart(session, "legacy_dl", live_dim = "leeftijd", can_replay = FALSE)

    spec <- favorites_prepare_live_spec(
      list(label = "L", module_id = "legacy_dl", selections = list(dim = "inkomen"),
           template_override = ""), session, templates_dir = replay_templates_dir)

    expect_equal(spec$figure_title, "Chart for leeftijd")
    logged <- as.character(unlist(export_history_list()[[1]]$selections)[["dim"]])
    expect_equal(logged, "leeftijd")
    expect_false(identical(logged, "inkomen"))
  })
})

test_that("export_history_replay_selections returns stored options, or NULL when absent", {
  expect_equal(export_history_replay_selections(list(selections = list(dim = "inkomen"))),
               list(dim = "inkomen"))
  expect_null(export_history_replay_selections(list(selections = list())))
  expect_null(export_history_replay_selections(list()))
})

test_that("tc_assert_replayable fails for a chart with no data_for and passes when all are wired", {
  session <- fake_session()
  register_replayable_chart(session, "good_dl", can_replay = TRUE)
  expect_silent(tc_assert_replayable(session))

  register_replayable_chart(session, "bad_dl", can_replay = FALSE)
  expect_error(tc_assert_replayable(session), "bad_dl")
  # An explicit exemption is honored (for a genuinely option-free figure).
  expect_silent(tc_assert_replayable(session, ignore = "bad_dl"))
})
