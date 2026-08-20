#' Dictionary tab UI, decoupled from git deploys -- lets anyone add or fix a
#' raw-name -> pretty-label mapping (see `utils/dictionary.R`) without a
#' developer editing code and redeploying. Follows the same
#' `*_ui(id)`/`*_server(id)` module shape as `utils/template_admin.R`, and
#' the same list/modal conventions as `favorites_panel_server()` in
#' `utils/favorites.R`.

#' A safe, stable HTML-id fragment for one `(raw_key, scope)` pair, so each
#' row's "Edit" button gets a distinct, reusable input id across re-renders.
dict_entry_ui_id <- function(raw_key, scope) {
  raw <- jsonlite::base64_enc(charToRaw(paste0(tc_or(raw_key, ""), "", tc_or(scope, ""))))
  paste0("row_", gsub("[^A-Za-z0-9]", "", raw))
}

#' Human-readable category headings for the Dictionary tab's entry list,
#' keyed by an entry's own `scope` -- entries are grouped by `scope` for
#' display (see `output$list` below), reusing that field rather than adding
#' a separate one, since it already meaningfully partitions the data. Not
#' exhaustive on purpose: any scope not listed here still gets a readable
#' heading via [dictionary_scope_label()]'s fallback, so a dashboard-specific
#' scope introduced later never renders blank. A dashboard can freely extend
#' or override this vector (e.g. `DICTIONARY_SCOPE_LABELS["my_scope"] <-
#' "My category"`, after sourcing this file) to add its own category names.
DICTIONARY_SCOPE_LABELS <- stats::setNames("Overig (geen scope)", "")

#' Display heading for one scope value -- a curated label from
#' [DICTIONARY_SCOPE_LABELS] if one exists, else a generic prettified
#' version of the raw scope string ([dictionary_default_prettify()] in
#' `utils/dictionary.R`) so every scope present in the data gets *some*
#' readable heading, curated or not.
dictionary_scope_label <- function(scope) {
  scope <- tc_or(scope, "")
  # match(), not `[[`/`["scope"]` -- both of those treat "" (the "no scope"
  # case) as "no name" rather than a literal empty-string name to match,
  # and silently fail to find it (`[[` errors, `[` returns NA) even though
  # `names(DICTIONARY_SCOPE_LABELS)` genuinely contains "". match() compares
  # the strings directly and has no such special case.
  idx <- match(scope, names(DICTIONARY_SCOPE_LABELS))
  if (!is.na(idx)) return(unname(DICTIONARY_SCOPE_LABELS[idx]))
  dictionary_default_prettify(scope)
}

#' Order a set of scope values for display: curated scopes first (in
#' [DICTIONARY_SCOPE_LABELS]'s own order), anything else alphabetically
#' after.
#' @param scopes_present Unique scope values actually present in the data.
dictionary_scope_order <- function(scopes_present) {
  known <- names(DICTIONARY_SCOPE_LABELS)
  c(
    known[known %in% scopes_present],
    sort(setdiff(scopes_present, known))
  )
}

#' Shiny's own `.shiny-input-container` defaults every input's wrapper to
#' `width: 300px; margin-bottom: 15px` -- fine for a form, but it misaligns
#' an inline textInput sitting in a single-line flex row alongside a raw-name
#' code chip and Save/Delete buttons (see `output$list` below). Scoped to
#' `.tc-dict-pretty-input`, same reasoning as `TC_FAVORITES_CSS`/
#' `TC_EXPORT_HISTORY_CSS`'s own row-checkbox fix.
TC_DICTIONARY_ADMIN_CSS <- r"(
.tc-dict-pretty-input .shiny-input-container { width: 100%; margin-bottom: 0; }
)"

#' UI for the Dictionary admin panel.
#' @param id Module id.
dictionary_admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Dictionary"),
    shiny::p(
      class = "text-muted",
      "Maps a raw name from the underlying data source (e.g. \"zvwktotaal\") to the ",
      "pretty label shown on chart bars and in every download instead (e.g. \"Totale ",
      "ZVW kosten\"). Prefilled automatically where possible -- add or fix an entry below."
    ),
    shiny::fluidRow(
      shiny::column(width = 4, shiny::actionButton(ns("add"), "Add entry", class = "btn-primary")),
      shiny::column(
        width = 8,
        shiny::textInput(ns("search"), NULL, placeholder = "Search raw name, scope, or label...", width = "100%")
      )
    ),
    shiny::uiOutput(ns("status")),
    shiny::tags$hr(),
    shiny::uiOutput(ns("list")),
    shiny::tags$style(shiny::HTML(TC_DICTIONARY_ADMIN_CSS))
  )
}

#' Server logic for the Dictionary admin panel.
#' @param id Module id.
#' @param poll_interval_ms How often to check the dictionary file for
#'   changes made by another session, so edits show up everywhere without
#'   any direct wiring between modules (same pattern as
#'   `favorites_panel_server()`).
dictionary_admin_server <- function(id, poll_interval_ms = 2000) {
  shiny::moduleServer(id, function(input, output, session) {
    status_rv <- shiny::reactiveValues(message = NULL, ok = NA)

    entries_reactive <- shiny::reactivePoll(
      poll_interval_ms, session,
      checkFunc = function() {
        path <- dictionary_path()
        if (file.exists(path)) as.character(file.info(path)$mtime) else ""
      },
      valueFunc = dictionary_list
    )

    filtered_entries <- shiny::reactive({
      entries <- entries_reactive()
      q <- trimws(tolower(tc_or(input$search, "")))
      if (!nzchar(q)) return(entries)
      Filter(function(e) {
        hay <- tolower(paste(tc_or(e$raw_key, ""), tc_or(e$scope, ""), tc_or(e$pretty_label, "")))
        grepl(q, hay, fixed = TRUE)
      }, entries)
    })

    # "Add entry" still uses a modal -- a brand-new entry needs raw_key +
    # scope, fields an existing entry never changes, so there's no row for
    # it to live inline in yet.
    open_add_editor <- function() {
      shiny::showModal(shiny::modalDialog(
        title = "Add dictionary entry",
        shiny::textInput(session$ns("edit_raw_key"), "Raw name (as it appears in the data source)"),
        shiny::textInput(
          session$ns("edit_scope"), "Scope (optional)",
          placeholder = "e.g. a column name -- only needed if the same raw name means different things in different places"
        ),
        shiny::textInput(session$ns("edit_pretty_label"), "Pretty label"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("save"), "Save", class = "btn-primary")
        )
      ))
    }

    shiny::observeEvent(input$add, open_add_editor(), ignoreInit = TRUE)

    shiny::observeEvent(input$save, {
      raw_key <- trimws(tc_or(input$edit_raw_key, ""))
      scope <- trimws(tc_or(input$edit_scope, ""))
      pretty_label <- trimws(tc_or(input$edit_pretty_label, ""))

      if (!nzchar(raw_key) || !nzchar(pretty_label)) {
        status_rv$message <- "Both the raw name and the pretty label are required."
        status_rv$ok <- FALSE
        return(invisible(NULL))
      }

      dictionary_set_entry(raw_key, scope, pretty_label)
      status_rv$message <- sprintf("Saved '%s' -> '%s'.", raw_key, pretty_label)
      status_rv$ok <- TRUE
      shiny::removeModal()
    })

    # Every existing entry's pretty label is an always-visible, always-
    # editable textInput right in its own row (see output$list below) --
    # opening a category shows every entry ready to edit at once, no
    # per-entry click-to-reveal step. Each row's own Save/Delete buttons are
    # lazily registered exactly once per row id, same reasoning as
    # favorites_panel_server()'s own per-row observers: an observeEvent
    # isn't idempotent, so re-registering on every re-render would stack
    # duplicate handlers.
    registered_row_buttons <- new.env()
    shiny::observe({
      entries <- filtered_entries()
      new_rows <- Filter(
        function(e) !exists(dict_entry_ui_id(e$raw_key, e$scope), envir = registered_row_buttons, inherits = FALSE),
        entries
      )
      lapply(new_rows, function(e) {
        row_id <- dict_entry_ui_id(e$raw_key, e$scope)
        assign(row_id, TRUE, envir = registered_row_buttons)
        raw_key <- e$raw_key
        scope <- tc_or(e$scope, "")
        pretty_input_id <- paste0("pretty_", row_id)
        save_id <- paste0("save_", row_id)
        delete_id <- paste0("delete_", row_id)

        shiny::observeEvent(input[[save_id]], {
          pretty_label <- trimws(tc_or(input[[pretty_input_id]], ""))
          if (!nzchar(pretty_label)) {
            status_rv$message <- "The pretty label is required."
            status_rv$ok <- FALSE
            return(invisible(NULL))
          }
          dictionary_set_entry(raw_key, scope, pretty_label)
          status_rv$message <- sprintf("Saved '%s' -> '%s'.", raw_key, pretty_label)
          status_rv$ok <- TRUE
        }, ignoreInit = TRUE)

        shiny::observeEvent(input[[delete_id]], {
          dictionary_remove_entry(raw_key, scope)
          status_rv$message <- sprintf("Removed '%s'.", raw_key)
          status_rv$ok <- TRUE
        }, ignoreInit = TRUE)
      })
    })

    output$status <- shiny::renderUI({
      shiny::req(status_rv$message)
      cls <- if (isTRUE(status_rv$ok)) "text-success" else "text-danger"
      shiny::tags$p(class = cls, status_rv$message)
    })

    # Grouped by scope rather than one flat list -- with 100+ entries (a
    # real dashboard) that's an unscrollable wall; the group heading already
    # conveys the scope, so individual rows no longer repeat it.
    output$list <- shiny::renderUI({
      entries <- filtered_entries()
      if (length(entries) == 0) {
        return(shiny::tags$p(class = "text-muted", "No dictionary entries yet."))
      }

      scopes <- vapply(entries, function(e) tc_or(e$scope, ""), character(1))
      groups <- split(entries, scopes)
      is_searching <- nzchar(trimws(tc_or(input$search, "")))

      sections <- lapply(dictionary_scope_order(names(groups)), function(sc) {
        # match() + positional [[, not groups[[sc]] -- same "" pitfall as
        # dictionary_scope_label() above: `[[` treats an empty-string name
        # as "no name" and silently returns a length-0 result instead of
        # the actual "no scope" group.
        group_entries <- groups[[match(sc, names(groups))]]
        rows <- lapply(group_entries, function(e) {
          row_id <- dict_entry_ui_id(e$raw_key, e$scope)
          shiny::tags$div(
            style = paste(
              "display:flex; justify-content:space-between; align-items:center;",
              "gap:8px; padding:8px 0; border-bottom:1px solid #eee;"
            ),
            shiny::tags$code(style = "flex:0 0 auto;", tc_or(e$raw_key, "")),
            shiny::div(
              class = "tc-dict-pretty-input",
              style = "flex:1 1 auto;",
              shiny::textInput(session$ns(paste0("pretty_", row_id)), NULL, value = tc_or(e$pretty_label, ""), width = "100%")
            ),
            shiny::actionButton(session$ns(paste0("save_", row_id)), "Save", class = "btn-primary btn-sm"),
            shiny::actionButton(session$ns(paste0("delete_", row_id)), "Delete", class = "btn-danger btn-sm")
          )
        })
        shiny::tags$details(
          open = if (is_searching) NA else NULL,
          shiny::tags$summary(
            style = "cursor:pointer; font-weight:600; padding:8px 0; list-style:revert;",
            sprintf("%s (%d)", dictionary_scope_label(sc), length(group_entries))
          ),
          shiny::tags$div(style = "padding-left:8px;", do.call(shiny::tagList, rows))
        )
      })
      do.call(shiny::tagList, sections)
    })

    invisible(NULL)
  })
}
