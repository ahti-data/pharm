resolve_tc_chart_type <- function(chart_type) {
  if (shiny::is.reactive(chart_type)) {
    return(chart_type())
  }
  chart_type
}

#' Delegated click handler for the "Choose a slide template" modal's
#' thumbnail grid (see `chart_data_downloads_server()`'s `slide_template_open`
#' observer) -- replaces the old `selectizeInput`-based picker, whose large
#' thumbnails made its dropdown awkward to open/scroll reliably. A modal
#' always reflects the current template list fresh (built at open time, via
#' [tc_template_choice_items()]), so unlike the old picker there's no
#' separate refresh-on-upload mechanism to maintain. Idempotent registration,
#' same pattern as [TC_CHART_CAPTURE_JS].
TC_TEMPLATE_MODAL_JS <- r"(
if (!window.__tcTemplateModalInit) {
  window.__tcTemplateModalInit = true;
  $(document).on('click', '.tc-template-grid-item', function() {
    Shiny.setInputValue($(this).data('picker-input-id'), $(this).data('value'), {priority: 'event'});
  });
}
)"

#' Styling for the template-picker modal's thumbnail grid (see
#' [TC_TEMPLATE_MODAL_JS]) -- a comfortably large, scrollable grid of
#' clickable cards, each showing a full preview image and its file name, with
#' a highlighted border on the currently-chosen one.
TC_TEMPLATE_MODAL_CSS <- r"(
.tc-template-grid { display:flex; flex-wrap:wrap; gap:16px; max-height:75vh; overflow-y:auto; padding:2px; }
.tc-template-grid-item { cursor:pointer; border:2px solid #E4E7EE; border-radius:8px; padding:10px; width:440px; text-align:center; }
.tc-template-grid-item:hover { border-color:#93C5FD; background:#F0F7FF; }
.tc-template-grid-item-selected { border-color:#2563EB; background:#EFF6FF; }
.tc-template-grid-item img { width:100%; height:248px; object-fit:contain; background:#fff; border:1px solid #E4E7EE; border-radius:4px; }
.tc-template-grid-noimg { width:100%; height:248px; display:flex; align-items:center; justify-content:center; color:#9CA3AF; font-size:12px; background:#F9FAFB; border:1px dashed #E4E7EE; border-radius:4px; }
.tc-template-grid-label { margin-top:8px; font-size:14px; color:#374151; word-break:break-word; }
)"

#' Client-side PNG snapshot for "Download slide", Export History's
#' "Regenerate selected", and Favorites' live "Download slides" bulk button,
#' so their ZIPs can include a fresh `charts_overview.html` (see
#' `tc_build_charts_overview_html()` in `utils/favorites.R`) captured at the
#' moment of download/regenerate, not a stale one.
#'
#' Every `downloadButton` this replaces becomes a plain `actionButton` (class
#' `tc-slide-go-btn` / `tc-regenerate-go-btn`) paired with a hidden *real*
#' `downloadButton` that the client clicks programmatically once it has
#' either captured a screenshot or given up waiting -- a real download can't
#' pause mid-flight for a screenshot, so the screenshot has to arrive
#' *before* the browser ever requests the file. A capture that fails, times
#' out, or has no Plotly widget to capture in the first place still lets the
#' download through (with `image: null`) -- a missing snapshot only means
#' that chart's overview page is absent from the ZIP, never a broken export.
#'
#' `tc-slide-go-btn` (one chart, e.g. `plot_basispopulatie`) reads its own
#' `data-plot-output-id` directly. `tc-regenerate-go-btn` (Export History's
#' bottom banner, or Favorites' "Download slides" button, each spanning an
#' arbitrary multi-chart selection) instead reads a JSON-encoded
#' `data-module-ids` list (refreshed by the server whenever the selection
#' changes) and looks each one up via `data-module-id` on whichever chart
#' panel(s) happen to be rendered right now -- a chart outside the current
#' tab/session simply isn't found, which is exactly the same "not live" case
#' the underlying data regenerate/favorites rebuild already skips.
TC_CHART_CAPTURE_JS <- r"(
if (!window.__tcChartCaptureInit) {
  window.__tcChartCaptureInit = true;

  function __tcFindPlotlyDiv(outputId) {
    var el = outputId ? document.getElementById(outputId) : null;
    if (!el) return null;
    if (el.classList && el.classList.contains('js-plotly-plot')) return el;
    return el.querySelector('.js-plotly-plot');
  }

  function __tcFindPlotlyDivByModule(moduleId) {
    var wrap = document.querySelector('[data-module-id="' + moduleId + '"][data-plot-output-id]');
    if (!wrap) return null;
    return __tcFindPlotlyDiv(wrap.getAttribute('data-plot-output-id'));
  }

  function __tcCapturePng(gd) {
    if (!gd || typeof Plotly === 'undefined') return Promise.resolve(null);
    var w = (gd._fullLayout && gd._fullLayout.width) || 900;
    var h = (gd._fullLayout && gd._fullLayout.height) || 550;
    return Plotly.toImage(gd, {format: 'png', width: w, height: h}).catch(function() { return null; });
  }

  $(document).on('click', '.tc-slide-go-btn', function() {
    var $btn = $(this);
    var done = false;
    function finish(dataUrl) {
      if (done) return;
      done = true;
      Shiny.setInputValue($btn.data('capture-input-id'), {image: dataUrl || null, nonce: Math.random()}, {priority: 'event'});
    }
    __tcCapturePng(__tcFindPlotlyDiv($btn.attr('data-plot-output-id'))).then(finish);
    setTimeout(function() { finish(null); }, 2000);
  });

  $(document).on('click', '.tc-regenerate-go-btn', function() {
    var $btn = $(this);
    var moduleIds = [];
    try { moduleIds = JSON.parse($btn.attr('data-module-ids') || '[]'); } catch (e) {}
    var captures = {};
    var done = false;
    function finish() {
      if (done) return;
      done = true;
      Shiny.setInputValue($btn.data('capture-input-id'), {captures: captures, nonce: Math.random()}, {priority: 'event'});
    }
    var promises = moduleIds.map(function(mid) {
      return __tcCapturePng(__tcFindPlotlyDivByModule(mid)).then(function(dataUrl) {
        if (dataUrl) captures[mid] = dataUrl;
      });
    });
    Promise.all(promises).then(finish);
    setTimeout(finish, 3000);
  });

  $(document).on('shiny:connected', function() {
    Shiny.addCustomMessageHandler('tc_trigger_download', function(msg) {
      var el = document.getElementById(msg.download_id);
      if (el) el.click();
    });
  });
}
)"

#' UI for raw and think-cell chart data downloads.
#'
#' The think-cell button is only shown when `chart_type` is supported.
#'
#' @param id Module id.
#' @param chart_type think-cell chart type for this chart.
#' @param raw_label Download button label for raw data.
#' @param thinkcell_label Download button label for think-cell data.
#' @param favorite_label Label for the "save as favorite" button.
#' @param plot_output_id Optional Shiny output id (top-level, NOT namespaced --
#'   e.g. `"plot_basispopulatie"`) of the Plotly widget this chart's downloads
#'   pair with. When set, a "Download slide"/"Regenerate"/live favorites-deck
#'   click snapshots that widget as a PNG (see [TC_CHART_CAPTURE_JS]) for the
#'   ZIP's `charts_overview.html`. Omit for charts with no Plotly widget
#'   (e.g. `renderPlot()`-based) -- the export still works, just without an
#'   image.
#' @param default_slide_order Initial "Category order" selection (one of
#'   `"auto"`, `"as_is"`, `"cat_asc"`, `"cat_desc"`, `"val_asc"`,
#'   `"val_desc"` -- see [tc_order_slide_matrix()]). `"auto"` only reorders
#'   by *numeric* category value (e.g. years); for a chart whose plot instead
#'   orders a text category by `reorder(category, value)` (ascending mean),
#'   set this to `"val_asc"`/`"val_desc"` to match so the exported table
#'   reads in the same order as the chart on screen, instead of "auto"
#'   silently falling back to whatever order the data happens to arrive in.
#'   Still just the *default* -- the dropdown remains user-changeable per
#'   download.
chart_data_downloads_ui <- function(
    id,
    chart_type,
    raw_label = "Download Excel data (raw)",
    thinkcell_label = "Download Excel data (think-cell formatted)",
    slide_label = "Download slides",
    favorite_label = "☆ Save as favorite",
    plot_output_id = NULL,
    default_slide_order = "auto"
) {
  ns <- shiny::NS(id)

  buttons <- list(
    shiny::downloadButton(ns("raw"), raw_label, class = "btn-default")
  )

  # Controls whether this chart's own downloads (raw, think-cell, and slide
  # table alike) relabel category/series *values* through the shared
  # Dictionary (utils/dictionary.R) before writing them out -- never the
  # on-screen chart, which always reflects the dictionary already (see
  # CLAUDE.md's dictionary_relabel() convention). Checked by default since
  # dictionary-formatted labels are the normal expectation; unchecking is an
  # escape hatch back to the underlying raw data-source names, e.g. for
  # re-joining an export with source data.
  dictionary_checkbox <- shiny::checkboxInput(
    ns("dictionary_format"), "Format from dictionary", value = TRUE
  )

  show_thinkcell <- if (shiny::is.reactive(chart_type)) {
    TRUE
  } else {
    is_tc_chart_type_supported(chart_type)
  }

  if (show_thinkcell) {
    buttons <- c(
      buttons,
      list(
        shiny::downloadButton(ns("thinkcell"), thinkcell_label, class = "btn-primary")
      )
    )
  }

  # The slide (.pptx) download appears whenever a usable template exists for the
  # chart. For reactive chart types we show it and validate at click time.
  show_slide <- if (shiny::is.reactive(chart_type)) {
    TRUE
  } else {
    tc_template_available(chart_type)
  }

  slide_extra <- NULL
  if (show_slide) {
    buttons <- c(
      buttons,
      list(
        shiny::actionButton(
          ns("slide_go"), slide_label, class = "btn-primary tc-slide-go-btn",
          `data-plot-output-id` = plot_output_id,
          `data-capture-input-id` = ns("slide_capture")
        ),
        shiny::tags$span(
          style = "display:none;",
          shiny::downloadButton(ns("slide"), "")
        )
      )
    )
    slide_extra <- shiny::tagList(
      shiny::div(
        class = "tc-slide-template",
        style = "margin-top:8px;",
        shiny::tags$label("Slide template", style = "font-weight:600; display:block; margin-bottom:4px;"),
        shiny::uiOutput(ns("slide_template_info")),
        shiny::actionButton(
          ns("slide_template_open"), "Choose template...",
          class = "btn-default btn-sm", style = "margin-top:4px;"
        ),
        shiny::selectInput(
          ns("slide_order"),
          label = "Category order",
          choices = c(
            "Automatic (numeric / as displayed)" = "auto",
            "As displayed"                        = "as_is",
            "Category ascending"                  = "cat_asc",
            "Category descending"                 = "cat_desc",
            "Value ascending"                     = "val_asc",
            "Value descending"                    = "val_desc"
          ),
          selected = default_slide_order
        ),
        shiny::tags$div(
          # data-plot-output-id/data-module-id aren't used by the favorite
          # button itself -- they're the lookup anchor TC_CHART_CAPTURE_JS's
          # ".tc-regenerate-go-btn" handler uses to screenshot this chart's
          # live widget from Export History's or Favorites' bulk downloads.
          `data-plot-output-id` = plot_output_id,
          `data-module-id` = id,
          shiny::actionButton(ns("favorite"), favorite_label,
                              class = "btn-default",
                              style = "margin-top:8px;"),
          shiny::uiOutput(ns("favorite_status"))
        ),
        shiny::tags$script(shiny::HTML(TC_TEMPLATE_MODAL_JS)),
        shiny::tags$script(shiny::HTML(TC_CHART_CAPTURE_JS)),
        shiny::tags$style(shiny::HTML(TC_TEMPLATE_MODAL_CSS))
      )
    )
  }

  # Everything download/preview-related for this chart lives in one visually
  # contained panel, rather than a loose sequence of buttons and controls.
  shiny::tags$div(
    class = "tc-export-panel",
    style = paste(
      "border:1px solid #E4E7EE; border-radius:8px; padding:12px 14px 14px;",
      "background:#FAFAFA; margin-bottom:10px;"
    ),
    shiny::uiOutput(ns("source_updated_info")),
    dictionary_checkbox,
    do.call(shiny::tagList, buttons),
    slide_extra
  )
}

#' Server logic for raw and think-cell chart data downloads.
#'
#' @param id Module id.
#' @param data Reactive returning the exact data frame used to build the ggplot.
#'   Internally shadowed by a wrapped copy that conditionally applies
#'   `dictionary_relabel()` to `category_col`/`series_col` (see the
#'   "Format from dictionary" checkbox below) -- every download this module
#'   builds reads that wrapped version, never this raw argument directly.
#' @param chart_type think-cell chart type. The think-cell handler is registered
#'   only when this type is supported. May be a reactive for dynamic chart types.
#' @param category_col,series_col,value_col Column names for think-cell export.
#' @param filename_prefix Prefix for downloaded file names.
#' @param agg_fun Aggregation function passed to [format_tc_data()].
#' @param category_order,series_order Optional order vectors for think-cell export.
#' @param waterfall_end_col,waterfall_subtotal_cols Optional waterfall markers.
#' @param facet_col Optional facet column for `facet_wrap()` / `facet_grid()` plots.
#' @param source_output,source_sheet Optional data-source identifiers (see
#'   `tc_build_datasheet_log()` in `utils/slide_download.R`) -- e.g. a
#'   pipeline output id ("3a") and, within it, a sheet name -- for a
#'   dashboard whose chart data is assembled from named external outputs.
#'   Each may be a plain string or a reactive/function (like `slide_title`).
#'   Stamped into the corner cell/header of every export this chart offers
#'   (raw download excluded -- it isn't a think-cell matrix), so a chart
#'   found later can be traced back to its source. Omit both for dashboards
#'   with no such concept.
#' @param source_mtime Optional last-modified date of `source_output`'s
#'   underlying file, already formatted via `tc_format_source_mtime()` in
#'   `utils/slide_download.R` -- like `source_output`/`source_sheet`, a plain
#'   string or a reactive/function. Stamped as its own `source_updated=`
#'   field, distinct from the export's own `timestamp=`. Omit if
#'   `source_output` isn't set either.
#' @param category_scope,series_scope Dictionary `scope` (see
#'   `dictionary_relabel()` in `utils/dictionary.R`) for `category_col`'s and
#'   `series_col`'s *values* -- normally the same scope this chart's own
#'   plot rendering already uses for that column. Each may be a plain string
#'   or a reactive/function (like `slide_title`), for a chart whose scope
#'   depends on a current selection (e.g. a demographic-split column picked
#'   via a dropdown). Defaults to `category_col`/`series_col` themselves,
#'   which is only meaningful when that column name IS the scope (as in a
#'   single-chart template) -- a dashboard with many charts sharing generic
#'   column names (e.g. every chart's raw data using "category"/"series")
#'   should pass the real scope explicitly, or downloads will fall back to
#'   dictionary_default_prettify() instead of the intended lookup.
chart_data_downloads_server <- function(
    id,
    data,
    chart_type,
    category_col,
    series_col,
    value_col,
    filename_prefix = "chart_data",
    agg_fun = NULL,
    category_order = NULL,
    series_order = NULL,
    waterfall_end_col = NULL,
    waterfall_subtotal_cols = NULL,
    facet_col = NULL,
    slide_title = NULL,
    figure_title = NULL,
    template_override = NULL,
    source_output = NULL,
    source_sheet = NULL,
    source_mtime = NULL,
    category_scope = NULL,
    series_scope = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    resolve_opt <- function(x) {
      if (is.null(x)) return("")
      if (shiny::is.reactive(x)) return(tc_or(x(), ""))
      if (is.function(x)) return(tc_or(x(), ""))
      x
    }

    # resolve_opt() returns "" for a NULL/unset arg -- fall back to the
    # column name itself in that case, but re-resolve on every call rather
    # than once at module setup, since category_scope/series_scope may be a
    # reactive/function whose value can change (e.g. a chart whose scope
    # depends on a currently-selected split column).
    scope_or_col <- function(scope_arg, col) {
      resolved <- resolve_opt(scope_arg)
      if (nzchar(resolved)) resolved else col
    }

    # Every internal use of `data()` below (raw download, think-cell
    # download, and the slide/favorites spec) goes through this shadowed
    # reactive instead of the raw `data` argument, so the "Format from
    # dictionary" checkbox (see chart_data_downloads_ui()) affects every
    # download this chart offers uniformly, with no per-download-handler
    # changes needed. Never touches the live chart -- callers render their
    # own plot from the original `data` reactive (or their own
    # already-dictionary-formatted variant of it), independent of this one.
    #
    # fallback = identity (not dictionary_relabel()'s own default,
    # dictionary_default_prettify()) deliberately: a value with no matching
    # dictionary entry is left exactly as given rather than guessed at.
    # This matters a lot for a chart whose category/series values are
    # *already* dictionary-formatted upstream (common once several charts
    # share this module) -- running an already-correct value back through a
    # generic title-caser on a miss can corrupt it (e.g. "18-29 jaar" ->
    # "18 29 Jaar"); leaving it untouched makes checking the box a safe
    # no-op for any chart that hasn't been explicitly wired with a matching
    # scope, while still fully relabeling a genuinely raw value that does
    # have an entry.
    identity_fallback <- function(x) x

    # dictionary_relabel() always returns a plain character vector, even
    # given a factor -- fine for an unordered column, but silently loses an
    # *ordered* factor's level order (e.g. a chart built with
    # `stats::reorder(name, value)` so its export matches the plot's bar
    # order; format_tc_data()/prepare_tc_long_data() specifically check
    # `is.factor()` to inherit that order). Relabel the levels themselves
    # (in their existing order) and rebuild a factor from them, so a
    # dictionary hit changes the label without silently reordering the bars.
    relabel_column <- function(x, scope) {
      relabeled <- dictionary_relabel(x, scope = scope, fallback = identity_fallback)
      if (is.factor(x)) {
        relabeled <- factor(relabeled, levels = dictionary_relabel(levels(x), scope = scope, fallback = identity_fallback))
      }
      relabeled
    }

    export_category_order <- category_order
    export_series_order <- series_order
    raw_data <- data
    data <- shiny::reactive({
      df <- raw_data()
      if (!isTRUE(input$dictionary_format)) return(df)
      if (category_col %in% names(df)) {
        df[[category_col]] <- relabel_column(df[[category_col]], scope_or_col(category_scope, category_col))
      }
      if (series_col %in% names(df)) {
        df[[series_col]] <- relabel_column(df[[series_col]], scope_or_col(series_scope, series_col))
      }
      df
    })
    # category_order/series_order fix an explicit level order for the raw
    # values -- once those values are relabeled above, the order vector has
    # to be relabeled the same way, or format_tc_data()'s factor(levels=...)
    # silently drops anything that no longer matches.
    resolved_category_order <- shiny::reactive({
      if (is.null(export_category_order)) return(NULL)
      if (!isTRUE(input$dictionary_format)) return(export_category_order)
      dictionary_relabel(export_category_order, scope = scope_or_col(category_scope, category_col), fallback = identity_fallback)
    })
    resolved_series_order <- shiny::reactive({
      if (is.null(export_series_order)) return(NULL)
      if (!isTRUE(input$dictionary_format)) return(export_series_order)
      dictionary_relabel(export_series_order, scope = scope_or_col(series_scope, series_col), fallback = identity_fallback)
    })

    # For the corner-cell log (see tc_build_datasheet_log()'s
    # dictionary_crosswalk param): every distinct raw category/series value
    # that a real dictionary hit actually changed for this specific
    # download, deduplicated across both columns. A value with no matching
    # entry never contributes a pair (relabel_column()'s identity fallback
    # leaves it unchanged, so raw == pretty and it's filtered out below) --
    # this stays empty, and adds nothing to the log, whenever the checkbox
    # is off or no value in this chart happens to be in the dictionary.
    dictionary_crosswalk <- shiny::reactive({
      if (!isTRUE(input$dictionary_format)) return(NULL)
      df <- raw_data()
      pairs <- character(0)
      collect <- function(col, scope) {
        if (!(col %in% names(df))) return(character(0))
        raw_vals <- unique(as.character(df[[col]]))
        raw_vals <- raw_vals[!is.na(raw_vals)]
        pretty_vals <- dictionary_relabel(raw_vals, scope = scope, fallback = identity_fallback)
        changed <- raw_vals != pretty_vals
        stats::setNames(pretty_vals[changed], raw_vals[changed])
      }
      pairs <- c(
        collect(category_col, scope_or_col(category_scope, category_col)),
        collect(series_col, scope_or_col(series_scope, series_col))
      )
      pairs[!duplicated(names(pairs))]
    })

    # The template that will be used for the slide: the user's manual choice
    # (picked from the TC_TEMPLATE_MODAL_JS grid, see below) if set, otherwise
    # the one auto-detected from the displayed figure. A plain reactiveVal,
    # not an input -- the modal always rebuilds its grid fresh from
    # tc_template_choice_items() at open time (see slide_template_open
    # below), so unlike the old selectizeInput-based picker there's no
    # separate poll-and-refresh mechanism needed for a template uploaded at
    # runtime (Manage Templates tab) to show up.
    slide_template_manual <- shiny::reactiveVal("")

    slide_effective_override <- function() {
      ui_choice <- tc_or(slide_template_manual(), "")
      if (nzchar(ui_choice)) ui_choice else resolve_opt(template_override)
    }

    slide_ui_present <- if (shiny::is.reactive(chart_type)) TRUE else tc_template_available(chart_type)
    if (slide_ui_present) {
      shiny::observeEvent(input$slide_template_open, {
        current <- slide_template_manual()
        # tc_template_choice_items()'s own first entry (value = "") is
        # already the "Automatisch (gedetecteerd)" reset option -- no need
        # to add a second one here.
        grid_items <- tc_template_choice_items()
        shiny::showModal(shiny::modalDialog(
          title = "Choose a slide template",
          size = "l",
          easyClose = TRUE,
          shiny::tags$div(
            class = "tc-template-grid",
            lapply(grid_items, function(it) {
              is_selected <- identical(it$value, current)
              shiny::tags$div(
                class = paste(
                  "tc-template-grid-item",
                  if (is_selected) "tc-template-grid-item-selected" else ""
                ),
                `data-value` = it$value,
                `data-picker-input-id` = session$ns("slide_template_picked"),
                if (nzchar(it$preview)) {
                  shiny::tags$img(src = it$preview)
                } else {
                  shiny::tags$div(class = "tc-template-grid-noimg", "No preview")
                },
                shiny::tags$div(class = "tc-template-grid-label", it$label)
              )
            })
          ),
          footer = shiny::modalButton("Cancel")
        ))
      })

      shiny::observeEvent(input$slide_template_picked, {
        slide_template_manual(tc_or(input$slide_template_picked, ""))
        shiny::removeModal()
      }, ignoreInit = TRUE)
    }

    slide_chosen_template <- shiny::reactive({
      override <- slide_effective_override()
      if (nzchar(override)) {
        path <- tc_template_for_chart_type("", override = override)
        return(list(name = basename(override), source = "manual",
                    available = !is.na(path)))
      }
      rct <- resolve_tc_chart_type(chart_type)
      df  <- tryCatch(data(), error = function(e) NULL)
      slide_type <- if (!is.null(df) && is.null(facet_col)) {
        tryCatch(tc_detect_slide_type(df, rct, category_col, series_col),
                 error = function(e) rct)
      } else {
        rct
      }
      path <- tc_template_for_chart_type(slide_type)
      list(name = if (is.na(path)) NA_character_ else basename(path),
           source = "auto", type = slide_type, available = !is.na(path))
    })

    output$slide_template_info <- shiny::renderUI({
      info <- tryCatch(slide_chosen_template(), error = function(e) NULL)
      if (is.null(info) || is.na(info$name)) {
        return(shiny::tags$div(
          style = "font-size:12px; color:#991B1B;",
          "No matching think-cell template for the current figure."
        ))
      }
      label <- if (identical(info$source, "manual")) {
        "Chosen template (manual): "
      } else {
        "Chosen template (auto): "
      }
      warn <- if (!isTRUE(info$available)) {
        shiny::tags$div(style = "font-size:11px; color:#B45309;",
                        "(file not found in templates/ \u2014 add it to render the slide)")
      } else {
        NULL
      }
      preview <- tryCatch(tc_preview_data_uri(info$name), error = function(e) NA_character_)
      thumb <- if (!is.na(preview)) {
        shiny::tags$img(
          src = preview,
          style = "width:56px;height:32px;object-fit:contain;margin-right:6px;vertical-align:middle;border:1px solid #E4E7EE;border-radius:3px;background:#fff;"
        )
      } else {
        NULL
      }
      shiny::tags$div(
        style = "font-size:12px; color:#374151; display:flex; align-items:center;",
        thumb,
        shiny::tags$span(label, shiny::tags$strong(info$name), warn)
      )
    })

    # Surfaces the same "source_updated=" date the export's A1 corner-cell log
    # carries (see tc_build_datasheet_log()), so the underlying data's
    # last-edited date is visible on the dashboard without downloading first.
    # Renders nothing when this chart has no external source file wired
    # (source_mtime unset) -- e.g. a chart built from inline/synthetic data.
    output$source_updated_info <- shiny::renderUI({
      mt <- resolve_opt(source_mtime)
      if (!nzchar(mt)) return(NULL)
      shiny::tags$div(
        style = "font-size:11px; color:#6B7280; margin-bottom:8px;",
        shiny::tags$span(style = "color:#9CA3AF;", "Source data updated: "),
        mt
      )
    })

    output$raw <- shiny::downloadHandler(
      filename = function() {
        # A per-download id in the file name (mirrors output$thinkcell/slide) --
        # purely a traceability tag here, since the raw download writes no
        # corner-cell provenance to tie it back to.
        paste0(filename_prefix, "_raw_", export_history_new_id(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # The exact filtered data frame behind the plot, written with NO
        # think-cell reshaping -- the raw, long-format companion to the
        # pivoted "Download data (think-cell)" table. "Raw" means unpivoted,
        # not un-dictionaried: data() is the shadowed, dictionary-aware
        # reactive, so the "Format from dictionary" checkbox still relabels
        # its category/series values, same as every other download here. No
        # corner-cell provenance stamp by convention -- this is just the data.
        write_tc_xlsx(data(), file)
      }
    )

    register_thinkcell <- if (shiny::is.reactive(chart_type)) {
      TRUE
    } else {
      is_tc_chart_type_supported(chart_type)
    }

    if (register_thinkcell) {
      # Minted in filename() (which Shiny resolves before content()) and reused
      # in content() so the id in the file name also appears as download_id= in
      # the workbook's own A1 corner-cell log -- file name and log carry the
      # same id. It's a download tag, not an Export History key: the
      # single-chart Excel download still isn't logged to history (only the
      # slide export is).
      pending_thinkcell_id <- shiny::reactiveVal(NULL)
      output$thinkcell <- shiny::downloadHandler(
        filename = function() {
          id <- export_history_new_id()
          pending_thinkcell_id(id)
          paste0(filename_prefix, "_thinkcell_", id, "_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          resolved_chart_type <- resolve_tc_chart_type(chart_type)
          if (!is_tc_chart_type_supported(resolved_chart_type)) {
            stop("Think-cell export is not supported for chart type: ", resolved_chart_type)
          }

          tc_data <- format_tc_data(
            df = data(),
            chart_type = resolved_chart_type,
            category_col = category_col,
            series_col = series_col,
            value_col = value_col,
            agg_fun = agg_fun,
            category_order = resolved_category_order(),
            series_order = resolved_series_order(),
            waterfall_end_col = waterfall_end_col,
            waterfall_subtotal_cols = waterfall_subtotal_cols,
            facet_col = facet_col
          )

          # Write the exact matrix embedded in the slide's think-cell chart
          # (categories across the header, series in rows -- the template
          # orientation from tc_resolve_slide_matrix()), NOT format_tc_data()'s
          # own `tc_data` orientation, which for bar-family charts is the
          # transpose. This is what lets a PM paste the downloaded table
          # straight into the chart's datasheet without re-pivoting first.
          # The "Category order" dropdown is honored via slide_order. A
          # faceted tc_data is a per-facet named list -- the slide embeds only
          # the first facet, so keep all facets in tc_data orientation there,
          # reordered to the first facet's category order (unchanged behavior
          # for the faceted case).
          ordered_matrix <- tc_resolve_slide_matrix(
            if (is_tc_workbook_list(tc_data)) tc_data[[1]] else tc_data,
            resolved_chart_type, NULL, tc_or(input$slide_order, "auto")
          )
          tc_table <- if (is_tc_workbook_list(tc_data)) {
            tc_reorder_by_categories(tc_data, names(ordered_matrix)[-1])
          } else {
            ordered_matrix
          }

          # Same corner-cell provenance idea as the slide/favorites downloads
          # (see tc_build_ppttc_slide_block()), just stamped onto the plain
          # workbook's own header instead of a ppttc chart datasheet, since
          # this export never goes through ppttc.exe. chart_id is this
          # download's own file-name id (see pending_thinkcell_id above), so
          # the log and file name agree -- it is NOT an Export History key
          # (this Excel download isn't logged to history).
          log_line <- tc_build_datasheet_log(
            dashboard_title = tc_ctx_dashboard_title(),
            tab_label       = tc_ctx_active_tab(),
            subtab_label    = tc_ctx_active_subtab(),
            chart_type      = resolved_chart_type,
            selections      = tc_ctx_selections(module_id = id),
            chart_id        = pending_thinkcell_id(),
            source_output   = resolve_opt(source_output),
            source_sheet    = resolve_opt(source_sheet),
            source_mtime    = resolve_opt(source_mtime),
            dictionary_crosswalk = dictionary_crosswalk(),
            dictionary_format = isTRUE(input$dictionary_format)
          )
          write_tc_xlsx(tc_stamp_tc_matrix_corner(tc_table, log_line), file)
        }
      )
    }

    # PowerPoint slide (+ table + log) ZIP download. Shown for any chart that a
    # think-cell template can match. Builds the slide from the same data that
    # backs the table download, so the two never disagree.
    register_slide <- if (shiny::is.reactive(chart_type)) {
      TRUE
    } else {
      tc_template_available(chart_type)
    }

    if (register_slide) {
      # Derives this chart's current exportable state from *live* reactive
      # data (implicitly isolated -- downloadHandler/registry calls run
      # outside a reactive context) -- everything build_export_now() needs
      # to write a ZIP, and everything a bulk regenerate
      # (utils/export_history.R) or a live favorites rebuild
      # (utils/favorites.R) needs to fold this chart into a combined deck
      # without writing a standalone ZIP for it first. raw_data is only
      # consumed by favorites' "raw" bulk download -- build_export_now()
      # and Export History never read it.
      build_export_spec <- function() {
        resolved_chart_type <- resolve_tc_chart_type(chart_type)

        # Underlying table only makes sense for think-cell-supported types.
        if (!is_tc_chart_type_supported(resolved_chart_type)) {
          shiny::showNotification(
            paste0("No think-cell export is available for this chart (type: ",
                   resolved_chart_type, ")."),
            type = "warning"
          )
        }

        tc_data <- tryCatch(
          format_tc_data(
            df = data(),
            chart_type = resolved_chart_type,
            category_col = category_col,
            series_col = series_col,
            value_col = value_col,
            agg_fun = agg_fun,
            category_order = resolved_category_order(),
            series_order = resolved_series_order(),
            waterfall_end_col = waterfall_end_col,
            waterfall_subtotal_cols = waterfall_subtotal_cols,
            facet_col = facet_col
          ),
          error = function(e) NULL
        )

        if (is.null(tc_data)) {
          # Fall back to the raw data so the ZIP is still useful.
          tc_data <- data()
        }

        if (!tc_template_available(resolved_chart_type)) {
          shiny::showNotification(
            paste0("No suitable think-cell template for the displayed chart ",
                   "(type: ", resolved_chart_type, "). ",
                   "The ZIP contains the data table and an explanation, but no slide."),
            type = "warning", duration = 8
          )
        }

        # Determine the template that matches the *displayed* figure from the
        # actual data (e.g. a grouped/stacked bar with one series -> plain bar),
        # and build the slide matrix in the templates' expected orientation.
        slide_type   <- resolved_chart_type
        slide_matrix <- NULL
        if (is.null(facet_col)) {
          prep <- tryCatch(
            tc_prepare_slide(
              df = data(),
              chart_type = resolved_chart_type,
              category_col = category_col,
              series_col = series_col,
              value_col = value_col,
              agg_fun = agg_fun,
              category_order = resolved_category_order(),
              series_order = resolved_series_order()
            ),
            error = function(e) NULL
          )
          if (!is.null(prep)) {
            slide_type   <- prep$chart_type
            slide_matrix <- prep$matrix
          }
        }

        list(
          tc_data = tc_data,
          raw_data = data(),
          chart_type = slide_type,
          slide_matrix = slide_matrix,
          is_faceted = !is.null(facet_col) || is_tc_workbook_list(tc_data),
          slide_title = resolve_opt(slide_title),
          figure_title = resolve_opt(figure_title),
          template_override = slide_effective_override(),
          slide_order = tc_or(input$slide_order, "auto"),
          dashboard_title = tc_ctx_dashboard_title(),
          tab_label = tc_ctx_active_tab(),
          subtab_label = tc_ctx_active_subtab(),
          selections = tc_ctx_selections(module_id = id),
          source_output = resolve_opt(source_output),
          source_sheet = resolve_opt(source_sheet),
          source_mtime = resolve_opt(source_mtime),
          filename_prefix = filename_prefix,
          # Captured here (not recomputed downstream) so a bulk favorites /
          # export-history rebuild logs the same dictionary provenance the
          # single-chart download does -- see tc_build_datasheet_log()'s
          # dictionary_format/dictionary_crosswalk params.
          dictionary_format = isTRUE(input$dictionary_format),
          dictionary_crosswalk = dictionary_crosswalk()
        )
      }

      # Set by the tc_slide_capture observer below, just before it triggers
      # the real (hidden) download -- so by the time build_export_now() runs
      # (synchronously, inside the downloadHandler content() the trigger
      # fires), any client-side screenshot has already arrived. NULL if the
      # capture failed, timed out, or this chart has no plot_output_id --
      # build_export_now() treats that exactly like "no image available".
      pending_slide_capture <- shiny::reactiveVal(NULL)

      # Minted by the same observer, for the same reason: Shiny resolves a
      # downloadHandler's filename() before running its content() function,
      # so the id that will end up in the ZIP's provenance log has to be
      # generated *before* the click flow triggers the real download, not
      # inside build_export_now() as it used to be -- otherwise filename()
      # would have no id to embed yet.
      pending_slide_id <- shiny::reactiveVal(NULL)

      # Builds this chart's slide ZIP from a freshly-derived spec and logs it
      # to Export History. Used by the "Download slide" button below, and
      # (via the chart registry's build_zip) by Export History's own
      # "Regenerate selected" -- which supplies its own `captured_image`
      # (this session's bulk-capture round, or a copy of the entry's last
      # stored snapshot -- see `export_history_regenerate_entry()`) rather
      # than whatever this chart's own button last captured, since the two
      # capture rounds are entirely independent. `missing()`, not a `NULL`
      # default, distinguishes "not supplied" (this chart's own button,
      # which should use its own `pending_slide_capture()`) from "supplied,
      # but no image" (Export History's round found nothing to use either).
      build_export_now <- function(zip_path, favorite_download_id = NULL, captured_image, chart_id_override = NULL) {
        image_to_use <- if (missing(captured_image)) pending_slide_capture() else captured_image
        spec <- build_export_spec()

        # Auto-log this export to the shared Export history tab (see
        # utils/export_history.R), using exactly this already-resolved spec
        # -- not a second, independent re-derivation -- so the history
        # snapshot always matches what's actually downloaded below. Skipped
        # for faceted charts (tc_data is a per-facet list there), same scope
        # limitation favorites_capture() has today.
        chart_id <- NULL
        asset_path <- NULL
        if (!spec$is_faceted) {
          history_entry <- tc_history_capture(
            tc_data           = spec$tc_data,
            chart_type        = spec$chart_type,
            slide_matrix      = spec$slide_matrix,
            raw_data          = spec$raw_data,
            slide_title       = spec$slide_title,
            figure_title      = spec$figure_title,
            template_override = spec$template_override,
            slide_order       = spec$slide_order,
            dashboard_title   = spec$dashboard_title,
            tab_label         = spec$tab_label,
            subtab_label      = spec$subtab_label,
            selections        = spec$selections,
            source_output     = spec$source_output,
            source_sheet      = spec$source_sheet,
            source_mtime      = spec$source_mtime,
            favorite_download_id = favorite_download_id,
            module_id         = id,
            filename_prefix   = spec$filename_prefix,
            dictionary_format = spec$dictionary_format,
            dictionary_crosswalk = spec$dictionary_crosswalk
          )
          # chart_id_override lets a caller mint this download's id *before*
          # calling build_export_now() -- needed so the button's own
          # downloadHandler filename() (which Shiny resolves before running
          # this content function) can embed the exact same id in the
          # downloaded ZIP's file name (see the pending_slide_id reactiveVal
          # below). Regenerate/registry callers omit it and get a fresh id
          # here, same as before.
          history_entry$id <- tc_or(chart_id_override, export_history_new_id())
          chart_id <- export_history_add(history_entry)
          asset_path <- export_history_asset_path(chart_id)
          tc_write_captured_asset(image_to_use, asset_path)
        }

        tc_build_slide_zip(
          zip_path          = zip_path,
          tc_data           = spec$tc_data,
          chart_type        = spec$chart_type,
          raw_data          = spec$raw_data,
          slide_matrix      = spec$slide_matrix,
          slide_title       = spec$slide_title,
          figure_title      = spec$figure_title,
          dashboard_title   = spec$dashboard_title,
          tab_label         = spec$tab_label,
          subtab_label      = spec$subtab_label,
          selections        = spec$selections,
          source_output     = spec$source_output,
          source_sheet      = spec$source_sheet,
          source_mtime      = spec$source_mtime,
          filename_prefix   = spec$filename_prefix,
          template_override = spec$template_override,
          slide_order       = spec$slide_order,
          chart_id          = chart_id,
          favorite_download_id = favorite_download_id,
          dictionary_format = spec$dictionary_format,
          dictionary_crosswalk = spec$dictionary_crosswalk,
          asset_path        = asset_path,
          asset_label       = tc_or(spec$figure_title, tc_or(spec$slide_title, spec$filename_prefix))
        )
        invisible(chart_id)
      }

      # Registered into this session's chart registry (utils/slide_download.R)
      # so Export History's "regenerate" can rebuild this chart later against
      # whatever the dashboard's data looks like *then* -- either as a
      # standalone ZIP (build_zip, solo regenerate) or folded into a combined
      # deck alongside other charts (get_spec, bulk regenerate) -- rather
      # than only ever replaying today's snapshot.
      tc_chart_registry_register(session, id, list(
        build_zip = build_export_now,
        get_spec  = build_export_spec
      ))

      output$slide <- shiny::downloadHandler(
        filename = function() {
          id_part <- tc_or(pending_slide_id(), "")
          paste0(
            filename_prefix, "_slide_",
            if (nzchar(id_part)) paste0(id_part, "_") else "",
            Sys.Date(), ".zip"
          )
        },
        content = function(file) {
          build_export_now(file, chart_id_override = pending_slide_id())
        }
      )
      # This download link lives inside a `display:none` wrapper (see
      # chart_data_downloads_ui()) -- Shiny suspends any output bound to a
      # hidden element by default, which would otherwise leave its href
      # permanently empty/disabled and the button's own click would never
      # actually trigger a download.
      shiny::outputOptions(output, "slide", suspendWhenHidden = FALSE)

      # Written by TC_CHART_CAPTURE_JS's ".tc-slide-go-btn" click handler,
      # either with a real screenshot or `image: NULL` (capture failed,
      # timed out, or no plot_output_id was wired) -- either way, this is the
      # signal to finally trigger the real (hidden) download, exactly once
      # per click. Also mints this download's id here (see pending_slide_id
      # above) -- the earliest point before Shiny resolves output$slide's
      # own filename().
      shiny::observeEvent(input$slide_capture, {
        pending_slide_capture(input$slide_capture$image)
        pending_slide_id(export_history_new_id())
        session$sendCustomMessage("tc_trigger_download", list(download_id = session$ns("slide")))
      }, ignoreInit = TRUE)

      favorite_status_rv <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$favorite, {
        entry <- favorites_capture(
          chart_type        = resolve_tc_chart_type(chart_type),
          slide_title       = resolve_opt(slide_title),
          figure_title      = resolve_opt(figure_title),
          dashboard_title   = tc_ctx_dashboard_title(),
          tab_label         = tc_ctx_active_tab(),
          subtab_label      = tc_ctx_active_subtab(),
          selections        = tc_ctx_selections(module_id = id),
          module_id         = id,
          filename_prefix   = filename_prefix,
          dictionary_format = isTRUE(input$dictionary_format),
          # The slide template chosen for this chart right now (manual pick, or
          # "" for auto-detect) -- persisted on the favorite so a later
          # download uses the template the user actually picked, not whatever
          # the live picker has reset to. See favorites_capture().
          template_override = slide_effective_override()
        )
        favorites_add(entry)
        favorite_status_rv(sprintf("Saved '%s' to favorites.", entry$label))
      })

      output$favorite_status <- shiny::renderUI({
        shiny::req(favorite_status_rv())
        shiny::tags$p(style = "font-size:12px; color:#065F46; margin-top:4px;",
                      favorite_status_rv())
      })
    }
  })
}
