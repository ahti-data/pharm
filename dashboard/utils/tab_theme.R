#' Colour accents for the shared Favorites / Export history / Manage
#' templates nav tabs, so they visually stand out from the dashboard's own
#' iteration/data tabs at a glance -- these three exist in every dashboard
#' built from this template (see `utils/favorites.R`/`utils/export_history.R`/
#' `utils/template_admin.R`), so a consistent, branded distinction is worth
#' having centrally rather than styled ad hoc per dashboard.

#' Finds this app's nav tabs (top-level, and any same-named nested sub-tab --
#' e.g. a per-iteration "Favorites" sub-tab) by their exact visible text and
#' tags each with a colour class (see [tc_tab_color_css()]). Runs once on
#' `shiny:connected`; tab titles are static text set at UI-build time, so
#' there's nothing to re-run later. Idempotent registration, same pattern as
#' `TC_CHART_CAPTURE_JS` in `utils/chart_downloads.R`.
TC_TAB_COLOR_JS <- r"(
if (!window.__tcTabColorInit) {
  window.__tcTabColorInit = true;
  $(document).on('shiny:connected', function() {
    var colorClass = {
      'Favorites': 'tc-tab-favorites',
      'Export history': 'tc-tab-history',
      'Manage templates': 'tc-tab-templates'
    };
    $('.nav > li > a').each(function() {
      var cls = colorClass[$(this).text().trim()];
      if (cls) $(this).parent('li').addClass(cls);
    });
  });
}
)"

#' Build the `<style>` CSS for [TC_TAB_COLOR_JS]'s three tab classes, picking
#' colours from `branding$scale_discrete` (`data/metadata/brand_colors.R`) --
#' the same palette already used for categorical distinction elsewhere in
#' the dashboard (e.g. a chart's `scale_fill_manual()`), not arbitrary new
#' colours.
#' @param branding The `ahti_branding` list (from `data/metadata/brand_colors.R`).
#' @return A single CSS string.
tc_tab_color_css <- function(branding) {
  pal <- branding$scale_discrete
  sprintf(r"(
.tc-tab-favorites > a { color: %1$s !important; }
.tc-tab-favorites.active > a, .tc-tab-favorites.active > a:hover, .tc-tab-favorites.active > a:focus { background-color: %1$s !important; color: #fff !important; border-color: %1$s !important; }
.tc-tab-history > a { color: %2$s !important; }
.tc-tab-history.active > a, .tc-tab-history.active > a:hover, .tc-tab-history.active > a:focus { background-color: %2$s !important; color: #fff !important; border-color: %2$s !important; }
.tc-tab-templates > a { color: %3$s !important; }
.tc-tab-templates.active > a, .tc-tab-templates.active > a:hover, .tc-tab-templates.active > a:focus { background-color: %3$s !important; color: #fff !important; border-color: %3$s !important; }
)", pal[[1]], pal[[2]], pal[[5]])
}

#' Emit the script + style tags wiring up [TC_TAB_COLOR_JS]/[tc_tab_color_css()]
#' -- call once, anywhere in the app's top-level UI (e.g. right after the
#' outer `tabsetPanel`/`navbarPage`).
#' @param branding The `ahti_branding` list (from `data/metadata/brand_colors.R`).
tc_tab_color_theme <- function(branding) {
  shiny::tagList(
    shiny::tags$script(shiny::HTML(TC_TAB_COLOR_JS)),
    shiny::tags$style(shiny::HTML(tc_tab_color_css(branding)))
  )
}
