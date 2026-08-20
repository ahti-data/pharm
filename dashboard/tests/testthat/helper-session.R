# A plain list(userData = new.env()) stands in for a Shiny session here --
# tc_chart_registry()/*_register()/*_get() only ever touch session$userData,
# so this is a faithful, dependency-free substitute wherever tests need a
# session (export_history and favorites live-rebuild tests both do).
fake_session <- function() list(userData = new.env())
