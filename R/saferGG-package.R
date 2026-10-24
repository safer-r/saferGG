#' @keywords internal
#' @importFrom lemon facet_rep_grid
#' @importFrom lemon facet_rep_wrap
"_PACKAGE"

# lemon is called through the add argument strings of gg_box(), gg_donut() and gg_scatter() (e.g., add = "+ lemon::facet_rep_wrap(...)"), hence the imports above
# .x is the pronoun of the expressions passed to scales::math_format() (e.g., scales::math_format(expr = 10^.x, format = base::force)), not a global variable
utils::globalVariables(names = base::c(".x"), package = "saferGG", add = TRUE)
