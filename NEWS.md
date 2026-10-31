# saferGG 1.0.0

## Major changes
- Backbone v21.1: the invented `x` argument names (absent from the called function signature, for instance `base::any(x = ...)`, `base::all(x = ...)`, `base::sum(x = ...)`, `base::order(x = ...)`) are removed from all the function calls.
- Backbone v21.2: the lib_path argument is now checked whatever the value of safer_check (the `if(safer_check == TRUE){` and `}` lines around the check of lib_path are commented, because lib_path is used outside the safer_check checkings, for instance in the `saferDev::arg_check()` calls): an incorrect lib_path now returns an error even with `safer_check = FALSE`. The documentation of the safer_check argument no longer lists the lib_path check, and the documentation of the lib_path argument no longer says that lib_path is ignored if safer_check is FALSE.
- gg_scatter(): the check of lib_path now adds lib_path to the initial library paths (as in the other functions), instead of replacing them by lib_path, which dropped the user library paths.
- Full migration of all functions to the safer-r backbone v21.0: exact `class`/`typeof`/`mode` argument checking through `saferDev::arg_check()` (saferDev 2.0.0 semantics: exact class matching, e.g. `class = c("matrix", "array")`, `double_as_integer_allowed`, per-R-version handling via multiple `arg_check()` calls).
- Internal shared functions now called directly as `saferDev:::.pack_and_function_check()` and `saferDev:::.base_op_check()` (saferDev imported in DESCRIPTION).
- New functions: `gg_donut()`, `gg_heatmap()`, `gg_miami()`, `gg_point_rast()`, `gg_scatter()`.

## Bug fixes
- `gg_box()`: fixed crash when the `add` argument contains layers (`geom_hline()`, `geom_abline()`: "replacement has 0 rows"; `annotate()`: "Discrete value supplied to a continuous scale"). The layers of `add` are now drawn above the boxes and dots, while its other components (facets, scales, theme, labs) are used as before.
- `gg_scatter()`: fixed "INTERNAL ERROR 5" with `return = TRUE` when the `add` argument contains layers (`geom_hline()`, `geom_vline()`, `geom_abline()`, `annotate()`). The layers of `add` are now drawn above the other layers and their data are not returned in the `$plot` compartment.
- `gg_box()`: fixed crash of `dot.tidy = TRUE` with non-NULL `dot.categ` (the `binwidth` check also matched the second blank `geom_dotplot` legend layer).
- `gg_scatter()`: fixed wrong `fake_categ` column length when a `geom_hline`/`geom_vline` compartment with `categ = NULL` is not the last data frame of a multi-`data1` call.

## Testing
- Expanded test suite (13 test files) targeting 100% coverage.
- Tests clean up all files and graphic devices they create.
