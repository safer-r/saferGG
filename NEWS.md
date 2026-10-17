# saferGG 1.0.0

## Major changes
- Full migration of all functions to the safer-r backbone v20.0: exact `class`/`typeof`/`mode` argument checking through `saferDev::arg_check()` (saferDev 2.0.0 semantics: exact class matching, e.g. `class = c("matrix", "array")`, `double_as_integer_allowed`, per-R-version handling via multiple `arg_check()` calls).
- Internal shared functions now called directly as `saferDev:::.pack_and_function_check()` and `saferDev:::.base_op_check()` (saferDev imported in DESCRIPTION).
- New functions: `gg_donut()`, `gg_heatmap()`, `gg_miami()`, `gg_point_rast()`, `gg_scatter()`.

## Bug fixes
- `gg_box()`: fixed crash of `dot.tidy = TRUE` with non-NULL `dot.categ` (the `binwidth` check also matched the second blank `geom_dotplot` legend layer).
- `gg_scatter()`: fixed wrong `fake_categ` column length when a `geom_hline`/`geom_vline` compartment with `categ = NULL` is not the last data frame of a multi-`data1` call.

## Testing
- Expanded test suite (13 test files) targeting 100% coverage.
- Tests clean up all files and graphic devices they create.
