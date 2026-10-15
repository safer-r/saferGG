# load the dev versions of the dependency packages BEFORE saferGG, so that cross-package
# calls (saferTool::round2, saferGraph::scale2, ...) resolve to the dev versions and not
# to any older installed versions. The paths are resolved from this test directory.
dep_dir <- base::dirname(base::dirname(base::dirname(base::getwd()))) # .../safer-r
for(dep in base::c("saferTool", "saferGraph")){
    pkg_dir <- base::file.path(dep_dir, dep)
    if(base::dir.exists(pkg_dir)){
        base::suppressMessages(expr = tryCatch(expr = pkgload::load_all(path = pkg_dir, quiet = TRUE), error = function(e){NULL}))
    }
}