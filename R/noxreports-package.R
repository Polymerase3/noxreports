#' @keywords internal
#' @importFrom ggplot2 .data
"_PACKAGE"

# Package state for the R session that renders a book:
#   book  - the config loaded by load_book(), used as the default by helpers
#   key   - cache key of that config (path + modification time)
#   cache - tables read from the results (delta tables, ...), keyed by path
#   refs  - results files linked from the rendered pages (the bundle manifest)
.nox <- new.env(parent = emptyenv())
.nox$cache <- new.env(parent = emptyenv())
.nox$refs <- character()

`%||%` <- function(x, y) if (is.null(x)) y else x

reset_state <- function() {
  .nox$book <- NULL
  .nox$key <- NULL
  .nox$cache <- new.env(parent = emptyenv())
  .nox$refs <- character()
}

cached <- function(key, compute) {
  if (!exists(key, envir = .nox$cache, inherits = FALSE)) {
    assign(key, compute(), envir = .nox$cache)
  }
  get(key, envir = .nox$cache, inherits = FALSE)
}
