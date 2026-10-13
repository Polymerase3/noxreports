usage <- "Usage: noxreports <command> [options]

Commands:
  init     Create <project>_book/ next to the phipflow results
             --metadata FILE    phipflow metadata CSV (required; first column = sample IDs)
             --results DIR      phipflow results directory [results]
             --project NAME     project name [directory above --results]
             --out DIR          book directory [<project>_book next to --results]
             --title TEXT       report title [\"<project> PhIP-seq report\"]
             --author TEXT      author shown on the first page
             --age-col COL      metadata column with age [case-insensitive 'age']
             --sex-col COL      metadata column with sex [case-insensitive 'sex' or 'gender']
             --stability MODE   auto: stability pages where phipflow ran it; no: none [auto]
             --top-n N          DELTA features per rank and direction [15]
             --min-m-eff X      phipflow's delta_min_m_eff [read from the DELTA file names]
             --features CSV     features to always show (columns comparison, feature[, rank])
             --force            overwrite the template files and report_config.yml
  render   Render the book:   noxreports render [BOOK_DIR]
  bundle   Zip the rendered book with the results files it links to:
                              noxreports bundle [BOOK_DIR] [--out FILE]
  help     Show this message
"

cli_spec <- list(
  init = c("metadata", "results", "project", "out", "title", "author", "age-col", "sex-col",
           "stability", "top-n", "min-m-eff", "features", "force"),
  render = character(),
  bundle = "out"
)
cli_flags <- "force"

#' Command-line entry point
#'
#' `noxreports <command> [options]`; see `noxreports help`. Installed as
#' `exec/noxreports`; also callable as `Rscript -e 'noxreports::main()' ...`.
#'
#' @param args Command-line arguments.
#' @export
main <- function(args = commandArgs(trailingOnly = TRUE)) {
  tryCatch(run_cli(args), error = function(e) {
    message("Error: ", conditionMessage(e))
    if (!interactive()) quit(save = "no", status = 1)
    invisible(NULL)
  })
}

run_cli <- function(args) {
  p <- parse_args(args)
  if (is.null(p$command) || p$command %in% c("help", "--help", "-h")) {
    cat(usage)
    return(invisible(NULL))
  }
  if (!p$command %in% names(cli_spec)) stop("Unknown command '", p$command, "'.\n", usage, call. = FALSE)
  unknown <- setdiff(names(p$options), cli_spec[[p$command]])
  if (length(unknown)) {
    stop("Unknown option(s) for ", p$command, ": --", paste(unknown, collapse = ", --"), call. = FALSE)
  }
  o <- stats::setNames(p$options, gsub("-", "_", names(p$options)))
  switch(p$command,
    init = {
      if (length(p$positional)) stop("init takes no positional arguments.", call. = FALSE)
      if (is.null(o$metadata)) stop("init needs --metadata.", call. = FALSE)
      if (!is.null(o$top_n)) o$top_n <- as.integer(o$top_n)
      if (!is.null(o$min_m_eff)) o$min_m_eff <- as.numeric(o$min_m_eff)
      do.call(nox_init, o)
    },
    render = nox_render(p$positional[1] %|NA|% "."),
    bundle = nox_bundle(p$positional[1] %|NA|% ".", out = o$out)
  )
  invisible(NULL)
}

`%|NA|%` <- function(x, y) if (is.na(x)) y else x

# --key value, --key=value and boolean flags (cli_flags); the first other
# argument is the command, the rest are positional.
parse_args <- function(args) {
  out <- list(command = NULL, options = list(), positional = character())
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    if (startsWith(a, "--") && !a %in% c("--help")) {
      key <- sub("^--", "", a)
      if (grepl("=", key, fixed = TRUE)) {
        out$options[[sub("=.*", "", key)]] <- sub("^[^=]*=", "", key)
      } else if (key %in% cli_flags) {
        out$options[[key]] <- TRUE
      } else {
        if (i == length(args)) stop("Option --", key, " needs a value.", call. = FALSE)
        out$options[[key]] <- args[i + 1]
        i <- i + 1
      }
    } else if (is.null(out$command)) {
      out$command <- a
    } else {
      out$positional <- c(out$positional, a)
    }
    i <- i + 1
  }
  out
}
