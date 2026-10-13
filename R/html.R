#' HTML building blocks for chapters
#'
#' All return a single HTML string for `cat()` in a `results='asis'` chunk.
#'
#' * `nox_img()` shows an image (usually an SVG) from the results.
#' * `nox_iframe()` embeds an interactive HTML file from the results.
#' * `nox_plot()` uses the interactive version when `interactive: true` in
#'   `report_config.yml` and it exists, otherwise the static one.
#' * `nox_link()` links to a results file (e.g. a CSV to download).
#' * `nox_table()` formats a data frame as a report table.
#' * `callout()` is a highlighted box (`type` "note", "warn" or "none").
#' * `no_plot()` is the placeholder for missing output.
#' * `inline_plot()` embeds a ggplot as an SVG data URI, so no file is written.
#'
#' @param rel,html,svg Paths relative to the results directory.
#' @param alt,title,text Alternative text, iframe title, link text.
#' @param book Book loaded by [load_book()].
#' @export
nox_img <- function(rel, alt = basename(rel), book = current_book()) {
  src <- use_file(rel, book)
  if (is.null(src)) return(no_plot(paste("Not available:", rel)))
  sprintf('<div class="plot-square"><img src="%s" alt="%s" loading="lazy"></div>',
          src, htmltools::htmlEscape(alt, attribute = TRUE))
}

#' @rdname nox_img
#' @export
nox_iframe <- function(rel, title = basename(rel), book = current_book()) {
  src <- use_file(rel, book)
  if (is.null(src)) return(no_plot(paste("Not available:", rel)))
  sprintf('<div class="plot-square"><iframe src="%s" class="feature-iframe" title="%s" loading="lazy"></iframe></div>',
          src, htmltools::htmlEscape(title, attribute = TRUE))
}

#' @rdname nox_img
#' @export
nox_plot <- function(html, svg, alt = basename(svg), book = current_book()) {
  if (isTRUE(book$interactive) && file.exists(res_fs(html, book))) {
    return(nox_iframe(html, alt, book))
  }
  nox_img(svg, alt, book)
}

#' @rdname nox_img
#' @export
nox_link <- function(rel, text = basename(rel), book = current_book()) {
  href <- use_file(rel, book)
  if (is.null(href)) return("")
  sprintf('<a href="%s">%s</a>', href, htmltools::htmlEscape(text))
}

#' @rdname nox_img
#' @param df Data frame.
#' @export
nox_table <- function(df) {
  paste(knitr::kable(df, format = "html", escape = TRUE, row.names = FALSE,
                     table.attr = 'class="summary-table"'), collapse = "\n")
}

#' @rdname nox_img
#' @param type Callout style: "note", "warn" or "none".
#' @export
callout <- function(text, type = "note") {
  sprintf('<div class="callout callout-%s">%s</div>', type, text)
}

#' @rdname nox_img
#' @param msg Message shown in the placeholder.
#' @export
no_plot <- function(msg) {
  sprintf('<div class="no-plot">%s</div>', htmltools::htmlEscape(msg))
}

#' @rdname nox_img
#' @param plot A ggplot object.
#' @param width,height Size in inches.
#' @export
inline_plot <- function(plot, alt = "", width = 7, height = 4.5) {
  svg <- svglite::svgstring(width = width, height = height, standalone = TRUE)
  print(plot)
  grDevices::dev.off()
  sprintf('<div class="plot-inline"><img src="data:image/svg+xml;base64,%s" alt="%s"></div>',
          jsonlite::base64_enc(svg()), htmltools::htmlEscape(alt, attribute = TRUE))
}
