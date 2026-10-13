#' Repertoire stability
#'
#' phipflow's `similarity/similarity_results.rds` (paired comparisons) holds
#' the Kulczynski similarity of every sample pair across the two timepoints,
#' labelled "Within individual" (a subject with themselves) or "Between
#' individuals". `similarity_summary()` summarises both sets, tests the
#' difference (Wilcoxon rank-sum) and gives the ratio of means;
#' `similarity_block()` shows that as a table and a sentence.
#'
#' @param cmp One element of `book$comparisons`.
#' @param book Book loaded by [load_book()].
#' @export
similarity_summary <- function(cmp, book = current_book()) {
  df <- read_rds(file.path(cmp_base(cmp), "similarity", "similarity_results.rds"), book)
  if (is.null(df)) return(NULL)
  type <- as.character(df$comparison)
  w <- df$similarity[type == "Within individual"]
  b <- df$similarity[type == "Between individuals"]
  if (!length(w) || !length(b)) return(NULL)
  list(
    table = data.frame(
      Pairs = c("Within individual", "Between individuals"),
      n = c(length(w), length(b)),
      Mean = c(mean(w), mean(b)),
      Median = c(stats::median(w), stats::median(b)),
      SD = c(stats::sd(w), stats::sd(b)),
      Min = c(min(w), min(b)),
      Max = c(max(w), max(b)),
      check.names = FALSE
    ),
    p_wilcox = suppressWarnings(stats::wilcox.test(w, b))$p.value,
    ratio = mean(w) / mean(b)
  )
}

#' @rdname similarity_summary
#' @export
similarity_block <- function(cmp, book = current_book()) {
  s <- similarity_summary(cmp, book)
  if (is.null(s)) return(no_plot("Similarity results not available."))
  paste0(
    nox_table(signif_cols(s$table)), "\n\n",
    callout(sprintf(paste0(
      "A subject resembles their own other timepoint <strong>%.2fx</strong> more than they ",
      "resemble other subjects (mean Kulczynski similarity %s within individual vs %s between ",
      "individuals; Wilcoxon rank-sum <code>p = %s</code>)."),
      s$ratio, signif(s$table$Mean[1], 3), signif(s$table$Mean[2], 3), fmt_p(s$p_wilcox)))
  )
}
