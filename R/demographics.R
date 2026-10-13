#' Cohort and demographics
#'
#' Computed from the metadata at render time; nothing is written to the
#' results. Age and sex come from the columns named under `columns` in
#' `report_config.yml`; a `null` column is left out.
#'
#' * `comparison_samples()`: the samples phipflow compared, one row per sample
#'   (`sample`, `group`, `subject`, `age`, `sex`). Like phipflow, paired
#'   comparisons keep only subjects present in both groups. `NULL` when the
#'   group's labels are in no metadata column (`group_col: null`).
#' * `demographics_table()`: samples, subjects, age and sex per group.
#' * `demographics_tests()`: Welch t-test and Wilcoxon rank-sum test for age,
#'   Fisher's exact test for sex; paired comparisons get a note instead, as
#'   both groups are the same people.
#' * `age_plot()`: age per group (box plot with points).
#' * `cohort_table()`: every group label of the book with subjects counted
#'   once, plus all samples together.
#'
#' @param cmp One element of `book$comparisons`.
#' @param book Book loaded by [load_book()].
#' @export
comparison_samples <- function(cmp, book = current_book()) {
  group_col <- book$groups[[cmp$group]]$group_col
  if (is.null(group_col)) return(NULL)
  m <- book$meta
  m <- m[m[[group_col]] %in% c(cmp$g1, cmp$g2), , drop = FALSE]
  subject_col <- book$columns$subject
  if (identical(cmp$design, "paired") && !is.null(subject_col)) {
    both <- intersect(m[[subject_col]][m[[group_col]] == cmp$g1],
                      m[[subject_col]][m[[group_col]] == cmp$g2])
    m <- m[m[[subject_col]] %in% both, , drop = FALSE]
  }
  samples_frame(m, m[[group_col]], book)
}

samples_frame <- function(m, group, book) {
  col <- function(name) if (is.null(name)) rep(NA_character_, nrow(m)) else m[[name]]
  sex <- col(book$columns$sex)
  sex[!is.na(sex) & trimws(sex) == ""] <- NA
  data.frame(sample = m[[1]], group = group, subject = col(book$columns$subject),
             age = suppressWarnings(as.numeric(col(book$columns$age))), sex = sex)
}

#' @rdname comparison_samples
#' @export
demographics_table <- function(cmp, book = current_book()) {
  s <- comparison_samples(cmp, book)
  if (is.null(s)) return(no_plot("The group labels of this comparison are not in the metadata."))
  s$group <- factor(s$group, levels = c(cmp$g1, cmp$g2))
  nox_table(summarise_demographics(s, book))
}

summarise_demographics <- function(s, book) {
  sex_levels <- sort(unique(stats::na.omit(s$sex)))
  rows <- lapply(split(s, s$group), function(x) {
    row <- data.frame(Group = label_group(as.character(x$group[1]), book), `n samples` = nrow(x),
                      check.names = FALSE)
    if (!all(is.na(x$subject))) row$`n subjects` <- length(unique(x$subject))
    if (!is.null(book$columns$age)) {
      a <- x$age[!is.na(x$age)]
      row$`Age mean (SD)` <- if (length(a)) sprintf("%.1f (%.1f)", mean(a), stats::sd(a)) else "NA"
      row$`Age median` <- if (length(a)) fmt_num(stats::median(a), 1) else "NA"
      row$`Age range` <- if (length(a)) paste0(min(a), "-", max(a)) else "NA"
      if (any(is.na(x$age))) row$`Age missing` <- sum(is.na(x$age))
    }
    if (!is.null(book$columns$sex)) {
      for (l in sex_levels) {
        n <- sum(x$sex == l, na.rm = TRUE)
        row[[paste("Sex", l)]] <- sprintf("%d (%.0f%%)", n, 100 * n / nrow(x))
      }
      if (any(is.na(x$sex))) row$`Sex missing` <- sum(is.na(x$sex))
    }
    row
  })
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) { r[setdiff(cols, names(r))] <- 0L; r[cols] })
  do.call(rbind, rows)
}

#' @rdname comparison_samples
#' @export
demographics_tests <- function(cmp, book = current_book()) {
  s <- comparison_samples(cmp, book)
  if (is.null(s)) return("")
  if (identical(cmp$design, "paired")) {
    return(callout(paste0("Both groups are the <strong>same subjects</strong> at two timepoints, ",
                          "so their age and sex do not differ and no test applies.")))
  }
  g1 <- s[s$group == cmp$g1, , drop = FALSE]
  g2 <- s[s$group == cmp$g2, , drop = FALSE]
  out <- character()
  a1 <- stats::na.omit(g1$age)
  a2 <- stats::na.omit(g2$age)
  if (!is.null(book$columns$age) && length(a1) >= 2 && length(a2) >= 2) {
    out <- c(out, sprintf(paste0("<strong>Age:</strong> Welch t-test <code>p = %s</code>, ",
                                 "Wilcoxon rank-sum <code>p = %s</code>."),
                          fmt_p(stats::t.test(a1, a2)$p.value),
                          fmt_p(suppressWarnings(stats::wilcox.test(a1, a2))$p.value)))
  }
  sex <- s[!is.na(s$sex), , drop = FALSE]
  if (!is.null(book$columns$sex) && length(unique(sex$sex)) >= 2 && length(unique(sex$group)) == 2) {
    out <- c(out, sprintf("<strong>Sex:</strong> Fisher's exact test <code>p = %s</code>.",
                          fmt_p(stats::fisher.test(table(sex$group, sex$sex))$p.value)))
  }
  if (!length(out)) return("")
  callout(paste(out, collapse = "<br>"))
}

#' @rdname comparison_samples
#' @export
age_plot <- function(cmp, book = current_book()) {
  s <- comparison_samples(cmp, book)
  if (is.null(s) || is.null(book$columns$age) || all(is.na(s$age))) return("")
  s <- s[!is.na(s$age), , drop = FALSE]
  s$group <- factor(label_group(s$group, book), levels = label_group(c(cmp$g1, cmp$g2), book))
  p <- ggplot2::ggplot(s, ggplot2::aes(.data$group, .data$age, fill = .data$group)) +
    ggplot2::geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.55) +
    ggplot2::geom_jitter(width = 0.12, height = 0, size = 2, alpha = 0.8, colour = "grey20") +
    ggplot2::scale_fill_manual(values = c("#4C72B0", "#C44E52")) +
    ggplot2::labs(x = NULL, y = "Age (years)") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "none")
  inline_plot(p, alt = paste("Age by group:", cmp$label), width = 6, height = 4)
}

#' @rdname comparison_samples
#' @export
cohort_table <- function(book = current_book()) {
  parts <- list()
  for (cmp in book$comparisons) {
    group_col <- book$groups[[cmp$group]]$group_col
    if (is.null(group_col)) next
    m <- book$meta[book$meta[[group_col]] %in% c(cmp$g1, cmp$g2), , drop = FALSE]
    parts[[length(parts) + 1]] <- samples_frame(m, m[[group_col]], book)
  }
  if (!length(parts)) return(no_plot("No group labels of the comparisons are in the metadata."))
  s <- unique(do.call(rbind, parts))
  all <- s
  all$group <- "All samples"
  s <- rbind(s, all)
  s$group <- factor(s$group, levels = unique(s$group))
  # one row per subject (age and sex are recorded per subject)
  if (!all(is.na(s$subject))) {
    n_samples <- table(s$group)
    s <- s[!duplicated(s[c("group", "subject")]), , drop = FALSE]
    t <- summarise_demographics(s, book)
    t$`n samples` <- as.integer(n_samples)
    return(nox_table(t))
  }
  nox_table(summarise_demographics(s, book))
}
