#' FastQC summary
#'
#' noxqc writes one JSON per sample to
#' `<qc_root>/<sample name>.fastq_qc.json` on LiSC; its `summary` holds the
#' sample's row of noxDB's `sample_fastq_qc` table. The sample names are the
#' first metadata column, as in noxDB.
#'
#' `read_fastqc()` returns `NULL`, with a message, when the summary is
#' disabled, `qc_root` cannot be read (e.g. when rendering away from LiSC) or no
#' sample has a result. Otherwise a list with `samples` (one row per checked
#' sample), `missing` (sample names without a result) and `thresholds`.
#'
#' `fastqc_tile()` turns that into a short section (the summary sentences of
#' noxqc's own report, a plate table and links to the full QC report), or `""`.
#'
#' @param book Book loaded by [load_book()].
#' @export
read_fastqc <- function(book = current_book()) {
  qc <- book$fastqc
  if (!isTRUE(qc$enabled)) return(NULL)
  if (!dir.exists(qc$qc_root) || file.access(qc$qc_root, 4) != 0) {
    message("FastQC: ", qc$qc_root, " is not readable here, so the FastQC summary is left out ",
            "(it is available when rendering on LiSC).")
    return(NULL)
  }
  names <- unique(book$meta[[1]])
  files <- file.path(qc$qc_root, paste0(names, ".fastq_qc.json"))
  have <- file.exists(files)
  if (!any(have)) {
    message("FastQC: none of the ", length(names), " samples has a result in ", qc$qc_root,
            ", so the FastQC summary is left out.")
    return(NULL)
  }
  docs <- lapply(files[have], jsonlite::read_json)
  value <- function(x) if (is.null(x)) NA else x
  samples <- do.call(rbind, lapply(docs, function(d) {
    s <- d$summary
    data.frame(sample = d$sample, plate = value(d$plate), type = value(d$type),
               worst_flag = value(s$worst_flag), pair_ok = value(s$pair_ok),
               reads = value(s$reads_r1), q30_r1 = value(s$q30_r1), q30_r2 = value(s$q30_r2),
               adapter_max = value(s$adapter_max), barcode_match = value(s$barcode_match))
  }))
  samples$worst_flag[is.na(samples$worst_flag)] <- "OK"
  list(samples = samples, missing = names[!have], thresholds = docs[[1]]$thresholds)
}

#' @rdname read_fastqc
#' @export
fastqc_tile <- function(book = current_book()) {
  qc <- read_fastqc(book)
  if (is.null(qc)) return("")
  s <- qc$samples
  thr <- qc$thresholds
  ok <- s[!is.na(s$reads) & s$reads >= (thr$min_reads_qc %||% 10000), , drop = FALSE]
  n_total <- nrow(s) + length(qc$missing)
  fail <- s$sample[s$worst_flag == "FAIL"]

  sent <- c(
    sprintf("<strong>Checked:</strong> %d of %d samples have a FastQC result%s.", nrow(s), n_total,
            if (length(qc$missing)) sprintf("; %d are not checked yet", length(qc$missing)) else ""),
    sprintf("<strong>File integrity:</strong> %d of %d have intact, matching R1/R2 files.",
            sum(s$pair_ok %in% TRUE), nrow(s)),
    sprintf("<strong>%d sample(s) with a FAIL</strong> and <strong>%d with a WARN</strong> as their worst flag%s.",
            length(fail), sum(s$worst_flag == "WARN"),
            if (length(fail)) paste0(" (FAIL: ", htmltools::htmlEscape(paste(fail, collapse = ", ")), ")") else "")
  )
  smp <- s[s$type %in% "Sample" & !is.na(s$reads), , drop = FALSE]
  if (nrow(smp)) {
    big <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
    pm <- tapply(smp$reads, smp$plate, stats::median)
    sent <- c(sent, sprintf("<strong>Depth:</strong> the median sample has %s read pairs%s.",
                            big(stats::median(smp$reads)),
                            if (length(pm) > 1) sprintf(" (plate medians %s to %s)", big(min(pm)), big(max(pm))) else ""))
  }
  if (any(!is.na(ok$q30_r1))) {
    sent <- c(sent, sprintf("<strong>Quality:</strong> median %%Q30 is %.0f%% (R1) and %.0f%% (R2); WARN below %s%%.",
                            stats::median(ok$q30_r1, na.rm = TRUE), stats::median(ok$q30_r2, na.rm = TRUE),
                            thr$q30_min %||% 85))
  }
  if (any(!is.na(ok$adapter_max))) {
    sent <- c(sent, sprintf("<strong>Adapters:</strong> highest adapter content %.2f%% (trimming needed above %s%%).",
                            max(ok$adapter_max, na.rm = TRUE), thr$adapter_max %||% 5))
  }
  bc <- ok[ok$barcode_match %in% c("match", "mismatch"), , drop = FALSE]
  if (nrow(bc)) {
    sent <- c(sent, sprintf("<strong>Barcodes:</strong> %d of %d samples with a barcode on the run sheet carry it as their main index.",
                            sum(bc$barcode_match == "match"), nrow(bc)))
  }

  types <- intersect(c("Sample", "Mock", "Anchor", "NC", "Input"), s$type)
  plates <- lapply(split(s, s$plate), function(x) {
    row <- data.frame(Plate = x$plate[1])
    for (t in types) row[[t]] <- sum(x$type == t)
    for (f in c("FAIL", "WARN", "INFO", "OK")) row[[f]] <- sum(x$worst_flag == f)
    row
  })

  qc_url <- book$fastqc$url
  report <- sprintf("%s/qc/report?project=%s", qc_url,
                    utils::URLencode(book$fastqc$project, reserved = TRUE))
  paste0(
    "\n\n## Sequencing quality (FastQC) {#fastqc}\n\n",
    "<ul>", paste0("<li>", sent, "</li>", collapse = ""), "</ul>\n\n",
    nox_table(do.call(rbind, plates)), "\n\n",
    sprintf(paste0('Full report: <a href="%s">FastQC report for %s</a> &middot; ',
                   '<a href="%s/d/noxdb-fastq-qc">Grafana FastQC dashboard</a> (LiSC login needed).\n\n'),
            report, htmltools::htmlEscape(book$fastqc$project), qc_url)
  )
}
