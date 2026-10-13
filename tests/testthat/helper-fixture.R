# A small phipflow project: one group "Cond" with an unpaired comparison
# (A vs B, with DELTA hits and per-feature plots) and a paired one (A vs C,
# with similarity output and no DELTA hits), plus metadata and noxqc JSONs.
make_project <- function(dir = withr::local_tempdir(.local_envir = parent.frame())) {
  svg <- '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10"></svg>'
  put <- function(path, text = svg) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, path)
  }
  res <- file.path(dir, "proj", "results", "Cond")

  meta <- data.frame(
    sample_id = sprintf("R1_%02d", 1:9),
    subject_id = c("s1", "s2", "s3", "s4", "s5", "s6", "s1", "s2", "s9"),
    Cond = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    Age = c(30, 40, 50, 60, 70, 80, 30, 40, 99),
    Sex = c("F", "M", "F", "M", "M", "F", "F", "M", "")
  )
  dir.create(file.path(dir, "proj", "Metadata"), recursive = TRUE)
  utils::write.csv(meta, file.path(dir, "proj", "Metadata", "meta.csv"), row.names = FALSE)

  for (cmp in c("A_vs._B", "A_vs._C")) {
    base <- file.path(res, cmp)
    for (f in c("enrichment_counts.svg", "alpha_diversity/plot.svg", "alpha_diversity/plot_species.svg",
                "beta_diversity/scree_plot.svg", "beta_diversity/pcoa_plot.svg",
                "beta_diversity/dispersion_plot.svg", "beta_diversity/tsne2d_plot.svg",
                "POP_framework/plots/peptide_id_static.svg", "POP_framework/plots/species_static.svg")) {
      put(file.path(base, f))
    }
    put(file.path(base, "beta_diversity", "tsne3d_plot.html"), "<html></html>")
    put(file.path(base, "POP_framework/plots/species_interactive.html"), "<html></html>")
    saveRDS(data.frame(scope = "global", term = "group_char", F_stat = 1.2, R2 = 0.05, p_value = 0.01,
                       n_perm = 999), file.path(base, "beta_diversity", "permanova_results.rds"))
    saveRDS(list(tests = data.frame(term = "dispersion", p_value = 0.5, n_perm = 999)),
            file.path(base, "beta_diversity", "dispersion_results.rds"))
    put(file.path(base, "DELTA_framework", "log.txt"), "  - B per contrast: 1000")
  }

  delta <- data.frame(
    rank = c("species", "species", "species", "genus", "protein_seq_id", "protein_seq_id"),
    feature = c("Virus one", "Virus two", "Virus three", "Genus x", "protseq_1", "protseq_2"),
    protein_annotation = c(NA, NA, NA, NA, "Virus one: capsid", ""),
    design = "unpaired", m_eff = c(10, 10, 10, 10, 10, 10),
    T_obs_stand = c(3, -2, 1, 2, 2.5, 3), p_perm = c(0.001, 0.01, 0.2, 0.02, 0.03, 0.01),
    max_delta = c(0.5, 0.3, 0.1, 0.2, 0.4, 0.4), n_peptides_used = 10, frac_delta_pos = 0.5,
    direction = ifelse(c(3, -2, 1, 2, 2.5, 3) > 0, "More in B", "More in A")
  )
  dd <- file.path(res, "A_vs._B", "DELTA_framework")
  utils::write.csv(delta, file.path(dd, "delta_table.csv"), row.names = FALSE)
  sig <- delta[delta$m_eff > 5 & delta$p_perm < 0.05, ]
  utils::write.csv(sig, file.path(dd, "delta_table_m_eff_gt5_p_perm_lt0.05.csv"), row.names = FALSE)
  for (f in phipflow_slug(sig$feature)) {
    for (s in c("_scatter_static.svg", "_deltaplot_static.svg", "_ecdfplot_static.svg")) {
      put(file.path(dd, "interesting_features", paste0(f, s)))
    }
    put(file.path(dd, "interesting_features", paste0(f, "_scatter_interactive.html")), "<html></html>")
  }

  paired <- transform(delta, design = "paired", p_perm = 0.5)
  dp <- file.path(res, "A_vs._C", "DELTA_framework")
  utils::write.csv(paired, file.path(dp, "delta_table.csv"), row.names = FALSE)
  utils::write.csv(paired[0, ], file.path(dp, "delta_table_m_eff_gt5_p_perm_lt0.05.csv"), row.names = FALSE)
  sim <- expand.grid(x_subject = c("s1", "s2"), y_subject = c("s1", "s2"))
  sim$similarity <- ifelse(sim$x_subject == sim$y_subject, 0.8, 0.2)
  sim$comparison <- factor(ifelse(sim$x_subject == sim$y_subject, "Within individual", "Between individuals"),
                           levels = c("Within individual", "Between individuals"))
  dir.create(file.path(res, "A_vs._C", "similarity"))
  saveRDS(sim, file.path(res, "A_vs._C", "similarity", "similarity_results.rds"))
  for (f in c("boxplot", "combined", "heatmap")) put(file.path(res, "A_vs._C", "similarity", paste0("kulczynski_", f, ".svg")))

  qc <- file.path(dir, "qc")
  dir.create(qc)
  for (i in 1:2) {
    doc <- list(sample = meta$sample_id[i], plate = "R1", type = "Sample",
                thresholds = list(q30_min = 85, adapter_max = 5, min_reads_qc = 10000),
                summary = list(reads_r1 = 1e6 * i, pair_ok = TRUE, q30_r1 = 95, q30_r2 = 93,
                               adapter_max = 0.1, barcode_match = "match",
                               worst_flag = if (i == 1) "OK" else "WARN"))
    jsonlite::write_json(doc, file.path(qc, paste0(meta$sample_id[i], ".fastq_qc.json")), auto_unbox = TRUE)
  }
  list(dir = dir, project = file.path(dir, "proj"), results = dirname(res), qc = qc)
}
