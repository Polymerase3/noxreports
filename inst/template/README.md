# Editing this report book

This book was created by `noxreports init`. Its figures and tables come from the
phipflow results named in `report_config.yml` (`results_dir`); they are linked,
not copied, so keep the book and the results where they are.

## Render, share

```bash
noxreports render .     # pages go to _book/ (open _book/index.html)
noxreports bundle .     # one zip with _book/ and only the results files it links to
```

On LiSC, run these inside the phipflow container, e.g.
`apptainer exec <phipflow .sif> noxreports render .`. Use `noxreports render`
rather than calling bookdown directly: it also writes the list of linked files
that `bundle` packs.

## Settings: report_config.yml

Comparison labels and questions, their order, group display names, which
comparisons get a stability page, how many DELTA features are shown (`top_n`),
features to always show (`delta: features`), interactive vs static plots, and
the FastQC summary. Re-render after editing.

## Chapters

Every `.Rmd` file here is a chapter, in alphabetical order after `index.Rmd`.

- **Edit** any chapter; the "Key findings" section of `index.Rmd` is meant to
  be written by hand.
- **Add** a chapter by creating e.g. `05-my-analysis.Rmd` starting with a
  heading such as `# My analysis {#my-analysis}`.
- **Switch a chapter off or on** by renaming it: files starting with `_` are
  skipped (`init` leaves `_00-demographics.Rmd` or `_02-stability.Rmd` off when
  their data is missing).

`_common.R` runs before every chapter, so each one has `library(noxreports)`
and `book` (the config with `book$meta`, the metadata, and
`book$comparisons`). In chunks with `results='asis'`, `cat()` the helpers:

```r
for (cmp in book$comparisons) {
  cat(cmp_heading(cmp, 2, "mine"))                      # "## (i) A vs B {#mine-<id>}"
  cat(nox_img(file.path(cmp_base(cmp), "enrichment_counts.svg")), "\n\n")
  cat(nox_table(head(delta_features(cmp)[, c("feature", "p_perm")])), "\n\n")
}
```

Paths given to `nox_img()`, `nox_iframe()`, `nox_plot()` and `nox_link()` are
relative to the results directory; the files they show are the ones `bundle`
packs. See `help(package = "noxreports")` for all helpers.
