# noxreports

Editable [bookdown](https://bookdown.org) reports for
[phipflow](https://github.com/Vogl-Lab-Research/phipflow) PhIP-seq results.

`noxreports init` creates a book directory next to the phipflow results and
`noxreports render` turns it into an HTML gitbook:

1. **About this report**: cohort numbers, an optional FastQC summary, the
   comparisons at a glance (PERMANOVA, stability, DELTA counts) and a "Key
   findings" section to write by hand.
2. **Cohort and demographics**: age and sex for the cohort and per comparison,
   with tests for unpaired comparisons (only when the metadata has age or sex).
3. **General analyses**: enrichment, alpha and beta diversity and POP per
   comparison (and for `all/` when phipflow ran `--all TRUE`).
4. **Repertoire stability**: within- vs between-individual similarity for the
   paired (longitudinal) comparisons (only when phipflow produced it).
5. **DELTA overview** and **DELTA features**: the selected features of each
   comparison with their statistics and plots.

The results are **linked, not copied**: a book adds about 40 files next to the
results. `noxreports bundle` zips the rendered book with only the results files
it links to, for sharing.

## Use on LiSC

noxreports is installed in the phipflow container, so there is nothing to
install on LiSC. Replace `<project>` with your phipflow project and
`<your_dir>` with your own scratch directory:

```bash
# on ccr-lab
SIF=/lisc/data/scratch/ccr/CR_projects/phipflow/containers/phipflow-r4.6.0-quarto1.9.37.sif
P=/lisc/data/scratch/ccr/CR_projects/phiper/<project>
OUT=/lisc/data/scratch/ccr/<your_dir>/<project>_book

# 1. create the editable book (seconds)
apptainer exec $SIF noxreports init \
  --results $P/results \
  --metadata $P/Metadata/<project>_metadata.csv \
  --out $OUT

# 2. render it on a compute node (a few minutes)
srun --mem=8G --time=00:30:00 apptainer exec $SIF noxreports render $OUT

# 3. zip the book with only the results files it links to
apptainer exec $SIF noxreports bundle $OUT
```

Without `--out` the book goes to `<project>_book/` in the project directory,
next to `results/`. On LiSC the FastQC summary is filled in automatically.

To open it on your computer, copy the zip and unzip it. `bundle` prints where
`index.html` is inside it:

```bash
scp <user>@ccr-lab.lisc.univie.ac.at:<OUT>/<project>_book.zip .
unzip <project>_book.zip
# e.g. open ccr/<your_dir>/<project>_book/_book/index.html
```

To change the book, edit `report_config.yml` or the `.Rmd` files in `$OUT`
(or add your own), then repeat steps 2 and 3.

For a new phipflow run, `--noxreport true` builds the book as the pipeline's
last step.

The container has the noxreports version of the latest phipflow release.
Changes to this repository reach LiSC with the next phipflow Release and the
nightly rebuild of the `.sif`.

## Install

```r
pak::pak("Polymerase3/noxreports")
```

It is also installed in the phipflow container image, with `noxreports` on the
`PATH`.

## Use

From the phipflow project directory (`<base_dir>/<project>/`, holding
`results/` and `Metadata/`):

```bash
noxreports init --metadata Metadata/<project>_metadata.csv   # creates <project>_book/
noxreports render <project>_book                              # -> <project>_book/_book/index.html
noxreports bundle <project>_book                              # -> <project>_book/<project>_book.zip
```

Outside the container, `Rscript -e 'noxreports::main()' <command> ...` works
the same way. `noxreports help` lists all options.

### init options

| Option | Default | |
|---|---|---|
| `--metadata FILE` | required | phipflow metadata CSV; first column = sample IDs |
| `--results DIR` | `results` | phipflow results directory |
| `--project NAME` | directory above `--results` | title, FastQC link, book name |
| `--out DIR` | `<project>_book` next to `--results` | book directory |
| `--title`, `--author` | | first page |
| `--age-col`, `--sex-col` | `age`, `sex`/`gender` (any case) | metadata columns |
| `--stability auto\|no` | `auto` | stability pages where phipflow ran the similarity analysis |
| `--top-n N` | 15 | DELTA features per rank and direction |
| `--min-m-eff X` | from the DELTA file names | phipflow's `delta_min_m_eff` |
| `--features CSV` | | features to always show: `comparison,feature[,rank]` |
| `--force` | | overwrite the template files and `report_config.yml` of an existing book |

## What goes into the book

**Comparisons** are the `<group>/<g1>_vs._<g2>/` directories of the results.
A comparison is paired when DELTA says so (`design` in `delta_table.csv`), and
gets a stability page when phipflow wrote `similarity/similarity_results.rds`
(phipflow does this for longitudinal comparisons).

**DELTA features**: phipflow's own selection, recomputed from
`delta_table.csv`. Features with `m_eff > min_m_eff` and `p_perm < 0.05`,
scored per rank as `rescale01(|T_obs_stand|) * rescale01(max_delta)`; the
`top_n` best per rank and direction are shown. Features listed under
`delta: features` in `report_config.yml` (or given with `--features`) are
always shown, also when they do not pass the filter.

**Age and sex** come from the metadata columns; nothing is read from noxDB.

**FastQC**: noxqc's per-sample JSON files
(`/lisc/data/work/ccr/mariaDB/fastq_qc/json/<sample>.fastq_qc.json`), matched
on the metadata's sample IDs. Where that directory is not readable (anywhere but
LiSC), the summary is left out with a message.

## Editing a book

Everything is set in `report_config.yml` (labels, questions, order of the
comparisons, which have stability pages, `top_n`, manual features,
`interactive: false` for static plots and a smaller bundle) and in the `.Rmd`
chapters, which can be edited, added or switched off (prefix `_`). See the
`README.md` that `init` puts in each book.
