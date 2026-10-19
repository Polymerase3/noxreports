# noxreports 0.1.2

* The profile plot tab of DELTA features now shows its plot: `style.css` had
  no rule for the `profile` tab, so its panel stayed hidden. Books created
  with 0.1.1 keep their own `style.css` and need the two `profile` lines added
  by hand.

# noxreports 0.1.1

* DELTA feature pages lead with phipflow's profile plot where there is one
  (protein features): prevalence along the protein, as the first tab.
* `pop: ranks` in `report_config.yml` selects which POP scatterplots the
  general analyses show; `noxreports init` writes peptide, species and
  protein_seq_id. Without the key every plotted rank is shown, as before.

# noxreports 0.1.0

* First version: `noxreports init`, `render` and `bundle`; chapters for
  demographics, FastQC, general analyses, repertoire stability and DELTA;
  results linked in place.
