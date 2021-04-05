highlight_defaults = list(
  demx = c(
    setNames(paste0("count/.*l", c("5PE", "3v3", "3v2or5"), "_.*ry.pdf"), c("5PE", "3v3", "3v2or5")),
    "VDJ" = "vdj/.*lVDJ_.*ry.pdf"
  ),
  cite = c(`Doublets` = "0_doublet_rate", `Gex/CITE intersection` = "0_intersection"),
  qctr = c(`Violins` = "*qc_all*ht_id.global.pdf", `Metrics` = "filters_summary", `Scatter` = "0_qc_nCount_RNA"),
  gsea = c(`Heatmap` = "heatmap", `Radar` = "radar"),
  mods = c(`UMAP example` = "umap", `Violin example` = "violin"),
  traj = c(`Trajectory` = "umap|trajectory.*pdf", "Pseudotime" = "pseudotime"),
  tcrs = c(`Clonal expansion` = "dim_red.*/CloneSizeOnFeaturePlot.pdf")
)

preset_titles = list(
  seqr = "Library demultiplexing",
  demx = "Cell demultiplexing",
  cite = "Donor demultiplexing",
  qctr = "Quality control",
  clu = "Clustering",
  gsea = "GSEA",
  dgea = "Differential GE analysis",
  mods = "Module scoring",
  traj = "Trajectory",
  tcrs = "TCR analysis"
)

preset_description = list(
  cite =
"You will make use of the [ab_capture](https://github.com/vijaybioinfo/ab_capture) scripts.

1. Have the donor metadata if you want to include it in the single-cell metadata.
Up yo you if you want it in the object or if you'll make use of it after clustering.

2. Check the hashtag structure is correct (eg. donor~hashtag_n~hashtag_id corresponds to 'DONOR1-TSC5-C0305').
This is derived from the feature names (indicated in the column 'names' when you run Cell Ranger).",
  mult =
"Further doublet detection needs to be performed because using hashtags won't
allow us to eliminate heterotypic (transcriptionally different) doublets
coming from the same patient.",
  qctr =
"We're using [this script](https://github.com/vijaybioinfo/quality_control)
tailored to explore QC metrics.[^2]

1. Be mindful of the variables you have in the metadata.",
  clus =
"You need to be extra careful with this analysis because it branches very easily.
The main variables you want to tweak are:

1. Percentage of variability explained by the highly variable features.
2. Number of principal components.
3. Resolution.",
  clu1 =
"These were carefully selected based on the Singlet's QC metrics. We aim to
exclude the Doublets and Negative.",
  clu2 =
"Using loose quality thresholds and keeping the doublets. The thresholds are
selected based on the global QC metrics distribution.",
  lock =
"After we explore the biology revealed by the clustering analysis, we select a
cluster resolution that gives us the most interesting prospect for a story.",
  gsea =
"**Gene Set Enrichment Analysis** ([GSEA](https://www.gsea-msigdb.org/gsea/index.jsp))
is a computational method that determines whether an a priori defined set of
genes shows statistically significant, concordant differences between
**two biological states** (e.g. phenotypes).",
  mods =
"Briefly, the score is defined for each cell by subtracting the mean expression
of an aggregate of control gene lists from the mean of the signature gene list.
Control gene lists were randomly selected (same size as the signature list)
from bins delimited based on the level of expression of the signature list.",
  gsva =
"**Gene Set Variation Analysis**
([GSVA](https://www.bioconductor.org/packages/release/bioc/html/GSVA.html))
performs a change in coordinate systems, transforming the data from a gene by
sample matrix to a gene set by sample matrix. Thereby allowing for the evaluation
of pathway enrichment for each sample. This transformation is done
**without the use of a phenotype**, thus facilitating very powerful and
open-ended analyses in a now pathway centric manner.",
  metadata =
"One of the most important things about the projects is having the **experimental
design perfectly summarised and understandable**. This includes agreeing on the
samples and donors/subjects names and keep them consistent throughout batches
(harder than you thought, huh?).

<details>
  <summary>Show examples</summary>

  > **Appropriate: Only add 'necessary' information for you to distinguish libraries.**
  <span style='color:red'>102_</span>B2_CD4_STIM_<span style='color:red'>8D</span>, <span style='color:red'>103_</span>B2_CD8_STIM; the first part may not be really necessary (if you allow enough information to make the library unique). and the second name is missing a section (8D).

  > **Consistent: Keep the information in the names in the same order across your runs.**
  B1_CD4_STIM, B2_<span style='color:red'>STIM_CD4</span>.; the cell type and stimulation status changed in the second batch.
</details>"
)

usethis::use_data(highlight_defaults, overwrite = TRUE)
usethis::use_data(preset_titles, overwrite = TRUE)
usethis::use_data(preset_description, overwrite = TRUE)
