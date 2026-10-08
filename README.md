# Multimorbidity Risk Network

Research code for multimorbidity network construction, disease risk patterns and patient profiling.

**Publication:** Xiaochen Wang, Runtong Zhang and Xiaomin Zhu. [*What can we learn from multimorbidity? A deep dive from its risk patterns to the corresponding patient profiles*](https://doi.org/10.1016/j.dss.2024.114313). *Decision Support Systems*.

> **Reproducibility status:** These are original research scripts and notebooks, **not** an end-to-end automated pipeline. Several files use hard-coded `G:/...` paths and require authorized clinical data and intermediate files.

## Research workflow

Clinical data preparation → missing-value handling → multimorbidity matrices → network construction → network pattern analysis → patient-level models and interpretation.

## Code guide

| Stage | Files |
| --- | --- |
| Data preparation | `Python code/Preprocessing.ipynb`, `R code/Imputation_of_missing_values.R` |
| Disease networks | `Python code/multimorbidity_risk_network.ipynb`, `R code/network_construction.R` |
| Matrix statistics | `R code/descriptive_statistics_of_matrices.R` |
| Structural analysis | `Python code/assortativity_coefficient_and_homophily.ipynb`, `Python code/dense_subgraphs_mining.ipynb`, `R code/pattern_recognition_of_the_MRN.R` |
| Patient-level modeling | `Python code/model_development_and_interpretable_learning.ipynb` |
| Disease categories | `Python code/Distribution_of_disease_categories.ipynb` |

See [Supplementary Material](Supplementary%20Material.pdf) for additional study details.

## Data and environment

Some analyses refer to the [eICU Collaborative Research Database](https://physionet.org/content/eicu-crd/) and derived CSV/matrix files that are **not** distributed here. Researchers must obtain data through the authorized provider and comply with applicable access terms. Never commit patient-level exports or credentials.

Python imports across the notebooks include pandas, numpy, scipy, networkx, scikit-learn, matplotlib, holoviews, shap, hcuppy and dsd. R scripts use packages including tidyverse, igraph, mice, VIM, corrplot, readxl, circlize and openxlsx. This inventory does **not** establish validated package versions.

To explore the analysis: configure a local authorized data workspace; replace `G:/...` paths; follow the preprocessing and matrix construction steps before downstream network and patient analyses; validate intermediate column schemas. There is presently no tested one-command execution path or dependency lockfile.

## Citation

If this work is useful to your research, please cite the [associated article](https://doi.org/10.1016/j.dss.2024.114313). Machine-readable metadata are in [CITATION.cff](CITATION.cff).

## Disclaimer

Research use only; not a diagnostic or clinical decision system. Reproduction depends on access-controlled data, preprocessing decisions and original study assumptions.