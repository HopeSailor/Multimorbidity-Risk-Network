This repository contains the official research implementation of our paper. If you find this work useful for your research, please cite:

> Xiaochen Wang, Runtong Zhang<sup>\*</sup>, and Xiaomin Zhu. [**What can we learn from multimorbidity? A deep dive from its risk patterns to the corresponding patient profiles**](https://doi.org/10.1016/j.dss.2024.114313). *Decision Support Systems*, 2024, 186: 114313.

## Introduction

We investigate how disease risk patterns in multimorbidity can be analyzed with network methods and related to patient profiles. The study constructs a **Multimorbidity Risk Network (MRN)** by combining multimorbidity prevalence, severity and complexity, then examines network patterns and patient characteristics.

The research workflow covers: (1) clinical data preprocessing; (2) construction of multimorbidity matrices and the disease-risk network; (3) analysis of network structure, communities, and dense subgraphs; and (4) patient-level modeling and interpretability.

## Dataset

Some analyses use the [eICU Collaborative Research Database (eICU-CRD)](https://physionet.org/content/eicu-crd/) and additional study-specific processed files. Researchers must obtain authorized data access from the provider and comply with all applicable data-use requirements.

The source datasets and derived patient-level records are **not** bundled as a runnable public benchmark. Avoid committing raw medical records, sensitive attributes, credentials or unauthorized derivatives.

## Project Structure

```text
├── Python code/
│   ├── Preprocessing.ipynb
│   ├── multimorbidity_risk_network.ipynb
│   ├── assortativity_coefficient_and_homophily.ipynb
│   ├── dense_subgraphs_mining.ipynb
│   ├── model_development_and_interpretable_learning.ipynb
│   └── Distribution_of_disease_categories.ipynb
├── R code/
│   ├── Imputation_of_missing_values.R
│   ├── network_construction.R
│   ├── descriptive_statistics_of_matrices.R
│   └── pattern_recognition_of_the_MRN.R
├── Supplementary Material.pdf
├── CITATION.cff
└── README.md
```

## Method

The analysis is organized around a progression from clinical data to multimorbidity networks and patient profiles:

1. **Preprocessing and imputation:** extract clinical features and handle missing values.
2. **Network construction:** derive multimorbidity matrices and build disease-level nodes and edges.
3. **Pattern recognition:** inspect community structure, assortativity, homophily, centrality, and dense subgraphs.
4. **Patient profiling:** use machine-learning and interpretability analyses to examine patient characteristics across patterns.

See the [Supplementary Material](Supplementary%20Material.pdf) for further research details.

## How to Run

The released research code consists of notebooks in `Python code/` and R scripts in `R code/`; it is **not** a single executable pipeline.

1. Obtain authorized eICU-CRD access and prepare the study-specific input files.
2. Install a suitable Python/Jupyter and R environment. Python imports include `pandas`, `numpy`, `scipy`, `networkx`, `scikit-learn`, `matplotlib`, `holoviews`, `shap`, `hcuppy`, and `dsd`; R scripts use `tidyverse`, `igraph`, `mice`, `VIM`, `corrplot`, `readxl`, `circlize`, and `openxlsx`.
3. Replace experiment-specific absolute paths such as `G:/...` with paths in your own authorized data workspace.
4. Run and validate preprocessing before network analyses, and verify required intermediate input/output schemas before patient-profile analyses.

For orientation:

| Stage | Research files |
| --- | --- |
| Clinical data preparation | `Python code/Preprocessing.ipynb`, `R code/Imputation_of_missing_values.R` |
| Network construction | `Python code/multimorbidity_risk_network.ipynb`, `R code/network_construction.R` |
| Matrix summaries | `R code/descriptive_statistics_of_matrices.R` |
| Network patterns | `Python code/assortativity_coefficient_and_homophily.ipynb`, `Python code/dense_subgraphs_mining.ipynb`, `R code/pattern_recognition_of_the_MRN.R` |
| Patient profiling | `Python code/model_development_and_interpretable_learning.ipynb` |
| Disease categories | `Python code/Distribution_of_disease_categories.ipynb` |

## Reproducibility Notes

This repository is an archive of original research scripts, not a fully reproducible package. The notebooks contain hard-coded local paths and rely on access-controlled datasets and generated intermediate files. Exact original dependency versions, a clean-environment installation recipe and end-to-end automated tests have not been established. No experiment has been re-run as part of this documentation update.

## Citation and Data Availability

Please cite the [published article](https://doi.org/10.1016/j.dss.2024.114313); machine-readable citation metadata are in [CITATION.cff](CITATION.cff). Source data are governed by their original data providers and may require controlled access. This software is for research purposes only and is not a validated clinical decision-support or diagnostic system.
