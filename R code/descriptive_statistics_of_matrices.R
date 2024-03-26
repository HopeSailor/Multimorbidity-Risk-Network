path <- "G:/共病/数据/"
file_name <- "prevalence_matrix.csv"
prevalence_matrix <- read.csv(paste0(path, file_name), row.names = 1, check.names = FALSE)
file_name <- "severity_matrix.csv"
severity_matrix <- read.csv(paste0(path, file_name), row.names = 1, check.names = FALSE)
file_name <- "complexity_matrix.csv"
complexity_matrix <- read.csv(paste0(path, file_name), row.names = 1, check.names = FALSE)
file_name <- "multimorbidity_matrix.csv"
multimorbidity_matrix <- read.csv(paste0(path, file_name), row.names = 1, check.names = FALSE)
# Prevalence
prevalence_upper_triangle <- prevalence_matrix[upper.tri(prevalence_matrix, diag = FALSE)]
prevalence_num_zeros <- sum(prevalence_upper_triangle == 0)
prevalence_upper_triangle_nonzero <- prevalence_upper_triangle[prevalence_upper_triangle != 0]
prevalence_summary <- summary(prevalence_upper_triangle_nonzero)
prevalence_sd <- sd(prevalence_upper_triangle_nonzero)
print(prevalence_summary)
cat("Standard Deviation:", prevalence_sd, "\n")
# Severity
severity_upper_triangle <- severity_matrix[upper.tri(severity_matrix, diag = FALSE)]
severity_num_zeros <- sum(severity_upper_triangle == 0)
severity_upper_triangle_nonzero <- severity_upper_triangle[severity_upper_triangle != 0]
severity_summary <- summary(severity_upper_triangle_nonzero)
severity_sd <- sd(severity_upper_triangle_nonzero)
print(severity_summary)
cat("severity Standard Deviation:", severity_sd, "\n")
# Complexity
complexity_upper_triangle <- complexity_matrix[upper.tri(complexity_matrix, diag = FALSE)]
complexity_num_zeros <- sum(complexity_upper_triangle == 0)
complexity_upper_triangle_nonzero <- complexity_upper_triangle[complexity_upper_triangle != 0]
complexity_summary <- summary(complexity_upper_triangle_nonzero)
complexity_sd <- sd(complexity_upper_triangle_nonzero)
print(complexity_summary)
cat("Complexity Standard Deviation:", complexity_sd, "\n")