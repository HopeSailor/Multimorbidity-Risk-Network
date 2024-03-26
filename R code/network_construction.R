library(readxl)
library(circlize)
library(tidyverse)
library(purrr)
library(ggplot2)
library(igraph)
# import dict
diseases_cat_dict <- read_xlsx("G:/final_chronic_diseases_dict.xlsx")
diseases_cat_dict <- diseases_cat_dict[!duplicated(diseases_cat_dict$`ICD-10 Code`),]
# import multimorbidity matrix
path <- "G:/共病/数据/"
file_name <- "multimorbidity_matrix.csv"
multimorbidity_matrix <- read.csv(paste0(path, file_name), row.names = 1, check.names = FALSE)
# complex multimorbidity network edge
long_data <- multimorbidity_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "from") %>%
  gather(key = "to", value = "value", -from) %>%
  filter(from < to) %>%
  filter(value > 0)
multimorbidity_net_edges <- long_data %>%
  rename(source = from, target = to)
# import AP matrix
path <- "G:/共病/数据/"
file_name <- "ap_matrix.csv"
ap_matrix <- read.csv(paste0(path, file_name), row.names = 1, check.names = FALSE)
# complex multimorbidity network edge label
long_data_label <- ap_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "from") %>%
  gather(key = "to", value = "value", -from) %>%
  filter(from < to) %>%
  filter(value > 0)
multimorbidity_net_edge_labels <- long_data_label %>%
  rename(source = from, target = to)
merged_data <- merge(multimorbidity_net_edges, multimorbidity_net_edge_labels, by = c("source", "target"), all.x = TRUE)
colnames(merged_data) <- c('source', 'target', 'value', 'label')
# Write to CSV
write.table(merged_data, "G:/共病/数据/multimorbidity_net_edges.csv", row.names = FALSE, col.names = TRUE, sep = ",")
# Find_category
find_category <- function(code, dict) {
  category <- dict$Category[dict$`ICD-10 Code` == code]
  if(length(category) > 0) {
    return(category)
  } else {
    return(NA)
  }
}
# complex multimorbidity network node
multimorbidity_net_nodes <- data.frame(
  id = unique(c(long_data$from, long_data$to))
)
# According to the diseases_cat_dict assigned category to each node
multimorbidity_net_nodes$label <- sapply(multimorbidity_net_nodes$id, find_category, dict = diseases_cat_dict)
# Write to csv
write.table(multimorbidity_net_nodes, "G:/共病/数据/multimorbidity_net_nodes.csv", row.names = FALSE, col.names = TRUE, sep = ",")
g <- graph_from_data_frame(multimorbidity_net_edges, directed = FALSE)
degree_values <- degree(g)
degree_freq <- table(degree_values)
degree_prob <- degree_freq / vcount(g)
write.csv(data.frame(degree = as.numeric(names(degree_prob)), probability = as.vector(degree_prob)), "G:/共病/数据/degree_probability_distribution.csv", row.names = FALSE)
# The fundamental characteristics of the CMN
number_of_nodes <- vcount(g)
number_of_edges <- ecount(g)
average_degree <- mean(degree(g))
network_density <- edge_density(g)
weighted_clustering_coefficient <- transitivity(g, type = "localaverage", weights = E(g)$value)
g_components <- components(g)
g_largest <- induced_subgraph(g, which(g_components$membership == which.max(g_components$csize)))
average_path_length <- mean_distance(g_largest, weights = E(g_largest)$value)
