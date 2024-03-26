library(igraph)
library(dplyr)
library(openxlsx)
nodes <- read.csv("G:/共病/数据/multimorbidity_net_nodes_with_community_labels.csv")
edges <- read.csv("G:/共病/数据/multimorbidity_net_edges.csv")
edges$weight <- edges$value
edges <-  select(edges, -value)
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

community1_nodes <- filter(nodes, community == 1)
community2_nodes <- filter(nodes, community == 2)
community3_nodes <- filter(nodes, community == 3)
community1_nodes <- select(community1_nodes, -community)
community2_nodes <- select(community2_nodes, -community)
community3_nodes <- select(community3_nodes, -community)
community1_edges <- data.frame()
community2_edges <- data.frame()
community3_edges <- data.frame()
for (i in 1:nrow(edges)) {
  if (edges$source[i] %in% community1_nodes$id & edges$target[i] %in% community1_nodes$id) {
    community1_edges <- rbind(community1_edges, edges[i, ])
  }
}
for (i in 1:nrow(edges)) {
  if (edges$source[i] %in% community2_nodes$id & edges$target[i] %in% community2_nodes$id) {
    community2_edges <- rbind(community2_edges, edges[i, ])
  }
}
for (i in 1:nrow(edges)) {
  if (edges$source[i] %in% community3_nodes$id & edges$target[i] %in% community3_nodes$id) {
    community3_edges <- rbind(community3_edges, edges[i, ])
  }
}
community1_label_counts <- community1_nodes %>%
  count(label) %>%
  mutate(Percentage = n / sum(n) * 100)
community2_label_counts <- community2_nodes %>%
  count(label) %>%
  mutate(Percentage = n / sum(n) * 100)
community3_label_counts <- community3_nodes %>%
  count(label) %>%
  mutate(Percentage = n / sum(n) * 100)
community1_label_counts_sorted <- community1_label_counts %>% arrange(desc(Percentage))
community2_label_counts_sorted <- community2_label_counts %>% arrange(desc(Percentage))
community3_label_counts_sorted <- community3_label_counts %>% arrange(desc(Percentage))
# wb <- createWorkbook()
# addWorksheet(wb, "Community1")
# writeData(wb, sheet = "Community1", community1_label_counts_sorted)
# addWorksheet(wb, "Community2")
# writeData(wb, sheet = "Community2", community2_label_counts_sorted)
# addWorksheet(wb, "Community3")
# writeData(wb, sheet = "Community3", community3_label_counts_sorted)
## Save the workbook to a file
# saveWorkbook(wb, file = "G:/共病/数据/community_label_counts.xlsx", overwrite = TRUE)


g1 <- graph_from_data_frame(d = community1_edges, vertices = community1_nodes, directed = FALSE)
community1_nodes$DC <- degree(g1, mode = "all")
community1_nodes$EC <- eigen_centrality(g1)$vector
E(g1)$inverse_weight <- 1 / E(g1)$weight
community1_nodes$bc <- betweenness(g1, v = V(g1), weight = E(g1)$inverse_weight)
community1_nodes$cc <- closeness(g1, v = V(g1), weights = E(g1)$inverse_weight)
output_path <- "G:/共病/数据/community1_nodes_with_centrality.csv"
write.csv(community1_nodes, output_path, row.names = FALSE)
output_path <- "G:/共病/数据/community1_edges.csv"
write.csv(community1_edges, output_path, row.names = FALSE)

g2 <- graph_from_data_frame(d = community2_edges, vertices = community2_nodes, directed = FALSE)
community2_nodes$DC <- degree(g2, mode = "all")
community2_nodes$EC <- eigen_centrality(g2)$vector
E(g2)$inverse_weight <- 1 / E(g2)$weight
community2_nodes$bc <- betweenness(g2, v = V(g2), weight = E(g2)$inverse_weight)
community2_nodes$cc <- closeness(g2, v = V(g2), weights = E(g2)$inverse_weight)
output_path_community2 <- "G:/共病/数据/community2_nodes_with_centrality.csv"
write.csv(community2_nodes, output_path_community2, row.names = FALSE)
output_path <- "G:/共病/数据/community2_edges.csv"
write.csv(community2_edges, output_path, row.names = FALSE)

g3 <- graph_from_data_frame(d = community3_edges, vertices = community3_nodes, directed = FALSE)
community3_nodes$DC <- degree(g3, mode = "all")
community3_nodes$EC <- eigen_centrality(g3)$vector
E(g3)$inverse_weight <- 1 / E(g3)$weight
community3_nodes$bc <- betweenness(g3, v = V(g3), weight = E(g3)$inverse_weight)
community3_nodes$cc <- closeness(g3, v = V(g3), weights = E(g3)$inverse_weight)
output_path_community3 <- "G:/共病/数据/community3_nodes_with_centrality.csv"
write.csv(community3_nodes, output_path_community3, row.names = FALSE)
output_path <- "G:/共病/数据/community3_edges.csv"
write.csv(community3_edges, output_path, row.names = FALSE)