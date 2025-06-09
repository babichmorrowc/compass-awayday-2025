library(readr)
library(tidyverse)
library(data.table)
library(janitor)
library(word2vec)

# Import the survey data
clustering_responses <- read_csv("clustering-activity/clustering_responses.csv") %>% 
  clean_names() %>% 
  select(name,
         starts_with("your"))

# Clean the data
responses_clean <- clustering_responses %>% 
  # lowercase all key words
  mutate(across(starts_with("your"), tolower)) %>%
  # remove punctuation
  mutate(across(starts_with("your"), ~ gsub("[[:punct:]]", " ", .))) %>% 
  unite(keywords, starts_with("your"), sep = " ", remove = FALSE, na.rm = TRUE) %>% 
  mutate(tokens = strsplit(keywords, "\\s+"))

#  Load GloVe vectors
# Downloaded from https://nlp.stanford.edu/projects/glove/
glove <- fread("./clustering-activity/glove/glove.6B.50d.txt", data.table = FALSE, header = FALSE, quote = "")
rownames(glove) <- glove$V1
glove <- glove[, -1]

# Helper function for getting the average vector
get_avg_vector <- function(words, embeddings) {
  valid_words <- intersect(words, rownames(embeddings))
  if (length(valid_words) == 0) {
    return(rep(0, ncol(embeddings)))  # fallback if no matches
  }
  colMeans(embeddings[valid_words, , drop = FALSE])
}

# Apply to each person
person_vectors <- t(sapply(responses_clean$tokens, get_avg_vector, embeddings = glove))
rownames(person_vectors) <- responses_clean$name

# Dimensionality reduction
pca <- prcomp(person_vectors, center = TRUE, scale. = TRUE)
pca_vectors <- pca$x[,1:min(ncol(pca[["x"]]), 10)] # first 10 principal components at a maximum

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(pca_vectors, centers = 3)
# Add cluster assignments to the original data
responses_clean$kmeans_cluster <- as.factor(kmeans_result$cluster)
# Plot K-means clustering
ggplot(data = responses_clean, aes(x = pca_vectors[,1], y = pca_vectors[,2], color = kmeans_cluster, fill = kmeans_cluster)) +
  geom_point() +
  geom_text(aes(label = name), vjust = -1, size = 3) +
  # Plot the centers
  # geom_point(data = as.data.frame(kmeans_result$centers), aes(x = PC1, y = PC2), color = "black", fill = "black", size = 3, shape = 8) +
  labs(title = "K-means Clustering", x = "PC1", y = "PC2") +
  stat_ellipse(level = 0.95, alpha = 0.2) +
  theme_minimal()

plot(pca_vectors, col = kmeans_result$cluster,
     main = "K-means Clustering",
     xlab = "PC1", ylab = "PC2")
points(kmeans_result$centers, col = 1:3, pch = 8, cex = 2)

# Hierarchical clustering
hc <- hclust(dist(pca_vectors))
responses_clean$hcluster <- cutree(hc, k = 4)
plot(hc, labels = responses_clean$name)
# Colored plot
dend <- as.dendrogram(hc)
dend_colored <- color_branches(dend, k = 4, col = RColorBrewer::brewer.pal(4, "Set1"))
plot(dend_colored, main = "Hierarchical Clustering", ylab = "Height")
plot(dend)

raw_clusters <- cutree(hc, k = 4)
cluster_sizes <- sort(table(raw_clusters), decreasing = TRUE)
relabel_map <- setNames(seq_along(cluster_sizes), names(cluster_sizes))
new_clusters <- as.integer(relabel_map[as.character(raw_clusters)])

# Assign new labels to dendrogram
labels_colors <- setNames(RColorBrewer::brewer.pal(4, "Set1")[new_clusters], names(new_clusters))
dend_colored <- dend %>%
  set("labels", responses_clean$name) %>%
  set("branches_k_color", k = 4, value = RColorBrewer::brewer.pal(4, "Set1")[unique(new_clusters)])

# dend_colored <- color_branches(dend, k = input$nclust, col = RColorBrewer::brewer.pal(input$nclust, "Set1"))
plot(dend_colored, main = "Hierarchical Clustering", ylab = "Height")

# Get the cosine similarity matrix
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}
n <- nrow(person_vectors)
similarity_matrix <- matrix(0, nrow = n, ncol = n)
rownames(similarity_matrix) <- responses_clean$name
colnames(similarity_matrix) <- responses_clean$name
for (i in 1:n) {
  for (j in 1:n) {
    similarity_matrix[i, j] <- cosine_similarity(person_vectors[i, ], person_vectors[j, ])
  }
}

# Get the closest and furthest person for each person
closest_persons <- apply(similarity_matrix, 1, function(x) {
  names(sort(x, decreasing = TRUE)[2])  # second highest is the closest
})
furthest_persons <- apply(similarity_matrix, 1, function(x) {
  names(sort(x, decreasing = FALSE)[1])  # second lowest is the furthest
})
# Add closest and furthest persons to the data
responses_clean$closest_person <- closest_persons
responses_clean$furthest_person <- furthest_persons

