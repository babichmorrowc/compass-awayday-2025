library(readr)
library(tidyverse)
library(data.table)
library(janitor)
library(word2vec)

# Import the survey data
clustering_responses <- read_csv("clustering-activity/clustering_responses.csv") %>% 
  clean_names() %>% 
  select(what_is_your_name,
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
rownames(person_vectors) <- responses_clean$what_is_your_name

# Dimensionality reduction
pca <- prcomp(person_vectors, center = TRUE, scale. = TRUE)
pca_vectors <- pca$x[,1:min(ncol(pca[["x"]]), 10)] # first 10 principal components at a maximum

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(pca_vectors, centers = 3)
# Add cluster assignments to the original data
responses_clean$kmeans_cluster <- as.factor(kmeans_result$cluster)

# Hierarchical clustering
hc <- hclust(dist(pca_vectors))
responses_clean$hcluster <- cutree(hc, k = 3)
plot(hc, labels = responses_clean$what_is_your_name)

# Get the cosine similarity matrix
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}
n <- nrow(person_vectors)
similarity_matrix <- matrix(0, nrow = n, ncol = n)
rownames(similarity_matrix) <- responses_clean$what_is_your_name
colnames(similarity_matrix) <- responses_clean$what_is_your_name
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

