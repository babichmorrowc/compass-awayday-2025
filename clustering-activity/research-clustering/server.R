library(shiny)
library(readr)
library(tidyverse)
library(data.table)
library(janitor)
library(word2vec)
library(dendextend)
library(DT)

# Import data
clustering_responses <- read_csv("../clustering_responses.csv") %>% 
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
glove <- fread("../glove/glove.6B.50d.txt", data.table = FALSE, header = FALSE, quote = "")
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

function(input, output, session) {
  # K-means clustering
  set.seed(123)
  kmeans_result <- reactive({
    kmeans(pca_vectors, centers = input$nclust, nstart = 20)
  })
  
  # Balanced K-means clustering
  
  
  # Hierarchical clustering
  hc <- hclust(dist(pca_vectors))
  dend <- as.dendrogram(hc)
  clusters_hclust <- reactive({cutree(hc, k = input$nclust)})
  
  # Add cluster assignment to data:
  clustered_data <- reactive({
    df <- responses_clean
    if (input$clust_method == "kmeans") {
      df$cluster <- as.factor(kmeans_result()$cluster)
    } else {
      df$cluster <- as.factor(cutree(hc, k = input$nclust))
    }
    df
  })
  
  # Plot of clustering
  output$clustPlot <- renderPlot({
    
    # Check which clustering method is selected
    if (input$clust_method == "kmeans") {
      
      # Plot K-means clustering
      ggplot(data = clustered_data(), aes(x = pca_vectors[,1],
                                         y = pca_vectors[,2],
                                         color = cluster)
      ) +
        geom_point(size = 5) +
        geom_text(aes(label = name), vjust = -1, size = 5) +
        scale_color_brewer(palette = "Set1") +
        labs(title = "K-means Clustering", x = "PC1", y = "PC2", color = "Cluster") +
        guides(label = "none") +
        theme_minimal()
      
    } else {
      # Plot hierarchical clustering
      dend_colored <- color_branches(dend, k = input$nclust, col = RColorBrewer::brewer.pal(input$nclust, "Set1"))
      labels(dend_colored) <- responses_clean$name
      plot(dend_colored, main = "Hierarchical Clustering", ylab = "Height")
    }
    
  })
  
  # Table of cluster membership
  output$cluster_table <- renderDT({
    df <- clustered_data()
    cluster_assignments <- df %>%
      select(name, cluster) %>% 
      arrange(cluster)
    
    all_clusters <- sort(unique(cluster_assignments$cluster))
    palette <- RColorBrewer::brewer.pal(input$nclust, "Set1")
    cluster_colors <- setNames(palette[seq_along(all_clusters)], all_clusters)
    
    datatable(
      cluster_assignments,
      escape = FALSE,
      options = list(pageLength = 25),
      rownames = FALSE,
      caption = "Cluster membership:"
    ) %>%
      formatStyle(
        columns = names(cluster_assignments),
        target = "row",
        backgroundColor = styleEqual(names(cluster_colors), cluster_colors)
      )
  })
  
  # Table of closest and furthest researchers
  output$close_far_table <- renderDT({
    df <- clustered_data()
    cluster_assignments <- df %>%
      select(name, cluster)
    
    closest_furthest <- data.frame(
      Name = rownames(similarity_matrix),
      Closest = apply(similarity_matrix, 1, function(x) {
        x[which.max(x)] <- -Inf
        rownames(similarity_matrix)[which.max(x)]
      }),
      Furthest = apply(similarity_matrix, 1, function(x) {
        x[which.min(x)] <- Inf
        rownames(similarity_matrix)[which.min(x)]
      })
    ) %>%
      left_join(cluster_assignments, by = c("Name" = "name")) %>%
      left_join(cluster_assignments, by = c("Closest" = "name"), suffix = c("", "_closest")) %>%
      left_join(cluster_assignments, by = c("Furthest" = "name"), suffix = c("", "_furthest"))
    
    all_clusters <- sort(unique(c(
      closest_furthest$cluster,
      closest_furthest$cluster_closest,
      closest_furthest$cluster_furthest
    )))
    palette <- RColorBrewer::brewer.pal(max(3, length(all_clusters)), "Set3")
    cluster_colors <- setNames(palette[seq_along(all_clusters)], all_clusters)
    
    datatable(
      closest_furthest %>% select(Name, Closest, Furthest),
      escape = FALSE,
      options = list(pageLength = 10),
      rownames = FALSE,
      caption = "Closest and furthest researchers based on cosine similarity."
    ) %>%
      formatStyle('Name',
                  color = styleEqual(names(cluster_colors), cluster_colors)
      ) %>%
      formatStyle('Closest',
                  color = styleEqual(names(cluster_colors), cluster_colors)
      ) %>%
      formatStyle('Furthest',
                  color = styleEqual(names(cluster_colors), cluster_colors)
      )
  })
  
  
}
