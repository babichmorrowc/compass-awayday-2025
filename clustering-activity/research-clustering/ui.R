library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Research clustering!"),
  
  # Sidebar with a slider input for number of clusters
  sidebarLayout(
    sidebarPanel(
      sliderInput("nclust",
                  "Number of clusters:",
                  min = 2,
                  max = 10,
                  value = 4),
      radioButtons("clust_method",
                   "Clustering method:",
                   choices = c("K-means" = "kmeans",
                               "Hierarchical" = "hclust"),
                   selected = "hclust")
    ),
    
    mainPanel(
      # Tab for the clustering
      tabsetPanel(
        tabPanel("Clustering",
                 plotOutput("clustPlot")
        ),
        # Tab for a table
        tabPanel("Closest / Furthest research",
                 DTOutput("table")
        )
      )
    )
  )
)
