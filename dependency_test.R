library(shiny)
library(igraph)

# Sample methods and their dependencies (just for demonstration)
methods <- data.frame(
  MethodID = 1:6,
  MethodName = c("Sample Creation", "Sample Delivery", "Analytical Method 1", "Analytical Method 2", "Analytical Method 3", "Sample Disposal"),
  MethodType = c("Creating", "Follow-Up", "Follow-Up", "Follow-Up", "Follow-Up", "Terminating"),
  ParentMethodID = c(NA, 1, 2, 3, 3, 5)
)

# Define the Shiny app
ui <- fluidPage(
  selectInput("selectedMethod", "Select Method", choices = methods$MethodName),
  plotOutput("dependencyGraph")
)

server <- function(input, output, session) {
  output$dependencyGraph <- renderPlot({
    selectedMethod <- input$selectedMethod
    selectedMethodID <- methods$MethodID[methods$MethodName == selectedMethod]
    
    # Create an igraph graph based on the sample data
    graph <- graph.data.frame(methods, directed = TRUE)
    
    # Color nodes based on method type
    colors <- ifelse(methods$MethodType == "Creating", "green",
                     ifelse(methods$MethodType == "Terminating", "red", "blue"))
    
    # Plot the graph
    plot(graph,
         layout = layout.circle,
         vertex.color = colors,
         vertex.size = 30,
         vertex.label.cex = 1.2,
         vertex.label.color = "black",
         vertex.label.dist = 0.5,
         edge.arrow.size = 0.5,
         main = selectedMethod
    )
  })
}

shinyApp(ui, server)