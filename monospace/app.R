library(shiny)

ui <- fluidPage(
  tags$style(HTML("
    .monospace-textarea {
      font-family: 'Courier New', monospace;
    }
  ")),
  textAreaInput("userInput", "Enter some text:", ""),
  verbatimTextOutput("outputText")
)

server <- function(input, output, session) {
  output$outputText <- renderPrint({
    user_text <- input$userInput
    cat("User input:", user_text)
  })
}

shinyApp(ui, server)
