library(shiny)
library(popbio)

ui <- fluidPage(
  titlePanel("Matrix Population Analysis"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n1", "Value 1:", value = 0),
      numericInput("n2", "Value 2:", value = 0),
      numericInput("n3", "Value 3:", value = 0),
      numericInput("n4", "Value 4:", value = 0),
      numericInput("n5", "Value 5:", value = 0),
      numericInput("n6", "Value 6:", value = 0),
      numericInput("n7", "Value 7:", value = 0),
      numericInput("n8", "Value 8:", value = 0),
      numericInput("n9", "Value 9:", value = 0),
      actionButton("analyse", "Analyse")
    ),
    mainPanel(
      verbatimTextOutput("lambda_output"),
      plotOutput("projection_plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$analyse, {
    mat <- matrix(c(input$n1, input$n2, input$n3,
                    input$n4, input$n5, input$n6,
                    input$n7, input$n8, input$n9),
                  nrow = 3, byrow = TRUE)
    
    lambda_val <- lambda(mat)
    pop_proj <- pop.projection(mat, c(2,2,2), 20)
    
    output$lambda_output <- renderText({
      paste("Lambda (Growth Rate):", round(lambda_val, 4))
    })
    
    output$projection_plot <- renderPlot({
      projection <- pop.projection(mat, c(2,2,2), 20)
      matplot(t(projection$stage.vectors), type = "l", lty = 1, col = 1:3,
              xlab = "Time Steps", ylab = "Population Size", main = "Population Projection")
      legend("topright", legend = paste("Stage", 1:3), col = 1:3, lty = 1)
    })
  })
}

shinyApp(ui, server)