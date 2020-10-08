##myShinyApp: Generating the random two correlative variables

library(shiny)

ui <- fluidPage(
    
    titlePanel(strong("Generating two correlated variables")),
    hr(),

    sidebarPanel(
      h4(strong("Enter the parameters:")),
      numericInput("n", "Sample Size:", value = 30),
      numericInput("r", "Aproximate correlation:", value = .5),
      numericInput("mean1", "mean for first variable", value = 0),
      numericInput("sd1", "standard deviation for first variable", value = 1),
      numericInput("mean2", "mean for second variable", value = 0),
      numericInput("sd2", "standard deviation for second variable", value = 1)
    ),

    mainPanel(
      plotOutput("plot", width = 600, height = 600)
    )
  )

server <- function(input, output){
    
  output$plot <- renderPlot({ 
    u <- rnorm(input$n)
    v <- rnorm(input$n)
    a <- sqrt((1-input$r) / (1+input$r))
  
    x1 <- u + a*v
    x2 <- u - a*v

    Var1 <- scale(x1)*input$sd1 + input$mean1
    Var2 <- scale(x2)*input$sd2 + input$mean2   
    
    plot(Var1, Var2, pch = 16, col = "blue")
    abline(lm(Var2 ~ Var1), lwd = 2)
  })
}

shinyApp(ui, server)
