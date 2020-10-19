##myShinyApp: Generating the random two correlative variables

library(shiny)

ui <- fluidPage(
    
    titlePanel(strong("Generating two correlated variables")),
    hr(),

    sidebarPanel(width = 3,
      h4(strong("Enter the parameters:")),
      br(),
      numericInput("n", "Sample Size:", value = 30),
      numericInput("r", "Aproximate correlation:", value = .5),
      numericInput("mean1", "Mean for first variable", value = 0),
      numericInput("sd1", "Standard deviation for first variable", value = 1),
      numericInput("mean2", "Mean for second variable", value = 0),
      numericInput("sd2", "Standard deviation for second variable", value = 1),
      actionButton("go", "Draw Graphs", class = "btn-primary"),
      br(),
      br(),
      strong("You can download the generated data file:"),
      downloadButton("downloadData", "Download")
    ),

    mainPanel(

      fluidRow(

        column(8,
          h4(strong("Scatter plot of the variables")),
          plotOutput("plot", width = 600, height = 600),
          textOutput("text1"),
          textOutput("text2")
        ),

        column(4,
          h4(strong("Dataset for first 20 pairs")),
          tableOutput ("table")
        )
      )
    )
  )

server <- function(input, output){
 
  output$plot <- renderPlot({
    
    input$go
    
    isolate({
      u <- rnorm(input$n)
      v <- rnorm(input$n) 
      a <- sqrt((1-input$r) / (1+input$r))
  
      x1 <- u + a*v 
      x2 <- u - a*v 

      Var1 <<- scale(x1)*input$sd1 + input$mean1 
      Var2 <<- scale(x2)*input$sd2 + input$mean2
  
      data1 <<- data.frame(X = round(Var1, 2), Y = round(Var2, 2))
    })     

    plot(Var1, Var2, pch = 16, col = "blue")
    abline(lm(Var2 ~ Var1), lwd = 2)    
  })

  output$text1 <- renderText({
    input$go
    paste("Correlations between variables; r =", round(cor(Var1, Var2), 2))
  })

  output$table <- renderTable({
    input$go
    head(data1, 20)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data1, file)
    }
  )
}

shinyApp(ui, server)
