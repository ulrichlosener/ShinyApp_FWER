library(bslib)
library(shiny)


ui <- page_sidebar(title="Illustration of Familywise Type I error rates", bg = "#ffd800",
                   sidebar=sidebar(selectInput(inputId="alpha", label="alpha level", 
                                               choices=c(0.001, 0.01, 0.05, 0.1),
                                               selected = 0.05),
                                   sliderInput(inputId="n.tests", label="number of conducted significance tests",
                                               min=1, max=100, value=3),
                                   textOutput(outputId="actual_alpha")
                                   ),
                   plotOutput(outputId="fwerplot")
                   )


server <- function(input, output){
  
  output$actual_alpha <- renderText({
    paste("The actual type I error rate is equal to", 1-(1-as.numeric(input$alpha))^as.numeric(input$n.tests))
  })
  
  output$fwerplot <- renderPlot({
    alpha <- as.numeric(input$alpha)
    x <- seq(1,100)
    fwer <- 1-(1-alpha)^x
    plot(fwer, type="b", xlab="Number of significance tests", 
         ylab="Family wise type I error probability", main="Familywise type I error as a function of the number of tests conducted")
  })
}

shinyApp(ui, server)

