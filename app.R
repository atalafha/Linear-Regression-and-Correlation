library(shiny)
library(MASS)  # for the mvrnorm function

ui <- fluidPage(
  titlePanel("Effect of Correlation on Linear Regression"),
  
  # Slider for correlation coefficient
  sliderInput(inputId = "cor_slider",
              label = "Correlation Coefficient:",
              min = -1,
              max = 1,
              value = 0.5,
              step = 0.01),
  
  # Scatter plot with regression line
  plotOutput("regPlot")
)


server <- function(input, output) {
  output$regPlot <- renderPlot({
    set.seed(123)  # for reproducibility
    
    # Generate synthetic data
    sigma <- matrix(c(1, input$cor_slider, input$cor_slider, 1), 2)
    data <- mvrnorm(n = 100, mu = c(0, 0), Sigma = sigma)
    x <- data[, 1]
    y <- data[, 2]
    
    # Create scatter plot
    plot(x, y, xlab = "X", ylab = "Y", pch = 19, col = 'blue')
    
    # Add regression line
    abline(lm(y ~ x), col = "red")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
