#
# Shiny web application for illustrating the influence of the regression line in
# the sample size, error variance and predictor variance
#

library(shiny)

# Load predictor's data
load("xData.RData")

# UI for application
ui <- fluidPage(

  # Vertical bar with a select input for the data pattern and type of distance, and slider input for intercept and slope
  verticalLayout(

    inputPanel(

      actionButton("newSample", "Generate sample"),
      selectInput(inputId = "n", label = "Sample size:",
                  choices = c(10, 50, 100, 200, 500), selected = 100),
      sliderInput(inputId = "beta0", label = "Intercept:",
                  min = -3, max = 3, value = 0.5, step = 0.5),
      sliderInput(inputId = "beta1", label = "Slope:",
                  min = -3, max = 3, value = 0.5, step = 0.5),
      sliderInput(inputId = "sigma2", label = "Error variance:",
                  min = 0.1, max = 4, value = 1, step = 0.5),
      sliderInput(inputId = "sigma2x", label = "Predictor variance:",
                  min = 0.1, max = 4, value = 1, step = 0.5)

    ),

    # Show the regression plot
    mainPanel(

      plotOutput("regressionPlot")

    )

  )

)

# Server logic
server <- function(input, output) {

  eps <- eventReactive(input$newSample, {

    rnorm(input$n)

  })

  output$regressionPlot <- renderPlot({

    # Plot
    x <- sqrt(input$sigma2x) * xData[1:input$n]
    regX <- input$beta0 + input$beta1 * x
    y <- regX + sqrt(input$sigma2) * eps()
    plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 16, xlab = "x", ylab = "y")
    abline(lm(y ~ x)$coefficients, col = 2, lwd = 3)
    abline(a = input$beta0, b = input$beta1, col = 1, lwd = 3)
    legend("bottomright", legend = c("True regression", "Estimated regression"),
           lwd = 2, col = 1:2, cex = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

