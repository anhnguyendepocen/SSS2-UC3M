#
# Shiny web application for illustrating the influence of the regression line in
# the sample size, error variance and predictor variance
#

library(shiny)

# Load predictor's data
load("xData.RData")

# UI for application
ui <- fluidPage(

  # Arrange in 3 columns:
  # - the slider inputs for intercept and slope
  # - the slider inputs variance of error and predictor
  # - the sample options
  fluidRow(

    br(),
    column(width = 3, offset = 0,

      sliderInput(inputId = "beta0", label = "Intercept:",
                  min = -3, max = 3, value = 0.5, step = 0.5),
      sliderInput(inputId = "beta1", label = "Slope:",
                  min = -3, max = 3, value = 0.5, step = 0.5)
    ),
    column(width = 3,

      sliderInput(inputId = "sigma2", label = "Error variance:",
                  min = 0.1, max = 4, value = 1, step = 0.5),
      sliderInput(inputId = "sigma2x", label = "Predictor variance:",
                  min = 0.1, max = 4, value = 1, step = 0.5)

    ),
    column(width = 1,

      h4("Samples:"),
      actionButton("newSample", "New!"),
      br(),
      br(),
      selectInput(inputId = "n", label = "Size:",
                  choices = c(10, 50, 100, 200, 500), selected = 100)

    ),

    # Show the regression plot
    mainPanel(

      plotOutput("regressionPlot")

    )

  )

)

# Server logic
server <- function(input, output) {

  # Add a default Manage the first call
  values <- reactiveValues(default = 0)
  observeEvent(input$newSample, {

    values$default <- input$newSample

  })

  # Error sampling
  eps <- eventReactive(input$newSample, {

    rnorm(500)

  })

  output$regressionPlot <- renderPlot({

    # Check if buttom was clicked
    if (values$default == 0){

      error <- rnorm(500)

    } else {

      error <- eps()

    }

    # Plot
    x <- sqrt(input$sigma2x) * xData[1:input$n]
    regX <- input$beta0 + input$beta1 * x
    y <- regX + sqrt(input$sigma2) * error[1:input$n]
    plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 16, xlab = "x", ylab = "y")
    abline(a = input$beta0, b = input$beta1, col = 1, lwd = 3)
    abline(lm(y ~ x)$coefficients, col = 2, lwd = 3)
    legend("bottomright", legend = c("True regression", "Estimated regression"),
           lwd = 2, col = 1:2, cex = 1.5)

  }, width = 650, height = 650)

}

# Run the application
shinyApp(ui = ui, server = server)

