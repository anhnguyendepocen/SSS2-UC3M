#
# Shiny web application for illustrating the choice of distances to minimize in
# linear regression: vertical, horizontal or perpendicular
#

library(shiny)
library(rgl)

# Data
set.seed(34567)
x <- rnorm(50)
xCol <- x + rnorm(50, sd = 0.05)
y <- xCol#rnorm(50)
eps <- rnorm(50)
zLin <- -0.5 + 1.5 * x + 2 * y + eps
zQua <- -0.5 + 1.5 * x^2 + 2 * y^3 + eps
zExp <- -0.5 + 1.5 * x + xCol + eps

# UI for application
ui <- fluidPage(align = "center",

  # Vertical layout with:
  # - the select inputs for the data pattern and type of distance
  # - the slider inputs for intercept and slope
  verticalLayout(

    inputPanel(

      selectInput(inputId = "dataType", label = "Data pattern:",
                  choices = c("linear", "quadratic", "exponential"))

    ),

    rglwidgetOutput("regressionPlot")

  )

)

# Server logic
server <- function(input, output) {

  output$regressionPlot <- renderRglwidget({

    # Response
    z <- switch(input$dataType,
                linear = zLin,
                quadratic = zQua,
                exponential = zExp)

    # Vertical projection
    mod <- lm(z ~ x + y)
    beta0 <- mod$coefficients[1]
    beta1 <- mod$coefficients[2]
    beta2 <- mod$coefficients[3]
    proj <- cbind(x, y, beta0 + beta1 * x + beta2 * y)
    
    # Plot
    plot3d(x, y, z, xlim = c(-5, 5), ylim = c(-5, 5), zlim = c(-5, 5), add = FALSE)
    planes3d(a = beta1, b = beta2, c = -1, d = beta0, alpha = 0.5, col = "lightblue")
    segments3d(x = rep(x, each = 2), y = rep(y, each = 2), z = c(rbind(z, proj[, 3])), col = 3)
    points3d(proj, col = 2)
    rglwidget()

  })

}

# Run the application
shinyApp(ui = ui, server = server)

