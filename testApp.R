library(shiny)
library(tidyverse)

# DATA PREPARATION --------------------------------------------------------------------------------
load("yield_curve_2015-05-31.RData")

yield.curve$fun <- approxfun(x = yield.curve$curve.data$term_Y,
                             y = yield.curve$curve.data$yield_bsp,
                             rule = 2)

# SHINY APP ---------------------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <-
  fluidPage(

  # Application title
  titlePanel("Yield Curve"),

  # Sidebar with a slider input for min and max shown values
  sidebarLayout(
    sidebarPanel(
      sliderInput("window",
                  "Minimum and maximum term [years]",
                  min = 0,
                  max = 2*max(yield.curve$curve.data$term_Y, na.rm = TRUE),
                  value = c(min(yield.curve$curve.data$term_Y, na.rm = TRUE),
                            max(yield.curve$curve.data$term_Y, na.rm = TRUE)),
                  step = 1)),
    # Show a plot of the data
    mainPanel(
      plotOutput("ycPlot"))))

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$ycPlot <-
    renderPlot({

      # draw the YC approximation function
      plot.function(yield.curve$fun, from = input$window[1], to = input$window[2])
    })
}

shinyApp(ui, server)
