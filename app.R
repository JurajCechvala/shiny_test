library(shiny)
library(tidyverse)
library(ggplot2)

# DATA PREPARATION --------------------------------------------------------------------------------
load("yield_curve_2015-05-31.RData")

yield.curve$fun <- approxfun(x = yield.curve$curve.data$term_Y,
                             y = yield.curve$curve.data$yield_bsp,
                             rule = 2)

# SHINY APP ---------------------------------------------------------------------------------------
# Define UI
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

# Define server
server <- function(input, output) {

  output$ycPlot <-
    renderPlot({
      # draw the YC approximation function using ggplot
      ggplot(data.frame(x = c(input$window[1], input$window[2])), aes(x)) +
        stat_function(fun = yield.curve$fun) +
        labs(x = "term [years]", y = "yield [bsp]")
    })
}

shinyApp(ui, server)
