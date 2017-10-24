library(shiny)
library(tidyverse)
library(ggplot2)

# DATA PREPARATION --------------------------------------------------------------------------------
load("yield_curve_2015-05-31.RData")

yield.curve$fun <- approxfun(x = yield.curve$curve.data$term_Y,
                             y = yield.curve$curve.data$yield_bps,
                             rule = 2)

# stress scenarios as a list of functions to be superimposed on real YC
stress.fun <- list()
stress.fun$no.stress  <- function(x) {rep(   0, length(x))}
stress.fun$p200bps    <- function(x) {rep( 200, length(x))}
stress.fun$m200bps    <- function(x) {rep(-200, length(x))}
stress.fun$asc200bps  <- approxfun(x = c(0.5, 50),
                                   y = c(0, 200),
                                   rule = 2)
stress.fun$desc200bps <- approxfun(x = c(0.5, 50),
                                   y = c(200, 0),
                                   rule = 2)
names(stress.fun) <- c("No stress", "+200 bps uniform shift", "-200 bps uniform shift",
                       "linear increase to +200 bps", "linear decrease from +200 bps")

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
                  step = 1),
      radioButtons("stress",
                   "Select stress scenario",
                   choices = names(stress.fun))),
    # Show a plot of the data
    mainPanel(
      plotOutput("ycPlot"))))

# Define server
server <- function(input, output) {

  # superposition of real YC and stress scenario
  function.to.plot <-
    reactive({
      function(x){
        yield.curve$fun(x) + stress.fun[[input$stress]](x)
      }
    })

  output$ycPlot <-
    renderPlot({
      # draw the YC function using ggplot
      ggplot(data.frame(x = c(input$window[1], input$window[2])), aes(x)) +
        stat_function(fun = function.to.plot()) +
        labs(x = "term [years]", y = "yield [bps]")
    })
}

shinyApp(ui, server)
