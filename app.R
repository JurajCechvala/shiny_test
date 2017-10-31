library(shiny)
library(ggplot2)

# DATA PREPARATION --------------------------------------------------------------------------------
load("yield_curve_2015-05-31.RData")

yield.curve$fun <- approxfun(x = yield.curve$curve.data$tenor_Y,
                             y = yield.curve$curve.data$yield_bps,
                             rule = 2)

# Stress scenarios as a list of functions to be superimposed on real YC. The list is
# composed of stress scenarios required by Basel guidelines.
ir.constant <- c("parallel" = 200,
                 "short"    = 250,
                 "long"     = 100)

stress.fun <- list()
stress.fun$no.stress <- function(x) {
  rep(0, length(x))
}
stress.fun$parallel.up <- function(x) {
  rep(ir.constant["parallel"], length(x))
}
stress.fun$parallel.down <- function(x) {
  rep(-ir.constant["parallel"], length(x))
}
stress.fun$short.up <- function(x) {
  ir.constant["short"]*exp(-x/4)
}
stress.fun$short.down <- function(x) {
  -ir.constant["short"]*exp(-x/4)
}
stress.fun$long.up <- function(x) {
  ir.constant["long"]*(1 - exp(-x/4))
}
stress.fun$long.down <- function(x) {
  -ir.constant["long"]*(1 - exp(-x/4))
}
stress.fun$steepener <- function(x) {
  -0.65*ir.constant["short"]*exp(-x/4) + 0.9*ir.constant["long"]*(1 - exp(-x/4))
}
stress.fun$flattener <- function(x) {
  0.8*ir.constant["short"]*exp(-x/4) - 0.6*ir.constant["long"]*(1 - exp(-x/4))
}

names(stress.fun) <- c("no shift",
                       "upward parallel shift",
                       "downward parallel shift",
                       "short end upward shift",
                       "short end downward shift",
                       "long end upward shift",
                       "long end downward shift",
                       "steepening shift",
                       "flattening shift")

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
                  "Minimum and maximum tenor [years]",
                  min = 0,
                  max = 2*max(yield.curve$curve.data$tenor_Y, na.rm = TRUE),
                  value = c(min(yield.curve$curve.data$tenor_Y, na.rm = TRUE),
                            max(yield.curve$curve.data$tenor_Y, na.rm = TRUE)),
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
        labs(x = "tenor [years]", y = "yield [bps]")
    })
}

shinyApp(ui, server)
