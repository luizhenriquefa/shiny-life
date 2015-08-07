
library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(
      width = 3
    ),
    column(
      width = 6,
      plotOutput('grid')
    ),
    column(
      width = 3
    )
  )
))