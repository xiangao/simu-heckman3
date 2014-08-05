library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Heckman model compared with OLS"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    sliderInput("cr1w", "Correlation between x1 and w:",
                min=0, max=.9, value=0, step=.3),
    sliderInput("cr1u", "Correlation between x1 and u:",
                min=0, max=.9, value=0, step=.3),
    sliderInput("sel.cri", "selection proportion (low,  high):",
                min=0, max=1, value=0, step=1)
      ),
      mainPanel(plotOutput("plot1")
                )
      ))
