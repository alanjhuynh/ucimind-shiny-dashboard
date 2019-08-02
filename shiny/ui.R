#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Clinical Summary Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
#      sliderInput(inputId = "report",
#                  label = "Report number:",
#                  min = 621,
#                  max = 623,
#                  value = 621
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Patient Cohort", "Control Cohort", "Home Cohort")),
      h4(" "),
      h4("Sex Data"),
      verbatimTextOutput("sexSummary"),
      h4("Age Data"),
      verbatimTextOutput("ageSummary"),
      h4("Education Data"),
      verbatimTextOutput("eduSummary")
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot", height = "2250px")
    )
  )
)
)

