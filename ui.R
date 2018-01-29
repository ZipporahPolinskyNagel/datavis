#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

# Define UI for application that draws bar graph
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Department of Consumer Affairs charges for 2016-2017"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("drilldown", # choose the drilldown
                  label = "Drill down by ",
                  choices = c("Borough", "Industry", "Charge type"),
                  selected = "Borough"),

      checkboxGroupInput("year", label = "Year",
                         choices = c("2016","2017"),
                         selected = "2016")
    ),
    
    # Show a bar graph
   mainPanel(
    textOutput("header"),
    tableOutput("test"),
   plotOutput("bar", height = "300px")
   )
    
  )
))
  
  
  
  
  
 # sidebarLayout(
  #  sidebarPanel(
      
  #    selectInput("eth", # choose the race
  #                label = "Choose a ethinicity to display",
  #                choices = c("White", "Black", "Hispanic", "Asian", "All 4 Races"),
  #                selected = "Black"),
  #    selectInput("s", # choose the sex
  #                label = "Choose a sex to display",
   #               choices = c("Male","Female"),
   #               selected = "Male"),
  #    sliderInput("r", label = "Year",
   #               min = 2007, max = 2014,
   #               value = 2007, step = 1,
   #               pre = "", sep = "",
   #               animate = TRUE)
   # ),
    
  #Show a bar graph
   # mainPanel(
   #   textOutput("head"),
   #   tableOutput("t"),
   #   plotOutput("b", height = "300px")
   # )
    
  #)

