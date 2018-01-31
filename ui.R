#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)

# Define UI for application that draws bar graph
shinyUI(fluidPage(
  theme = "bootstrap.css",
  
  # Application title
  headerPanel("Department of Consumer Affairs charges for 2016-2017"),
  
  sidebarPanel(
      
      radioButtons("drilldown", # choose the drilldown
                  label = "Drill down by ",
                  choices = c("Borough", "Industry", "Charge type"),
                  selected = "Borough"),

      #checkboxGroupInput("year", label = "Year",
      #                   choices = c("2016","2017"),
      #                   selected = "2016"),
      
      sliderInput("industry_number", label = "No. Industries",
                  min = 1, max = 50,
                  value = 20, step = 1,
                  pre = "", sep = "",
                  animate = TRUE),
      
      sliderInput("charge_number", label = "No. Charge types",
                  min = 1, max = 50,
                  value = 20, step = 1,
                  pre = "", sep = "",
                  animate = TRUE)
    ),
    
    # Show a bar graph
   mainPanel(
     tabsetPanel(
       tabPanel("Plots", plotOutput("bar",  width = "100%")),
       tabPanel("Word cloud", plotOutput("words", width = "100%")),
       #tabPanel("Over time", tableOutput("table")),
       tabPanel("Charge types", tableOutput("charges"))
     )
   )
    
  )
)
  

