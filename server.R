library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)

#server code
shinyServer(function(input, output) {
  
  readAndProcessCharges()
  
  output$bar <- renderPlot({
    
    if (input$drilldown == "Borough")
    {
      drilldown.data = charge_by_boro
      xlabel = 'Borough'
      xvector = charge_by_boro$borough
      count = charge_by_boro$count
    }
    if (input$drilldown == "Industry")
    {
      drilldown.data = charge_by_industry
      xlabel = 'Industry'
      xvector = charge_by_industry$industry.name
      count = charge_by_industry$count
    }
    if (input$drilldown == "Charge type")
    {
      drilldown.data = charge_by_chargeid
      xlabel = 'Charge ID'
      xvector = charge_by_chargeid$charge.id
      count = charge_by_chargeid$count    
    }
    
    ggplot(data = drilldown.data, aes(x = xvector, y = count, fill = count)) +
      geom_bar(stat = "identity") +
      labs(x = xlabel,
           y = 'Number of charges')  + 
      theme_classic() +
      coord_flip() 
  })
  output$test <- renderTable({
      charge_by_chargeid
    })
    
})
      #ifelse(input$borough == "Borough",  
    #         filtered.charges <- charges %>% filter(Year == input$year, Borough == input$borough),
    #         filtered.charges <- charges %>% filter(Year == input$year, Industry == input$industry))
      
      #input_borough = "Manhattan"
      #input_year = 2016
      #input_industry = "Laundry"
      
      #filtered.charges.borough <- charges.df %>% filter(year == input$year, input$borough)
      
      #filtered.charges.industry <- charges.df %>% filter(year == input$year, industry.name == input$industry)
      
      #g <- ggplot(data= filtered.charges,aes(x=charges.df$charge_id, charges)) 
      
      
      

  

# function ----------------------------------------------------------------

readAndProcessCharges <- function() {

  charges <- read.csv("charges.csv",stringsAsFactors = FALSE)
  charges.df <- as.data.frame(charges)
  names(charges.df)[1:23] <- tolower(names(charges.df)[1:23])
  
  #extract month and year
  charges.df$violation.date = as.Date(charges.df$violation.date, "%m/%d/%Y")
  charges.df$month = as.numeric(month(charges.df$violation.date))
  charges.df$year = as.numeric(year(charges.df$violation.date))
  
  #extract industry no.
  charges.df <- separate(data = charges.df, col = industry, into = c("industry.name", "industry.id"), sep = " \\- ")
  #TODO remove blank industry id at 70 locations for Other (industry_name)
  
  #extract charge type
  charges.df <- separate(data = charges.df, col = charge, into = c("charge.id", "charge.desc"), sep = " \\-") 
  charges.df$charge.cat = gsub("..+§ ","",charges.df$charge.id) 
  charges.df$charge.cat =  gsub(" *\\(.*?\\) *","",charges.df$charge.cat) 
  
  #fix boro casing
  capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
  }
  
  charges.df$borough <- tolower(charges.df$borough)
  charges.df$borough <- capFirst(charges.df$borough)
  
  top_industries <- 
    group_by(charges.df, industry.name) %>% 
    summarize( count = n()) %>%
    arrange( desc(count)) %>%
    top_n(25)
  
  charges.df.filtered = semi_join(charges.df, top_industries, by = 'industry.name')
  
  
  top_chargeids <- charges.df %>% 
    group_by(., charge.id) %>% 
    summarize( count = n()) %>%
    arrange( desc(count)) %>%
    top_n(25) 
  
  charges.df.filtered = semi_join(charges.df.filtered, top_chargeids, by = 'charge.id')
  
  charges.df.filtered = filter( charges.df.filtered, !grepl('Outside', borough) )
  charges.df.filtered = filter( charges.df.filtered, borough != "")
  
  charge_by_boro = charges.df.filtered %>%
    group_by(., borough) %>%
    summarize( count = sum(charge.count)) %>%
    arrange( desc(count))
  
  charge_by_industry = charges.df.filtered %>%
    group_by(., industry.name) %>%
    summarize( count = sum(charge.count))%>%
    arrange( desc(count)) %>%
    top_n(5) 
  
  charge_by_chargeid = charges.df.filtered %>%
    group_by(., charge.id) %>%
    mutate(., cat.descsample = first(charge.desc)) %>%
    summarize( count = sum(charge.count), cat.descsample = first(cat.descsample))%>%
    arrange( desc(count)) %>%
    top_n(10) 
  
}
