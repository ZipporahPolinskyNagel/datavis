library(shiny)


#server code
shinyServer(function(input, output) {

  
  output$bar <- renderPlot({
    resetChargeData(input$charge_number, input$industry_number)
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
      count = input$industry_number
    }
    if (input$drilldown == "Charge type")
    {
      drilldown.data = charge_by_chargeid
      xlabel = 'Charge ID'
      xvector = charge_by_chargeid$charge.id
      count = input$charge_number    
    }
    
    ggplot(data = drilldown.data, aes(x = reorder(xvector, count), y = count, fill = count)) +
      geom_bar(stat = "identity") +
      labs(x = xlabel,
           y = 'Number of charges')  + 
      theme_classic() +
      coord_flip() 
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$words <- renderPlot({
    
    resetChargeData(input$charge_number, input$industry_number)
    wordcloud_rep(words = d$word, freq = d$freq, min.freq = 2, scale=c(2.5,.25),
    max.words=500, random.order=FALSE, rot.per=0.15, 
    colors=brewer.pal(8, "Dark2"))
  })
  
  output$charges <- renderTable({
    resetChargeData(input$charge_number, input$industry_number)
    charge_by_chargeid
  })
    
})
    
resetChargeData <- function( charge_count, industry_count ) {
  
  top_industries <- 
    group_by(charges.df, industry.name) %>% 
    summarize( count = n()) %>%
    arrange( desc(count)) %>%
    top_n(industry_count)
  
  charges.df.filtered = semi_join(charges.df, top_industries, by = 'industry.name')
  
  top_chargeids <- charges.df %>% 
    group_by(., charge.id) %>% 
    summarize( count = n()) %>%
    arrange( desc(count)) %>%
    top_n(charge_count) 
  
  charges.df.filtered = semi_join(charges.df.filtered, top_chargeids, by = 'charge.id')
  
  charge_by_boro = charges.df.filtered %>%
    group_by(., borough) %>%
    summarize( count = sum(charge.count)) %>%
    arrange( desc(count))
  
  charge_by_industry = charges.df.filtered %>%
    group_by(., industry.name) %>%
    summarize( count = sum(charge.count))%>%
    arrange( desc(count)) %>%
    top_n(industry_count) 
  
  charge_by_chargeid = charges.df.filtered %>%
    group_by(., charge.id) %>%
    mutate(., description = first(charge.desc)) %>%
    summarize( count = sum(charge.count), description = first(description))%>%
    arrange( desc(count)) %>%
    top_n(charge_count) 
  
  # word cloud
  library(wordcloud) # this requires the tm and NLP packages
  library(devtools)
  library(SnowballC)
  
  #word cloud
  all_desc_vector <- as.vector(charges.df.filtered$description)
  entire_desc_text = capture.output(cat(all_desc_vector))
  #entire_desc_text = paste(charge_by_chargeid$description, collapse =" ")
  docs <- Corpus(VectorSource(entire_desc_text))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("near", "required", "upon", "use")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  #docs <- tm_map(docs, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v) 
}
      
      
      

  
