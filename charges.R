library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(wordcloud)


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

ggplot(data=charge_by_boro, aes(x=borough, y=count, fill = count)) +
  geom_bar(stat = "identity", colour="blue")

# word cloud
library(wordcloud) # this requires the tm and NLP packages
library(devtools)
library(SnowballC)

all_desc_vector <- as.vector(charge_by_chargeid$cat.descsample)
entire_desc_text = capture.output(cat(all_desc_vector))
#entire_desc_text = paste(charge_by_chargeid$cat.descsample, collapse =" ")
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
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2, scale=c(1.5,.5),
          max.words=500, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = entire_desc_text, min.freq = 50)
 # w/o min.req=1, you get just "merc"
# clean up the 1.03 invalid entries
# track repeat offenders, where it's greater than 1

#create range input for top_n


print(g)

