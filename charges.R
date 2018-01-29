library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)

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
  top_n(20)

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
  arrange( desc(count))

ggplot(data=charge_by_boro, aes(x=borough, y=count, fill = count)) +
  geom_bar(stat = "identity", colour="blue")

p <- ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(stat="identity")

# clean up the 1.03 invalid entries
# track repeat offenders, where it's greater than 1

#create range input for top_n


print(g)

