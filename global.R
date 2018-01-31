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

charges.df = filter( charges.df, !grepl('Outside', borough) )
charges.df = filter( charges.df, borough != "")



