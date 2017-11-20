library(dplyr)


# mannually convert the data from xls to csv. 
# read csv, get data frame 
tanzania.feb22 <-read.csv("Tanzania.feb22.csv", stringsAsFactors = FALSE)

# delete the columns that are not necessary. 
tanzania.feb22 <-tanzania.feb22 [,]%>%
  select(-matches('comments|Last|Name|Email|Entry|May|How|Date|Created|Updated'))

# rename
rownames(tanzania.feb22) <-NULL
tanzania.feb22$Overall.Assessment.of.Your.Tour="overall_assessment"
tanzania.feb22$Tour.Itinerary...Included.Excursions="tour_itinerary"

c <-tanzania.feb22 %>% select(contains("Director"))
colnames(c) <-"tour_director"
tanzania.feb22 <-tanzania.feb22 %>% select(-contains("Director"))
tanzania.feb22 <-mutate(tanzania.feb22,as.numercia(c))



# add tour name and tour operator. this would not be automically done by now since it is not included in our raw data. I suggest make a not in the future. 
colnames(tanzania.feb22)
