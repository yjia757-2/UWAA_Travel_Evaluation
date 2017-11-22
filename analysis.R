library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
datasummary2017 <-read.csv("data/data2017_jan_oct.csv",stringsAsFactors = FALSE)

# I got the summary of how much percentage of each evaluation. 
Group.Evaluation <-function (x) {
  col_name <- as.name(colnames(datasummary2017 %>% select(x)))
  group.eval <-datasummary2017 %>% 
    group_by_(col_name) %>% 
    summarise(n=n())
  group.eval <-group.eval[complete.cases(group.eval), ]
  total.number <-sum(group.eval$n)

pect.excellent <-paste(round(as.numeric(group.eval[group.eval[,1]=="Excellent",2])/total.number,4)*100, "%", sep="")
pect.good <-paste(round(as.numeric(group.eval[group.eval[,1]=="Good",2])/total.number,4)*100, "%", sep="")
pect.verygood <-paste(round(as.numeric(group.eval[group.eval[,1]=="Very Good",2])/total.number,4)*100, "%", sep="")
pect.fair <-paste(round(as.numeric(group.eval[group.eval[,1]=="Fair",2])/total.number,4)*100, "%", sep="")
pect.poor <-paste(round(as.numeric(group.eval[group.eval[,1]=="Poor",2])/total.number,4)*100, "%", sep="")
  
eval.data <- data.frame(
                      Excellent=pect.excellent,
                      VeryGood=pect.verygood,
                      Good=pect.good,
                      Fair=pect.fair,
                      Poor=pect.poor,
                      TourEvaluation=colnames(datasummary2017%>% select(x)))}


eval.sum <-lapply(colnames(datasummary2017[,3:10]),Group.Evaluation)
eval.sum <-as.data.frame(do.call(rbind, eval.sum ))



#I wrote function to get how many "good" in a given data set, which will be group_by tour_operator or tour_name. Each operator would have ten rows. since there are 10 column in the original dataframe. The computer will go over each row to count how many "good" in that. 


CalGood <-function (x){
  data <-x
  str_count(data,"Good")
}
CalVeryGood <-function (x){
  data <-x
  str_count(data,"Very Good")
}
CalExcellent <-function (x){
  data <-x
  str_count(data,"Excellent")
}
CalFair <-function (x){
  data <-x
  str_count(data,"Fair")
}
CalPoor <-function (x){
  data <-x
  str_count(data,"Poor")
}

datasum.name <- datasummary2017 %>% 
  group_by(tour_name) %>% 
    do(data.frame(excellent=CalExcellent(.),
                  very_good=CalVeryGood(.),
                  good=CalGood(.),
                  fair=CalFair(.),
                  poor=CalPoor(.)
                  ))

datasum.operator <- datasummary2017 %>% 
  group_by(tour_operator) %>% 
  do(data.frame(excellent=CalExcellent(.),
                very_good=CalVeryGood(.),
                good=CalGood(.),
                fair=CalFair(.),
                poor=CalPoor(.))) 
datasum.operator %>% group_by(tour_operator) %>% colSums(.)


# number of each evaluation we get through the year 
GoodEvaluation <-function (x){
  as.data.frame(lapply(datasummary2017[,3:10],function (i)sum(i=="Good",na.rm=TRUE)))}
cal <-function (x){
  data <-x
  sum(data$overall_assessment=="Good")
}





