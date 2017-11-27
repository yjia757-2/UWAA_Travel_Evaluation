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



 #Data grouped by tour operator and tour's name.  


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
                  poor=CalPoor(.))) 
datasum.name[,"total"] <-rowSums(datasum.name[,c(2:6)])
name.eval<- datasum.name %>% group_by(tour_name) %>% summarise("Total"=sum(total),
                                                       "Excellent"=sum(excellent)/Total,
                                                       "VeryGood"=sum(very_good)/Total,
                                                       "Good"=sum(good)/Total,
                                                       "Fair"=sum(fair)/Total,
                                                       "Poor"=sum(poor)/Total)
name.eval$Excellent <-paste(round(name.eval$Excellent*100,2),"%",sep="")
name.eval$VeryGood <-paste(round(name.eval$VeryGood*100,2),"%",sep="")
name.eval$Good <-paste(round(name.eval$Good*100,2),"%",sep="")
name.eval$Fair <-paste(round(name.eval$Fair*100,2),"%",sep="")
name.eval$Poor <-paste(round(name.eval$Poor*100,2),"%",sep="")


datasum.operator <- datasummary2017 %>% 
  group_by(tour_operator) %>% 
  do(data.frame(excellent=CalExcellent(.),
                very_good=CalVeryGood(.),
                good=CalGood(.),
                fair=CalFair(.),
                poor=CalPoor(.))) %>% 
  select("tour_operator","excellent","very_good","good","fair","poor")
datasum.operator[,"total"] <-rowSums(datasum.operator[,c(2:6)])
operator.eval<- datasum.operator %>% group_by(tour_operator) %>% summarise("Total"=sum(total),
                                                               "Excellent"=sum(excellent)/Total,
                                                               "VeryGood"=sum(very_good)/Total,
                                                               "Good"=sum(good)/Total,
                                                               "Fair"=sum(fair)/Total,
                                                               "Poor"=sum(poor)/Total)
operator.eval$Excellent <-paste(round(operator.eval$Excellent*100,2),"%",sep="")
operator.eval$VeryGood <-paste(round(operator.eval$VeryGood*100,2),"%",sep="")
operator.eval$Good <-paste(round(operator.eval$Good*100,2),"%",sep="")
operator.eval$Fair <-paste(round(operator.eval$Fair*100,2),"%",sep="")
operator.eval$Poor <-paste(round(operator.eval$Poor*100,2),"%",sep="")




