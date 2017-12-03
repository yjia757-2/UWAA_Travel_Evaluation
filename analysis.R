library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
library(formattable)

# Write a function about reading the file 
InputData <-function(x){
  read.csv(x,stringsAsFactors = FALSE)
}
# I got the summary of how much percentage of each evaluation. 

Group.Evaluation <- function(annualdata){
  f <-function (x) {
  col_name <- as.name(colnames(annualdata %>% select(x)))
  group.eval <-annualdata %>% 
    group_by_(col_name) %>% 
    summarise(n=n())
  group.eval <-group.eval[complete.cases(group.eval), ]
  total.number <-sum(group.eval$n)
  
  Excellent <-percent(as.numeric(group.eval[group.eval[,1]=="Excellent",2])/total.number)
  VeryGood <-percent(as.numeric(group.eval[group.eval[,1]=="Very Good",2])/total.number)
  Good <-percent(as.numeric(group.eval[group.eval[,1]=="Good",2])/total.number)
  Fair <-percent(as.numeric(group.eval[group.eval[,1]=="Fair",2])/total.number)
  Poor <-percent(as.numeric(group.eval[group.eval[,1]=="Poor",2])/total.number)
  
  eval.data <- data.frame(TourEvaluation=colnames(annualdata%>% select(x)),
                      Excellent,
                      VeryGood,
                      Good,
                      Fair,
                      Poor)}
  eval.sum<-lapply(colnames(annualdata[,3:10]),f)
  eval.sum<-as.data.frame(do.call(rbind,eval.sum))
  eval.sum.avg <-as.data.frame(t(data.frame(as.character(percent(colMeans(eval.sum[,2:6],na.rm=TRUE))))))
  row.names(eval.sum.avg) <-row.names(NULL)
  eval.sum.avg <-eval.sum.avg %>% mutate("TourEvaluation"="Average") %>% select(TourEvaluation,everything())
  eval.total <-data.frame(rbind(as.matrix(eval.sum), as.matrix(eval.sum.avg)))
  return(eval.total)}



 #Data grouped by tour's name. 

NameEval <- function(annualdata) {

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
  
  datasum.name <-annualdata%>% 
  group_by(tour_name) %>% 
    do(data.frame(excellent=CalExcellent(.),
                  very_good=CalVeryGood(.),
                  good=CalGood(.),
                  fair=CalFair(.),
                  poor=CalPoor(.)))  
  datasum.name[,"total"] <-rowSums(datasum.name[,c(2:6)])
  datasum.name[is.na(datasum.name)]<-0
  name.eval<- datasum.name %>% group_by(tour_name) %>% summarise("Total"=sum(total),
                                                       "Excellent"=percent(sum(excellent)/Total),
                                                       "VeryGood"=percent(sum(very_good)/Total),
                                                       "Good"=percent(sum(good)/Total),
                                                       "Fair"=percent(sum(fair)/Total),
                                                       "Poor"=percent(sum(poor)/Total))
  
  return(name.eval)}

#Data grouped by tour's operator. 

OperatorEval <- function (annualdata) {
  
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
  
  datasum.operator <-annualdata%>% 
  group_by(tour_operator) %>% 
  do(data.frame(excellent=CalExcellent(.),
                very_good=CalVeryGood(.),
                good=CalGood(.),
                fair=CalFair(.),
                poor=CalPoor(.))) %>% 
  select("tour_operator","excellent","very_good","good","fair","poor")
  datasum.operator[,"total"] <-rowSums(datasum.operator[,c(2:6)])
  datasum.operator[is.na(datasum.operator)]<-0
  operator.eval<- datasum.operator %>% group_by(tour_operator) %>% summarise("Total"=sum(total),
                                                               "Excellent"=percent(sum(excellent)/Total),
                                                               "VeryGood"=percent(sum(very_good)/Total),
                                                               "Good"=percent(sum(good)/Total),
                                                               "Fair"=percent(sum(fair)/Total),
                                                               "Poor"=percent(sum(poor)/Total))

  return(operator.eval)}





