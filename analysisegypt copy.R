library(dplyr)
library(ggplot2)
egypt.jan9 <-read.csv("Egypt.jan9.test.csv",stringsAsFactors = FALSE)
egypt.jan16 <-read.csv("Egypt.jan16.test.csv",stringsAsFactors = FALSE)

Group.Evaluation <-function (x) {
  
  
  col_name <- as.name(colnames(egypt.jan9 %>% select(x)))
  group.eval <-egypt.jan9 %>% 
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
                      TourEvaluation=colnames(egypt.jan9%>% select(x)))}

# 一旦把所有的csv都下载下来了 已经一样的格式安排好，这样用mapply就可以了，因为有两个arguments data set 和x
eval.sum <-lapply(colnames(egypt.jan9[,3:10]),Group.Evaluation)
eval.sum <-as.data.frame(do.call(rbind, eval.sum ))
eval.sum <-eval.sum %>% mutate(TourOperator=egypt.jan9[1,]$tour_operator, TourName=egypt.jan9[1,]$tour_name)

