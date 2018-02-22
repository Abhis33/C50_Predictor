#' A C50 Predictor Function
#'
#' Suspect(homicide) probaility predictor using C50 (decision tree variant) algorithm.
#' @param data.csv and counter = 4
#' @keywords C50 Predictor
#' @export
#' @predictor("abc.csv",4), predictor("data.csv",9). 
#' predictor(file="data.csv",cnt=4)

#library(C50)

predictor <- function(file="data.csv",cnt=4){

#Models downloaded and loaded from Github

result_age <- readRDS(url("https://github.com/Abhis33/Models/raw/master/model_age.rds"))
result_race <- readRDS(url("https://github.com/Abhis33/Models/raw/master/model_race.rds"))
result_rel <- readRDS(url("https://github.com/Abhis33/Models/raw/master/model_rel.rds"))
result_sex <- readRDS(url("https://github.com/Abhis33/Models/raw/master/model_sex.rds"))

MyData <- read.csv(file=file, header=TRUE, sep=",")
test_set <- MyData[1:cnt,]

#Prediction for Suspect Age.

pred <-as.character(predict(result_age, newdata = test_set[]))
prob <- data.frame(predict(result_age, test_set[,-7],type="prob"))
resultset_age = array(0,dim=c(cnt))

for(i in 1:cnt){
  resultset_age[i]=(prob[[pred[i]]][i])
  if(pred[i]==as.character(test_set[i,7])){
    resultset_age[i]=(prob[[pred[i]]][i])
  }
}

#Prediction for Suspect Race.

pred <-as.character(predict(result_race, newdata = test_set[]))
prob <- data.frame(predict(result_race, test_set[,-8],type="prob"))
resultset_race = array(0,dim=c(cnt))

for(i in 1:cnt){
  resultset_race[i]=(prob[[pred[i]]][i])
  if(pred[i]==as.character(test_set[i,8])){
    resultset_race[i]=(prob[[pred[i]]][i])
  }
}

#Prediction for Suspect Reltaionship with victim.

pred <-as.character(predict(result_rel, newdata = test_set[]))
prob <- data.frame(predict(result_rel, test_set[,-9],type="prob"))
resultset_rel = array(0,dim=c(cnt))

for(i in 1:cnt){
  resultset_rel[i]=(prob[[pred[i]]][i])
  if(pred[i]==as.character(test_set[i,9])){
    resultset_rel[i]=(prob[[pred[i]]][i])
  }
}

#Prediction for Suspect Sex.

pred <-as.character(predict(result_sex, newdata = test_set[]))
prob <- data.frame(predict(result_sex, test_set[,-6],type="prob"))
resultset_sex = array(0,dim=c(cnt))

for(i in 1:cnt){
  resultset_sex[i]=(prob[[pred[i]]][i])
  if(pred[i]==as.character(test_set[i,6])){
    resultset_sex[i]=(prob[[pred[i]]][i])
  }
}

resultset_all = array(0,dim=c(cnt))

#Simple averaging of all fields.

for(i in 1:cnt){
  resultset_all[i]=(resultset_age[i]+resultset_rel[i]+resultset_race[i]+resultset_sex[i])/4
}

return(resultset_all)
}
