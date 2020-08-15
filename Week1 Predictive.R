library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(readr)
library(zeallot)
library(countrycode)
library(ISLR)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(GoodmanKruskal)
library(arm)
library(randomForest)

ECG <- read.csv("C:\Users\sonal\Desktop\Predictive analysis\ECG200.txt")


data<-read.table(file.choose(), header = T, sep = ",", dec = ".")#Importing the data 
head(data)  #Top observations present in the data
dim(data)   #Check the dimensions of the data
summary(data) #Summarise the data

#Splitting the data set into train and test
set.seed(2)

part <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))

train<- read.csv("C:\Users\sonal\Desktop\Predictive analysis\ECG200_TRAIN.txt")

test <- read.csv("C:\Users\sonal\Desktop\Predictive analysis\ECG200_TEST.txt")

#Calculating the euclidean distance

ED<-function(data1,data2){
  distance=0
  for (i in (1:(length(data1)-1))){
    distance=distance+(data1[i]-data2[i])^2
  }
  return(sqrt(distance))
}

#Writing the function to predict kNN
knn_predict <- function(test, train, k_value){
  pred <- c()  
  #LOOP-1
  for(i in c(1:nrow(test))){   
    dist = c()          
    char = c()
    setosa =0              
    versicolor = 0
    virginica = 0
  }
  
  #LOOP-2-looping over train data 
  for(j in c(1:nrow(train))){}
  
  dist <- c(dist, ED(test[i,], train[j,]))
  char <- c(char, as.character(train[j,][[5]]))
  
  
  df <- data.frame(char, dist$SepalLength) 
  df <- df[order(df$dist.SepalLength),]       #sorting dataframe
  df <- df[1:k_value,]               
  
  
  #Loop 3: loops over df and counts classes of neibhors.
  for(k in c(1:nrow(df))){
    if(as.character(df[k,"char"]) == "setosa"){
      setosa = setosa + 1
    }else if(as.character(df[k,"char"]) == "versicolor"){
      versicolor = versicolor + 1
    }else
      virginica = virginica + 1
  }
  
  
  n<-table(df$char)
  pred=names(n)[which(n==max(n))]
  
  return(pred) #return prediction vector
}

#Predicting the value for K=3
K=3
predictions <- knn_predict(test, train, K)

#Predicting the value for K=5
K=5
predictions <- knn_predict(test, train, K)

#Predicting the value for K=11
K=11
predictions <- knn_predict(test, train, K)



#p = 0.5, 1, 2, 4 

