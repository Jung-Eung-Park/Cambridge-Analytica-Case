#######################################
## Cambridge Analytica-Facebook Demo ##
## version December 16, 2019 ###########
## by Jung PARK #######################
#######################################

## Basic confirguration
library(hrbrthemes)
library(rstudioapi) 
library(data.table)
library(ggplot2)
library(stargazer)
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path )); print( getwd() )


## Read & Process data
DataRaw <- read.csv("data.csv", stringsAsFactors=FALSE)
DataSelected <- DataRaw[-1:-2,19:28] 
DataSelected[DataSelected=="Like"] <- 1
Num <-data.frame(data.matrix(DataSelected))
Num[is.na(Num)] <- 0

ID <- DataRaw[-1:-2,18]
AgeTrue <- DataRaw[-1:-2,29]
GenderTrue <- DataRaw[-1:-2,30]
GenderTrue[GenderTrue=="Female"] <- 0
GenderTrue[GenderTrue=="Male"] <- 1
GenderTrue[GenderTrue=="Female,Male"] <- ""


## Create Linear Regression Models using the training data
TrainData <- read.csv("traindata.csv", stringsAsFactors=FALSE)      

AgeLinearModel <- lm(age ~ Q6+Q8+Q3+Q7+Q9, data=TrainData) 
summary(AgeLinearModel)

GenderLinearModel <- lm(gender ~ Q2+Q5+Q10+Q1+Q4, data=TrainData) 
summary(GenderLinearModel)



## Predict using models

AgePred <- predict(AgeLinearModel, Num)
GenderPred <- predict(GenderLinearModel, Num)

AgePred<- round(AgePred, digits = 1)
GenderPred<- round(GenderPred, digits = 1)
Prediction <- data.frame(t(rbind(ID,AgePred,GenderPred, AgeTrue, GenderTrue)))


## Visualization
ggplot(Prediction, aes(x=AgePred, y=GenderPred))+
  geom_point(size=6, color="#69b3a2") +
  labs(x = "Predicted age [years]", y = "Gender characteristic [female=0/ male=1]") +
  theme_ipsum() +
  geom_text(
    label=ID,
    nudge_x = 0.25, nudge_y = 0.25,
    check_overlap = T
  )


ggplot(Prediction, aes(x=AgeTrue, y=AgePred))+
  geom_point(size=6, color="#69b3a2") +
  labs(x = "True age [years]", y = "Predicted age [years]") +
  theme_ipsum() +
  geom_text(
    label=ID,
    nudge_x = 0.25, nudge_y = 0.25,
    check_overlap = T
  )

ggplot(Prediction, aes(x=GenderTrue, y=GenderPred))+
  geom_point(size=6, color="#69b3a2") +
  labs(x = "True Gender [female=0/ male=1]", y = "Predicted Gender characteristic [female=0/ male=1]") +
  theme_ipsum() +
  geom_text(
    label=ID,
    nudge_x = 0.25, nudge_y = 0.25,
    check_overlap = T
  )








### 
# 
# 
# ## Generate artifical 2000 data as training data
# set.seed(1)
# TrainData<- matrix(c(0), nrow =2000, ncol =12)
# colnames(TrainData) <- c("Q1","Q2","Q3","Q4","Q5",
#                   "Q6","Q7","Q8","Q9","Q10","age","gender")
# TrainData <-data.table(TrainData)
# 
# i<-1
# for (g in 0:1){
#   TrainData[seq(i,i+999),12]<-g
#   pg <- 0.2 + g*0.6
#   a<- sample(c(1,0), size=1000, replace = TRUE, prob = c(pg, 1-pg))
#   TrainData[seq(i,i+999),2]<-a  
#   a<- sample(c(1,0), size=1000, replace = TRUE, prob = c(pg, 1-pg))
#   TrainData[seq(i,i+999),5]<-a  
#   a<- sample(c(1,0), size=1000, replace = TRUE, prob = c(pg, 1-pg))
#   TrainData[seq(i,i+999),10]<-a  
#   a<- sample(c(0,1), size=1000, replace = TRUE, prob = c(pg, 1-pg))
#   TrainData[seq(i,i+999),1]<-a  
#   a<- sample(c(0,1), size=1000, replace = TRUE, prob = c(pg, 1-pg))
#   TrainData[seq(i,i+999),4]<-a  
#   
#   for (a in 1:10){
#     p<-a*0.1
#     TrainData[seq(i,i+99),11]<- 20 + (a-1)*5    
#     a<- sample(c(1,0), size=100, replace = TRUE, prob = c(p,1-p))
#     TrainData[seq(i,i+99),6]<-a
#     a<- sample(c(1,0), size=100, replace = TRUE, prob = c(p,1-p))
#     TrainData[seq(i,i+99),8]<-a
#     a<- sample(c(0,1), size=100, replace = TRUE, prob = c(p,1-p))
#     TrainData[seq(i,i+99),3]<-a
#     a<- sample(c(0,1), size=100, replace = TRUE, prob = c(p,1-p))
#     TrainData[seq(i,i+99),7]<-a
#     a<- sample(c(0,1), size=100, replace = TRUE, prob = c(p,1-p))
#     TrainData[seq(i,i+99),9]<-a
#     i<-i+100
#   }
# }
# write.csv(TrainData,"traindata.csv")

# ## Create Linear Regression Models using the training data
# AgePred <- predict(AgeLinearModel, TrainData)
# GenderPred <- predict(GenderLinearModel, TrainData)
# ID <- 1:2000; AgeTrue <- TrainData$age; GenderTrue <- TrainData$gender
# Prediction <- data.frame(t(rbind(ID,AgePred,GenderPred, AgeTrue, GenderTrue)))
# 
# 
# ## Visualization
# ggplot(Prediction, aes(x=AgeTrue, y=AgePred))+ 
#   geom_point(size=6, color="#69b3a2") +
#   labs(x = "Trud age [years]", y = "Predicted age [years]") +
#   theme_ipsum() +
#   geom_text(
#     label=ID, 
#     nudge_x = 0.25, nudge_y = 0.25, 
#     check_overlap = T
#   )   
# 
# ggplot(Prediction, aes(x=GenderTrue, y=GenderPred))+ 
#   geom_point(size=6, color="#69b3a2") +
#   labs(x = "True Gender [female=0/ male=1]", y = "Predicted Gender characteristic [female=0/ male=1]") +
#   theme_ipsum() +
#   geom_text(
#     label=ID, 
#     nudge_x = 0.25, nudge_y = 0.25, 
#     check_overlap = T
#   )   

