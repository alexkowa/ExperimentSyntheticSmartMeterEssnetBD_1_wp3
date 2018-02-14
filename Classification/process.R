# Author: Alexander Kowarik
# Test classification methods for smart meter data sets
source("dataRead.R")# read csv files
source("helpers.R")# vacant and vacantArima function previously used for estimating vacancies
library(forecast);library(dtwclust);library(ggplot2);
library(tscount);library(BNPTSclust);library(kohonen)
library(lubridate)
library(cluster)
library(randomForest)
# daytime is assumed to be from 8:00 to 19:59, this could be refined in future
dp[,daytime:=!hour(time)%in%c(0:7,20:23)]
# Aggregate household smartmeters per day and ID
aggP <- dp[,.(
   v=vacant(value),
   outvol=sd(diff(scale(value)),na.rm=TRUE),
   outvar=var(value,na.rm=TRUE),
   outm=mean(value,na.rm=TRUE)/mean(value[!daytime],na.rm=TRUE),
   m=mean(value,na.rm=TRUE)
   ),by=.(day(time),month(time),year(time),ID)]

# Aggregate business smartmeters per day and ID
dl[,value:=as.numeric(value)]
dl[,daytime:=!hour(time)%in%c(0:7,20:23)]
aggL <- dl[,.(
  outvol=sd(diff(scale(value)),na.rm=TRUE),
  outvar=var(value,na.rm=TRUE),
  outm=mean(value,na.rm=TRUE)/mean(value[!daytime],na.rm=TRUE),
  m=mean(value,na.rm=TRUE)
),by=.(day(time),month(time),year(time),ID)]

# Helper to get the estonian names of weekdays Mon-Sun
wd <- c(weekdays(ymd("2018-02_05")),
  weekdays(ymd("2018-02_06")),
  weekdays(ymd("2018-02_07")),
  weekdays(ymd("2018-02_08")),
  weekdays(ymd("2018-02_09")),
  weekdays(ymd("2018-02_10")),
  weekdays(ymd("2018-02_11")))

aggP[,weekd:=weekdays(ymd(paste0(year,"-",month,"-",day)))]
# Aggregate household smartmeters per ID
aggP2 <- aggP[,.(vm=mean(v,na.rm = TRUE),vsd=sd(v,na.rm = TRUE),
                 outvolm=mean(outvol,na.rm = TRUE),outvolsd=sd(outvol,na.rm = TRUE),
                 outvarm=mean(outvar,na.rm = TRUE),outvarsd=sd(outvar,na.rm = TRUE),
                 outmm=mean(outm,na.rm = TRUE),
                 outmsd=sd(outm,na.rm = TRUE),mm=mean(m,na.rm = TRUE),
                 msd=sd(m,na.rm = TRUE),
                 ratiowe=mean(m[weekd%in%wd[1:5]],na.rm=TRUE)/
                   mean(m[weekd%in%wd[6:7]],
                                                  na.rm=TRUE))
              ,by=ID]
save(aggP,aggP2,file="../desktop/aggPrivate.Rdata")
aggL[,weekd:=weekdays(ymd(paste0(year,"-",month,"-",day)))]

# Aggregate business smartmeters per ID
aggL2 <- aggL[,.(
                 outvolm=mean(outvol,na.rm = TRUE),outvolsd=sd(outvol,na.rm = TRUE),
                 outvarm=mean(outvar,na.rm = TRUE),outvarsd=sd(outvar,na.rm = TRUE),
                 outmm=mean(outm,na.rm = TRUE),
                 outmsd=sd(outm,na.rm = TRUE),mm=mean(m,na.rm = TRUE),
                 msd=sd(m,na.rm = TRUE),
                 ratiowe=mean(m[weekd%in%wd[1:5]],na.rm=TRUE)/
                   mean(m[weekd%in%wd[6:7]],
                        na.rm=TRUE))
              ,by=ID]
aggP2[,type:="HH"]
aggL2[,type:="B"]
agg2 <- rbind(aggP2,aggL2,fill=TRUE)
agg2[,type:=as.factor(type)]
dat <- agg2[!is.na(outmm)&outmsd!=Inf&ratiowe!=Inf&
              !is.na(outvolm)&!is.na(outvolsd)]

# Using the library caret to train different kind of models with repeated cross validation
# confusion matrix used as simple measure of goodness of fit

# First step is to model the distinction business vs. household
library(caret)
TrainData <- dat[,4:13]

# k Nearest neighbour
knnFit <- train(type~.,data=TrainData,
                 method = "knn",
                 tuneLength = 10,
                 trControl = trainControl(method = "repeatedcv",number=10,repeats = 3))
print(cm <- confusionMatrix(knnFit))
write.csv2(cm$table,file.path(datapath,"Output","knnFit.csv"))

# Logistic regression
glmFit <- train(type~.,data=TrainData,
                 method = "glm",family="binomial",
                 preProcess = c("center", "scale"),
                 tuneLength = 10, 
                 trControl = trainControl(method = "repeatedcv",number=10,repeats = 3))
print(cm <- confusionMatrix(glmFit))
write.csv2(cm$table,file.path(datapath,"Output","glmFit.csv"))

# random forest
rfFit <- train(type~.,data=TrainData,
               method = "rf",
               preProcess = "range", 
               ntree=1000,#stand=TRUE,
               trControl = trainControl(method = "repeatedcv",number=10,repeats = 3))
print(cm <- confusionMatrix(rfFit))
write.csv2(cm$table,file.path(datapath,"Output","rfFit.csv"))

#neural net
nnetFit <- train(type~.,data=TrainData,
                 method = "nnet",
                 preProcess = "range", 
                 tuneLength = 4,
                 trace = FALSE,
                 trControl = trainControl(method = "repeatedcv",number=10,repeats = 3),
                 maxit = 100)
print(cm <- confusionMatrix(nnetFit))
write.csv2(cm$table,file.path(datapath,"Output","nnetFit.csv"))

#support vector machine
svmFit <- train(type~.,data=TrainData,
                 method = "svmLinearWeights",
                 trControl = trainControl(method = "repeatedcv",number=10,repeats = 3)
                )
print(cm <- confusionMatrix(svmFit))
write.csv2(cm$table,file.path(datapath,"Output","svmFit.csv"))

#Bagged CART
treeFit <- train(type~.,data=TrainData,
                 method = "treebag",
                 trControl = trainControl(method = "repeatedcv",number=10,repeats = 3)
)
print(cm <- confusionMatrix(treeFit))
write.csv2(cm$table,file.path(datapath,"Output","treeFit.csv"))

#Bayesian Generalized Linear Model
bayesglmFit <- train(type~.,data=TrainData,
                     method = "bayesglm",
                     trControl = trainControl(method = "repeatedcv",number=10,repeats = 3)
)
print(cm <- confusionMatrix(bayesglmFit))
write.csv2(cm$table,file.path(datapath,"Output","bayesglmFit.csv"))

#Boosted Logistic Regression
logitBoostFit <- train(type~.,data=TrainData,
                     method = "LogitBoost",
                     trControl = trainControl(method = "repeatedcv",number=10,repeats = 3)
)
print(cm <- confusionMatrix(logitBoostFit))
write.csv2(cm$table,file.path(datapath,"Output","logitBoostFit.csv"))


TrainData2 <- copy(TrainData)

######## NACE
TrainDataB <- aggL2[!is.na(outmm)&outmsd!=Inf&ratiowe!=Inf&
                      !is.na(outvolm)&!is.na(outvolsd),c(-11),with=FALSE]
TrainDataB <- merge(TrainDataB,pl[,.(ID,nace)],by="ID")
TrainDataB[,nace:=floor(nace/100)]
ngr <- list(1:3,5:9,10:33,35:39,41:43,45:47,49:53,55:56,58:63,64:66,68,69:75,77:82,85:99)
nnam <- sapply(ngr,function(x)paste0(x,collapse="-"))
TrainDataB[,naceG:=nnam[sapply(ngr,function(x)nace[1]%in%x)],by=ID]
TrainDataB[,nace:=NULL]
rfFit <- train(naceG~.,data=TrainDataB[!is.na(naceG)],
               method = "rf",
               #preProcess = "BoxCox", 
               ntree=500,#stand=TRUE,
               trControl = trainControl(method = "repeatedcv",number=10,repeats = 3),
               savePredictions=TRUE)
print(cm <- confusionMatrix(rfFit))
write.csv2(cm$table,file.path(datapath,"Output","rfFitNACE.csv"))

######## Household Size
TrainDataH <- aggP2[!is.na(outmm)&outmsd!=Inf&ratiowe!=Inf&
                      !is.na(outvolm)&!is.na(outvolsd),c(-13),with=FALSE]
TrainDataH <- merge(TrainDataH,pp[,.(ID,hh_size)],by="ID")

TrainDataH[,table(hh_size)]
TrainDataH[hh_size>5,hh_size:=5]
TrainDataH[,hh_size:=ordered(hh_size)]

rfFit <- train(hh_size~.,data=TrainDataH,
               method = "rf",
               #preProcess = "BoxCox", 
               ntree=50,#stand=TRUE,
               trControl = trainControl(method = "repeatedcv",number=10,repeats = 3),
               savePredictions=TRUE)
print(cm <- confusionMatrix(rfFit))
write.csv2(cm$table,file.path(datapath,"Output","rfFitHHSIZE.csv"))



######## Urban
TrainDataH <- aggP2[!is.na(outmm)&outmsd!=Inf&ratiowe!=Inf&
                      !is.na(outvolm)&!is.na(outvolsd),c(-13),with=FALSE]
TrainDataH <- merge(TrainDataH,pp[,.(ID,urban)],by="ID")


rfFit <- train(urban~.,data=TrainDataH,
               method = "rf",
               #preProcess = "BoxCox", 
               ntree=50,#stand=TRUE,
               trControl = trainControl(method = "repeatedcv",number=10,repeats = 3),
               savePredictions=TRUE)
print(cm <- confusionMatrix(rfFit))
write.csv2(cm$table,file.path(datapath,"Output","rfFitURBAN.csv"))

