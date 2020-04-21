rm(list=ls())
library(randomForest)
library(h2o) #use h2o pacakge for random forest
library(data.table) #load data.table pacakge for faster reading data
h2o.init(nthreads=-1,           ## -1: use all available threads
         max_mem_size = '8G')   ## specify the memory size for the H2O cloud
### load both files using fread for faster reading
dftrain<-fread("train.csv", stringsAsFactors=TRUE)
dftest<-fread("test.csv", stringsAsFactors=TRUE) 
###convert data into h2o dataframe for training model.
#this step will automically handle NAs in dataset
train <- as.h2o(dftrain, destination_frame="train.hex")
test<-as.h2o(dftest, destination_frame="test.hex")
##convert y variable into factor
train$target<-as.factor(train$target)
##split train dataset into train and validation dataset
splits<-h2o.splitFrame(train,0.8,destination_frames = c("trainSplit","validSplit"),seed=111111111)
##model section
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  
  training_frame = splits[[1]],        ## the H2O frame for training
  
  validation_frame = splits[[2]],      ## the H2O frame for validation
  
  x=3:133,                        ## the predictor columns, by column index
  
  y=2,                          ## the target index (what we are predicting)
  
  
  model_id = "randomforest",              ## name the model in H2O
  
  ntrees = 300,                  ## use a maximum of 300 trees to create the random forest model. 
  
                                 ##  I have increased it because I will let the early stopping criteria decide when
  
                                  ##  the random forest is sufficiently accurate
  
  sample_rate = 0.9,                ## 80% row sampling
  
  
  
  seed = 1000000)                ## Set the random seed so that this can be reproduced.
summary(rf1)
h2o.logloss(rf1,valid=T)        ##print validation loss
h2o.logloss(rf1,train=T)
final_pred<-as.data.frame(h2o.predict(object = rf1,newdata = test)) ##make predictions on test dataset
testIds<-as.data.frame(test$ID)
results<-data.frame(cbind(testIds,final_pred$p1))
colnames(results)<-c("ID","PredictedProb")
#write.csv(submission,"H2O_GBM.csv",row.names=F)
