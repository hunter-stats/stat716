#Load Packages
#install.packages("randomForest")
library(ggplot2) 
library(ISLR)
library(caret)
library(randomForest)
library(boot)
library(MASS)
library(tree)
library(dplyr)
library(pROC)
library(gbm)


#Import Data
data<-read.csv(file = 'train_data.csv') # data using for training and testing 
data_for_final_model_submission<-read.csv('test_data.csv') # input data for final model
data=na.omit(data)
dim(data)   # 31647 x17
summary(data)


# change job level into groups
data<-data%>%
  mutate(job_lessGroup= ifelse(job%in%c('unknown'),yes = 'Unknown',
                               ifelse(job%in%c('Housemaid','retired','student','unemployed'),yes = 'Not_Workforce',
                                      no='Workforce')))
data$job_lessGroup<-factor(data$job_lessGroup)
summary(factor(data$job_lessGroup))


#############################################################################################################
# Modeling
#############################################################################################################
#############
# spliting the data into training and testing sets 80/20 splits
#############
set.seed(100)
split = sample(x = nrow(data),size = 0.8*nrow(data))
data.train<-data[split,]
data.test<-data[-split,]
summary(factor(data.train$y))/nrow(data.train)
summary(factor(data.test$y))/nrow(data.test) # it looks like the proportion of yes is close enough, no Stratified Sampling needed.
#############
#Bagging with CrossValidation####
#############
library(caret)
bg.cvControl=trainControl(method="cv",number=5,classProbs = TRUE) # could try 5 or more fold cross validation
set.seed(100)
mod.cvBagging<- train(y~age+job_lessGroup+education+marital+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,
                      data=data.train,
                      method="treebag",
                      trControl=bg.cvControl,
                      importance=TRUE) #final model of bagging


#saveRDS(mod.cvBagging,file = 'Final Model Objects/mod.bagging')# saving the final model object
#mod.cvBagging<-readRDS('Final Model Objects/mod.bagging')

#############
#Random Forest with CrossValidation####
#############
RF.cvControl=trainControl(method="cv",number=5,classProbs = TRUE)
RF.tuneGrid = expand.grid(mtry=1:5)
set.seed(100)
mod.cvForest = train(y~age+job_lessGroup+education+marital+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,
                     data=data.train,
                     method="rf",ntree=100,trControl=RF.cvControl,tuneGrid=RF.tuneGrid )
mod.cvForest 
#saveRDS(mod.cvForest,file = 'Final Model Objects/mod.Forest')# saving the final model object
#mod.cvForest<-readRDS('Final Model Objects/mod.Forest')


#############
#Boosting with CrossValidation####
#############
library(e1071)

# Boosting model with cross-validation - using the whole training data
modelLookup(model='gbm') # FYI
gbm.cvControl = trainControl(method="cv", number=5,classProbs = TRUE) #5 folds cross validation
tuneGrid=  expand.grid(n.trees = 100, interaction.depth = c(1,2),
                       shrinkage = (1:10)*0.01,n.minobsinnode=5)
set.seed(100)
mod.gbmCV = train(y~age+job_lessGroup+education+marital+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,
                  data=data.train, method="gbm",distribution="bernoulli", 
                  trControl=gbm.cvControl, verbose=F,tuneGrid=tuneGrid)
plot(varImp(object=mod.gbmCV),main='Vars of Imp') # plot variables of importance
mod.gbmCV$bestTune # list out the best turning parameter
plot(mod.gbmCV) # visualize parameter turning results
#saveRDS(mod.gbmCV,file = 'Final Model Objects/mod.gbm')# saving the final model object
#mod.gbmCV<-readRDS('Final Model Objects/mod.gbm')


#######################################################
# Comparing bagging, random forest & Boosting performance
#######################################################
#Bagging
pred.bagging<-predict(object=mod.cvBagging,newdata =data.test[,names(data.test)!='y'],
                            type="raw")
confusionMatrix(pred.bagging,data.test[,'y']) # confusing matrix from final model of training data
roc.bag<-roc(predict(object=mod.cvBagging,newdata = data.test[,names(data.test)!='y'],type="prob")$yes,
             response = data.test[,'y'])
plot(roc.bag,main='ROC of Bagging')
# Random Forest 
#final model performance overview -
pred.forest<-predict(object=mod.cvForest,newdata =data.test[,names(data.test)!='y'],type="raw") # using all data to check out the overall performace of the final model
confusionMatrix(pred.forest,data.test[,"y"]) # confusing matrix from final model of training data
roc.rf<-roc(predict(object=mod.cvForest,newdata=data.test[,names(data.test)!='y'],type="prob")$yes,
            response = data.test[,"y"])
plot(roc.rf,main='ROC of Random Forest')

# Boosting=GBM
pred.GBM<-predict(object=mod.gbmCV,newdata =data.test[,names(data.test)!='y'],type="raw")
confusionMatrix(pred.GBM,data.test[,"y"]) # confusing matrix from final model of training data
roc.gbm<-roc(predict.train(object=mod.gbmCV,newdata=data.test[,names(data.test)!='y'],type="prob")$yes,
             response = data.test[,"y"])
plot(roc.gbm,main='ROC of Boosting Model')

results<-data.frame(Model=c('Bagging','RandomForest','Boosting'),
                    Accuracy=c(confusionMatrix(pred.bagging,data.test[,"y"])$overall[1],
                               confusionMatrix(pred.forest,data.test[,"y"])$overall[1],
                               confusionMatrix(pred.GBM,data.test[,"y"])$overall[1]),
                    AUC=c(auc(roc.bag),auc(roc.rf),auc(roc.gbm))
                    )

saveRDS(results,'Final Model Objects/ModelResults')
print(results)
print(paste('Best model based on Accuracy -->',
            results$Model[results$Accuracy==max(results$Accuracy)]))
print(paste('Best model based on AUC -->',
            results$Model[results$AUC==max(results$AUC)]))
#Comment: Bagging performes the best accuracy and ROC AUC.






