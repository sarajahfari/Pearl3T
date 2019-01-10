#------------------ load libaries---------------------------------------------#
install.packages('doParallel')

# load required packages into memory
library(rpart)
library(rpart.plot)
library(randomForest)
#library(rattle)
library(RColorBrewer)
library(caret)
library(AUC)
library(splitstackshape)
library(doParallel)



#------------------ load data---------------------------------------------#

#setwd('/home/jahfari/Documents/NEW')
setwd('/Users/sarajahfari/Github/Pearl3T')
#setwd('/home/jahfari/Documents/NEW')
load('./RF/RF_data/forestDat_N43.Rdat')
forestinall$conflict=droplevels(forestinall)$conflict
forestinall$option=droplevels(forestinall)$option
forestinall$response=ifelse(forestinall$response==0,'incorret','correct')


#------------------ define regions ---------------------------------------------#
set.seed(7)
# Select regions for evaluation
CBase=c("response","subject_id",'conflict',
     "Caudate40exc","Putamen40exc","Accumbens", 
     "FFA23","V1","VMPFC31thr","MotorBA4",
     "DLPFCposterior","preSMAsmall")


Forestinall=forestinall[,CBase]
samp=sample(1:dim(Forestinall)[1]) # get shuffle index
test=Forestinall[samp[1:floor(0.3*length(samp))],] # define test set (1/3 of trials)
train=Forestinall[-samp[1:floor(0.3*length(samp))],] # define train set (2/3 of trials)


class.prevalence=table(as.factor(train$response))/dim(train)[1] # get bias 0(incorrect),1(correct)
rare.class.prevalence = min(class.prevalence) # prob minority class
nRareSamples = min(table(as.factor(train$response)))-1


# make a random training set to assess basline prediction
RandomTest=test
RandomTest$response=sample(RandomTest$response)


#---------------
## library(doMC)
## registerDoMC(2)

# set cores

cl <- makePSOCKcluster(detectCores()-20); 
clusterEvalQ(cl, library(foreach)); registerDoParallel(cl)

seed <- 7
control <- trainControl(method="repeatedcv", number=10, repeats=3,allowParallel = TRUE)
metric <- "Accuracy"
set.seed(seed)
x=train[,-train$response]
y=as.factor(train$response)
tunegrid =data.frame(mtry = ncol(x))

rf2000 <- train(y=y, x=x, trControl = control, tuneGrid = tunegrid,method="rf",
	strata=y,sampsize=c(nRareSamples,nRareSamples),
	ntree=2000,metric="Accuracy",importance=TRUE)
stopCluster(cl);


#mtry <- sqrt(ncol(train))
#tunegrid <- expand.grid(.mtry=1:length(CBase))



rf2000 <- train(as.factor(response)~., data = train, trControl = control, tuneGrid = tunegrid,method="rf",
	strata=train$response,sampsize=c(nRareSamples,nRareSamples),
	ntree=2000,metric="Accuracy",importance=TRUE)
stopCluster(cl);
print(rf)
fm <- rf$finalModel

# with 500 trees best mtry =11, mean decrease gini accumbens, putamen, ffa, caudate, vi, vmpfc, rest



# predict the outcome on a test set
#rf_pred <- predict(fm, train)
# compare predicted outcome and true outcome
confusionMatrix(rf_pred, train$response)


data(fgl, package="MASS")
fgl.res <- tuneRF(Forestinall, Forestinall$response, stepFactor=1,ntree=2000,improve=0.05)


sqtmtry<- round(sqrt(ncol(mydata) - 1))
rfGrid <- expand.grid(mtry = c(round(sqtmtry / 2), sqtmtry, 2 * sqtmtry))

ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary, number = 10) 

rf.auc <- train(as.factor(response)~., data = train, trControl = ctrl, tunelength = 10,method="rf",
	strata=train$response,sampsize=c(nRareSamples,nRareSamples),ntree=2000,metric = "ROC")



stopCluster(cl);



