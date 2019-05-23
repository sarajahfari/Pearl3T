#------------------------set-up script to run 1000 RFs with different seed to grasp variability ---------------#
rm(list=ls())
#1- ----------------------load/install libraries ----------------------
# load required packages into memory
#install.packages('pacman')
library(pacman)

packages=c('rpart','rpart.plot','randomForest','RColorBrewer','AUC','doParallel','dplyr')
p_load(packages,character.only = TRUE)


#2--------------------load single trial and RF input data ---------------

# define test and training set
set.seed(218)

#setwd('/Users/sarajahfari/Spinoza/NEW')
setwd('~/Documents/Pearl3T/NEW/RF/RF_data')
load('~/Documents/Pearl3T/NEW/RF/RF_data/forestDat_N43_Rev.Rdat')

forestinall=forestinallR2R
rm(forestinallR2R)


#3- ----------------------load data, define predictors ----------------------


#control_regions=c('LO31','maxSTN25exc','PO14','SMA')

CBase=c("response","subject_id",'conflict',
     "Caudate40exc","Putamen40exc","Accumbens", 
     "FFA23","V1","VMPFC31thr","MotorBA4",
     "DLPFCposterior","preSMAsmall",'Q.diff')


# this was initially used to find optimal MTry (5 or 6)
#tuneRF(select(forestinallR2R,-response), as.factor(forestinallR2R$response), stepFactor=2,ntree=2000,improve=0.01,plot=T,mtryStart=3)->tune

#3- ----------------------define variables to save resutls and number of seeds --
# variables
Type='ALL'
SEEDS=100
PROX='FALSE'
MTRy=5
NTREE=2000
#block=5

#set.seed(218)

#setwd('/home/jahfari/Documents/NEW')
#setwd('/Users/sarajahfari/Library/Mobile\ Documents/com~apple~CloudDocs/Pearl3T')
#---------------------------------------------------------------------#


forestinall$conflict=droplevels(forestinall)$conflict
forestinall$option=droplevels(forestinall)$option
forestinall$response=ifelse(forestinall$response==0,'incorret','correct')

if (Type=='ALL') {Forestinall=forestinall[,CBase]} else
if (Type=='good') {Forestinall=forestinall[forestinall$Learner=='good',CBase]} else
if (Type=='bad') {Forestinall=forestinall[forestinall$Learner=='bad',CBase]}

#Forestinall=forestinall[,CBase]
samp=sample(1:dim(Forestinall)[1]) # get shuffle index
test=Forestinall[samp[1:floor(0.3*length(samp))],] # define test set (1/3 of trials)
train=Forestinall[-samp[1:floor(0.3*length(samp))],] # define train set (2/3 of trials)
#4.1 sample test and training set
# get number of counts minority class for stratification
nRareSamples = min(table(as.factor(train$response)))-1

# make a random test set to assess basline prediction/shuffle responses
RandomTest=test
RandomTest$response=sample(RandomTest$response)

#---------------------------------------------------------------------#
BLOCK=1:10
crusor=seq(1,1000,100)

for (block in BLOCK)
{
  
	#---------------------------------------------------------------------
  # traditional
	#data.frames
	PREDICT=matrix(,nrow=SEEDS,ncol=3)
	rownames(PREDICT)=paste0('seed_',crusor[block]:(crusor[block]+99))
	colnames(PREDICT)=c('pTrain','pTest','prandomTest')

	AUC=matrix(,nrow=SEEDS,ncol=3)
	colnames(AUC)=c('roc','specifity','sensitivity')
	rownames(AUC)=paste0('seed_',crusor[block]:(crusor[block]+99))

	# IMP=data.frame(regions=1:{length(Forest)-1})
	ex=2 # Q, response,control regions
	IMP.name=matrix(,nrow=SEEDS,ncol=length(CBase)-ex) # save variable names of imp sorted
	IMP.Daccuracy=matrix(,nrow=SEEDS,ncol=length(CBase)-ex) # save accuracy of imp sorted
	colnames(IMP.name)=paste0('Best_',1:length(IMP.name[1,]))
	rownames(IMP.name)=paste0('seed_',crusor[block]:(crusor[block]+99))
	colnames(IMP.Daccuracy)=paste0('Best_',1:length(IMP.name[1,]))
	rownames(IMP.Daccuracy)=paste0('seed_',crusor[block]:(crusor[block]+99))
	
	conf=list()
	#---------------------------------------------------------------------
	
	#------------------------start loop ----------------------
	track=0
	# start for loop over seeds
	for (seed in crusor[block]:(crusor[block]+99))
	{

		#4.0 set seed
		set.seed(seed)
	    track=track+1

		  
	    # ----------- standard as submitted in firs ms -------------------------------------------------#
	    # run forest
	    rf.strata = randomForest(as.factor(response) ~ .,
	        data=train[,-which(colnames(train)=='Q.diff')],strata=train$response,
	             sampsize=c(nRareSamples,nRareSamples),
	             ntree=NTREE,mtry=MTRy,importance=TRUE)
	    
	    
	    # prediction of test, train, dummy test
	    ### Check how well these complex model behave for stratified data 
	    Backtesting<- predict(rf.strata, train)
	    Prediction <- predict(rf.strata, test)
	    RPrediction <- predict(rf.strata, RandomTest) # for randomly shuffled data
	    ptr<-length(which(Backtesting==train$response))/length(train$response)
	    pts<-length(which(Prediction==test$response))/length(test$response)
	    prts<-length(which(RPrediction==RandomTest$response))/length(test$response)

	    PREDICT[track,]=round(c(ptr,pts,prts),digits=2)
      
	    
	    # get AUC, roc, specificity, sensitifity
	    AUC[track,]=round(c(
	    	auc(roc(rf.strata$votes[,2],as.factor(train$response))),
	    	auc(specificity(rf.strata$votes[,2],as.factor(train$response))),
	    	auc(sensitivity(rf.strata$votes[,2],as.factor(train$response)))
	    	),digits=2)

	    #ci(roc(rf.strata$votes,as.factor(train$response)),of = c("auc"))
	    # if requested you can add plot curves here

	    # now get importance of each region
	    im2=as.matrix(importance(rf.strata,scale=T,type=1))
	    imp2.sort =  as.matrix(im2[order(-im2[,'MeanDecreaseAccuracy']),])
	    iMP2=data.frame(rownames(imp2.sort),imp2.sort[,1])

	    IMP.name[track,]=rownames(imp2.sort)
	    IMP.Daccuracy[track,]=round(imp2.sort[,1],digits=2)

	    # now also save the confusion matrix
	    conf[[track]]=rf.strata$confusion

	    print(paste('done' ,seed))

	}# end seed loop

	names(conf)=paste0('seed_',crusor[block]:(crusor[block]+99))

	save(list=c('conf','IMP.name','IMP.Daccuracy','AUC','PREDICT'),
	    file=paste('~/Documents/Pearl3T/NEW/RF/RF_forests/R2R/PAUC_FORESTN43_onetraintest','_ntree',NTREE,'_block',block,
	        '_seeds',SEEDS, '_MTRY',MTRy,'type_',Type,'_prox',PROX,'.Rdat',sep=''))

	print(paste('done', block))
}# end block loop


