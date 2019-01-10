rm(list=ls())

#------------------ load libaries---------------------------------------------#
# load required packages into memory
# install.packages('rpart')
# install.packages('rpart.plot')
# install.packages('randomForest')
# install.packages('rattle')
# install.packages('RColorBrewer')
# install.packages('caret')
# install.packages('AUC')
# install.packages('splitstackshape')

#------------------ load libaries---------------------------------------------#
# load required packages into memory
library(rpart)
library(rpart.plot)
library(randomForest)
#library(rattle)
library(RColorBrewer)
library(caret)
library(AUC)
library(splitstackshape)

#------------------ load data---------------------------------------------#

setwd('/home/jahfari/Documents/NEW')


# ------------------ Select regions for evaluation----------------------#
          Base=c("response","subject_id",
                 "Caudate40exc","Putamen40exc","Accumbens", 
                 "FFA23","V1","VMPFC31thr","MotorBA4",
                 "DLPFCposterior","preSMAsmall")
          
          BaseSide=c("response","subject_id",
                     "Caudate40exc","Putamen40exc","Accumbens", 
                     "V1.contra.chosen","FFA.contra.chosen",'FFA.diff','V1.diff'
                     ,"VMPFC31thr","MotorBA4","DLPFCposterior","preSMAsmall")
          
          Basedeon=c("response","subject_id",
                 "Caudate40exc","Putamen40exc","Accumbens", 
                 "FFA23","V1")
          
          # Select regions for evaluation
          CBase=c("response","subject_id",'conflict',
                 "Caudate40exc","Putamen40exc","Accumbens", 
                 "FFA23","V1","VMPFC31thr","MotorBA4",
                 "DLPFCposterior","preSMAsmall")
          
          CBaseSide=c("response","subject_id",'conflict',
                     "Caudate40exc","Putamen40exc","Accumbens", 
                     "V1.contra.chosen","FFA.contra.chosen",'FFA.diff','V1.diff'
                     ,"VMPFC31thr","MotorBA4","DLPFCposterior","preSMAsmall")
          
          CBasedeon=c("response","subject_id",'conflict',
                     "Caudate40exc","Putamen40exc","Accumbens", 
                     "FFA23","V1")
          
          # Select regions for evaluation
          LBase=c("response","subject_id",'Learner',
                  "Caudate40exc","Putamen40exc","Accumbens", 
                  "FFA23","V1","VMPFC31thr","MotorBA4",
                  "DLPFCposterior","preSMAsmall")
          
          LBaseSide=c("response","subject_id",'Learner',
                      "Caudate40exc","Putamen40exc","Accumbens", 
                      "V1.contra.chosen","FFA.contra.chosen",'FFA.diff','V1.diff'
                      ,"VMPFC31thr","MotorBA4","DLPFCposterior","preSMAsmall")
          
          LBasedeon=c("response","subject_id",'Learner',
                      "Caudate40exc","Putamen40exc","Accumbens", 
                      "FFA23","V1")
          
          # Select regions for evaluation
          CLBase=c("response","subject_id",'Learner','conflict',
                  "Caudate40exc","Putamen40exc","Accumbens", 
                  "FFA23","V1","VMPFC31thr","MotorBA4",
                  "DLPFCposterior","preSMAsmall")
          
          CLBaseSide=c("response","subject_id",'Learner','conflict',
                      "Caudate40exc","Putamen40exc","Accumbens", 
                      "V1.contra.chosen","FFA.contra.chosen",'FFA.diff','V1.diff'
                      ,"VMPFC31thr","MotorBA4","DLPFCposterior","preSMAsmall")
          
          CLBasedeon=c("response","subject_id",'Learner','conflict',
                      "Caudate40exc","Putamen40exc","Accumbens", 
                      "FFA23","V1")
          
          

Type='ALL'
SEED=2018
NTREE=2000
MTRy='SQRT'
PROX='none'
#------------------------------------- First run evaluations for all participants -------------------------------#
set.seed(2018)
          
          load('./RF/RF_data/forestDat_N43.Rdat')
          forestinall$conflict=droplevels(forestinall)$conflict
          forestinall$option=droplevels(forestinall)$option
          forestinall$response=ifelse(forestinall$response==0,'incorret','correct')
          
          # Define set, all, only good, or only bad
          if (Type=='ALL') {Forestinall=forestinall} else
          if (Type=='good') {Forestinall=forestinall[forestinall$Learner=='good',]} else
          if (Type=='bad') {Forestinall=forestinall[forestinall$Learner=='bad',]}
          
          samp=sample(1:dim(Forestinall)[1]) # get shuffle index
          test=Forestinall[samp[1:floor(0.3*length(samp))],] # define test set (1/3 of trials)
          train=Forestinall[-samp[1:floor(0.3*length(samp))],] # define train set (2/3 of trials)
          
          # make a random training set to assess basline prediction
          RandomTest=test
          RandomTest$response=sample(RandomTest$response)
          
          class.prevalence=table(as.factor(train$response))/dim(train)[1] # get bias 0(incorrect),1(correct)
          rare.class.prevalence = min(class.prevalence) # prob minority class
          nRareSamples = min(table(as.factor(train$response)))-1
          cRareSamples = min(table(as.factor(train$conflict)))
          
          # also make a stratified set that collects balanced sets per subject
          response=as.vector(apply(as.matrix(table(Forestinall$subject_id, Forestinall$response)),1,min))
          Str_subjects=data.frame()
          for (s in 1:length(response)){
            
            new=stratified(Forestinall[Forestinall$subject_id==levels(Forestinall$subject_id)[s],], "response", size=response[s])
            Str_subjects=rbind(new,Str_subjects)
          }
          
          sbSamp=sample(1:dim(Str_subjects)[1])
          sbTrain=as.data.frame(Str_subjects[-sbSamp[1:floor(0.2*length(sbSamp))],])
          sbTest=as.data.frame(Str_subjects[sbSamp[1:floor(0.2*length(sbSamp))],])
          # also make a test set with random responses
          sbTest_rr=sbTest
          sbTest_rr$response=sample(sbTest_rr$response)

                    # # put this in a list
          regions=list(
             BASE=colnames(forestinall)[which(colnames(forestinall)%in%Base)],# Base regions of interest, with sub_id
             BASESide=colnames(forestinall)[which(colnames(forestinall)%in%BaseSide)], # Base regions of interest, with sub_id,vis.chosen, and diff
             Basedecon=colnames(forestinall)[which(colnames(forestinall)%in%Basedeon)], # Fir regions + sub_id
             
             # do same but add valance, was choice trail (LL,WW or WL)
             CBASE=colnames(forestinall)[which(colnames(forestinall)%in%CBase)],# Base regions of interest, with sub_id
             CBASESide=colnames(forestinall)[which(colnames(forestinall)%in%CBaseSide)], # Base regions of interest, with sub_id,vis.chosen, and diff
             CBasedecon=colnames(forestinall)[which(colnames(forestinall)%in%CBasedeon)], # Fir regions + sub_id

             # do same but add Learner, was the subject a good or bad learner
             LBASE=colnames(forestinall)[which(colnames(forestinall)%in%LBase)],# Base regions of interest, with sub_id
             LBASESide=colnames(forestinall)[which(colnames(forestinall)%in%LBaseSide)], # Base regions of interest, with sub_id,vis.chosen, and diff
             LBasedecon=colnames(forestinall)[which(colnames(forestinall)%in%LBasedeon)], # Fir regions + sub_id

             # do same but add Learner, was the subject a good or bad learner
             CLBASE=colnames(forestinall)[which(colnames(forestinall)%in%CLBase)],# Base regions of interest, with sub_id
             CLBASESide=colnames(forestinall)[which(colnames(forestinall)%in%CLBaseSide)], # Base regions of interest, with sub_id,vis.chosen, and diff
             CLBasedecon=colnames(forestinall)[which(colnames(forestinall)%in%CLBasedeon)] # Fir regions + sub_id
           )
   
#------------------------------------- Now run evaluations for all participants -------------------------------#
          # define variables needed for the loop
          # start looping over all subjects
          track=0
          
          PREDICT.prox=matrix(,nrow=3,ncol=2*length(regions))
          rownames(PREDICT.prox)=c('pTrain','pTest','prandomTest')
          colnames(PREDICT.prox)=paste0(c('str_','sub_str_'),rep(names(regions),each=2))
          
          AUC.prox=matrix(,nrow=3,ncol=2*length(regions))
          rownames(AUC.prox)=c('roc','specifity','sensitivity')
          colnames(AUC.prox)=paste0(c('str','sub_str_'),rep(names(regions),each=2))
          
          # IMP=data.frame(regions=1:{length(Forest)-1})
          IMP.prox=list()
          conf.prox=list()
          
          for (RForest in regions)
          {
            track=track+1
          
            #2 - Balance the RF by sampling stratification
            rf.strata = randomForest(as.factor(response) ~ .,data=train[,RForest],strata=train$response,
                         sampsize=c(nRareSamples,nRareSamples),importance=TRUE,
                         ntree=NTREE)
            
            rf.strata.subject = randomForest(as.factor(response) ~ .,data=sbTrain[,RForest],importance=TRUE,
                                     ntree=NTREE)

            # prediction of test, train, dummy train
            ### Check how well these complex model behave
            # for stratified data random
            Backtesting<- predict(rf.strata, train)
            Prediction <- predict(rf.strata, test)
            RPrediction <- predict(rf.strata, RandomTest)
            ptr<-length(which(Backtesting==train$response))/length(train$response)
            pts<-length(which(Prediction==test$response))/length(test$response)
            prts<-length(which(RPrediction==RandomTest$response))/length(test$response)
            
            # for stratified data per subject
            sBacktesting<- predict(rf.strata.subject, sbTrain)
            sPrediction <- predict(rf.strata.subject, sbTest)
            sRPrediction <- predict(rf.strata.subject, sbTest_rr)
            sptr<-length(which(sBacktesting==sbTrain$response))/length(sbTrain$response)
            spts<-length(which(sPrediction==sbTest$response))/length(sbTest$response)
            sprts<-length(which(sRPrediction==sbTest_rr$response))/length(sbTest_rr$response)
            
            PREDICT.prox[,{(track*2)-1}:(track*2)]=round(cbind(c(ptr,pts,prts),c(sptr,spts,sprts)),digits=2)
            
            # AUC train, 
            AUC.prox[,{(track*2)-1}:(track*2)]=round(cbind(
                       c(auc(roc(rf.strata$votes[,2],as.factor(train$response))),
                       auc(specificity(rf.strata$votes[,2],as.factor(train$response))),
                       auc(sensitivity(rf.strata$votes[,2],as.factor(train$response)))),
                       c(auc(roc(rf.strata.subject$votes[,2],as.factor(sbTrain$response))),
                         auc(specificity(rf.strata.subject$votes[,2],as.factor(sbTrain$response))),
                         auc(sensitivity(rf.strata.subject$votes[,2],as.factor(sbTrain$response))))
                       ),digits=2)
            
            # importance and regions
            im2=as.matrix(importance(rf.strata,typ=1))
            imp2.sort =  as.matrix(im2[order(-im2[,'MeanDecreaseAccuracy']),])
            iMP2=data.frame(rownames(imp2.sort),imp2.sort[,1])
            rownames(iMP2)=1:{length(RForest)-1}
            names(iMP2) <- c('Region','accuracy_seed')
            
            s.im2=as.matrix(importance(rf.strata.subject,typ=1))
            s.imp2.sort =  as.matrix(s.im2[order(-s.im2[,'MeanDecreaseAccuracy']),])
            s.iMP2=data.frame(rownames(s.imp2.sort),s.imp2.sort[,1])
            rownames(s.iMP2)=1:{length(RForest)-1}
            names(s.iMP2) <- c('stsub.Region','stsub.accuracy_seed')
            
            IMP.prox[[track]]=data.frame(IMP.strata=iMP2,IMP.substrata=s.iMP2)
            
            # save confustion matrix
            conf.prox[[track]]=data.frame(strata=rf.strata$confusion,sub.strata=rf.strata.subject$confusion)
            
            print(paste('done' ,names(regions)[track]))
    
          }

names(IMP.prox)=names(regions)
names(conf.prox)=names(regions)

save(list=c('conf.prox','IMP.prox','AUC.prox','PREDICT.prox'),file=paste('./RF/RF_forests/PAUC_FORESTN43',
                                                                         '_ntree',NTREE,'_seed',SEED,
                                                                         '_MTRY',MTRy,'type_',Type,'_prox',PROX,
                                                                         '.Rdat',sep=''))

# neem side mee

#view OOB-CV specificity and sensitiviy
#plot(roc(rf.strata$votes[,2],as.factor(train$response)),main="AUC ROC")



