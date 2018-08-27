# Pearl3T Behavior
rm(list=ls())

#--------------- load libraries ------------------------------------------------#

library(MASS)
library(plotrix)
library(reshape2)
library(nlme)
library(multcomp)
library(ez)
library(MuMIn)
library(heplots)
library(ggplot2)

#install.packages('heplots')

#---------------      load preprocessed data files  ----------------------------#
setwd('/Users/sarajahfari/Documents/Github/Pearl3T')
load('./Data/Behavior/Pearl3T_beh43.Rdat') # load summarized behavior and Qlearning estimates
load('./Data/Behavior/Obs_simulated43.Rdat')
source('./Code/Beh_comparesimtoobserved.R')
load("./Data/Behavior/Group_STANQposteriors.Rdat") # group posteriors stan file also used of control_conflict 
#---------------        load libraries              ----------------------------#

# learning phase accuracy, what is effect of feedback on accuracy
# is learning in each pair better than 50%
t.test(Pearl3T_beh43$Learn[,'%AB'],mu=0.5)
t.test(Pearl3T_beh43$Learn[,'%CD'],mu=0.5)
t.test(Pearl3T_beh43$Learn[,'%EF'],mu=0.5)

# is there a main effect of pair on accuracy: yes -> AB, CD, EF 
RL.av.ac=as.data.frame(as.table(Pearl3T_beh43$Learn[,7:9]))
colnames(RL.av.ac)=c('ppn','pair','AC')
lme_ac=lme(AC~pair,data=RL.av.ac,random=~1|ppn)
anova(lme_ac) 
#summary(lme_ac)
# now do corrected post-hoc test to compare pairs
summary(glht(lme_ac, linfct=mcp(pair = "Tukey")), test = adjusted(type = "bonferroni"))	

# Is there an effect of pair on Q.diff
qdiff=as.data.frame(as.table(Pearl3T_beh43$Qpar[,c("QL-a","QL-c","QL-e")]-Pearl3T_beh43$Qpar[,c("QL-b","QL-d","QL-f")]))
colnames(qdiff)=c('ppn','pair','Q')
lme_Q=lme(Q~pair,data=qdiff,random=~1|ppn)
anova(lme_Q) 
# now do corrected post-hoc test to compare pairs
summary(glht(lme_Q, linfct=mcp(pair = "Tukey")), test = adjusted(type = "bonferroni"))	


# now look at model-simulated vs observed
#0) load simulated vs observed, and compute 6bins for each
NBINS=6
obs.vs.sim=created_simvsobs(obs.learn,simulated,NBINS)	 

#1) is simulated comparable to observed in all bins
which(obs.vs.sim$p.ttest.bins<0.05) # yes!



