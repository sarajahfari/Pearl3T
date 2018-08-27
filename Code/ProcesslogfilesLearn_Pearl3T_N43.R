##############################################
#### Get summary learning phase            ###
##############################################

rm(list=ls())

datadir='~/Dropbox/Projects/Ciras/MRI_study/Logfiles/_scanner/learning'
outputdir='/Users/sarajahfari/Documents/Github/Pearl3T/Data/Behavior'


Learning_beh = function(datadir, outputdir)

{
	########################
	# load library's
	########################

	# Load libraries 

	library(MASS)
	#library(plotrix)
	library(lattice)
	library(foreign)
	library(gtools)
  

	
	#################################
	#### setwd to run function from
	#################################

	setwd(datadir)

	all = list.files()
	N = length(all)
	
	# get the new BIDS names
	read.table('~/Dropbox/Scripts/CiRaS/sctoBIDS/OldandNewSubjID.txt',h=T) -> sctobids
	# people to remove momevent, incompete, or weired scans
	out=paste('sub-',sprintf("%03d", c(4,12,17,18,33,46)),sep='')
	# change old names to BIDS
	ALL=sctobids[sctobids[,'subject_id']%in%all,'BIDS_names']

	# learning total
	learn = matrix(,length(all),15)
	colnames(learn)=c('RTA','RTB','RTC','RTD','RTE','RTF',
						'%AB','%CD','%EF','medRTA','medRTB','medRTC','medRTD','medRTE','medRTF')
	
	Omission=matrix(,length(all),3)
	colnames(Omission)=c('L1','L2','L')
	
	rownames(learn)=ALL
	rownames(Omission)=ALL
	
	get.num = function(x)
		{if(is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)}
		
	for ( i in 1:length(all))

		{	# run function to compute all variables
			
			
			# set working directory to run the function from
		setwd(all[i])

			#########################################################
			######## Analyse script voor reinforcelearning 	#########
			#########################################################

			data = list.files()
			
			Learn1 = read.table(data[grep('plearning1_',data)],h=T)
			Learn2 = read.table(data[grep('plearning2_',data)],h=T)
			
			learn1=rbind(Learn1, Learn2)
			###########################################################
			# learn
			# select correct (most rewarding) choice trials
			Clearn =learn1[odd(learn1[,2])&learn1[,4]==1
						|even(learn1[,2])&learn1[,4]==2,]
						
			NClearn =learn1[odd(learn1[,2])&learn1[,4]==2
						|even(learn1[,2])&learn1[,4]==1,]
						
			RT_A= Clearn[Clearn[,2]<3,c(2,4,7)]
			RT_B= NClearn[NClearn[,2]<3,c(2,4,7)]
			
			RT_C= Clearn[Clearn[,2]>2&Clearn[,2]<5,c(2,4,7)]
			RT_D= NClearn[NClearn[,2]>2&NClearn[,2]<5,c(2,4,7)]
			
			RT_E= Clearn[Clearn[,2]>4,c(2,4,7)]
			RT_F= NClearn[NClearn[,2]>4,c(2,4,7)]
						
			
			RT=c(mean(RT_A[,3]),mean(RT_B[,3]),mean(RT_C[,3]),
				mean(RT_D[,3]),mean(RT_E[,3]),mean(RT_F[,3]))
				
			medRT=c(median(RT_A[,3]),median(RT_B[,3]),median(RT_C[,3]),
				median(RT_D[,3]),median(RT_E[,3]),median(RT_F[,3]))
				
			aCC = table(Clearn[,2])/table(learn1[,2])
			AC = c(mean(aCC[1:2]),mean(aCC[3:4]),mean(aCC[5:6]))
			
			OML1=length(which(Learn1[,5]==2))/length(Learn1[,5]==2)*100
			OML2=length(which(Learn2[,5]==2))/length(Learn2[,5]==2)*100
			OMT=length(which(learn1[,5]==2))/length(learn1[,5]==2)*100
			OML=c(OML1,OML2,OMT)
			
			
			learn[i,]=round(c(RT,AC,medRT),digits=2)
			Omission[i,]=OML
			
			setwd(datadir)
			}
			
			setwd(outputdir)
			# remove subjects with odd scans, movement
			ALL43=ALL[-which(ALL%in%out)] 
			Pearl3T_learn43=list(learn= learn[rownames(learn)%in%ALL43,],Omission= round(Omission[rownames(Omission)%in%ALL43,],digits=2))
			print(Pearl3T_learn43)
			save(Pearl3T_learn43,file='Pearl3T_learn43.Rdat')
			
			}
			
			
Learning_beh=Learning_beh(datadir,outputdir)