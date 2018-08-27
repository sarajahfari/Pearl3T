# behaviroal plots - run after Behavioral_stats

source('./Code/Behavior_stats.R')


#----------------- prepare files for plotting ------------------------------------------#

# behavior files

# learn
plearn=data.frame(name=c('Ab','Cd','Ef'), val=colMeans(Pearl3T_beh43$Learn[,7:9]))
sdlearn=apply(Pearl3T_beh43$Learn[,7:9],2,sd)/sqrt(length(Pearl3T_beh43$Learn[,1])) 

# Q.diff all
Stan.Q=Pearl3T_beh43$Qpar[,c("QL-a","QL-c","QL-e")]-Pearl3T_beh43$Qpar[,c("QL-b","QL-d","QL-f")]
Qdiff=data.frame(name=c('Ab','Cd','Ef'), val=colMeans(Stan.Q))
Qdiffsd=apply(Stan.Q,2,sd)/sqrt(length(Stan.Q[,1])) 

#2) now oragnize simulated and observed data

SIM_plot=function(obs.vs.sim)
{
  Sim.all=data.frame(Bin=c(1:NBINS),simulated=obs.vs.sim$Meancompare[({NBINS*3}+1):length(obs.vs.sim$Meancompare)],
                     observed=obs.vs.sim$Meancompare[1:{NBINS*3}],
                     pair=toupper(substring(names(obs.vs.sim$Meancompare[1:{NBINS*3}]), 2,3)))
  sim.all=melt(Sim.all,id=c('Bin','pair')); colnames(sim.all)=c('Bin','pair','cond','p_correct')
  
  
  se.sim.all=data.frame(Bin=c(1:NBINS),simulated=obs.vs.sim$SEcompare[({NBINS*3}+1):length(obs.vs.sim$SEcompare)],
                        observed=obs.vs.sim$SEcompare[1:{NBINS*3}],
                        pair=toupper(substring(names(obs.vs.sim$SEcompare[1:{NBINS*3}]),2,3)))
  
  simse.all=melt(se.sim.all,id=c('Bin','pair')); colnames(simse.all)=c('Bin','pair','cond','SEM')
  sim.data=data.frame(sim.all,SEM=simse.all$SEM)
  print(sim.data)
  
}

sim.data<-SIM_plot(obs.vs.sim)

#-----------------                make plots           ------------------------------------------#

# plot end-belief Q.values and qdiff
COL=c('red','darkgreen','blue')

# Q.diff
pl1 <- ggplot(Qdiff, aes(x = factor(name), y = val)) + 
  geom_bar(stat = "identity", position="dodge", fill= COL,width=0.5) +
  geom_errorbar(aes(ymin=val-Qdiffsd, ymax=val+Qdiffsd), width=.1, color= COL)+
  coord_cartesian(ylim=c(0, 0.4))+ 
  xlab('Pair') +   
  ylab('difference in Q value') +theme_classic()


# plot distrubutions Q-learning parameters
l=qplot(posterior, data=Gposteriors, geom="density", fill=par, alpha=I(.6),
        xlab="parameters",
        ylab="posterior density [a.u.]",xlim=c(0,0.25),ylim=c(0,200))
l <- l+ scale_fill_manual( values = c('blue','darkgreen','red'))
l<- l+ theme_classic()
l<- l+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = c(0.8,.8)
             ,legend.text=element_text(size=rel(0.7)),legend.title=element_text(size=rel(0.7)) )
#l

#####STOPPED HERE########

# simulate all
# red = observed
#head(sim.data)

sim.ab <- ggplot(sim.data[sim.data['pair']=='AB',], aes(x = Bin, y = p_correct, color = cond))
sim.ab <- sim.ab + geom_line() + ylab("P(A|AB)") + xlab("Bins")
sim.ab <- sim.ab + geom_point() 
sim.ab <- sim.ab + xlim(0.95,6) + ylim(0.4,1)
sim.ab <- sim.ab + geom_errorbar(data=sim.data[sim.data['pair']=='AB',], aes(x= Bin,ymin = p_correct-SEM, ymax = p_correct+SEM),width=0.1)
sim.ab <- sim.ab + scale_color_manual(values=c('black','red'))
sim.ab <- sim.ab + scale_x_continuous(breaks=c(1:6),labels=c(1:6)) 
sim.ab<- sim.ab+ theme_classic()
sim.ab<- sim.ab+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = c(0.2,0.1)
                       ,legend.text=element_text(size=rel(0.7)),legend.title=element_text(size=rel(0.7)))

sim.cd <- ggplot(sim.data[sim.data['pair']=='CD',], aes(x = Bin, y = p_correct, color = cond))
sim.cd <- sim.cd + geom_line() + ylab("P(C|CD)") + xlab("Bins")
sim.cd <- sim.cd + geom_point() 
sim.cd <- sim.cd + xlim(0.95,6) + ylim(0.4,1)
sim.cd <- sim.cd + geom_errorbar(data=sim.data[sim.data['pair']=='CD',], aes(x= Bin,ymin = p_correct-SEM, ymax = p_correct+SEM),width=0.1)
sim.cd <- sim.cd + scale_color_manual(values=c('black','red'))
sim.cd <- sim.cd + scale_x_continuous(breaks=c(1:6),labels=c(1:6)) 
sim.cd<- sim.cd+ theme_classic()
sim.cd<- sim.cd+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = "none")

sim.ef <- ggplot(sim.data[sim.data['pair']=='EF',], aes(x = Bin, y = p_correct, color = cond))
sim.ef <- sim.ef + geom_line() + ylab("P(E|EF)") + xlab("Bins")
sim.ef <- sim.ef + geom_point() 
sim.ef <- sim.ef + xlim(0.95,6) + ylim(0.4,1)
sim.ef <- sim.ef + geom_errorbar(data=sim.data[sim.data['pair']=='EF',], aes(x= Bin,ymin = p_correct-SEM, ymax = p_correct+SEM),width=0.1)
sim.ef <- sim.ef + scale_color_manual(values=c('black','red'))
sim.ef <- sim.ef + scale_x_continuous(breaks=c(1:6),labels=c(1:6)) 
sim.ef<- sim.ef+ theme_classic()
sim.ef<- sim.ef+ theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = "none")



#require(gridExtra)
#dev.new()
grid.arrange(pl1,l,sim.ab, sim.cd,sim.ef,nrow=2,layout_matrix=rbind(c(1,1,1,2,2,2),c(3,3,4,4,5,5)))


#grid.arrange(l,sim.ab, sim.cd,sim.ef,nrow=2)

#dev.new()
#grid.arrange(l,pl2,pl3,nrow=3)







