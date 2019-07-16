
#----------------------------------- load libs ---------------------------------------#
library(pacman)

packages=c('rpart','rpart.plot','randomForest','RColorBrewer','AUC','doParallel','dplyr',
           'ggplot2','lattice','ggthemes','grid','gridExtra','plotrix','ggpubr','corrplot',
           'psych','rquery',"Hmisc")
p_load(packages,character.only = TRUE)


#----------------------------------- load data -------------------------------------#
setwd("/Users/sarajahfari/Documents/Github/Pearl3T")
set.seed(218)
load('./Data/RF_data/forestDat_N43.Rdat')

#---------------------------------------------------------------------#
#     Define base regions included in RF and set to good or all       #
#---------------------------------------------------------------------#

CBase=c(
  "Caudate40exc","Putamen40exc","Accumbens", 
  "FFA23","V1","VMPFC31thr","MotorBA4",
  "DLPFCposterior","preSMAsmall",'Q.diff',"subject_id")


Creg=c(
  "Caudate40exc","Putamen40exc","Accumbens", 
  "FFA23","V1","VMPFC31thr","MotorBA4",
  "DLPFCposterior","preSMAsmall",'Q.diff')

CMreg=c(
  "Caudate40exc","Putamen40exc","Accumbens", 
  "FFA23","V1","VMPFC31thr","MotorBA4",
  "DLPFCposterior","preSMAsmall")

forestinall=forestinallR2R

Type='good'

forestinall$conflict=droplevels(forestinall)$conflict
forestinall$option=droplevels(forestinall)$option
forestinall$response=ifelse(forestinall$response==0,'incorret','correct')

if (Type=='ALL') {Forestinall=forestinall[,CBase]} else
  if (Type=='good') {Forestinall=forestinall[forestinall$Learner=='good',CBase]} else
    if (Type=='bad') {Forestinall=forestinall[forestinall$Learner=='bad',CBase]}

#------------------ now look at the correlations ------------------------------#

M=cor(Forestinall[,Creg])
ord <- corrMatOrder(M, order = "hclust")
M2 <- M[ord,ord]

# corrplot(M2,type='upper',order='hclust',col=c("black", "white"),
#          bg="lightblue")
# corrplot.mixed(M2, lower = "ellipse", upper = "circle")
# corrplot.mixed(M2, lower = "square", upper = "circle")
# corrplot.mixed(M2, lower = "shade", upper = "circle")
# corrplot.mixed(M2, tl.pos = "lt")
# corrplot.mixed(M2, tl.pos = "lt", diag = "u")
# corrplot.mixed(M2, tl.pos = "lt", diag = "l")
# corrplot.mixed(M2, tl.pos = "n")


# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# matrix of the p-value of the correlation
p.mat <- cor.mtest(Forestinall[,Creg])
# 
# corrplot(M, type="upper", order="hclust", 
#          p.mat = p.mat, sig.level = 0.01, insig = "blank")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# col <- colorRamp(c("red", "green"))( (0:4) )
# 
# rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
#                                 space = "rgb")

dev.new()
corrplot(M, method="ellipse", col=col(10),  bg='lightgrey',
         type="lower", 
         addCoef.col = "black", number.cex=0.7,# Add coefficient of correlation
         tl.col="black", 
         tl.srt=45,
         number.digits=2,
         order = "hclust", #Text label color and rotation
         # Combine with significance
         p.mat = round(p.mat,digits=2), sig.level = 0.01, insig = "n", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         title='Good Participants'
)






dev.new()
pairs(Forestinall, pch = 19)

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
# Create the plots
pairs(Forestinall, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

dev.new()
pairs.panels(Forestinall[,CMreg], 
             method = "pearson", # correlation method
             hist.col = "blue",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipse
             lm=TRUE,
             stars=TRUE,
             scale=F,
             smooth=TRUE,
             ci=T,
             jiggle=T
)




Forestinall$subject_id=droplevels(Forestinall)$subject_id
cor_list=list()
p_list=list()
  
for (SJ in levels(Forestinall$subject_id)){
  cor=rcorr(as.matrix(Forestinall[Forestinall$subject_id==SJ,Creg]))
  cor_list[[SJ]]=cor$r
  p_list[[SJ]]=round(cor$P,digits=2)
}

names(cor_list)=levels(Forestinall$subject_id)
names(p_list)=levels(Forestinall$subject_id)


acor_list <- array( unlist(cor_list) , c(dim(cor_list[[1]]),length(cor_list)))
pcor_list <- array( unlist(p_list) , c(dim(p_list[[1]]),length(p_list)))

#  Get mean of third dimension
apply( acor_list , 1:2 , mean ) -> sumacor
apply( pcor_list , 1:2 , mean )
  
colnames(sumacor)=colnames(cor_list[[1]])
rownames(sumacor)=colnames(sumacor)
