library(ggplot2)
library(plyr)
library(dplyr)

march1=read.csv("madness_past1.csv")

train1<-march1[0:289,]
test1=march1[290:413,]

#1. bagging-drawing bootstrap samples for bagging

#a. function for calculating the log likelihood (for deviance)
loglikelihood <- function(y, py) {
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1, 1-1e-12, py))
  sum(y * log(pysmooth) + (1-y)*log(1 - pysmooth))
}

#b. accuracy measures (F1, Recall, and Precision)
accuracyMeasures<- function(pred, truth, name="model") {
  dev.norm <- -2*loglikelihood(as.numeric(truth), pred)/length(pred)
  ctable <- table(truth=truth,
                  pred=(pred>0.5)) #threshold 0.5 
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- precision*recall
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}

#subset data 
sub1=march1[,11:54]

train1=train1[,11:54]
train1$W.L._u=as.numeric(train1$W.L._u)
test1=test1[,11:54]
test1$W.L._u=as.numeric(test1$W.L._u) 
table(train1$win_home)
table(test1$win_home)


#a. Decision tree predictor ------------------
winVars <- setdiff(colnames(num_vars),list('rgroup','win_home'))
winFormula <- as.formula(paste('win_home=="1"',
                               paste(winVars,collapse=' + '),sep=' ~ '))

treemodel<-rpart(win_home~.,data=train1)

#F1 (combines precision and recall) for accuracy measures 
accuracyMeasures(predict(treemodel,newdata=train1),
                 train1$win_home==1,
                 name="tree,training")

accuracyMeasures(predict(treemodel,newdata=test1),
                 test1$win_home==1,
                 name="tree,test")

#bagging decision trees -----------------------
ntrain<-dim(train1)[1]
n<-ntrain 
ntree<-100 #number of trees 

#sapply a function over a list 
samples<-sapply(1:ntree,FUN=function(iter)
{sample(1:ntrain,size=n,replace=T)}
)

treelist<-lapply(1:ntree,
                 FUN=function(iter)
                 {samp<-samples[,iter];
                 rpart(winFormula,train1[samp,])})

treelist1<-lapply(1:ntree,
                  FUN=function(iter)
                  {samp<-samples[,iter];
                  rpart(winFormula,test1[samp,])})

opt<-which.min(treemodel$cptable[,"xerror"])
cp<-treemodel$ctable[opt,"CP"]
prune1<-prune(treemodel,cp=cp)


predict.bag<-function(treelist,newdata){
  preds<-sapply(1:length(treelist),
                FUN=function(iter) {
                  predict(treelist[[iter]],newdata=newdata)})
  
  predsums<-rowSums(preds)
  predsums/length(treelist)
}

#training set accuracy
accuracyMeasures(predict.bag(treelist,newdata=train1),
                 train1$win_home==1,
                 name="bagging,training")

#testing set accuracy
accuracyMeasures(predict.bag(treelist1,newdata=test1),
                 test1$win_home==1,
                 name="bagging,test")


#model predictions 
preds<-predict(treemodel,test1) 
preds<-as.data.frame(preds)
teams<-march1[290:413,]
teams<-subset(teams,select=c("Year.1","team_fav","team_under","win_home"))
teams<-cbind(teams,preds)



#II. Using decision trees--------------
tmodel<-rpart(win_home~.,data=train1,
              control=rpart.control(cp=0.001,minsplit=1000,
                                    minbucket = 1000,maxdepth=5))


#area under curve (AUC) function
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}

#under the area curve (0.5 like guessing, 1.0 is perfect predictor)
library("ROCR")
test2=test1[,2:43]
pred<-prediction(tmodel,test2)


#Nearest neighbors---------
library('class')
nK<-150 
knn.train<-train1
knnCl<-train1[,"win_home"]==1

knnPred <- function(df) {
  knnDecision <- knn(knn.train,df,knnCl,k=nK,prob=T)
  ifelse(knnDecision==TRUE,
         attributes(knnDecision)$prob,
         1-(attributes(knnDecision)$prob))
}


##iv. random forest------
set.seed(424)

fmodel<-randomForest(as.factor(win_home)~.,data=train1,importance=TRUE,ntree=100) 
pred_rf<-predict(fmodel,data=test[,2:43],type="prob")

#model quality (cocao playa??) error aqui?? 
accuracyMeasures(predict(fmodel,newdata=train1,type="prob")[train1$win_home],
                 train1$win_home==1,name="random forest,train")

#var importancce
varImp<-importance(fmodel)
varImp[1:10,]
varImpPlot(fmodel,type=1)

#fitting with fewer variables
selVars<-names(sort(varImp[,1],decreasing=T))[1:25]
fsel<-randomForest(x=train1[,selVars],y=as.factor(train1$win_home),ntree=100,nodesize = 7,
                   importance=T) #74.4% accuracy 


#i. pooled diff (high vs. low seeds?)--difference in shooting approaches?
#do higher seeded teams that shoot above average 3pa win more?
mean_3pa=mean(march1$X3PA.G_f)

high1=subset(march1,X3PA.G_f>=mean_3pa)
win_high=mean(high1$win_home)
low1=subset(march1,X3PA.G_f<mean_3pa)
win_low=mean(low1$win_home)

diff=win_high-win_low #6.7% diff 

#proportions
p=((191*win_high)+(222*win_low))/(191+222)
se_pool_diff=sqrt((p*(1-p))*((1/191)+(1/222)))
z=diff/se_pool_diff #1.36 (0.9131) the two proportions don't differ sign...

#ii. hypothesis testing (do underdogs that attempt more threes per game win?)
mean_3paU=mean(march1$X3PA.G_u) #17.0 

high1U=subset(march1,X3PA.G_u>=mean_3paU)
win_highU=mean(high1U$win_home)
win_highU_sd=sd(high1U$win_home)
low1U=subset(march1,X3PA.G_u<mean_3paU)
win_lowU=mean(low1U$win_home)
win_lowU_sd=sd(low1U$win_home)

#mean diff between win percentages?
mean_win_diff=win_highU-win_lowU

#proportions 
p=((269*win_highU)+(144*win_lowU))/(269+144)
se_pool_diff=sqrt((p*(1-p))*((1/269)+(1/144)))
z=diff/se_pool_diff

###paried observations---------
mean(march1$X3PA.G_u)
count_wins_und=subset(march1,X3PA.G_u<=17.0)
mean(count_wins_und$win_home) #52.4% >17.0 3pa, 34% <17.0 3pa

march1$Year.1=as.factor(march1$Year.1)
#three point attempts per year (high seed)
x3pa_fav=ddply(march1, .(Year.1), summarize,  x3pa=mean(X3PA.G_f))
x3pa_und=ddply(march1, .(Year.1), summarize,  x3pa=mean(X3PA.G_u))

x3pa=cbind(x3pa_fav,x3pa_und) #bartend madrid? 
colnames(x3pa)[3] <- "year"
colnames(x3pa)[4]<-"x3pa1"
xpa=subset(x3pa,select=c("year","x3pa","x3pa1"))

#paired t-test 3PA (high vs low seed?) 
diff=march1$X3PA.G_f-march1$X3PA.G_u
mean_diff=mean(diff)
sd_diff=sd(diff)
se_diff=sd_diff/sqrt(dim(march1)[1])

test_stat=(mean_diff/se_diff) #t=-0.30
hist(diff)

#100(1-alpha)% confidencce interval? 0 to 0.12 
low_ci=mean_diff-(test_stat*se_diff)
high_ci=mean_diff+(test_stat*se_diff)

#paired samples
sd_und_3pa=sd(march1$X3PA.G_u)
mean_und_3pa=mean(march1$X3PA.G_u)
se_und=sd_und_3pa/sqrt(dim(march1)[1])
sd_fav_3pa=sd(march1$X3PA.G_f)
mean_fav_3pa=mean(march1$X3PA.G_f)
se_fav=sd_fav_3pa/sqrt(dim(march1)[1])


