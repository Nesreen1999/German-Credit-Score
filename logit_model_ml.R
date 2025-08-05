#Utilisation d'une base de données sur un échantillon d'assurés

#library(Deducer)
library(caret)
library(ROCR)
library(pROC)
library(readxl)
library(aod)
couverture <- read_excel("économétrie/LogitModel/couverture d'assurance.xlsx")
View(couverture)

#Variables
#AGE: integer age of policyholder
#SENIORITY: number of years at company
#MEN: 1 = male, 0 = female
#URBAN: 1= urban environment, 0 = rural environment
#PRIVATE: 1 = private use, 0 = commercial use
#MARTIAL: "c" = married couple, "s" = single, "o" = other
#Y: dependent, or response variable, 1 = full coverage, 0 = not full coverage
#Y2: 1=full, 2=Not full
couverture$y2<-ifelse(couverture$y==0,2,1)
couverture$y2<-as.factor(couverture$y2)
couverture$y1=couverture$y

couverture$men <- factor(couverture$men)
couverture$urban <- factor(couverture$urban)
couverture$private <- factor(couverture$private)
couverture$y <- factor(couverture$y)
couverture$marital <- factor(couverture$marital)
attach(couverture)
summary(couverture)
plot(density(age))
plot(density(seniority))
plot <- ggplot(data=couverture, aes(x=age, y=seniority, col=y))+
geom_point(aes(size = 1))+
xlab("Age") + ylab("Seniority") +
  scale_color_discrete(name = "Gender")
plot



#Modèle logit, ROC, AUC base globale
logit_g <- step(glm(y~men+urban+private+marital+age+seniority,data=couverture,family=binomial))
summary(logit_g)
summary(logit_g)$coefficient

library(aod)
## Intervalle de confiance des paramètres
ci=confint(logit_g)
#Testde significativité globale
wald.test(b = coef(logit_g), Sigma = vcov(logit_g), Terms = 2:6)
#ODD Ratio
or=exp(coef(logit_g))

## odds ratios et intervalle de confiance des OR CI
exp(cbind(or,ci))


#library Deducer
roc_ful_g<-rocplot(logit_g)
roc_ful_g


#library pROC
prob=predict(logit_g,couverture,type = "response") #Calcul des probabilité
couverture$prob<-prob
roc_g<-roc(couverture$y, couverture$prob,print.auc=TRUE,ci=TRUE,boot.n=1000,smooth=FALSE)
plot(roc_g,grid=TRUE)
auc_proc_g=auc(roc_g) #Aire sous la courbe _ Area Under Curve
auc_proc_g
ci=ci(roc_g)
text(0.5,0.7,paste("AUC =",round(auc_proc_g,digits = 3)))
ci
#Library ROCR
prob_g=predict(logit_g,couverture,type = "response")
pred_g <- prediction(prob_g, couverture$y) 
perf_rocr_g <- performance(pred_g, measure = "tpr", x.measure = "fpr") #courbe roc
perf_lift1_g <- performance(pred_g, measure = "lift", x.measure = "rpp") #courbe de Lift
perf_lift2_g <- performance(pred_g, measure = "tpr", x.measure = "rpp") #courbe de Lift
perf_auc_g <- performance(pred_g, measure = "auc") #AUC
perf_mat_g <- performance(pred_g, measure = "mat") #Ceofficient de corrélation de Matthews

plot(perf_rocr_g,main="Courbe ROC",col="blue",xlab="Taux des faux positif",ylab="Taux des vraix positif")
abline(0,1,col="gray",lty=2)
text(0.5,0.7,paste("AUC = ",round(perf_auc_g@y.values[[1]],digits = 3)))

#Courbe de Lift
plot(perf_lift1_g,main="Courbe de Lift",xlab="RPP",ylab="Taux des Vraix Positifs")
plot(perf_lift2_g,main="Courbe de Lift",xlab="RPP",ylab="Taux des Vraix Positifs",col="red")
segments(0,0,1,1,col="blue")
plot(perf_mat_g,main="Coefficient de corrélation de Mathews",col="blue",cex.main=1.1,xlab="Seuil")
prediction<-as.numeric(pred_g@predictions[[1]]>=0.289)
confusionMatrix(couverture$y,prediction)

#Machine Learning

#Sélection d'un échantillon  correspondant à 75% de la base
train_id=sample(4000,3000)
data_train=couverture[train_id,]#Base Train
data_test=couverture[-train_id,]#Base test
#Estimation du modèle
logit <- step(glm(y~men+urban+private+factor(marital)+age+seniority,data=data_train,family=binomial))
summary(logit)
#roc plot using Deducer library
roc_ful<-rocplot(logit)
roc_ful
#Calcul des indicateurs pour la base train
prob_train=predict(logit,data_train,type = "response")
pred_train <- prediction(prob_train, data_train$y) 
perf_roc_train <- performance(pred_train, measure = "tpr", x.measure = "fpr")
perf_lift1_train <- performance(pred_train, measure = "lift", x.measure = "rpp") #courbe de Lift
perf_lift2_train <- performance(pred_train, measure = "tpr", x.measure = "rpp") #courbe de Lift
auc_train <- performance(pred_train, measure = "auc") 
#Calcul des indicateurs pour la base test 
auc_train
prob_test=predict(logit,data_test,type = "response")
pred_test <- prediction(prob_test, data_test$y)    
perf_roc_test <- performance(pred_test, measure = "tpr", x.measure = "fpr") 
perf_lift1_test <- performance(pred_test, measure = "lift", x.measure = "rpp") #courbe de Lift
perf_lift2_test <- performance(pred_test, measure = "tpr", x.measure = "rpp") #courbe de Lift
auc_test <- performance(pred_test, measure = "auc") 
#mettre les deux ROC
plot(perf_roc_train, col="blue", main="Courbe ROC", xlab="1-Spécificité (fpr)", ylab="Sensibilité (tpr)",
     bg="white",cex.main=2,cex.lab=1,print.cutoffs.at=seq(0,1,by=0.1),lwd=3) 
abline(0, 1,col="green",lty=3) #rajout d'une première bisectrice

#rajout de la courbe ROC pour la base test
lines(perf_roc_test@x.values[[1]],perf_roc_test@y.values[[1]],col="red",lwd=2) 
text(1,.05,labels=paste("__ train, AUC = ",round(auc_train@y.values[[1]],digits=3),sep=""),adj=1,col = "blue")
text(1,.15,labels=paste("__ test,  AUC = ",round(auc_test@y.values[[1]],digits=3),sep=""),adj=1,col = "red")


#mettre les deux de Lift ensemble
plot(perf_lift1_train, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilité (tpr)",
     bg="white",cex.main=1,cex.lab=1,lwd=3) 

lines(perf_lift1_test@x.values[[1]],perf_lift1_test@y.values[[1]],col="red",lwd=2) 
text(1,.25,labels="__ train",adj=1,col = "blue")
text(1,.15,labels="__ test",adj=1,col = "red")



#mettre les deux de Lift ensemble
plot(perf_lift2_train, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilité (tpr)",
     bg="white",cex.main=1,cex.lab=1,lwd=3) 

lines(perf_lift2_test@x.values[[1]],perf_lift2_test@y.values[[1]],col="red",lwd=2) 
text(1,.25,labels="__ train",adj=1,col = "blue")
text(1,.15,labels="__ test",adj=1,col = "red")
segments(0,0,1,1)

perf <- performance(pred_train, "mat")
plot(perf,main="Coeffcicient de corrélation de Matthews",xlab="Seuil",ylab="Corrélation")
abline(v=seuil)

#Calcul du AUC trapézoide
tpr<-perf_roc_test@y.values[[1]]
fpr<-perf_roc_test@x.values[[1]]

#identification du seuil optimal
obj<-tpr-fpr
m<-max(obj,na.rm = T)
seuil <- perf_roc_test@alpha.values[[1]][which.max(obj>=m)]
print(paste("Le seuil optimal est = ",round(seuil,digits = 3)))
corr<-perf@y.values[[1]]
m<-max(corr,na.rm = T)
alpha<-perf@x.values[[1]]
seuil2 <- alpha[which.max(corr>=m)]
print(paste("Le seuil optimal est = ",round(seuil2,digits = 3)))
n<-length(tpr)
auc_test2 <- round(sum(0.5 * (tpr[-1] + tpr[-n]) * (fpr[-1] - fpr[-n])),digits = 3)
auc_test2
roc<-data.frame(tpr,fpr)
tpr_train<-perf_roc_train@y.values[[1]]
View(roc)
library(ggplot2)
ggplot(data=NULL, aes(x=fpr))+
  labs(title="Courbe ROC",x="Taux des faux positifs (1-Spécificité)",y="Taux des vraix positifs (Sensibilité") +                    
  geom_line(aes(y=tpr), colour="red",size=1.1) + # Sans remise 
  geom_line(aes(y=fpr), colour="blue",size=1.1)+
  theme(plot.title = element_text(size = rel(2),colour = "blue",hjust = 0.5))+
  theme(panel.border = element_rect(linetype = NULL, fill = NA))+
  annotate("text", x = 0.5, y = 0.75, label = paste("AUC (base test) = ",round(auc_test@y.values[[1]],digits=3),sep=""), colour="blue",size=3)


library(pROC)
data_test$prob_test<-prob_test
roc1<-roc(data_test$y, data_test$prob_test,print.auc=TRUE,ci=TRUE,boot.n=1000,smooth=FALSE)
plot(roc1,grid=TRUE)
auc_proc=auc(roc1)
text(0,.15,labels=paste("AUC (Base Test) = ",round(auc_proc,digits=3),sep=""),adj=1,col = "red")
data_train$prob_train<-prob_train
roc2<-roc(data_train$y, data_train$prob_train,print.auc=TRUE,ci=TRUE,boot.n=1000,smooth=FALSE)
plot(roc2,add=TRUE,col="blue",grid=TRUE)
auc_proc2=auc(roc2)
text(0,.05,labels=paste("AUC (Base Train) = ",round(auc_proc2,digits=3),sep=""),adj=1,col = "blue")


#réseau de neurones
library(nnet)
nnet <- nnet(y~men+urban+private+factor(marital)+age+seniority,data=data_train,family=binomial,size=2)

nnet_train=predict(nnet,data_train,type = "raw")
data_train$nnet_pred<-nnet_train
pred_nn_train <- prediction(nnet_train, data_train$y) 
perf_roc_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "fpr")
perf_auc_nn_train <- performance(pred_nn_train, measure = "auc")
perf_lift2_nn_train <- performance(pred_nn_train, measure = "lift", x.measure = "rpp")
perf_lift1_nn_train <- performance(pred_nn_train, measure = "tpr", x.measure = "rpp")
#Courbe de ROC
plot(perf_roc_nn_train,main="Courbe ROC Réseau de neurônes",col="blue")
text(0.5,.7,paste("AUC - NN = ",round(perf_auc_nn_train@y.values[[1]],3)),col="blue",cex=0.75)
segments(0,0,1,1,lty=3,col="green")
#Courbe de Lift
plot(perf_lift2_nn_train,main="Courbe de Lift Réseau de neurônes")
plot(perf_lift1_nn_train,main="Courbe de Lift Réseau de neurônes",col="blue",grid=TRUE)
segments(0,0,1,1,lty=3)
abline(h=.69,col="red",lty=3)
text(0.15,0.75, "TPR = 0.69",col=250)
abline(v=0.4,col=redblue(20),lty=3)

cf_matrix_nnet<-confusionMatrix(data_train$y,data_train$nnet_pred)
cf_matrix_nnet

#LASSO Regression
library(glmnet)
xfactors <- model.matrix(data_train$y ~ data_train$men + data_train$urban + data_train$private + data_train$marital)[, -1]
x        <- as.matrix(data.frame(data_train$age, data_train$seniority, xfactors))
glmmod <- glmnet(x, y=as.factor(data_train$y), alpha=0, family="binomial",type.logistic= "Newton")
plot(glmmod, xvar="lambda")
set.seed(235)
lasso_cv <- cv.glmnet(x, y=as.factor(data_train$y), alpha=0, family="binomial",
                      type.measure = "auc", nlambda=50,nfolds = 500,type.logistic="Newton")
plot(lasso_cv, xvar="lambda")
lasso <- glmnet(x, y=as.factor(data_train$y), alpha=0, family="binomial",type.logistic= "Newton",
                lambda = seq(lasso_cv$lambda[1],lasso_cv$lambda[NROW(lasso_cv$lambda)],length=10000),standardize = T)
#type.measure = c("mse","auc","deviance","class","mae")
plot(lasso)

xfactors_test <- model.matrix(data_test$y ~ data_test$men + data_test$urban + data_test$private + data_test$marital)[, -1]
xtest        <- as.matrix(data.frame(data_test$age, data_test$seniority, xfactors_test))
ytest<-data_test[,"y"]

pred_<-predict(lasso,newx=xtest,type="response")
roc<-function(x){
  performance(prediction(pred_[,x],ytest),"auc")@y.values[[1]]
}
vauc<-Vectorize(roc)(1:length(lasso$lambda))
yhat_test<-prediction(pred_[,which.max(vauc)],ytest,label.ordering=c(0,1))
perf_roc_lasso_test<-performance(yhat_test,"tpr","fpr")

perf_auc_lasso_test<-performance(yhat_test,"auc")
perf_mat_lasso_test<-performance(yhat_test,"mat")

plot(perf_mat_lasso_test)
plot(perf_roc_lasso_test)
segments(0,0,1,1,lty=3,col="blue")
perf_lift1_lasso_test<-performance(yhat_test,"tpr","rpp")
plot(perf_lift1_lasso_test)
abline(v=0.4,lty=3,col="red")
abline(h=0.69,lty=3,col="red")

perf_lift2_lasso_test<-performance(yhat_test,"lift","rpp")
plot(perf_lift2_lasso_test)
abline(h=1.725,lty=3,col="red")
abline(v=0.4,lty=3,col="red")




