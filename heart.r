library(dplyr)
library(irr)
library(rpart)
library(caret)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)
donees<-read.table(file="E:/R/PROJECTPPT/knn and dectree/heart.txt",dec=" ",header=TRUE)
summary(donees)

arbre.full<-rpart(coeur~ .,data=donees,method="class")
print(arbre.full)

library(rpart)
library(rpart.plot)
plot(arbre.full,margin=0.1,main="Occurece of heart diseases")
text(arbre.full, use.n=TRUE, all=TRUE, cex=.7)
library(rattle)
fancyRpartPlot(arbre.full)
printcp(arbre.full)
plotcp(arbre.full, minline = TRUE)
arbre.full1<-prune(arbre.full,cp= 0.047)
fancyRpartPlot(arbre.full1)
arbre.full1

donees%>%mutate(Target=ifelse(coeur=='presence',1,0))->donees
donees%>%select(-coeur)->donees
#Confusion Matrix
actual<-donees$Target
predicted<-predict(arbre.full1,type = "class")

head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)
confusionMatrix(predicted,actual,positive="1")

#kappa metric
kappa2(data.frame(actual,predicted))

#ROC curve analysis
library(ROCR)
pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)
#knn
r1<-read.table(file="E:/R/PROJECTPPT/knn and dectree/heart.txt",dec=" ",header=TRUE)
r1$sexe<-NULL
r1$type_douleur<-NULL
r1$sucre<-NULL
r1$angine<-NULL
r1$vaisseau<-NULL
r1$electro<-NULL
row_index=sample(1:nrow(r1),0.8*nrow(r1))
decimal<-as.data.frame( decscale(r1[1:6] ))
train_data_i=decimal[row_index,]
test_data_i=decimal[-row_index,]
pred_model=knn(train_data_i,test_data_i,r1[row_index,7],k=5)
pred_model
predicted_table=table(pred_model,r1[-row_index,7])
predicted_table
a=0.2*nrow(r1)
diag_element=diag(predicted_table)
diag_element
accuracy_r1=(sum(diag_element)/a)*100
accuracy_r1
r1NormZ <- as.data.frame( scale(r1[1:6] ))
r1NormZ

#class(r1NormZ)
row_index=sample(1:nrow(r1),0.8*nrow(r1))
train_data_i=r1NormZ[row_index,]
test_data_i=r1NormZ[-row_index,]
pred_model=knn(train_data_i,test_data_i,r1[row_index,7],k=3)
pred_model
predicted_table=table(pred_model,r1[-row_index,7])
predicted_table
a=0.2*nrow(r1)
diag_element=diag(predicted_table)
diag_element
accuracy_r1=(sum(diag_element)/a)*100
accuracy_r1

summary(r1)
View(r1)
row_index=sample(1:nrow(r1),0.8*nrow(r1))
d=function(x){((x-min(x))/(max(x)-min(x)))}
#install.packages("class")
r1$sexe<-NULL
r1$type_douleur<-NULL
r1$sucre<-NULL
r1$angine<-NULL
r1$vaisseau<-NULL
r1$electro<-NULL
library("class")
View(r1)

norm_data=lapply(r1[,-7],d)
class(norm_data)
norm_data=as.data.frame(norm_data)
train_data_i=norm_data[row_index,]
test_data_i=norm_data[-row_index,]
pred_model=knn(train_data_i,test_data_i,r1[row_index,7],k=5)
pred_model
predicted_table=table(pred_model,r1[-row_index,7])
predicted_table
a=0.2*nrow(r1)
diag_element=diag(predicted_table)
diag_element
accuracy_r1=(sum(diag_element)/a)*100
accuracy_r1



