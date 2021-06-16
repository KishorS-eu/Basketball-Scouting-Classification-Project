library(MASS)
library(tidyverse)
library(caret)
library(class)
library(e1071)

#Assigning our training and testing data-sets
NBALoadSet <- read.csv("~/Documents/term 3 coursework/Project Files/NCAA NBA 2006-2016 Draftee Data Post-PCA.csv")
NBABaseSet <- NBALoadSet[,c(2:8)]
TrainSet <- NBABaseSet[c(1:332),]
TestSet <- NBABaseSet[c(333:512),]

#LDA
model_lda <- lda(BUST~., data = TrainSet)
predict_lda <- model_lda %>% predict(TestSet)
mean(predict_lda$class==TestSet$BUST)

#QDA
model_qda <- qda(BUST~., data = TrainSet)
predict_qda <- model_qda %>% predict(TestSet)
mean(predict_qda$class==TestSet$BUST)

#k-NN
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

nn_13 <- knn(TrainSet,TestSet,cl=TrainSet$BUST,k=13)
tab_13 <- table(nn_13,TestSet$BUST)
accuracy(tab_13)

nn_14 <- knn(TrainSet,TestSet,cl=TrainSet$BUST,k=14)
tab_14 <- table(nn_14,TestSet$BUST)
accuracy(tab_14)

nn_20 <- knn(TrainSet,TestSet,cl=TrainSet$BUST,k=20)
tab_20 <- table(nn_20,TestSet$BUST)
accuracy(tab_20)

nn_30 <- knn(TrainSet,TestSet,cl=TrainSet$BUST,k=30)
tab_30 <- table(nn_30,TestSet$BUST)
accuracy(tab_30)

#SVM
SVM_linear = svm(TrainSet$BUST~.,data = TrainSet,type = 'C-classification',kernel = 'linear')
predict_SVMLinear <- predict(SVM_linear,TestSet)
mean(predict_SVMLinear==TestSet$BUST)

SVM_radial = svm(TrainSet$BUST~.,data = TrainSet,type = 'C-classification',kernel = 'radial')
predict_SVMRadial <- predict(SVM_radial,TestSet)
mean(predict_SVMRadial==TestSet$BUST)
