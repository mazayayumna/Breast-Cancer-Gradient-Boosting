library(rpart) #classification and regression trees
library(partykit) #treeplots
library(MASS) #breast and pima indiandata
install.packages("randomForest")
library(randomForest) #random forests
install.packages("gbm")
library(gbm) #gradient boosting
install.packages("caret")
library(caret) #tune hyper-parameters

bcancer=read.table("D:\\..\\breastcancer.txt",header=T)
bcanc = sample(2, nrow(bcancer), replace=TRUE, prob=c(0.7,0.3))
trainData = bcancer[bcanc==1,]
testData = bcancer[bcanc==2,]
control = trainControl(method="CV", number=10)
set.seed(123)
gbm.biop.train= train(as.factor(diagnosis)~., data=trainData, method="gbm", trControl=control) #tuneGrid=grid
gbm.biop.train
trainData$diagnosis= ifelse(trainData$diagnosis=="B",0,1)
gbm.biop= gbm(diagnosis~., distribution="bernoulli", data=trainData, n.trees=150, interaction.depth=2, shrinkage=0.1)
gbm.biop.test= predict(gbm.biop, newdata=testData, type="response", n.trees=150)
gbm.class= ifelse(gbm.biop.test<0.5,"B", "M")
table(gbm.class, testData$diagnosis)
(96+62)/164
summary(gbm.biop)
(62/(96+62)) #TPR
(4/(2+4)) #FPR
library(pROC)
roc_df <- data.frame(
	TPR=(62/(96+62)), #TPR
	FPR=(4/(2+4))) #FPR
rectangle <- function(x, y, width, height, density=12, angle=-45, ...) 
  polygon(c(x,x,x+width,x+width), c(y,y+height,y+height,y), 
          density=density, angle=angle, ...)
roc_df <- transform(roc_df, 
  dFPR = c(diff(FPR), 0),
  dTPR = c(diff(TPR), 0))
plot(0:10/10, 0:10/10, type='n', xlab="FPR", ylab="TPR")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")
with(roc_df, {
  mapply(rectangle, x=FPR, y=0,   
         width=dFPR, height=TPR, col="green", lwd=2)
  mapply(rectangle, x=FPR, y=TPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  lines(FPR, TPR, type='b', lwd=3, col="red")
})
