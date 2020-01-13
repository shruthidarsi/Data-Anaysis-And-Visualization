#Load data
banknoteData <- read.csv("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./data_banknote_authentication.csv")
banknoteData$Class <- factor(banknoteData$Class,levels=c(0,1),labels=c("Genuine", "Forged"))

set.seed (99999)

train <- sample(nrow(banknoteData), 0.8*nrow(banknoteData))
bankNoteDataTrain <- banknoteData[train,]
bankNoteDataTest <- banknoteData[-train,]

library(randomForest)
myFormula <- as.formula("Class ~ .")
rForest <-randomForest(formula=myFormula, mtry=3,ntree=400, data=bankNoteDataTrain, importance=TRUE)
print(rForest)

rForestPrediction <- predict(rForest,newdata=bankNoteDataTest)
forestPredTable <- table(bankNoteDataTest$Class,   rForestPrediction, dnn=c("Actual", "Predicted"))
print(forestPredTable)

importance(rForest, type=1)

varImpPlot(rForest, type=1)  

pp1 <- predict(rForest,data.frame(WaveletVar=1.69, WaveletSkew=4.68, WaveletCurt=-3.8073,
                                     Entropy=-0.18))

pp2 <- predict(rForest,data.frame(WaveletVar=5.5459, WaveletSkew=4.1674, WaveletCurt=-2.4586,
                                     Entropy=-0.4628))

pp3 <- predict(rForest,data.frame(WaveletVar=7.866, WaveletSkew=-4.6383, WaveletCurt=1.9242,
                                     Entropy=1.10648))


#Bagging
library(ipred)
baggingModel <-bagging(banknoteData$Class ~ .,data=banknoteData, coob=TRUE)
print(baggingModel)

rBagPred <- predict(baggingModel,newdata=bankNoteDataTest)
bagPredTable <- table(bankNoteDataTest$Class,   rBagPred, dnn=c("Actual", "Predicted"))
print(bagPredTable)

accuracy<-(forestPredTable[1,1]+forestPredTable[2,2])/sum(forestPredTable)

pp1 <- predict(baggingModel,data.frame(WaveletVar=1.69, WaveletSkew=4.68, WaveletCurt=-3.8073,
                                  Entropy=-0.18))

pp2 <- predict(baggingModel,data.frame(WaveletVar=5.5459, WaveletSkew=4.1674, WaveletCurt=-2.4586,
                                  Entropy=-0.4628))

pp3 <- predict(baggingModel,data.frame(WaveletVar=7.866, WaveletSkew=-4.6383, WaveletCurt=1.9242,
                                  Entropy=1.10648))
