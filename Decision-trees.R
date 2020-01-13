#Load data
banknoteData <- read.csv("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./data_banknote_authentication.csv")
set.seed (10032)
banknoteData$Class <- factor(banknoteData$Class,levels=c(0,1),labels=c("Genuine", "Forged"))

train <- sample(nrow(banknoteData), 0.8*nrow(banknoteData))
bankNoteDataTrain <- banknoteData[train,]
bankNoteDataTest <- banknoteData[-train,]

library(rpart)
bankNoteDataTree <- rpart(Class~., method="class", data=bankNoteDataTrain, control=rpart.control(minsplit=4,cp=0.000001))

print(bankNoteDataTree)
bankNoteDataTree$cptable

par(cex=0.5)
plot(bankNoteDataTree, uniform=T)
text(bankNoteDataTree)

library(rpart.plot)
prp(bankNoteDataTree, type = 2, fallen.leaves = TRUE, main="Bank Note Data Tree")

plotcp(bankNoteDataTree)

printcp(bankNoteDataTree)

treePruned <- prune(bankNoteDataTree, cp=0.009)
prp(treePruned, type = 2, fallen.leaves = TRUE, main="Bank Note Pruned Tree")

bankNotePrediction <- predict(treePruned, bankNoteDataTest, type="class")
table(bankNoteDataTest$Class, bankNotePrediction, dnn=c("Actual", "Predicted"))

pp1 <- predict(treePruned,data.frame(WaveletVar=1.69, WaveletSkew=4.68, WaveletCurt=-3.8073,
                                           Entropy=-0.18))

pp2 <- predict(treePruned,data.frame(WaveletVar=5.5459, WaveletSkew=4.1674, WaveletCurt=-2.4586,
                                     Entropy=-0.4628))

pp3 <- predict(treePruned,data.frame(WaveletVar=7.866, WaveletSkew=-4.6383, WaveletCurt=1.9242,
                                     Entropy=1.10648))

