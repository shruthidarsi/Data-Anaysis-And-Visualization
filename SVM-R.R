install.packages("kernlab")
install.packages("caret")
install.packages("e1071")
library(kernlab)
library(caret)
library(e1071)

letterData <- read.csv("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./letterdata.csv", stringsAsFactors = T, header = TRUE)

train <- sample(nrow(letterData), 0.8*nrow(letterData))
trainData <- letterData[train,]
testData <- letterData[-train,]

svmModel <- ksvm(letter~.,data=trainData, kernel='vanilladot')

testData$svmPred <- predict(svmModel,newdata=testData)

table(testData$letter, testData$svmPred, dnn=c("Actual", "Predicted"))

svmModel_rbfdot <- ksvm(letter~.,data=trainData, kernel='rbfdot')

testData$svmPred_rbfdot <- predict(svmModel_rbfdot,newdata=testData)

table(testData$letter, testData$svmPred_rbfdot, dnn=c("Actual", "Predicted"))


pp1 <- predict(svmModel_rbfdot , data.frame(xbox=9, ybox=11, width=0, height=5, onpix=11, xbar=8, ybar=11, x2bar=0, y2bar=4, xybar=5, x2ybar=5, 
                                            xy2bar=8, xedge=4, xedgey=8, yedge=0, yedgex=12 ))
pp2 <- predict(svmModel_rbfdot , data.frame(xbox=6, ybox=1, width=3, height=5, onpix=2, xbar=9, ybar=5, x2bar=9, y2bar=6, xybar=4, x2ybar=3, 
                                            xy2bar=7, xedge=2, xedgey=8, yedge=4, yedgex=9 ))
pp3 <- predict(svmModel_rbfdot , data.frame(xbox=4, ybox=1, width=6, height=9, onpix=6, xbar=5, ybar=6, x2bar=5, y2bar=4, xybar=5, x2ybar=3, 
                                            xy2bar=7, xedge=3, xedgey=9, yedge=3, yedgex=1 ))

pp4 <- predict(svmModel_rbfdot , data.frame(xbox=4, ybox=10, width=6, height=6, onpix=3, xbar=4, ybar=9, x2bar=4, y2bar=5, xybar=4, x2ybar=4, 
                                            xy2bar=10, xedge=6, xedgey=10, yedge=2, yedgex=11 ))


                                       
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
