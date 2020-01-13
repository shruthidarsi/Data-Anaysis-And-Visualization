library(ggplot2)
library(corrgram)
library(car)
#part 1
heartData <- read.csv("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./heart.csv", stringsAsFactors = T)
#pqrt 2
#(A)
heartData$CHESTPAIN = factor(heartData$CHESTPAIN)
heartData$THAL = factor(heartData$THAL)
heartData$ECG = factor(heartData$ECG)
heartData$EXERCISE = factor(heartData$EXERCISE)
heartData$OUTPUT = factor(heartData$OUTPUT)
summary(heartData$CHESTPAIN)
#(B)
colnames(heartData)
y <- factor(heartData$OUTPUT)
log.regression <- ifelse( na.omit(heartData$CHESTPAIN) + na.omit(heartData$THAL) + na.omit(heartData$ECG) + na.omit(heartData$EXERCISE) > 1, 1, 0 )
summary(log.regression)
glm.fit <- glm(y ~ heartData$CHESTPAIN + heartData$THAL + heartData$ECG + heartData$EXERCISE, data = heartData, family=binomial(link="logit"))


#Part 3 - Scatter plot
ggplot(heartData) +
  geom_point(aes(heartData$AGE,  heartData$CHOL)) +
  theme_minimal()

#(B) Scatter plot
ggplot(heartData) +
  geom_point(aes(heartData$AGE,heartData$RESTBP)) +
  theme_minimal()
  
# (C) 
ggplot(heartData) +
  geom_point(aes(heartData$AGE,heartData$MAXHR)) +
  theme_minimal()

# (D)
ggplot( heartData ) +
  geom_histogram(aes(x=heartData$AGE) ) +
  theme_minimal() 

mean(heartData$AGE)
median(heartData$AGE)

# Part 4
nrows <- nrow(heartData)
alpha <- 0.8
trainIndex <- sample(1:nrows, alpha*nrows)
trainDataset <- heartData[trainIndex,]
testDataset <- heartData[-trainIndex,]

#Part 5
y <- "OUTPUT"
x <- c("CHESTPAIN",
       "THAL",
       "ECG",
       "EXERCISE")

colnames(heartData)
#y <- factor(heartData$OUTPUT)
#log.regression <- ifelse( na.omit(heartData$CHESTPAIN) + na.omit(heartData$THAL) + na.omit(heartData$ECG) + na.omit(heartData$EXERCISE) > 1, 1, 0 )
#summary(log.regression)
#glm.fit <- glm(y ~ heartData$CHESTPAIN + heartData$THAL + heartData$ECG + heartData$EXERCISE, data = heartData, family=binomial(link="logit"))
#heartData$OUTPUT <- ifelse(y ==2, 1, 0)

fmla <- paste(y, paste(x, collapse="+"), sep="~")
print(fmla)
glm.fit <- glm(fmla, data = trainDataset, family=binomial(link="logit"))

summary(glm.fit)

trainDataset$pred <- predict(glm.fit, newdata=trainDataset, type="response")
testDataset$pred <- predict(glm.fit, newdata=testDataset, type="response")

#Confusion matrix
heartdata.matrix <- table(pred=testDataset$pred>0.5, OUTPUT = testDataset$OUTPUT)
print(heartdata.matrix)
precision <- heartdata.matrix[2,2]/sum(heartdata.matrix[2,])
print(precision)
recall <- heartdata.matrix[2,2]/sum(heartdata.matrix[,2])
print(recall)
enrich <- precision/mean(as.numeric(testDataset$OUTPUT))
enrich

library('ROCR')
eval <- prediction(testDataset$pred , testDataset$OUTPUT)
plot(performance(eval, "tpr", "fpr"))
print(attributes(performance(eval, 'auc'))$y.values[[1]])


heartData$AgeRange<-cut(heartData$AGE, seq(10,80,10))
print(heartData$AgeRange)

ggplot(heartData) + geom_bar(aes(x=heartData$OUTPUT,fill=heartData$AgeRange),position="dodge")

colnames(heartData)
AGE<-c(69,46)
SEX<-c(1,0)
CHESTPAIN <-as.factor(c(3,4))
RESTBP <- c(150,95)
CHOL<- c(182,230)
SUGAR <- c(1,0)
ECG <- as.factor(c(1,2))       
MAXHR <- c(805, 360)
ANGINA <- c(1,0)
DEP <- c(4.5,3.6)     
EXERCISE <- as.factor(c(1,2))
FLOUR <- c(2,3)     
THAL <- as.factor(c(3,7))

newDataFrame<-data.frame(AGE, SEX, as.factor(CHESTPAIN), RESTBP, CHOL, SUGAR, as.factor(ECG), MAXHR, ANGINA, DEP, as.factor(EXERCISE),FLOUR, as.factor(THAL))

predictResultsForNewData <- predict.glm(glm.fit, newdata=newDataFrame, type="response")
predictResultsForNewData


