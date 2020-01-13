installed.packages("mgcv")
library(mgcv)

scienceData <- read.csv("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./science2006.csv", stringsAsFactors = T)
dim(scienceData)

scienceData <- na.omit(scienceData, cols = seq_along(Overall, Health, Edu, Income), invert=FALSE)

train <- sample(nrow(scienceData), 0.8*nrow(scienceData))
trainData <- scienceData[train,]
testData <- scienceData[-train,]

library(ggplot2)
ggplot(scienceData) +
  geom_point(aes(scienceData$Income, scienceData$Overall)) +
  theme_minimal()
ggplot(scienceData) +
  geom_point(aes(scienceData$Edu, scienceData$Overall)) +
  theme_minimal()
ggplot(scienceData) +
  geom_point(aes(scienceData$Health, scienceData$Overall)) +
  theme_minimal()


gamFormula <- as.formula("Overall ~ s(Income) + s(Edu) + s(Health)")
gamModel <- gam(gamFormula, data=trainData)

gamModel$converged

plot.gam(gamModel, pages=1, pch=19, cex=0.25,scheme=1, col='blue', shade=T,shade.col='gray90')

predictionResult <- predict(gamModel, newdata = testData)

sqrt(mean(testData$Overall-predictionResult)**2)

(summary(gamModel)$sigma)**2





