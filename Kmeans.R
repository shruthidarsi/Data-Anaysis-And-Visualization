#K-means
movieData <- read.csv("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./movie.csv")

movieDataWithoutNames <- movieData[2:564]

movieDataWithoutNamesMatrix <- scale(movieDataWithoutNames)

clusterkmeans <- kmeans(movieDataWithoutNamesMatrix,5)

movieData$fiveClusters <- clusterkmeans$cluster

#aggregate(data=movieData, movieData$movieName~movieData$fiveClusters , FUN=mean)

table(movieData$fiveClusters)

ggplot( movieData ) +
  geom_bar(aes(x = factor(movieData$fiveClusters))) + 
    theme_minimal()

show_data <- function(labels, kval)
{
  for (i in 1:kval)
  {
    print(paste("cluster : ", i))
    print(movieData$movieName[clusterkmeans$cluster == i])
    
  }
}

show_data(movieData, 5)
#aggregate(data=movieData, movieData$movieName~clusterkmeans$cluster,mean)

#Part 5 
library(fpc)
kmeansfit <- kmeansruns(movieDataWithoutNamesMatrix, krange=1:10, criterion="ch")

movieData$chClusters <- kmeansfit$cluster

table(movieData$chClusters)

ggplot( movieData ) +
  geom_bar(aes(x = factor(movieData$chClusters))) + 
  theme_minimal()


show_data <- function(labels, kval)
{
  for (i in 1:kval)
  {
    print(paste("cluster : ", i))
    print(movieData$movieName[kmeansfit$cluster == i])
    
  }
}

show_data(movieData, 5)



