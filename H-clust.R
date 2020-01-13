library(data.table)
library(ggplot2)
# Part 1. Load Data 
kidneyData = data.table(read.csv(file="/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./kidneyDisease.csv", head=TRUE, sep=','))

#Part 2. Visualize data
# (A) Scatterplot - Age VS Blood Urea
ggplot(kidneyData,aes(x=kidneyData$age, y=kidneyData$bu)) + geom_point()

# (B) Boxer - Whisker plot of BP
ggplot(kidneyData) + 
  geom_boxplot(aes(x='', y=kidneyData$bp))

# (C) density plot of serum creatinine(sc)
ggplot(kidneyData) + 
  geom_density(aes(x=kidneyData$sc))

#Part-3 Convert categorical to Numerical
kidneyData$appet<-ifelse(kidneyData$appet=='good', 1, ifelse(kidneyData$appet=='poor',0,NA))
kidneyData$rbc<-ifelse(kidneyData$rbc=='normal', 1, ifelse(kidneyData$rbc=='abnormal',0,NA))
kidneyData$pc<-ifelse(kidneyData$pc=='normal', 1, ifelse(kidneyData$pc=='abnormal',0,NA))
kidneyData$pcc<-ifelse(kidneyData$pcc=='present', 1, ifelse(kidneyData$pcc=='notpresent',0,NA))
kidneyData$ba<-ifelse(kidneyData$ba=='present', 1, ifelse(kidneyData$ba=='notpresent',0,NA))
kidneyData$htn<-ifelse(kidneyData$htn=='yes', 1, ifelse(kidneyData$htn=='no',0,NA))
kidneyData$dm<-ifelse(kidneyData$dm=='yes', 1, ifelse(kidneyData$dm=='no',0,NA))
kidneyData$cad<-ifelse(kidneyData$cad=='yes', 1, ifelse(kidneyData$cad=='no',0,NA))
kidneyData$pe<-ifelse(kidneyData$pe=='yes', 1, ifelse(kidneyData$pe=='no',0,NA))
kidneyData$ane<-ifelse(kidneyData$ane=='yes', 1, ifelse(kidneyData$ane=='no',0,NA))
factor(kidneyData$appet)
#Part 4 - Replace all NAs by 0's
kidneyData$appet<-ifelse(is.na(kidneyData$appet), 0, kidneyData$appet)
kidneyData$rbc<-ifelse(is.na(kidneyData$rbc), 0, kidneyData$rbc)
kidneyData$pc<-ifelse(is.na(kidneyData$pc), 0, kidneyData$pc)
kidneyData$pcc<-ifelse(is.na(kidneyData$pcc), 0, kidneyData$pcc)
kidneyData$ba<-ifelse(is.na(kidneyData$ba), 0, kidneyData$ba)
kidneyData$htn<-ifelse(is.na(kidneyData$htn), 0, kidneyData$htn)
kidneyData$dm<-ifelse(is.na(kidneyData$dm), 0, kidneyData$dm)
kidneyData$cad<-ifelse(is.na(kidneyData$cad), 0, kidneyData$cad)
kidneyData$pe<-ifelse(is.na(kidneyData$pe), 0, kidneyData$pe)
kidneyData$ane<-ifelse(is.na(kidneyData$ane), 0, kidneyData$ane)
colnames(kidneyData)
#Part 5 - Scale data
kidneyDataMatrix <- scale(kidneyData[,1:24])

#Part 6 - distance matrix
kidneyDistMatrix <- dist(kidneyDataMatrix, method="euclidean")

#Part 7 - Hierarchical clustering
kidneyHclust <- hclust(kidneyDistMatrix, method = "ward.D2")
hclust(kidneyDistMatrix, method = "complete")
hclust(kidneyDistMatrix, method = "average")
hclust(kidneyDistMatrix, method = "single")
hclust(kidneyDistMatrix, method = "centroid")
hclust(kidneyDistMatrix, method = "ward.D")

#Part 8 - plot dendrogram
plot(kidneyHclust)

#Part 10 - cutree 
kidneyData$cluster <- cutree(kidneyHclust, k = 13)

#Part 11 - Visualize PCA
library(FactoMineR)
PCA(kidneyDataMatrix, scale.unit = TRUE, ncp = 4, graph = TRUE)




