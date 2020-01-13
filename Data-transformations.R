install.packages("ggplot2")
library(ggplot2)

custData<-readRDS("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./custdata.RDS")
dim(custData)
colnames(custData)
sum(is.na(custData))

custData<-ifelse( housing_type == NA, 0, dataVector )

V <- c(1, 3, NA, 10)
V[is.na(V)] <- 0
V

colnames(custData)
# ( 5 )
#Create 3 new variables and assign 0/1 based on condition
#dummy_cols(custData, select_columns = "gas_with_rent", "gas_with_electricity", "no_gas_bill")
custData$gas_with_rent <- ifelse(custData$gas_usage == 1, 1, 0)
custData$gas_with_electricity <- ifelse(custData$gas_usage == 2, 1, 0)  
custData$no_gas_bill <- ifelse(custData$gas_usage == 3, 1, 0)
#Print col no.s and col names (new 3 cols should be added)
dim(custData)
colnames(custData)

# ( 6 )
custData$age <- ifelse(custData$age > 0, custData$age, NA)
custData$income <- ifelse(custData$income > 0, custData$income, NA)
custData$gas_usage <- ifelse(custData$gas_usage > 4, NA, custData$gas_usage )
summary(custData$income)
# (7)
#num_vehicles
ggplot( custData) +
  geom_bar(aes(x = factor(custData$num_vehicles))) +
  ggtitle("num_vehicles") + 
  theme(axis.text.x = element_text(angle = 90, size=10, colour="#110000"))
#recent_move
ggplot( custData) +
  geom_bar(aes(x = factor(custData$recent_move))) +
  ggtitle("recent_move") + 
  theme(axis.text.x = element_text(angle = 90, size=10, colour="#110000"))
#health_ins
ggplot( custData) +
  geom_bar(aes(x = factor(custData$health_ins))) +
  ggtitle("health_ins") + 
  theme(axis.text.x = element_text(angle = 90, size=10, colour="#110000"))
#marital_status
ggplot( custData) +
  geom_bar(aes(x = factor(custData$marital_status))) +
  ggtitle("marital_status") + 
  theme(axis.text.x = element_text(angle = 90, size=10, colour="#110000"))
#is_employed
ggplot( custData) +
  geom_bar(aes(x = factor(custData$is_employed))) +
  ggtitle("is_employed") + 
  theme(axis.text.x = element_text(angle = 90, size=10, colour="#110000"))
#housing_type
ggplot( custData) +
  geom_bar(aes(x = factor(custData$housing_type))) +
  ggtitle("housing_type") + 
  theme(axis.text.x = element_text(angle = 90, size=10, colour="#110000"))

# (B) Histogram
# age
ggplot( custData) +
  geom_histogram(aes(x=custData$age) ) +
  theme(axis.text.x = element_text(hjust = 1)) 

ggplot( custData) +
  geom_histogram(aes(x=custData$income) ) +
  theme(axis.text.x = element_text(hjust = 1)) 

# ( C ) Scatter plot - Removed missing values before plotting
ggplot(custData) +
  geom_point(aes(custData$age,  custData$income)) +
  theme_minimal()

# 9 (A) 
# Denisity plots - Income
ggplot( custData) +
  geom_density(aes(x=custData$income)) + theme_minimal()
# Denisity plots - Age
ggplot( custData) +
  geom_density(aes(x=custData$age)) + theme_minimal()

#9 (C)
# Denisity plots - Income
signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

ggplot( custData) +
  geom_density(aes(x = signedlog10(custData$income))) + theme_minimal()


#Continuous to descrete variable
# (A) cut age ranges
age_range_func <- function() {
  brks <- c(0,25,65,130)
  custData$age.ageRange <- cut(custData$age, breaks=brks, include.lowest = T)
  summary(custData$age.ageRange)
  
  #Plot age ranges
  ggplot( custData) +
    geom_bar(aes(x = factor(custData$age.ageRange))) +
    ggtitle("Age Ranges") + 
    theme_minimal()
}

age_range_func()
#Mean of Age
age.mean <- mean(na.omit(custData$age))
print(age.mean)
#as.integer(age.mean)

#replacing NAs with integer( mean of Age)
custData$age <- ifelse(is.na(custData$age),as.integer(age.mean),custData$age)
summary(custData$age)

age_range_func()



