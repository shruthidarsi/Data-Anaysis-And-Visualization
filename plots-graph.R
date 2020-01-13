install.packages("ggplot2")
library(ggplot2)
file.path="/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/churn_data.txt"
churnData <- read.csv(file.path, stringsAsFactors = FALSE, header = TRUE)
axis.text.x = element_text(face="bold",angle=90,vjust=0.5, size=11)
axis.title.x = element_text(face="bold", colour="#990000", size=12)

#Vertical Bar Charts
#(1)
ggplot( churnData) +
  geom_bar(aes(x = factor(churnData$State))) +
  ggtitle("State Frequency") + 
  theme(axis.text.x = element_text(angle = 90, size=5, colour="#110000"))
ggplot( churnData) +
  geom_bar(aes(x = factor(churnData$Area.Code))) +
  ggtitle("Area Code Frequency") +
  theme(axis.text.x = element_text(angle = 90, size=15))
ggplot( churnData) +
  geom_bar(aes(x = factor(churnData$Int.l.Plan))) +
  ggtitle("International Plan Frequency") +
  theme(axis.text.x = element_text(hjust = 1))
ggplot( churnData) +
  geom_bar(aes(x = factor(churnData$VMail.Plan), position="dodge")) +
  ggtitle("VMail Plan Frequency") +
  theme(axis.text.x = element_text(hjust = 1, size=15))
ggplot( churnData) +
  geom_bar(aes(x = factor(churnData$CustServ.Calls), position="dodge")) +
  ggtitle("Cust.Service Calls Frequency") + 
  theme(axis.text.x = element_text(hjust = 1, size=15))
ggplot( churnData) +
  geom_bar(aes(x = factor(churnData$Churn), position="dodge")) +
  ggtitle("Churn Frequency") + 
  theme(axis.text.x = element_text(hjust = 1))


#Horizontal Bar charts
#(2)
ggplot( churnData) +
  geom_bar(aes(x = factor(churnData$CustServ.Calls), position="dodge")) +
  ggtitle("Cust.Service Calls Frequency") + 
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=5))

#(3) - Horizontal Bar Charts with Sorted Categories
Cust.serv.data <- table(churnData$CustServ.Calls)
Cust.df <- as.data.frame(Cust.serv.data)
colnames(Cust.df) <- c("Cust.Serv.calls", "count")
summary(Cust.df)
Cust.df <- transform(Cust.df, Cust.Serv.calls = reorder(Cust.Serv.calls, count))
summary(Cust.df)
ggplot( Cust.df) +
  geom_bar(aes(x = Cust.Serv.calls, y=count),stat="identity",fill="gray") +
  ggtitle("Cust.Service Calls Frequency") + 
  coord_flip() +
  theme_minimal()


#library(scales)
#Part-2 (1)
ggplot( churnData) +
  geom_histogram(aes(x=churnData$Account.Length) ) +
  theme(axis.text.x = element_text(hjust = 1)) 
  #scale_y_continuous(limits = c(0, 250))

mean(churnData$Account.Length)
median(churnData$Account.Length)
ggplot( churnData) +
  geom_histogram(aes(x=churnData$VMail.Message))
ggplot( churnData) +
  geom_histogram(aes(x=churnData$Day.Mins))
mean(churnData$Day.Mins)
median(churnData$Day.Mins)
ggplot( churnData) +
  geom_histogram(aes(x=churnData$Intl.Call))

#(2)
ggplot( churnData) +
  geom_density(aes(x=churnData$Account.Length))
mean(churnData$Account.Length)
median(churnData$Account.Length)
ggplot( churnData) +
  geom_density(aes(x=churnData$VMail.Message))
ggplot( churnData) +
  geom_density(aes(x=churnData$Day.Mins))
ggplot( churnData) +
  geom_density(aes(x=churnData$Intl.Call))

#Part-3 Scatter plots
#(A)
ggplot(churnData) +
  geom_point(aes(churnData$Day.Mins, churnData$Eve.Mins)) +
  theme_minimal()
ggplot(churnData) +
  geom_point(aes(churnData$Day.Mins, churnData$Day.Charge)) +
  theme_minimal()
ggplot(churnData) +
  geom_point(aes(churnData$Eve.Mins, churnData$Eve.Charge)) +
  theme_minimal()
ggplot(churnData) +
  geom_point(aes(churnData$Day.Mins, churnData$Day.Calls)) +
  theme_minimal()

#(B)
ggplot(churnData) +
  geom_point(aes(color=churnData$Churn, x=churnData$Eve.Mins, y=churnData$Day.Mins)) +
  theme_minimal()

#Part-4
ggplot(churnData) +
  geom_boxplot(aes(y=churnData$CustServ.Calls, x=churnData$Churn))
ggplot(churnData) + 
  geom_boxplot(aes(x=churnData$Churn, y=churnData$Night.Calls))

median(subset(churnData, churnData$Churn=="FALSE" & na.omit(churnData$Night.Calls))$Night.Calls)

ggplot(churnData) + 
  geom_boxplot(aes(x=churnData$Churn, y=churnData$Intl.Charge))

#Part-5
ggplot(churnData) +
  geom_bar(aes(x=churnData$Churn, fill=churnData$Int.l.Plan), position="dodge")+
  theme(axis.text.x = element_text(hjust = 1,size=7))

ggplot(churnData) +
  geom_bar(aes(x=churnData$CustServ.Calls, fill=churnData$Churn)) 


