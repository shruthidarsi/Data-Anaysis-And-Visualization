file = "/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/churn_data.txt"
churndata <- read.csv(file, stringsAsFactors = FALSE,header=TRUE, sep=",")

#A colnames
colnames(churndata)

#B No. of rows and cols
dim(churndata)

#C count the No. calls per state
table(churndata$State, churndata$Day.Calls + churndata$Eve.Calls + churndata$Night.Calls + churndata$Intl.Calls + churndata$CustServ.Calls)
#data.table(churndata)
#calls <- churn.data["Day.Calls"] + churn.data["Eve.Calls"] + churn.data["Night.Calls"] + churn.data["Intl.Calls"] + churn.data["CustServ.Calls"]
#count(calls)


#D mean, median, Standard deviation and varience of any column data
mean(churndata$Night.Charge)
median(churndata$Night.Charge)
sd(churndata$Night.Charge)
var(churndata$Night.Charge)

#E min and max vals
min(churndata$Intl.Charge)
max(churndata$Intl.Charge)
min(churndata$CustServ.Calls)
max(churndata$CustServ.Calls)
min(churndata$Day.Charge)
max(churndata$Day.Charge)

#F 
summ <- c(churndata$Eve.Charge, churndata$Night.Mins, churndata$Night.Calls, 
          churndata$Night.Charge, churndata$Intl.Mins, churndata$Intl.Calls)
summary(summ)
summary(churndata$Eve.Charge)
summary(churndata$Night.Mins)
summary(churndata$Night.Calls)
summary(churndata$Night.Charge)
summary(churndata$Intl.Mins)
summary(churndata$Intl.Calls)


#G
uniq <- c(churndata$State, churndata$Area.Code, churndata$Churn)
unique(uniq)

unique(churndata$State)
unique(churndata$Area.Code)
unique(churndata$Churn)

#H
churn_df <- as.data.frame(churndata)
NROW(subset(churn_df, churn="TRUE"))

#I
NROW(subset(churn_df, churn_df$CustServ.Calls>3))

#J
NROW(subset(churn_df, churn_df$Int.l.Plan=="no" & churn_df$VMail.Plan=="no"))

#K
NROW(subset(churn_df, churn_df$State=="CA" & churn_df$Churn=="FALSE" & churn_df$CustServ.Calls>2))

#L
mean(subset(churn_df, churn_df$Churn=="FALSE")$CustServ.Calls)
