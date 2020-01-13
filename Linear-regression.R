library(ggplot2)
library(corrgram)
library(car)

#PArt 1
insurance <- read.csv("/Users/pavanshru/Desktop/Shru-TAMUC/classes/Aug-Fall19/Data Analysis & Vis./insurance.csv", stringsAsFactors = T)
dim(insurance)

#Part 2
ggplot( insurance ) +
  geom_histogram(aes(x=insurance$charges) ) +
  theme(axis.text.x = element_text(hjust = 1))

scatterplotMatrix(insurance[c("age", "bmi", "children", "charges")],
                  spread=FALSE, smoother.args=list(lty=1),
                  main="Scatter Plot Matrix", theme_minimal())

#Part 3
corrgram(insurance[c("children", "charges", "age", "bmi")],order=TRUE,
         main="Corrgram of insurance variables",
         lower.panel=panel.shade, upper.panel=panel.pie,
         text.panel=panel.txt)

#Part 4
nrows <- nrow(insurance)
alpha <- 0.8
trainIndex <- sample(1:nrows, alpha*nrows)
trainDataset <- insurance[trainIndex,]
testDataset <- insurance[-trainIndex,]

#Part 5
age = insurance$age
sex = insurance$sex
bmi = insurance$bmi
children = insurance$children
smoker = insurance$smoker
region = insurance$region
charges = insurance$charges
#( A )
lm1 <- lm(charges ~ age, data=trainDataset)
lm2 <- lm(charges ~ sex, data=trainDataset)
lm3 <- lm(charges ~ bmi, data=trainDataset)
lm4 <- lm(charges ~ children, data=trainDataset)
lm5 <- lm(charges ~ smoker, data=trainDataset)
lm6 <- lm(charges ~ region, data=trainDataset)

#( B )
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)

#( D )
ggplot(insurance, aes(x=age, y=charges)) + geom_point(method="lm") + stat_smooth()
ggplot(insurance, aes(x=sex, y=charges)) + geom_point() + stat_smooth()
ggplot(insurance, aes(x=bmi, y=charges)) + geom_point() + stat_smooth()
ggplot(insurance, aes(x=children, y=charges)) + geom_point() + stat_smooth(method="lm")
ggplot(insurance, aes(x=smoker, y=charges)) + geom_point() + stat_smooth(method="lm")
ggplot(insurance, aes(x=region, y=charges)) + geom_point() + stat_smooth(method="lm")


#(E)
p1 <- predict(lm1,testDataset)
p2 <- predict(lm2,testDataset)
p3 <- predict(lm3,testDataset)
p4 <- predict(lm4,testDataset)
p5 <- predict(lm5,testDataset)
p6 <- predict(lm6,testDataset)

ggplot(testDataset,aes(x=p1, y=charges)) + geom_point() + geom_smooth()
ggplot(testDataset,aes(x=p2, y=charges)) + geom_point() + geom_smooth()
ggplot(testDataset,aes(x=p3, y=charges)) + geom_point() + geom_smooth()
ggplot(testDataset,aes(x=p4, y=charges)) + geom_point() + geom_smooth()
ggplot(testDataset,aes(x=p5, y=charges)) + geom_point() + geom_smooth()
ggplot(testDataset,aes(x=p6, y=charges)) + geom_point() + geom_smooth()


#(F)
ggplot(aes(x=.fitted,y=.resid), data=lm1)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
ggplot(aes(x=.fitted,y=.resid), data=lm2)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
ggplot(aes(x=.fitted,y=.resid), data=lm3)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
ggplot(aes(x=.fitted,y=.resid), data=lm4)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
ggplot(aes(x=.fitted,y=.resid), data=lm5)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
ggplot(aes(x=.fitted,y=.resid), data=lm6)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")


#Part 6
lm.polynomial <- lm(charges ~ age + sex + bmi + children + smoker + region, data=trainDataset)
regr.summary = summary(lm.polynomial)
#Residual standard error
k=length(regr.summary$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(regr.summary$residuals**2)
n=length(regr.summary$residuals)
sqrt(SSE/(n-(1+k)))
#(OR) this way also we can find RSE
sigma(lm.polynomial)

#R-squared
regr.summary$r.squared


#Part 7
plot(lm.polynomial)
pairs.panels(insurance[c("age", "bmi", "smoker", "charges")])



insurance$ageSquared <- (insurance$age**2)
ins1 <- lm(charges ~ age + sex + bmi + children + smoker + region + ageSquared, data=insurance)
summary(ins1)








sigma(lm.poly.new)
plot(lm.poly.new)

lm.poly.modified <- lm(charges ~ age + sex + bmi + children + smoker + region + I(ageSquared) + bmi*smoker , data=insurance)
summary(lm.poly.modified)

ggplot(aes(x=.fitted,y=.resid), data=lm.polynomial)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
ggplot(aes(x=.fitted,y=.resid), data=lm.poly.new)+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")
ggplot(aes(x=.fitted,y=.resid), data=lm.poly.modified )+geom_point()+
  geom_smooth(se=F)+labs(x="Fitted Values", y="Residuals")



pp1 <- predict(lm.poly.modified,data.frame(age=53, smoker="yes", children=2,
                                                bmi=41, sex="female", region="southwest", ageSquared=53**2))

pp2 <- predict(lm.poly.modified,data.frame(age=28, smoker="no", children=1,
                                          bmi=41, sex="male", region="southeast", ageSquared=28**2))




