gender <- c("male", "female", "male", "male", "female")

factor.data <- factor(gender)
print(factor.data)


product1 <- c(20, 19, 18, 20, 22, 12, 33, 23,  4,  5,  6, 20 ,19, 18 ,20, 22, 12, 33, 23 , 4 , 5 , 6)
product1
product2 <- c(23, 22, 11, 22, 33, 33 ,88 ,33 ,22, 33 ,33 ,33,  7 , 4, 11, 33 ,11 , 3, 22,  6,  5 , 5)
product3 <- c(23 ,19 , 1 , 7 , 3, 11, 23, 19 , 1 , 7 , 3 ,11, 33, 23,  4 , 5 , 6 ,20, 19 ,18, 20, 44)
product4 <- c(4 , 8, 10 ,7 , 8 , 6 ,10 , 7,  8 , 6 ,23 ,19,  1 , 7 , 3 ,11, 33, 23 , 4 , 5 , 6 ,20)
temp <- c(60 ,60 ,70, 60 ,70, 60, 60, 70, 60, 70, 80, 80 ,80, 80 ,80, 80, 60, 70 ,60, 70, 80 ,80)


product1
product2
product3
product4
temp
df <- data.frame(temp, product1, product2, product3, product4)
df
temp.factor <- factor(df$temp)
temp.factor

