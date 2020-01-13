#Problem 1
v <- c(21,10,32,2,-3,4,5,6,7,4,-22)
w <- c(-18,72,11,-9,10,2,34,-5,18,9,2)

# (A)
length(v)
length(w)

#(B)
v
w

# (C)
v[3:7]
w[3:7]

#(D)
sum(v)
sum(w)

#(E)
mean(v)
mean(w)

#(F)
sort(v, decreasing = TRUE)
sort(w, decreasing = TRUE)

#(G)
v+w

#(H)
v*w

#(I)
v[v>0]

#(J)
w[w<0]

#(K)
v*6

#(L)
print("Max and Min of Vector v are:")
max(v)
min(v)
print("Max and Min of w are:")
max(w)
min(w)

#(M)
v[v<0] <- mean(v)
v
w[w<0] <- mean(w)
w

#Problem (2)
for (i in 100:1) {
  if(i%%2==0) print(i)
}

#Problem (3)
city<- c("Chicago","Kenosha","Aurora","Elgin","Gary","Joliet","Naperville","Arlington Heights","Bolingbrook","Cicero","Evanston","Hammond","Palatine","Schaumburg","Skokie","Waukegan")
county<- c( "Cook", "Kenosha", "Kane", "Kane", "Lake(IN)", "Kendall", "DuPage", "Cook", "Will", "Cook", "Cook", "Lake(IN)", "Cook", "Cook", "Cook", "Lake(IL)")
state<- c( "IL", "WI", "IL", "IL", "IN", "IL", "IL", "IL", "IL", "IL", "IL", "IN", "IL", "IL", "IL", "IL")
population<-c(2853114,90352,171782,94487,102746,106221,147779,76031,70834,72616,74239,83048,67232,75386,63348,91452)

df <- data.frame(city, county, state, population)

#(A)
mean_population <- mean(df$population)
mean_population

#(B)
df[df$population>800000,]$population

#(C)
df[df$population<1000000,]

#(D)
subset(df, population == max(population))

#(E)
subset(df, population == min(population))


