#Question 1
x <- c(6, 8, 2, 4, 4, 5)
y <- c(7, 10, 4, 3, 5, 6)
wilcox.test(y,x, paired= FALSE)
data.xy <- c(x,y)
data.xy.rank.small <-rank(data.xy)
data.xy.rank.large <-rank(-data.xy)
t1 <- sum(data.xy.rank.small[data.xy==x]) 
t1.star <- sum(data.xy.rank.large[data.xy==x])
t<-min(t1, t1.star)
t

#Question 2

#A) Are these two groups of sampling paired or independent
#paired, since measuring the difference in population
a <- c(159, 135, 141, 101, 102, 168, 62, 167, 174, 159, 66, 118, 181, 171, 112)
b <- c(214, 159, 169, 202, 103, 119, 200, 109, 132, 142, 194, 104, 219, 119, 234)
wilcox.test(a,b,paired =T)


#C)
diff <- c(a - b) #Calculating the vector containing the differences
diff.abs.rank <- rank(abs(diff)) #rank of all the differences
diff.pos <- diff.abs.rank[diff > 0] #all pos ranks
diff.neg <- diff.abs.rank[diff < 0] #all neg ranks
pos.sum <- sum(diff.pos)
neg.sum <- sum(diff.neg)
T <- min(pos.sum, neg.sum) #T stat!
neg.sum
pos.sum

#Question 3

zip <- c(10, 44, 65, 77, 43, 44, 22, 66, 50, 100, 55, 99, 44, 23, 100, 88, 200, 220, 110, 551)
tar <- c(20, 55, 75, 60, 55, 88, 35, 33, 35, 80, 65, 82, 47, 35, 97, 110, 250, 190, 111, 600)

plot(density(zip)) #not normal
plot(density(tar)) #not normal

diff <- c(zip - tar) #Calculating the vector containing the differences
diff.abs.rank <- rank(abs(diff)) #rank of all the differences
diff.pos <- diff.abs.rank[diff > 0] #all pos ranks
diff.neg <- diff.abs.rank[diff < 0] #all neg ranks
pos.sum <- sum(diff.pos)
neg.sum <- sum(diff.neg)
T <- min(pos.sum, neg.sum) #T stat!
neg.sum
pos.sum

wilcox.test(zip,tar,paired =T)
