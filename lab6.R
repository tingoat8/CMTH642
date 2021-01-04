# Q1:
df.data <-airquality
head(df.data)

data.all <- df.data$Ozone
data.rank <-rank(data.all)

n <- length(df.data$Ozone)

may <-sum(data.rank[df.data$Month==5])
june <-sum(data.rank[df.data$Month==6])
july <-sum(data.rank[df.data$Month==7])
aug <- sum(data.rank[df.data$Month==8])
sep <-sum(data.rank[df.data$Month==9])

n1 <- sum(df.data$Month==5)
n2 <- sum(df.data$Month==6)
n3 <- sum(df.data$Month==7)
n4 <- sum(df.data$Month==8)
n5 <- sum(df.data$Month==9)

a <- 12/(n*(n+1))
t1 <- (may*may)/n1
t2 <- (june*june)/n2
t3 <- (july*july)/n3
t4 <- (aug*aug)/n4 
t5 <- (sep*sep)/n5
b <- 3*(n +1)

H <- a*(t1 + t2 + t3 + t4 + t5) - b
H

H > qchisq(.95, df=4)

kruskal.test(airquality$Ozone ~ airquality$Month, data = airquality)

Women.data <- c(23, 41, 54, 66, 78)
Men.data <- c(45, 55, 60, 70, 72)
Minorities.data <- c(18, 30, 34, 40, 44)
data.all <- c(Women.data , Men.data, Minorities.data)

factor <- (rep(c(1, 2, 5), each =5))
df.data <- data.frame(factor, data.all)
n <- length(data.all)
data.rank <-rank(data.all)
Women.data.rank.sum <-sum(data.rank[data.all == Women.data ])
Men.data.rank.sum <-sum(data.rank[data.all == Men.data])
min.rank.sum <-sum(data.rank[data.all == Minorities.data])

a <- 12/(n*(n+1))
t1 <- (Men.data.rank.sum*Men.data.rank.sum)/length(Men.data)
t2 <- (Women.data.rank.sum*Women.data.rank.sum)/length(Women.data )
t3 <- (min.rank.sum *min.rank.sum) /length(Minorities.data)
b <- 3*(n +1)
H <- a* (t1 + t2 + t3) - b
H

H <- (12/(15*(15+1)) )* ( (44*44)/5 + (56*56)/5 + (20*20)/5 ) - 3 * (15+1)

kruskal.test(data.all ~ factor, data = airquality)


# Q2:
English = c(56,	75,	45,	71,	62,	64,	58,	80,	76,	61)
Math = c(66, 70, 40, 60, 65, 56, 59, 77, 67, 63)

plot(density(English)) #not normal
plot(density(Math)) #not normal
#Not normal, so use Spearman
data.all<- c(English, Math)
rank.all <-rank(data.all)
rank.english <-rank.all[data.all == English]
rank.Math <-rank.all[data.all == Math]
n <- length(data.all)
n.math <-length(Math)
n.english <- length(English)

Sxy <- as.numeric((English - mean(English)) %*% (Math-mean(Math)))

Sxx <- as.numeric((English - mean(English)) %*% (English - mean(English)))

Syy <- as.numeric((Math-mean(Math)) %*% (Math-mean(Math)))
r.spearman <- Sxy/(sqrt(Sxx*Syy))
r.spearman

cor(rank.english,rank.Math)
cor.test(English, Math, method = "spearman")

