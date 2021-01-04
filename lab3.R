#Prelim
install.packages("ISwR")
require(ISwR)

pain <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug <- as.factor(c(rep("A",9), rep("B",9), rep("C",9))) 
migraine <- data.frame(drug,pain)
migraine
plot(pain ~ drug, data=migraine)
results <-lm(pain ~ drug, data = migraine)
anova(results)

pairwise.t.test(pain, drug, p.adjust.method = "bonferroni", alternative = c("two.sided", "less", "greater"))

#Ex1
group1 <- c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1) 
group2 <- c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7) 
group3 <- c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)
response <-c(group1, group2, group3)#Create column of responses
groupNumbers <-as.factor(rep(1:3, rep(7,3)))#create column of factors
data<-data.frame(groupNumbers,response)#create dataframe
plot(response ~ groupNumbers, data = data)#box plot of response regressed with groups
#Notice, mean for group 3 is lower than all others
model <- aov(response ~ groupNumbers, data = data)
anova(model)

#Ex2

attach(red.cell.folate)
ventilation <- as.factor(ventilation)
model <-aov(folate ~ ventilation, data = red.cell.folate)
summary(model)
anova(model)

#Ex3
attach(juul)
str(juul)
tanner <- factor(tanner, labels = c("i", "ii", "iii", "iv", "v"))
summary(tanner)
model <- lm(igf1 ~ tanner, data =data.frame(tanner, igf1))
anova(model)
