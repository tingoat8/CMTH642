# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

#Get Data
train.data <-read.csv(file = "https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
train <-train.data

#View data
attach(train)
head(train)
str(train)

#Change Pclass and Survived to factors
Pclass <- as.factor(Pclass)
Survived <- as.factor(Survived)
str(train)

#check for missing values in age and name
Na.NameAge <- matrix(c(sum(is.na(Name)), sum(is.na(Age))), ncol=1)
colnames(Na.NameAge) <- c('NAs')
rownames(Na.NameAge) <- c('Name', 'Age')
Na.NameAge

#Find the mean of the Age of Names with Mr. in title
mean.mr <- mean(Age[!is.na(Age)& grepl("Mr\\.", Name)])
#Replace missing ages with mean
Age[is.na(Age)& grepl("Mr\\.", Name)] <- mean.mr

#Find the mean of the Age of Names with Mrs. in title
mean.mrs <- mean(Age[!is.na(Age) & grepl("Mrs\\.", Name)])
#Replace missing ages with mean
Age[is.na(Age)& grepl("Mrs\\.", Name)] <- mean.mrs

#Find the mean of the Age of Names with Dr. in title
mean.dr <- mean(Age[!is.na(Age) & grepl("Dr\\.", Name)])
#Replace missing ages with mean
Age[is.na(Age)& grepl("Dr\\.", Name)] <- mean.dr

#Find the mean of the Age of Names with Dr. in title
mean.miss <- mean(Age[!is.na(Age) & grepl("Miss\\.", Name)])
#Replace missing ages with mean
Age[is.na(Age)& grepl("Miss\\.", Name)] <- mean.miss

#Find the mean of the Age of Names with Master. in title
mean.master <- mean(Age[!is.na(Age) & grepl("Master\\.", Name)])
#Replace missing ages with mean
Age[is.na(Age)& grepl("Master\\.", Name)] <- mean.master

#Check if there are remaining NAs
which(is.na(Age))

#Check the Port of Embarkation for NAs
str(train)
#Replace empty with NA
Embarked[Embarked== ""] <-NA

#Distributions
distribution <-as.data.frame(table(Embarked, useNA = "always"))
View(distribution)
plot(table(Embarked, useNA = "always"), type ="h", col = "red")
distribution$Embarked[distribution$Freq==max(distribution$Freq)]##S has the greatest freq



#Assign Southhampton to NAs
Embarked[is.na(Embarked)] = distribution$Embarked[distribution$Freq==max(distribution$Freq)]
sum(is.na(Embarked))

#Distributions
distribution <-as.data.frame(table(Embarked, useNA = "always"))
View(distribution)






