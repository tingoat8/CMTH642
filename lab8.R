#Question 3
yelp.data <- read.csv(file = "yelp_lab8.csv")

train.index <-sample(1:nrow(yelp.data), 0.7*nrow(yelp.data))

data.train <- yelp.data[train.index,] 
data.test <- yelp.data[-train.index,]

data.test.factors <-data.train[,-2]
data.test.response <-data.train[,2]


glm_model <-glm(stars~., family = "binomial", data = data.train)
summary(glm_model)

# # Let's split the yelp dataset into training and test set.
# 
# yelp_reduced <- read.csv(file = "yelp_lab8.csv")
# 
# train_index <- sample(1:nrow(yelp_reduced), 0.7 * nrow(yelp_reduced))
# train.set <- yelp_reduced[train_index,]
# test.set  <- yelp_reduced[-train_index,]
# 
# # Now, we remove the stars column from our training and test datasets.
# 
# train.set_new <- train.set[-2]
# test.set_new <- test.set[-2]
# 
# # Now, we store the labels from our training and test datasets.
# 
# stars_train_labels <- train.set$stars
# stars_test_labels <- test.set$stars
# 
# glm_model <- glm(stars~.,train.set, family = "binomial")
# summary(glm_model)
# 
# predict(glm_model, newdata=data.frame(city="Toronto", review_count=200, categories="Coffee or Sandwiches"), type="response")
# 
# predicted_V1 <- ifelse(predicted>=0.5, 1, 0)
# confusionMatrix_V1 <- table(actual = test.set$stars, predicted = predicted_V1)
# sum(diag(confusionMatrix_V1))/nrow(test.set) 


predict(glm_model, newdata = data.frame(city ="Toronto", review_count = 200, categories = "Coffee or Sandwiches"), type = "response")

predicted <- predict(glm_model, data.test.factors, type = "response")

predicted_V1 <- ifelse(predicted>=0.5, 1, 0)
confusionMatrix_V1 <- table(actual = data.test.response, predicted = predicted_V1)
sum(diag(confusionMatrix_V1))/nrow(data.test)
