library(ggplot2)
library(GGally)
library(dplyr)
        library(tree)
library(rpart)
library(rpart.plot)

movie <- read.csv("data/movie_metadata.csv", header = T, stringsAsFactors = F)

columns <- c()
for(i in 1:dim(movie)[2])
{
  if(is.numeric(movie[,i]) || is.integer(movie[,i]))
  {
    columns[i] = T
  }
  else
  {
    columns[i] = F
  }
}
print(columns)
temp <- na.omit(movie[,columns])

ggplot(temp, aes(x=imdb_score)) + geom_histogram()

correlation <- c()
for(i in 1:dim(temp)[2])
{
  correlation[i] <- cor(temp[,i], temp[, 'imdb_score'])
}

correlation

ggplot(temp, aes(x = num_voted_users, y = imdb_score)) + geom_point(color="grey60") + stat_smooth(method=lm, se=FALSE, color="black") + ggtitle(paste('R:',correlation[7]))

ggplot(temp, aes(x = duration, y=imdb_score)) + geom_point(color="grey60") + stat_smooth(method=lm, se=FALSE, color="black") + ggtitle(paste('R:', correlation[2]))



# Linear Model

set.seed(2)
train <- sample(dim(temp)[1], dim(temp)[1] * 0.9)
temp_train <- temp[train,]
temp_test <- temp[-train,]

lmfit = lm(imdb_score ~ num_voted_users +duration, data= temp_train)
summary(lmfit)


pred <- predict(lmfit, temp_test)

mean((temp_test$imdb_score-pred)^2)

# Regression Trees

library(rpart)

set.seed(3)
m.rpart <- rpart(imdb_score~., data = temp_train)
print(m.rpart)
m.rpart

rpart.plot(m.rpart, digits = 3)


p.rpart <- predict(m.rpart, temp_test)


tree_dataframe <- data.frame(p.rpart, temp_test$imdb_score)

ggplot(tree_dataframe, aes(x=p.rpart)) + geom_histogram(fill="white", colour="black")

ggplot(tree_dataframe, aes(x=temp_test.imdb_score)) + geom_histogram(fill="white", colour="black")


cor(p.rpart, temp_test$imdb_score)

mean((p.rpart-temp_test$imdb_score)^2)


#Regression Tree
