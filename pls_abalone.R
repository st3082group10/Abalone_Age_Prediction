data = read.csv("abalone.csv")
library(car)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(GGally)
library(packHV)
library(dplyr)
head(data)

data = data[data$Height > 0 & data$Height < 1,]
encoded_data <- model.matrix(~Sex-1, data)
abalone_encoded <- cbind(data, encoded_data)
head(abalone_encoded)
abalone_encoded = abalone_encoded[,-11]

set.seed(123)
train_indices <- sample(c(1:nrow(data)), nrow(data)*0.8)

#Descriptive Data Analysis
train <- abalone_encoded[train_indices,]
test <- abalone_encoded[-train_indices,]
head(test)
library(mdatools)
library(caret)

yc = train$Rings
xc = train[,c(2:8,10:11)]
head(xc)
m = pls(xc, yc,scale = TRUE, cv = 5)
summary(m)
plot(m)
plotXScores(m)
plotXYLoadings(m, show.labels = T, cex = 1, lab.cex = 1, lab.col = "black")
