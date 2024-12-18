library(randomForest)
library(datasets)
library(caret)
UFdata<-All.worm.video.trait.features
data<-UFdata[!rowSums(is.na(UFdata)) > ncol(UFdata)*.3,]
str(data)
data[is.na(data)] <- 0
data$fileid <- as.factor(data$fileid)
table(data$fileid)
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]
rf <- randomForest(fileid~., data=train, proximity=TRUE, na.action=na.roughfix)
print(rf)
p1 <- predict (rf, train)
confusionMatrix(p1, train$ fileid)
p2 <- predict(rf, test)
confusionMatrix(p2, test$ fileid)
plot(rf)
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)