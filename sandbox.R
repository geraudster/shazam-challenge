data.file <- xzfile('../train.csv.xz', open = 'r')
data <- read.csv(data.file, header = TRUE, stringsAsFactors = FALSE)
close(data.file)

colnames(data)
data$class <- factor(data$class)
levels(data$class)
str(data)

library(reshape2)
library(rpart)
library(caret)
library(flexclust)

inTrain <- createDataPartition(data$idSound, p = 0.8, list = FALSE)
trainset <- data[inTrain,]
testset <- data[-inTrain,]

k <- 40
clusters <- cclust(trainset[, 2:14], k)

trainset$cluster <- predict(clusters)

trainset.words <- dcast(trainset, idSound + class ~ cluster, length, margins = c('idSound', 'cluster'))
trainset.words$idSound <- as.character(trainset.words$idSound)

trainset.wordsFreqs <- trainset.words[-nrow(trainset.words), 3:(k+2)] / trainset.words[-nrow(trainset.words), 3 + k]
trainset.wordsFreqs <- cbind(trainset.wordsFreqs, trainset.words[-nrow(trainset.words), c('idSound', 'class')])
trainset.wordsFreqs$class <- factor(trainset.wordsFreqs$class)
str(trainset.wordsFreqs)

qplot(trainset.wordsFreqs$class)
set.seed(1234)
model <- train(class ~ ., trainset.wordsFreqs[,-21], method = 'rf',
               trControl = trainControl(verboseIter = TRUE),
               tuneLength = 5)

confusionMatrix(model)

library(pROC)
library(ROCR)

preds <- predict(model, newdata = trainset.wordsFreqs)
table(preds, trainset.wordsFreqs$class)
preds.dog <- ifelse(preds == 'dog_bark',1,0)
actual.dog <- ifelse(trainset.wordsFreqs$class == 'dog_bark',1,0)

predictions.ROCR <- prediction(preds.dog, actual.dog)

perf <- performance(predictions.ROCR, measure = 'tpr', x.measure = 'fpr')

roc.data <- data.frame(fpr=unlist(perf@x.values),
                     tpr=unlist(perf@y.values),
                     model="GLM")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
       geom_ribbon(alpha=0.2) +
       geom_line(aes(y=tpr)) +
       ggtitle('Courbe ROC pour le modèle de régression logistique')

applyModel <- function(dataset) {
  dataset$cluster <- predict(clusters, dataset[,2:14])
  dataset.words <- dcast(dataset, idSound ~ cluster, length, margins = c('idSound', 'cluster'))
  dataset.wordsFreqs <- dataset.words[-nrow(dataset.words), 2:(k+1)] / dataset.words[-nrow(dataset.words), k + 2]
  dataset.wordsFreqs <- cbind(dataset.wordsFreqs, idSound = dataset.words[-nrow(dataset.words), c('idSound')])
  dataset.wordsFreqs$idSound <- as.character(dataset.wordsFreqs$idSound)

  preds.test <- predict(model, newdata = dataset.wordsFreqs)
  data.frame(idSound = dataset.wordsFreqs$idSound, class = preds.test, stringsAsFactors = FALSE)
}

testset.predict <- applyModel(testset)
testset.observed <- join(testset.predict, testset, by = 'idSound', match = 'first')[,c(1, 16)]
confusionMatrix(testset.predict$class, testset.observed$class)
head(testset.predict)
head(testset.observed)

validationFile <- xzfile('../validation.csv.xz', open = 'r')
validation <- read.csv(validationFile, header = TRUE, stringsAsFactors = FALSE)
close(validationFile)

validation.predict <- applyModel(validation)

submissionDf <- data.frame(idSound = validation.predict$idSound, class = validation.predict$class)
