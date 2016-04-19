data.file <- xzfile('../train.csv.xz', open = 'r')
data <- read.csv(data.file, header = TRUE, stringsAsFactors = FALSE)
close(data.file)

colnames(data)
data$class <- factor(data$class)
levels(data$class)
str(data)

library(dplyr)

plotByClass <- function(dataset) {
  ggplot(dataset, aes(x = class, fill = class)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    coord_flip() +
    geom_text(aes(y = (..count..)/sum(..count..),
                  label = paste(round((..count..)/sum(..count..)*100), "%")), stat = 'count') +
    theme(legend.position="none") +
    ylab('Ratio')
}

byIdSound <- group_by(data, idSound)
plotByClass(byIdSound)

# +
#   stat_bin(geom = "text",
#            aes(x=class, label = paste(round((..count..)/sum(..count..)*100), "%")),
#            vjust = 5)
#+
#  scale_y_continuous(labels = percent)

library(reshape2)
library(rpart)
library(caret)
library(flexclust)
library(plyr)

set.seed(1234)
inTrain <- createDataPartition(data$idSound, p = 0.8, list = FALSE)
trainset <- data[inTrain,]
testset <- data[-inTrain,]

plotByClass(trainset)
plotByClass(testset)

k <- 30
system.time(
  clusters <- cclust(trainset[, 2:14], k)
)

trainset$cluster <- predict(clusters)

trainset.words <- dcast(trainset, idSound + class ~ cluster, length, margins = c('idSound', 'cluster'))
trainset.words$idSound <- as.character(trainset.words$idSound)

trainset.wordsFreqs <- trainset.words[-nrow(trainset.words), 3:(k+2)] / trainset.words[-nrow(trainset.words), 3 + k]
trainset.wordsFreqs <- cbind(trainset.wordsFreqs, trainset.words[-nrow(trainset.words), c('idSound', 'class')])
trainset.wordsFreqs$class <- factor(trainset.wordsFreqs$class)
str(trainset.wordsFreqs)

qplot(trainset.wordsFreqs$class)
set.seed(1234)
system.time(
  model <- train(class ~ ., trainset.wordsFreqs[,-(k+1)], method = 'rf',
                 trControl = trainControl(verboseIter = TRUE),
                 tuneLength = 3)
)

set.seed(1234)
system.time(
  model <- train(class ~ ., trainset.wordsFreqs[,-(k+1)], method = 'xgbLinear',
                 trControl = trainControl(verboseIter = TRUE),
                 tuneLength = 3)
)

confusionMatrix(model)

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
write.csv(submissionDf, 'my-submission.csv', row.names = FALSE)


## Plot spider

accuracies <- confusionMatrix(trimws(merged$classsubmission), trimws(merged$classknown))$byClass[,8] %>%
  data.frame(class = gsub('Class: ', '', names(.)), score = .)

library(radarchart)
chartJSRadar(scores = accuracies, maxScale = 1, scaleStepWidth = 0.25)

## build partial submission
library(plyr)
library(dplyr)
partialSubmission <- ldply(levels(submissionDf$class),
                           function(x) {
                             submissionDf[first(which(submissionDf$class == x)),]
                           })
write.csv(partialSubmission, '../my-submission-partial.csv', row.names = FALSE)
getScore('../my-submission-partial.csv')
