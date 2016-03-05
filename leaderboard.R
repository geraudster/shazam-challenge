submission.classes <- read.csv('../submission-label.csv', header = TRUE, stringsAsFactors = TRUE)
str(submission.classes)

library(magrittr)
merged <- merge(submission.classes, submissionDf, by = 'idSound')

confusionMatrix(merged$class.x, merged$class.y)

submissionDf[submissionDf$idSound == 0,]
submission.classes[submission.classes$idSound == 0,]
?confusionMatrix
