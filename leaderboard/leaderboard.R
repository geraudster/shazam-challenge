library(caret)
library(e1071)

# Load reference data
known.classes <- read.csv('submission-label.csv', header = TRUE, stringsAsFactors = TRUE)

getScore <- function(fileName) {
  # Load submission
  submission.classes <- read.csv(fileName, header = TRUE, stringsAsFactors = TRUE)
  
  merged <- merge(known.classes, submission.classes, by = 'idSound', suffix = c('known', 'submission'))

  # Measure performance
  confMatrix <- confusionMatrix(trimws(merged$classsubmission), trimws(merged$classknown))
  confMatrix$overall['Accuracy']
}
