library(caret)
library(e1071)
library(magrittr)

# Load reference data
known.classes <- read.csv('submission-label.csv', header = TRUE, stringsAsFactors = TRUE)
known.classes.levels <- levels(known.classes$class)

getScore <- function(fileName) {
  # Load submission
  submission.classes <- read.csv(fileName, header = TRUE, stringsAsFactors = TRUE)
  submission.classes$class <- factor(trimws(submission.classes$class),
                                     levels = known.classes.levels)
  stopifnot(setequal(submission.classes$idSound, known.classes$idSound))
  merged <- merge(known.classes, submission.classes, by = 'idSound', suffix = c('known', 'submission'))

  # Measure performance
  confMatrix <- confusionMatrix(merged$classsubmission, merged$classknown)
  list(Accuracy = confMatrix$overall['Accuracy'],
       ByClass = confMatrix$byClass[,8] %>%
         data.frame(class = gsub('Class: ', '', names(.)), score = .))
}
