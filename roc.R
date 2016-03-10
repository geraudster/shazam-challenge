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

