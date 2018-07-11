#' Creates Confusion Matrix
#'
#' Takes in both actual and predicted values and calculates various metrcis
#' @param response.values Actual respose values
#' @param predicted.values Predicted respose values
#' @return A list of accuracy, misclassification.rate, true.positive.rate, false.positive.rate, specificity, precision, prevalence
#' @export

conf.matrix = function(response.values, predicted.values){
  confusion = table(response.values, predicted.values)
  TN = confusion[1,1]   # TN
  FP = confusion[1,2]   # FP
  FN = confusion[2,1]   # FN
  TP = confusion[2,2]   # TP
  N = TN + FP + FN + TP
  accuracy = (TP + TN)/N  # Accuracy
  misclassification.rate = 1 - accuracy
  true.positive.rate = TP/(FN + TP)  # True Positive Rate
  false.positive.rate = FP/(TN + FP)
  specificity = 1 - false.positive.rate
  precision = TP/(FP + TP)
  prevalence = (FN + TP)/N
  rates = cbind(accuracy, misclassification.rate, true.positive.rate,
                false.positive.rate, specificity, precision,
                prevalence)
  list(confusion, rates)
}
