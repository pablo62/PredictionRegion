#' Runs k-folds on either rpart or ctree
#'
#' Depending on the call (rpart or ctree), runs either with data split into k partitions
#' @param ctree.OR.rpart User specified to use rpart or ctree
#' @param data_frame Data to be used
#' @param cp.min Minimum cp value to be used if rpart is selected
#' @param k Number of partitions in which to partition the data
#' @param hseed Avoids randomizations according to the seed placed
#' @return Metrics from confusion matrix
#' @export

kfold = function(ctree.OR.rpart, data_frame, cp.min = 0, k = 5, hseed = 10){
  libraries()

  data.k = split_data.header(data_frame, k, hseed)
  response.name = names(data_frame[1])
  data.formula = as.formula(paste(response.name, " ~ ", ".", sep = ""))

  pred_val = NULL

  i.length = length(data.k)

  if (identical(ctree.OR.rpart, rpart) == TRUE){
    pred_val = unlist(lapply(seq_len(i.length), function(i){
      test.data = data_frame[-data.k[[i]],]
      predict.data = data_frame[data.k[[i]],]

      r.tree = ctree.OR.rpart(data.formula, data = test.data, cp = cp.min, method = "class")
      apply(predict(r.tree, predict.data), 1, which.max) - 1

    }))} else {
      pred_val = unlist(lapply(seq_len(i.length), function(i){
        test.data = data_frame[-data.k[[i]],]
        predict.data = data_frame[data.k[[i]],]

        c.tree = ctree.OR.rpart(data.formula, data = test.data)
        t = predict(c.tree, predict.data, type = "response")
        names(t) = NULL
        names(t) = unlist(data.k[[i]])
        round.func(t, mean(t))
      }))
    }
  true_value = data_frame[names(pred_val),response.name]
  conf.matrix(true_value, pred_val)
}
