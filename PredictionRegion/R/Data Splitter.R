#' Splits the data into k partitions
#'
#' Randomizes the data and then partitions it out into k folds
#' @param data.set Data to be used
#' @param n.folds Number of partitions of the data
#' @param seed Avoids randomizations according to the seed placed
#' @return The data randomly split into k partitions
#' @export

split_data.header = function(data.set, n.folds, seed = NULL){
  set.seed(seed)
  data.size = nrow(data.set)
  sort.var.names = sample(seq_len(data.size), data.size, replace = F)

  df = split(sort.var.names, cut(seq_along(sort.var.names), n.folds, labels = F))
  df
}
