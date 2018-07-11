#' Creates Mean Absolute Difference
#'
#' Takes in a data set and calculates the Mean Absolute Difference
#' @param df A data set used to calculate the mean absolute difference
#' @return Mean Absolute Difference
#' @export

mean_abs_dev = function(df){
  mean_df = mean(df)
  n = length(df)
  val = vector()
  for(i in seq_len(length(df))){
    val[i] = abs(df[i] - mean_df)
  }
  sum(val)/n
}
