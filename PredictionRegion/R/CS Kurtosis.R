#' Creates CS Kurtosis
#'
#' Takes in a data set and calculates CS Kurtosis
#' @param data_set A data set used to calculate CS Kurtosis
#' @return CS Kurtosis
#' @export
cs_kurt = function(data_set){
  quant = quantile(data_set, c(.05,.95,.25,.75))
  num = quant[2] - quant[1]
  den = quant[4] - quant[3]
  beta_cs = num/den
  round(beta_cs, digits = 4)
}
