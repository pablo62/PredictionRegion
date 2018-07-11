#' Builds prediction region if data meets criteria
#'
#' Uses ensemble to conclude whether the data is fit to be used with this method
#' @param df The data to be used
#' @return If data passes criteria, prediction endpoints are returned along with a plot of bootstrap intervals, t-interval & bootstrap percentile interval
#' @export

FinalPredictionRegion = function(df){
  data("rpart.models_data")

  y_excessCSkurtosis = cs_kurt(df)
  y_kurtosis = kurtosis(df)
  y_normSW = unlist(shapiro.test(df)[2])
  lower.threshold = quantile(df)[2] - (quantile(df)[4] - quantile(df)[2])*1.5
  upper.threshold = (quantile(df)[4] - quantile(df)[2])*1.5 + quantile(df)[4]
  y_num.outliers = as.factor(sum(df < lower.threshold | df > upper.threshold))
  y_skewness = skewness(df)
  y_sd = sd(df)
  y_stand.norm = (df - mean(df))/y_sd
  y_unif = pnorm(y_stand.norm)
  y_normKS = ks.test(y_unif, "punif")$p.value
  y_norm_skewnessAgostino = agostino.test(df)$p.value
  y_norm_skewnessSWC = skewness.norm.test(df)$p.value
  y_norm_kurtosisSWC = kurtosis.norm.test(df)$p.value
  y_normAD = ad.test(df)$p.value
  y_Lmomemnts = Lcoefs(df,rmax=4,na.rm=FALSE,trim=c(0,0))
  y_Lkurtosis = y_Lmomemnts[3]
  y_Lskewness = y_Lmomemnts[4]
  y_cvm = cvm.test(df)$p.value
  y_pearson = pearson.test(df)$p.value
  y_sf = sf.test(df)$p.value

  df_test = as.data.frame(cbind(y_excessCSkurtosis, y_kurtosis, y_normSW,
                                y_num.outliers, y_skewness, y_normKS,
                                y_norm_skewnessAgostino, y_norm_skewnessSWC, y_norm_kurtosisSWC,
                                y_normAD, y_Lkurtosis, y_Lskewness, y_cvm, y_pearson, y_sf))
  vote = vector()

  for(i in seq_len(length(rpart.models_data))){
    vote[i] = as.numeric(levels(droplevels(predict(rpart.models_data[[i]], df_test, type = "class"))))
  }
  final_vote = sum(vote)
  if(final_vote > round(length(rpart.models_data)*0.95)){
    print("The data passed, the code will continue to run")
    real.int.bin.boot(df)
  } else{
    print("The data does not meet criteria, the code will stop")
  }
}
