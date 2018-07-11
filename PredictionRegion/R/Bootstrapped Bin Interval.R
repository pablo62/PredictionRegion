#' Creates various metrics of the original data
#'
#' Generates normal data set according to parameters, builds multiple bootstrap intervals & calculates various normality metrics. Also generates a visual of the bootstrapped intervals along with the prediction region and t-interval
#' @param n.boot Number of bootstrap samples you want to use (# of intervals)
#' @param PIC The minimum percentage of boostrapped CI's that must lie within prediction region
#' @param n Sample size of simulated data
#' @param mu Mean of simulated data
#' @param sig Standard deviation of simulated data
#' @param b.CL Confidence level used for bootstrap CI
#' @param inc Size increments (Construction of prediction region)
#' @param seed Avoids randomizations according to the seed placed
#' @param testCL Use this to be able to test any CL for t-test against that of the prediction region
#' @param m Used to determine what iteration the data was created; only used for plotting purposes
#' @param symmetric If TRUE, a symmetric prediction region will be created; else, an unsymmetric prediction region will be created
#' @param mean_boot_means If TRUE, the mean of bootstrapped means will be used as the starting point; else, the mean of the original data will be used
#' @return Various metrics based of off the simulated data
#' @export

int.bin.boot = function(n.boot = 750, PIC = 0.95, n = 10, mu = 8 , sig = 2, b.CL = 0.999,
                        inc = 0.01, seed = NULL, testCL = 0.95, m = 1,
                        symmetric = TRUE, mean_boot_means = TRUE){
  set.seed(seed)

  # y is the original data
  y <- rnorm(n = n, mean = mu, sd = sig)
  y_range = max(y)-min(y)
  y_sd = sd(y)
  lower.threshold = quantile(y)[2] - (quantile(y)[4] - quantile(y)[2])*1.5
  upper.threshold = (quantile(y)[4] - quantile(y)[2])*1.5 + quantile(y)[4]
  y_num.outliers = sum(y < lower.threshold | y > upper.threshold)
  y_kurtosis = kurtosis(y)
  y_excessCSkurtosis = cs_kurt(y)
  y_skewness = skewness(y)
  y_normSW = unlist(shapiro.test(y)[2])
  # Converts normal data to uniform, to use ks.test()
  y_stand.norm = (y - mean(y))/y_sd #y/sd(y)
  y_unif = pnorm(y_stand.norm)
  y_normKS = ks.test(y_unif, "punif")$p.value
  y_norm_skewnessAgostino = agostino.test(y)$p.value
  y_norm_skewnessSWC = skewness.norm.test(y)$p.value
  y_norm_kurtosisSWC = kurtosis.norm.test(y)$p.value
  y_normAD = ad.test(y)$p.value
  y_cv = y_sd/mean(y)
  y_range_sd = y_range/y_sd
  y_MAD = mean_abs_dev(y)
  y_Lmomemnts = Lcoefs(y,rmax=4,na.rm=FALSE,trim=c(0,0))
  y_Lscale = y_Lmomemnts[2]
  y_Lkurtosis = y_Lmomemnts[3]
  y_Lskewness = y_Lmomemnts[4]
  y_cvm = cvm.test(y)$p.value
  y_pearson = pearson.test(y)$p.value
  y_sf = sf.test(y)$p.value

  # In each iteration:
  #   Computes bootstrapped dataset
  #   Stores left endpoint, mean & right endpoint of the CI of bootstrapped data
  x <- ldply(seq_len(n.boot), function(i){
    boot.y = sample(y, n, replace = TRUE)
    y.stat = t.test(boot.y, conf.level = b.CL)
    y.sd = sd(boot.y)
    c(xleft = y.stat$conf.int[1], y.stat$estimate, xright = y.stat$conf.int[2], ysd = y.sd)
  })

  # Creating overall mean and endpoints according to specific ratio
  xmeanmean = mean(x$`mean of x`)
  left.end <- xmeanmean
  right.end <- xmeanmean

  if(symmetric == TRUE){
    if(mean_boot_means == TRUE){
      ratio <- sum(cbind( x$xleft <= xmeanmean & x$xright >= xmeanmean ))/n.boot
    } else{
      ratio <- sum(cbind( x$xleft <= mean(y) & x$xright >= mean(y)))/n.boot
    }
    # Widens the interval until it meets the PIC criteria
    while(ratio >= PIC){
      left.end <- left.end - inc
      right.end <- right.end + inc
      ratio <- sum(x$xleft <= left.end & x$xright >= right.end)/(n.boot)
    }
  } else {
    if(mean_boot_means == TRUE){
      l.ratio = sum(x$xleft <= xmeanmean)/n.boot
      r.ratio = sum(x$xright >= xmeanmean)/n.boot
    } else{
      l.ratio = sum(x$xleft <= mean(y))/n.boot
      r.ratio = sum(x$xright >= mean(y))/n.boot
    }
    PIC2 = PIC + (1-PIC)/2
    while(l.ratio >= PIC2){
      left.end <- left.end - inc
      l.ratio <- sum(x$xleft <= left.end)/(n.boot)
    }
    while(r.ratio >= PIC2){
      right.end <- right.end + inc
      r.ratio <- sum(x$xright >= right.end)/(n.boot)
    }
  }
  bin.len <- right.end - left.end

  # Original data t-test
  y.int1 <- t.test(y, conf.level = testCL)
  t.len = y.int1$conf.int[2] - y.int1$conf.int[1]

  # Plots histogram along with density of the difference between xmeanmean and xmean
  hist(xmeanmean - x$`mean of x`, freq = FALSE, main = paste("n.boot = ", n.boot,
                                                             "    n = ", n, "    mu = ", mu, "    sigma = ", sig, "    m = ", m,
                                                             sep=""))
  lines(density(xmeanmean - x$`mean of x`))

  # Plot bootstrap CIs
  z <- 1:n.boot
  plotCI(z, x$`mean of x`, ui=x$xright, li=x$xleft, main = paste("n.boot = ", n.boot,
                                                                 "    n = ", n, "    mu = ", mu, "    sigma = ", sig, "    m = ", m,
                                                                 sep=""), cex.main = 0.8, ylab = "bootstrap intervals", pch=21, pt.bg="gray48",
         cex=0.5, xlab = paste(sprintf("t.len = %.2f", t.len) , sprintf("   bin.len = %.2f",
                                                                        bin.len)), cex.lab = 0.8, cex.sub = 0.8, cex.axis = 0.8)

  # Plot original data CI
  ablineclip(h=y.int1$conf.int[1], x1 = 0, x2 = n.boot, col = "orange")
  ablineclip(h=y.int1$conf.int[2], x1 = 0, x2 = n.boot, col = "orange")

  # Bin Interval
  ablineclip(h=right.end, x1 = 0, x2 = n.boot, col = "red")
  ablineclip(h=left.end, x1 = 0, x2 = n.boot, col = "red")

  # Percentile Bootstrap Interval
  boot.q.left = quantile(x$`mean of x`, (1-testCL)/2)
  boot.q.right = quantile(x$`mean of x`, 1 - (1-testCL)/2)
  ablineclip(h=boot.q.left, x1 = 0, x2 = n.boot, col = "cyan")
  ablineclip(h=boot.q.right, x1 = 0, x2 = n.boot, col = "cyan")
  boot.q.len = boot.q.right - boot.q.left

  # True data mean (mu)
  ablineclip(h=mu, x1 = 0, x2 = n.boot, col = "blue")

  # Mean of bootstrapped means
  ablineclip(h=xmeanmean, x1 = 0, x2 = n.boot, col = "purple")

  if(mean_boot_means == TRUE){
    legend("topright", legend=c("Mu", "Xmeanmean", paste("PIC", PIC), paste("b.CL.", b.CL),
                                paste("testCL.", testCL), paste("Precentile", testCL)), col=c("blue", "purple", "red",
                                                                                              "green", "orange", "cyan"),
           lty=1, cex=0.4)
  }
  else{
    legend("topright", legend=c("Mu", "Xmean", paste("PIC", PIC), paste("b.CL.", b.CL),
                                paste("testCL.", testCL), paste("Precentile", testCL)), col=c("blue", "purple", "red",
                                                                                              "green", "orange", "cyan"),
           lty=1, cex=0.4)
  }
  # This function returns:
  #   1.Test if mu is included in bin interval
  #   2.Test if mu is included in t-test interval
  #   3.Test if bin length is narrower than t-test
  #   4.Original data sd
  #   5.Original data range
  #   6.Original data number of outliers
  #   7.Original data excess kurtosis
  #   8.Original data t-test interval length
  #   9.Bootstrap data bin interval length
  #   10. Bin < t & mu is in bin interval
  #   11. Bin < t & mu is in both intervals
  #   12. Original data excess CS Kurtosis
  #   13. Original data skewness
  #   14. Original data Shapiro Wilks test p-value
  #   15. Original data Kolmogorov-Smirnov test p-value
  #   16. Original data D'Agostino Skewness test p-value [deviate from 0]
  #   17. Original data Shapio/Wilks/Chen Skewness test with Monte Carlo Simulation p-value
  #   18. Original data Shapio/Wilks/Chen Kurtosis test with Monte Carlo Simulation p-value
  #   19. Original data Anderson Darling test p-value
  #   20. Original data coefficient of variation
  #   21. Original data range / sd
  #   22. Original data Mean Absolute Deviance
  #   23. Original data L-moment standard deviation
  #   24. Original data L-moment kurtosis
  #   25. Original data L-moment skewness
  #   26. Original data Cramer-Von Mises test p-value
  #   27. Original data Pearson Chi-Squared test p-value
  #   28. Original data Shapiro-Francia test p-value
  #   29. Length of Bootstrap Percentile Interval of Original Data
  #   30. Checks if mu is contained within Bootstrap Percentile Interval
  rbind(mu > left.end & mu < right.end, #1
        mu > y.int1$conf.int[1] & mu < y.int1$conf.int[2], #2
        bin.len<t.len, #3
        y_sd, #4
        y_range, #5
        y_num.outliers, #6
        y_kurtosis, #7
        t.len, #8
        bin.len, #9
        mu > left.end & mu < right.end & bin.len<t.len, #10
        mu > left.end & mu < right.end & bin.len<t.len & mu > y.int1$conf.int[1] & mu < y.int1$conf.int[2], #11
        y_excessCSkurtosis, #12
        y_skewness, #13
        y_normSW, #14
        y_normKS, #15
        y_norm_skewnessAgostino, # 16
        y_norm_skewnessSWC, # 17
        y_norm_kurtosisSWC, # 18
        y_normAD, #19
        y_cv, #20
        y_range_sd, #21
        y_MAD, #22
        y_Lscale, #23
        y_Lkurtosis, #24
        y_Lskewness, #25
        y_cvm, #26
        y_pearson, #27
        y_sf, #28
        boot.q.len, #29
        mu > boot.q.left & mu < boot.q.right #30
  )
}
