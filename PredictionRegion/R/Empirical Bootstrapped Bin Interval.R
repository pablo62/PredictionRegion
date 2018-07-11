#' Tests whether prediction region contains mu or not
#'
#' Similar to int.bin.boot with the exception that this function only check to see if mu is contained within prediction region &
#' @param n.boot Number of bootstrap samples you want to use (# of intervals)
#' @param PIC The minimum percentage of boostrapped CI's that must lie within prediction region
#' @param n Sample size of simulated data
#' @param mu Mean of simulated data
#' @param sig Standard deviation of simulated data
#' @param b.CL Confidence level used for bootstrap CI
#' @param inc Size increments (Construction of prediction region)
#' @param seed Avoids randomizations according to the seed placed
#' @param testCL Use this to be able to test any CL for t-test against that of the prediction region
#' @param symmetric If TRUE, a symmetric prediction region will be created; else, an unsymmetric prediction region will be created
#' @param mean_boot_means If TRUE, the mean of bootstrapped means will be used as the starting point; else, the mean of the original data will be used
#' @return 0 if mu is not contained, 1 if mu is contained within prediction region
#' @export

emp_int.bin.boot = function(n.boot = 750, PIC = 0.95, n = 10, mu = 8, sig = 2, b.CL = 0.999,
                            inc = 0.01, seed = NULL, testCL = 0.95, symmetric = TRUE, mean_boot_means = TRUE){
  set.seed(seed)

  # Creates original Normal dataset
  y <- rnorm(n = n, mean = mu, sd = sig)

  # In each iteration:
  #   Computes bootstrapped dataset
  #   Stores left endpoint, mean & right endpoint of the CI of bootstrapped data
  x <- ldply(seq_len(n.boot), function(i){
    boot.y = sample(y, n, replace = TRUE)
    y.stat = t.test(boot.y, conf.level = b.CL)
    c(xleft = y.stat$conf.int[1], y.stat$estimate, xright = y.stat$conf.int[2])
  })

  xmeanmean = mean(x$`mean of x`)

  # Gives a starting point for the prediction interval
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

  # Returns boolean if mu was contained within prediction interval
  mu > left.end & mu < right.end
}
