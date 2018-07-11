#' Creates prediction region for real data
#'
#' Creates prediction region aling with t-interval & bootstrap percentile interval
#' @param df Data to be used to construct prediction region
#' @param n.boot Number of bootstrap samples you want to use (# of intervals)
#' @param PIC The minimum percentage of boostrapped CI's that must lie within prediction region
#' @param b.CL Confidence level used for bootstrap CI
#' @param inc Size increments (Construction of prediction region)
#' @param seed Avoids randomizations according to the seed placed
#' @param testCL Use this to be able to test any CL for t-test against that of the prediction region
#' @param m Used to determine what iteration the data was created; only used for plotting purposes
#' @param symmetric If TRUE, a symmetric prediction region will be created; else, an unsymmetric prediction region will be created
#' @param mean_boot_means If TRUE, the mean of bootstrapped means will be used as the starting point; else, the mean of the original data will be used
#' @return Gives prediction region endpoints along with a plot
#' @export

real.int.bin.boot = function(df, n.boot = 750, PIC = 0.95, b.CL = 0.999,
                        inc = 0.01, seed = NULL, testCL = 0.95, m = 1,
                        symmetric = TRUE, mean_boot_means = TRUE){
  set.seed(seed)

  # y is the original data
  y <- df
  n <- length(df)

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
                                                             "    n = ", n, "    m = ", m,
                                                             sep=""))
  lines(density(xmeanmean - x$`mean of x`))

  # Plot bootstrap CIs
  z <- 1:n.boot
  plotCI(z, x$`mean of x`, ui=x$xright, li=x$xleft, main = paste("n.boot = ", n.boot,
                                                                 "    n = ", n, "    m = ", m,
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

  # Mean of bootstrapped means
  ablineclip(h=xmeanmean, x1 = 0, x2 = n.boot, col = "purple")

  if(mean_boot_means == TRUE){
    legend("topright", legend=c("Xmeanmean", paste("PIC", PIC), paste("b.CL.", b.CL),
                                paste("testCL.", testCL), paste("Precentile", testCL)), col=c("purple", "red",
                                                                                              "green", "orange", "cyan"),
           lty=1, cex=0.4)
    c(left.end, xmeanmean, right.end)
  }
  else{
    legend("topright", legend=c("Xmean", paste("PIC", PIC), paste("b.CL.", b.CL),
                                paste("testCL.", testCL), paste("Precentile", testCL)), col=c("purple", "red",
                                                                                              "green", "orange", "cyan"),
           lty=1, cex=0.4)
    c(left.end, mean(y),right.end)
  }
}
