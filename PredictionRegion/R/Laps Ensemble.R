#' Creates ensemble of trees
#'
#' Uses multiple laps to create ensemble of trees to create vote as to whether this method works for a specified data set
#' @param emp.reps Number of decision trees (i.e laps)
#' @param reps Number of prediction regions to test to create a single decision tree
#' @param int.seed Largest possoble value to sample to obtain random seeds for every lap
#' @param n.boot.ens Number of bootstrap samples you want to use (# of intervals)
#' @param PIC.ens The minimum percentage of boostrapped CI's that must lie within prediction region
#' @param n.ens Sample size of simulated data
#' @param mu.ens Mean of simulated data
#' @param sig.ens Standard deviation of simulated data
#' @param inc.ens Size increments (Construction of prediction region)
#' @param b.CL.ens Confidence level used for bootstrap CI
#' @param testCL.ens Use this to be able to test any CL for t-test against that of the prediction region
#' @param symmetric.ens If TRUE, a symmetric prediction region will be created; else, an unsymmetric prediction region will be created
#' @param mean_boot_mean.ens If TRUE, the mean of bootstrapped means will be used as the starting point; else, the mean of the original data will be used
#' @return A list composed of rpart models, ctree models, rpart confusion matrices, ctree confusion matrices & empirical quantities for t-test, bootstrap percentile & prediction region intervals. Also creates a pdf in working directorate of every lap.
#' @export

laps_ensemble = function(emp.reps = 701, reps = 725, int.seed = 10000,
                         n.boot.ens = 750, PIC.ens = 0.95, n.ens = 10, mu.ens = 8, sig.ens = 2, inc.ens = 0.01,
                         b.CL.ens = 0.999, testCL.ens = 0.95, symmetric.ens = TRUE, mean_boot_means.ens = TRUE){
  rpart.models = list()
  ctree.models = list()
  rpart.matrix = list()
  ctree.matrix = list()
  all.data = list()

  emp.run <- for(nnn in seq_len(emp.reps)){
    pdf(file = paste("plots", nnn, ".pdf", sep = ""))
    par(mfrow = c(1,1))
    print(".......................")
    print(nnn)
    print(".......................")
    sample.seed = nnn
    set.seed(sample.seed)
    rand = sample(int.seed, reps)

    # Calculates different parameters used for criteria
    dat <- ldply(seq_len(reps), function(k){
      print(k)
      val = int.bin.boot(n.boot = n.boot.ens, PIC = PIC.ens, n = n.ens, mu = mu.ens,
                         sig = sig.ens, inc = inc.ens, b.CL = b.CL.ens, testCL = testCL.ens, seed = rand[k], m = k,
                         symmetric = symmetric.ens, mean_boot_means = mean_boot_means.ens)
      c(b.freq = val[1,], t.freq = val[2,], len.count = val[3,], y_sd = val[4,], y_range = val[5,], y_num.outliers = val[6,],
        y_kurtosis = val[7,], t.len = val[8,], bin.len = val[9,], check1 = val[10,], check2 = val[11,], y_excessCSkurtosis = val[12,],
        y_skewness = val[13,], y_normSW = val[14,], y_normKS = val[15,], y_norm_skewnessAgostino = val[16,],
        y_norm_skewnessSWC = val[17,], y_norm_kurtosisSWC = val[18,], y_normAD = val[19,], y_cv = val[20,], y_range_sd = val[21,],
        y_MAD = val[22,], y_Lscale = val[23,], y_Lkurtosis = val[24,], y_Lskewness = val[25,],
        y_cvm = val[26,], y_pearson = val[27,] , y_sf = val[28,], boot.q.len = val[29,], boot.q.freq = val[30,])
    })

    b.freq = dat$b.freq
    t.freq = dat$t.freq
    len.count = dat$len.count
    y_sd = dat$y_sd
    y_range = dat$y_range
    y_num.outliers = dat$y_num.outliers
    y_kurtosis = dat$y_kurtosis
    check1 = dat$check1
    check2 = dat$check2
    t.len = dat$t.len
    bin.len = dat$bin.len
    y_excessCSkurtosis = dat$y_excessCSkurtosis
    y_skewness = dat$y_skewness
    y_normSW = dat$y_normSW
    y_normKS = dat$y_normKS
    y_norm_skewnessAgostino = dat$y_norm_skewnessAgostino
    y_norm_skewnessSWC = dat$y_norm_skewnessSWC
    y_norm_kurtosisSWC = dat$y_norm_kurtosisSWC
    y_normAD = dat$y_normAD
    y_cv = dat$y_cv
    y_cv_red = abs(y_cv)
    y_cv_red[which(y_cv_red > 100)] = 100
    y_range_sd = dat$y_range_sd
    y_MAD = dat$y_MAD
    y_Lscale = dat$y_Lscale
    y_Lkurtosis = dat$y_Lkurtosis
    y_Lskewness = dat$y_Lskewness
    y_cvm = dat$y_cvm
    y_pearson = dat$y_pearson
    y_sf = dat$y_sf
    boot.q.len = dat$boot.q.len
    boot.q.freq = dat$boot.q.freq

    not.in.bin = which(bin.len < t.len & b.freq == 0)
    counter = length(not.in.bin)
    test = which(bin.len < t.len & b.freq == 1)
    count = length(test)

    par(mfrow = c(2,2))

    # bin > t : BLUE
    # bin < t & mu is contained : RED
    # bin < t & mu is not contained : GREEN

    # Plots y_range;
    plot(seq_len(reps)[-test], y_range[-test],  col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_range), max(y_range)))
    points(test, y_range[test], col = "red")
    text(test, y_range[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_range[not.in.bin], col = "green")
    text(not.in.bin, y_range[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_sd;
    plot(seq_len(reps)[-test], y_sd[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_sd), max(y_sd)))
    points(test, y_sd[test], col = "red")
    text(test, y_sd[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_sd[not.in.bin], col = "green")
    text(not.in.bin, y_sd[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_num.outliers;
    plot(seq_len(reps)[-test], y_num.outliers[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_num.outliers - 0.5), max(y_num.outliers)))
    points(test, y_num.outliers[test], col = "red")
    text(test, y_num.outliers[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_num.outliers[not.in.bin], col = "green")
    text(not.in.bin, y_num.outliers[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_kurtosis;
    plot(seq_len(reps)[-test], y_kurtosis[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_kurtosis), max(y_kurtosis)))
    points(test, y_kurtosis[test], col = "red")
    text(test, y_kurtosis[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_kurtosis[not.in.bin], col = "green")
    text(not.in.bin, y_kurtosis[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_excessCSkurtosis;
    plot(seq_len(reps)[-test], y_excessCSkurtosis[-test],  col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_excessCSkurtosis), max(y_excessCSkurtosis)))
    points(test, y_excessCSkurtosis[test], col = "red")
    text(test, y_excessCSkurtosis[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_excessCSkurtosis[not.in.bin], col = "green")
    text(not.in.bin, y_excessCSkurtosis[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_skewness;
    plot(seq_len(reps)[-test], y_skewness[-test],  col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_skewness) - 0.5, max(y_skewness) + 0.5))
    points(test, y_skewness[test], col = "red")
    text(test, y_skewness[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_skewness[not.in.bin], col = "green")
    text(not.in.bin, y_skewness[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_normSW;
    plot(seq_len(reps)[-test], y_normSW[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_normSW) - 0.1, max(y_normSW)))
    points(test, y_normSW[test], col = "red")
    text(test, y_normSW[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_normSW[not.in.bin], col = "green")
    text(not.in.bin, y_normSW[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_normKS;
    plot(seq_len(reps)[-test], y_normKS[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_normKS) - 0.1, max(y_normKS)))
    points(test, y_normKS[test], col = "red")
    text(test, y_normKS[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_normKS[not.in.bin], col = "green")
    text(not.in.bin, y_normKS[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_norm_skewnessAgostino;
    plot(seq_len(reps)[-test], y_norm_skewnessAgostino[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_norm_skewnessAgostino) - 0.1, max(y_norm_skewnessAgostino)))
    points(test, y_norm_skewnessAgostino[test], col = "red")
    text(test, y_norm_skewnessAgostino[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_norm_skewnessAgostino[not.in.bin], col = "green")
    text(not.in.bin, y_norm_skewnessAgostino[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_norm_skewnessSWC;
    plot(seq_len(reps)[-test], y_norm_skewnessSWC[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_norm_skewnessSWC) - 0.1, max(y_norm_skewnessSWC)))
    points(test, y_norm_skewnessSWC[test], col = "red")
    text(test, y_norm_skewnessSWC[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_norm_skewnessSWC[not.in.bin], col = "green")
    text(not.in.bin, y_norm_skewnessSWC[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_norm_kurtosisSWC;
    plot(seq_len(reps)[-test], y_norm_kurtosisSWC[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_norm_kurtosisSWC) - 0.1, max(y_norm_kurtosisSWC)))
    points(test, y_norm_kurtosisSWC[test], col = "red")
    text(test, y_norm_kurtosisSWC[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_norm_kurtosisSWC[not.in.bin], col = "green")
    text(not.in.bin, y_norm_kurtosisSWC[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_normAD;
    plot(seq_len(reps)[-test], y_normAD[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_normAD) - 0.1, max(y_normAD)))
    points(test, y_normAD[test], col = "red")
    text(test, y_normAD[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_normAD[not.in.bin], col = "green")
    text(not.in.bin, y_normAD[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_cv;
    plot(seq_len(reps)[-test], y_cv[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_cv), max(y_cv)))
    points(test, y_cv[test], col = "red")
    text(test, y_cv[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_cv[not.in.bin], col = "green")
    text(not.in.bin, y_cv[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_cv_red;
    plot(seq_len(reps)[-test], y_cv_red[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_cv_red) - 0.1, max(y_cv_red)))
    points(test, y_cv_red[test], col = "red")
    text(test, y_cv_red[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_cv_red[not.in.bin], col = "green")
    text(not.in.bin, y_cv_red[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_range_sd;
    plot(seq_len(reps)[-test], y_range_sd[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_range_sd) - 0.1, max(y_range_sd)))
    points(test, y_range_sd[test], col = "red")
    text(test, y_range_sd[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_range_sd[not.in.bin], col = "green")
    text(not.in.bin, y_range_sd[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_MAD;
    plot(seq_len(reps)[-test], y_MAD[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_MAD) - 0.1, max(y_MAD)))
    points(test, y_MAD[test], col = "red")
    text(test, y_MAD[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_MAD[not.in.bin], col = "green")
    text(not.in.bin, y_MAD[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_Lscale;
    plot(seq_len(reps)[-test], y_Lscale[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_Lscale) - 0.1, max(y_Lscale)))
    points(test, y_Lscale[test], col = "red")
    text(test, y_Lscale[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_Lscale[not.in.bin], col = "green")
    text(not.in.bin, y_Lscale[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_Lkurtosis;
    plot(seq_len(reps)[-test], y_Lkurtosis[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_Lkurtosis) - 0.1, max(y_Lkurtosis)))
    points(test, y_Lkurtosis[test], col = "red")
    text(test, y_Lkurtosis[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_Lkurtosis[not.in.bin], col = "green")
    text(not.in.bin, y_Lkurtosis[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_Lskewness;
    plot(seq_len(reps)[-test], y_Lskewness[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_Lskewness) - 0.1, max(y_Lskewness)))
    points(test, y_Lskewness[test], col = "red")
    text(test, y_Lskewness[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_Lskewness[not.in.bin], col = "green")
    text(not.in.bin, y_Lskewness[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_cvm;
    plot(seq_len(reps)[-test], y_cvm[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_cvm) - 0.1, max(y_cvm)))
    points(test, y_cvm[test], col = "red")
    text(test, y_cvm[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_cvm[not.in.bin], col = "green")
    text(not.in.bin, y_cvm[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_pearson;
    plot(seq_len(reps)[-test], y_pearson[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_pearson) - 0.1, max(y_pearson)))
    points(test, y_pearson[test], col = "red")
    text(test, y_pearson[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_pearson[not.in.bin], col = "green")
    text(not.in.bin, y_pearson[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_sf;
    plot(seq_len(reps)[-test], y_sf[-test], col="blue", xlim = c(-2, reps + 2), ylim = c(min(y_sf) - 0.1, max(y_sf)))
    points(test, y_sf[test], col = "red")
    text(test, y_sf[test], labels = test, cex= 0.4, pos = 1, col = "red")

    points(not.in.bin, y_sf[not.in.bin], col = "green")
    text(not.in.bin, y_sf[not.in.bin], labels = not.in.bin, cex= 0.4, pos = 1, col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_kurtosis vs y_skewness;
    plot(y_kurtosis[-test], y_skewness[-test], col = "blue")
    points(y_kurtosis[test], y_skewness[test], col = "red")
    points(y_kurtosis[not.in.bin], y_skewness[not.in.bin], col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_kurtosis vs y_normSW;
    plot(y_kurtosis[-test], y_normSW[-test], col = "blue")
    points(y_kurtosis[test], y_normSW[test], col = "red")
    points(y_kurtosis[not.in.bin], y_normSW[not.in.bin], col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_skewness vs y_normSW;
    plot(y_skewness[-test], y_normSW[-test], col = "blue")
    points(y_skewness[test], y_normSW[test], col = "red")
    points(y_skewness[not.in.bin], y_normSW[not.in.bin], col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)

    # Plots y_normKS vs y_normSW;
    plot(y_normKS[-test], y_normSW[-test], col = "blue", xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
    points(y_normKS[test], y_normSW[test], col = "red")
    points(y_normKS[not.in.bin], y_normSW[not.in.bin], col = "green")

    legend("topright", legend=c("t < bin", "bin < t & mu", "bin < t & NOT mu"),
           col=c("blue", "red", "green"), lty=1, cex=0.4)


    plotcount = which(y_normKS > 0.9)

    # check to see which one works best;
    plotcount_yes = plotcount[which(plotcount %in% test)]
    plotcount_no = plotcount[which(plotcount %in% not.in.bin)]

    #plotcount_yes = test[which(plotcount %in% test)]
    #plotcount_no = not.in.bin[which(plotcount %in% not.in.bin)]

    par(mfrow = c(1,1))
    # Allows to note certain metrics
    t.true.CL = sum(t.freq)/reps * 100
    b.true.CL = sum(b.freq)/reps * 100
    true.per.criteria = length(plotcount_yes)/length(plotcount)*100
    q.true.CL = sum(boot.q.freq)/reps * 100


    dat1 =  as.data.frame(cbind(check1, y_excessCSkurtosis, y_kurtosis, y_normSW,
                                y_num.outliers = as.factor(y_num.outliers), y_skewness, y_normKS,
                                y_norm_skewnessAgostino, y_norm_skewnessSWC, y_norm_kurtosisSWC,
                                y_normAD, y_Lkurtosis, y_Lskewness, y_cvm, y_pearson, y_sf))
    #, y_cv_red, y_MAD, y_Lscale, y_cv_red, y_range_sd, y_sd, y_range))


    rfit <- rpart(check1 ~ ., data = dat1, method = "class")
    rpredict <- apply(predict(rfit), 1, which.max) - 1
    conf.matrix(check1, rpredict)

    plot(rfit, uniform=TRUE, main="Classification Tree Check")
    text(rfit, use.n=TRUE, all=TRUE, cex=.8)
    rpart.plot(rfit)

    cfit <- ctree(check1 ~ ., data = dat1)
    cpredict = predict(cfit, dat1)
    cpredict = round.func(cpredict, mean(cpredict))
    #####cpredict <- unlist(lapply(predict(cfit), function(i){round.func(i, 0.7)}))
    #cpredict <- predict(cfit)
    #names(cpredict) <- NULL
    #cpredict <- round.func(unlist(cpredict), 0.7)
    conf.matrix(check1, cpredict)

    plot(cfit, uniform=TRUE, main="Classification Tree Check")

    # r.kfold = kfold(rpart, dat1, cp.min = 0, k = 50, hseed = 1039)
    # r.kfold
    #
    # c.kfold = kfold(ctree, dat1, k = 50, hseed = 1039)
    # c.kfold

    dev.off()

    test = which(bin.len < t.len & b.freq == 1)
    test1 = which(bin.len < boot.q.len & b.freq == 1)

    rpart.models[[nnn]] <- rfit
    rpart.matrix[[nnn]] <- conf.matrix(check1, rpredict)
    ctree.models[[nnn]] <- cfit
    ctree.matrix[[nnn]] <- conf.matrix(check1, cpredict)

    all.data[[nnn]] = c(Orig.Seed = nnn, b.true.CL = b.true.CL, t.true.CL = t.true.CL, q.true.CL = q.true.CL,
                        bin.len.t = mean(bin.len[test]),
                        bin.len.q = mean(bin.len[test1]),
                        boot.len.t = mean(boot.q.len[test]),
                        boot.len.q = mean(boot.q.len[test1]),
                        t.len.t = mean(t.len[test]),
                        t.len.q = mean(t.len[test1]))
  }
  list(rpart.models, ctree.models, rpart.matrix, ctree.matrix, all.data)
}
