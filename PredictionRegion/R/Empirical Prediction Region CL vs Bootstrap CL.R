#' Compares bootstrap confidence level to empirical prediction region confidence level
#'
#' Creates plot for bootstrap confidence level vs empirical prediction region cnfidence level
#' @param reps Number of times to test against a given bootstrap confidence level
#' @param start.boot.CL Starting bootstrap confidence level
#' @param inc.boot.CL Increment value to add to start.boot.CL until a confidence level of 1 is achieved
#' @param int.seed Largest possoble value to sample to obtain random seeds for every lap; intial seed depends on the loop based on the number of reps
#' @param n.boot.emp Number of bootstrap samples you want to use (# of intervals)
#' @param PIC.emp The minimum percentage of boostrapped CI's that must lie within prediction region
#' @param n.emp Sample size of simulated data
#' @param mu.emp Mean of simulated data
#' @param sig.emp Standard deviation of simulated data
#' @param inc.emp Size increments (Construction of prediction region)
#' @param testCL.emp Use this to be able to test any CL for t-test against that of the prediction region
#' @param symmetric.emp If TRUE, a symmetric prediction region will be created; else, an unsymmetric prediction region will be created
#' @param mean_boot_mean.emps If TRUE, the mean of bootstrapped means will be used as the starting point; else, the mean of the original data will be used
#' @return Data frame with bootstrap CL and empirical CL for prediction region along with a plot
#' @export

emp_boot.CL.plot = function(reps = 725, start.boot.CL = 0.99, inc.boot.CL = 0.00005, int.seed = 10000,
                         n.boot.emp = 750, PIC.emp = 0.95, n.emp = 10, mu.emp = 8, sig.emp = 2, inc.emp = 0.01,
                         testCL.emp = 0.95, symmetric.emp = TRUE, mean_boot_means.emp = TRUE){

  values = vector()
  perc = seq(start.boot.CL, 1, inc.boot.CL) # Bootstrap CL
  size = seq(1, length(perc) - 1, 1)

  # Computes the empirical CL for the Bin Interval
  # Loops over the different bootstrap CLs
  values = unlist(lapply(seq_len(length(perc)-1), function(j){
    print("######################################")
    print(perc[j])
    print("######################################")

    set.seed(j)
    rand = sample(int.seed, reps)

    # Calculates boolean whether mu is in bin interval or not; "reps" amount of times
    # Does so at a given bootstrap CL
    b.freq = unlist(lapply(seq_len(reps), function(m){
      print(m)
      emp_int.bin.boot( n.boot = n.boot.emp, PIC = PIC.emp, n = n.emp, mu = mu.emp,
                        sig = sig.emp, b.CL = perc[j], testCL = testCL.emp, seed = rand[m], inc = inc.emp,
                        symmetric = symmetric.emp, mean_boot_means = mean_boot_means.emp)
    }))

    sum(b.freq)/reps

  }))
  # Plots Bootstrap CL vs Empirical Bin CL
  plot(perc[-length(perc)], values, xlab = "Bootstrap CL", ylab = "Empirical Bin CL")
  data.frame(bootCL = perc[-length(perc)], binEmpiricalCL= values, "i" = size, reps = reps, set.seed.i = paste("sample(", int.seed, ", reps)"))
}
