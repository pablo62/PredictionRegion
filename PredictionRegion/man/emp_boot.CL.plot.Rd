% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Empirical Prediction Region CL vs Bootstrap
%   CL.R
\name{emp_boot.CL.plot}
\alias{emp_boot.CL.plot}
\title{Compares bootstrap confidence level to empirical prediction region confidence level}
\usage{
emp_boot.CL.plot(reps = 725, start.boot.CL = 0.99, inc.boot.CL = 5e-05,
  int.seed = 10000, n.boot.emp = 750, PIC.emp = 0.95, n.emp = 10,
  mu.emp = 8, sig.emp = 2, inc.emp = 0.01, testCL.emp = 0.95,
  symmetric.emp = TRUE, mean_boot_means.emp = TRUE)
}
\arguments{
\item{reps}{Number of times to test against a given bootstrap confidence level}

\item{start.boot.CL}{Starting bootstrap confidence level}

\item{inc.boot.CL}{Increment value to add to start.boot.CL until a confidence level of 1 is achieved}

\item{int.seed}{Largest possoble value to sample to obtain random seeds for every lap; intial seed depends on the loop based on the number of reps}

\item{n.boot.emp}{Number of bootstrap samples you want to use (# of intervals)}

\item{PIC.emp}{The minimum percentage of boostrapped CI's that must lie within prediction region}

\item{n.emp}{Sample size of simulated data}

\item{mu.emp}{Mean of simulated data}

\item{sig.emp}{Standard deviation of simulated data}

\item{inc.emp}{Size increments (Construction of prediction region)}

\item{testCL.emp}{Use this to be able to test any CL for t-test against that of the prediction region}

\item{symmetric.emp}{If TRUE, a symmetric prediction region will be created; else, an unsymmetric prediction region will be created}

\item{mean_boot_mean.emps}{If TRUE, the mean of bootstrapped means will be used as the starting point; else, the mean of the original data will be used}
}
\value{
Data frame with bootstrap CL and empirical CL for prediction region along with a plot
}
\description{
Creates plot for bootstrap confidence level vs empirical prediction region cnfidence level
}
