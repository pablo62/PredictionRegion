% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/User Prediction Interval Construction.R
\name{FinalPredictionRegion}
\alias{FinalPredictionRegion}
\title{Builds prediction region if data meets criteria}
\usage{
FinalPredictionRegion(df)
}
\arguments{
\item{df}{The data to be used}
}
\value{
If data passes criteria, prediction endpoints are returned along with a plot of bootstrap intervals, t-interval & bootstrap percentile interval
}
\description{
Uses ensemble to conclude whether the data is fit to be used with this method
}
