% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.cosinor.param.R
\name{create.cosinor.param}
\alias{create.cosinor.param}
\title{Create cosinor parameters using the time variable and the cosinor period}
\usage{
create.cosinor.param(time, period, data)
}
\arguments{
\item{time}{a vector of time variable name, with same units as that of the cosinor period}

\item{period}{the length of the cosinor period, eg.24}

\item{data}{the dataframe with the time and period information}
}
\description{
Create cosinor parameters using the time variable and the cosinor period
}
\examples{
db.model<-create.cosinor.param(time="Hour_of_Day", period=24, data=data)
}
