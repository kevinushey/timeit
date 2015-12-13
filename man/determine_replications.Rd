\name{determine_replications}
\alias{determine_replications}
\title{Replication Helper}
\usage{
  determine_replications(call, interval, base = 1e+06)
}
\arguments{
  \item{call}{the call.}

  \item{interval}{the timing interval as passed from
  \code{\link{timeit}}.}

  \item{base}{the base number of replications to use.}
}
\description{
  This function is used to infer an appropriate number of
  replications in \code{\link{timeit}}.
}

