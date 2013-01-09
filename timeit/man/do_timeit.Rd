\name{do_timeit}
\alias{do_timeit}
\title{Profile a Function Call}
\usage{
  do_timeit(call, interval = 0.005,
    memory.profiling = FALSE)
}
\arguments{
  \item{call}{a call; this can be a single function call or
  a whole block.}

  \item{interval}{real: time interval between samples.}

  \item{memory.profiling}{logical: write memory use
  information to file?}
}
\value{
  a data.frame of the profiling times
}
\description{
  This is the workhorse function called by
  \code{\link{timeit}}. It is almost a direct export of the
  example on \code{\link{summaryRprof}}.
}

