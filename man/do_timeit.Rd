\name{do_timeit}
\alias{do_timeit}
\title{Profile a Function Call}
\usage{
  do_timeit(call, replications = NULL, interval = 0.005,
    memory.profiling = FALSE, show.warnings = FALSE, i = 1,
    gcFirst = TRUE, gcDuring = FALSE)
}
\arguments{
  \item{call}{a call (typically passed down through
  \code{timeit}).}

  \item{interval}{real. time interval between samples.}

  \item{replications}{integer; by default \code{NULL},
  which indicates we should 'guess' an appropriate number
  of replications. in order to more accurately profile
  quickly-running functions, we run the call
  \code{replications} times, and then infer the run-time as
  \code{<time>/replications}. by default, the argument is
  \code{NULL} and we attempt to infer an appropriate number
  of replications.}

  \item{memory.profiling}{logical. include memory use in
  output?}

  \item{show.warnings}{boolean. output a warning if any
  iteration of the run did not produce results?}

  \item{i}{integer. the iteration number. primarily for use
  from \code{\link{timeit}}.}

  \item{gcFirst}{boolean. run the garbage collector before
  any evaluation of the function call?}

  \item{gcDuring}{boolean. run the garbage collector before
  each iteration, as produced by \code{replications}? (very
  slow)}
}
\value{
  A data.frame of the profiling times.
}
\description{
  This is the workhorse function called by
  \code{\link{timeit}}, and is primarily meant to be called
  through \code{\link{timeit}}. However, if you desire a
  more direct wrapper to \code{Rprof} then this can be
  useful.
}

