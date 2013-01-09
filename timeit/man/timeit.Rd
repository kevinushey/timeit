\name{timeit}
\alias{timeit}
\title{Profile a Function Call}
\usage{
  timeit(call, interval = 0.01, memory.profiling = TRUE,
    times = 5)
}
\arguments{
  \item{call}{a call; this can be a single function call or
  a whole block.}

  \item{interval}{real: time interval between samples.}

  \item{memory.profiling}{logical: write memory use
  information to file?}

  \item{times}{integer. how many times to call the
  function?}
}
\value{
  an object of S3 classes \code{timeit} and
  \code{data.frame}.
}
\description{
  This is a wrapper to \code{\link{Rprof}} that cleans up
  some of the profile hand-holding and provides easier
  usage. This allows you to profile either a single
  function call, or a whole block. Evalutation can be run
  multiple times in order to assess variability in the
  timings of each function call (to some level of
  precision).
}
\details{
  Function calls that get executed very quickly will be
  missed by \code{Rprof}, unless you set \code{interval}
  very low. However, doing this will probably break things
  (and isn't really important, since profiling is there to
  help you catch the longest-running functions.)
}
\examples{
\dontrun{
tmp <- timeit({
  x <- 1:1E5; y <- x + runif(1E5)
  lm( y ~ x )
  }, times=5)
summary(tmp)
y <- 1E6
f <- function(x) { summary( sort( rnorm(x) ) ) }
tmp <- timeit( f(y), times=5 )
summary(tmp)
mean(tmp)
plot(tmp)}
}
\seealso{
  \code{\link{summary.timeit}} for summary output,
  \code{\link{plot.timeit}} for generating a boxplot of the
  returned times, \code{\link{do_timeit}} for the workhorse
  function, and \code{\link{Rprof}} for information on how
  R profiles execution of \R expressions.
}

