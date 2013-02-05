\name{timeit}
\alias{timeit}
\title{Profile a Function Call}
\usage{
  timeit(call, replications = NULL, interval = 0.01,
    memory.profiling = TRUE, times = 10,
    show.warnings = FALSE)
}
\arguments{
  \item{call}{a call; this can be a single function call or
  a whole block.}

  \item{replications}{integer; by default \code{NULL},
  which indicates we should 'guess' an appropriate number
  of replications. in order to more accurately profile
  quickly-running functions, we run the call
  \code{replications} times, and then infer the run-time as
  \code{<time>} / \code{replications}. by default, the
  argument is \code{NULL} and we attempt to infer an
  appropriate number of replications.}

  \item{interval}{real. time interval between samples.}

  \item{memory.profiling}{logical. output memory usage
  statistics?}

  \item{times}{integer. how many times to call the
  function?}

  \item{show.warnings}{boolean. output a warning if any
  iteration of the run did not produce results?}
}
\value{
  An object of S3 classes \code{timeit} and
  \code{data.frame}.
}
\description{
  This is a wrapper to \code{\link{Rprof}} that cleans up
  some of the profile hand-holding and provides easier
  usage. This allows you to profile either a single
  function call, or a whole block. Evaluation can be run
  multiple times in order to assess variability in the
  timings of each function call.
}
\details{
  Function calls that get executed very quickly will be
  missed by \code{Rprof}, unless you set \code{interval}
  very low. However, doing this will probably break things
  (and isn't really important, since profiling is there to
  help you catch the longest-running functions.) If you
  really want to time quickly executed functions, you can
  manually set the \code{replications} argument: we
  evaluate the \code{call} \code{replications} times, and
  infer the (average) run-time of the function as
  \code{<time taken> / replications}.
}
\note{
  If you set the \code{replications} argument high, you
  will likely see some output from the \code{do_timeout}
  call that is unrelated to your function call. This is due
  to all the wrapping of a function call to be executed by
  \code{Rprof} introduces a minor overhead. For other
  caveats, please see \code{\link{Rprof}}.

  Currently, \code{timeit} does not support passing through
  of arguments, so don't try to wrap \code{timeit} in a
  function call, whereby the call it attempts to evaluate
  is passed from a parent function. For example,

  \code{f <- function(x) { timeit(x) }; f(rnorm(10))}

  won't work properly; a fix may come in the future.
}
\examples{
\dontrun{
tmp <- timeit({
  x <- 1:1E4; y <- x + runif(1E4)
  lm( y ~ x )
  }, times=5)
summary(tmp)
y <- 1E4
f <- function(x) { summary( sort( rnorm(x) ) ) }
tmp <- timeit( f(y), times=5 )
if( !is.null(tmp) ) {
  summary(tmp)
  mean(tmp)
  if( require(ggplot2) ) { plot(tmp) }
}}
}
\seealso{
  \code{\link{mean.timeit}} for mean running times over all
  iterations processed, \code{\link{summary.timeit}} for
  summary statistics, \code{\link{plot.timeit}} for
  generating a boxplot of the returned times,
  \code{\link{do_timeit}} for the workhorse function, and
  \code{\link{Rprof}} for information on how \R profiles
  the execution of expressions.
}

