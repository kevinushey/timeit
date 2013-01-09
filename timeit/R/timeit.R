#' Profile a Function Call
#' 
#' This is a wrapper to \code{\link{Rprof}} that cleans up some of the
#' profile hand-holding and provides easier usage. This allows you to profile
#' either a single function call, or a whole block. Evalutation can be run
#' multiple times in order to assess variability in the timings of each
#' function call (to some level of precision).
#' 
#' Function calls that get executed very quickly will be missed
#' by \code{Rprof}, unless you set \code{interval} very low. However, doing
#' this will probably break things (and isn't really important, since profiling
#' is there to help you catch the longest-running functions.)
#' 
#' @param call a call; this can be a single function call or a whole block.
#' @param interval real: time interval between samples.
#' @param memory.profiling logical: write memory use information to file?
#' @param times integer. how many times to call the function?
#' @export
#' @return an object of S3 classes \code{timeit} and \code{data.frame}.
#' @seealso \code{\link{summary.timeit}} for summary output, 
#' \code{\link{plot.timeit}} for generating a boxplot of the returned
#' times, \code{\link{do_timeit}} for the workhorse function, and 
#' \code{\link{Rprof}} for information on how R profiles 
#' execution of \R expressions.
#' @examples \dontrun{
#' tmp <- timeit({
#'   x <- 1:1E5; y <- x + runif(1E5)
#'   lm( y ~ x )
#'   }, times=5)
#' summary(tmp)
#' y <- 1E6
#' f <- function(x) { summary( sort( rnorm(x) ) ) }
#' tmp <- timeit( f(y), times=5 )
#' summary(tmp)
#' mean(tmp)
#' plot(tmp)}
timeit <- function(call,
                   interval=0.01,
                   memory.profiling=TRUE,
                   times=5
                   ) {
  
  stopifnot( times > 0 )
  
  out_list <- vector("list", 5)
  for( i in 1:times ) {
    cat("Running iteration", i, "of", times, "\n")
    call_me <- match.call()$call
    out_list[[i]] <- do_timeit( call_me, interval, memory.profiling )$by.self
  }
  
  out_list <- out_list[ sapply( out_list, function(x) { !is.null(x) } ) ]
  
  for( i in 1:length(out_list) ) {
    out_list[[i]]$func <- rownames( out_list[[i]] )
    out_list[[i]]$iter <- i
  }
  
  out <- as.data.frame( stringsAsFactors=FALSE, optional=TRUE,
                        do.call( rbind, out_list )
                        )
  out$func <- factor( out$func,
                      levels=names( sort( tapply( out$self.time, out$func, median ) ) )
                      )
  class(out) <- c("timeit", "data.frame")
  
  return(out)
  
}

#' Profile a Function Call
#' 
#' This is the workhorse function called by \code{\link{timeit}}. It is
#' almost a direct export of the example on \code{\link{summaryRprof}}.
#' @param call a call; this can be a single function call or a whole block.
#' @param interval real: time interval between samples.
#' @param memory.profiling logical: write memory use information to file?
#' @export
#' @return a data.frame of the profiling times
do_timeit <- function(call, 
                      interval=0.005, 
                      memory.profiling=FALSE
                      ) {
  
  if( isTRUE( memory.profiling ) ) {
    memory <- "both"
  } else {
    memory <- "none"
  }
  
  tmp <- tempfile()
  on.exit( unlink(tmp) )
    
  Rprof( tmp, interval=interval, memory.profiling=memory.profiling )
  invisible( eval( call ) )
  Rprof(NULL)
  
  out <- summaryRprof(tmp, memory=memory)
  return( out )
  
}

#' Summarize an 'timeit' Object
#' 
#' This function generates some summary statistics for output from a
#' \code{\link{timeit}}.
#' 
#' @param object an object of class \code{timeit}.
#' @param ... ignored.
#' @export
#' @method summary timeit
#' @S3method summary timeit
summary.timeit <- function( object, ... ) {
  
  return(
    do.call( cbind, tapply( object$self.time, object$func, function(x) {
      tmp <- c( summary(x), length(x) )
      names(tmp)[7] <- "n"
      return(tmp)
    } ) )
  )
}

#' Plot a 'timeit' Object
#' 
#' This generates a boxplot of the timing output for a \code{timeit} object.
#' 
#' @param x the \code{timeit} object.
#' @param y unused.
#' @param ... unused additional arguments.
#' @export
#' @method plot timeit
#' @S3method plot timeit
plot.timeit <- function( x, y=NULL, ... ) {
  
  require("ggplot2")
  
  func <- as.symbol("func")
  self.time <- as.symbol("self.time")
  
  print( ggplot( x, aes(x=func, y=self.time*1E3) ) +
           geom_boxplot(outlier.size=0) +
           geom_point( pch=21, fill="red", col="black", alpha=0.4 ) +
           ylab("Time (milliseconds)") +
           xlab("Function") +
           ggtitle("Time (milliseconds) spent in each function call") +
           coord_flip()
  )
  
}

#' Calculate the mean of a 'timeit' Object
#' 
#' This function calculates the mean running time of each function call.
#' 
#' @param x the 'timeit' object.
#' @param ... additional arguments supplied to \code{\link{mean.default}}.
#' @export
#' @S3method mean timeit
#' @method mean timeit
mean.timeit <- function(x, ...) {
  names <- names(x)
  out <- aggregate( x[ !(names(x) %in% "func") ], x["func"], FUN=mean )
  out[ !(names(out) %in% "iter") ]
}