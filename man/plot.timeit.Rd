\name{plot.timeit}
\alias{plot.timeit}
\title{Plot a 'timeit' Object}
\usage{
  \method{plot}{timeit} (x, y = NULL, min.pct = 5, ...)
}
\arguments{
  \item{x}{the \code{timeit} object.}

  \item{y}{unused.}

  \item{min.pct}{number between 0 and 100. when set, we
  only plot functions whose calling time makes up greater
  than \code{min.pct} of the total calling time.}

  \item{...}{unused additional arguments.}
}
\description{
  This generates a boxplot of the timing output for a
  \code{timeit} object.
}

