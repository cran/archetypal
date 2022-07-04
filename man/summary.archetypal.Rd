\name{summary.archetypal}
\alias{summary.archetypal}
\title{
Summary for an object of the class archetypal.
}
\description{
It gives a summary for the output of \code{\link{archetypal}}
}
\usage{
\method{summary}{archetypal}(object, ...)
}
\arguments{
  \item{object}{
An object of the class archetypal
}
  \item{\dots}{
Other arguments (ignored)
}
}
\details{
Next info is given:
\enumerate{
\item the number of observations or the row number of the data frame
\item the dimension of the data variables
\item the number of archetypes that was used
\item the computed archetypes 
\item a vector of run details: SSE, VarianceExplained, Convergence, Iterations, EllapsedTime
\item the calling command
}
}
\examples{
{\donttest{
## Use the sample data "wd2"
data(wd2)
aa=archetypal(wd2, 3)
summary(aa)
}
}
}
